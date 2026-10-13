unit ERD.Components.VT.WorkerThread;

// ERD adaptation: namespaced VCL engine; original attribution is retained below.
// Distributed under the selected Mozilla Public License 1.1.
// Full terms and upstream provenance: ../LICENSE.MPL-1.1.txt and ../upstream.json.


interface

uses
  System.Classes,
  ERD.Components.VT.Types,
  ERD.Components.VT.BaseTree;

type
  // internal worker thread
  TERDVTWorkerThread = class(TThread)
  private
    FCurrentTree: TERDVTBaseVirtualTree;
    FWaiterList: TThreadList;
    FRefCount: Integer;
    FWorkEvent: THandle;
    class procedure EnsureCreated();
    class procedure Dispose(CanBlock: Boolean);
    procedure WaitForValidationTermination(Tree: TERDVTBaseVirtualTree);
  protected
    procedure Execute; override;
  public
    constructor Create();
    destructor Destroy; override;

    /// For lifeteime management of the TWorkerThread
    class procedure AddThreadReference;
    class procedure ReleaseThreadReference(ACanBlock: Boolean = False);

    class procedure AddTree(Tree: TERDVTBaseVirtualTree);
    class procedure RemoveTree(pTree: TERDVTBaseVirtualTree; pWaitForValidationTermination: Boolean);
  end;





implementation

uses
  Winapi.Windows,
  System.Types,
  System.SysUtils;

type
  TERDVTBaseVirtualTreeCracker = class(TERDVTBaseVirtualTree)
  end;

var
  WorkerThread: TERDVTWorkerThread = nil;

//----------------- TWorkerThread --------------------------------------------------------------------------------------

class procedure TERDVTWorkerThread.EnsureCreated();
begin
  if not Assigned(WorkerThread) then
    // Create worker thread, initialize it and send it to its wait loop.
    WorkerThread := TERDVTWorkerThread.Create();
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure TERDVTWorkerThread.Dispose(CanBlock: Boolean);
var
  LRef: TThread;
begin
  WorkerThread.FreeOnTerminate := not CanBlock;
  WorkerThread.Terminate();
  SetEvent(WorkerThread.FWorkEvent);
  LRef := WorkerThread;
  WorkerThread := nil; //Will be freed usinf TThread.FreeOnTerminate
  if CanBlock then
    LRef.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure TERDVTWorkerThread.AddThreadReference;
begin
  TERDVTWorkerThread.EnsureCreated();
  InterlockedIncrement(WorkerThread.FRefCount);
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure TERDVTWorkerThread.ReleaseThreadReference(ACanBlock: Boolean);
begin
  if Assigned(WorkerThread) then
  begin
    if InterlockedDecrement(WorkerThread.FRefCount) = 0 then
    begin
      WorkerThread.Dispose(ACanBlock);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TERDVTWorkerThread.Create();

begin
  FWaiterList := TThreadList.Create;
  // Create an event used to trigger our worker thread when something is to do.
  FWorkEvent := CreateEvent(nil, False, False, nil);
  if FWorkEvent = 0 then
    RaiseLastOSError;
  inherited Create(False);
  FreeOnTerminate := True;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TERDVTWorkerThread.Destroy;

begin
  // First let the ancestor stop the thread before freeing our resources.
  inherited;
  CloseHandle(FWorkEvent);
  FWaiterList.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TERDVTWorkerThread.WaitForValidationTermination(Tree: TERDVTBaseVirtualTree);
begin
  // Wait for any references to this tree to be released.
  while FCurrentTree = Tree do
  begin
    Sleep(1); // Don't do busy waiting, let the OS scheduler give other threads a time slice
    CheckSynchronize(); // We need to call CheckSynchronize here because we are using TThread.Synchronize in TBaseVirtualTree.MeasureItemHeight() and ChangeTreeStatesAsync()
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TERDVTWorkerThread.Execute();

// Does some background tasks, like validating tree caches.

var
  EnterStates: TVirtualTreeStates;
  lExceptAddr: Pointer;
  lException: TObject;
  lCurrentTree: TERDVTBaseVirtualTree;
begin
  TThread.NameThreadForDebugging('VirtualTrees.TWorkerThread');
  while not Terminated do
  try
    WaitForSingleObject(FWorkEvent, INFINITE);
    if Terminated then
      exit;

    // Get the next waiting tree.
    with FWaiterList.LockList do
    try
      if Count > 0 then
      begin
        lCurrentTree := Items[0];
        // Remove this tree from waiter list.
        Delete(0);
        // If there is yet another tree to work on then set the work event to keep looping.
        if Count > 0 then
          SetEvent(FWorkEvent);
      end
      else
        lCurrentTree := nil;
    finally
      FWaiterList.UnlockList;
    end;

    // Something to do?
    if Assigned(lCurrentTree) then
    begin
      try
        TERDVTBaseVirtualTreeCracker(lCurrentTree).ChangeTreeStatesAsync([tsValidating], [tsUseCache, tsValidationNeeded]);
        FCurrentTree := lCurrentTree;
        EnterStates := [];
        if not (tsStopValidation in FCurrentTree.TreeStates) and TERDVTBaseVirtualTreeCracker(FCurrentTree).DoValidateCache then
          EnterStates := [tsUseCache];
      finally
        FCurrentTree := nil; // Important: Clear variable before calling ChangeTreeStatesAsync() to prevent deadlock in WaitForValidationTermination(). See issue #1001
        TERDVTBaseVirtualTreeCracker(lCurrentTree).ChangeTreeStatesAsync(EnterStates, [tsValidating, tsStopValidation]);
      end;
    end;
  except
    on Exception do
    begin
      lExceptAddr := ExceptAddr;
      lException := AcquireExceptionObject;
      TThread.Synchronize(nil, procedure
        begin
          raise lException at lExceptAddr;
        end);
      Continue; //the thread should continue to run
    end;
  end;//while
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure TERDVTWorkerThread.AddTree(Tree: TERDVTBaseVirtualTree);

begin
  Assert(Assigned(Tree), 'Tree must not be nil.');
  TERDVTWorkerThread.EnsureCreated();

  // Remove validation stop flag, just in case it is still set.
  TERDVTBaseVirtualTreeCracker(Tree).DoStateChange([], [tsStopValidation]);
  with WorkerThread.FWaiterList.LockList do
  try
    if IndexOf(Tree) = -1 then
      Add(Tree);
  finally
    WorkerThread.FWaiterList.UnlockList;
  end;

  SetEvent(WorkerThread.FWorkEvent);
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure TERDVTWorkerThread.RemoveTree(pTree: TERDVTBaseVirtualTree; pWaitForValidationTermination: Boolean);
begin
  if not Assigned(WorkerThread) then
    exit;
  Assert(Assigned(pTree), 'pTree must not be nil.');

  with WorkerThread.FWaiterList.LockList do
  try
    Remove(pTree);
  finally
    WorkerThread.FWaiterList.UnlockList; // Seen several AVs in this line, was called from TWorkerThrea.Destroy. Joachim Marder.
  end;
  if pWaitForValidationTermination then
    WorkerThread.WaitForValidationTermination(pTree);
end;


end.
