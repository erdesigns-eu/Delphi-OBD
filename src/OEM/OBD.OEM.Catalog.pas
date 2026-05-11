//------------------------------------------------------------------------------
//  OBD.OEM.Catalog
//
//  TOBDOEMCatalog — non-visual component that loads OEM-specific
//  JSON overlay files into a <see cref="TOBDOEMOverlay"/> and
//  registers the overlay with the process-wide
//  <see cref="TOBDOEMRegistry"/>.
//
//  Expected JSON layout (any subset is accepted; missing top-
//  level arrays are silently skipped):
//
//    {
//      "oem": "vag",
//      "dids":  [ { "id": "0xF190", "name": "Vehicle Identification" }, ... ],
//      "pids":  [ { "mode": "0x22", "pid": "0xF1A0",
//                   "name": "Coding Index" }, ... ],
//      "dtcs":  [ { "code": "P171F", "name": "Lambda Bank 1 Slow" }, ... ],
//      "spns":  [ { "spn": 1234, "name": "Custom SPN" }, ... ],
//      "fmis":  [ { "fmi": 31, "name": "Vendor-specific" }, ... ],
//      "pgns":  [ { "pgn": "0xFE56", "name": "Custom PGN" }, ... ]
//    }
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.Catalog;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.IOUtils,
  OBD.Types,
  OBD.OEM.Registry;

type
  /// <summary>Fires after a successful load. Main thread.</summary>
  TOBDOEMCatalogLoadEvent = procedure(Sender: TObject;
    const AFileName: string; AEntryCount: Integer) of object;

  /// <summary>
  ///   OEM-overlay loader component.
  /// </summary>
  /// <remarks>
  ///   Drop on a form, set <c>FileName</c> to a JSON overlay,
  ///   set <c>AutoLoad := True</c> for the loader to consume it
  ///   on <c>Loaded</c>, or call <see cref="Load"/> explicitly.
  ///   The loaded overlay registers with
  ///   <c>TOBDOEMRegistry.Instance</c>; destroying the component
  ///   unregisters and frees the overlay.
  /// </remarks>
  TOBDOEMCatalog = class(TComponent)
  strict private
    FFileName: string;
    FOEM: string;
    FAutoLoad: Boolean;
    FOverlay: TOBDOEMOverlay;
    FEntryCount: Integer;
    FOnLoaded: TOBDOEMCatalogLoadEvent;
    function ParseHexOrInt(const AValue: string): Cardinal;
    procedure ConsumeArray(AArray: TJSONArray;
      AOverlay: TOBDOEMOverlay;
      AHandler: TFunc<TJSONObject, Boolean>);
    procedure UnregisterAndFreeOverlay;
    procedure FireLoaded(const AFileName: string;
      AEntryCount: Integer);
  protected
    procedure Loaded; override;
  public
    /// <summary>Constructs the loader. Nothing is read until
    /// <see cref="Load"/> is invoked (manually or via
    /// <c>AutoLoad</c> at design-loaded time).</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Unregisters and frees the owned overlay.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Loads and registers the catalogue at <c>FileName</c>.
    /// </summary>
    /// <returns>Total entry count across every kind in the
    /// loaded JSON.</returns>
    /// <exception cref="EOBDConfig">
    ///   <c>FileName</c> is empty.
    /// </exception>
    /// <exception cref="EOBDInternal">
    ///   The file is missing, unreadable, or contains malformed
    ///   JSON.
    /// </exception>
    function Load: Integer;

    /// <summary>Unregisters and frees the owned overlay (if any).
    /// Safe to call repeatedly.</summary>
    procedure Unload;

    /// <summary>The owned overlay, or <c>nil</c> when nothing has
    /// been loaded. Do not free directly — call
    /// <see cref="Unload"/>.</summary>
    property Overlay: TOBDOEMOverlay read FOverlay;

    /// <summary>Entry count from the last successful load.</summary>
    property EntryCount: Integer read FEntryCount;
  published
    /// <summary>Path to a JSON overlay file.</summary>
    property FileName: string read FFileName write FFileName;

    /// <summary>
    ///   OEM tag attributed to the overlay. Defaults to the
    ///   filename stem when empty.
    /// </summary>
    property OEM: string read FOEM write FOEM;

    /// <summary>
    ///   Load the file automatically when the form is loaded at
    ///   design or runtime. Default <c>False</c>.
    /// </summary>
    property AutoLoad: Boolean read FAutoLoad write FAutoLoad
      default False;

    /// <summary>Fires after a successful load. Main thread.</summary>
    property OnLoaded: TOBDOEMCatalogLoadEvent read FOnLoaded
      write FOnLoaded;
  end;

implementation

constructor TOBDOEMCatalog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TOBDOEMCatalog.Destroy;
begin
  UnregisterAndFreeOverlay;
  inherited;
end;

procedure TOBDOEMCatalog.Loaded;
begin
  inherited;
  if FAutoLoad and (FFileName <> '') and
     (not (csDesigning in ComponentState)) then
    Load;
end;

function TOBDOEMCatalog.ParseHexOrInt(const AValue: string): Cardinal;
var
  Trimmed: string;
  Value: Int64;
begin
  Trimmed := Trim(AValue);
  if (Length(Trimmed) >= 2) and
     (Trimmed[1] = '0') and
     ((Trimmed[2] = 'x') or (Trimmed[2] = 'X')) then
    Trimmed := '$' + Copy(Trimmed, 3, Length(Trimmed) - 2);
  if TryStrToInt64(Trimmed, Value) then
    Result := Cardinal(Value)
  else
    raise EOBDInternal.CreateFmt(
      'TOBDOEMCatalog: cannot parse number %s', [AValue]);
end;

procedure TOBDOEMCatalog.ConsumeArray(AArray: TJSONArray;
  AOverlay: TOBDOEMOverlay;
  AHandler: TFunc<TJSONObject, Boolean>);
var
  I: Integer;
  Obj: TJSONObject;
begin
  if AArray = nil then
    Exit;
  for I := 0 to AArray.Count - 1 do
  begin
    Obj := AArray.Items[I] as TJSONObject;
    if AHandler(Obj) then
      Inc(FEntryCount);
  end;
end;

function TOBDOEMCatalog.Load: Integer;
var
  Text: string;
  Root: TJSONValue;
  RootObj: TJSONObject;
  OEMTag: string;
  Arr: TJSONArray;
  Overlay: TOBDOEMOverlay;
begin
  if FFileName = '' then
    raise EOBDConfig.Create('TOBDOEMCatalog: FileName is empty');
  if not FileExists(FFileName) then
    raise EOBDInternal.CreateFmt(
      'TOBDOEMCatalog: file not found "%s"', [FFileName]);

  try
    Text := TFile.ReadAllText(FFileName, TEncoding.UTF8);
  except
    on E: Exception do
      raise EOBDInternal.CreateFmt(
        'TOBDOEMCatalog: cannot read "%s": %s', [FFileName, E.Message]);
  end;

  Root := TJSONObject.ParseJSONValue(Text);
  if not (Root is TJSONObject) then
  begin
    Root.Free;
    raise EOBDInternal.CreateFmt(
      'TOBDOEMCatalog: "%s" is not a JSON object', [FFileName]);
  end;

  RootObj := Root as TJSONObject;
  try
    UnregisterAndFreeOverlay;
    FEntryCount := 0;

    OEMTag := FOEM;
    if OEMTag = '' then
      OEMTag := RootObj.GetValue<string>('oem', '');
    if OEMTag = '' then
      OEMTag := TPath.GetFileNameWithoutExtension(FFileName);

    Overlay := TOBDOEMOverlay.Create(OEMTag);
    try
      Arr := RootObj.GetValue<TJSONArray>('dids');
      ConsumeArray(Arr, Overlay,
        function(Obj: TJSONObject): Boolean
        var
          IdText: string;
        begin
          Result := False;
          if not Assigned(Obj) then Exit;
          IdText := Obj.GetValue<string>('id', '');
          if IdText = '' then Exit;
          Overlay.AddDID(Word(ParseHexOrInt(IdText)),
            Obj.GetValue<string>('name', ''));
          Result := True;
        end);

      Arr := RootObj.GetValue<TJSONArray>('pids');
      ConsumeArray(Arr, Overlay,
        function(Obj: TJSONObject): Boolean
        var
          ModeText: string;
          PIDText: string;
        begin
          Result := False;
          if not Assigned(Obj) then Exit;
          ModeText := Obj.GetValue<string>('mode', '');
          PIDText := Obj.GetValue<string>('pid', '');
          if (ModeText = '') or (PIDText = '') then Exit;
          Overlay.AddPID(Byte(ParseHexOrInt(ModeText)),
                         Byte(ParseHexOrInt(PIDText)),
                         Obj.GetValue<string>('name', ''));
          Result := True;
        end);

      Arr := RootObj.GetValue<TJSONArray>('dtcs');
      ConsumeArray(Arr, Overlay,
        function(Obj: TJSONObject): Boolean
        var
          Code: string;
        begin
          Result := False;
          if not Assigned(Obj) then Exit;
          Code := Obj.GetValue<string>('code', '');
          if Code = '' then Exit;
          Overlay.AddDTC(Code, Obj.GetValue<string>('name', ''));
          Result := True;
        end);

      Arr := RootObj.GetValue<TJSONArray>('spns');
      ConsumeArray(Arr, Overlay,
        function(Obj: TJSONObject): Boolean
        var
          SPN: Int64;
        begin
          Result := False;
          if not Assigned(Obj) then Exit;
          if not Obj.TryGetValue<Int64>('spn', SPN) then Exit;
          Overlay.AddSPN(Cardinal(SPN),
            Obj.GetValue<string>('name', ''));
          Result := True;
        end);

      Arr := RootObj.GetValue<TJSONArray>('fmis');
      ConsumeArray(Arr, Overlay,
        function(Obj: TJSONObject): Boolean
        var
          FMI: Int64;
        begin
          Result := False;
          if not Assigned(Obj) then Exit;
          if not Obj.TryGetValue<Int64>('fmi', FMI) then Exit;
          Overlay.AddFMI(Byte(FMI),
            Obj.GetValue<string>('name', ''));
          Result := True;
        end);

      Arr := RootObj.GetValue<TJSONArray>('pgns');
      ConsumeArray(Arr, Overlay,
        function(Obj: TJSONObject): Boolean
        var
          PGNText: string;
        begin
          Result := False;
          if not Assigned(Obj) then Exit;
          PGNText := Obj.GetValue<string>('pgn', '');
          if PGNText = '' then Exit;
          Overlay.AddPGN(ParseHexOrInt(PGNText),
            Obj.GetValue<string>('name', ''));
          Result := True;
        end);

      FOverlay := Overlay;
      Overlay := nil;
    finally
      Overlay.Free;
    end;

    TOBDOEMRegistry.Instance.Register(FOverlay);
    Result := FEntryCount;
    FireLoaded(FFileName, Result);
  finally
    Root.Free;
  end;
end;

procedure TOBDOEMCatalog.Unload;
begin
  UnregisterAndFreeOverlay;
end;

procedure TOBDOEMCatalog.UnregisterAndFreeOverlay;
begin
  if FOverlay = nil then
    Exit;
  TOBDOEMRegistry.Instance.Unregister(FOverlay);
  FreeAndNil(FOverlay);
  FEntryCount := 0;
end;

procedure TOBDOEMCatalog.FireLoaded(const AFileName: string;
  AEntryCount: Integer);
var
  Self_: TOBDOEMCatalog;
  FN: string;
  N: Integer;
begin
  if not Assigned(FOnLoaded) then
    Exit;
  Self_ := Self;
  FN := AFileName;
  N := AEntryCount;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnLoaded(Self_, FN, N)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(Self_.FOnLoaded) then
          Self_.FOnLoaded(Self_, FN, N);
      end);
end;

end.
