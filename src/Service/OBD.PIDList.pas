//------------------------------------------------------------------------------
//  OBD.PIDList
//
//  TOBDPIDItem + TOBDPIDList — design-time-editable collection of
//  OBD-II PIDs that can be wired to a TOBDLiveData or TOBDFreezeFrame.
//  Designed for the Object Inspector: drop a collection on a form,
//  add each PID by Mode + PID number + display name, ship.
//
//  Components that consume this list iterate it at runtime to drive
//  Subscribe / Read calls. The collection itself is data-only: it
//  does not own a Protocol or talk to a vehicle.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.PIDList;

interface

uses
  System.SysUtils,
  System.Classes;

type
  /// <summary>
  ///   One PID entry in a <see cref="TOBDPIDList"/>.
  /// </summary>
  /// <remarks>
  ///   Owned by the parent <see cref="TOBDPIDList"/>; the host does
  ///   not free items directly. Editable from the Object Inspector
  ///   on the parent collection.
  /// </remarks>
  TOBDPIDItem = class(TCollectionItem)
  strict private
    FMode: Byte;
    FPID: Byte;
    FName: string;
    FEnabled: Boolean;
    FPollIntervalMs: Cardinal;
  protected
    function GetDisplayName: string; override;
  public
    /// <summary>Constructs a new item with sane defaults.</summary>
    /// <param name="ACollection">Owning collection.</param>
    constructor Create(ACollection: TCollection); override;
    /// <summary>Copies field values from <c>ASource</c>.</summary>
    /// <param name="ASource">Another <c>TOBDPIDItem</c> or the
    /// inherited fallback for unsupported types.</param>
    procedure Assign(ASource: TPersistent); override;
  published
    /// <summary>
    ///   OBD service / mode byte. Default <c>0x01</c> (current
    ///   data).
    /// </summary>
    property Mode: Byte read FMode write FMode default $01;

    /// <summary>PID byte for the chosen <c>Mode</c>.</summary>
    property PID: Byte read FPID write FPID default $00;

    /// <summary>
    ///   Optional human-readable display name. Informational only;
    ///   consumed by the Object-Inspector display name and any
    ///   host-supplied UI.
    /// </summary>
    property Name: string read FName write FName;

    /// <summary>
    ///   Whether the consumer should include this entry. Default
    ///   <c>True</c>. Disabled entries are returned by
    ///   <see cref="TOBDPIDList.Find"/> but skipped by
    ///   <see cref="TOBDPIDList.EnabledFor"/>.
    /// </summary>
    property Enabled: Boolean read FEnabled write FEnabled default True;

    /// <summary>
    ///   Per-PID poll-interval override in milliseconds.
    ///   <c>0</c> (the default) means "use the host component's
    ///   default poll interval".
    /// </summary>
    property PollIntervalMs: Cardinal read FPollIntervalMs
      write FPollIntervalMs default 0;
  end;

  /// <summary>
  ///   Owned collection of <see cref="TOBDPIDItem"/>.
  /// </summary>
  /// <remarks>
  ///   Owned by a parent component (typically a
  ///   <c>TOBDLiveData</c>); freed alongside the parent.
  /// </remarks>
  TOBDPIDList = class(TOwnedCollection)
  public
    /// <summary>Constructs an empty list.</summary>
    /// <param name="AOwner">Parent component.</param>
    constructor Create(AOwner: TPersistent);

    /// <summary>
    ///   Adds a PID and returns the new item.
    /// </summary>
    /// <param name="AMode">OBD mode byte.</param>
    /// <param name="APID">PID byte.</param>
    /// <param name="AName">Optional display name.</param>
    /// <param name="AEnabled">Initial <c>Enabled</c> flag. Default
    /// <c>True</c>.</param>
    /// <returns>The newly-added item, owned by the collection.</returns>
    function Add(AMode: Byte; APID: Byte; const AName: string = '';
      AEnabled: Boolean = True): TOBDPIDItem; reintroduce;

    /// <summary>
    ///   Finds the item with (<c>AMode</c>, <c>APID</c>).
    /// </summary>
    /// <param name="AMode">OBD mode byte.</param>
    /// <param name="APID">PID byte.</param>
    /// <returns>The matching item, or <c>nil</c> when not present.</returns>
    function Find(AMode: Byte; APID: Byte): TOBDPIDItem;

    /// <summary>
    ///   Returns every enabled PID byte for <c>AMode</c>.
    /// </summary>
    /// <param name="AMode">OBD mode byte to filter on.</param>
    /// <returns>PID bytes in collection order.</returns>
    function EnabledFor(AMode: Byte): TArray<Byte>;
  end;

implementation

{ TOBDPIDItem }

constructor TOBDPIDItem.Create(ACollection: TCollection);
begin
  inherited;
  FMode := $01;
  FPID := $00;
  FEnabled := True;
end;

function TOBDPIDItem.GetDisplayName: string;
begin
  if FName <> '' then
    Result := Format('%s (Mode %.2x PID %.2x)', [FName, FMode, FPID])
  else
    Result := Format('Mode %.2x PID %.2x', [FMode, FPID]);
end;

procedure TOBDPIDItem.Assign(ASource: TPersistent);
var
  Src: TOBDPIDItem;
begin
  if ASource is TOBDPIDItem then
  begin
    Src := TOBDPIDItem(ASource);
    FMode := Src.FMode;
    FPID := Src.FPID;
    FName := Src.FName;
    FEnabled := Src.FEnabled;
    FPollIntervalMs := Src.FPollIntervalMs;
  end
  else
    inherited;
end;

{ TOBDPIDList }

constructor TOBDPIDList.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDPIDItem);
end;

function TOBDPIDList.Add(AMode: Byte; APID: Byte; const AName: string;
  AEnabled: Boolean): TOBDPIDItem;
begin
  Result := TOBDPIDItem(inherited Add);
  Result.Mode := AMode;
  Result.PID := APID;
  Result.Name := AName;
  Result.Enabled := AEnabled;
end;

function TOBDPIDList.Find(AMode: Byte; APID: Byte): TOBDPIDItem;
var
  I: Integer;
  It: TOBDPIDItem;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    It := TOBDPIDItem(Items[I]);
    if (It.Mode = AMode) and (It.PID = APID) then
      Exit(It);
  end;
end;

function TOBDPIDList.EnabledFor(AMode: Byte): TArray<Byte>;
var
  I: Integer;
  N: Integer;
  It: TOBDPIDItem;
begin
  N := 0;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
  begin
    It := TOBDPIDItem(Items[I]);
    if It.Enabled and (It.Mode = AMode) then
    begin
      Result[N] := It.PID;
      Inc(N);
    end;
  end;
  SetLength(Result, N);
end;

end.
