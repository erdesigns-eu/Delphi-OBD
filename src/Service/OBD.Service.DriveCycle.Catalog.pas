//------------------------------------------------------------------------------
//  OBD.Service.DriveCycle.Catalog
//
//  Loads per-monitor drive-cycle definitions from the JSON
//  catalogues and answers per-monitor lookups.
//
//  Catalogue layering:
//
//    1. catalogs/drive-cycle-generic.json (ISO 15031-7) is the
//       baseline - one entry per monitor with a description
//       and a duration.
//    2. catalogs/drive-cycle-<oem>.json overlays may register
//       OEM-specific cycles via the same file shape; they
//       replace the generic entry on a name match.
//    3. Hosts can register additional cycles in code via
//       <see cref="TOBDDriveCycleCatalog.RegisterCycle"/>.
//
//  Threading: the catalogue is process-wide (single instance)
//  and lock-protected on first load; lookups are read-only and
//  thread-safe afterwards.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//------------------------------------------------------------------------------

unit OBD.Service.DriveCycle.Catalog;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.Generics.Collections,
  OBD.Service.DriveCycle.Types;

type
  /// <summary>Static facade for drive-cycle lookups.</summary>
  TOBDDriveCycleCatalog = class
  strict private
    class var FCycles:        TDictionary<TOBDMonitor, TOBDDriveCycle>;
    class var FLoaded:        Boolean;
    class var FCatalogDir:    string;
    class var FLock:          TObject;
    class procedure EnsureLoaded; static;
    class procedure LoadFile(const AFile: string;
      AOverlay: Boolean); static;
  public
    class constructor Create;
    class destructor  Destroy;

    /// <summary>Catalogue root directory. Defaults to
    /// <c>catalogs/</c> next to the executable.</summary>
    class property CatalogDir: string read FCatalogDir write FCatalogDir;

    /// <summary>Force a (re)load. Clears the in-memory cycle
    /// table, reads <c>drive-cycle-generic.json</c> first, then
    /// overlays every <c>drive-cycle-*.json</c> in the same
    /// directory.</summary>
    class procedure Reload; static;

    /// <summary>Returns the cycle for <c>AMonitor</c>. Sets
    /// <c>AOut</c> to a default-zero record and returns False
    /// when no cycle is registered for the monitor.</summary>
    class function TryGetCycle(AMonitor: TOBDMonitor;
      out AOut: TOBDDriveCycle): Boolean; static;

    /// <summary>List of monitors that have a cycle registered.</summary>
    class function RegisteredMonitors: TArray<TOBDMonitor>; static;

    /// <summary>Hosts can inject / override cycles at runtime
    /// (e.g. when shipping per-OEM cycle data alongside their
    /// app rather than via JSON).</summary>
    class procedure RegisterCycle(const ACycle: TOBDDriveCycle); static;
  end;

implementation

class constructor TOBDDriveCycleCatalog.Create;
begin
  FLock        := TObject.Create;
  FCatalogDir  := TPath.Combine(
    TPath.GetDirectoryName(ParamStr(0)), 'catalogs');
  FCycles      := TDictionary<TOBDMonitor, TOBDDriveCycle>.Create;
end;

class destructor TOBDDriveCycleCatalog.Destroy;
begin
  FreeAndNil(FCycles);
  FreeAndNil(FLock);
end;

class procedure TOBDDriveCycleCatalog.LoadFile(const AFile: string;
  AOverlay: Boolean);
var
  Doc:    TJSONObject;
  Arr:    TJSONArray;
  Item:   TJSONObject;
  Source: string;
  Cycle:  TOBDDriveCycle;
  Step:   TOBDDriveCycleStep;
  Steps:  TJSONArray;
  StepObj: TJSONObject;
  Mon:    TOBDMonitor;
  V:      TJSONValue;
  I, J:   Integer;
begin
  if not TFile.Exists(AFile) then Exit;
  Doc := TJSONObject.ParseJSONValue(
    TFile.ReadAllText(AFile, TEncoding.UTF8)) as TJSONObject;
  if Doc = nil then Exit;
  try
    V := Doc.GetValue('spec');
    if V <> nil then Source := V.Value else Source := AFile;
    Arr := Doc.GetValue<TJSONArray>('entries');
    if Arr = nil then Exit;
    for I := 0 to Arr.Count - 1 do
    begin
      Item := Arr.Items[I] as TJSONObject;
      if not TryParseMonitor(
        Item.GetValue<string>('monitor'), Mon) then Continue;

      Cycle := Default(TOBDDriveCycle);
      Cycle.Monitor := Mon;
      V := Item.GetValue('source');
      if V <> nil then Cycle.Source := V.Value else Cycle.Source := Source;

      // Two shapes supported for the per-monitor body:
      // (a) the generic catalogue uses one description +
      //     duration_seconds (single implicit step), and
      // (b) richer overlays can use a "steps":[ {description,
      //     duration_seconds}, ... ] array.
      Steps := nil;
      V := Item.GetValue('steps');
      if (V <> nil) and (V is TJSONArray) then Steps := TJSONArray(V);

      if Steps <> nil then
      begin
        SetLength(Cycle.Steps, Steps.Count);
        for J := 0 to Steps.Count - 1 do
        begin
          StepObj := Steps.Items[J] as TJSONObject;
          Step := Default(TOBDDriveCycleStep);
          Step.Index := J + 1;
          V := StepObj.GetValue('description');
          if V <> nil then Step.Description := V.Value;
          V := StepObj.GetValue('duration_seconds');
          if V <> nil then
            Step.DurationSec := StrToIntDef(V.Value, 0);
          Cycle.TotalSec := Cycle.TotalSec + Step.DurationSec;
          Cycle.Steps[J] := Step;
        end;
      end
      else
      begin
        SetLength(Cycle.Steps, 1);
        Step := Default(TOBDDriveCycleStep);
        Step.Index := 1;
        V := Item.GetValue('description');
        if V <> nil then Step.Description := V.Value;
        V := Item.GetValue('duration_seconds');
        if V <> nil then
          Step.DurationSec := StrToIntDef(V.Value, 0);
        Cycle.TotalSec := Step.DurationSec;
        Cycle.Steps[0] := Step;
      end;

      if AOverlay then
        FCycles.AddOrSetValue(Mon, Cycle)
      else
        if not FCycles.ContainsKey(Mon) then
          FCycles.Add(Mon, Cycle);
    end;
  finally
    Doc.Free;
  end;
end;

class procedure TOBDDriveCycleCatalog.EnsureLoaded;
begin
  if FLoaded then Exit;
  Reload;
end;

class procedure TOBDDriveCycleCatalog.Reload;
var
  Files: TArray<string>;
  F:     string;
begin
  TMonitor.Enter(FLock);
  try
    FCycles.Clear;
    LoadFile(TPath.Combine(FCatalogDir,
      'drive-cycle-generic.json'), False);
    if TDirectory.Exists(FCatalogDir) then
    begin
      Files := TDirectory.GetFiles(FCatalogDir, 'drive-cycle-*.json');
      for F in Files do
        if not SameText(TPath.GetFileName(F),
                        'drive-cycle-generic.json') then
          LoadFile(F, True);
    end;
    FLoaded := True;
  finally
    TMonitor.Exit(FLock);
  end;
end;

class function TOBDDriveCycleCatalog.TryGetCycle(AMonitor: TOBDMonitor;
  out AOut: TOBDDriveCycle): Boolean;
begin
  EnsureLoaded;
  Result := FCycles.TryGetValue(AMonitor, AOut);
  if not Result then AOut := Default(TOBDDriveCycle);
end;

class function TOBDDriveCycleCatalog.RegisteredMonitors: TArray<TOBDMonitor>;
var
  M:   TOBDMonitor;
  Acc: TList<TOBDMonitor>;
begin
  EnsureLoaded;
  Acc := TList<TOBDMonitor>.Create;
  try
    for M in FCycles.Keys do Acc.Add(M);
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

class procedure TOBDDriveCycleCatalog.RegisterCycle(
  const ACycle: TOBDDriveCycle);
begin
  EnsureLoaded;
  TMonitor.Enter(FLock);
  try
    FCycles.AddOrSetValue(ACycle.Monitor, ACycle);
  finally
    TMonitor.Exit(FLock);
  end;
end;

end.
