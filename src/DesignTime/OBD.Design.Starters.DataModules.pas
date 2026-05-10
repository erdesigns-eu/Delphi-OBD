//------------------------------------------------------------------------------
//  OBD.Design.Starters.DataModules
//
//  P-A7 commit 2: pre-wired TDataModule starters for the
//  common quick-start scenarios. Each one drops a coherent
//  bundle of components into a TDataModule the host can use
//  as the central plumbing for a multi-form app.
//
//  Seven datamodules ship:
//    DM_OBDConnection     bare connection chain
//    DM_OBDDiagnostics    connection + service-mode reads
//    DM_OBDCoding         connection + write-side
//    DM_OBDFlashing       connection + flash pipeline (safety off)
//    DM_OBDRadio          connection + radio-code calculator + VINInspector
//    DM_OBDEVBattery      connection + EV battery + catalogue
//    DM_OBDKeyAdaptation  connection + key-adaptation (safety off)
//
//  Each generator emits a .dpr that just opens the .dfm
//  (the DataModule is the only "form" in the project) plus
//  the .pas / .dfm pair.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//------------------------------------------------------------------------------

unit OBD.Design.Starters.DataModules;

interface

implementation

uses
  System.SysUtils,
  System.Classes,
  OBD.Design.Starters;

function Expand(const ATemplate: string;
  const AContext: TOBDStarterContext): string; inline;
begin
  Result := ExpandTemplate(ATemplate, AContext);
end;

function MakeDpr(const AContext: TOBDStarterContext): TOBDStarterArtifact; inline;
begin
  Result := MakeDprArtifact(AContext);
end;

function PasArtifact(const AContext: TOBDStarterContext;
  const ATemplate: string): TOBDStarterArtifact;
begin
  Result := Default(TOBDStarterArtifact);
  Result.RelativePath := AContext.UnitName + '.pas';
  Result.Content := Expand(ATemplate, AContext);
end;

function DfmArtifact(const AContext: TOBDStarterContext;
  const ATemplate: string): TOBDStarterArtifact;
begin
  Result := Default(TOBDStarterArtifact);
  Result.RelativePath := AContext.UnitName + '.dfm';
  Result.Content := Expand(ATemplate, AContext);
end;

{ ---- DM_OBDConnection ------------------------------------------------------- }

function GenerateDMConnection(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS =
    'unit {UNIT};'#13#10#13#10 +
    'interface'#13#10#13#10 +
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol;'#13#10#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TDataModule)'#13#10 +
    '    Connection: TOBDConnection;'#13#10 +
    '    Adapter:    TOBDAdapter;'#13#10 +
    '    Protocol:   TOBDProtocol;'#13#10 +
    '  end;'#13#10#13#10 +
    'var {FORMVAR}: {FORM};'#13#10#13#10 +
    'implementation'#13#10#13#10 +
    '{$R *.dfm}'#13#10#13#10 +
    'end.'#13#10;
  DFM =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  OldCreateOrder = False  Height = 200  Width = 360'#13#10 +
    '  object Connection: TOBDConnection Left = 32 Top = 24 end'#13#10 +
    '  object Adapter:    TOBDAdapter Connection = Connection Left = 96 Top = 24 end'#13#10 +
    '  object Protocol:   TOBDProtocol Adapter = Adapter Left = 160 Top = 24 end'#13#10 +
    'end'#13#10;
begin
  SetLength(Result, 3);
  Result[0] := MakeDpr(AContext);
  Result[1] := PasArtifact(AContext, PAS);
  Result[2] := DfmArtifact(AContext, DFM);
end;

{ ---- DM_OBDDiagnostics ------------------------------------------------------ }

function GenerateDMDiagnostics(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS =
    'unit {UNIT};'#13#10#13#10 +
    'interface'#13#10#13#10 +
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol,'#13#10 +
    '  OBD.Service.LiveData, OBD.Service.DTCs, OBD.Service.VIN,'#13#10 +
    '  OBD.Service.VINInspector, OBD.Service.FreezeFrame,'#13#10 +
    '  OBD.Service.OnBoardMonitor, OBD.Service.VehicleHealth;'#13#10#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TDataModule)'#13#10 +
    '    Connection:      TOBDConnection;'#13#10 +
    '    Adapter:         TOBDAdapter;'#13#10 +
    '    Protocol:        TOBDProtocol;'#13#10 +
    '    LiveData:        TOBDLiveData;'#13#10 +
    '    DTCs:            TOBDDTCs;'#13#10 +
    '    VIN:             TOBDVIN;'#13#10 +
    '    VINInspector:    TOBDVINInspector;'#13#10 +
    '    FreezeFrame:     TOBDFreezeFrame;'#13#10 +
    '    OnBoardMonitor:  TOBDOnBoardMonitor;'#13#10 +
    '    VehicleHealth:   TOBDVehicleHealth;'#13#10 +
    '  end;'#13#10#13#10 +
    'var {FORMVAR}: {FORM};'#13#10#13#10 +
    'implementation'#13#10#13#10 +
    '{$R *.dfm}'#13#10#13#10 +
    'end.'#13#10;
  DFM =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  OldCreateOrder = False  Height = 360  Width = 480'#13#10 +
    '  object Connection:     TOBDConnection Left = 32 Top = 24 end'#13#10 +
    '  object Adapter:        TOBDAdapter Connection = Connection Left = 96 Top = 24 end'#13#10 +
    '  object Protocol:       TOBDProtocol Adapter = Adapter Left = 160 Top = 24 end'#13#10 +
    '  object LiveData:       TOBDLiveData Protocol = Protocol Left = 32 Top = 88 end'#13#10 +
    '  object DTCs:           TOBDDTCs Protocol = Protocol Left = 96 Top = 88 end'#13#10 +
    '  object VIN:            TOBDVIN Protocol = Protocol Left = 160 Top = 88 end'#13#10 +
    '  object VINInspector:   TOBDVINInspector AutoDecode = True Left = 224 Top = 88 end'#13#10 +
    '  object FreezeFrame:    TOBDFreezeFrame Protocol = Protocol Left = 32 Top = 152 end'#13#10 +
    '  object OnBoardMonitor: TOBDOnBoardMonitor Protocol = Protocol Left = 96 Top = 152 end'#13#10 +
    '  object VehicleHealth:  TOBDVehicleHealth Protocol = Protocol VIN = VIN DTCs = DTCs LiveData = LiveData OnBoardMonitor = OnBoardMonitor FreezeFrame = FreezeFrame Left = 160 Top = 152 end'#13#10 +
    'end'#13#10;
begin
  SetLength(Result, 3);
  Result[0] := MakeDpr(AContext);
  Result[1] := PasArtifact(AContext, PAS);
  Result[2] := DfmArtifact(AContext, DFM);
end;

{ ---- DM_OBDCoding ----------------------------------------------------------- }

function GenerateDMCoding(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS =
    'unit {UNIT};'#13#10#13#10 +
    'interface'#13#10#13#10 +
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol,'#13#10 +
    '  OBD.Coding.SecurityAccess, OBD.Coding.DataIdentifierIO,'#13#10 +
    '  OBD.Coding.RoutineControl, OBD.Coding.Flasher,'#13#10 +
    '  OBD.Coding.AuditLog, OBD.Coding.Session;'#13#10#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TDataModule)'#13#10 +
    '    Connection:        TOBDConnection;'#13#10 +
    '    Adapter:           TOBDAdapter;'#13#10 +
    '    Protocol:          TOBDProtocol;'#13#10 +
    '    SecurityAccess:    TOBDSecurityAccess;'#13#10 +
    '    DID:               TOBDDataIdentifierIO;'#13#10 +
    '    RoutineControl:    TOBDRoutineControl;'#13#10 +
    '    Flasher:           TOBDFlasher;'#13#10 +
    '    AuditLog:          TOBDCodingAuditLog;'#13#10 +
    '    Session:           TOBDCodingSession;'#13#10 +
    '  end;'#13#10#13#10 +
    'var {FORMVAR}: {FORM};'#13#10#13#10 +
    'implementation'#13#10#13#10 +
    '{$R *.dfm}'#13#10#13#10 +
    'end.'#13#10;
  DFM =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  OldCreateOrder = False  Height = 320  Width = 440'#13#10 +
    '  object Connection:     TOBDConnection Left = 32 Top = 24 end'#13#10 +
    '  object Adapter:        TOBDAdapter Connection = Connection Left = 96 Top = 24 end'#13#10 +
    '  object Protocol:       TOBDProtocol Adapter = Adapter Left = 160 Top = 24 end'#13#10 +
    '  object SecurityAccess: TOBDSecurityAccess Protocol = Protocol Left = 32 Top = 88 end'#13#10 +
    '  object DID:            TOBDDataIdentifierIO Protocol = Protocol Left = 96 Top = 88 end'#13#10 +
    '  object RoutineControl: TOBDRoutineControl Protocol = Protocol Left = 160 Top = 88 end'#13#10 +
    '  object Flasher:        TOBDFlasher Protocol = Protocol AutoExecute = False Left = 32 Top = 152 end'#13#10 +
    '  object AuditLog:       TOBDCodingAuditLog Left = 96 Top = 152 end'#13#10 +
    '  object Session:        TOBDCodingSession Protocol = Protocol AuditLog = AuditLog Left = 160 Top = 152 end'#13#10 +
    'end'#13#10;
begin
  SetLength(Result, 3);
  Result[0] := MakeDpr(AContext);
  Result[1] := PasArtifact(AContext, PAS);
  Result[2] := DfmArtifact(AContext, DFM);
end;

{ ---- DM_OBDFlashing -------------------------------------------------------- }

function GenerateDMFlashing(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS =
    'unit {UNIT};'#13#10#13#10 +
    'interface'#13#10#13#10 +
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol,'#13#10 +
    '  OBD.UDS.Transfer, OBD.Flash.VoltageGate, OBD.Flash.Pipeline;'#13#10#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TDataModule)'#13#10 +
    '    Connection:    TOBDConnection;'#13#10 +
    '    Adapter:       TOBDAdapter;'#13#10 +
    '    Protocol:      TOBDProtocol;'#13#10 +
    '    Transfer:      TOBDUDSTransfer;'#13#10 +
    '    VoltageGate:   TOBDVoltageGate;'#13#10 +
    '    FlashPipeline: TOBDFlashPipeline;'#13#10 +
    '  end;'#13#10#13#10 +
    'var {FORMVAR}: {FORM};'#13#10#13#10 +
    'implementation'#13#10#13#10 +
    '{$R *.dfm}'#13#10#13#10 +
    '// IMPORTANT: every flash component ships with AutoExecute=False'#13#10 +
    '// so the bus stays safe until the host wires OnConfirmExecute and'#13#10 +
    '// flips AutoExecute=True at the moment the user authorises.'#13#10#13#10 +
    'end.'#13#10;
  DFM =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  OldCreateOrder = False  Height = 240  Width = 440'#13#10 +
    '  object Connection:    TOBDConnection Left = 32 Top = 24 end'#13#10 +
    '  object Adapter:       TOBDAdapter Connection = Connection Left = 96 Top = 24 end'#13#10 +
    '  object Protocol:      TOBDProtocol Adapter = Adapter Left = 160 Top = 24 end'#13#10 +
    '  object Transfer:      TOBDUDSTransfer Protocol = Protocol AutoExecute = False Left = 32 Top = 88 end'#13#10 +
    '  object VoltageGate:   TOBDVoltageGate AutoExecute = False Left = 96 Top = 88 end'#13#10 +
    '  object FlashPipeline: TOBDFlashPipeline Protocol = Protocol Transfer = Transfer VoltageGate = VoltageGate AutoExecute = False Left = 160 Top = 88 end'#13#10 +
    'end'#13#10;
begin
  SetLength(Result, 3);
  Result[0] := MakeDpr(AContext);
  Result[1] := PasArtifact(AContext, PAS);
  Result[2] := DfmArtifact(AContext, DFM);
end;

{ ---- DM_OBDRadio ----------------------------------------------------------- }

function GenerateDMRadio(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS =
    'unit {UNIT};'#13#10#13#10 +
    'interface'#13#10#13#10 +
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol,'#13#10 +
    '  OBD.RadioCode, OBD.RadioCode.FrenchItalian,'#13#10 +
    '  OBD.Service.VINInspector;'#13#10#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TDataModule)'#13#10 +
    '    Connection:    TOBDConnection;'#13#10 +
    '    Adapter:       TOBDAdapter;'#13#10 +
    '    Protocol:      TOBDProtocol;'#13#10 +
    '    Calc:          TOBDRadioCodePeugeot;'#13#10 +
    '    VINInspector:  TOBDVINInspector;'#13#10 +
    '  end;'#13#10#13#10 +
    'var {FORMVAR}: {FORM};'#13#10#13#10 +
    'implementation'#13#10#13#10 +
    '{$R *.dfm}'#13#10#13#10 +
    'end.'#13#10;
  DFM =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  OldCreateOrder = False  Height = 220  Width = 380'#13#10 +
    '  object Connection:   TOBDConnection Left = 32 Top = 24 end'#13#10 +
    '  object Adapter:      TOBDAdapter Connection = Connection Left = 96 Top = 24 end'#13#10 +
    '  object Protocol:     TOBDProtocol Adapter = Adapter Left = 160 Top = 24 end'#13#10 +
    '  object Calc:         TOBDRadioCodePeugeot Left = 32 Top = 88 end'#13#10 +
    '  object VINInspector: TOBDVINInspector AutoDecode = True Left = 96 Top = 88 end'#13#10 +
    'end'#13#10;
begin
  SetLength(Result, 3);
  Result[0] := MakeDpr(AContext);
  Result[1] := PasArtifact(AContext, PAS);
  Result[2] := DfmArtifact(AContext, DFM);
end;

{ ---- DM_OBDEVBattery ------------------------------------------------------- }

function GenerateDMEVBattery(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS =
    'unit {UNIT};'#13#10#13#10 +
    'interface'#13#10#13#10 +
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol,'#13#10 +
    '  OBD.Service.EVBattery,'#13#10 +
    '  OBD.Service.EVBattery.Catalog.Component;'#13#10#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TDataModule)'#13#10 +
    '    Connection: TOBDConnection;'#13#10 +
    '    Adapter:    TOBDAdapter;'#13#10 +
    '    Protocol:   TOBDProtocol;'#13#10 +
    '    EVCatalog:  TOBDEVBatteryCatalogComp;'#13#10 +
    '    EVBattery:  TOBDEVBattery;'#13#10 +
    '  end;'#13#10#13#10 +
    'var {FORMVAR}: {FORM};'#13#10#13#10 +
    'implementation'#13#10#13#10 +
    '{$R *.dfm}'#13#10#13#10 +
    'end.'#13#10;
  DFM =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  OldCreateOrder = False  Height = 220  Width = 380'#13#10 +
    '  object Connection: TOBDConnection Left = 32 Top = 24 end'#13#10 +
    '  object Adapter:    TOBDAdapter Connection = Connection Left = 96 Top = 24 end'#13#10 +
    '  object Protocol:   TOBDProtocol Adapter = Adapter Left = 160 Top = 24 end'#13#10 +
    '  object EVCatalog:  TOBDEVBatteryCatalogComp AutoLoad = True Left = 32 Top = 88 end'#13#10 +
    '  object EVBattery:  TOBDEVBattery Protocol = Protocol PollIntervalMs = 2000 Left = 96 Top = 88 end'#13#10 +
    'end'#13#10;
begin
  SetLength(Result, 3);
  Result[0] := MakeDpr(AContext);
  Result[1] := PasArtifact(AContext, PAS);
  Result[2] := DfmArtifact(AContext, DFM);
end;

{ ---- DM_OBDKeyAdaptation --------------------------------------------------- }

function GenerateDMKeyAdaptation(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS =
    'unit {UNIT};'#13#10#13#10 +
    'interface'#13#10#13#10 +
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol,'#13#10 +
    '  OBD.OEM.KeyAdaptation.Ford;'#13#10#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TDataModule)'#13#10 +
    '    Connection:    TOBDConnection;'#13#10 +
    '    Adapter:       TOBDAdapter;'#13#10 +
    '    Protocol:      TOBDProtocol;'#13#10 +
    '    KeyAdaptation: TOBDKeyAdaptationFord;'#13#10 +
    '  end;'#13#10#13#10 +
    'var {FORMVAR}: {FORM};'#13#10#13#10 +
    'implementation'#13#10#13#10 +
    '{$R *.dfm}'#13#10#13#10 +
    '// IMPORTANT: KeyAdaptation ships with AutoExecute=False;'#13#10 +
    '// the host MUST wire OnConfirmExecute before any AddKey /'#13#10 +
    '// ClearAllKeys / ClearOneSlot call.'#13#10#13#10 +
    'end.'#13#10;
  DFM =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  OldCreateOrder = False  Height = 220  Width = 380'#13#10 +
    '  object Connection:    TOBDConnection Left = 32 Top = 24 end'#13#10 +
    '  object Adapter:       TOBDAdapter Connection = Connection Left = 96 Top = 24 end'#13#10 +
    '  object Protocol:      TOBDProtocol Adapter = Adapter Left = 160 Top = 24 end'#13#10 +
    '  object KeyAdaptation: TOBDKeyAdaptationFord Protocol = Protocol AutoExecute = False Left = 32 Top = 88 end'#13#10 +
    'end'#13#10;
begin
  SetLength(Result, 3);
  Result[0] := MakeDpr(AContext);
  Result[1] := PasArtifact(AContext, PAS);
  Result[2] := DfmArtifact(AContext, DFM);
end;

{ ---- registration --------------------------------------------------------- }

procedure RegisterAll;
var S: TOBDStarter;
begin
  S := Default(TOBDStarter);
  S.Id := 'dm-connection';
  S.Title := 'DataModule: Connection chain';
  S.Description :=
    'Bare connection / adapter / protocol chain on a TDataModule. ' +
    'Foundation for any project that needs to share one bus across ' +
    'several forms.';
  S.Category := 'DataModule';
  S.Generate := GenerateDMConnection;
  TOBDStarterRegistry.Default.Register(S);

  S := Default(TOBDStarter);
  S.Id := 'dm-diagnostics';
  S.Title := 'DataModule: Diagnostics';
  S.Description :=
    'Connection chain + LiveData + DTCs + VIN + VINInspector + ' +
    'FreezeFrame + OnBoardMonitor + VehicleHealth on one DataModule.';
  S.Category := 'DataModule';
  S.Generate := GenerateDMDiagnostics;
  TOBDStarterRegistry.Default.Register(S);

  S := Default(TOBDStarter);
  S.Id := 'dm-coding';
  S.Title := 'DataModule: Coding session';
  S.Description :=
    'Connection chain + SecurityAccess + DataIdentifierIO + ' +
    'RoutineControl + Flasher + CodingAuditLog + CodingSession. ' +
    'Flasher.AutoExecute = False (safety default).';
  S.Category := 'DataModule';
  S.Generate := GenerateDMCoding;
  TOBDStarterRegistry.Default.Register(S);

  S := Default(TOBDStarter);
  S.Id := 'dm-flashing';
  S.Title := 'DataModule: Flash pipeline';
  S.Description :=
    'Connection chain + UDS.Transfer + VoltageGate + FlashPipeline. ' +
    'Every destructive component ships with AutoExecute = False; ' +
    'host wires OnConfirmExecute before flipping it.';
  S.Category := 'DataModule';
  S.Generate := GenerateDMFlashing;
  TOBDStarterRegistry.Default.Register(S);

  S := Default(TOBDStarter);
  S.Id := 'dm-radio';
  S.Title := 'DataModule: Radio code calculator';
  S.Description :=
    'Connection chain + radio-code calculator + VINInspector ' +
    'pre-wired. Swap the calculator class for your target vendor ' +
    'after generation.';
  S.Category := 'DataModule';
  S.Generate := GenerateDMRadio;
  TOBDStarterRegistry.Default.Register(S);

  S := Default(TOBDStarter);
  S.Id := 'dm-evbattery';
  S.Title := 'DataModule: EV battery health';
  S.Description :=
    'Connection chain + EV battery catalogue (AutoLoad) + ' +
    'EVBattery component (PollIntervalMs = 2000). Set Vendor on ' +
    'the EVBattery component to one of the bundled keys.';
  S.Category := 'DataModule';
  S.Generate := GenerateDMEVBattery;
  TOBDStarterRegistry.Default.Register(S);

  S := Default(TOBDStarter);
  S.Id := 'dm-keyadaptation';
  S.Title := 'DataModule: Key adaptation';
  S.Description :=
    'Connection chain + key-adaptation component pre-wired. Ships ' +
    'with one bundled vendor dropped in - swap class to your ' +
    'target after generation. AutoExecute = False (safety).';
  S.Category := 'DataModule';
  S.Generate := GenerateDMKeyAdaptation;
  TOBDStarterRegistry.Default.Register(S);
end;

initialization
  RegisterAll;

end.
