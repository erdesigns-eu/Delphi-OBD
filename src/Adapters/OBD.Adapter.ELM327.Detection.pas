//------------------------------------------------------------------------------
// UNIT           : OBD.Adapter.ELM327.Detection.pas
// CONTENTS       : ELM327 Adapter Detection (Including Chinese Clones)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/12/2024
//------------------------------------------------------------------------------
unit OBD.Adapter.ELM327.Detection;

interface

uses
  System.SysUtils, System.Classes, System.StrUtils,
  OBD.Adapter.ELM327, OBD.Adapter.Types;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   ELM327 Chip Type
  /// </summary>
  TELM327ChipType = (
    ctGenuineELM327,      // Genuine Elm Electronics ELM327
    ctChineseCloneV12,    // Chinese clone claiming v1.2
    ctChineseCloneV13,    // Chinese clone claiming v1.3
    ctChineseCloneV14,    // Chinese clone claiming v1.4
    ctChineseCloneV15,    // Chinese clone claiming v1.5
    ctChineseCloneV21,    // Chinese clone claiming v2.1
    ctChineseCloneV22,    // Chinese clone claiming v2.2
    ctChineseCloneV23,    // Chinese clone claiming v2.3
    ctSTN1xxx,            // OBDLink STN11xx/STN22xx chips
    ctUnknown             // Unknown or undetected
  );

  /// <summary>
  ///   ELM327 Detection Result
  /// </summary>
  TELM327DetectionResult = record
    ChipType: TELM327ChipType;
    Version: string;
    IsGenuine: Boolean;
    IsClone: Boolean;
    Manufacturer: string;
    FirmwareDate: string;
    SupportedProtocols: string;
    ConfidenceLevel: Integer; // 0-100%
    DetectionMethod: string;
    Quirks: TStringList; // Known quirks/issues with this adapter
    Recommendations: string;
  end;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   ELM327 Adapter Detector
  ///   Detects genuine ELM327 vs Chinese clones and identifies quirks
  /// </summary>
  TELM327Detector = class
  private
    FAdapter: TELM327Adapter;
    FResult: TELM327DetectionResult;
    
    /// <summary>
    ///   Test AT command response patterns
    /// </summary>
    function TestCommandPatterns: Integer;
    
    /// <summary>
    ///   Test timing characteristics
    /// </summary>
    function TestTiming: Integer;
    
    /// <summary>
    ///   Test protocol support
    /// </summary>
    function TestProtocolSupport: Integer;
    
    /// <summary>
    ///   Test voltage reading
    /// </summary>
    function TestVoltageReading: Boolean;
    
    /// <summary>
    ///   Detect based on version string
    /// </summary>
    procedure DetectFromVersion(const Version: string);
    
    /// <summary>
    ///   Identify known quirks
    /// </summary>
    procedure IdentifyQuirks;
    
    /// <summary>
    ///   Generate recommendations
    /// </summary>
    procedure GenerateRecommendations;
  public
    constructor Create(Adapter: TELM327Adapter);
    destructor Destroy; override;
    
    /// <summary>
    ///   Perform full detection
    /// </summary>
    function Detect: TELM327DetectionResult;
    
    /// <summary>
    ///   Quick detection (version string only)
    /// </summary>
    function QuickDetect: TELM327DetectionResult;
    
    /// <summary>
    ///   Get chip type description
    /// </summary>
    class function GetChipTypeDescription(ChipType: TELM327ChipType): string;
  end;

implementation

uses System.DateUtils;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TELM327Detector.Create(Adapter: TELM327Adapter);
begin
  inherited Create;
  FAdapter := Adapter;
  FResult.Quirks := TStringList.Create;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TELM327Detector.Destroy;
begin
  FResult.Quirks.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// TEST COMMAND PATTERNS
//------------------------------------------------------------------------------
function TELM327Detector.TestCommandPatterns: Integer;
var
  Response: string;
  Score: Integer;
begin
  Score := 0;
  
  // Test 1: AT I (Identify) - Genuine chips respond with "ELM327 vX.X"
  Response := FAdapter.WriteATCommandSync('AT I');
  if Pos('ELM327', Response) > 0 then
    Inc(Score, 20)
  else if Pos('OBD', Response) > 0 then
    Inc(Score, 5); // Clone indicator
  
  // Test 2: AT @1 (Device description) - Only genuine supports this
  Response := FAdapter.WriteATCommandSync('AT @1');
  if (Pos('?', Response) = 0) and (Length(Response) > 0) then
    Inc(Score, 30) // Genuine
  else
    Dec(Score, 10); // Clone doesn't support @1
  
  // Test 3: AT @2 (Device identifier) - Only genuine supports this
  Response := FAdapter.WriteATCommandSync('AT @2');
  if (Pos('?', Response) = 0) and (Length(Response) > 0) then
    Inc(Score, 30) // Genuine
  else
    Dec(Score, 10); // Clone doesn't support @2
  
  // Test 4: AT Z (Reset) - Check response pattern
  Response := FAdapter.WriteATCommandSync('AT Z');
  if ContainsText(Response, 'ELM327') then
    Inc(Score, 10);
  
  // Test 5: AT ST FF (Set timeout to maximum) - Clones often don't respect this
  FAdapter.WriteATCommandSync('AT ST FF');
  Response := FAdapter.WriteATCommandSync('AT ST 00'); // Reset
  
  Result := Score;
end;

//------------------------------------------------------------------------------
// TEST TIMING
//------------------------------------------------------------------------------
function TELM327Detector.TestTiming: Integer;
var
  StartTime, EndTime: TDateTime;
  ElapsedMs: Int64;
  Response: string;
  Score: Integer;
begin
  Score := 0;
  
  // Genuine ELM327 has consistent timing, clones are often faster or erratic
  StartTime := Now;
  Response := FAdapter.WriteATCommandSync('AT I');
  EndTime := Now;
  
  ElapsedMs := MilliSecondsBetween(EndTime, StartTime);
  
  // Genuine typically responds in 50-150ms
  if (ElapsedMs >= 50) and (ElapsedMs <= 150) then
    Inc(Score, 20)
  else if ElapsedMs < 20 then
    Dec(Score, 20); // Too fast = likely clone
  
  // Test multiple commands for consistency
  StartTime := Now;
  FAdapter.WriteATCommandSync('AT RV');
  EndTime := Now;
  ElapsedMs := MilliSecondsBetween(EndTime, StartTime);
  
  if (ElapsedMs >= 30) and (ElapsedMs <= 100) then
    Inc(Score, 10);
  
  Result := Score;
end;

//------------------------------------------------------------------------------
// TEST PROTOCOL SUPPORT
//------------------------------------------------------------------------------
function TELM327Detector.TestProtocolSupport: Integer;
var
  Response: string;
  Score: Integer;
begin
  Score := 0;
  
  // Test protocol selection
  Response := FAdapter.WriteATCommandSync('AT SP 0');
  if Pos('OK', Response) > 0 then
    Inc(Score, 10);
  
  // Test protocol description
  Response := FAdapter.WriteATCommandSync('AT DP');
  if Length(Response) > 5 then
    Inc(Score, 10);
  
  // Test adaptive timing (AT AT0, AT AT1, AT AT2)
  Response := FAdapter.WriteATCommandSync('AT AT 1');
  if Pos('OK', Response) > 0 then
    Inc(Score, 5);
  
  Result := Score;
end;

//------------------------------------------------------------------------------
// TEST VOLTAGE READING
//------------------------------------------------------------------------------
function TELM327Detector.TestVoltageReading: Boolean;
var
  Response: string;
  Voltage: Single;
begin
  Result := False;
  
  Response := FAdapter.WriteATCommandSync('AT RV');
  
  // Try to parse voltage (format: "12.5V")
  Response := Trim(Response);
  if (Length(Response) > 0) and (Pos('V', Response) > 0) then
  begin
    Response := StringReplace(Response, 'V', '', [rfReplaceAll]);
    Result := TryStrToFloat(Response, Voltage) and (Voltage >= 8.0) and (Voltage <= 16.0);
  end;
end;

//------------------------------------------------------------------------------
// DETECT FROM VERSION
//------------------------------------------------------------------------------
procedure TELM327Detector.DetectFromVersion(const Version: string);
begin
  FResult.Version := Version;
  
  // Analyze version string for clone indicators
  if ContainsText(Version, 'ELM327') then
  begin
    // Extract version number
    if ContainsText(Version, 'v1.0') or ContainsText(Version, 'v1.1') then
    begin
      FResult.ChipType := ctChineseCloneV12;
      FResult.IsClone := True;
      FResult.Manufacturer := 'Chinese Clone';
    end
    else if ContainsText(Version, 'v1.2') then
    begin
      FResult.ChipType := ctChineseCloneV12;
      FResult.IsClone := True;
      FResult.Manufacturer := 'Chinese Clone (v1.2)';
    end
    else if ContainsText(Version, 'v1.3') then
    begin
      FResult.ChipType := ctChineseCloneV13;
      FResult.IsClone := True;
      FResult.Manufacturer := 'Chinese Clone (v1.3)';
    end
    else if ContainsText(Version, 'v1.4') then
    begin
      FResult.ChipType := ctChineseCloneV14;
      FResult.IsClone := True;
      FResult.Manufacturer := 'Chinese Clone (v1.4)';
    end
    else if ContainsText(Version, 'v1.5') then
    begin
      FResult.ChipType := ctChineseCloneV15;
      FResult.IsClone := True;
      FResult.Manufacturer := 'Possible Chinese Clone (v1.5)';
    end
    else if ContainsText(Version, 'v2.1') then
    begin
      FResult.ChipType := ctChineseCloneV21;
      FResult.IsClone := True;
      FResult.Manufacturer := 'Chinese Clone (v2.1) - FAKE VERSION';
    end
    else if ContainsText(Version, 'v2.2') or ContainsText(Version, 'v2.3') then
    begin
      FResult.ChipType := ctChineseCloneV23;
      FResult.IsClone := True;
      FResult.Manufacturer := 'Chinese Clone (v2.x) - FAKE VERSION';
    end;
  end
  else if ContainsText(Version, 'STN') then
  begin
    FResult.ChipType := ctSTN1xxx;
    FResult.IsGenuine := True;
    FResult.Manufacturer := 'OBDLink STN';
  end;
  
  // Note: Genuine ELM327 never went beyond v2.3, and v2.x are rare
  // Most v2.x claims are clones
end;

//------------------------------------------------------------------------------
// IDENTIFY QUIRKS
//------------------------------------------------------------------------------
procedure TELM327Detector.IdentifyQuirks;
begin
  FResult.Quirks.Clear;
  
  case FResult.ChipType of
    ctChineseCloneV12, ctChineseCloneV13:
    begin
      FResult.Quirks.Add('May not support all AT commands');
      FResult.Quirks.Add('Timing may be inconsistent');
      FResult.Quirks.Add('May freeze on certain protocols');
      FResult.Quirks.Add('Limited or no J1850 VPW support');
    end;
    
    ctChineseCloneV14, ctChineseCloneV15:
    begin
      FResult.Quirks.Add('Improved over v1.2/v1.3 but still has issues');
      FResult.Quirks.Add('May have CAN filtering problems');
      FResult.Quirks.Add('Adaptive timing may not work correctly');
    end;
    
    ctChineseCloneV21, ctChineseCloneV22, ctChineseCloneV23:
    begin
      FResult.Quirks.Add('FAKE VERSION - No official v2.x ELM327 exists');
      FResult.Quirks.Add('Based on v1.x clone with modified version string');
      FResult.Quirks.Add('May have unpredictable behavior');
      FResult.Quirks.Add('Extended commands (@1, @2, @3) not supported');
    end;
    
    ctGenuineELM327:
    begin
      FResult.Quirks.Add('None - Genuine chip');
    end;
    
    ctSTN1xxx:
    begin
      FResult.Quirks.Add('Enhanced features beyond ELM327');
      FResult.Quirks.Add('Additional ST commands available');
    end;
  end;
end;

//------------------------------------------------------------------------------
// GENERATE RECOMMENDATIONS
//------------------------------------------------------------------------------
procedure TELM327Detector.GenerateRecommendations;
begin
  if FResult.IsClone then
  begin
    FResult.Recommendations := 
      'Chinese clone detected. Recommendations:'#13#10 +
      '- Reduce baud rate to 38400 for better stability'#13#10 +
      '- Increase timeout values (AT ST FF)'#13#10 +
      '- Avoid adaptive timing (AT AT0)'#13#10 +
      '- Use simple commands and avoid advanced features'#13#10 +
      '- Consider upgrading to genuine ELM327 or OBDLink adapter for better reliability';
  end
  else if FResult.ChipType = ctSTN1xxx then
  begin
    FResult.Recommendations :=
      'OBDLink STN chip detected. Recommendations:'#13#10 +
      '- Use ST commands for enhanced features'#13#10 +
      '- Enable automatic protocol selection'#13#10 +
      '- Take advantage of faster response times';
  end
  else
  begin
    FResult.Recommendations :=
      'Genuine ELM327 detected.'#13#10 +
      '- All features fully supported'#13#10 +
      '- Use standard configuration';
  end;
end;

//------------------------------------------------------------------------------
// DETECT
//------------------------------------------------------------------------------
function TELM327Detector.Detect: TELM327DetectionResult;
var
  CommandScore, TimingScore, ProtocolScore: Integer;
  TotalScore: Integer;
  Version: string;
  VoltageOK: Boolean;
begin
  // Initialize result
  FResult.ChipType := ctUnknown;
  FResult.IsGenuine := False;
  FResult.IsClone := False;
  FResult.ConfidenceLevel := 0;
  FResult.Quirks.Clear;
  
  if not FAdapter.Connected then
  begin
    FResult.DetectionMethod := 'Not connected';
    Result := FResult;
    Exit;
  end;
  
  // Get version string
  Version := FAdapter.WriteATCommandSync('AT I');
  DetectFromVersion(Version);
  
  // Run detection tests
  CommandScore := TestCommandPatterns;
  TimingScore := TestTiming;
  ProtocolScore := TestProtocolSupport;
  VoltageOK := TestVoltageReading;
  
  TotalScore := CommandScore + TimingScore + ProtocolScore;
  if VoltageOK then
    Inc(TotalScore, 10);
  
  // Determine if genuine based on score
  if TotalScore >= 80 then
  begin
    FResult.IsGenuine := True;
    FResult.ChipType := ctGenuineELM327;
    FResult.Manufacturer := 'Elm Electronics (Genuine)';
    FResult.ConfidenceLevel := Min(100, TotalScore);
  end
  else if TotalScore >= 40 then
  begin
    FResult.ConfidenceLevel := Min(80, TotalScore);
    // Keep clone type from version detection
  end
  else
  begin
    FResult.ChipType := ctUnknown;
    FResult.ConfidenceLevel := TotalScore;
  end;
  
  FResult.DetectionMethod := Format('Command:%d, Timing:%d, Protocol:%d', 
    [CommandScore, TimingScore, ProtocolScore]);
  
  IdentifyQuirks;
  GenerateRecommendations;
  
  Result := FResult;
end;

//------------------------------------------------------------------------------
// QUICK DETECT
//------------------------------------------------------------------------------
function TELM327Detector.QuickDetect: TELM327DetectionResult;
var
  Version: string;
begin
  FResult.Quirks.Clear;
  
  if not FAdapter.Connected then
  begin
    FResult.DetectionMethod := 'Quick detect - Not connected';
    Result := FResult;
    Exit;
  end;
  
  Version := FAdapter.WriteATCommandSync('AT I');
  DetectFromVersion(Version);
  
  FResult.DetectionMethod := 'Quick detect - Version string only';
  FResult.ConfidenceLevel := 50; // Lower confidence without full tests
  
  IdentifyQuirks;
  GenerateRecommendations;
  
  Result := FResult;
end;

//------------------------------------------------------------------------------
// GET CHIP TYPE DESCRIPTION
//------------------------------------------------------------------------------
class function TELM327Detector.GetChipTypeDescription(ChipType: TELM327ChipType): string;
begin
  case ChipType of
    ctGenuineELM327:      Result := 'Genuine Elm Electronics ELM327';
    ctChineseCloneV12:    Result := 'Chinese Clone (v1.2)';
    ctChineseCloneV13:    Result := 'Chinese Clone (v1.3)';
    ctChineseCloneV14:    Result := 'Chinese Clone (v1.4)';
    ctChineseCloneV15:    Result := 'Chinese Clone (v1.5)';
    ctChineseCloneV21:    Result := 'Chinese Clone (FAKE v2.1)';
    ctChineseCloneV22:    Result := 'Chinese Clone (FAKE v2.2)';
    ctChineseCloneV23:    Result := 'Chinese Clone (FAKE v2.3)';
    ctSTN1xxx:            Result := 'OBDLink STN11xx/STN22xx';
    ctUnknown:            Result := 'Unknown';
  else
    Result := 'Unidentified';
  end;
end;

end.
