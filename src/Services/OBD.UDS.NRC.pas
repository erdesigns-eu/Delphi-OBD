//------------------------------------------------------------------------------
// UNIT           : OBD.UDS.NRC.pas
// CONTENTS       : ISO 14229-1 §A.1 Negative Response Code (NRC) catalog.
//                : Maps each 0x10..0x9F NRC byte to its short name,
//                : description, and standardised category. Production
//                : code uses DescribeNRC(Byte) as the canonical formatter
//                : everywhere the wire layer surfaces a NRC value.
//
// Spec ref       : ISO 14229-1:2020 Annex A — Diagnostic Service / NRC.
//                : Spec is public; the table below mirrors §A.1 verbatim.
//------------------------------------------------------------------------------
unit OBD.UDS.NRC;

interface

uses
  System.SysUtils;

type
  TOBDUDSNrcCategory = (
    nrcGeneral,
    nrcSecurity,
    nrcRequestData,
    nrcCondition,
    nrcServer,
    nrcReserved
  );

  TOBDUDSNrcInfo = record
    Code: Byte;
    ShortName: string;       // e.g. 'GR', 'SAS', 'ROOR'
    Description: string;     // ISO 14229-1 prose
    Category: TOBDUDSNrcCategory;
  end;

/// <summary>Look up an NRC. Unknown / reserved codes return a record
/// with category=nrcReserved and a synthetic description; never raises.
/// </summary>
function DescribeNRC(NRC: Byte): TOBDUDSNrcInfo;

/// <summary>One-line formatter convenient for log lines and exception
/// messages: "NRC 0x33 (SAD: securityAccessDenied)".</summary>
function FormatNRC(NRC: Byte): string;

/// <summary>True if the byte is in a category clients should retry
/// (busy / repeat-request, conditions-not-correct).</summary>
function IsTransientNRC(NRC: Byte): Boolean;

implementation

function NewInfo(Code: Byte; const Short, Desc: string;
  Cat: TOBDUDSNrcCategory): TOBDUDSNrcInfo;
begin
  Result.Code := Code;
  Result.ShortName := Short;
  Result.Description := Desc;
  Result.Category := Cat;
end;

function DescribeNRC(NRC: Byte): TOBDUDSNrcInfo;
begin
  case NRC of
    $00: Result := NewInfo($00, 'PR',   'positiveResponse',                                   nrcGeneral);
    $10: Result := NewInfo($10, 'GR',   'generalReject',                                       nrcGeneral);
    $11: Result := NewInfo($11, 'SNS',  'serviceNotSupported',                                 nrcGeneral);
    $12: Result := NewInfo($12, 'SFNS', 'subFunctionNotSupported',                             nrcGeneral);
    $13: Result := NewInfo($13, 'IMLOIF','incorrectMessageLengthOrInvalidFormat',              nrcGeneral);
    $14: Result := NewInfo($14, 'RTL',  'responseTooLong',                                     nrcGeneral);
    $21: Result := NewInfo($21, 'BRR',  'busyRepeatRequest',                                   nrcCondition);
    $22: Result := NewInfo($22, 'CNC',  'conditionsNotCorrect',                                nrcCondition);
    $24: Result := NewInfo($24, 'RSE',  'requestSequenceError',                                nrcCondition);
    $25: Result := NewInfo($25, 'NRFSC','noResponseFromSubnetComponent',                       nrcServer);
    $26: Result := NewInfo($26, 'FPEORA','failurePreventsExecutionOfRequestedAction',          nrcServer);
    $31: Result := NewInfo($31, 'ROOR', 'requestOutOfRange',                                   nrcRequestData);
    $33: Result := NewInfo($33, 'SAD',  'securityAccessDenied',                                nrcSecurity);
    $34: Result := NewInfo($34, 'AR',   'authenticationRequired',                              nrcSecurity);
    $35: Result := NewInfo($35, 'IK',   'invalidKey',                                          nrcSecurity);
    $36: Result := NewInfo($36, 'ENOA', 'exceededNumberOfAttempts',                            nrcSecurity);
    $37: Result := NewInfo($37, 'RTDNE','requiredTimeDelayNotExpired',                         nrcSecurity);
    $38: Result := NewInfo($38, 'SDTR', 'secureDataTransmissionRequired',                      nrcSecurity);
    $39: Result := NewInfo($39, 'SDTNA','secureDataTransmissionNotAllowed',                    nrcSecurity);
    $3A: Result := NewInfo($3A, 'SDVF', 'secureDataVerificationFailed',                        nrcSecurity);
    $50: Result := NewInfo($50, 'CVFITP','certificateVerificationFailed_InvalidTimePeriod',    nrcSecurity);
    $51: Result := NewInfo($51, 'CVFIS','certificateVerificationFailed_InvalidSignature',      nrcSecurity);
    $52: Result := NewInfo($52, 'CVFITC','certificateVerificationFailed_InvalidChainOfTrust',  nrcSecurity);
    $53: Result := NewInfo($53, 'CVFIT','certificateVerificationFailed_InvalidType',           nrcSecurity);
    $54: Result := NewInfo($54, 'CVFIF','certificateVerificationFailed_InvalidFormat',         nrcSecurity);
    $55: Result := NewInfo($55, 'CVFIC','certificateVerificationFailed_InvalidContent',        nrcSecurity);
    $56: Result := NewInfo($56, 'CVFIS2','certificateVerificationFailed_InvalidScope',         nrcSecurity);
    $57: Result := NewInfo($57, 'CVFIC2','certificateVerificationFailed_InvalidCertificate',   nrcSecurity);
    $58: Result := NewInfo($58, 'OVF',  'ownershipVerificationFailed',                         nrcSecurity);
    $59: Result := NewInfo($59, 'CCF',  'challengeCalculationFailed',                          nrcSecurity);
    $5A: Result := NewInfo($5A, 'SARF', 'settingAccessRightsFailed',                           nrcSecurity);
    $5B: Result := NewInfo($5B, 'SKDF', 'sessionKeyCreation/DerivationFailed',                 nrcSecurity);
    $5C: Result := NewInfo($5C, 'CDUF', 'configurationDataUsageFailed',                        nrcSecurity);
    $5D: Result := NewInfo($5D, 'DVFAA','deAuthenticationFailed',                              nrcSecurity);
    $70: Result := NewInfo($70, 'UDNA', 'uploadDownloadNotAccepted',                           nrcServer);
    $71: Result := NewInfo($71, 'TDS',  'transferDataSuspended',                               nrcServer);
    $72: Result := NewInfo($72, 'GPF',  'generalProgrammingFailure',                           nrcServer);
    $73: Result := NewInfo($73, 'WBSC', 'wrongBlockSequenceCounter',                           nrcServer);
    $78: Result := NewInfo($78, 'RCRRP','requestCorrectlyReceived-ResponsePending',            nrcCondition);
    $7E: Result := NewInfo($7E, 'SFNSIAS','subFunctionNotSupportedInActiveSession',            nrcCondition);
    $7F: Result := NewInfo($7F, 'SNSIAS','serviceNotSupportedInActiveSession',                 nrcCondition);
    $81: Result := NewInfo($81, 'RPMTH','rpmTooHigh',                                          nrcCondition);
    $82: Result := NewInfo($82, 'RPMTL','rpmTooLow',                                           nrcCondition);
    $83: Result := NewInfo($83, 'EIR',  'engineIsRunning',                                     nrcCondition);
    $84: Result := NewInfo($84, 'EINR', 'engineIsNotRunning',                                  nrcCondition);
    $85: Result := NewInfo($85, 'ERTTL','engineRunTimeTooLow',                                 nrcCondition);
    $86: Result := NewInfo($86, 'TEMPTH','temperatureTooHigh',                                 nrcCondition);
    $87: Result := NewInfo($87, 'TEMPTL','temperatureTooLow',                                  nrcCondition);
    $88: Result := NewInfo($88, 'VSTH', 'vehicleSpeedTooHigh',                                 nrcCondition);
    $89: Result := NewInfo($89, 'VSTL', 'vehicleSpeedTooLow',                                  nrcCondition);
    $8A: Result := NewInfo($8A, 'TPTH', 'throttle/PedalTooHigh',                               nrcCondition);
    $8B: Result := NewInfo($8B, 'TPTL', 'throttle/PedalTooLow',                                nrcCondition);
    $8C: Result := NewInfo($8C, 'TRNIN','transmissionRangeNotInNeutral',                       nrcCondition);
    $8D: Result := NewInfo($8D, 'TRNIG','transmissionRangeNotInGear',                          nrcCondition);
    $8F: Result := NewInfo($8F, 'BSNC', 'brakeSwitch(es)NotClosed (Brake Pedal not pressed or not applied)', nrcCondition);
    $90: Result := NewInfo($90, 'SLNIP','shifterLeverNotInPark',                               nrcCondition);
    $91: Result := NewInfo($91, 'TCCL', 'torqueConverterClutchLocked',                         nrcCondition);
    $92: Result := NewInfo($92, 'VTH',  'voltageTooHigh',                                      nrcCondition);
    $93: Result := NewInfo($93, 'VTL',  'voltageTooLow',                                       nrcCondition);
    $94: Result := NewInfo($94, 'RTNT', 'resourceTemporarilyNotAvailable',                     nrcServer);
  else
    Result := NewInfo(NRC,
      Format('NRC_0x%.2x', [NRC]),
      Format('reserved or manufacturer-specific NRC 0x%.2x', [NRC]),
      nrcReserved);
  end;
end;

function FormatNRC(NRC: Byte): string;
var
  Info: TOBDUDSNrcInfo;
begin
  Info := DescribeNRC(NRC);
  Result := Format('NRC 0x%.2x (%s: %s)',
    [NRC, Info.ShortName, Info.Description]);
end;

function IsTransientNRC(NRC: Byte): Boolean;
begin
  Result := (NRC = $21) or (NRC = $22) or (NRC = $78) or (NRC = $94);
end;

end.
