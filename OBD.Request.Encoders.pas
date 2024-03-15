//------------------------------------------------------------------------------
// UNIT           : OBD.Request.Encoders.pas
// CONTENTS       : OBD Request Encoders
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 15/03/2024
//------------------------------------------------------------------------------
unit OBD.Request.Encoders;

interface

uses
  System.SysUtils,

  OBD.Request.Constants;

//------------------------------------------------------------------------------
// INTERFACES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Service Request Encoder (INTERFACE)
  /// </summary>
  IOBDServiceRequestEncoder = interface
    ['{C044D8B6-163A-48CA-A221-4DFF64E5B4EF}']
    /// <summary>
    ///   Get service PID (01 - 0A)
    /// </summary>
    function GetServicePID: Byte;
    /// <summary>
    ///   Encode service request
    /// </summary>
    function EncodeServiceRequest(PID: Byte): string; overload;
    /// <summary>
    ///   Encode service request (with aditional data)
    /// </summary>
    function EncodeServiceRequest(PID: Byte; Data: TBytes): string; overload;
  end;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Service Request Encoder (CLASS)
  /// </summary>
  TOBDServiceRequestEncoder = class(TInterfacedObject, IOBDServiceRequestEncoder)
  protected
    /// <summary>
    ///   Get service PID (01 - 0A)
    /// </summary>
    function GetServicePID: Byte; virtual; abstract;
  public
    /// <summary>
    ///   Encode service request
    /// </summary>
    function EncodeServiceRequest(PID: Byte): string; overload; virtual;
    /// <summary>
    ///   Encode service request (with aditional data)
    /// </summary>
    function EncodeServiceRequest(PID: Byte; Data: TBytes): string; overload; virtual;
  end;

  /// <summary>
  ///   OBD Service 01 Request Encoder
  /// </summary>
  TOBDService01RequestEncoder = class(TOBDServiceRequestEncoder)
  protected
    /// <summary>
    ///   Get service PID (01)
    /// </summary>
    function GetServicePID: Byte; override;
  end;

  /// <summary>
  ///   OBD Service 02 Request Encoder
  /// </summary>
  TOBDService02RequestEncoder = class(TOBDServiceRequestEncoder)
  protected
    /// <summary>
    ///   Get service PID (02)
    /// </summary>
    function GetServicePID: Byte; override;
  end;

  /// <summary>
  ///   OBD Service 03 Request Encoder
  /// </summary>
  TOBDService03RequestEncoder = class(TOBDServiceRequestEncoder)
  protected
    /// <summary>
    ///   Get service PID (03)
    /// </summary>
    function GetServicePID: Byte; override;
  end;

  /// <summary>
  ///   OBD Service 04 Request Encoder
  /// </summary>
  TOBDService04RequestEncoder = class(TOBDServiceRequestEncoder)
  protected
    /// <summary>
    ///   Get service PID (04)
    /// </summary>
    function GetServicePID: Byte; override;
  end;

  /// <summary>
  ///   OBD Service 05 Request Encoder
  /// </summary>
  TOBDService05RequestEncoder = class(TOBDServiceRequestEncoder)
  protected
    /// <summary>
    ///   Get service PID (05)
    /// </summary>
    function GetServicePID: Byte; override;
  end;

  /// <summary>
  ///   OBD Service 06 Request Encoder
  /// </summary>
  TOBDService06RequestEncoder = class(TOBDServiceRequestEncoder)
  protected
    /// <summary>
    ///   Get service PID (06)
    /// </summary>
    function GetServicePID: Byte; override;
  end;

  /// <summary>
  ///   OBD Service 07 Request Encoder
  /// </summary>
  TOBDService07RequestEncoder = class(TOBDServiceRequestEncoder)
  protected
    /// <summary>
    ///   Get service PID (07)
    /// </summary>
    function GetServicePID: Byte; override;
  end;

  /// <summary>
  ///   OBD Service 08 Request Encoder
  /// </summary>
  TOBDService08RequestEncoder = class(TOBDServiceRequestEncoder)
  protected
    /// <summary>
    ///   Get service PID (08)
    /// </summary>
    function GetServicePID: Byte; override;
  end;

  /// <summary>
  ///   OBD Service 09 Request Encoder
  /// </summary>
  TOBDService09RequestEncoder = class(TOBDServiceRequestEncoder)
  protected
    /// <summary>
    ///   Get service PID (09)
    /// </summary>
    function GetServicePID: Byte; override;
  end;

  /// <summary>
  ///   OBD Service 0A Request Encoder
  /// </summary>
  TOBDService0ARequestEncoder = class(TOBDServiceRequestEncoder)
  protected
    /// <summary>
    ///   Get service PID (0A)
    /// </summary>
    function GetServicePID: Byte; override;
  end;

implementation

//------------------------------------------------------------------------------
// ENCODE SERVICE REQUEST
//------------------------------------------------------------------------------
function TOBDServiceRequestEncoder.EncodeServiceRequest(PID: Byte): string;
begin
  // Format the request
  Result := Format('%02X%02X', [GetServicePID, PID]);
end;

//------------------------------------------------------------------------------
// ENCODE SERVICE REQUEST (WITH ADDITIONAL DATA)
//------------------------------------------------------------------------------
function TOBDServiceRequestEncoder.EncodeServiceRequest(PID: Byte; Data: TBytes): string;
var
  I: Integer;
  DataStr: String;
begin
  // initialize data string
  DataStr := '';
  // Loop over bytes and convert to hex
  for I := 0 to High(Data) do DataStr := DataStr + IntToHex(Data[I], 2);
  // Format the request
  Result := Format('%02X%02X%s', [GetServicePID, PID, DataStr]);
end;

//------------------------------------------------------------------------------
// GET SERVICE PID (01)
//------------------------------------------------------------------------------
function TOBDService01RequestEncoder.GetServicePID: Byte;
begin
  Result := OBD_SERVICE_01;
end;

//------------------------------------------------------------------------------
// GET SERVICE PID (02)
//------------------------------------------------------------------------------
function TOBDService02RequestEncoder.GetServicePID: Byte;
begin
  Result := OBD_SERVICE_02;
end;

//------------------------------------------------------------------------------
// GET SERVICE PID (03)
//------------------------------------------------------------------------------
function TOBDService03RequestEncoder.GetServicePID: Byte;
begin
  Result := OBD_SERVICE_03;
end;

//------------------------------------------------------------------------------
// GET SERVICE PID (04)
//------------------------------------------------------------------------------
function TOBDService04RequestEncoder.GetServicePID: Byte;
begin
  Result := OBD_SERVICE_04;
end;

//------------------------------------------------------------------------------
// GET SERVICE PID (05)
//------------------------------------------------------------------------------
function TOBDService05RequestEncoder.GetServicePID: Byte;
begin
  Result := OBD_SERVICE_05;
end;

//------------------------------------------------------------------------------
// GET SERVICE PID (06)
//------------------------------------------------------------------------------
function TOBDService06RequestEncoder.GetServicePID: Byte;
begin
  Result := OBD_SERVICE_06;
end;

//------------------------------------------------------------------------------
// GET SERVICE PID (07)
//------------------------------------------------------------------------------
function TOBDService07RequestEncoder.GetServicePID: Byte;
begin
  Result := OBD_SERVICE_07;
end;

//------------------------------------------------------------------------------
// GET SERVICE PID (08)
//------------------------------------------------------------------------------
function TOBDService08RequestEncoder.GetServicePID: Byte;
begin
  Result := OBD_SERVICE_08;
end;

//------------------------------------------------------------------------------
// GET SERVICE PID (09)
//------------------------------------------------------------------------------
function TOBDService09RequestEncoder.GetServicePID: Byte;
begin
  Result := OBD_SERVICE_09;
end;

//------------------------------------------------------------------------------
// GET SERVICE PID (0A)
//------------------------------------------------------------------------------
function TOBDService0ARequestEncoder.GetServicePID: Byte;
begin
  Result := OBD_SERVICE_0A;
end;

end.
