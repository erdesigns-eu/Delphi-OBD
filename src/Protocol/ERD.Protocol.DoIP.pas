// ------------------------------------------------------------------------------
// ERD.Protocol.DoIP
//
// Facade unit for the DoIP stack. Re-exports the public types from
// ERD.Protocol.DoIP.Header / .Messages / .Transport / .Client so a
// host needs to add only a single unit to its <c>uses</c> clause.
//
// The OpenSSL TLS plug lives in ERD.Protocol.DoIP.TLS.OpenSSL and
// is intentionally <i>not</i> re-exported here so hosts that don't
// ship the OpenSSL DLLs are not forced to take that dependency.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : MIT — see LICENSE
//
// History     :
// 2026-05-09  ERD  Initial implementation.
// ------------------------------------------------------------------------------

unit ERD.Protocol.DoIP;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
  ERD.Protocol.DoIP.Header,
  ERD.Protocol.DoIP.Messages,
  ERD.Protocol.DoIP.Transport,
  ERD.Protocol.DoIP.Client;

type
  /// <summary>Re-export of <see cref="ERD.Protocol.DoIP.Header.TOBDDoIPHeader"/>.</summary>
  TOBDDoIPHeader = ERD.Protocol.DoIP.Header.TOBDDoIPHeader;

  TOBDDoIPRoutingActivationRequest = ERD.Protocol.DoIP.Messages.
    TOBDDoIPRoutingActivationRequest;
  TOBDDoIPRoutingActivationResponse = ERD.Protocol.DoIP.Messages.
    TOBDDoIPRoutingActivationResponse;
  TOBDDoIPVehicleAnnouncement = ERD.Protocol.DoIP.Messages.
    TOBDDoIPVehicleAnnouncement;
  TOBDDoIPVehicleIDRequestEID = ERD.Protocol.DoIP.Messages.
    TOBDDoIPVehicleIDRequestEID;
  TOBDDoIPVehicleIDRequestVIN = ERD.Protocol.DoIP.Messages.
    TOBDDoIPVehicleIDRequestVIN;
  TOBDDoIPDiagnosticMessage = ERD.Protocol.DoIP.Messages.
    TOBDDoIPDiagnosticMessage;
  TOBDDoIPDiagnosticAck = ERD.Protocol.DoIP.Messages.TOBDDoIPDiagnosticAck;
  TOBDDoIPAliveCheckResponse = ERD.Protocol.DoIP.Messages.
    TOBDDoIPAliveCheckResponse;
  TOBDDoIPEntityStatusResponse = ERD.Protocol.DoIP.Messages.
    TOBDDoIPEntityStatusResponse;
  TOBDDoIPPowerModeResponse = ERD.Protocol.DoIP.Messages.
    TOBDDoIPPowerModeResponse;
  TOBDDoIPCodec = ERD.Protocol.DoIP.Messages.TOBDDoIPCodec;

  IOBDDoIPTransport = ERD.Protocol.DoIP.Transport.IOBDDoIPTransport;
  TOBDDoIPPlainTransport = ERD.Protocol.DoIP.Transport.TOBDDoIPPlainTransport;

  TOBDDoIPClient = ERD.Protocol.DoIP.Client.TOBDDoIPClient;
  TOBDDoIPClientStatus = ERD.Protocol.DoIP.Client.TOBDDoIPClientStatus;

implementation

end.
