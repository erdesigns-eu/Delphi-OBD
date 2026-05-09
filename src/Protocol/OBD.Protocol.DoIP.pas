//------------------------------------------------------------------------------
//  OBD.Protocol.DoIP
//
//  Facade unit for the DoIP stack. Re-exports the public types from
//  OBD.Protocol.DoIP.Header / .Messages / .Transport / .Client so a
//  host needs to add only a single unit to its <c>uses</c> clause.
//
//  The OpenSSL TLS plug lives in OBD.Protocol.DoIP.TLS.OpenSSL and
//  is intentionally <i>not</i> re-exported here so hosts that don't
//  ship the OpenSSL DLLs are not forced to take that dependency.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 4d initial.
//------------------------------------------------------------------------------

unit OBD.Protocol.DoIP;

interface

uses
  OBD.Protocol.DoIP.Header,
  OBD.Protocol.DoIP.Messages,
  OBD.Protocol.DoIP.Transport,
  OBD.Protocol.DoIP.Client;

type
  /// <summary>Re-export of <see cref="OBD.Protocol.DoIP.Header.TOBDDoIPHeader"/>.</summary>
  TOBDDoIPHeader = OBD.Protocol.DoIP.Header.TOBDDoIPHeader;

  TOBDDoIPRoutingActivationRequest = OBD.Protocol.DoIP.Messages.TOBDDoIPRoutingActivationRequest;
  TOBDDoIPRoutingActivationResponse = OBD.Protocol.DoIP.Messages.TOBDDoIPRoutingActivationResponse;
  TOBDDoIPVehicleAnnouncement = OBD.Protocol.DoIP.Messages.TOBDDoIPVehicleAnnouncement;
  TOBDDoIPVehicleIDRequestEID = OBD.Protocol.DoIP.Messages.TOBDDoIPVehicleIDRequestEID;
  TOBDDoIPVehicleIDRequestVIN = OBD.Protocol.DoIP.Messages.TOBDDoIPVehicleIDRequestVIN;
  TOBDDoIPDiagnosticMessage = OBD.Protocol.DoIP.Messages.TOBDDoIPDiagnosticMessage;
  TOBDDoIPDiagnosticAck = OBD.Protocol.DoIP.Messages.TOBDDoIPDiagnosticAck;
  TOBDDoIPAliveCheckResponse = OBD.Protocol.DoIP.Messages.TOBDDoIPAliveCheckResponse;
  TOBDDoIPEntityStatusResponse = OBD.Protocol.DoIP.Messages.TOBDDoIPEntityStatusResponse;
  TOBDDoIPPowerModeResponse = OBD.Protocol.DoIP.Messages.TOBDDoIPPowerModeResponse;
  TOBDDoIPCodec = OBD.Protocol.DoIP.Messages.TOBDDoIPCodec;

  IOBDDoIPTransport = OBD.Protocol.DoIP.Transport.IOBDDoIPTransport;
  TOBDDoIPPlainTransport = OBD.Protocol.DoIP.Transport.TOBDDoIPPlainTransport;

  TOBDDoIPClient = OBD.Protocol.DoIP.Client.TOBDDoIPClient;
  TOBDDoIPClientStatus = OBD.Protocol.DoIP.Client.TOBDDoIPClientStatus;

implementation

end.
