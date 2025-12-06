unit Examples.Minimal.MinimalDashboard;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls,
  OBD.Connection.Component, OBD.Protocol, OBD.Protocol.Component,
  Examples.ServiceDemo;

/// <summary>
///   Minimal dashboard that connects, reads stored diagnostic trouble codes,
///   and clears the MIL using the shared non-visual components.
/// </summary>
/// <remarks>
///   Intended as the smallest runnable sample: one form, connect/disconnect,
///   read codes, and clear them with a live log.
/// </remarks>
TMILDashboardForm = class(TForm)
  ControlPanel: TPanel;
  ConnectButton: TButton;
  DisconnectButton: TButton;
  ReadCodesButton: TButton;
  ClearMILButton: TButton;
  DiagnosticsMemo: TMemo;
  ConnectionComponent: TOBDConnectionComponent;
  ProtocolComponent: TOBDProtocolComponent;
private
  /// <summary>
  ///   Helper that sends mode 03 and mode 04 requests and logs responses.
  /// </summary>
  FServiceDemo: TOBDServiceDemo;
  /// <summary>
  ///   Creates the shared service helper and attaches to protocol messages.
  /// </summary>
  procedure FormCreate(Sender: TObject);
  /// <summary>
  ///   Frees the helper and detaches from protocol events.
  /// </summary>
  procedure FormDestroy(Sender: TObject);
  /// <summary>
  ///   Initiates a connection through the configured transport.
  /// </summary>
  procedure ConnectButtonClick(Sender: TObject);
  /// <summary>
  ///   Disconnects the active transport instance.
  /// </summary>
  procedure DisconnectButtonClick(Sender: TObject);
  /// <summary>
  ///   Requests stored diagnostic trouble codes (mode 03).
  /// </summary>
  procedure ReadCodesButtonClick(Sender: TObject);
  /// <summary>
  ///   Sends a clear command to reset DTCs and the MIL (mode 04).
  /// </summary>
  procedure ClearMILButtonClick(Sender: TObject);
end;

var
  MILDashboardForm: TMILDashboardForm;

implementation

{$R *.dfm}

procedure TMILDashboardForm.ClearMILButtonClick(Sender: TObject);
begin
  FServiceDemo.ClearStoredDTCs;
end;

procedure TMILDashboardForm.ConnectButtonClick(Sender: TObject);
begin
  ConnectionComponent.Connect;
end;

procedure TMILDashboardForm.DisconnectButtonClick(Sender: TObject);
begin
  ConnectionComponent.Disconnect;
end;

procedure TMILDashboardForm.FormCreate(Sender: TObject);
begin
  FServiceDemo := TOBDServiceDemo.Create(ConnectionComponent, ProtocolComponent, DiagnosticsMemo);
  FServiceDemo.Attach;
end;

procedure TMILDashboardForm.FormDestroy(Sender: TObject);
begin
  FServiceDemo.Free;
end;

procedure TMILDashboardForm.ReadCodesButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestStoredDTCs;
end;

end.
