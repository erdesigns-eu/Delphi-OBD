object MILDashboardForm: TMILDashboardForm
  Caption = 'Minimal DTC Reader'
  ClientHeight = 360
  ClientWidth = 520
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  Position = poScreenCenter
  TextHeight = 15
  object ControlPanel: TPanel
    Align = alTop
    Height = 48
    TabOrder = 0
    object ConnectButton: TButton
      Left = 8
      Top = 10
      Width = 90
      Height = 25
      Caption = 'Connect'
      TabOrder = 0
      OnClick = ConnectButtonClick
    end
    object DisconnectButton: TButton
      Left = 104
      Top = 10
      Width = 90
      Height = 25
      Caption = 'Disconnect'
      TabOrder = 1
      OnClick = DisconnectButtonClick
    end
    object ReadCodesButton: TButton
      Left = 200
      Top = 10
      Width = 120
      Height = 25
      Caption = 'Read Codes (03)'
      TabOrder = 2
      OnClick = ReadCodesButtonClick
    end
    object ClearMILButton: TButton
      Left = 328
      Top = 10
      Width = 120
      Height = 25
      Caption = 'Clear MIL (04)'
      TabOrder = 3
      OnClick = ClearMILButtonClick
    end
  end
  object DiagnosticsMemo: TMemo
    Align = alClient
    Lines.Strings = (
      'Click Connect, then Read Codes or Clear MIL to send requests.')
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object ConnectionComponent: TOBDConnectionComponent
    ConnectionType = ctSerial
    SerialPort = 'COM3'
    SerialBaudRate = br115200
    Left = 440
    Top = 80
  end
  object ProtocolComponent: TOBDProtocolComponent
    ConnectionComponent = ConnectionComponent
    ProtocolClass = TOBDProtocolISO9141
    AutoBindConnection = True
    Left = 440
    Top = 136
  end
end
