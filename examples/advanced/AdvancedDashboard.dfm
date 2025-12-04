object AdvancedDashboardForm: TAdvancedDashboardForm
  Caption = 'Advanced Dashboard'
  ClientHeight = 560
  ClientWidth = 900
  Position = poScreenCenter
  TextHeight = 15
  object Header: TOBDTouchHeader
    Align = alTop
    Height = 72
    Caption = 'Advanced Demo'
    ParentBackground = False
    TabOrder = 0
  end
  object Subheader: TOBDTouchSubheader
    Align = alTop
    Height = 52
    Caption = 'VCI: pending'
    ProtocolCaption = 'Protocol: pending'
    ParentBackground = False
    TabOrder = 1
  end
  object Statusbar: TOBDTouchStatusbar
    Align = alBottom
    Height = 32
    ParentBackground = False
    TabOrder = 4
  end
  object ControlPanel: TPanel
    Align = alBottom
    Height = 48
    TabOrder = 3
    object ConnectButton: TButton
      Left = 8
      Top = 10
      Width = 120
      Height = 25
      Caption = 'Connect'
      TabOrder = 0
      OnClick = ConnectButtonClick
    end
    object DisconnectButton: TButton
      Left = 136
      Top = 10
      Width = 120
      Height = 25
      Caption = 'Disconnect'
      TabOrder = 1
      OnClick = DisconnectButtonClick
    end
  end
  object DiagnosticsMemo: TMemo
    Align = alRight
    Width = 260
    TabOrder = 2
  end
  object Gauge: TOBDCircularGauge
    Align = alClient
    TabOrder = 5
    Value = 0
  end
  object ConnectionComponent: TOBDConnectionComponent
    ConnectionType = ctSerial
    SerialPort = 'COM3'
    SerialBaudRate = br115200
    Left = 784
    Top = 88
  end
  object ProtocolComponent: TOBDProtocolComponent
    ConnectionComponent = ConnectionComponent
    ProtocolClass = TOBDProtocolISO15765
    AutoBindConnection = True
    DiagnosticsDepth = 200
    OnDiagnosticsUpdated = DiagnosticsUpdated
    Left = 784
    Top = 136
  end
  object HeaderComponent: TOBDHeaderComponent
    Header = Header
    ConnectionComponent = ConnectionComponent
    AutoBindConnection = True
    AutoApplyCaption = True
    AutoApplyBattery = True
    Left = 784
    Top = 184
  end
  object SubheaderComponent: TOBDSubheaderComponent
    Subheader = Subheader
    ConnectionComponent = ConnectionComponent
    ProtocolComponent = ProtocolComponent
    AutoBindConnection = True
    AutoBindProtocol = True
    AutoApplyConnectionCaptions = True
    AutoApplyProtocolCaption = True
    Left = 784
    Top = 232
  end
  object GaugeComponent: TOBDGaugeComponent
    Gauge = Gauge
    ProtocolComponent = ProtocolComponent
    AutoBindProtocol = True
    AutoApplyValue = True
    OnResolveValue = ResolveGaugeValue
    Left = 784
    Top = 280
  end
end
