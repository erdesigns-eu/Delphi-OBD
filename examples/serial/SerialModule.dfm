object SerialDashboardForm: TSerialDashboardForm
  Caption = 'Serial Dashboard'
  ClientHeight = 480
  ClientWidth = 800
  Position = poScreenCenter
  TextHeight = 15
  object Header: TOBDTouchHeader
    Align = alTop
    Height = 72
    Caption = 'Serial Demo'
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
    TabOrder = 3
  end
  object ControlPanel: TPanel
    Align = alBottom
    Height = 48
    TabOrder = 2
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
  object Gauge: TOBDCircularGauge
    Align = alClient
    TabOrder = 4
    Value = 0
  end
  object ConnectionComponent: TOBDConnectionComponent
    ConnectionType = ctSerial
    SerialPort = 'COM1'
    SerialBaudRate = br38400
    Left = 688
    Top = 72
  end
  object ProtocolComponent: TOBDProtocolComponent
    ConnectionComponent = ConnectionComponent
    ProtocolClass = TOBDProtocolISO9141
    AutoBindConnection = True
    Left = 688
    Top = 120
  end
  object HeaderComponent: TOBDHeaderComponent
    Header = Header
    ConnectionComponent = ConnectionComponent
    AutoBindConnection = True
    AutoApplyCaption = True
    AutoApplyBattery = True
    Left = 688
    Top = 168
  end
  object SubheaderComponent: TOBDSubheaderComponent
    Subheader = Subheader
    ConnectionComponent = ConnectionComponent
    ProtocolComponent = ProtocolComponent
    AutoBindConnection = True
    AutoBindProtocol = True
    AutoApplyConnectionCaptions = True
    AutoApplyProtocolCaption = True
    Left = 688
    Top = 216
  end
  object GaugeComponent: TOBDGaugeComponent
    Gauge = Gauge
    ProtocolComponent = ProtocolComponent
    AutoBindProtocol = True
    AutoApplyValue = True
    OnResolveValue = ResolveGaugeValue
    Left = 688
    Top = 264
  end
end
