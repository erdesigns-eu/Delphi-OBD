object BluetoothDashboardForm: TBluetoothDashboardForm
  Caption = 'Bluetooth Dashboard'
  ClientHeight = 600
  ClientWidth = 900
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  Position = poScreenCenter
  TextHeight = 15
  object Header: TOBDTouchHeader
    Align = alTop
    Height = 72
    Caption = 'Bluetooth Demo'
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
  object DiagnosticsPanel: TPanel
    Align = alBottom
    Height = 200
    TabOrder = 3
    object DiagnosticButtons: TPanel
      Align = alTop
      Height = 48
      TabOrder = 0
      object MonitorStatusButton: TButton
        Left = 8
        Top = 10
        Width = 90
        Height = 25
        Caption = 'Mode 01'
        TabOrder = 0
        OnClick = MonitorStatusButtonClick
      end
      object FreezeFrameButton: TButton
        Left = 104
        Top = 10
        Width = 90
        Height = 25
        Caption = 'Mode 02'
        TabOrder = 1
        OnClick = FreezeFrameButtonClick
      end
      object StoredDTCButton: TButton
        Left = 200
        Top = 10
        Width = 90
        Height = 25
        Caption = 'Mode 03'
        TabOrder = 2
        OnClick = StoredDTCButtonClick
      end
      object ClearDTCButton: TButton
        Left = 296
        Top = 10
        Width = 90
        Height = 25
        Caption = 'Mode 04'
        TabOrder = 3
        OnClick = ClearDTCButtonClick
      end
      object OxygenSensorButton: TButton
        Left = 392
        Top = 10
        Width = 90
        Height = 25
        Caption = 'Mode 05'
        TabOrder = 4
        OnClick = OxygenSensorButtonClick
      end
      object MonitorTestsButton: TButton
        Left = 488
        Top = 10
        Width = 90
        Height = 25
        Caption = 'Mode 06'
        TabOrder = 5
        OnClick = MonitorTestsButtonClick
      end
      object PendingDTCButton: TButton
        Left = 584
        Top = 10
        Width = 90
        Height = 25
        Caption = 'Mode 07'
        TabOrder = 6
        OnClick = PendingDTCButtonClick
      end
      object ControlSystemButton: TButton
        Left = 680
        Top = 10
        Width = 90
        Height = 25
        Caption = 'Mode 08'
        TabOrder = 7
        OnClick = ControlSystemButtonClick
      end
      object VehicleInfoButton: TButton
        Left = 776
        Top = 10
        Width = 90
        Height = 25
        Caption = 'Mode 09'
        TabOrder = 8
        OnClick = VehicleInfoButtonClick
      end
      object PermanentDTCButton: TButton
        Left = 872
        Top = 10
        Width = 90
        Height = 25
        Caption = 'Mode 0A'
        TabOrder = 9
        OnClick = PermanentDTCButtonClick
      end
    end
    object DiagnosticsMemo: TMemo
      Align = alClient
      TabOrder = 1
    end
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
    TabOrder = 5
    Value = 0
  end
  object ConnectionComponent: TOBDConnectionComponent
    ConnectionType = ctBluetooth
    Left = 784
    Top = 72
  end
  object ProtocolComponent: TOBDProtocolComponent
    ConnectionComponent = ConnectionComponent
    ProtocolClass = TOBDProtocolISO9141
    AutoBindConnection = True
    Left = 784
    Top = 120
  end
  object HeaderComponent: TOBDHeaderComponent
    Header = Header
    ConnectionComponent = ConnectionComponent
    AutoBindConnection = True
    AutoApplyCaption = True
    AutoApplyBattery = True
    Left = 784
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
    Left = 784
    Top = 216
  end
  object GaugeComponent: TOBDGaugeComponent
    Gauge = Gauge
    ProtocolComponent = ProtocolComponent
    AutoBindProtocol = True
    AutoApplyValue = True
    OnResolveValue = ResolveGaugeValue
    Left = 784
    Top = 264
  end
end
