object FormDoIPMain: TFormDoIPMain
  Left = 0
  Top = 0
  Caption = 'DoIP (Diagnostics over IP) Example'
  ClientHeight = 600
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 800
    Height = 60
    Align = alTop
    BevelOuter = bvNone
    Color = clNavy
    ParentBackground = False
    TabOrder = 0
    object LabelTitle: TLabel
      Left = 16
      Top = 10
      Width = 361
      Height = 23
      Caption = 'DoIP (Diagnostics over IP) Example'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object LabelInfo: TLabel
      Left = 16
      Top = 36
      Width = 115
      Height = 13
      Caption = 'Status: Disconnected'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
  end
  object GroupBoxConnection: TGroupBox
    Left = 8
    Top = 68
    Width = 380
    Height = 100
    Caption = ' Connection Settings '
    TabOrder = 1
    object LabelHost: TLabel
      Left = 16
      Top = 24
      Width = 49
      Height = 13
      Caption = 'Host (IP):'
    end
    object LabelPort: TLabel
      Left = 200
      Top = 24
      Width = 25
      Height = 13
      Caption = 'Port:'
    end
    object EditHost: TEdit
      Left = 16
      Top = 40
      Width = 160
      Height = 21
      TabOrder = 0
      Text = '192.168.0.10'
    end
    object EditPort: TEdit
      Left = 200
      Top = 40
      Width = 80
      Height = 21
      TabOrder = 1
      Text = '13400'
    end
    object ButtonConnect: TButton
      Left = 16
      Top = 68
      Width = 120
      Height = 25
      Caption = 'Connect'
      TabOrder = 2
      OnClick = ButtonConnectClick
    end
    object ButtonDisconnect: TButton
      Left = 160
      Top = 68
      Width = 120
      Height = 25
      Caption = 'Disconnect'
      Enabled = False
      TabOrder = 3
      OnClick = ButtonDisconnectClick
    end
  end
  object GroupBoxDiscovery: TGroupBox
    Left = 8
    Top = 174
    Width = 380
    Height = 60
    Caption = ' Vehicle Discovery '
    Enabled = False
    TabOrder = 2
    object ButtonDiscover: TButton
      Left = 16
      Top = 24
      Width = 120
      Height = 25
      Caption = 'Discover Vehicles'
      TabOrder = 0
      OnClick = ButtonDiscoverClick
    end
  end
  object GroupBoxRouting: TGroupBox
    Left = 8
    Top = 240
    Width = 380
    Height = 110
    Caption = ' Routing Activation '
    Enabled = False
    TabOrder = 3
    object LabelSourceAddr: TLabel
      Left = 16
      Top = 24
      Width = 93
      Height = 13
      Caption = 'Source Address (Hex):'
    end
    object LabelTargetAddr: TLabel
      Left = 200
      Top = 24
      Width = 89
      Height = 13
      Caption = 'Target Address (Hex):'
    end
    object EditSourceAddr: TEdit
      Left = 16
      Top = 40
      Width = 80
      Height = 21
      CharCase = ecUpperCase
      MaxLength = 4
      TabOrder = 0
      Text = '0E00'
    end
    object EditTargetAddr: TEdit
      Left = 200
      Top = 40
      Width = 80
      Height = 21
      CharCase = ecUpperCase
      MaxLength = 4
      TabOrder = 1
      Text = '0010'
    end
    object ButtonActivateRouting: TButton
      Left = 16
      Top = 72
      Width = 120
      Height = 25
      Caption = 'Activate Routing'
      TabOrder = 2
      OnClick = ButtonActivateRoutingClick
    end
  end
  object GroupBoxDiagnostics: TGroupBox
    Left = 8
    Top = 356
    Width = 380
    Height = 180
    Caption = ' Diagnostics '
    Enabled = False
    TabOrder = 4
    object ButtonReadVIN: TButton
      Left = 16
      Top = 24
      Width = 120
      Height = 25
      Caption = 'Read VIN'
      TabOrder = 0
      OnClick = ButtonReadVINClick
    end
    object ButtonReadDTC: TButton
      Left = 16
      Top = 56
      Width = 120
      Height = 25
      Caption = 'Read DTCs'
      TabOrder = 1
      OnClick = ButtonReadDTCClick
    end
    object ButtonClearDTC: TButton
      Left = 16
      Top = 88
      Width = 120
      Height = 25
      Caption = 'Clear DTCs'
      TabOrder = 2
      OnClick = ButtonClearDTCClick
    end
    object ButtonReadRPM: TButton
      Left = 160
      Top = 24
      Width = 120
      Height = 25
      Caption = 'Read RPM'
      TabOrder = 3
      OnClick = ButtonReadRPMClick
    end
    object ButtonReadSpeed: TButton
      Left = 160
      Top = 56
      Width = 120
      Height = 25
      Caption = 'Read Speed'
      TabOrder = 4
      OnClick = ButtonReadSpeedClick
    end
  end
  object MemoLog: TMemo
    Left = 394
    Top = 68
    Width = 398
    Height = 468
    ScrollBars = ssVertical
    TabOrder = 5
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 581
    Width = 800
    Height = 19
    Panels = <>
    SimplePanel = True
  end
end
