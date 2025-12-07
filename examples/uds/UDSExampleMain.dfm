object UDSForm: TUDSForm
  Left = 0
  Top = 0
  Caption = 'UDS Protocol Example'
  ClientHeight = 600
  ClientWidth = 800
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  object MainPanel: TPanel
    Left = 0
    Top = 0
    Width = 800
    Height = 581
    Align = alClient
    object TopPanel: TPanel
      Left = 1
      Top = 1
      Width = 798
      Height = 50
      Align = alTop
      Color = clNavy
      ParentBackground = False
      object TitleLabel: TLabel
        Left = 16
        Top = 12
        Width = 244
        Height = 29
        Caption = 'UDS Protocol Example'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -24
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object ConnectionGroupBox: TGroupBox
      Left = 8
      Top = 57
      Width = 250
      Height = 90
      Caption = ' Connection '
      object PortLabel: TLabel
        Left = 16
        Top = 24
        Width = 22
        Height = 13
        Caption = 'Port'
      end
      object PortComboBox: TComboBox
        Left = 72
        Top = 21
        Width = 145
        Height = 21
        Style = csDropDownList
      end
      object ConnectButton: TButton
        Left = 16
        Top = 54
        Width = 97
        Height = 25
        Caption = 'Connect'
        OnClick = ConnectButtonClick
      end
      object DisconnectButton: TButton
        Left = 120
        Top = 54
        Width = 97
        Height = 25
        Caption = 'Disconnect'
        OnClick = DisconnectButtonClick
      end
    end
    object SessionGroupBox: TGroupBox
      Left = 264
      Top = 57
      Width = 250
      Height = 90
      Caption = ' Session Control '
      object Label1: TLabel
        Left = 16
        Top = 24
        Width = 36
        Height = 13
        Caption = 'Session'
      end
      object SessionCombo: TComboBox
        Left = 16
        Top = 43
        Width = 217
        Height = 21
        Style = csDropDownList
      end
      object StartSessionButton: TButton
        Left = 16
        Top = 70
        Width = 217
        Height = 25
        Caption = 'Start Session'
        OnClick = StartSessionButtonClick
      end
    end
    object SecurityGroupBox: TGroupBox
      Left = 520
      Top = 57
      Width = 272
      Height = 90
      Caption = ' Security Access '
      object Label2: TLabel
        Left = 16
        Top = 24
        Width = 26
        Height = 13
        Caption = 'Level'
      end
      object SecurityLevelEdit: TEdit
        Left = 56
        Top = 21
        Width = 49
        Height = 21
        Text = '1'
      end
      object RequestSeedButton: TButton
        Left = 112
        Top = 19
        Width = 145
        Height = 25
        Caption = 'Request Seed'
        OnClick = RequestSeedButtonClick
      end
      object SendKeyButton: TButton
        Left = 112
        Top = 50
        Width = 145
        Height = 25
        Caption = 'Send Key'
        OnClick = SendKeyButtonClick
      end
    end
    object DTCGroupBox: TGroupBox
      Left = 8
      Top = 153
      Width = 380
      Height = 120
      Caption = ' Diagnostic Trouble Codes '
      object Label3: TLabel
        Left = 16
        Top = 24
        Width = 63
        Height = 13
        Caption = 'Subfunction'
      end
      object DTCSubfunctionCombo: TComboBox
        Left = 96
        Top = 21
        Width = 265
        Height = 21
        Style = csDropDownList
      end
      object ReadDTCButton: TButton
        Left = 16
        Top = 54
        Width = 345
        Height = 25
        Caption = 'Read DTC'
        OnClick = ReadDTCButtonClick
      end
      object ClearDTCButton: TButton
        Left = 16
        Top = 85
        Width = 345
        Height = 25
        Caption = 'Clear DTC'
        OnClick = ClearDTCButtonClick
      end
    end
    object DataGroupBox: TGroupBox
      Left = 394
      Top = 153
      Width = 398
      Height = 120
      Caption = ' Data Services '
      object Label4: TLabel
        Left = 16
        Top = 24
        Width = 42
        Height = 13
        Caption = 'Data ID'
      end
      object Label5: TLabel
        Left = 16
        Top = 54
        Width = 27
        Height = 13
        Caption = 'Value'
      end
      object DataIDEdit: TEdit
        Left = 72
        Top = 21
        Width = 121
        Height = 21
        Text = '$F190'
      end
      object DataValueEdit: TEdit
        Left = 72
        Top = 51
        Width = 305
        Height = 21
      end
      object ReadDataButton: TButton
        Left = 200
        Top = 19
        Width = 177
        Height = 25
        Caption = 'Read Data'
        OnClick = ReadDataButtonClick
      end
      object WriteDataButton: TButton
        Left = 16
        Top = 85
        Width = 361
        Height = 25
        Caption = 'Write Data'
        OnClick = WriteDataButtonClick
      end
    end
    object RoutineGroupBox: TGroupBox
      Left = 8
      Top = 279
      Width = 784
      Height = 80
      Caption = ' Routine Control '
      object RoutineIDEdit: TEdit
        Left = 16
        Top = 24
        Width = 121
        Height = 21
        Text = '$0203'
      end
      object StartRoutineButton: TButton
        Left = 152
        Top = 22
        Width = 145
        Height = 25
        Caption = 'Start Routine'
        OnClick = StartRoutineButtonClick
      end
      object StopRoutineButton: TButton
        Left = 304
        Top = 22
        Width = 145
        Height = 25
        Caption = 'Stop Routine'
        OnClick = StopRoutineButtonClick
      end
      object GetRoutineResultButton: TButton
        Left = 456
        Top = 22
        Width = 145
        Height = 25
        Caption = 'Get Result'
        OnClick = GetRoutineResultButtonClick
      end
    end
    object ResultMemo: TMemo
      Left = 8
      Top = 365
      Width = 784
      Height = 208
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 581
    Width = 800
    Height = 19
    SimplePanel = True
  end
end
