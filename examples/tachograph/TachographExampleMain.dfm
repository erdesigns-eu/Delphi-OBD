object TachographForm: TTachographForm
  Left = 0
  Top = 0
  Caption = 'Tachograph / Odometer Example'
  ClientHeight = 720
  ClientWidth = 1024
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
  object MainPanel: TPanel
    Left = 0
    Top = 0
    Width = 1024
    Height = 701
    Align = alClient
    TabOrder = 0
    object TopPanel: TPanel
      Left = 1
      Top = 1
      Width = 1022
      Height = 60
      Align = alTop
      Color = clNavy
      ParentBackground = False
      TabOrder = 0
      object TitleLabel: TLabel
        Left = 16
        Top = 16
        Width = 360
        Height = 29
        Caption = 'Tachograph / Odometer Example'
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
      Top = 68
      Width = 320
      Height = 120
      Caption = ' Connection '
      TabOrder = 1
      object PortLabel: TLabel
        Left = 16
        Top = 24
        Width = 22
        Height = 13
        Caption = 'Port'
      end
      object BaudRateLabel: TLabel
        Left = 16
        Top = 56
        Width = 50
        Height = 13
        Caption = 'Baud Rate'
      end
      object PortComboBox: TComboBox
        Left = 96
        Top = 21
        Width = 145
        Height = 21
        Style = csDropDownList
        TabOrder = 0
      end
      object BaudRateComboBox: TComboBox
        Left = 96
        Top = 53
        Width = 145
        Height = 21
        Style = csDropDownList
        TabOrder = 1
      end
      object ConnectButton: TButton
        Left = 16
        Top = 85
        Width = 113
        Height = 25
        Caption = 'Connect'
        TabOrder = 2
        OnClick = ConnectButtonClick
      end
      object DisconnectButton: TButton
        Left = 144
        Top = 85
        Width = 113
        Height = 25
        Caption = 'Disconnect'
        TabOrder = 3
        OnClick = DisconnectButtonClick
      end
    end
    object VehicleGroupBox: TGroupBox
      Left = 336
      Top = 68
      Width = 400
      Height = 120
      Caption = ' Vehicle Configuration '
      TabOrder = 2
      object VehicleMakeLabel: TLabel
        Left = 16
        Top = 24
        Width = 67
        Height = 13
        Caption = 'Vehicle Make'
      end
      object TachoManufacturerLabel: TLabel
        Left = 16
        Top = 56
        Width = 87
        Height = 13
        Caption = 'Tacho Manufacturer'
      end
      object StandardLabel: TLabel
        Left = 16
        Top = 88
        Width = 45
        Height = 13
        Caption = 'Standard'
      end
      object VehicleMakeComboBox: TComboBox
        Left = 128
        Top = 21
        Width = 185
        Height = 21
        Style = csDropDownList
        TabOrder = 0
      end
      object TachoManufacturerComboBox: TComboBox
        Left = 128
        Top = 53
        Width = 185
        Height = 21
        Style = csDropDownList
        TabOrder = 1
      end
      object StandardComboBox: TComboBox
        Left = 128
        Top = 85
        Width = 185
        Height = 21
        Style = csDropDownList
        TabOrder = 2
      end
      object AutoDetectButton: TButton
        Left = 320
        Top = 21
        Width = 65
        Height = 85
        Caption = 'Auto Detect'
        TabOrder = 3
        WordWrap = True
        OnClick = AutoDetectButtonClick
      end
    end
    object OdometerGroupBox: TGroupBox
      Left = 8
      Top = 194
      Width = 320
      Height = 200
      Caption = ' Odometer '
      TabOrder = 3
      object OdometerPanel: TPanel
        Left = 16
        Top = 24
        Width = 289
        Height = 105
        BevelOuter = bvLowered
        TabOrder = 0
        object TotalDistanceLabel: TLabel
          Left = 16
          Top = 16
          Width = 74
          Height = 13
          Caption = 'Total Distance:'
        end
        object TotalDistanceValue: TLabel
          Left = 120
          Top = 16
          Width = 28
          Height = 13
          Caption = '0 km'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object TripDistanceLabel: TLabel
          Left = 16
          Top = 40
          Width = 70
          Height = 13
          Caption = 'Trip Distance:'
        end
        object TripDistanceValue: TLabel
          Left = 120
          Top = 40
          Width = 28
          Height = 13
          Caption = '0 km'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object CurrentSpeedLabel: TLabel
          Left = 16
          Top = 64
          Width = 74
          Height = 13
          Caption = 'Current Speed:'
        end
        object CurrentSpeedValue: TLabel
          Left = 120
          Top = 64
          Width = 48
          Height = 13
          Caption = '0.0 km/h'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
      object ReadOdometerButton: TButton
        Left = 16
        Top = 144
        Width = 129
        Height = 25
        Caption = 'Read Odometer'
        TabOrder = 1
        OnClick = ReadOdometerButtonClick
      end
      object ResetTripButton: TButton
        Left = 160
        Top = 144
        Width = 129
        Height = 25
        Caption = 'Reset Trip Counter'
        TabOrder = 2
        OnClick = ResetTripButtonClick
      end
    end
    object CalibrationGroupBox: TGroupBox
      Left = 336
      Top = 194
      Width = 400
      Height = 200
      Caption = ' Calibration '
      TabOrder = 4
      object TireCircumferenceLabel: TLabel
        Left = 16
        Top = 24
        Width = 114
        Height = 13
        Caption = 'Tire Circumference (mm)'
      end
      object PulsesPerKmLabel: TLabel
        Left = 16
        Top = 56
        Width = 74
        Height = 13
        Caption = 'Pulses per km'
      end
      object LastCalibrationLabel: TLabel
        Left = 16
        Top = 120
        Width = 78
        Height = 13
        Caption = 'Last Calibration:'
      end
      object LastCalibrationValue: TLabel
        Left = 112
        Top = 120
        Width = 23
        Height = 13
        Caption = 'N/A'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object TireCircumferenceEdit: TEdit
        Left = 152
        Top = 21
        Width = 121
        Height = 21
        TabOrder = 0
        Text = '2000'
      end
      object PulsesPerKmEdit: TEdit
        Left = 152
        Top = 53
        Width = 121
        Height = 21
        TabOrder = 1
        Text = '4000'
      end
      object CalibrateButton: TButton
        Left = 152
        Top = 85
        Width = 121
        Height = 25
        Caption = 'Calibrate'
        TabOrder = 2
        OnClick = CalibrateButtonClick
      end
    end
    object DriverActivityGroupBox: TGroupBox
      Left = 744
      Top = 68
      Width = 272
      Height = 326
      Caption = ' Driver Activities '
      TabOrder = 5
      object DaysLabel: TLabel
        Left = 16
        Top = 26
        Width = 26
        Height = 13
        Caption = 'Days'
      end
      object ActivityGrid: TStringGrid
        Left = 16
        Top = 82
        Width = 240
        Height = 200
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
        TabOrder = 0
      end
      object ReadActivitiesButton: TButton
        Left = 120
        Top = 51
        Width = 136
        Height = 25
        Caption = 'Read Activities'
        TabOrder = 1
        OnClick = ReadActivitiesButtonClick
      end
      object DaysEdit: TEdit
        Left = 56
        Top = 23
        Width = 49
        Height = 21
        TabOrder = 2
        Text = '7'
      end
    end
    object VehicleInfoGroupBox: TGroupBox
      Left = 8
      Top = 400
      Width = 728
      Height = 120
      Caption = ' Vehicle Information '
      TabOrder = 6
      object VINLabel: TLabel
        Left = 16
        Top = 32
        Width = 21
        Height = 13
        Caption = 'VIN:'
      end
      object VINValue: TLabel
        Left = 88
        Top = 32
        Width = 61
        Height = 13
        Caption = 'Not Read'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object VehicleUnitIDLabel: TLabel
        Left = 16
        Top = 56
        Width = 66
        Height = 13
        Caption = 'Vehicle Unit:'
      end
      object VehicleUnitIDValue: TLabel
        Left = 88
        Top = 56
        Width = 61
        Height = 13
        Caption = 'Not Read'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object ReadVehicleInfoButton: TButton
        Left = 16
        Top = 85
        Width = 145
        Height = 25
        Caption = 'Read Vehicle Info'
        TabOrder = 0
        OnClick = ReadVehicleInfoButtonClick
      end
    end
    object LogMemo: TMemo
      Left = 8
      Top = 552
      Width = 1008
      Height = 140
      ScrollBars = ssVertical
      TabOrder = 7
    end
    object LogLabel: TLabel
      Left = 8
      Top = 531
      Width = 62
      Height = 13
      Caption = 'Activity Log:'
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 701
    Width = 1024
    Height = 19
    Panels = <>
    SimplePanel = True
  end
end
