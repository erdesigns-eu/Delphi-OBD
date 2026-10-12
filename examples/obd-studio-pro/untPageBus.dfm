object frmBus: TfrmBus
  Left = 0
  Top = 0
  Width = 1208
  Height = 660
  Padding.Left = 12
  Padding.Top = 12
  Padding.Right = 12
  Padding.Bottom = 12
  TabOrder = 0
  object pnlReplay: TPanel
    AlignWithMargins = True
    Left = 12
    Top = 532
    Width = 1184
    Height = 116
    Margins.Left = 0
    Margins.Top = 12
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 2
    object replayBar: TOBDReplayBar
      Left = 0
      Top = 0
      Width = 1184
      Height = 116
      Align = alClient
      FileName = 'Golf_1.6TDI_2026-10-12_0705.obdrec'
      InfoText = 'Recorded 07:05 '#183' 12:18 '#183' 41 signals '#183' CAN trace'
      PositionMs = 222000
      DurationMs = 738000
      Speed = 1.0
      OnPlay = replayBarPlay
      OnPause = replayBarPause
      OnSeek = replayBarSeek
      OnSpeedChange = replayBarSpeedChange
      TabOrder = 0
    end
  end
  object pnlActuators: TPanel
    AlignWithMargins = True
    Left = 12
    Top = 12
    Width = 430
    Height = 508
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 12
    Margins.Bottom = 0
    Align = alLeft
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    object actTests: TOBDActuatorPanel
      Left = 0
      Top = 0
      Width = 430
      Height = 508
      Align = alClient
      OnStart = actTestsStart
      OnStop = actTestsStop
      OnStopAll = actTestsStopAll
      TabOrder = 0
    end
  end
  object pnlBus: TPanel
    Left = 454
    Top = 12
    Width = 742
    Height = 508
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 1
    object sendFrame: TOBDBusSend
      AlignWithMargins = True
      Left = 0
      Top = 380
      Width = 742
      Height = 128
      Margins.Left = 0
      Margins.Top = 12
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alBottom
      Title = 'Send frame'
      CANIdText = '7E0'
      DataText = '02 01 0C 00 00 00 00 00'
      PeriodMs = 500
      OnSend = sendFrameSend
      OnPeriodicChange = sendFramePeriodicChange
      TabOrder = 1
    end
    object traceBus: TOBDBusTrace
      Left = 0
      Top = 0
      Width = 742
      Height = 368
      Align = alClient
      Title = 'CAN trace'
      Subtitle = 'Powertrain CAN '#183' 500 kbit/s '#183' 2016 Golf VII 1.6 TDI'
      Capacity = 600
      AutoScroll = True
      TabOrder = 0
    end
  end
end
