object frmFlash: TfrmFlash
  Left = 0
  Top = 0
  Width = 1208
  Height = 660
  Padding.Left = 12
  Padding.Top = 12
  Padding.Right = 12
  Padding.Bottom = 12
  TabOrder = 0
  object pnlLeft: TPanel
    Left = 12
    Top = 12
    Width = 440
    Height = 636
    Align = alLeft
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    object cdFlashImage: TOBDCard
      Left = 0
      Top = 0
      Width = 440
      Height = 216
      Align = alTop
      Title = 'Flash image'
      StatusEdge = ceSuccess
      TabOrder = 0
      object lstFlashChecks: TOBDPreconditionList
        Left = 16
        Top = 64
        Width = 408
        Height = 136
        Items = <
          item
            State = pcnMet
            LabelText = '04L906056KJ_0012.frf'
            ValueText = '2.0 MB / 664 blocks'
          end
          item
            State = pcnMet
            LabelText = 'Part number family matches'
            ValueText = '04L906056HT -> KJ'
          end
          item
            State = pcnMet
            LabelText = 'Signature valid'
            ValueText = 'RSA-2048'
          end
          item
            State = pcnMet
            LabelText = 'Block checksums'
            ValueText = 'OK'
          end>
        Title = 'Image checks'
        FooterText = 'Hardware 04L907309B supported.'
        TabOrder = 0
      end
    end
    object preFlashReady: TOBDPreconditionList
      AlignWithMargins = True
      Left = 0
      Top = 228
      Width = 440
      Height = 196
      Margins.Left = 0
      Margins.Top = 12
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Items = <
        item
          State = pcnMet
          LabelText = 'Ignition on'
          ValueText = 'Terminal 15'
        end
        item
          State = pcnMet
          LabelText = 'Battery charger'
          ValueText = '13.6 V'
        end
        item
          State = pcnMet
          LabelText = 'Engine off'
          ValueText = '0 rpm'
        end
        item
          State = pcnMet
          LabelText = 'Backup saved'
          ValueText = '01_Engine_1851.bin'
        end
        item
          State = pcnMet
          LabelText = 'Security access'
          ValueText = 'Level 0x11'
        end>
      Title = 'Preconditions'
      FooterText = 'Checked again before every phase.'
      TabOrder = 1
    end
    object tlFlash: TOBDTimeline
      AlignWithMargins = True
      Left = 0
      Top = 436
      Width = 440
      Height = 200
      Margins.Left = 0
      Margins.Top = 12
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      Title = 'Flash history'
      Subtitle = 'Garage Peeters'
      Glyph = glFlash
      Entries = <
        item
          TimeText = '18:51'
          Kind = otlBackup
          Glyph = glDatabase
          Status = skSuccess
          Title = 'Backup saved'
          Detail = '01_Engine_1851.bin before programming.'
          UserName = 'Workshop'
        end
        item
          TimeText = '18:54'
          Kind = otlSecurity
          Glyph = glUnlock
          Status = skSuccess
          Title = 'Security access granted'
          Detail = 'Level 0x11 programming unlock.'
          UserName = 'Host'
        end>
      TabOrder = 2
    end
  end
  object pnlMain: TPanel
    AlignWithMargins = True
    Left = 464
    Top = 12
    Width = 732
    Height = 636
    Margins.Left = 12
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 1
    object cdFlash: TOBDCard
      Left = 0
      Top = 0
      Width = 732
      Height = 636
      Align = alClient
      Title = 'Flash 01 Engine electronics'
      FooterText = 'UDS on CAN / 0x7E0 / programming session'
      ShowFooter = True
      StatusEdge = ceAccent
      TabOrder = 0
      object pbFlashSteps: TOBDProgressBar
        Left = 16
        Top = 64
        Width = 700
        Height = 54
        Style = pbsSteps
        Steps = <
          item
            Caption = 'Pre-check'
            State = stActive
          end
          item
            Caption = 'Session'
          end
          item
            Caption = 'Erase'
          end
          item
            Caption = 'Transfer'
          end
          item
            Caption = 'Verify'
          end
          item
            Caption = 'Reset'
          end>
        TabOrder = 0
      end
      object pbFlashTransfer: TOBDProgressBar
        Left = 16
        Top = 124
        Width = 700
        Height = 52
        Caption = 'Ready to transfer'
        TabOrder = 1
      end
      object pnlMetrics: TPanel
        Left = 16
        Top = 184
        Width = 700
        Height = 88
        BevelOuter = bvNone
        ParentBackground = False
        ParentColor = True
        ShowCaption = False
        TabOrder = 2
        object tileBlock: TOBDValueTile
          Left = 0
          Top = 0
          Width = 166
          Height = 88
          Caption = 'Block'
          Min = 0.000000000000000000
          Max = 664.000000000000000000
          Unit = 'of 664'
          TabOrder = 0
        end
        object tileSpeed: TOBDValueTile
          Left = 178
          Top = 0
          Width = 166
          Height = 88
          Caption = 'Speed'
          Min = 0.000000000000000000
          Max = 60.000000000000000000
          Unit = 'kB/s'
          Decimals = 1
          TabOrder = 1
        end
        object tileRemaining: TOBDValueTile
          Left = 356
          Top = 0
          Width = 166
          Height = 88
          Caption = 'Remaining'
          Min = 0.000000000000000000
          Max = 120.000000000000000000
          Unit = 's'
          TabOrder = 2
        end
        object tileRetries: TOBDValueTile
          Left = 534
          Top = 0
          Width = 166
          Height = 88
          Caption = 'Retries'
          Min = 0.000000000000000000
          Max = 5.000000000000000000
          TabOrder = 3
        end
      end
      object rbrVoltage: TOBDRangeBar
        Left = 16
        Top = 284
        Width = 700
        Height = 44
        Caption = 'Voltage gate'
        Value = 13.600000000000000000
        Min = 11.000000000000000000
        Max = 15.000000000000000000
        Low = 12.500000000000000000
        High = 15.000000000000000000
        Decimals = 1
        UnitText = 'V'
      end
      object bnrIgnition: TOBDBanner
        Left = 16
        Top = 340
        Width = 700
        Height = 56
        Kind = bnInfo
        Title = 'Do not switch off ignition'
        Text = 'Keep the charger connected and do not disconnect the adapter while programming.'
        TabOrder = 3
      end
      object lvFlash: TOBDLogViewer
        Left = 16
        Top = 408
        Width = 700
        Height = 124
        BorderStyle = bsSingle
        ShowLevelTag = False
        TabOrder = 4
      end
      object pnlFlashButtons: TPanel
        Left = 16
        Top = 544
        Width = 700
        Height = 44
        BevelOuter = bvNone
        ParentBackground = False
        ParentColor = True
        ShowCaption = False
        TabOrder = 5
        object btnAbortFlash: TOBDButton
          Left = 0
          Top = 6
          Width = 118
          Height = 32
          Kind = bkDangerOutline
          Glyph = glStop
          Caption = 'Abort'
          OnClick = btnAbortFlashClick
          TabOrder = 0
        end
        object btnStartFlash: TOBDButton
          Left = 562
          Top = 6
          Width = 138
          Height = 32
          Glyph = glFlash
          Caption = 'Start flash'
          OnClick = btnStartFlashClick
          TabOrder = 1
        end
      end
    end
  end
end
