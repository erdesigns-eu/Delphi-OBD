object frmSecurity: TfrmSecurity
  Left = 0
  Top = 0
  Width = 1208
  Height = 660
  Padding.Left = 12
  Padding.Top = 12
  Padding.Right = 12
  Padding.Bottom = 12
  TabOrder = 0
  object pnlSecurityLeft: TPanel
    Left = 12
    Top = 12
    Width = 720
    Height = 636
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    object procKeyLearn: TOBDProcedureView
      Left = 0
      Top = 0
      Width = 720
      Height = 360
      Align = alTop
      Title = 'Key learning'
      Subtitle = '17 Instruments / immobilizer 5 / transponder ID48'
      Glyph = glKey
      Steps = <
        item
          Caption = 'Preconditions'
          Title = 'Check all keys and voltage'
          Instructions.Strings = (
            'Confirm all customer keys are present.'
            'Keep the battery charger connected.'
            'Read the immobilizer PIN from 17 Instruments.')
          Glyph = glCheck
          ActionCaption = 'Start learning'
        end
        item
          Caption = 'Login'
          Title = 'Security login'
          Instructions.Strings = (
            'Enter extended diagnostic session.'
            'Send the immobilizer PIN through security access.'
            'Keep tester-present active every 2 seconds.')
          Glyph = glUnlock
        end
        item
          Caption = 'Key count'
          Title = 'Store the total key count'
          Instructions.Strings = (
            'Tell the cluster that three keys will be learned.'
            'Existing keys not presented during this session are removed.')
          Glyph = glKey
        end
        item
          Caption = 'Learn keys'
          Title = 'Hold key 3 against the marker'
          Instructions.Strings = (
            'Hold key 3 against the steering-column marker.'
            'Switch ignition on and wait for transponder ID48 detection.'
            'The slot changes to learned when the cluster accepts the key.')
          Glyph = glKey
          WarningTitle = 'Key table will be rewritten'
          WarningText = 'Keys not learned in this session stop working on a real car.'
        end
        item
          Caption = 'Finish'
          Title = 'Finish the immobilizer session'
          Instructions.Strings = (
            'Cycle ignition off and on.'
            'Verify the immobilizer lamp goes out.'
            'Save the adapted key table in the job record.')
          Glyph = glCheck
        end>
      LiveValues = <
        item
          LabelText = 'Keys learned'
          ValueText = '2 of 3'
          State = opvAccent
          Fraction = 0.660000000000000000
          ShowBar = True
        end
        item
          LabelText = 'Transponder'
          ValueText = 'Detected / ID48'
          State = opvSuccess
        end
        item
          LabelText = 'Time left'
          ValueText = '00:41'
          State = opvAccent
          Fraction = 0.700000000000000000
          ShowBar = True
        end>
      SourceText = 'Procedure catalogue: VAG MQB immobilizer 5'
      OnAbort = procKeyLearnAbort
      OnNext = procKeyLearnNext
      OnAction = procKeyLearnAction
      TabOrder = 0
    end
    object cdKeySlots: TOBDCard
      AlignWithMargins = True
      Left = 0
      Top = 372
      Width = 720
      Height = 264
      Margins.Left = 0
      Margins.Top = 12
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      Title = 'Key slots'
      FooterText = 'The session ends safely if time runs out; learned keys are kept.'
      ShowFooter = True
      StatusEdge = ceAccent
      TabOrder = 1
      object slotsKeys: TOBDKeySlots
        Left = 16
        Top = 64
        Width = 688
        Height = 144
        Slots = <
          item
            SlotLabel = 'Key 1'
            KeyId = 'Learned'
            State = kssLearned
          end
          item
            SlotLabel = 'Key 2'
            KeyId = 'Learned'
            State = kssLearned
          end
          item
            SlotLabel = 'Key 3'
            KeyId = 'Hold to marker'
            State = kssLearning
          end
          item
            SlotLabel = 'Key 4'
            KeyId = 'Empty'
          end
          item
            SlotLabel = 'Key 5'
            KeyId = 'Empty'
          end
          item
            SlotLabel = 'Key 6'
            KeyId = 'Empty'
          end
          item
            SlotLabel = 'Key 7'
            KeyId = 'Empty'
          end
          item
            SlotLabel = 'Key 8'
            KeyId = 'Empty'
          end>
        Cols = 4
        ItemIndex = 2
        OnSlotClick = slotsKeysSlotClick
        TabOrder = 0
      end
      object btnStartLearning: TOBDButton
        Left = 16
        Top = 220
        Width = 132
        Height = 32
        Glyph = glKey
        Caption = 'Learn key'
        OnClick = btnStartLearningClick
        TabOrder = 1
      end
      object btnCancelLearning: TOBDButton
        Left = 158
        Top = 220
        Width = 100
        Height = 32
        Kind = bkSecondary
        Glyph = glStop
        Caption = 'Cancel'
        OnClick = btnCancelLearningClick
        TabOrder = 2
      end
    end
  end
  object pnlSecurityRight: TPanel
    AlignWithMargins = True
    Left = 744
    Top = 12
    Width = 452
    Height = 636
    Margins.Left = 12
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alRight
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 1
    object secAccess: TOBDSecurityAccessCard
      Left = 0
      Top = 0
      Width = 452
      Height = 184
      Align = alTop
      State = sacUnlocked
      ControlUnit = '17 Instruments'
      SessionText = 'Extended (0x03)'
      LevelText = 'Immobilizer login / PIN'
      KeepAliveText = 'Tester present / 2 s'
      KeepAliveProgress = 0.700000000000000000
      TabOrder = 0
    end
    object preKeyLearning: TOBDPreconditionList
      AlignWithMargins = True
      Left = 0
      Top = 196
      Width = 452
      Height = 172
      Margins.Left = 0
      Margins.Top = 12
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Items = <
        item
          State = pcnMet
          LabelText = 'All keys present'
          ValueText = '3 keys'
        end
        item
          State = pcnMet
          LabelText = 'Ignition on'
          ValueText = 'Terminal 15'
        end
        item
          State = pcnMet
          LabelText = 'Battery voltage >= 12.5 V'
          ValueText = '12.7 V'
        end
        item
          State = pcnMet
          LabelText = 'Immobilizer PIN'
          ValueText = 'Read from 17'
        end
        item
          State = pcnNotRequired
          LabelText = 'Online login'
          ValueText = 'Not required'
        end>
      Title = 'Preconditions'
      FooterText = 'Learning starts when every condition is met.'
      TabOrder = 1
    end
    object cdProtection: TOBDCard
      AlignWithMargins = True
      Left = 0
      Top = 380
      Width = 452
      Height = 256
      Margins.Left = 0
      Margins.Top = 12
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      Title = 'Component protection'
      FooterText = 'Removing protection needs an online manufacturer session and licence.'
      ShowFooter = True
      StatusEdge = ceDanger
      TabOrder = 2
      object grdProtection: TOBDDataGrid
        Left = 16
        Top = 64
        Width = 420
        Height = 136
        DataColumns = <
          item
            Caption = 'Addr'
            Width = 44
            Kind = dckMono
          end
          item
            Caption = 'Component'
            Width = 128
          end
          item
            Caption = 'Part number'
            Width = 128
            Kind = dckMono
          end
          item
            Caption = 'State'
            Width = 96
            Kind = dckChip
          end>
        SortColumn = 0
        EmptyTitle = 'No control units scanned'
        EmptyHint = 'Click Refresh after connecting to the vehicle.'
        EmptyGlyph = glShield
        BorderVisible = True
        TabOrder = 0
      end
      object btnRefreshProtection: TOBDButton
        Left = 316
        Top = 212
        Width = 120
        Height = 32
        Kind = bkGhost
        Glyph = glRefresh
        Caption = 'Refresh'
        OnClick = btnRefreshProtectionClick
        TabOrder = 1
      end
    end
  end
end
