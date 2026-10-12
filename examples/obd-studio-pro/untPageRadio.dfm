object frmRadio: TfrmRadio
  Left = 0
  Top = 0
  Width = 1208
  Height = 660
  Padding.Left = 12
  Padding.Top = 12
  Padding.Right = 12
  Padding.Bottom = 12
  TabOrder = 0
  object pnlRadio: TPanel
    Left = 12
    Top = 12
    Width = 540
    Height = 636
    Align = alLeft
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    object cdRadio: TOBDCard
      Left = 0
      Top = 0
      Width = 540
      Height = 636
      Align = alClient
      Title = 'Radio code'
      StatusEdge = ceSuccess
      TabOrder = 0
      object segRadioMethod: TOBDSegmented
        Left = 16
        Top = 64
        Width = 508
        Height = 32
        Items.Strings = (
          'Serial number'
          'EEPROM dump'
          'Online')
        ItemIndex = 0
        TabOrder = 0
      end
      object cbRadioBrand: TOBDComboBox
        Left = 16
        Top = 112
        Width = 248
        Height = 32
        Items.Strings = (
          'Volkswagen'
          'Audi'
          'SEAT'
          'Skoda')
        ItemIndex = 0
        TabOrder = 1
      end
      object cbRadioUnit: TOBDComboBox
        Left = 276
        Top = 112
        Width = 248
        Height = 32
        Items.Strings = (
          'RCD 330 (Delphi)'
          'RNS 510'
          'Gamma 5'
          'Premium V')
        ItemIndex = 0
        TabOrder = 2
      end
      object edRadioSerial: TOBDEdit
        Left = 16
        Top = 160
        Width = 508
        Height = 32
        Text = 'VWZ1Z2N1234567'
        MaxLength = 14
        Mono = True
        CharCase = ecUpperCase
        OnChange = edRadioSerialChange
        TabOrder = 3
      end
      object chpRadioStatus: TOBDChip
        Left = 16
        Top = 204
        Width = 104
        Height = 26
        Caption = 'CODE FOUND'
        Kind = skSuccess
        Filled = True
      end
      object edCode1: TOBDEdit
        Left = 16
        Top = 260
        Width = 64
        Height = 72
        Text = '4'
        Alignment = taCenter
        ReadOnly = True
        Mono = True
        TabOrder = 4
      end
      object edCode2: TOBDEdit
        Left = 92
        Top = 260
        Width = 64
        Height = 72
        Text = '1'
        Alignment = taCenter
        ReadOnly = True
        Mono = True
        TabOrder = 5
      end
      object edCode3: TOBDEdit
        Left = 168
        Top = 260
        Width = 64
        Height = 72
        Text = '7'
        Alignment = taCenter
        ReadOnly = True
        Mono = True
        TabOrder = 6
      end
      object edCode4: TOBDEdit
        Left = 244
        Top = 260
        Width = 64
        Height = 72
        Text = '2'
        Alignment = taCenter
        ReadOnly = True
        Mono = True
        TabOrder = 7
      end
      object btnCalcRadio: TOBDButton
        Left = 340
        Top = 256
        Width = 184
        Height = 32
        Glyph = glRadio
        Caption = 'Calculate'
        OnClick = btnCalcRadioClick
        TabOrder = 8
      end
      object btnCopyRadio: TOBDButton
        Left = 340
        Top = 298
        Width = 88
        Height = 32
        Kind = bkSecondary
        Glyph = glCopy
        Caption = 'Copy'
        OnClick = btnCopyRadioClick
        TabOrder = 9
      end
      object btnSaveRadio: TOBDButton
        Left = 436
        Top = 298
        Width = 88
        Height = 32
        Kind = bkGhost
        Glyph = glSave
        Caption = 'Save'
        OnClick = btnSaveRadioClick
        TabOrder = 10
      end
      object bnrRadio: TOBDBanner
        Left = 16
        Top = 356
        Width = 508
        Height = 64
        Kind = bnInfo
        Title = 'Entering the code'
        Text = 'Preset keys 1-4 select each digit; hold OK for 2 s. Algorithms are supplied by the host.'
        TabOrder = 11
      end
    end
  end
  object pnlRadioRight: TPanel
    AlignWithMargins = True
    Left = 564
    Top = 12
    Width = 632
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
    object hexRadio: TOBDHexView
      Left = 0
      Top = 0
      Width = 632
      Height = 386
      Align = alTop
      Title = 'EEPROM 24C32'
      BaseAddress = 0
      BytesPerRow = 16
      TabOrder = 0
    end
    object cdRadioEvidence: TOBDCard
      AlignWithMargins = True
      Left = 0
      Top = 398
      Width = 632
      Height = 238
      Margins.Left = 0
      Margins.Top = 12
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      Title = 'From the EEPROM'
      FooterText = 'Layout from catalogs/radio-codes/vw-rcd330.json.'
      ShowFooter = True
      TabOrder = 1
      object lstRadioEvidence: TOBDListBox
        Left = 16
        Top = 64
        Width = 600
        Height = 128
        EmptyText = 'No evidence loaded'
        TabOrder = 0
      end
    end
  end
end
