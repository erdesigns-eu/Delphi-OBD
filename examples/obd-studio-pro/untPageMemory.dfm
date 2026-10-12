object frmMemory: TfrmMemory
  Left = 0
  Top = 0
  Width = 1208
  Height = 660
  Padding.Left = 12
  Padding.Top = 12
  Padding.Right = 12
  Padding.Bottom = 12
  TabOrder = 0
  object hexDump: TOBDHexView
    Left = 12
    Top = 12
    Width = 780
    Height = 636
    Align = alClient
    Title = 'EEPROM 24C32 dump'
    BaseAddress = 0
    BytesPerRow = 16
    TabOrder = 0
  end
  object pnlSide: TPanel
    AlignWithMargins = True
    Left = 804
    Top = 12
    Width = 392
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
    object cdMemoryActions: TOBDCard
      Left = 0
      Top = 0
      Width = 392
      Height = 636
      Align = alClient
      Title = 'Read and write'
      FooterText = 'Every real write should create a fresh backup first.'
      ShowFooter = True
      StatusEdge = ceAccent
      TabOrder = 0
      object segMemorySource: TOBDSegmented
        Left = 16
        Top = 64
        Width = 360
        Height = 32
        Items.Strings = (
          'ECU memory'
          'EEPROM file')
        ItemIndex = 1
        TabOrder = 0
      end
      object edMemoryFile: TOBDEdit
        Left = 16
        Top = 112
        Width = 360
        Height = 32
        Text = 'RCD330_24C32_2026-10-12.bin'
        ReadOnly = True
        Mono = True
        TabOrder = 1
      end
      object edMemoryFrom: TOBDEdit
        Left = 16
        Top = 156
        Width = 174
        Height = 32
        Text = '0x0000'
        ReadOnly = True
        Mono = True
        TabOrder = 2
      end
      object edMemoryTo: TOBDEdit
        Left = 202
        Top = 156
        Width = 174
        Height = 32
        Text = '0x0FFF'
        ReadOnly = True
        Mono = True
        TabOrder = 3
      end
      object pbReadMemory: TOBDProgressBar
        Left = 16
        Top = 204
        Width = 360
        Height = 46
        Caption = 'Read progress'
        TabOrder = 4
      end
      object btnReadMemory: TOBDButton
        Left = 16
        Top = 266
        Width = 86
        Height = 32
        Glyph = glRead
        Caption = 'Read'
        OnClick = btnReadMemoryClick
        TabOrder = 5
      end
      object btnWriteMemory: TOBDButton
        Left = 110
        Top = 266
        Width = 86
        Height = 32
        Kind = bkDangerOutline
        Glyph = glWrite
        Caption = 'Write'
        OnClick = btnWriteMemoryClick
        TabOrder = 6
      end
      object btnSaveMemory: TOBDButton
        Left = 204
        Top = 266
        Width = 78
        Height = 32
        Kind = bkSecondary
        Glyph = glSave
        Caption = 'Save'
        OnClick = btnSaveMemoryClick
        TabOrder = 7
      end
      object btnCompareMemory: TOBDButton
        Left = 290
        Top = 266
        Width = 86
        Height = 32
        Kind = bkSecondary
        Glyph = glCompare
        Caption = 'Compare'
        OnClick = btnCompareMemoryClick
        TabOrder = 8
      end
      object preMemoryChecks: TOBDPreconditionList
        Left = 16
        Top = 316
        Width = 360
        Height = 150
        Items = <
          item
            State = pcnMet
            LabelText = 'VIN region'
            ValueText = '0x0000..0x0010'
          end
          item
            State = pcnMet
            LabelText = 'Radio code bytes'
            ValueText = '04 01 07 02'
          end
          item
            State = pcnMet
            LabelText = 'Checksum'
            ValueText = '5A3C OK'
          end
          item
            State = pcnPending
            LabelText = 'Backup compare'
            ValueText = '4 differences'
          end>
        Title = 'Checksums'
        FooterText = 'Catalogue: VW RCD 330 / Delphi 24C32.'
        TabOrder = 9
      end
      object lstMemoryInfo: TOBDListBox
        Left = 16
        Top = 482
        Width = 360
        Height = 98
        EmptyText = 'No dump loaded'
        TabOrder = 10
      end
    end
  end
end
