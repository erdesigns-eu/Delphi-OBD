object ShowcaseForm: TShowcaseForm
  Left = 0
  Top = 0
  Caption = 'ERD Components - general VCL showcase'
  ClientHeight = 760
  ClientWidth = 1280
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyDown = ShowcaseKeyDown
  TextHeight = 15
  object Chrome: TERDPanel
    Left = 0
    Top = 0
    Width = 1280
    Height = 244
    Align = alTop
    TabOrder = 0
    OnResize = LayoutChrome
    BorderWidth = 0
    DesignSize = (
      1280
      244)
    object TitleBar: TERDTitleBar
      Left = 0
      Top = 0
      Width = 1280
      Height = 40
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      TabStop = True
      OnResize = LayoutChrome
      AutoSaveVisible = True
      AutoSave = True
      AutoSaveCaption = 'AutoSave'
      OnAutoSaveChange = AutoSaveChanged
      Buttons = <
        item
          Icon.BuiltIn = ei027Instellingen
          Hint = 'Light / dark'
          OnClick = ToggleTheme
        end>
      QuickAccess = <
        item
          Icon.BuiltIn = ei003Opslaan
          Hint = 'Save'
          OnClick = QuickAccessClicked
        end
        item
          Icon.BuiltIn = ei010Ongedaan
          Hint = 'Undo'
          OnClick = QuickAccessClicked
        end
        item
          Icon.BuiltIn = ei011Opnieuw
          Hint = 'Redo'
          OnClick = QuickAccessClicked
        end>
      Menu = MainMenu
      MenuPlacement = mpBelow
      CommandStyle = csRibbon
      Ribbon = Ribbon
      Title = 'ERD Components'
      Subtitle = 'General VCL showcase'
      AppInitials = 'ERD'
      StatusCaption = 'CONNECTED'
      OnCommandStyleChange = LayoutChrome
    end
    object Ribbon: TERDRibbon
      Left = 0
      Top = 48
      Width = 1280
      Height = 140
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnResize = LayoutChrome
      Tabs = <
        item
          Caption = 'General'
          Groups = <
            item
              Caption = 'Application'
              Items = <
                item
                  Icon.BuiltIn = ei002Openen
                  Caption = 'Open'
                  Size = risLarge
                  OnClick = OpenFile
                end
                item
                  Icon.BuiltIn = ei028Info
                  Caption = 'Dialog'
                  Size = risLarge
                  OnClick = ShowDialog
                end
                item
                  Icon.BuiltIn = ei027Instellingen
                  Caption = 'Light / dark'
                  Size = risLarge
                  OnClick = ToggleTheme
                end>
            end
            item
              Control = RibbonInputs
              ControlWidth = 220
              Caption = 'Inputs'
              Items = <>
            end
            item
              Control = RibbonGallery
              ControlWidth = 280
              Caption = 'Style gallery'
              Items = <>
            end>
        end
        item
          Caption = 'Media'
          Groups = <
            item
              Caption = 'Player'
              Items = <
                item
                  Icon.BuiltIn = ei161Afspelen
                  Caption = 'Play'
                  Size = risLarge
                  Kind = rikToggle
                  Down = True
                  GroupIndex = 1
                end
                item
                  Icon.BuiltIn = ei162Pauzeren
                  Caption = 'Pause'
                  Size = risLarge
                  Kind = rikToggle
                  GroupIndex = 1
                end
                item
                  Icon.BuiltIn = ei249Teletext
                  Caption = 'Teletext'
                  Size = risLarge
                  Kind = rikToggle
                end
                item
                  Icon.BuiltIn = ei192Ondertiteling
                  Caption = 'Subtitles'
                  Size = risLarge
                  Kind = rikToggle
                end>
            end>
        end>
      ContextualTabs = <>
      FileCaption = 'File'
      Backstage = DemoBackstage
      SearchHint = 'Search commands  Alt+Q'
      OnFileClick = ShowBackstage
      object RibbonInputs: TERDPanel
        Left = 185
        Top = 42
        Width = 218
        Height = 72
        TabOrder = 1
        BorderWidth = 0
        object RibbonSearch: TERDSearchEdit
          Left = 2
          Top = 2
          Width = 214
          Height = 26
          TabOrder = 0
          Text = ''
          TextHint = 'Search content'
          SearchDelay = 0
          OnSearch = RibbonInputChanged
        end
        object RibbonCombo: TERDComboBox
          Left = 2
          Top = 29
          Width = 140
          Height = 26
          TabOrder = 1
          TabStop = False
          Style = ecsDropDown
          FilterMode = cfsContains
          Text = 'Normal'
          Items.Strings = (
            'Normal'
            'Heading'
            'Subtitle')
          OnChange = RibbonInputChanged
        end
        object RibbonSpin: TERDSpinEdit
          Left = 148
          Top = 29
          Width = 68
          Height = 26
          TabOrder = 2
          Text = '12'
          OnChange = RibbonInputChanged
          Value = 12.000000000000000000
          MinValue = 6.000000000000000000
          MaxValue = 72.000000000000000000
          Increment = 1.000000000000000000
        end
      end
      object RibbonGallery: TERDTileGrid
        Left = 422
        Top = 42
        Width = 278
        Height = 72
        TabOrder = 2
        TabStop = True
        Items = <
          item
            Caption = 'Normal'
            Subtitle = 'Body text'
          end
          item
            Caption = 'Heading'
            Subtitle = 'Title'
          end
          item
            Caption = 'Subtitle'
            Subtitle = 'Secondary'
          end>
        TileWidth = 80
        TileHeight = 52
        Spacing = 4
        ShowGroups = False
        ItemIndex = 0
        OnChange = RibbonGalleryChanged
      end
    end
    object Choices: TERDPanel
      Left = 0
      Top = 204
      Width = 970
      Height = 40
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      BorderWidth = 0
      object ThemeChoice: TERDComboBox
        Left = 0
        Top = 2
        Width = 220
        Height = 26
        TabOrder = 0
        Items.Strings = (
          'ERDesigns Light'
          'ERDesigns Dark'
          'Windows'
          'Office Light'
          'Office Dark'
          'Office Word Light'
          'Office Word Dark'
          'Office Excel Light'
          'Office Excel Dark'
          'Office Access Light'
          'Office Access Dark'
          'Office PowerPoint Light'
          'Office PowerPoint Dark'
          'Office Outlook Light'
          'Office Outlook Dark'
          'Office OneNote Light'
          'Office OneNote Dark')
        ItemIndex = 0
        OnChange = ThemeSelected
      end
      object AppearanceChoice: TERDComboBox
        Left = 240
        Top = 2
        Width = 220
        Height = 26
        TabOrder = 1
        Items.Strings = (
          'ERDesigns ribbon'
          'Office ribbon'
          'Flat ribbon')
        ItemIndex = 0
        OnChange = AppearanceSelected
      end
      object LayoutChoice: TERDComboBox
        Left = 480
        Top = 2
        Width = 220
        Height = 26
        TabOrder = 2
        Items.Strings = (
          'Classic layout'
          'Simplified layout')
        ItemIndex = 0
        OnChange = LayoutSelected
      end
      object CommandChoice: TERDComboBox
        Left = 720
        Top = 2
        Width = 220
        Height = 26
        TabOrder = 3
        Items.Strings = (
          'Ribbon'
          'Menu')
        ItemIndex = 0
        OnChange = CommandSelected
      end
    end
  end
  object Sidebar: TERDSidebar
    Left = 0
    Top = 244
    Width = 208
    Height = 488
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    Items = <
      item
        Icon.BuiltIn = ei028Info
        Caption = 'Overview'
      end
      item
        Icon.BuiltIn = ei005Bestand
        Caption = 'General icons'
        Tag = 112
      end
      item
        Icon.BuiltIn = ei027Instellingen
        Caption = 'Desktop'
        Tag = 576
      end
      item
        Icon.BuiltIn = ei081Database
        Caption = 'Data'
        Tag = 1040
      end
      item
        Icon.BuiltIn = ei161Afspelen
        Caption = 'Media player'
        Tag = 1968
      end
      item
        Icon.BuiltIn = ei017Download
        Caption = 'Playlists'
        Badge = 5
        Tag = 2432
      end
      item
        Icon.BuiltIn = ei249Teletext
        Caption = 'Live TV / VOD'
        Badge = 12
        Tag = 2896
      end
      item
        Icon.BuiltIn = ei027Instellingen
        Caption = 'Settings'
        Footer = True
      end
      item
        Icon.BuiltIn = ei005Bestand
        Caption = 'Inputs'
        Tag = -1
      end
      item
        Icon.BuiltIn = ei005Bestand
        Caption = 'Layout'
        Tag = -2
      end
      item
        Icon.BuiltIn = ei005Bestand
        Caption = 'Data'
        Tag = -3
      end
      item
        Icon.BuiltIn = ei005Bestand
        Caption = 'Files and colors'
        Tag = -4
      end
      item
        Icon.BuiltIn = ei005Bestand
        Caption = 'Dialogs'
        Tag = -5
      end>
    ItemIndex = 0
    CollapseCaption = 'Collapse'
    OnItemClick = SidebarSelected
  end
  object DemoPages: TERDPanel
    Left = 208
    Top = 244
    Width = 1072
    Height = 488
    Align = alClient
    TabOrder = 2
    BorderWidth = 0
    object PageTabs: TERDTabs
      Left = 0
      Top = 0
      Width = 1072
      Height = 40
      Align = alTop
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Tabs = <
        item
          Caption = 'Icons / themes'
        end
        item
          Caption = 'Inputs'
        end
        item
          Caption = 'Layout'
        end
        item
          Caption = 'Data'
        end
        item
          Caption = 'Files / colors'
        end
        item
          Caption = 'Dialogs / notifications'
        end>
      TabIndex = 0
      OnChange = PageSelected
    end
    object IconsPage: TERDPanel
      Left = 0
      Top = 40
      Width = 1072
      Height = 448
      Align = alClient
      TabOrder = 1
      BorderWidth = 0
      object Gallery: TERDScrollBox
        Left = 0
        Top = 0
        Width = 1072
        Height = 448
        Align = alClient
        TabOrder = 0
        object ImageButton: TERDButton
          Left = 12
          Top = 8
          Width = 184
          Height = 48
          TabOrder = 0
          AutoSize = False
          Caption = 'ImageList source'
          Icon.Source = eisImageList
          Icon.Images = Images
          Icon.ImageIndex = 0
        end
        object GlyphButton: TERDButton
          Left = 212
          Top = 8
          Width = 184
          Height = 48
          TabOrder = 1
          AutoSize = False
          Caption = 'Own glyph'
          Icon.Source = eisGlyph
        end
        object NoIconButton: TERDButton
          Left = 412
          Top = 8
          Width = 184
          Height = 48
          TabOrder = 2
          AutoSize = False
          Caption = 'No icon'
          Icon.Source = eisNone
        end
        object IconButton001: TERDButton
          Left = 12
          Top = 112
          Width = 184
          Height = 48
          TabOrder = 3
          OnClick = IconClicked
          AutoSize = False
          Caption = '001 Nieuw'
          Icon.BuiltIn = ei001Nieuw
        end
        object IconButton002: TERDButton
          Left = 206
          Top = 112
          Width = 184
          Height = 48
          TabOrder = 4
          OnClick = IconClicked
          AutoSize = False
          Caption = '002 Openen'
          Icon.BuiltIn = ei002Openen
        end
        object IconButton003: TERDButton
          Left = 400
          Top = 112
          Width = 184
          Height = 48
          TabOrder = 5
          OnClick = IconClicked
          AutoSize = False
          Caption = '003 Opslaan'
          Icon.BuiltIn = ei003Opslaan
        end
        object IconButton004: TERDButton
          Left = 594
          Top = 112
          Width = 184
          Height = 48
          TabOrder = 6
          OnClick = IconClicked
          AutoSize = False
          Caption = '004 Opslaan als'
          Icon.BuiltIn = ei004Opslaanals
        end
        object IconButton005: TERDButton
          Left = 788
          Top = 112
          Width = 184
          Height = 48
          TabOrder = 7
          OnClick = IconClicked
          AutoSize = False
          Caption = '005 Bestand'
          Icon.BuiltIn = ei005Bestand
        end
        object IconButton006: TERDButton
          Left = 12
          Top = 170
          Width = 184
          Height = 48
          TabOrder = 8
          OnClick = IconClicked
          AutoSize = False
          Caption = '006 Map'
          Icon.BuiltIn = ei006Map
        end
        object IconButton007: TERDButton
          Left = 206
          Top = 170
          Width = 184
          Height = 48
          TabOrder = 9
          OnClick = IconClicked
          AutoSize = False
          Caption = '007 Kopi'#235'ren'
          Icon.BuiltIn = ei007Kopieren
        end
        object IconButton008: TERDButton
          Left = 400
          Top = 170
          Width = 184
          Height = 48
          TabOrder = 10
          OnClick = IconClicked
          AutoSize = False
          Caption = '008 Plakken'
          Icon.BuiltIn = ei008Plakken
        end
        object IconButton009: TERDButton
          Left = 594
          Top = 170
          Width = 184
          Height = 48
          TabOrder = 11
          OnClick = IconClicked
          AutoSize = False
          Caption = '009 Knippen'
          Icon.BuiltIn = ei009Knippen
        end
        object IconButton010: TERDButton
          Left = 788
          Top = 170
          Width = 184
          Height = 48
          TabOrder = 12
          OnClick = IconClicked
          AutoSize = False
          Caption = '010 Ongedaan'
          Icon.BuiltIn = ei010Ongedaan
        end
        object IconButton011: TERDButton
          Left = 12
          Top = 228
          Width = 184
          Height = 48
          TabOrder = 13
          OnClick = IconClicked
          AutoSize = False
          Caption = '011 Opnieuw'
          Icon.BuiltIn = ei011Opnieuw
        end
        object IconButton012: TERDButton
          Left = 206
          Top = 228
          Width = 184
          Height = 48
          TabOrder = 14
          OnClick = IconClicked
          AutoSize = False
          Caption = '012 Verwijderen'
          Icon.BuiltIn = ei012Verwijderen
        end
        object IconButton013: TERDButton
          Left = 400
          Top = 228
          Width = 184
          Height = 48
          TabOrder = 15
          OnClick = IconClicked
          AutoSize = False
          Caption = '013 Bewerken'
          Icon.BuiltIn = ei013Bewerken
        end
        object IconButton014: TERDButton
          Left = 594
          Top = 228
          Width = 184
          Height = 48
          TabOrder = 16
          OnClick = IconClicked
          AutoSize = False
          Caption = '014 Zoeken'
          Icon.BuiltIn = ei014Zoeken
        end
        object IconButton015: TERDButton
          Left = 788
          Top = 228
          Width = 184
          Height = 48
          TabOrder = 17
          OnClick = IconClicked
          AutoSize = False
          Caption = '015 Filter'
          Icon.BuiltIn = ei015Filter
        end
        object IconButton016: TERDButton
          Left = 12
          Top = 286
          Width = 184
          Height = 48
          TabOrder = 18
          OnClick = IconClicked
          AutoSize = False
          Caption = '016 Vernieuwen'
          Icon.BuiltIn = ei016Vernieuwen
        end
        object IconButton017: TERDButton
          Left = 206
          Top = 286
          Width = 184
          Height = 48
          TabOrder = 19
          OnClick = IconClicked
          AutoSize = False
          Caption = '017 Download'
          Icon.BuiltIn = ei017Download
        end
        object IconButton018: TERDButton
          Left = 400
          Top = 286
          Width = 184
          Height = 48
          TabOrder = 20
          OnClick = IconClicked
          AutoSize = False
          Caption = '018 Upload'
          Icon.BuiltIn = ei018Upload
        end
        object IconButton019: TERDButton
          Left = 594
          Top = 286
          Width = 184
          Height = 48
          TabOrder = 21
          OnClick = IconClicked
          AutoSize = False
          Caption = '019 Import'
          Icon.BuiltIn = ei019Import
        end
        object IconButton020: TERDButton
          Left = 788
          Top = 286
          Width = 184
          Height = 48
          TabOrder = 22
          OnClick = IconClicked
          AutoSize = False
          Caption = '020 Export'
          Icon.BuiltIn = ei020Export
        end
        object IconButton021: TERDButton
          Left = 12
          Top = 344
          Width = 184
          Height = 48
          TabOrder = 23
          OnClick = IconClicked
          AutoSize = False
          Caption = '021 Print'
          Icon.BuiltIn = ei021Print
        end
        object IconButton022: TERDButton
          Left = 206
          Top = 344
          Width = 184
          Height = 48
          TabOrder = 24
          OnClick = IconClicked
          AutoSize = False
          Caption = '022 PDF'
          Icon.BuiltIn = ei022PDF
        end
        object IconButton023: TERDButton
          Left = 400
          Top = 344
          Width = 184
          Height = 48
          TabOrder = 25
          OnClick = IconClicked
          AutoSize = False
          Caption = '023 Rapport'
          Icon.BuiltIn = ei023Rapport
        end
        object IconButton024: TERDButton
          Left = 594
          Top = 344
          Width = 184
          Height = 48
          TabOrder = 26
          OnClick = IconClicked
          AutoSize = False
          Caption = '024 Mail'
          Icon.BuiltIn = ei024Mail
        end
        object IconButton025: TERDButton
          Left = 788
          Top = 344
          Width = 184
          Height = 48
          TabOrder = 27
          OnClick = IconClicked
          AutoSize = False
          Caption = '025 Gebruiker'
          Icon.BuiltIn = ei025Gebruiker
        end
        object IconButton026: TERDButton
          Left = 12
          Top = 402
          Width = 184
          Height = 48
          TabOrder = 28
          OnClick = IconClicked
          AutoSize = False
          Caption = '026 Gebruikers'
          Icon.BuiltIn = ei026Gebruikers
        end
        object IconButton027: TERDButton
          Left = 206
          Top = 402
          Width = 184
          Height = 48
          TabOrder = 29
          OnClick = IconClicked
          AutoSize = False
          Caption = '027 Instellingen'
          Icon.BuiltIn = ei027Instellingen
        end
        object IconButton028: TERDButton
          Left = 400
          Top = 402
          Width = 184
          Height = 48
          TabOrder = 30
          OnClick = IconClicked
          AutoSize = False
          Caption = '028 Info'
          Icon.BuiltIn = ei028Info
        end
        object IconButton029: TERDButton
          Left = 594
          Top = 402
          Width = 184
          Height = 48
          TabOrder = 31
          OnClick = IconClicked
          AutoSize = False
          Caption = '029 Help'
          Icon.BuiltIn = ei029Help
        end
        object IconButton030: TERDButton
          Left = 788
          Top = 402
          Width = 184
          Height = 48
          TabOrder = 32
          OnClick = IconClicked
          AutoSize = False
          Caption = '030 Waarschuwing'
          Icon.BuiltIn = ei030Waarschuwing
        end
        object IconButton031: TERDButton
          Left = 12
          Top = 460
          Width = 184
          Height = 48
          TabOrder = 33
          OnClick = IconClicked
          AutoSize = False
          Caption = '031 Fout'
          Icon.BuiltIn = ei031Fout
        end
        object IconButton032: TERDButton
          Left = 206
          Top = 460
          Width = 184
          Height = 48
          TabOrder = 34
          OnClick = IconClicked
          AutoSize = False
          Caption = '032 Succes'
          Icon.BuiltIn = ei032Succes
        end
        object IconButton033: TERDButton
          Left = 400
          Top = 460
          Width = 184
          Height = 48
          TabOrder = 35
          OnClick = IconClicked
          AutoSize = False
          Caption = '033 Terug'
          Icon.BuiltIn = ei033Terug
        end
        object IconButton034: TERDButton
          Left = 594
          Top = 460
          Width = 184
          Height = 48
          TabOrder = 36
          OnClick = IconClicked
          AutoSize = False
          Caption = '034 Vooruit'
          Icon.BuiltIn = ei034Vooruit
        end
        object IconButton035: TERDButton
          Left = 788
          Top = 460
          Width = 184
          Height = 48
          TabOrder = 37
          OnClick = IconClicked
          AutoSize = False
          Caption = '035 Omhoog'
          Icon.BuiltIn = ei035Omhoog
        end
        object IconButton036: TERDButton
          Left = 12
          Top = 518
          Width = 184
          Height = 48
          TabOrder = 38
          OnClick = IconClicked
          AutoSize = False
          Caption = '036 Omlaag'
          Icon.BuiltIn = ei036Omlaag
        end
        object IconButton037: TERDButton
          Left = 206
          Top = 518
          Width = 184
          Height = 48
          TabOrder = 39
          OnClick = IconClicked
          AutoSize = False
          Caption = '037 Vergrendelen'
          Icon.BuiltIn = ei037Vergrendelen
        end
        object IconButton038: TERDButton
          Left = 400
          Top = 518
          Width = 184
          Height = 48
          TabOrder = 40
          OnClick = IconClicked
          AutoSize = False
          Caption = '038 Ontgrendelen'
          Icon.BuiltIn = ei038Ontgrendelen
        end
        object IconButton039: TERDButton
          Left = 594
          Top = 518
          Width = 184
          Height = 48
          TabOrder = 41
          OnClick = IconClicked
          AutoSize = False
          Caption = '039 Meldingen'
          Icon.BuiltIn = ei039Meldingen
        end
        object IconButton040: TERDButton
          Left = 788
          Top = 518
          Width = 184
          Height = 48
          TabOrder = 42
          OnClick = IconClicked
          AutoSize = False
          Caption = '040 Meer'
          Icon.BuiltIn = ei040Meer
        end
        object IconButton041: TERDButton
          Left = 12
          Top = 576
          Width = 184
          Height = 48
          TabOrder = 43
          OnClick = IconClicked
          AutoSize = False
          Caption = '041 Minimaliseren'
          Icon.BuiltIn = ei041Minimaliseren
        end
        object IconButton042: TERDButton
          Left = 206
          Top = 576
          Width = 184
          Height = 48
          TabOrder = 44
          OnClick = IconClicked
          AutoSize = False
          Caption = '042 Maximaliseren'
          Icon.BuiltIn = ei042Maximaliseren
        end
        object IconButton043: TERDButton
          Left = 400
          Top = 576
          Width = 184
          Height = 48
          TabOrder = 45
          OnClick = IconClicked
          AutoSize = False
          Caption = '043 Herstellen'
          Icon.BuiltIn = ei043Herstellen
        end
        object IconButton044: TERDButton
          Left = 594
          Top = 576
          Width = 184
          Height = 48
          TabOrder = 46
          OnClick = IconClicked
          AutoSize = False
          Caption = '044 Volledig scherm'
          Icon.BuiltIn = ei044Volledigscherm
        end
        object IconButton045: TERDButton
          Left = 788
          Top = 576
          Width = 184
          Height = 48
          TabOrder = 47
          OnClick = IconClicked
          AutoSize = False
          Caption = '045 Scherm verlaten'
          Icon.BuiltIn = ei045Schermverlaten
        end
        object IconButton046: TERDButton
          Left = 12
          Top = 634
          Width = 184
          Height = 48
          TabOrder = 48
          OnClick = IconClicked
          AutoSize = False
          Caption = '046 Venster sluiten'
          Icon.BuiltIn = ei046Venstersluiten
        end
        object IconButton047: TERDButton
          Left = 206
          Top = 634
          Width = 184
          Height = 48
          TabOrder = 49
          OnClick = IconClicked
          AutoSize = False
          Caption = '047 Venster wisselen'
          Icon.BuiltIn = ei047Vensterwisselen
        end
        object IconButton048: TERDButton
          Left = 400
          Top = 634
          Width = 184
          Height = 48
          TabOrder = 50
          OnClick = IconClicked
          AutoSize = False
          Caption = '048 Vensters naast elkaar'
          Icon.BuiltIn = ei048Venstersnaastelkaar
        end
        object IconButton049: TERDButton
          Left = 594
          Top = 634
          Width = 184
          Height = 48
          TabOrder = 51
          OnClick = IconClicked
          AutoSize = False
          Caption = '049 Nieuwe tab'
          Icon.BuiltIn = ei049Nieuwetab
        end
        object IconButton050: TERDButton
          Left = 788
          Top = 634
          Width = 184
          Height = 48
          TabOrder = 52
          OnClick = IconClicked
          AutoSize = False
          Caption = '050 Tab sluiten'
          Icon.BuiltIn = ei050Tabsluiten
        end
        object IconButton051: TERDButton
          Left = 12
          Top = 692
          Width = 184
          Height = 48
          TabOrder = 53
          OnClick = IconClicked
          AutoSize = False
          Caption = '051 Volgende tab'
          Icon.BuiltIn = ei051Volgendetab
        end
        object IconButton052: TERDButton
          Left = 206
          Top = 692
          Width = 184
          Height = 48
          TabOrder = 54
          OnClick = IconClicked
          AutoSize = False
          Caption = '052 Vorige tab'
          Icon.BuiltIn = ei052Vorigetab
        end
        object IconButton053: TERDButton
          Left = 400
          Top = 692
          Width = 184
          Height = 48
          TabOrder = 55
          OnClick = IconClicked
          AutoSize = False
          Caption = '053 Vastzetten'
          Icon.BuiltIn = ei053Vastzetten
        end
        object IconButton054: TERDButton
          Left = 594
          Top = 692
          Width = 184
          Height = 48
          TabOrder = 56
          OnClick = IconClicked
          AutoSize = False
          Caption = '054 Losmaken'
          Icon.BuiltIn = ei054Losmaken
        end
        object IconButton055: TERDButton
          Left = 788
          Top = 692
          Width = 184
          Height = 48
          TabOrder = 57
          OnClick = IconClicked
          AutoSize = False
          Caption = '055 Paneel inklappen'
          Icon.BuiltIn = ei055Paneelinklappen
        end
        object IconButton056: TERDButton
          Left = 12
          Top = 750
          Width = 184
          Height = 48
          TabOrder = 58
          OnClick = IconClicked
          AutoSize = False
          Caption = '056 Paneel uitklappen'
          Icon.BuiltIn = ei056Paneeluitklappen
        end
        object IconButton057: TERDButton
          Left = 206
          Top = 750
          Width = 184
          Height = 48
          TabOrder = 59
          OnClick = IconClicked
          AutoSize = False
          Caption = '057 Zijbalk tonen'
          Icon.BuiltIn = ei057Zijbalktonen
        end
        object IconButton058: TERDButton
          Left = 400
          Top = 750
          Width = 184
          Height = 48
          TabOrder = 60
          OnClick = IconClicked
          AutoSize = False
          Caption = '058 Zijbalk verbergen'
          Icon.BuiltIn = ei058Zijbalkverbergen
        end
        object IconButton059: TERDButton
          Left = 594
          Top = 750
          Width = 184
          Height = 48
          TabOrder = 61
          OnClick = IconClicked
          AutoSize = False
          Caption = '059 Rasterweergave'
          Icon.BuiltIn = ei059Rasterweergave
        end
        object IconButton060: TERDButton
          Left = 788
          Top = 750
          Width = 184
          Height = 48
          TabOrder = 62
          OnClick = IconClicked
          AutoSize = False
          Caption = '060 Lijstweergave'
          Icon.BuiltIn = ei060Lijstweergave
        end
        object IconButton061: TERDButton
          Left = 12
          Top = 808
          Width = 184
          Height = 48
          TabOrder = 63
          OnClick = IconClicked
          AutoSize = False
          Caption = '061 Detailweergave'
          Icon.BuiltIn = ei061Detailweergave
        end
        object IconButton062: TERDButton
          Left = 206
          Top = 808
          Width = 184
          Height = 48
          TabOrder = 64
          OnClick = IconClicked
          AutoSize = False
          Caption = '062 Kaartweergave'
          Icon.BuiltIn = ei062Kaartweergave
        end
        object IconButton063: TERDButton
          Left = 400
          Top = 808
          Width = 184
          Height = 48
          TabOrder = 65
          OnClick = IconClicked
          AutoSize = False
          Caption = '063 Alles selecteren'
          Icon.BuiltIn = ei063Allesselecteren
        end
        object IconButton064: TERDButton
          Left = 594
          Top = 808
          Width = 184
          Height = 48
          TabOrder = 66
          OnClick = IconClicked
          AutoSize = False
          Caption = '064 Selectie wissen'
          Icon.BuiltIn = ei064Selectiewissen
        end
        object IconButton065: TERDButton
          Left = 788
          Top = 808
          Width = 184
          Height = 48
          TabOrder = 67
          OnClick = IconClicked
          AutoSize = False
          Caption = '065 Selectie omkeren'
          Icon.BuiltIn = ei065Selectieomkeren
        end
        object IconButton066: TERDButton
          Left = 12
          Top = 866
          Width = 184
          Height = 48
          TabOrder = 68
          OnClick = IconClicked
          AutoSize = False
          Caption = '066 Zoom in'
          Icon.BuiltIn = ei066Zoomin
        end
        object IconButton067: TERDButton
          Left = 206
          Top = 866
          Width = 184
          Height = 48
          TabOrder = 69
          OnClick = IconClicked
          AutoSize = False
          Caption = '067 Zoom uit'
          Icon.BuiltIn = ei067Zoomuit
        end
        object IconButton068: TERDButton
          Left = 400
          Top = 866
          Width = 184
          Height = 48
          TabOrder = 70
          OnClick = IconClicked
          AutoSize = False
          Caption = '068 Zoom herstellen'
          Icon.BuiltIn = ei068Zoomherstellen
        end
        object IconButton069: TERDButton
          Left = 594
          Top = 866
          Width = 184
          Height = 48
          TabOrder = 71
          OnClick = IconClicked
          AutoSize = False
          Caption = '069 Nieuwe map'
          Icon.BuiltIn = ei069Nieuwemap
        end
        object IconButton070: TERDButton
          Left = 788
          Top = 866
          Width = 184
          Height = 48
          TabOrder = 72
          OnClick = IconClicked
          AutoSize = False
          Caption = '070 Map hernoemen'
          Icon.BuiltIn = ei070Maphernoemen
        end
        object IconButton071: TERDButton
          Left = 12
          Top = 924
          Width = 184
          Height = 48
          TabOrder = 73
          OnClick = IconClicked
          AutoSize = False
          Caption = '071 Bestand hernoemen'
          Icon.BuiltIn = ei071Bestandhernoemen
        end
        object IconButton072: TERDButton
          Left = 206
          Top = 924
          Width = 184
          Height = 48
          TabOrder = 74
          OnClick = IconClicked
          AutoSize = False
          Caption = '072 Dupliceren'
          Icon.BuiltIn = ei072Dupliceren
        end
        object IconButton073: TERDButton
          Left = 400
          Top = 924
          Width = 184
          Height = 48
          TabOrder = 75
          OnClick = IconClicked
          AutoSize = False
          Caption = '073 Archief maken'
          Icon.BuiltIn = ei073Archiefmaken
        end
        object IconButton074: TERDButton
          Left = 594
          Top = 924
          Width = 184
          Height = 48
          TabOrder = 76
          OnClick = IconClicked
          AutoSize = False
          Caption = '074 Uitpakken'
          Icon.BuiltIn = ei074Uitpakken
        end
        object IconButton075: TERDButton
          Left = 788
          Top = 924
          Width = 184
          Height = 48
          TabOrder = 77
          OnClick = IconClicked
          AutoSize = False
          Caption = '075 Alles opslaan'
          Icon.BuiltIn = ei075Allesopslaan
        end
        object IconButton076: TERDButton
          Left = 12
          Top = 982
          Width = 184
          Height = 48
          TabOrder = 78
          OnClick = IconClicked
          AutoSize = False
          Caption = '076 Printvoorbeeld'
          Icon.BuiltIn = ei076Printvoorbeeld
        end
        object IconButton077: TERDButton
          Left = 206
          Top = 982
          Width = 184
          Height = 48
          TabOrder = 79
          OnClick = IconClicked
          AutoSize = False
          Caption = '077 Pagina-instelling'
          Icon.BuiltIn = ei077Paginainstelling
        end
        object IconButton078: TERDButton
          Left = 400
          Top = 982
          Width = 184
          Height = 48
          TabOrder = 80
          OnClick = IconClicked
          AutoSize = False
          Caption = '078 Toetsenbord'
          Icon.BuiltIn = ei078Toetsenbord
        end
        object IconButton079: TERDButton
          Left = 594
          Top = 982
          Width = 184
          Height = 48
          TabOrder = 81
          OnClick = IconClicked
          AutoSize = False
          Caption = '079 Muis'
          Icon.BuiltIn = ei079Muis
        end
        object IconButton080: TERDButton
          Left = 788
          Top = 982
          Width = 184
          Height = 48
          TabOrder = 82
          OnClick = IconClicked
          AutoSize = False
          Caption = '080 Touch'
          Icon.BuiltIn = ei080Touch
        end
        object IconButton081: TERDButton
          Left = 12
          Top = 1040
          Width = 184
          Height = 48
          TabOrder = 83
          OnClick = IconClicked
          AutoSize = False
          Caption = '081 Database'
          Icon.BuiltIn = ei081Database
        end
        object IconButton082: TERDButton
          Left = 206
          Top = 1040
          Width = 184
          Height = 48
          TabOrder = 84
          OnClick = IconClicked
          AutoSize = False
          Caption = '082 Database toevoegen'
          Icon.BuiltIn = ei082Databasetoevoegen
        end
        object IconButton083: TERDButton
          Left = 400
          Top = 1040
          Width = 184
          Height = 48
          TabOrder = 85
          OnClick = IconClicked
          AutoSize = False
          Caption = '083 Database verwijderen'
          Icon.BuiltIn = ei083Databaseverwijderen
        end
        object IconButton084: TERDButton
          Left = 594
          Top = 1040
          Width = 184
          Height = 48
          TabOrder = 86
          OnClick = IconClicked
          AutoSize = False
          Caption = '084 Database verbinden'
          Icon.BuiltIn = ei084Databaseverbinden
        end
        object IconButton085: TERDButton
          Left = 788
          Top = 1040
          Width = 184
          Height = 48
          TabOrder = 87
          OnClick = IconClicked
          AutoSize = False
          Caption = '085 Database backup'
          Icon.BuiltIn = ei085Databasebackup
        end
        object IconButton086: TERDButton
          Left = 12
          Top = 1098
          Width = 184
          Height = 48
          TabOrder = 88
          OnClick = IconClicked
          AutoSize = False
          Caption = '086 Database herstellen'
          Icon.BuiltIn = ei086Databaseherstellen
        end
        object IconButton087: TERDButton
          Left = 206
          Top = 1098
          Width = 184
          Height = 48
          TabOrder = 89
          OnClick = IconClicked
          AutoSize = False
          Caption = '087 Tabel'
          Icon.BuiltIn = ei087Tabel
        end
        object IconButton088: TERDButton
          Left = 400
          Top = 1098
          Width = 184
          Height = 48
          TabOrder = 90
          OnClick = IconClicked
          AutoSize = False
          Caption = '088 Rij toevoegen'
          Icon.BuiltIn = ei088Rijtoevoegen
        end
        object IconButton089: TERDButton
          Left = 594
          Top = 1098
          Width = 184
          Height = 48
          TabOrder = 91
          OnClick = IconClicked
          AutoSize = False
          Caption = '089 Rij verwijderen'
          Icon.BuiltIn = ei089Rijverwijderen
        end
        object IconButton090: TERDButton
          Left = 788
          Top = 1098
          Width = 184
          Height = 48
          TabOrder = 92
          OnClick = IconClicked
          AutoSize = False
          Caption = '090 Kolom toevoegen'
          Icon.BuiltIn = ei090Kolomtoevoegen
        end
        object IconButton091: TERDButton
          Left = 12
          Top = 1156
          Width = 184
          Height = 48
          TabOrder = 93
          OnClick = IconClicked
          AutoSize = False
          Caption = '091 Kolom verwijderen'
          Icon.BuiltIn = ei091Kolomverwijderen
        end
        object IconButton092: TERDButton
          Left = 206
          Top = 1156
          Width = 184
          Height = 48
          TabOrder = 94
          OnClick = IconClicked
          AutoSize = False
          Caption = '092 Sorteren oplopend'
          Icon.BuiltIn = ei092Sorterenoplopend
        end
        object IconButton093: TERDButton
          Left = 400
          Top = 1156
          Width = 184
          Height = 48
          TabOrder = 95
          OnClick = IconClicked
          AutoSize = False
          Caption = '093 Sorteren aflopend'
          Icon.BuiltIn = ei093Sorterenaflopend
        end
        object IconButton094: TERDButton
          Left = 594
          Top = 1156
          Width = 184
          Height = 48
          TabOrder = 96
          OnClick = IconClicked
          AutoSize = False
          Caption = '094 Groeperen'
          Icon.BuiltIn = ei094Groeperen
        end
        object IconButton095: TERDButton
          Left = 788
          Top = 1156
          Width = 184
          Height = 48
          TabOrder = 97
          OnClick = IconClicked
          AutoSize = False
          Caption = '095 Groep opheffen'
          Icon.BuiltIn = ei095Groepopheffen
        end
        object IconButton096: TERDButton
          Left = 12
          Top = 1214
          Width = 184
          Height = 48
          TabOrder = 98
          OnClick = IconClicked
          AutoSize = False
          Caption = '096 Filter toevoegen'
          Icon.BuiltIn = ei096Filtertoevoegen
        end
        object IconButton097: TERDButton
          Left = 206
          Top = 1214
          Width = 184
          Height = 48
          TabOrder = 99
          OnClick = IconClicked
          AutoSize = False
          Caption = '097 Filters wissen'
          Icon.BuiltIn = ei097Filterswissen
        end
        object IconButton098: TERDButton
          Left = 400
          Top = 1214
          Width = 184
          Height = 48
          TabOrder = 100
          OnClick = IconClicked
          AutoSize = False
          Caption = '098 CSV bestand'
          Icon.BuiltIn = ei098CSVbestand
        end
        object IconButton099: TERDButton
          Left = 594
          Top = 1214
          Width = 184
          Height = 48
          TabOrder = 101
          OnClick = IconClicked
          AutoSize = False
          Caption = '099 JSON bestand'
          Icon.BuiltIn = ei099JSONbestand
        end
        object IconButton100: TERDButton
          Left = 788
          Top = 1214
          Width = 184
          Height = 48
          TabOrder = 102
          OnClick = IconClicked
          AutoSize = False
          Caption = '100 XML bestand'
          Icon.BuiltIn = ei100XMLbestand
        end
        object IconButton101: TERDButton
          Left = 12
          Top = 1272
          Width = 184
          Height = 48
          TabOrder = 103
          OnClick = IconClicked
          AutoSize = False
          Caption = '101 Spreadsheet'
          Icon.BuiltIn = ei101Spreadsheet
        end
        object IconButton102: TERDButton
          Left = 206
          Top = 1272
          Width = 184
          Height = 48
          TabOrder = 104
          OnClick = IconClicked
          AutoSize = False
          Caption = '102 Tekstdocument'
          Icon.BuiltIn = ei102Tekstdocument
        end
        object IconButton103: TERDButton
          Left = 400
          Top = 1272
          Width = 184
          Height = 48
          TabOrder = 105
          OnClick = IconClicked
          AutoSize = False
          Caption = '103 Presentatie'
          Icon.BuiltIn = ei103Presentatie
        end
        object IconButton104: TERDButton
          Left = 594
          Top = 1272
          Width = 184
          Height = 48
          TabOrder = 106
          OnClick = IconClicked
          AutoSize = False
          Caption = '104 Document controleren'
          Icon.BuiltIn = ei104Documentcontroleren
        end
        object IconButton105: TERDButton
          Left = 788
          Top = 1272
          Width = 184
          Height = 48
          TabOrder = 107
          OnClick = IconClicked
          AutoSize = False
          Caption = '105 Document vergelijken'
          Icon.BuiltIn = ei105Documentvergelijken
        end
        object IconButton106: TERDButton
          Left = 12
          Top = 1330
          Width = 184
          Height = 48
          TabOrder = 108
          OnClick = IconClicked
          AutoSize = False
          Caption = '106 Staafgrafiek'
          Icon.BuiltIn = ei106Staafgrafiek
        end
        object IconButton107: TERDButton
          Left = 206
          Top = 1330
          Width = 184
          Height = 48
          TabOrder = 109
          OnClick = IconClicked
          AutoSize = False
          Caption = '107 Lijngrafiek'
          Icon.BuiltIn = ei107Lijngrafiek
        end
        object IconButton108: TERDButton
          Left = 400
          Top = 1330
          Width = 184
          Height = 48
          TabOrder = 110
          OnClick = IconClicked
          AutoSize = False
          Caption = '108 Cirkeldiagram'
          Icon.BuiltIn = ei108Cirkeldiagram
        end
        object IconButton109: TERDButton
          Left = 594
          Top = 1330
          Width = 184
          Height = 48
          TabOrder = 111
          OnClick = IconClicked
          AutoSize = False
          Caption = '109 Vlakgrafiek'
          Icon.BuiltIn = ei109Vlakgrafiek
        end
        object IconButton110: TERDButton
          Left = 788
          Top = 1330
          Width = 184
          Height = 48
          TabOrder = 112
          OnClick = IconClicked
          AutoSize = False
          Caption = '110 Spreidingsgrafiek'
          Icon.BuiltIn = ei110Spreidingsgrafiek
        end
        object IconButton111: TERDButton
          Left = 12
          Top = 1388
          Width = 184
          Height = 48
          TabOrder = 113
          OnClick = IconClicked
          AutoSize = False
          Caption = '111 Histogram'
          Icon.BuiltIn = ei111Histogram
        end
        object IconButton112: TERDButton
          Left = 206
          Top = 1388
          Width = 184
          Height = 48
          TabOrder = 114
          OnClick = IconClicked
          AutoSize = False
          Caption = '112 Dashboard'
          Icon.BuiltIn = ei112Dashboard
        end
        object IconButton113: TERDButton
          Left = 400
          Top = 1388
          Width = 184
          Height = 48
          TabOrder = 115
          OnClick = IconClicked
          AutoSize = False
          Caption = '113 Formule'
          Icon.BuiltIn = ei113Formule
        end
        object IconButton114: TERDButton
          Left = 594
          Top = 1388
          Width = 184
          Height = 48
          TabOrder = 116
          OnClick = IconClicked
          AutoSize = False
          Caption = '114 Berekenen'
          Icon.BuiltIn = ei114Berekenen
        end
        object IconButton115: TERDButton
          Left = 788
          Top = 1388
          Width = 184
          Height = 48
          TabOrder = 117
          OnClick = IconClicked
          AutoSize = False
          Caption = '115 Statistieken'
          Icon.BuiltIn = ei115Statistieken
        end
        object IconButton116: TERDButton
          Left = 12
          Top = 1446
          Width = 184
          Height = 48
          TabOrder = 118
          OnClick = IconClicked
          AutoSize = False
          Caption = '116 Tag'
          Icon.BuiltIn = ei116Tag
        end
        object IconButton117: TERDButton
          Left = 206
          Top = 1446
          Width = 184
          Height = 48
          TabOrder = 119
          OnClick = IconClicked
          AutoSize = False
          Caption = '117 Tags beheren'
          Icon.BuiltIn = ei117Tagsbeheren
        end
        object IconButton118: TERDButton
          Left = 400
          Top = 1446
          Width = 184
          Height = 48
          TabOrder = 120
          OnClick = IconClicked
          AutoSize = False
          Caption = '118 Link toevoegen'
          Icon.BuiltIn = ei118Linktoevoegen
        end
        object IconButton119: TERDButton
          Left = 594
          Top = 1446
          Width = 184
          Height = 48
          TabOrder = 121
          OnClick = IconClicked
          AutoSize = False
          Caption = '119 Link verwijderen'
          Icon.BuiltIn = ei119Linkverwijderen
        end
        object IconButton120: TERDButton
          Left = 788
          Top = 1446
          Width = 184
          Height = 48
          TabOrder = 122
          OnClick = IconClicked
          AutoSize = False
          Caption = '120 Kleurenpalet'
          Icon.BuiltIn = ei120Kleurenpalet
        end
        object IconButton121: TERDButton
          Left = 12
          Top = 1504
          Width = 184
          Height = 48
          TabOrder = 123
          OnClick = IconClicked
          AutoSize = False
          Caption = '121 Aanmelden'
          Icon.BuiltIn = ei121Aanmelden
        end
        object IconButton122: TERDButton
          Left = 206
          Top = 1504
          Width = 184
          Height = 48
          TabOrder = 124
          OnClick = IconClicked
          AutoSize = False
          Caption = '122 Afmelden'
          Icon.BuiltIn = ei122Afmelden
        end
        object IconButton123: TERDButton
          Left = 400
          Top = 1504
          Width = 184
          Height = 48
          TabOrder = 125
          OnClick = IconClicked
          AutoSize = False
          Caption = '123 Profiel bewerken'
          Icon.BuiltIn = ei123Profielbewerken
        end
        object IconButton124: TERDButton
          Left = 594
          Top = 1504
          Width = 184
          Height = 48
          TabOrder = 126
          OnClick = IconClicked
          AutoSize = False
          Caption = '124 Gebruiker toevoegen'
          Icon.BuiltIn = ei124Gebruikertoevoegen
        end
        object IconButton125: TERDButton
          Left = 788
          Top = 1504
          Width = 184
          Height = 48
          TabOrder = 127
          OnClick = IconClicked
          AutoSize = False
          Caption = '125 Gebruiker verwijderen'
          Icon.BuiltIn = ei125Gebruikerverwijderen
        end
        object IconButton126: TERDButton
          Left = 12
          Top = 1562
          Width = 184
          Height = 48
          TabOrder = 128
          OnClick = IconClicked
          AutoSize = False
          Caption = '126 Rollen'
          Icon.BuiltIn = ei126Rollen
        end
        object IconButton127: TERDButton
          Left = 206
          Top = 1562
          Width = 184
          Height = 48
          TabOrder = 129
          OnClick = IconClicked
          AutoSize = False
          Caption = '127 Rechten'
          Icon.BuiltIn = ei127Rechten
        end
        object IconButton128: TERDButton
          Left = 400
          Top = 1562
          Width = 184
          Height = 48
          TabOrder = 130
          OnClick = IconClicked
          AutoSize = False
          Caption = '128 Wachtwoord'
          Icon.BuiltIn = ei128Wachtwoord
        end
        object IconButton129: TERDButton
          Left = 594
          Top = 1562
          Width = 184
          Height = 48
          TabOrder = 131
          OnClick = IconClicked
          AutoSize = False
          Caption = '129 Sleutel'
          Icon.BuiltIn = ei129Sleutel
        end
        object IconButton130: TERDButton
          Left = 788
          Top = 1562
          Width = 184
          Height = 48
          TabOrder = 132
          OnClick = IconClicked
          AutoSize = False
          Caption = '130 Certificaat'
          Icon.BuiltIn = ei130Certificaat
        end
        object IconButton131: TERDButton
          Left = 12
          Top = 1620
          Width = 184
          Height = 48
          TabOrder = 133
          OnClick = IconClicked
          AutoSize = False
          Caption = '131 Certificaat geldig'
          Icon.BuiltIn = ei131Certificaatgeldig
        end
        object IconButton132: TERDButton
          Left = 206
          Top = 1620
          Width = 184
          Height = 48
          TabOrder = 134
          OnClick = IconClicked
          AutoSize = False
          Caption = '132 Certificaat verlopen'
          Icon.BuiltIn = ei132Certificaatverlopen
        end
        object IconButton133: TERDButton
          Left = 400
          Top = 1620
          Width = 184
          Height = 48
          TabOrder = 135
          OnClick = IconClicked
          AutoSize = False
          Caption = '133 Bescherming'
          Icon.BuiltIn = ei133Bescherming
        end
        object IconButton134: TERDButton
          Left = 594
          Top = 1620
          Width = 184
          Height = 48
          TabOrder = 136
          OnClick = IconClicked
          AutoSize = False
          Caption = '134 Privacy'
          Icon.BuiltIn = ei134Privacy
        end
        object IconButton135: TERDButton
          Left = 788
          Top = 1620
          Width = 184
          Height = 48
          TabOrder = 137
          OnClick = IconClicked
          AutoSize = False
          Caption = '135 Versleutelen'
          Icon.BuiltIn = ei135Versleutelen
        end
        object IconButton136: TERDButton
          Left = 12
          Top = 1678
          Width = 184
          Height = 48
          TabOrder = 138
          OnClick = IconClicked
          AutoSize = False
          Caption = '136 Ontsleutelen'
          Icon.BuiltIn = ei136Ontsleutelen
        end
        object IconButton137: TERDButton
          Left = 206
          Top = 1678
          Width = 184
          Height = 48
          TabOrder = 139
          OnClick = IconClicked
          AutoSize = False
          Caption = '137 Firewall'
          Icon.BuiltIn = ei137Firewall
        end
        object IconButton138: TERDButton
          Left = 400
          Top = 1678
          Width = 184
          Height = 48
          TabOrder = 140
          OnClick = IconClicked
          AutoSize = False
          Caption = '138 Proxy'
          Icon.BuiltIn = ei138Proxy
        end
        object IconButton139: TERDButton
          Left = 594
          Top = 1678
          Width = 184
          Height = 48
          TabOrder = 141
          OnClick = IconClicked
          AutoSize = False
          Caption = '139 Ethernet'
          Icon.BuiltIn = ei139Ethernet
        end
        object IconButton140: TERDButton
          Left = 788
          Top = 1678
          Width = 184
          Height = 48
          TabOrder = 142
          OnClick = IconClicked
          AutoSize = False
          Caption = '140 Wi-Fi'
          Icon.BuiltIn = ei140WiFi
        end
        object IconButton141: TERDButton
          Left = 12
          Top = 1736
          Width = 184
          Height = 48
          TabOrder = 143
          OnClick = IconClicked
          AutoSize = False
          Caption = '141 Bluetooth'
          Icon.BuiltIn = ei141Bluetooth
        end
        object IconButton142: TERDButton
          Left = 206
          Top = 1736
          Width = 184
          Height = 48
          TabOrder = 144
          OnClick = IconClicked
          AutoSize = False
          Caption = '142 USB'
          Icon.BuiltIn = ei142USB
        end
        object IconButton143: TERDButton
          Left = 400
          Top = 1736
          Width = 184
          Height = 48
          TabOrder = 145
          OnClick = IconClicked
          AutoSize = False
          Caption = '143 Cloud'
          Icon.BuiltIn = ei143Cloud
        end
        object IconButton144: TERDButton
          Left = 594
          Top = 1736
          Width = 184
          Height = 48
          TabOrder = 146
          OnClick = IconClicked
          AutoSize = False
          Caption = '144 Cloud downloaden'
          Icon.BuiltIn = ei144Clouddownloaden
        end
        object IconButton145: TERDButton
          Left = 788
          Top = 1736
          Width = 184
          Height = 48
          TabOrder = 147
          OnClick = IconClicked
          AutoSize = False
          Caption = '145 Cloud uploaden'
          Icon.BuiltIn = ei145Clouduploaden
        end
        object IconButton146: TERDButton
          Left = 12
          Top = 1794
          Width = 184
          Height = 48
          TabOrder = 148
          OnClick = IconClicked
          AutoSize = False
          Caption = '146 Online'
          Icon.BuiltIn = ei146Online
        end
        object IconButton147: TERDButton
          Left = 206
          Top = 1794
          Width = 184
          Height = 48
          TabOrder = 149
          OnClick = IconClicked
          AutoSize = False
          Caption = '147 Offline'
          Icon.BuiltIn = ei147Offline
        end
        object IconButton148: TERDButton
          Left = 400
          Top = 1794
          Width = 184
          Height = 48
          TabOrder = 150
          OnClick = IconClicked
          AutoSize = False
          Caption = '148 Verbinding verbroken'
          Icon.BuiltIn = ei148Verbindingverbroken
        end
        object IconButton149: TERDButton
          Left = 594
          Top = 1794
          Width = 184
          Height = 48
          TabOrder = 151
          OnClick = IconClicked
          AutoSize = False
          Caption = '149 Synchroniseren'
          Icon.BuiltIn = ei149Synchroniseren
        end
        object IconButton150: TERDButton
          Left = 788
          Top = 1794
          Width = 184
          Height = 48
          TabOrder = 152
          OnClick = IconClicked
          AutoSize = False
          Caption = '150 Sync conflict'
          Icon.BuiltIn = ei150Syncconflict
        end
        object IconButton151: TERDButton
          Left = 12
          Top = 1852
          Width = 184
          Height = 48
          TabOrder = 153
          OnClick = IconClicked
          AutoSize = False
          Caption = '151 Takenlijst'
          Icon.BuiltIn = ei151Takenlijst
        end
        object IconButton152: TERDButton
          Left = 206
          Top = 1852
          Width = 184
          Height = 48
          TabOrder = 154
          OnClick = IconClicked
          AutoSize = False
          Caption = '152 Taak toevoegen'
          Icon.BuiltIn = ei152Taaktoevoegen
        end
        object IconButton153: TERDButton
          Left = 400
          Top = 1852
          Width = 184
          Height = 48
          TabOrder = 155
          OnClick = IconClicked
          AutoSize = False
          Caption = '153 Taak voltooid'
          Icon.BuiltIn = ei153Taakvoltooid
        end
        object IconButton154: TERDButton
          Left = 594
          Top = 1852
          Width = 184
          Height = 48
          TabOrder = 156
          OnClick = IconClicked
          AutoSize = False
          Caption = '154 Taak gepland'
          Icon.BuiltIn = ei154Taakgepland
        end
        object IconButton155: TERDButton
          Left = 788
          Top = 1852
          Width = 184
          Height = 48
          TabOrder = 157
          OnClick = IconClicked
          AutoSize = False
          Caption = '155 Kalender'
          Icon.BuiltIn = ei155Kalender
        end
        object IconButton156: TERDButton
          Left = 12
          Top = 1910
          Width = 184
          Height = 48
          TabOrder = 158
          OnClick = IconClicked
          AutoSize = False
          Caption = '156 Klok'
          Icon.BuiltIn = ei156Klok
        end
        object IconButton157: TERDButton
          Left = 206
          Top = 1910
          Width = 184
          Height = 48
          TabOrder = 159
          OnClick = IconClicked
          AutoSize = False
          Caption = '157 Timer'
          Icon.BuiltIn = ei157Timer
        end
        object IconButton158: TERDButton
          Left = 400
          Top = 1910
          Width = 184
          Height = 48
          TabOrder = 160
          OnClick = IconClicked
          AutoSize = False
          Caption = '158 Stopwatch'
          Icon.BuiltIn = ei158Stopwatch
        end
        object IconButton159: TERDButton
          Left = 594
          Top = 1910
          Width = 184
          Height = 48
          TabOrder = 161
          OnClick = IconClicked
          AutoSize = False
          Caption = '159 Alarm'
          Icon.BuiltIn = ei159Alarm
        end
        object IconButton160: TERDButton
          Left = 788
          Top = 1910
          Width = 184
          Height = 48
          TabOrder = 162
          OnClick = IconClicked
          AutoSize = False
          Caption = '160 Meldingen uit'
          Icon.BuiltIn = ei160Meldingenuit
        end
        object IconButton161: TERDButton
          Left = 12
          Top = 1968
          Width = 184
          Height = 48
          TabOrder = 163
          OnClick = IconClicked
          AutoSize = False
          Caption = '161 Afspelen'
          Icon.BuiltIn = ei161Afspelen
        end
        object IconButton162: TERDButton
          Left = 206
          Top = 1968
          Width = 184
          Height = 48
          TabOrder = 164
          OnClick = IconClicked
          AutoSize = False
          Caption = '162 Pauzeren'
          Icon.BuiltIn = ei162Pauzeren
        end
        object IconButton163: TERDButton
          Left = 400
          Top = 1968
          Width = 184
          Height = 48
          TabOrder = 165
          OnClick = IconClicked
          AutoSize = False
          Caption = '163 Stoppen'
          Icon.BuiltIn = ei163Stoppen
        end
        object IconButton164: TERDButton
          Left = 594
          Top = 1968
          Width = 184
          Height = 48
          TabOrder = 166
          OnClick = IconClicked
          AutoSize = False
          Caption = '164 Volgende track'
          Icon.BuiltIn = ei164Volgendetrack
        end
        object IconButton165: TERDButton
          Left = 788
          Top = 1968
          Width = 184
          Height = 48
          TabOrder = 167
          OnClick = IconClicked
          AutoSize = False
          Caption = '165 Vorige track'
          Icon.BuiltIn = ei165Vorigetrack
        end
        object IconButton166: TERDButton
          Left = 12
          Top = 2026
          Width = 184
          Height = 48
          TabOrder = 168
          OnClick = IconClicked
          AutoSize = False
          Caption = '166 Vooruitspoelen'
          Icon.BuiltIn = ei166Vooruitspoelen
        end
        object IconButton167: TERDButton
          Left = 206
          Top = 2026
          Width = 184
          Height = 48
          TabOrder = 169
          OnClick = IconClicked
          AutoSize = False
          Caption = '167 Terugspoelen'
          Icon.BuiltIn = ei167Terugspoelen
        end
        object IconButton168: TERDButton
          Left = 400
          Top = 2026
          Width = 184
          Height = 48
          TabOrder = 170
          OnClick = IconClicked
          AutoSize = False
          Caption = '168 Opnemen'
          Icon.BuiltIn = ei168Opnemen
        end
        object IconButton169: TERDButton
          Left = 594
          Top = 2026
          Width = 184
          Height = 48
          TabOrder = 171
          OnClick = IconClicked
          AutoSize = False
          Caption = '169 Uitwerpen'
          Icon.BuiltIn = ei169Uitwerpen
        end
        object IconButton170: TERDButton
          Left = 788
          Top = 2026
          Width = 184
          Height = 48
          TabOrder = 172
          OnClick = IconClicked
          AutoSize = False
          Caption = '170 Dempen'
          Icon.BuiltIn = ei170Dempen
        end
        object IconButton171: TERDButton
          Left = 12
          Top = 2084
          Width = 184
          Height = 48
          TabOrder = 173
          OnClick = IconClicked
          AutoSize = False
          Caption = '171 Volume lager'
          Icon.BuiltIn = ei171Volumelager
        end
        object IconButton172: TERDButton
          Left = 206
          Top = 2084
          Width = 184
          Height = 48
          TabOrder = 174
          OnClick = IconClicked
          AutoSize = False
          Caption = '172 Volume hoger'
          Icon.BuiltIn = ei172Volumehoger
        end
        object IconButton173: TERDButton
          Left = 400
          Top = 2084
          Width = 184
          Height = 48
          TabOrder = 175
          OnClick = IconClicked
          AutoSize = False
          Caption = '173 Herhaal '#233#233'n'
          Icon.BuiltIn = ei173Herhaaleen
        end
        object IconButton174: TERDButton
          Left = 594
          Top = 2084
          Width = 184
          Height = 48
          TabOrder = 176
          OnClick = IconClicked
          AutoSize = False
          Caption = '174 Herhaal alles'
          Icon.BuiltIn = ei174Herhaalalles
        end
        object IconButton175: TERDButton
          Left = 788
          Top = 2084
          Width = 184
          Height = 48
          TabOrder = 177
          OnClick = IconClicked
          AutoSize = False
          Caption = '175 Herhalen uit'
          Icon.BuiltIn = ei175Herhalenuit
        end
        object IconButton176: TERDButton
          Left = 12
          Top = 2142
          Width = 184
          Height = 48
          TabOrder = 178
          OnClick = IconClicked
          AutoSize = False
          Caption = '176 Shuffle aan'
          Icon.BuiltIn = ei176Shuffleaan
        end
        object IconButton177: TERDButton
          Left = 206
          Top = 2142
          Width = 184
          Height = 48
          TabOrder = 179
          OnClick = IconClicked
          AutoSize = False
          Caption = '177 Shuffle uit'
          Icon.BuiltIn = ei177Shuffleuit
        end
        object IconButton178: TERDButton
          Left = 400
          Top = 2142
          Width = 184
          Height = 48
          TabOrder = 180
          OnClick = IconClicked
          AutoSize = False
          Caption = '178 Mediabibliotheek'
          Icon.BuiltIn = ei178Mediabibliotheek
        end
        object IconButton179: TERDButton
          Left = 594
          Top = 2142
          Width = 184
          Height = 48
          TabOrder = 181
          OnClick = IconClicked
          AutoSize = False
          Caption = '179 Album'
          Icon.BuiltIn = ei179Album
        end
        object IconButton180: TERDButton
          Left = 788
          Top = 2142
          Width = 184
          Height = 48
          TabOrder = 182
          OnClick = IconClicked
          AutoSize = False
          Caption = '180 Artiest'
          Icon.BuiltIn = ei180Artiest
        end
        object IconButton181: TERDButton
          Left = 12
          Top = 2200
          Width = 184
          Height = 48
          TabOrder = 183
          OnClick = IconClicked
          AutoSize = False
          Caption = '181 Muziektrack'
          Icon.BuiltIn = ei181Muziektrack
        end
        object IconButton182: TERDButton
          Left = 206
          Top = 2200
          Width = 184
          Height = 48
          TabOrder = 184
          OnClick = IconClicked
          AutoSize = False
          Caption = '182 Audiobestand'
          Icon.BuiltIn = ei182Audiobestand
        end
        object IconButton183: TERDButton
          Left = 400
          Top = 2200
          Width = 184
          Height = 48
          TabOrder = 185
          OnClick = IconClicked
          AutoSize = False
          Caption = '183 Videobestand'
          Icon.BuiltIn = ei183Videobestand
        end
        object IconButton184: TERDButton
          Left = 594
          Top = 2200
          Width = 184
          Height = 48
          TabOrder = 186
          OnClick = IconClicked
          AutoSize = False
          Caption = '184 Afbeelding'
          Icon.BuiltIn = ei184Afbeelding
        end
        object IconButton185: TERDButton
          Left = 788
          Top = 2200
          Width = 184
          Height = 48
          TabOrder = 187
          OnClick = IconClicked
          AutoSize = False
          Caption = '185 Muzieknoot'
          Icon.BuiltIn = ei185Muzieknoot
        end
        object IconButton186: TERDButton
          Left = 12
          Top = 2258
          Width = 184
          Height = 48
          TabOrder = 188
          OnClick = IconClicked
          AutoSize = False
          Caption = '186 Film'
          Icon.BuiltIn = ei186Film
        end
        object IconButton187: TERDButton
          Left = 206
          Top = 2258
          Width = 184
          Height = 48
          TabOrder = 189
          OnClick = IconClicked
          AutoSize = False
          Caption = '187 Camera'
          Icon.BuiltIn = ei187Camera
        end
        object IconButton188: TERDButton
          Left = 400
          Top = 2258
          Width = 184
          Height = 48
          TabOrder = 190
          OnClick = IconClicked
          AutoSize = False
          Caption = '188 Equalizer'
          Icon.BuiltIn = ei188Equalizer
        end
        object IconButton189: TERDButton
          Left = 594
          Top = 2258
          Width = 184
          Height = 48
          TabOrder = 191
          OnClick = IconClicked
          AutoSize = False
          Caption = '189 Audio-uitgang'
          Icon.BuiltIn = ei189Audiouitgang
        end
        object IconButton190: TERDButton
          Left = 788
          Top = 2258
          Width = 184
          Height = 48
          TabOrder = 192
          OnClick = IconClicked
          AutoSize = False
          Caption = '190 Hoofdtelefoon'
          Icon.BuiltIn = ei190Hoofdtelefoon
        end
        object IconButton191: TERDButton
          Left = 12
          Top = 2316
          Width = 184
          Height = 48
          TabOrder = 193
          OnClick = IconClicked
          AutoSize = False
          Caption = '191 Luidspreker'
          Icon.BuiltIn = ei191Luidspreker
        end
        object IconButton192: TERDButton
          Left = 206
          Top = 2316
          Width = 184
          Height = 48
          TabOrder = 194
          OnClick = IconClicked
          AutoSize = False
          Caption = '192 Ondertiteling'
          Icon.BuiltIn = ei192Ondertiteling
        end
        object IconButton193: TERDButton
          Left = 400
          Top = 2316
          Width = 184
          Height = 48
          TabOrder = 195
          OnClick = IconClicked
          AutoSize = False
          Caption = '193 Ondertiteling uit'
          Icon.BuiltIn = ei193Ondertitelinguit
        end
        object IconButton194: TERDButton
          Left = 594
          Top = 2316
          Width = 184
          Height = 48
          TabOrder = 196
          OnClick = IconClicked
          AutoSize = False
          Caption = '194 Audiotrack kiezen'
          Icon.BuiltIn = ei194Audiotrackkiezen
        end
        object IconButton195: TERDButton
          Left = 788
          Top = 2316
          Width = 184
          Height = 48
          TabOrder = 197
          OnClick = IconClicked
          AutoSize = False
          Caption = '195 Videokwaliteit'
          Icon.BuiltIn = ei195Videokwaliteit
        end
        object IconButton196: TERDButton
          Left = 12
          Top = 2374
          Width = 184
          Height = 48
          TabOrder = 198
          OnClick = IconClicked
          AutoSize = False
          Caption = '196 Afspeelsnelheid'
          Icon.BuiltIn = ei196Afspeelsnelheid
        end
        object IconButton197: TERDButton
          Left = 206
          Top = 2374
          Width = 184
          Height = 48
          TabOrder = 199
          OnClick = IconClicked
          AutoSize = False
          Caption = '197 Scene markeren'
          Icon.BuiltIn = ei197Scenemarkeren
        end
        object IconButton198: TERDButton
          Left = 400
          Top = 2374
          Width = 184
          Height = 48
          TabOrder = 200
          OnClick = IconClicked
          AutoSize = False
          Caption = '198 Picture-in-picture'
          Icon.BuiltIn = ei198Pictureinpicture
        end
        object IconButton199: TERDButton
          Left = 594
          Top = 2374
          Width = 184
          Height = 48
          TabOrder = 201
          OnClick = IconClicked
          AutoSize = False
          Caption = '199 Casten'
          Icon.BuiltIn = ei199Casten
        end
        object IconButton200: TERDButton
          Left = 788
          Top = 2374
          Width = 184
          Height = 48
          TabOrder = 202
          OnClick = IconClicked
          AutoSize = False
          Caption = '200 Live uitzending'
          Icon.BuiltIn = ei200Liveuitzending
        end
        object IconButton201: TERDButton
          Left = 12
          Top = 2432
          Width = 184
          Height = 48
          TabOrder = 203
          OnClick = IconClicked
          AutoSize = False
          Caption = '201 Nieuwe playlist'
          Icon.BuiltIn = ei201Nieuweplaylist
        end
        object IconButton202: TERDButton
          Left = 206
          Top = 2432
          Width = 184
          Height = 48
          TabOrder = 204
          OnClick = IconClicked
          AutoSize = False
          Caption = '202 Playlist openen'
          Icon.BuiltIn = ei202Playlistopenen
        end
        object IconButton203: TERDButton
          Left = 400
          Top = 2432
          Width = 184
          Height = 48
          TabOrder = 205
          OnClick = IconClicked
          AutoSize = False
          Caption = '203 Playlist opslaan'
          Icon.BuiltIn = ei203Playlistopslaan
        end
        object IconButton204: TERDButton
          Left = 594
          Top = 2432
          Width = 184
          Height = 48
          TabOrder = 206
          OnClick = IconClicked
          AutoSize = False
          Caption = '204 Playlist hernoemen'
          Icon.BuiltIn = ei204Playlisthernoemen
        end
        object IconButton205: TERDButton
          Left = 788
          Top = 2432
          Width = 184
          Height = 48
          TabOrder = 207
          OnClick = IconClicked
          AutoSize = False
          Caption = '205 Playlist dupliceren'
          Icon.BuiltIn = ei205Playlistdupliceren
        end
        object IconButton206: TERDButton
          Left = 12
          Top = 2490
          Width = 184
          Height = 48
          TabOrder = 208
          OnClick = IconClicked
          AutoSize = False
          Caption = '206 Playlists samenvoegen'
          Icon.BuiltIn = ei206Playlistssamenvoegen
        end
        object IconButton207: TERDButton
          Left = 206
          Top = 2490
          Width = 184
          Height = 48
          TabOrder = 209
          OnClick = IconClicked
          AutoSize = False
          Caption = '207 Playlist splitsen'
          Icon.BuiltIn = ei207Playlistsplitsen
        end
        object IconButton208: TERDButton
          Left = 400
          Top = 2490
          Width = 184
          Height = 48
          TabOrder = 210
          OnClick = IconClicked
          AutoSize = False
          Caption = '208 Track toevoegen'
          Icon.BuiltIn = ei208Tracktoevoegen
        end
        object IconButton209: TERDButton
          Left = 594
          Top = 2490
          Width = 184
          Height = 48
          TabOrder = 211
          OnClick = IconClicked
          AutoSize = False
          Caption = '209 Track verwijderen'
          Icon.BuiltIn = ei209Trackverwijderen
        end
        object IconButton210: TERDButton
          Left = 788
          Top = 2490
          Width = 184
          Height = 48
          TabOrder = 212
          OnClick = IconClicked
          AutoSize = False
          Caption = '210 Playlist sorteren'
          Icon.BuiltIn = ei210Playlistsorteren
        end
        object IconButton211: TERDButton
          Left = 12
          Top = 2548
          Width = 184
          Height = 48
          TabOrder = 213
          OnClick = IconClicked
          AutoSize = False
          Caption = '211 Track omhoog'
          Icon.BuiltIn = ei211Trackomhoog
        end
        object IconButton212: TERDButton
          Left = 206
          Top = 2548
          Width = 184
          Height = 48
          TabOrder = 214
          OnClick = IconClicked
          AutoSize = False
          Caption = '212 Track omlaag'
          Icon.BuiltIn = ei212Trackomlaag
        end
        object IconButton213: TERDButton
          Left = 400
          Top = 2548
          Width = 184
          Height = 48
          TabOrder = 215
          OnClick = IconClicked
          AutoSize = False
          Caption = '213 Queue'
          Icon.BuiltIn = ei213Queue
        end
        object IconButton214: TERDButton
          Left = 594
          Top = 2548
          Width = 184
          Height = 48
          TabOrder = 216
          OnClick = IconClicked
          AutoSize = False
          Caption = '214 Aan queue toevoegen'
          Icon.BuiltIn = ei214Aanqueuetoevoegen
        end
        object IconButton215: TERDButton
          Left = 788
          Top = 2548
          Width = 184
          Height = 48
          TabOrder = 217
          OnClick = IconClicked
          AutoSize = False
          Caption = '215 Uit queue verwijderen'
          Icon.BuiltIn = ei215Uitqueueverwijderen
        end
        object IconButton216: TERDButton
          Left = 12
          Top = 2606
          Width = 184
          Height = 48
          TabOrder = 218
          OnClick = IconClicked
          AutoSize = False
          Caption = '216 Queue leegmaken'
          Icon.BuiltIn = ei216Queueleegmaken
        end
        object IconButton217: TERDButton
          Left = 206
          Top = 2606
          Width = 184
          Height = 48
          TabOrder = 219
          OnClick = IconClicked
          AutoSize = False
          Caption = '217 Stream URL'
          Icon.BuiltIn = ei217StreamURL
        end
        object IconButton218: TERDButton
          Left = 400
          Top = 2606
          Width = 184
          Height = 48
          TabOrder = 220
          OnClick = IconClicked
          AutoSize = False
          Caption = '218 Stream testen'
          Icon.BuiltIn = ei218Streamtesten
        end
        object IconButton219: TERDButton
          Left = 594
          Top = 2606
          Width = 184
          Height = 48
          TabOrder = 221
          OnClick = IconClicked
          AutoSize = False
          Caption = '219 Stream actief'
          Icon.BuiltIn = ei219Streamactief
        end
        object IconButton220: TERDButton
          Left = 788
          Top = 2606
          Width = 184
          Height = 48
          TabOrder = 222
          OnClick = IconClicked
          AutoSize = False
          Caption = '220 Stream offline'
          Icon.BuiltIn = ei220Streamoffline
        end
        object IconButton221: TERDButton
          Left = 12
          Top = 2664
          Width = 184
          Height = 48
          TabOrder = 223
          OnClick = IconClicked
          AutoSize = False
          Caption = '221 TV-kanaal'
          Icon.BuiltIn = ei221TVkanaal
        end
        object IconButton222: TERDButton
          Left = 206
          Top = 2664
          Width = 184
          Height = 48
          TabOrder = 224
          OnClick = IconClicked
          AutoSize = False
          Caption = '222 Radiozender'
          Icon.BuiltIn = ei222Radiozender
        end
        object IconButton223: TERDButton
          Left = 400
          Top = 2664
          Width = 184
          Height = 48
          TabOrder = 225
          OnClick = IconClicked
          AutoSize = False
          Caption = '223 Programmagids'
          Icon.BuiltIn = ei223Programmagids
        end
        object IconButton224: TERDButton
          Left = 594
          Top = 2664
          Width = 184
          Height = 48
          TabOrder = 226
          OnClick = IconClicked
          AutoSize = False
          Caption = '224 Favoriet toevoegen'
          Icon.BuiltIn = ei224Favoriettoevoegen
        end
        object IconButton225: TERDButton
          Left = 788
          Top = 2664
          Width = 184
          Height = 48
          TabOrder = 227
          OnClick = IconClicked
          AutoSize = False
          Caption = '225 Favoriet verwijderen'
          Icon.BuiltIn = ei225Favorietverwijderen
        end
        object IconButton226: TERDButton
          Left = 12
          Top = 2722
          Width = 184
          Height = 48
          TabOrder = 228
          OnClick = IconClicked
          AutoSize = False
          Caption = '226 Download starten'
          Icon.BuiltIn = ei226Downloadstarten
        end
        object IconButton227: TERDButton
          Left = 206
          Top = 2722
          Width = 184
          Height = 48
          TabOrder = 229
          OnClick = IconClicked
          AutoSize = False
          Caption = '227 Download pauzeren'
          Icon.BuiltIn = ei227Downloadpauzeren
        end
        object IconButton228: TERDButton
          Left = 400
          Top = 2722
          Width = 184
          Height = 48
          TabOrder = 230
          OnClick = IconClicked
          AutoSize = False
          Caption = '228 Download hervatten'
          Icon.BuiltIn = ei228Downloadhervatten
        end
        object IconButton229: TERDButton
          Left = 594
          Top = 2722
          Width = 184
          Height = 48
          TabOrder = 231
          OnClick = IconClicked
          AutoSize = False
          Caption = '229 Download annuleren'
          Icon.BuiltIn = ei229Downloadannuleren
        end
        object IconButton230: TERDButton
          Left = 788
          Top = 2722
          Width = 184
          Height = 48
          TabOrder = 232
          OnClick = IconClicked
          AutoSize = False
          Caption = '230 Download opnieuw'
          Icon.BuiltIn = ei230Downloadopnieuw
        end
        object IconButton231: TERDButton
          Left = 12
          Top = 2780
          Width = 184
          Height = 48
          TabOrder = 233
          OnClick = IconClicked
          AutoSize = False
          Caption = '231 Download wachtend'
          Icon.BuiltIn = ei231Downloadwachtend
        end
        object IconButton232: TERDButton
          Left = 206
          Top = 2780
          Width = 184
          Height = 48
          TabOrder = 234
          OnClick = IconClicked
          AutoSize = False
          Caption = '232 Download actief'
          Icon.BuiltIn = ei232Downloadactief
        end
        object IconButton233: TERDButton
          Left = 400
          Top = 2780
          Width = 184
          Height = 48
          TabOrder = 235
          OnClick = IconClicked
          AutoSize = False
          Caption = '233 Download klaar'
          Icon.BuiltIn = ei233Downloadklaar
        end
        object IconButton234: TERDButton
          Left = 594
          Top = 2780
          Width = 184
          Height = 48
          TabOrder = 236
          OnClick = IconClicked
          AutoSize = False
          Caption = '234 Download mislukt'
          Icon.BuiltIn = ei234Downloadmislukt
        end
        object IconButton235: TERDButton
          Left = 788
          Top = 2780
          Width = 184
          Height = 48
          TabOrder = 237
          OnClick = IconClicked
          AutoSize = False
          Caption = '235 Downloadinstellingen'
          Icon.BuiltIn = ei235Downloadinstellingen
        end
        object IconButton236: TERDButton
          Left = 12
          Top = 2838
          Width = 184
          Height = 48
          TabOrder = 238
          OnClick = IconClicked
          AutoSize = False
          Caption = '236 Bandbreedtelimiet'
          Icon.BuiltIn = ei236Bandbreedtelimiet
        end
        object IconButton237: TERDButton
          Left = 206
          Top = 2838
          Width = 184
          Height = 48
          TabOrder = 239
          OnClick = IconClicked
          AutoSize = False
          Caption = '237 Downloadmap'
          Icon.BuiltIn = ei237Downloadmap
        end
        object IconButton238: TERDButton
          Left = 400
          Top = 2838
          Width = 184
          Height = 48
          TabOrder = 240
          OnClick = IconClicked
          AutoSize = False
          Caption = '238 Voltooide wissen'
          Icon.BuiltIn = ei238Voltooidewissen
        end
        object IconButton239: TERDButton
          Left = 594
          Top = 2838
          Width = 184
          Height = 48
          TabOrder = 241
          OnClick = IconClicked
          AutoSize = False
          Caption = '239 Batch downloaden'
          Icon.BuiltIn = ei239Batchdownloaden
        end
        object IconButton240: TERDButton
          Left = 788
          Top = 2838
          Width = 184
          Height = 48
          TabOrder = 242
          OnClick = IconClicked
          AutoSize = False
          Caption = '240 Downloadgeschiedenis'
          Icon.BuiltIn = ei240Downloadgeschiedenis
        end
        object IconButton241: TERDButton
          Left = 12
          Top = 2896
          Width = 184
          Height = 48
          TabOrder = 243
          OnClick = IconClicked
          AutoSize = False
          Caption = '241 VOD bibliotheek'
          Icon.BuiltIn = ei241VODbibliotheek
        end
        object IconButton242: TERDButton
          Left = 206
          Top = 2896
          Width = 184
          Height = 48
          TabOrder = 244
          OnClick = IconClicked
          AutoSize = False
          Caption = '242 Film details'
          Icon.BuiltIn = ei242Filmdetails
        end
        object IconButton243: TERDButton
          Left = 400
          Top = 2896
          Width = 184
          Height = 48
          TabOrder = 245
          OnClick = IconClicked
          AutoSize = False
          Caption = '243 Series'
          Icon.BuiltIn = ei243Series
        end
        object IconButton244: TERDButton
          Left = 594
          Top = 2896
          Width = 184
          Height = 48
          TabOrder = 246
          OnClick = IconClicked
          AutoSize = False
          Caption = '244 Seizoenen'
          Icon.BuiltIn = ei244Seizoenen
        end
        object IconButton245: TERDButton
          Left = 788
          Top = 2896
          Width = 184
          Height = 48
          TabOrder = 247
          OnClick = IconClicked
          AutoSize = False
          Caption = '245 Afleveringen'
          Icon.BuiltIn = ei245Afleveringen
        end
        object IconButton246: TERDButton
          Left = 12
          Top = 2954
          Width = 184
          Height = 48
          TabOrder = 248
          OnClick = IconClicked
          AutoSize = False
          Caption = '246 Verder kijken'
          Icon.BuiltIn = ei246Verderkijken
        end
        object IconButton247: TERDButton
          Left = 206
          Top = 2954
          Width = 184
          Height = 48
          TabOrder = 249
          OnClick = IconClicked
          AutoSize = False
          Caption = '247 Laatst bekeken'
          Icon.BuiltIn = ei247Laatstbekeken
        end
        object IconButton248: TERDButton
          Left = 400
          Top = 2954
          Width = 184
          Height = 48
          TabOrder = 250
          OnClick = IconClicked
          AutoSize = False
          Caption = '248 Kijklijst'
          Icon.BuiltIn = ei248Kijklijst
        end
        object IconButton249: TERDButton
          Left = 594
          Top = 2954
          Width = 184
          Height = 48
          TabOrder = 251
          OnClick = IconClicked
          AutoSize = False
          Caption = '249 Teletext'
          Icon.BuiltIn = ei249Teletext
        end
        object IconButton250: TERDButton
          Left = 788
          Top = 2954
          Width = 184
          Height = 48
          TabOrder = 252
          OnClick = IconClicked
          AutoSize = False
          Caption = '250 Teletextpagina'
          Icon.BuiltIn = ei250Teletextpagina
        end
        object IconButton251: TERDButton
          Left = 12
          Top = 3012
          Width = 184
          Height = 48
          TabOrder = 253
          OnClick = IconClicked
          AutoSize = False
          Caption = '251 Teletext volgende'
          Icon.BuiltIn = ei251Teletextvolgende
        end
        object IconButton252: TERDButton
          Left = 206
          Top = 3012
          Width = 184
          Height = 48
          TabOrder = 254
          OnClick = IconClicked
          AutoSize = False
          Caption = '252 Teletext vorige'
          Icon.BuiltIn = ei252Teletextvorige
        end
        object IconButton253: TERDButton
          Left = 400
          Top = 3012
          Width = 184
          Height = 48
          TabOrder = 255
          OnClick = IconClicked
          AutoSize = False
          Caption = '253 Teletext vergroten'
          Icon.BuiltIn = ei253Teletextvergroten
        end
        object IconButton254: TERDButton
          Left = 594
          Top = 3012
          Width = 184
          Height = 48
          TabOrder = 256
          OnClick = IconClicked
          AutoSize = False
          Caption = '254 Teletext transparant'
          Icon.BuiltIn = ei254Teletexttransparant
        end
        object IconButton255: TERDButton
          Left = 788
          Top = 3012
          Width = 184
          Height = 48
          TabOrder = 257
          OnClick = IconClicked
          AutoSize = False
          Caption = '255 Ondertiteltaal'
          Icon.BuiltIn = ei255Ondertiteltaal
        end
        object IconButton256: TERDButton
          Left = 12
          Top = 3070
          Width = 184
          Height = 48
          TabOrder = 258
          OnClick = IconClicked
          AutoSize = False
          Caption = '256 Ondertitelbestand'
          Icon.BuiltIn = ei256Ondertitelbestand
        end
        object IconButton257: TERDButton
          Left = 206
          Top = 3070
          Width = 184
          Height = 48
          TabOrder = 259
          OnClick = IconClicked
          AutoSize = False
          Caption = '257 Ondertitels synchroniseren'
          Icon.BuiltIn = ei257Ondertitelssynchroniseren
        end
        object IconButton258: TERDButton
          Left = 400
          Top = 3070
          Width = 184
          Height = 48
          TabOrder = 260
          OnClick = IconClicked
          AutoSize = False
          Caption = '258 Ondertitels vroeger'
          Icon.BuiltIn = ei258Ondertitelsvroeger
        end
        object IconButton259: TERDButton
          Left = 594
          Top = 3070
          Width = 184
          Height = 48
          TabOrder = 261
          OnClick = IconClicked
          AutoSize = False
          Caption = '259 Ondertitels later'
          Icon.BuiltIn = ei259Ondertitelslater
        end
        object IconButton260: TERDButton
          Left = 788
          Top = 3070
          Width = 184
          Height = 48
          TabOrder = 262
          OnClick = IconClicked
          AutoSize = False
          Caption = '260 Audiotaal'
          Icon.BuiltIn = ei260Audiotaal
        end
        object IconButton261: TERDButton
          Left = 12
          Top = 3128
          Width = 184
          Height = 48
          TabOrder = 263
          OnClick = IconClicked
          AutoSize = False
          Caption = '261 Stereo'
          Icon.BuiltIn = ei261Stereo
        end
        object IconButton262: TERDButton
          Left = 206
          Top = 3128
          Width = 184
          Height = 48
          TabOrder = 264
          OnClick = IconClicked
          AutoSize = False
          Caption = '262 Mono'
          Icon.BuiltIn = ei262Mono
        end
        object IconButton263: TERDButton
          Left = 400
          Top = 3128
          Width = 184
          Height = 48
          TabOrder = 265
          OnClick = IconClicked
          AutoSize = False
          Caption = '263 Surround'
          Icon.BuiltIn = ei263Surround
        end
        object IconButton264: TERDButton
          Left = 594
          Top = 3128
          Width = 184
          Height = 48
          TabOrder = 266
          OnClick = IconClicked
          AutoSize = False
          Caption = '264 Audiobalans'
          Icon.BuiltIn = ei264Audiobalans
        end
        object IconButton265: TERDButton
          Left = 788
          Top = 3128
          Width = 184
          Height = 48
          TabOrder = 267
          OnClick = IconClicked
          AutoSize = False
          Caption = '265 Equalizer presets'
          Icon.BuiltIn = ei265Equalizerpresets
        end
        object IconButton266: TERDButton
          Left = 12
          Top = 3186
          Width = 184
          Height = 48
          TabOrder = 268
          OnClick = IconClicked
          AutoSize = False
          Caption = '266 Bas'
          Icon.BuiltIn = ei266Bas
        end
        object IconButton267: TERDButton
          Left = 206
          Top = 3186
          Width = 184
          Height = 48
          TabOrder = 269
          OnClick = IconClicked
          AutoSize = False
          Caption = '267 Hoge tonen'
          Icon.BuiltIn = ei267Hogetonen
        end
        object IconButton268: TERDButton
          Left = 400
          Top = 3186
          Width = 184
          Height = 48
          TabOrder = 270
          OnClick = IconClicked
          AutoSize = False
          Caption = '268 Programmadetails'
          Icon.BuiltIn = ei268Programmadetails
        end
        object IconButton269: TERDButton
          Left = 594
          Top = 3186
          Width = 184
          Height = 48
          TabOrder = 271
          OnClick = IconClicked
          AutoSize = False
          Caption = '269 Volgend programma'
          Icon.BuiltIn = ei269Volgendprogramma
        end
        object IconButton270: TERDButton
          Left = 788
          Top = 3186
          Width = 184
          Height = 48
          TabOrder = 272
          OnClick = IconClicked
          AutoSize = False
          Caption = '270 Opname plannen'
          Icon.BuiltIn = ei270Opnameplannen
        end
        object IconButton271: TERDButton
          Left = 12
          Top = 3244
          Width = 184
          Height = 48
          TabOrder = 273
          OnClick = IconClicked
          AutoSize = False
          Caption = '271 Opnamen'
          Icon.BuiltIn = ei271Opnamen
        end
        object IconButton272: TERDButton
          Left = 206
          Top = 3244
          Width = 184
          Height = 48
          TabOrder = 274
          OnClick = IconClicked
          AutoSize = False
          Caption = '272 Timeshift'
          Icon.BuiltIn = ei272Timeshift
        end
        object IconButton273: TERDButton
          Left = 400
          Top = 3244
          Width = 184
          Height = 48
          TabOrder = 275
          OnClick = IconClicked
          AutoSize = False
          Caption = '273 Timeshift pauze'
          Icon.BuiltIn = ei273Timeshiftpauze
        end
        object IconButton274: TERDButton
          Left = 594
          Top = 3244
          Width = 184
          Height = 48
          TabOrder = 276
          OnClick = IconClicked
          AutoSize = False
          Caption = '274 Naar live'
          Icon.BuiltIn = ei274Naarlive
        end
        object IconButton275: TERDButton
          Left = 788
          Top = 3244
          Width = 184
          Height = 48
          TabOrder = 277
          OnClick = IconClicked
          AutoSize = False
          Caption = '275 Catch-up TV'
          Icon.BuiltIn = ei275CatchupTV
        end
        object IconButton276: TERDButton
          Left = 12
          Top = 3302
          Width = 184
          Height = 48
          TabOrder = 278
          OnClick = IconClicked
          AutoSize = False
          Caption = '276 Vanaf begin'
          Icon.BuiltIn = ei276Vanafbegin
        end
        object IconButton277: TERDButton
          Left = 206
          Top = 3302
          Width = 184
          Height = 48
          TabOrder = 279
          OnClick = IconClicked
          AutoSize = False
          Caption = '277 Kanaalgroepen'
          Icon.BuiltIn = ei277Kanaalgroepen
        end
        object IconButton278: TERDButton
          Left = 400
          Top = 3302
          Width = 184
          Height = 48
          TabOrder = 280
          OnClick = IconClicked
          AutoSize = False
          Caption = '278 Kanaalnummer'
          Icon.BuiltIn = ei278Kanaalnummer
        end
        object IconButton279: TERDButton
          Left = 594
          Top = 3302
          Width = 184
          Height = 48
          TabOrder = 281
          OnClick = IconClicked
          AutoSize = False
          Caption = '279 Kanaallogo'
          Icon.BuiltIn = ei279Kanaallogo
        end
        object IconButton280: TERDButton
          Left = 788
          Top = 3302
          Width = 184
          Height = 48
          TabOrder = 282
          OnClick = IconClicked
          AutoSize = False
          Caption = '280 M3U playlist'
          Icon.BuiltIn = ei280M3Uplaylist
        end
        object IconButton281: TERDButton
          Left = 12
          Top = 3360
          Width = 184
          Height = 48
          TabOrder = 283
          OnClick = IconClicked
          AutoSize = False
          Caption = '281 Playlist naar cloud'
          Icon.BuiltIn = ei281Playlistnaarcloud
        end
        object IconButton282: TERDButton
          Left = 206
          Top = 3360
          Width = 184
          Height = 48
          TabOrder = 284
          OnClick = IconClicked
          AutoSize = False
          Caption = '282 Playlist uit cloud'
          Icon.BuiltIn = ei282Playlistuitcloud
        end
        object IconButton283: TERDButton
          Left = 400
          Top = 3360
          Width = 184
          Height = 48
          TabOrder = 285
          OnClick = IconClicked
          AutoSize = False
          Caption = '283 Cloudplaylist delen'
          Icon.BuiltIn = ei283Cloudplaylistdelen
        end
        object IconButton284: TERDButton
          Left = 594
          Top = 3360
          Width = 184
          Height = 48
          TabOrder = 286
          OnClick = IconClicked
          AutoSize = False
          Caption = '284 Cloudplaylist sync'
          Icon.BuiltIn = ei284Cloudplaylistsync
        end
        object IconButton285: TERDButton
          Left = 788
          Top = 3360
          Width = 184
          Height = 48
          TabOrder = 287
          OnClick = IconClicked
          AutoSize = False
          Caption = '285 Xtream Codes'
          Icon.BuiltIn = ei285XtreamCodes
        end
        object IconButton286: TERDButton
          Left = 12
          Top = 3418
          Width = 184
          Height = 48
          TabOrder = 288
          OnClick = IconClicked
          AutoSize = False
          Caption = '286 Xtream server'
          Icon.BuiltIn = ei286Xtreamserver
        end
        object IconButton287: TERDButton
          Left = 206
          Top = 3418
          Width = 184
          Height = 48
          TabOrder = 289
          OnClick = IconClicked
          AutoSize = False
          Caption = '287 Xtream aanmelden'
          Icon.BuiltIn = ei287Xtreamaanmelden
        end
        object IconButton288: TERDButton
          Left = 400
          Top = 3418
          Width = 184
          Height = 48
          TabOrder = 290
          OnClick = IconClicked
          AutoSize = False
          Caption = '288 Xtream verversen'
          Icon.BuiltIn = ei288Xtreamverversen
        end
        object IconButton289: TERDButton
          Left = 594
          Top = 3418
          Width = 184
          Height = 48
          TabOrder = 291
          OnClick = IconClicked
          AutoSize = False
          Caption = '289 M3U importeren'
          Icon.BuiltIn = ei289M3Uimporteren
        end
        object IconButton290: TERDButton
          Left = 788
          Top = 3418
          Width = 184
          Height = 48
          TabOrder = 292
          OnClick = IconClicked
          AutoSize = False
          Caption = '290 M3U exporteren'
          Icon.BuiltIn = ei290M3Uexporteren
        end
        object IconButton291: TERDButton
          Left = 12
          Top = 3476
          Width = 184
          Height = 48
          TabOrder = 293
          OnClick = IconClicked
          AutoSize = False
          Caption = '291 M3U8 bestand'
          Icon.BuiltIn = ei291M3U8bestand
        end
        object IconButton292: TERDButton
          Left = 206
          Top = 3476
          Width = 184
          Height = 48
          TabOrder = 294
          OnClick = IconClicked
          AutoSize = False
          Caption = '292 PLS bestand'
          Icon.BuiltIn = ei292PLSbestand
        end
        object IconButton293: TERDButton
          Left = 400
          Top = 3476
          Width = 184
          Height = 48
          TabOrder = 295
          OnClick = IconClicked
          AutoSize = False
          Caption = '293 XSPF bestand'
          Icon.BuiltIn = ei293XSPFbestand
        end
        object IconButton294: TERDButton
          Left = 594
          Top = 3476
          Width = 184
          Height = 48
          TabOrder = 296
          OnClick = IconClicked
          AutoSize = False
          Caption = '294 ASX bestand'
          Icon.BuiltIn = ei294ASXbestand
        end
        object IconButton295: TERDButton
          Left = 788
          Top = 3476
          Width = 184
          Height = 48
          TabOrder = 297
          OnClick = IconClicked
          AutoSize = False
          Caption = '295 JSON playlist'
          Icon.BuiltIn = ei295JSONplaylist
        end
        object IconButton296: TERDButton
          Left = 12
          Top = 3534
          Width = 184
          Height = 48
          TabOrder = 298
          OnClick = IconClicked
          AutoSize = False
          Caption = '296 Playlist converteren'
          Icon.BuiltIn = ei296Playlistconverteren
        end
        object IconButton297: TERDButton
          Left = 206
          Top = 3534
          Width = 184
          Height = 48
          TabOrder = 299
          OnClick = IconClicked
          AutoSize = False
          Caption = '297 Playlist valideren'
          Icon.BuiltIn = ei297Playlistvalideren
        end
        object IconButton298: TERDButton
          Left = 400
          Top = 3534
          Width = 184
          Height = 48
          TabOrder = 300
          OnClick = IconClicked
          AutoSize = False
          Caption = '298 Playlist repareren'
          Icon.BuiltIn = ei298Playlistrepareren
        end
        object IconButton299: TERDButton
          Left = 594
          Top = 3534
          Width = 184
          Height = 48
          TabOrder = 301
          OnClick = IconClicked
          AutoSize = False
          Caption = '299 Dubbele kanalen'
          Icon.BuiltIn = ei299Dubbelekanalen
        end
        object IconButton300: TERDButton
          Left = 788
          Top = 3534
          Width = 184
          Height = 48
          TabOrder = 302
          OnClick = IconClicked
          AutoSize = False
          Caption = '300 Duplicaten verwijderen'
          Icon.BuiltIn = ei300Duplicatenverwijderen
        end
        object IconButton301: TERDButton
          Left = 12
          Top = 3592
          Width = 184
          Height = 48
          TabOrder = 303
          OnClick = IconClicked
          AutoSize = False
          Caption = '301 Kanalen zoeken'
          Icon.BuiltIn = ei301Kanalenzoeken
        end
        object IconButton302: TERDButton
          Left = 206
          Top = 3592
          Width = 184
          Height = 48
          TabOrder = 304
          OnClick = IconClicked
          AutoSize = False
          Caption = '302 Playlist filteren'
          Icon.BuiltIn = ei302Playlistfilteren
        end
        object IconButton303: TERDButton
          Left = 400
          Top = 3592
          Width = 184
          Height = 48
          TabOrder = 305
          OnClick = IconClicked
          AutoSize = False
          Caption = '303 Zoeken en vervangen'
          Icon.BuiltIn = ei303Zoekenenvervangen
        end
        object IconButton304: TERDButton
          Left = 594
          Top = 3592
          Width = 184
          Height = 48
          TabOrder = 306
          OnClick = IconClicked
          AutoSize = False
          Caption = '304 Alles vervangen'
          Icon.BuiltIn = ei304Allesvervangen
        end
        object IconButton305: TERDButton
          Left = 788
          Top = 3592
          Width = 184
          Height = 48
          TabOrder = 307
          OnClick = IconClicked
          AutoSize = False
          Caption = '305 HD bovenaan'
          Icon.BuiltIn = ei305HDbovenaan
        end
        object IconButton306: TERDButton
          Left = 12
          Top = 3650
          Width = 184
          Height = 48
          TabOrder = 308
          OnClick = IconClicked
          AutoSize = False
          Caption = '306 SD onderaan'
          Icon.BuiltIn = ei306SDonderaan
        end
        object IconButton307: TERDButton
          Left = 206
          Top = 3650
          Width = 184
          Height = 48
          TabOrder = 309
          OnClick = IconClicked
          AutoSize = False
          Caption = '307 Resolutie sorteren'
          Icon.BuiltIn = ei307Resolutiesorteren
        end
        object IconButton308: TERDButton
          Left = 400
          Top = 3650
          Width = 184
          Height = 48
          TabOrder = 310
          OnClick = IconClicked
          AutoSize = False
          Caption = '308 HD kanaal'
          Icon.BuiltIn = ei308HDkanaal
        end
        object IconButton309: TERDButton
          Left = 594
          Top = 3650
          Width = 184
          Height = 48
          TabOrder = 311
          OnClick = IconClicked
          AutoSize = False
          Caption = '309 Full HD kanaal'
          Icon.BuiltIn = ei309FullHDkanaal
        end
        object IconButton310: TERDButton
          Left = 788
          Top = 3650
          Width = 184
          Height = 48
          TabOrder = 312
          OnClick = IconClicked
          AutoSize = False
          Caption = '310 4K kanaal'
          Icon.BuiltIn = ei3104Kkanaal
        end
        object IconButton311: TERDButton
          Left = 12
          Top = 3708
          Width = 184
          Height = 48
          TabOrder = 313
          OnClick = IconClicked
          AutoSize = False
          Caption = '311 Kanaal verplaatsen'
          Icon.BuiltIn = ei311Kanaalverplaatsen
        end
        object IconButton312: TERDButton
          Left = 206
          Top = 3708
          Width = 184
          Height = 48
          TabOrder = 314
          OnClick = IconClicked
          AutoSize = False
          Caption = '312 Groep toevoegen'
          Icon.BuiltIn = ei312Groeptoevoegen
        end
        object IconButton313: TERDButton
          Left = 400
          Top = 3708
          Width = 184
          Height = 48
          TabOrder = 315
          OnClick = IconClicked
          AutoSize = False
          Caption = '313 Groep verwijderen'
          Icon.BuiltIn = ei313Groepverwijderen
        end
        object IconButton314: TERDButton
          Left = 594
          Top = 3708
          Width = 184
          Height = 48
          TabOrder = 316
          OnClick = IconClicked
          AutoSize = False
          Caption = '314 Groep hernoemen'
          Icon.BuiltIn = ei314Groephernoemen
        end
        object IconButton315: TERDButton
          Left = 788
          Top = 3708
          Width = 184
          Height = 48
          TabOrder = 317
          OnClick = IconClicked
          AutoSize = False
          Caption = '315 Groepen sorteren'
          Icon.BuiltIn = ei315Groepensorteren
        end
        object IconButton316: TERDButton
          Left = 12
          Top = 3766
          Width = 184
          Height = 48
          TabOrder = 318
          OnClick = IconClicked
          AutoSize = False
          Caption = '316 Kanalen samenvoegen'
          Icon.BuiltIn = ei316Kanalensamenvoegen
        end
        object IconButton317: TERDButton
          Left = 206
          Top = 3766
          Width = 184
          Height = 48
          TabOrder = 319
          OnClick = IconClicked
          AutoSize = False
          Caption = '317 Kanaal hernoemen'
          Icon.BuiltIn = ei317Kanaalhernoemen
        end
        object IconButton318: TERDButton
          Left = 400
          Top = 3766
          Width = 184
          Height = 48
          TabOrder = 320
          OnClick = IconClicked
          AutoSize = False
          Caption = '318 Kanalen vergelijken'
          Icon.BuiltIn = ei318Kanalenvergelijken
        end
        object IconButton319: TERDButton
          Left = 594
          Top = 3766
          Width = 184
          Height = 48
          TabOrder = 321
          OnClick = IconClicked
          AutoSize = False
          Caption = '319 Dode streams vinden'
          Icon.BuiltIn = ei319Dodestreamsvinden
        end
        object IconButton320: TERDButton
          Left = 788
          Top = 3766
          Width = 184
          Height = 48
          TabOrder = 322
          OnClick = IconClicked
          AutoSize = False
          Caption = '320 Dode streams verwijderen'
          Icon.BuiltIn = ei320Dodestreamsverwijderen
        end
      end
    end
    object InputsPage: TERDPanel
      Left = 0
      Top = 40
      Width = 1072
      Height = 448
      Align = alClient
      TabOrder = 2
      Visible = False
      BorderWidth = 0
      object InputsScroll: TERDScrollBox
        Left = 0
        Top = 0
        Width = 1072
        Height = 448
        Align = alClient
        TabOrder = 0
        object LabelCheckBox: TLabel
          Left = 16
          Top = 136
          Width = 80
          Height = 15
          Caption = 'TERDCheckBox'
        end
        object LabelRadioButton: TLabel
          Left = 506
          Top = 136
          Width = 94
          Height = 15
          Caption = 'TERDRadioButton'
        end
        object LabelChip: TLabel
          Left = 16
          Top = 406
          Width = 53
          Height = 15
          Caption = 'TERDChip'
        end
        object DemoChip: TERDChip
          Left = 16
          Top = 432
          Width = 16
          Height = 20
          Caption = 'Status chip'
        end
        object LabelBanner: TLabel
          Left = 506
          Top = 406
          Width = 65
          Height = 15
          Caption = 'TERDBanner'
        end
        object LabelEdit: TLabel
          Left = 16
          Top = 676
          Width = 48
          Height = 15
          Caption = 'TERDEdit'
        end
        object LabelSegmented: TLabel
          Left = 506
          Top = 676
          Width = 88
          Height = 15
          Caption = 'TERDSegmented'
        end
        object LabelRangeBar: TLabel
          Left = 16
          Top = 946
          Width = 78
          Height = 15
          Caption = 'TERDRangeBar'
        end
        object DemoRangeBar: TERDRangeBar
          Left = 16
          Top = 972
          Width = 460
          Height = 220
          Value = 45.000000000000000000
        end
        object LabelSpinEdit: TLabel
          Left = 506
          Top = 946
          Width = 71
          Height = 15
          Caption = 'TERDSpinEdit'
        end
        object LabelTrackBar: TLabel
          Left = 16
          Top = 1216
          Width = 72
          Height = 15
          Caption = 'TERDTrackBar'
        end
        object LabelMemo: TLabel
          Left = 506
          Top = 1216
          Width = 63
          Height = 15
          Caption = 'TERDMemo'
        end
        object LabelCalendar: TLabel
          Left = 16
          Top = 1486
          Width = 75
          Height = 15
          Caption = 'TERDCalendar'
        end
        object LabelDatePicker: TLabel
          Left = 506
          Top = 1486
          Width = 84
          Height = 15
          Caption = 'TERDDatePicker'
        end
        object LabelListBox: TLabel
          Left = 16
          Top = 1756
          Width = 65
          Height = 15
          Caption = 'TERDListBox'
        end
        object CommonControlsLabel: TLabel
          Left = 16
          Top = 2026
          Width = 279
          Height = 15
          Caption = 'Search / split button / tags / expander / vertical slider'
        end
        object LabelCheckListBox: TLabel
          Left = 16
          Top = 2380
          Width = 339
          Height = 15
          Caption = 'TERDCheckListBox - Space or checkbox to check; caption selects'
        end
        object LabelTimePicker: TLabel
          Left = 16
          Top = 2650
          Width = 316
          Height = 15
          Caption = 'TERDTimePicker - type / Left-Right / Up-Down; 24h and 12h'
        end
        object LabelDateTimePicker: TLabel
          Left = 16
          Top = 2740
          Width = 341
          Height = 15
          Caption = 'TERDDateTimePicker - editable date segments + calendar + time'
        end
        object LabelLinkLabel: TLabel
          Left = 16
          Top = 2830
          Width = 369
          Height = 15
          Caption = 
            'TERDLinkLabel - mouse / Enter / Space, visited state and optiona' +
            'l icon'
        end
        object LabelLoadingIndicator: TLabel
          Left = 16
          Top = 2930
          Width = 320
          Height = 15
          Caption = 'TERDLoadingIndicator - follows Windows animation settings'
        end
        object LabelDropZone: TLabel
          Left = 16
          Top = 3040
          Width = 306
          Height = 15
          Caption = 'TERDDropZone - Explorer files, masks and optional folders'
        end
        object RatingLabel: TLabel
          Left = 16
          Top = 3340
          Width = 272
          Height = 15
          Caption = 'TERDRating - half stars, hover preview and readonly'
        end
        object SearchComboLabel: TLabel
          Left = 16
          Top = 3450
          Width = 396
          Height = 15
          Caption = 
            'Editable ComboBox - contains filter, autocomplete and original I' +
            'tems index'
        end
        object PasswordEditLabel: TLabel
          Left = 16
          Top = 3550
          Width = 261
          Height = 15
          Caption = 'Edit password - eye button or Alt+V to show/hide'
        end
        object ValidationEditLabel: TLabel
          Left = 16
          Top = 3650
          Width = 282
          Height = 15
          Caption = 'Edit validation - required name, warning for short text'
        end
        object DemoCheckBox: TERDCheckBox
          Left = 16
          Top = 162
          Width = 112
          Height = 30
          TabOrder = 2
          Caption = 'Enabled option'
        end
        object DemoRadioButton: TERDRadioButton
          Left = 506
          Top = 162
          Width = 100
          Height = 30
          TabOrder = 3
          Caption = 'Radio choice'
        end
        object DemoBanner: TERDBanner
          Left = 506
          Top = 432
          Width = 460
          Height = 56
          TabOrder = 4
          ShowClose = True
          ActionCaption = 'Details'
          OnAction = ShowDialog
          Text = 'Example notification'
        end
        object DemoEdit: TERDEdit
          Left = 16
          Top = 702
          Width = 460
          Height = 26
          TabOrder = 5
          Text = 'Editable text'
        end
        object DemoSegmented: TERDSegmented
          Left = 506
          Top = 702
          Width = 143
          Height = 32
          TabOrder = 6
          Items.Strings = (
            'One'
            'Two'
            'Three')
        end
        object DemoSpinEdit: TERDSpinEdit
          Left = 506
          Top = 972
          Width = 460
          Height = 26
          TabOrder = 7
          Text = '12'
          Value = 12.000000000000000000
          Increment = 1.000000000000000000
        end
        object DemoTrackBar: TERDTrackBar
          Left = 16
          Top = 1242
          Width = 460
          Height = 26
          TabOrder = 8
          Position = 35
        end
        object DemoMemo: TERDMemo
          Left = 506
          Top = 1242
          Width = 460
          Height = 220
          TabOrder = 9
          Lines.Strings = (
            'Editable memo text'
            'Second line')
        end
        object DemoCalendar: TERDCalendar
          Left = 16
          Top = 1512
          Width = 240
          Height = 268
          TabOrder = 10
          Date = 46309.000000000000000000
        end
        object DemoDatePicker: TERDDatePicker
          Left = 506
          Top = 1512
          Width = 460
          Height = 26
          TabOrder = 11
          Date = 46309.000000000000000000
        end
        object DemoListBox: TERDListBox
          Left = 16
          Top = 1782
          Width = 460
          Height = 220
          TabOrder = 12
          TabStop = True
          Items.Strings = (
            'First item'
            'Second item'
            'Third item')
          ItemIndex = -1
        end
        object DemoSearchEdit: TERDSearchEdit
          Left = 16
          Top = 2052
          Width = 300
          Height = 26
          TabOrder = 13
          Text = ''
          TextHint = 'Search channels'
          OnSearch = SearchChannels
        end
        object DemoSplitButton: TERDSplitButton
          Left = 336
          Top = 2052
          Width = 93
          Height = 38
          TabOrder = 14
          OnClick = OpenFile
          Caption = 'Open'
          DropDownMenu = DemoPopupMenu
        end
        object DemoTagEdit: TERDTagEdit
          Left = 16
          Top = 2110
          Width = 460
          Height = 120
          TabOrder = 15
          Tags.Strings = (
            'Live TV'
            'HD'
            'Favorites')
        end
        object DemoExpander: TERDExpander
          Left = 506
          Top = 2110
          Width = 360
          Height = 160
          TabOrder = 16
          Caption = 'Advanced filters'
          object ExpanderText: TLabel
            Left = 16
            Top = 54
            Width = 229
            Height = 15
            Caption = 'Editable content; collapse using the header.'
          end
        end
        object DemoSeekBar: TERDSeekBar
          Left = 16
          Top = 2310
          Width = 460
          Height = 26
          TabOrder = 17
          Max = 3600
          Position = 900
          ShowValue = True
          Suffix = ' s'
          BufferedPosition = 1800
        end
        object DemoRangeSlider: TERDRangeSlider
          Left = 506
          Top = 2310
          Width = 460
          Height = 26
          TabOrder = 18
          Position = 20
          ShowValue = True
          UpperValue = 80
        end
        object DemoVerticalTrackBar: TERDTrackBar
          Left = 896
          Top = 2052
          Width = 60
          Height = 220
          TabOrder = 19
          Orientation = toVertical
          Position = 65
          ShowValue = True
          AutoSize = False
        end
        object DemoCheckListBox: TERDCheckListBox
          Left = 16
          Top = 2406
          Width = 460
          Height = 220
          TabOrder = 20
          Items = <
            item
              Caption = 'Live TV'
              State = cbChecked
            end
            item
              Caption = 'Movies / VOD'
            end
            item
              Caption = 'Series - partially selected'
              State = cbGrayed
            end
            item
              Caption = 'Unavailable source'
              Enabled = False
            end>
          AllowGrayed = True
          ItemIndex = -1
          OnClickCheck = CheckListChanged
        end
        object DemoTimePicker: TERDTimePicker
          Left = 16
          Top = 2676
          Width = 220
          Height = 26
          TabOrder = 21
          Time = 0.562500000000000000
          ShowSeconds = True
          OnChange = TimeChanged
        end
        object DemoTimePicker12: TERDTimePicker
          Left = 256
          Top = 2676
          Width = 220
          Height = 26
          TabOrder = 22
          Time = 0.562500000000000000
          TimeFormat = tf12Hour
          OnChange = TimeChanged
        end
        object DemoDateTimePicker: TERDDateTimePicker
          Left = 16
          Top = 2766
          Width = 460
          Height = 26
          TabOrder = 23
          DateTime = 46309.000000000000000000
          DateFormat = 'dd/mm/yyyy'
          ShowSeconds = True
          OnChange = DateTimeChanged
        end
        object DemoLinkLabel: TERDLinkLabel
          Left = 16
          Top = 2856
          Width = 133
          Height = 32
          Cursor = crHandPoint
          TabOrder = 24
          OnClick = LinkClicked
          Caption = 'ERDesigns website'
          URL = 'https://erdesigns.be'
          Icon.BuiltIn = ei028Info
        end
        object DemoLoadingIndicator: TERDLoadingIndicator
          Left = 16
          Top = 2956
          Width = 137
          Height = 32
          TabOrder = 25
          Caption = 'Loading playlists...'
        end
        object LoadingStyleChoice: TERDComboBox
          Left = 500
          Top = 2956
          Width = 200
          Height = 26
          TabOrder = 26
          Items.Strings = (
            'Spinner'
            'Ring'
            'Dots'
            'Bars'
            'Pulse')
          ItemIndex = 0
          OnChange = LoadingStyleChanged
        end
        object LoadingSizeChoice: TERDComboBox
          Left = 720
          Top = 2956
          Width = 160
          Height = 26
          TabOrder = 27
          Items.Strings = (
            '16 px'
            '24 px'
            '32 px'
            '48 px'
            '64 px')
          ItemIndex = 2
          OnChange = LoadingSizeChanged
        end
        object LoadingToggle: TERDCheckBox
          Left = 256
          Top = 2956
          Width = 79
          Height = 30
          TabOrder = 28
          Caption = 'Active'
          State = cbChecked
          Style = csSwitch
          OnChange = LoadingChanged
        end
        object DemoDropZone: TERDDropZone
          Left = 16
          Top = 3066
          Width = 460
          Height = 160
          TabOrder = 29
          OnClick = BrowseDropFiles
          Caption = 'Drop files here'
          Description = 'or click to browse'
          Icon.BuiltIn = ei018Upload
          FileMasks = '*'
          OnFilesDropped = FilesDropped
        end
        object ShowCommands: TERDButton
          Left = 16
          Top = 3260
          Width = 175
          Height = 38
          TabOrder = 30
          OnClick = OpenCommands
          Caption = 'Command palette (Ctrl+K)'
        end
        object DemoRating: TERDRating
          Left = 16
          Top = 3366
          Width = 128
          Height = 32
          TabOrder = 31
          Value = 3.500000000000000000
          AllowHalf = True
          OnChange = RatingChanged
        end
        object RatingReadOnly: TERDRating
          Left = 256
          Top = 3366
          Width = 128
          Height = 32
          TabOrder = 32
          Value = 4.500000000000000000
          AllowHalf = True
          ReadOnly = True
        end
        object SearchCombo: TERDComboBox
          Left = 16
          Top = 3476
          Width = 460
          Height = 26
          TabOrder = 33
          TabStop = False
          Style = ecsDropDown
          FilterMode = cfsContains
          Text = ''
          Items.Strings = (
            'Belgium'
            'Netherlands'
            'Belgium HD'
            'France'
            'Germany')
          OnChange = SearchComboChanged
        end
        object PasswordEdit: TERDEdit
          Left = 16
          Top = 3576
          Width = 460
          Height = 26
          TabOrder = 34
          PasswordChar = #8226
          ShowPasswordButton = True
          Text = 'Example password'
        end
        object ValidationEdit: TERDEdit
          Left = 16
          Top = 3676
          Width = 460
          Height = 46
          TabOrder = 35
          ValidationState = evsError
          ValidationText = 'A name is required.'
          ValidateOnChange = True
          OnValidate = ValidateExample
          Text = ''
          TextHint = 'Enter a name'
        end
        object LabelUnicodePreview: TLabel
          Left = 16
          Top = 16
          Width = 460
          Height = 15
          Caption = 'Self-painted Unicode preview - accents, emoji and password'
        end
        object LabelUnicodeState: TLabel
          Left = 16
          Top = 112
          Width = 1000
          Height = 15
          AutoSize = False
          Caption = 'Preview diagnostics appear when Inputs is selected.'
        end
      end
    end
    object LayoutPage: TERDPanel
      Left = 0
      Top = 40
      Width = 1072
      Height = 448
      Align = alClient
      TabOrder = 3
      Visible = False
      BorderWidth = 0
      object LayoutScroll: TERDScrollBox
        Left = 0
        Top = 0
        Width = 1072
        Height = 448
        Align = alClient
        TabOrder = 0
        object LabelCard: TLabel
          Left = 16
          Top = 16
          Width = 53
          Height = 15
          Caption = 'TERDCard'
        end
        object LabelMenuBar: TLabel
          Left = 506
          Top = 16
          Width = 76
          Height = 15
          Caption = 'TERDMenuBar'
        end
        object LabelBackstage: TLabel
          Left = 16
          Top = 286
          Width = 81
          Height = 15
          Caption = 'TERDBackstage'
        end
        object LabelTabs: TLabel
          Left = 506
          Top = 286
          Width = 269
          Height = 15
          Caption = 'TERDTabs - drag / Ctrl+Shift+arrows / Ctrl+Shift+D'
        end
        object LabelToolBar: TLabel
          Left = 16
          Top = 556
          Width = 67
          Height = 15
          Caption = 'TERDToolBar'
        end
        object LabelProgressBar: TLabel
          Left = 506
          Top = 556
          Width = 90
          Height = 15
          Caption = 'TERDProgressBar'
        end
        object LabelScrollBar: TLabel
          Left = 16
          Top = 826
          Width = 74
          Height = 15
          Caption = 'TERDScrollBar'
        end
        object LabelSplitter: TLabel
          Left = 506
          Top = 826
          Width = 65
          Height = 15
          Caption = 'TERDSplitter'
        end
        object DemoCard: TERDCard
          Left = 16
          Top = 42
          Width = 460
          Height = 220
          TabOrder = 0
          Title = 'Card container'
          Padding.Left = 16
          Padding.Top = 4
          Padding.Right = 16
          Padding.Bottom = 12
        end
        object DemoMenuBar: TERDMenuBar
          Left = 506
          Top = 42
          Width = 50
          Height = 30
          TabOrder = 1
          Menu = MainMenu
        end
        object OpenBackstageButton: TERDButton
          Left = 16
          Top = 312
          Width = 280
          Height = 44
          TabOrder = 2
          OnClick = ShowBackstage
          AutoSize = False
          Caption = 'Open backstage (File)'
          Icon.BuiltIn = ei005Bestand
        end
        object DemoTabs: TERDTabs
          Left = 506
          Top = 312
          Width = 460
          Height = 40
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          AllowReorder = True
          AllowDetach = True
          OnReordered = DocumentTabReordered
          OnDetached = DocumentTabDetached
          OnAttached = DocumentTabAttached
          Tabs = <
            item
              Page = DocumentOne
              Caption = 'First document'
            end
            item
              Page = DocumentTwo
              Caption = 'Modified document'
              Modified = True
            end>
          TabStyle = tsDocument
          TabIndex = 0
          ShowNewTab = True
          OnClose = DocumentTabClosing
          OnClosed = DocumentTabClosed
          OnNewTab = NewDocumentTab
        end
        object DocumentHost: TERDPanel
          Left = 506
          Top = 348
          Width = 460
          Height = 184
          TabOrder = 4
          object DocumentOne: TERDPanel
            Left = 1
            Top = 1
            Width = 458
            Height = 182
            Align = alClient
            TabOrder = 0
            BorderWidth = 0
            object DocumentOneEdit: TERDEdit
              Left = 16
              Top = 76
              Width = 400
              Height = 26
              TabOrder = 0
              Text = 'Edit this, then detach and return the tab'
              OnChange = DocumentEdited
            end
          end
          object DocumentTwo: TERDPanel
            Left = 1
            Top = 1
            Width = 458
            Height = 182
            Align = alClient
            TabOrder = 1
            Visible = False
            BorderWidth = 0
            object DocumentTwoEdit: TERDEdit
              Left = 16
              Top = 76
              Width = 400
              Height = 26
              TabOrder = 0
              Text = 'Another document'
              OnChange = DocumentEdited
            end
          end
        end
        object DemoToolBar: TERDToolBar
          Left = 16
          Top = 582
          Width = 460
          Height = 42
          Align = alNone
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
          Items = <
            item
              Icon.BuiltIn = ei002Openen
              Caption = 'Open'
              OnClick = OpenFile
            end
            item
              Icon.BuiltIn = ei028Info
              Caption = 'Dialog'
              OnClick = ShowDialog
            end>
          SearchHint = 'Find item'
        end
        object DemoProgressBar: TERDProgressBar
          Left = 506
          Top = 582
          Width = 460
          Height = 220
          TabOrder = 6
          Position = 65
          Caption = ''
          Steps = <>
        end
        object DemoScrollBar: TERDScrollBar
          Left = 16
          Top = 852
          Width = 460
          Height = 220
          TabOrder = 7
          TabStop = True
          Position = 25
        end
        object DemoSplitter: TERDSplitter
          Left = 506
          Top = 852
          Width = 460
          Height = 220
          Cursor = crHSplit
          Align = alNone
          TabOrder = 8
        end
      end
    end
    object DataPage: TERDPanel
      Left = 0
      Top = 40
      Width = 1072
      Height = 448
      Align = alClient
      TabOrder = 4
      Visible = False
      BorderWidth = 0
      object DataScroll: TERDScrollBox
        Left = 0
        Top = 0
        Width = 1072
        Height = 448
        Align = alClient
        TabOrder = 0
        object LabelInspector: TLabel
          Left = 506
          Top = 16
          Width = 77
          Height = 15
          Caption = 'TERDInspector'
        end
        object LabelDataGrid: TLabel
          Left = 16
          Top = 286
          Width = 74
          Height = 15
          Caption = 'TERDDataGrid'
        end
        object LabelTileGrid: TLabel
          Left = 506
          Top = 286
          Width = 68
          Height = 15
          Caption = 'TERDTileGrid'
        end
        object LabelKeyValueList: TLabel
          Left = 16
          Top = 556
          Width = 93
          Height = 15
          Caption = 'TERDKeyValueList'
        end
        object LabelHistogram: TLabel
          Left = 506
          Top = 556
          Width = 84
          Height = 15
          Caption = 'TERDHistogram'
        end
        object LabelVirtualStringTree: TLabel
          Left = 16
          Top = 826
          Width = 285
          Height = 15
          Caption = 'TERDVirtualStringTree - F2 edits names / type / quality'
        end
        object DemoInspector: TERDInspector
          Left = 506
          Top = 42
          Width = 460
          Height = 220
          TabOrder = 0
          Categories = <
            item
              Caption = 'General'
              Properties = <
                item
                  Name = 'Title'
                  Value = 'Example'
                end
                item
                  Name = 'Mode'
                  Value = 'Light'
                  Kind = ivPickList
                  PickList.Strings = (
                    'Light'
                    'Dark')
                end>
            end>
        end
        object DemoDataGrid: TERDDataGrid
          Left = 16
          Top = 312
          Width = 460
          Height = 220
          TabOrder = 1
          TabStop = True
          DataColumns = <
            item
              Caption = 'Name'
              Width = 220
            end
            item
              Caption = 'Status'
              Width = 120
            end>
          EmptyTitle = 'No data'
        end
        object DemoTileGrid: TERDTileGrid
          Left = 506
          Top = 312
          Width = 460
          Height = 220
          TabOrder = 2
          TabStop = True
          Items = <
            item
              Icon.BuiltIn = ei005Bestand
              Caption = 'Files'
            end
            item
              Icon.BuiltIn = ei027Instellingen
              Caption = 'Settings'
            end>
        end
        object DemoKeyValueList: TERDKeyValueList
          Left = 16
          Top = 582
          Width = 460
          Height = 220
          TabOrder = 3
          Items = <
            item
              Key = 'Application'
              Value = 'Demo'
            end
            item
              Key = 'Theme'
              Value = 'ERDesigns Light'
            end>
        end
        object DemoHistogram: TERDHistogram
          Left = 506
          Top = 582
          Width = 460
          Height = 220
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          Title = 'Sample distribution'
        end
        object DemoVirtualStringTree: TERDVirtualStringTree
          Left = 16
          Top = 852
          Width = 950
          Height = 300
          Color = 16447992
          Colors.BorderColor = 15131358
          Colors.DisabledColor = 6972252
          Colors.DropMarkColor = 1607920
          Colors.DropTargetColor = 13492982
          Colors.DropTargetBorderColor = 1607920
          Colors.FocusedSelectionColor = 14674935
          Colors.FocusedSelectionBorderColor = 14674935
          Colors.GridLineColor = 15131358
          Colors.HeaderHotColor = 1607920
          Colors.HotColor = 1710618
          Colors.SelectionRectangleBlendColor = 1607920
          Colors.SelectionRectangleBorderColor = 1607920
          Colors.SelectionTextColor = 1710618
          Colors.TreeLineColor = 15131358
          Colors.UnfocusedColor = 1710618
          Colors.UnfocusedSelectionColor = 14674935
          Colors.UnfocusedSelectionBorderColor = 14674935
          DefaultNodeHeight = 32
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 1710618
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          Header.AutoSizeIndex = 1
          Header.Height = 30
          Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
          ParentFont = False
          RootNodeCount = 20
          ScrollBarOptions.ScrollBars = ssNone
          StyleElements = []
          TabOrder = 5
          TreeOptions.MiscOptions = [toCheckSupport, toEditable, toFullRepaintOnResize, toInitOnSave, toWheelPanning]
          TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowRoot, toUseBlendedImages]
          TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect]
          OnCompareNodes = TreeCompareNodes
          OnFreeNode = TreeFreeNode
          OnGetText = TreeGetText
          OnGetNodeDataSize = TreeNodeDataSize
          OnHeaderClick = TreeHeaderClick
          OnInitChildren = TreeInitChildren
          OnInitNode = TreeInitNode
          OnNewText = TreeNewText
          Touch.InteractiveGestures = [igPan, igPressAndTap]
          Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
          Theme = Theme
          OnGetCellEditor = TreeCellEditor
          Columns = <
            item
              Position = 0
              Text = 'Playlist / channel'
              Width = 380
            end
            item
              Position = 1
              Text = 'Type'
              Width = 130
            end
            item
              Position = 2
              Text = 'Quality'
              Width = 100
            end>
        end
      end
    end
    object FilesColorsPage: TERDPanel
      Left = 0
      Top = 40
      Width = 1072
      Height = 448
      Align = alClient
      TabOrder = 5
      Visible = False
      BorderWidth = 0
      object FilesColorsScroll: TERDScrollBox
        Left = 0
        Top = 0
        Width = 1072
        Height = 448
        Align = alClient
        TabOrder = 0
        object LabelBreadcrumb: TLabel
          Left = 16
          Top = 16
          Width = 93
          Height = 15
          Caption = 'TERDBreadcrumb'
        end
        object LabelPlacesPane: TLabel
          Left = 506
          Top = 16
          Width = 87
          Height = 15
          Caption = 'TERDPlacesPane'
        end
        object LabelFolderTree: TLabel
          Left = 16
          Top = 286
          Width = 83
          Height = 15
          Caption = 'TERDFolderTree'
        end
        object LabelFileList: TLabel
          Left = 506
          Top = 286
          Width = 64
          Height = 15
          Caption = 'TERDFileList'
        end
        object LabelColorSwatches: TLabel
          Left = 16
          Top = 556
          Width = 106
          Height = 15
          Caption = 'TERDColorSwatches'
        end
        object LabelColorSpectrum: TLabel
          Left = 506
          Top = 556
          Width = 108
          Height = 15
          Caption = 'TERDColorSpectrum'
        end
        object DemoBreadcrumb: TERDBreadcrumb
          Left = 16
          Top = 42
          Width = 460
          Height = 220
          TabOrder = 0
          TabStop = True
          Items = <
            item
              Caption = 'Playlists'
              Path = 'playlists'
            end
            item
              Caption = 'Belgium'
              Path = 'country:be'
            end
            item
              Caption = 'Live TV'
              Path = 'live'
            end>
          OnDropDown = BreadcrumbChoices
          OnNavigate = BreadcrumbNavigate
        end
        object DemoPlacesPane: TERDPlacesPane
          Left = 506
          Top = 42
          Width = 460
          Height = 220
          TabOrder = 1
          TabStop = True
          Places = <>
        end
        object DemoFolderTree: TERDFolderTree
          Left = 16
          Top = 312
          Width = 460
          Height = 220
          TabOrder = 2
          TabStop = True
        end
        object DemoFileList: TERDFileList
          Left = 506
          Top = 312
          Width = 460
          Height = 220
          TabOrder = 3
          TabStop = True
          Mask = '*.*'
        end
        object DemoColorSwatches: TERDColorSwatches
          Left = 16
          Top = 582
          Width = 460
          Height = 220
          TabOrder = 4
          TabStop = True
        end
        object DemoColorSpectrum: TERDColorSpectrum
          Left = 506
          Top = 582
          Width = 460
          Height = 220
          TabOrder = 5
          TabStop = True
        end
      end
    end
    object DialogsPage: TERDPanel
      Left = 0
      Top = 40
      Width = 1072
      Height = 448
      Align = alClient
      TabOrder = 6
      Visible = False
      BorderWidth = 0
      object DialogsScroll: TERDScrollBox
        Left = 0
        Top = 0
        Width = 1072
        Height = 448
        Align = alClient
        TabOrder = 0
        object HintSample: TLabel
          Left = 16
          Top = 395
          Width = 195
          Height = 15
          Hint = 'ERD hint preview|A general tooltip with more detail.'
          Caption = 'Hover here to preview TERDHintStyle'
          ParentShowHint = False
          ShowHint = True
        end
        object LaunchDialog: TERDButton
          Left = 16
          Top = 16
          Width = 290
          Height = 44
          TabOrder = 0
          OnClick = ExecuteDemoDialog
          AutoSize = False
          Caption = 'TERDDialog'
        end
        object LaunchOpenDialog: TERDButton
          Tag = 1
          Left = 326
          Top = 16
          Width = 290
          Height = 44
          TabOrder = 1
          OnClick = ExecuteDemoDialog
          AutoSize = False
          Caption = 'TERDOpenDialog'
        end
        object LaunchSaveDialog: TERDButton
          Tag = 2
          Left = 636
          Top = 16
          Width = 290
          Height = 44
          TabOrder = 2
          OnClick = ExecuteDemoDialog
          AutoSize = False
          Caption = 'TERDSaveDialog'
        end
        object LaunchOpenPictureDialog: TERDButton
          Tag = 3
          Left = 16
          Top = 74
          Width = 290
          Height = 44
          TabOrder = 3
          OnClick = ExecuteDemoDialog
          AutoSize = False
          Caption = 'TERDOpenPictureDialog'
        end
        object LaunchSavePictureDialog: TERDButton
          Tag = 4
          Left = 326
          Top = 74
          Width = 290
          Height = 44
          TabOrder = 4
          OnClick = ExecuteDemoDialog
          AutoSize = False
          Caption = 'TERDSavePictureDialog'
        end
        object LaunchFolderDialog: TERDButton
          Tag = 5
          Left = 636
          Top = 74
          Width = 290
          Height = 44
          TabOrder = 5
          OnClick = ExecuteDemoDialog
          AutoSize = False
          Caption = 'TERDFolderDialog'
        end
        object LaunchPrintDialog: TERDButton
          Tag = 6
          Left = 16
          Top = 132
          Width = 290
          Height = 44
          TabOrder = 6
          OnClick = ExecuteDemoDialog
          AutoSize = False
          Caption = 'TERDPrintDialog'
        end
        object LaunchPageSetupDialog: TERDButton
          Tag = 7
          Left = 326
          Top = 132
          Width = 290
          Height = 44
          TabOrder = 7
          OnClick = ExecuteDemoDialog
          AutoSize = False
          Caption = 'TERDPageSetupDialog'
        end
        object LaunchFontDialog: TERDButton
          Tag = 8
          Left = 636
          Top = 132
          Width = 290
          Height = 44
          TabOrder = 8
          OnClick = ExecuteDemoDialog
          AutoSize = False
          Caption = 'TERDFontDialog'
        end
        object LaunchColorDialog: TERDButton
          Tag = 9
          Left = 16
          Top = 190
          Width = 290
          Height = 44
          TabOrder = 9
          OnClick = ExecuteDemoDialog
          AutoSize = False
          Caption = 'TERDColorDialog'
        end
        object LaunchFindDialog: TERDButton
          Tag = 10
          Left = 326
          Top = 190
          Width = 290
          Height = 44
          TabOrder = 10
          OnClick = ExecuteDemoDialog
          AutoSize = False
          Caption = 'TERDFindDialog'
        end
        object LaunchReplaceDialog: TERDButton
          Tag = 11
          Left = 636
          Top = 190
          Width = 290
          Height = 44
          TabOrder = 11
          OnClick = ExecuteDemoDialog
          AutoSize = False
          Caption = 'TERDReplaceDialog'
        end
        object LaunchTaskDialog: TERDButton
          Tag = 12
          Left = 16
          Top = 248
          Width = 290
          Height = 44
          TabOrder = 12
          OnClick = ExecuteDemoDialog
          AutoSize = False
          Caption = 'TERDTaskDialog'
        end
        object LaunchShowToast: TERDButton
          Left = 16
          Top = 330
          Width = 290
          Height = 44
          TabOrder = 13
          OnClick = ShowToast
          AutoSize = False
          Caption = 'Show toast'
        end
        object LaunchShowPopup: TERDButton
          Left = 326
          Top = 330
          Width = 290
          Height = 44
          TabOrder = 14
          OnClick = ShowPopup
          AutoSize = False
          Caption = 'Open popup menu'
        end
      end
    end
  end
  object StatusHost: TERDPanel
    Left = 0
    Top = 716
    Width = 300
    Height = 32
    TabOrder = 3
    BorderWidth = 0
    object BadgeOne: TERDBadge
      Left = 8
      Top = 7
      Width = 20
      Height = 18
      Count = 5
    end
    object BadgeTwo: TERDBadge
      Left = 46
      Top = 7
      Width = 20
      Height = 18
      Count = 12
      Kind = skSuccess
    end
    object DownloadButton: TERDButton
      Left = 96
      Top = 2
      Width = 170
      Height = 30
      TabOrder = 0
      OnClick = ShowDialog
      AutoSize = False
      Caption = 'Downloads'
      Icon.BuiltIn = ei017Download
      Kind = bkGhost
    end
  end
  object StatusBar: TERDStatusBar
    Left = 0
    Top = 732
    Width = 1280
    Height = 28
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    Panels = <
      item
        Kind = spkStatus
        Text = 'Ready'
        Width = 120
        StatusKind = skSuccess
      end
      item
        Kind = spkProgress
        Text = 'Example progress'
        Width = 280
        Position = 35
      end
      item
        Width = 310
        Alignment = paRight
      end>
  end
  object DemoBackstage: TERDBackstage
    Left = 208
    Top = 244
    Width = 1072
    Height = 488
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    Visible = False
    Items = <
      item
        Caption = 'Information'
        Page = BackstageInfo
      end
      item
        Caption = 'Settings'
        Page = BackstageSettings
      end>
    OnClose = BackstageClosed
    object BackstageInfo: TERDPanel
      Left = 0
      Top = 0
      Width = 240
      Height = 120
      TabOrder = 0
      Visible = False
      HeaderHeight = 32
      Caption = 'Application information'
      object BackstageInfoText: TLabel
        Left = 12
        Top = 48
        Width = 193
        Height = 15
        Caption = 'A page supplied by your application.'
      end
    end
    object BackstageSettings: TERDPanel
      Left = 0
      Top = 0
      Width = 240
      Height = 120
      TabOrder = 1
      Visible = False
      HeaderHeight = 32
      Caption = 'Application settings'
    end
  end
  object Theme: TERDTheme
    Left = 880
    Top = 8
  end
  object Images: TImageList
    Height = 24
    Width = 24
    Left = 920
    Top = 8
  end
  object MainMenu: TMainMenu
    Left = 960
    Top = 8
    object FileMenu: TERDMenuItem
      Caption = 'File'
      object OpenMenu: TERDMenuItem
        Caption = 'Open'
        OnClick = OpenFile
        Icon.BuiltIn = ei002Openen
      end
      object DialogMenu: TERDMenuItem
        Caption = 'Dialog'
        OnClick = ShowDialog
        Icon.BuiltIn = ei028Info
      end
    end
  end
  object DemoDialog: TERDDialog
    Theme = Theme
    Title = 'General dialog'
    Text = 'Example dialog content.'
    Buttons = [mbOK]
    Left = 16
    Top = 650
  end
  object DemoCommandPalette: TERDCommandPalette
    Theme = Theme
    Commands = <
      item
        Key = 'open'
        Caption = 'Open file'
        Category = 'Files'
        Keywords = 'browse import playlist'
        Icon.BuiltIn = ei002Openen
      end
      item
        Key = 'theme'
        Caption = 'Switch light / dark'
        Category = 'Appearance'
        Keywords = 'theme colors'
        Icon.BuiltIn = ei027Instellingen
      end
      item
        Key = 'dialog'
        Caption = 'Show example dialog'
        Category = 'Application'
        Keywords = 'message help'
        Icon.BuiltIn = ei028Info
      end>
    OnExecute = CommandExecuted
  end
  object DemoOpenDialog: TERDOpenDialog
    Theme = Theme
    Filter = 'All files|*.*'
    Options = [foPathMustExist, foFileMustExist]
    Places = <>
    Left = 56
    Top = 650
  end
  object DemoSaveDialog: TERDSaveDialog
    Theme = Theme
    Filter = 'All files|*.*'
    Options = [foPathMustExist, foFileMustExist]
    Places = <>
    Left = 96
    Top = 650
  end
  object DemoOpenPictureDialog: TERDOpenPictureDialog
    Theme = Theme
    Filter = 
      'All (*.wbmp;*.webp;*.svg;*.gif;*.jpg;*.jpeg;*.png;*.bmp;*.ico;*.' +
      'emf;*.wmf;*.tif;*.tiff)|*.wbmp;*.webp;*.svg;*.gif;*.jpg;*.jpeg;*' +
      '.png;*.bmp;*.ico;*.emf;*.wmf;*.tif;*.tiff|WBMP Images (*.wbmp)|*' +
      '.wbmp|WebP Images (*.webp)|*.webp|Scalable Vector Graphics (*.sv' +
      'g)|*.svg|GIF Image (*.gif)|*.gif|JPEG Image File (*.jpg)|*.jpg|J' +
      'PEG Image File (*.jpeg)|*.jpeg|Portable Network Graphics (*.png)' +
      '|*.png|Bitmaps (*.bmp)|*.bmp|Icons (*.ico)|*.ico|Enhanced Metafi' +
      'les (*.emf)|*.emf|Metafiles (*.wmf)|*.wmf|TIFF Images (*.tif)|*.' +
      'tif|TIFF Images (*.tiff)|*.tiff'
    Options = [foPathMustExist, foFileMustExist]
    Places = <>
    Left = 136
    Top = 650
  end
  object DemoSavePictureDialog: TERDSavePictureDialog
    Theme = Theme
    Filter = 
      'All (*.wbmp;*.webp;*.svg;*.gif;*.jpg;*.jpeg;*.png;*.bmp;*.ico;*.' +
      'emf;*.wmf;*.tif;*.tiff)|*.wbmp;*.webp;*.svg;*.gif;*.jpg;*.jpeg;*' +
      '.png;*.bmp;*.ico;*.emf;*.wmf;*.tif;*.tiff|WBMP Images (*.wbmp)|*' +
      '.wbmp|WebP Images (*.webp)|*.webp|Scalable Vector Graphics (*.sv' +
      'g)|*.svg|GIF Image (*.gif)|*.gif|JPEG Image File (*.jpg)|*.jpg|J' +
      'PEG Image File (*.jpeg)|*.jpeg|Portable Network Graphics (*.png)' +
      '|*.png|Bitmaps (*.bmp)|*.bmp|Icons (*.ico)|*.ico|Enhanced Metafi' +
      'les (*.emf)|*.emf|Metafiles (*.wmf)|*.wmf|TIFF Images (*.tif)|*.' +
      'tif|TIFF Images (*.tiff)|*.tiff'
    Options = [foPathMustExist, foFileMustExist]
    Places = <>
    Left = 176
    Top = 650
  end
  object DemoFolderDialog: TERDFolderDialog
    Theme = Theme
    Options = []
    Left = 216
    Top = 650
  end
  object DemoPrintDialog: TERDPrintDialog
    Theme = Theme
    Options = [opoPageNums, opoSelection, opoPrintToFile]
    Left = 256
    Top = 650
  end
  object DemoPageSetupDialog: TERDPageSetupDialog
    Theme = Theme
    PaperName = 'A4'
    PaperWidth = 210.000000000000000000
    PaperHeight = 297.000000000000000000
    MarginLeft = 20.000000000000000000
    MarginTop = 25.000000000000000000
    MarginRight = 20.000000000000000000
    MarginBottom = 20.000000000000000000
    Left = 296
    Top = 650
  end
  object DemoFontDialog: TERDFontDialog
    Theme = Theme
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    Options = [fdoEffects]
    Left = 336
    Top = 650
  end
  object DemoColorDialog: TERDColorDialog
    Theme = Theme
    Left = 376
    Top = 650
  end
  object DemoFindDialog: TERDFindDialog
    Theme = Theme
    Options = []
    Left = 416
    Top = 650
  end
  object DemoReplaceDialog: TERDReplaceDialog
    Theme = Theme
    Options = []
    Left = 456
    Top = 650
  end
  object DemoTaskDialog: TERDTaskDialog
    Theme = Theme
    Title = 'Task dialog'
    Caption = 'Choose an action'
    MainInstruction = 'Choose an action'
    Text = 'General application task.'
    Buttons = <>
    RadioButtons = <>
    Left = 16
    Top = 690
  end
  object DemoToastManager: TERDToastManager
    Theme = Theme
    Left = 56
    Top = 690
  end
  object DemoHintStyle: TERDHintStyle
    Theme = Theme
    Left = 96
    Top = 690
  end
  object DemoPopupMenu: TERDPopupMenu
    Theme = Theme
    Left = 136
    Top = 690
    object PopupAction: TERDMenuItem
      Caption = 'Show dialog'
      OnClick = ShowDialog
      Icon.BuiltIn = ei028Info
    end
  end
end
