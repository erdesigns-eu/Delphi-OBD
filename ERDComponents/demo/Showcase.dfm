object MainForm: TShowcaseForm
  Left = 0
  Top = 0
  ClientWidth = 1280
  ClientHeight = 760
  Caption = 'ERD Components - general VCL showcase'
  Position = poScreenCenter
  OnCreate = FormCreate
  object Chrome: TERDPanel
    Left = 0
    Top = 0
    Width = 1280
    Height = 244
    Align = alTop
    BorderWidth = 0
    OnResize = LayoutChrome
    object TitleBar: TERDTitleBar
      TabStop = True
      AutoSaveVisible = True
      AutoSave = True
      OnAutoSaveChange = AutoSaveChanged
      Left = 0
      Top = 0
      Width = 1280
      Height = 48
      Align = alNone
      Anchors = [akLeft, akTop, akRight]
      OnResize = LayoutChrome
      OnCommandStyleChange = LayoutChrome
      Title = 'ERD Components'
      Subtitle = 'General VCL showcase'
      AppInitials = 'ERD'
      CommandStyle = csRibbon
      MenuPlacement = mpBelow
      Ribbon = Ribbon
      Menu = MainMenu
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
        end
      >
      Buttons = <
        item
          Icon.BuiltIn = ei027Instellingen
          Hint = 'Light / dark'
          OnClick = ToggleTheme
        end
      >
    end
    object Ribbon: TERDRibbon
      Backstage = DemoBackstage
      OnFileClick = ShowBackstage
      Left = 0
      Top = 48
      Width = 1280
      Height = 156
      Align = alNone
      Anchors = [akLeft, akTop, akRight]
      OnResize = LayoutChrome
      Tabs = <
        item
          Caption = 'General'
          Groups = <
            item
              Caption = 'Application'
              Items = <
                item
                  Caption = 'Open'
                  Icon.BuiltIn = ei002Openen
                  Size = risLarge
                  OnClick = OpenFile
                end
                item
                  Caption = 'Dialog'
                  Icon.BuiltIn = ei028Info
                  Size = risLarge
                  OnClick = ShowDialog
                end
                item
                  Caption = 'Light / dark'
                  Icon.BuiltIn = ei027Instellingen
                  Size = risLarge
                  OnClick = ToggleTheme
                end
              >
            end
          >
        end
        item
          Caption = 'Media'
          Groups = <
            item
              Caption = 'Player'
              Items = <
                item
                  Caption = 'Play'
                  Kind = rikToggle
                  GroupIndex = 1
                  Down = True
                  Icon.BuiltIn = ei161Afspelen
                  Size = risLarge
                end
                item
                  Caption = 'Pause'
                  Kind = rikToggle
                  GroupIndex = 1
                  Icon.BuiltIn = ei162Pauzeren
                  Size = risLarge
                end
                item
                  Caption = 'Teletext'
                  Kind = rikToggle
                  Icon.BuiltIn = ei249Teletext
                  Size = risLarge
                end
                item
                  Caption = 'Subtitles'
                  Kind = rikToggle
                  Icon.BuiltIn = ei192Ondertiteling
                  Size = risLarge
                end
              >
            end
          >
        end
      >
    end
    object Choices: TERDPanel
      Left = 0
      Top = 204
      Width = 970
      Height = 40
      Align = alNone
      Anchors = [akLeft, akTop, akRight]
      BorderWidth = 0
      object ThemeChoice: TERDComboBox
        Left = 0
        Top = 2
        Width = 220
        Height = 34
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
          'Office OneNote Dark'
        )
        ItemIndex = 0
        OnChange = ThemeSelected
      end
      object AppearanceChoice: TERDComboBox
        Left = 240
        Top = 2
        Width = 220
        Height = 34
        Items.Strings = (
          'ERDesigns ribbon'
          'Office ribbon'
          'Flat ribbon'
        )
        ItemIndex = 0
        OnChange = AppearanceSelected
      end
      object LayoutChoice: TERDComboBox
        Left = 480
        Top = 2
        Width = 220
        Height = 34
        Items.Strings = (
          'Classic layout'
          'Simplified layout'
        )
        ItemIndex = 0
        OnChange = LayoutSelected
      end
      object CommandChoice: TERDComboBox
        Left = 720
        Top = 2
        Width = 220
        Height = 34
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
    Top = 204
    Width = 200
    Height = 512
    Align = alLeft
    ItemIndex = 0
    OnItemClick = SidebarSelected
    Items = <
      item
        Caption = 'Overview'
        Icon.BuiltIn = ei028Info
        Tag = 0
      end
      item
        Caption = 'General icons'
        Icon.BuiltIn = ei005Bestand
        Tag = 112
      end
      item
        Caption = 'Desktop'
        Icon.BuiltIn = ei027Instellingen
        Tag = 576
      end
      item
        Caption = 'Data'
        Icon.BuiltIn = ei081Database
        Tag = 1040
      end
      item
        Caption = 'Media player'
        Icon.BuiltIn = ei161Afspelen
        Tag = 1968
      end
      item
        Caption = 'Playlists'
        Icon.BuiltIn = ei017Download
        Tag = 2432
        Badge = 5
      end
      item
        Caption = 'Live TV / VOD'
        Icon.BuiltIn = ei249Teletext
        Tag = 2896
        Badge = 12
      end
      item
        Caption = 'Settings'
        Icon.BuiltIn = ei027Instellingen
        Tag = 0
        Footer = True
      end
      item
        Caption = 'Inputs'
        Icon.BuiltIn = ei005Bestand
        Tag = -1
      end
      item
        Caption = 'Layout'
        Icon.BuiltIn = ei005Bestand
        Tag = -2
      end
      item
        Caption = 'Data'
        Icon.BuiltIn = ei005Bestand
        Tag = -3
      end
      item
        Caption = 'Files and colors'
        Icon.BuiltIn = ei005Bestand
        Tag = -4
      end
      item
        Caption = 'Dialogs'
        Icon.BuiltIn = ei005Bestand
        Tag = -5
      end
    >
  end
  object DemoPages: TERDPanel
    Left = 200
    Top = 204
    Width = 1080
    Height = 512
    Align = alClient
    BorderWidth = 0
    object PageTabs: TERDTabs
      Left = 0
      Top = 0
      Width = 1080
      Height = 36
      Align = alTop
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
        end
      >
      TabIndex = 0
      OnChange = PageSelected
    end
    object IconsPage: TERDPanel
      Align = alClient
      BorderWidth = 0
      Visible = True
        object Gallery: TERDScrollBox
          Left = 0
          Top = 0
          Width = 1080
          Height = 530
          Align = alClient
          object ImageButton: TERDButton
            Left = 12
            Top = 8
            Width = 184
            Height = 48
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
            AutoSize = False
            Caption = 'Own glyph'
            Icon.Source = eisGlyph
          end
          object NoIconButton: TERDButton
            Left = 412
            Top = 8
            Width = 184
            Height = 48
            AutoSize = False
            Caption = 'No icon'
            Icon.Source = eisNone
          end
          object IconButton001: TERDButton
            Left = 12
            Top = 112
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '001 Nieuw'
            Icon.BuiltIn = ei001Nieuw
            OnClick = IconClicked
          end
          object IconButton002: TERDButton
            Left = 206
            Top = 112
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '002 Openen'
            Icon.BuiltIn = ei002Openen
            OnClick = IconClicked
          end
          object IconButton003: TERDButton
            Left = 400
            Top = 112
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '003 Opslaan'
            Icon.BuiltIn = ei003Opslaan
            OnClick = IconClicked
          end
          object IconButton004: TERDButton
            Left = 594
            Top = 112
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '004 Opslaan als'
            Icon.BuiltIn = ei004Opslaanals
            OnClick = IconClicked
          end
          object IconButton005: TERDButton
            Left = 788
            Top = 112
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '005 Bestand'
            Icon.BuiltIn = ei005Bestand
            OnClick = IconClicked
          end
          object IconButton006: TERDButton
            Left = 12
            Top = 170
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '006 Map'
            Icon.BuiltIn = ei006Map
            OnClick = IconClicked
          end
          object IconButton007: TERDButton
            Left = 206
            Top = 170
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '007 Kopiëren'
            Icon.BuiltIn = ei007Kopieren
            OnClick = IconClicked
          end
          object IconButton008: TERDButton
            Left = 400
            Top = 170
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '008 Plakken'
            Icon.BuiltIn = ei008Plakken
            OnClick = IconClicked
          end
          object IconButton009: TERDButton
            Left = 594
            Top = 170
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '009 Knippen'
            Icon.BuiltIn = ei009Knippen
            OnClick = IconClicked
          end
          object IconButton010: TERDButton
            Left = 788
            Top = 170
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '010 Ongedaan'
            Icon.BuiltIn = ei010Ongedaan
            OnClick = IconClicked
          end
          object IconButton011: TERDButton
            Left = 12
            Top = 228
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '011 Opnieuw'
            Icon.BuiltIn = ei011Opnieuw
            OnClick = IconClicked
          end
          object IconButton012: TERDButton
            Left = 206
            Top = 228
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '012 Verwijderen'
            Icon.BuiltIn = ei012Verwijderen
            OnClick = IconClicked
          end
          object IconButton013: TERDButton
            Left = 400
            Top = 228
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '013 Bewerken'
            Icon.BuiltIn = ei013Bewerken
            OnClick = IconClicked
          end
          object IconButton014: TERDButton
            Left = 594
            Top = 228
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '014 Zoeken'
            Icon.BuiltIn = ei014Zoeken
            OnClick = IconClicked
          end
          object IconButton015: TERDButton
            Left = 788
            Top = 228
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '015 Filter'
            Icon.BuiltIn = ei015Filter
            OnClick = IconClicked
          end
          object IconButton016: TERDButton
            Left = 12
            Top = 286
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '016 Vernieuwen'
            Icon.BuiltIn = ei016Vernieuwen
            OnClick = IconClicked
          end
          object IconButton017: TERDButton
            Left = 206
            Top = 286
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '017 Download'
            Icon.BuiltIn = ei017Download
            OnClick = IconClicked
          end
          object IconButton018: TERDButton
            Left = 400
            Top = 286
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '018 Upload'
            Icon.BuiltIn = ei018Upload
            OnClick = IconClicked
          end
          object IconButton019: TERDButton
            Left = 594
            Top = 286
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '019 Import'
            Icon.BuiltIn = ei019Import
            OnClick = IconClicked
          end
          object IconButton020: TERDButton
            Left = 788
            Top = 286
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '020 Export'
            Icon.BuiltIn = ei020Export
            OnClick = IconClicked
          end
          object IconButton021: TERDButton
            Left = 12
            Top = 344
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '021 Print'
            Icon.BuiltIn = ei021Print
            OnClick = IconClicked
          end
          object IconButton022: TERDButton
            Left = 206
            Top = 344
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '022 PDF'
            Icon.BuiltIn = ei022PDF
            OnClick = IconClicked
          end
          object IconButton023: TERDButton
            Left = 400
            Top = 344
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '023 Rapport'
            Icon.BuiltIn = ei023Rapport
            OnClick = IconClicked
          end
          object IconButton024: TERDButton
            Left = 594
            Top = 344
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '024 Mail'
            Icon.BuiltIn = ei024Mail
            OnClick = IconClicked
          end
          object IconButton025: TERDButton
            Left = 788
            Top = 344
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '025 Gebruiker'
            Icon.BuiltIn = ei025Gebruiker
            OnClick = IconClicked
          end
          object IconButton026: TERDButton
            Left = 12
            Top = 402
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '026 Gebruikers'
            Icon.BuiltIn = ei026Gebruikers
            OnClick = IconClicked
          end
          object IconButton027: TERDButton
            Left = 206
            Top = 402
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '027 Instellingen'
            Icon.BuiltIn = ei027Instellingen
            OnClick = IconClicked
          end
          object IconButton028: TERDButton
            Left = 400
            Top = 402
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '028 Info'
            Icon.BuiltIn = ei028Info
            OnClick = IconClicked
          end
          object IconButton029: TERDButton
            Left = 594
            Top = 402
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '029 Help'
            Icon.BuiltIn = ei029Help
            OnClick = IconClicked
          end
          object IconButton030: TERDButton
            Left = 788
            Top = 402
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '030 Waarschuwing'
            Icon.BuiltIn = ei030Waarschuwing
            OnClick = IconClicked
          end
          object IconButton031: TERDButton
            Left = 12
            Top = 460
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '031 Fout'
            Icon.BuiltIn = ei031Fout
            OnClick = IconClicked
          end
          object IconButton032: TERDButton
            Left = 206
            Top = 460
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '032 Succes'
            Icon.BuiltIn = ei032Succes
            OnClick = IconClicked
          end
          object IconButton033: TERDButton
            Left = 400
            Top = 460
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '033 Terug'
            Icon.BuiltIn = ei033Terug
            OnClick = IconClicked
          end
          object IconButton034: TERDButton
            Left = 594
            Top = 460
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '034 Vooruit'
            Icon.BuiltIn = ei034Vooruit
            OnClick = IconClicked
          end
          object IconButton035: TERDButton
            Left = 788
            Top = 460
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '035 Omhoog'
            Icon.BuiltIn = ei035Omhoog
            OnClick = IconClicked
          end
          object IconButton036: TERDButton
            Left = 12
            Top = 518
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '036 Omlaag'
            Icon.BuiltIn = ei036Omlaag
            OnClick = IconClicked
          end
          object IconButton037: TERDButton
            Left = 206
            Top = 518
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '037 Vergrendelen'
            Icon.BuiltIn = ei037Vergrendelen
            OnClick = IconClicked
          end
          object IconButton038: TERDButton
            Left = 400
            Top = 518
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '038 Ontgrendelen'
            Icon.BuiltIn = ei038Ontgrendelen
            OnClick = IconClicked
          end
          object IconButton039: TERDButton
            Left = 594
            Top = 518
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '039 Meldingen'
            Icon.BuiltIn = ei039Meldingen
            OnClick = IconClicked
          end
          object IconButton040: TERDButton
            Left = 788
            Top = 518
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '040 Meer'
            Icon.BuiltIn = ei040Meer
            OnClick = IconClicked
          end
          object IconButton041: TERDButton
            Left = 12
            Top = 576
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '041 Minimaliseren'
            Icon.BuiltIn = ei041Minimaliseren
            OnClick = IconClicked
          end
          object IconButton042: TERDButton
            Left = 206
            Top = 576
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '042 Maximaliseren'
            Icon.BuiltIn = ei042Maximaliseren
            OnClick = IconClicked
          end
          object IconButton043: TERDButton
            Left = 400
            Top = 576
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '043 Herstellen'
            Icon.BuiltIn = ei043Herstellen
            OnClick = IconClicked
          end
          object IconButton044: TERDButton
            Left = 594
            Top = 576
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '044 Volledig scherm'
            Icon.BuiltIn = ei044Volledigscherm
            OnClick = IconClicked
          end
          object IconButton045: TERDButton
            Left = 788
            Top = 576
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '045 Scherm verlaten'
            Icon.BuiltIn = ei045Schermverlaten
            OnClick = IconClicked
          end
          object IconButton046: TERDButton
            Left = 12
            Top = 634
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '046 Venster sluiten'
            Icon.BuiltIn = ei046Venstersluiten
            OnClick = IconClicked
          end
          object IconButton047: TERDButton
            Left = 206
            Top = 634
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '047 Venster wisselen'
            Icon.BuiltIn = ei047Vensterwisselen
            OnClick = IconClicked
          end
          object IconButton048: TERDButton
            Left = 400
            Top = 634
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '048 Vensters naast elkaar'
            Icon.BuiltIn = ei048Venstersnaastelkaar
            OnClick = IconClicked
          end
          object IconButton049: TERDButton
            Left = 594
            Top = 634
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '049 Nieuwe tab'
            Icon.BuiltIn = ei049Nieuwetab
            OnClick = IconClicked
          end
          object IconButton050: TERDButton
            Left = 788
            Top = 634
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '050 Tab sluiten'
            Icon.BuiltIn = ei050Tabsluiten
            OnClick = IconClicked
          end
          object IconButton051: TERDButton
            Left = 12
            Top = 692
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '051 Volgende tab'
            Icon.BuiltIn = ei051Volgendetab
            OnClick = IconClicked
          end
          object IconButton052: TERDButton
            Left = 206
            Top = 692
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '052 Vorige tab'
            Icon.BuiltIn = ei052Vorigetab
            OnClick = IconClicked
          end
          object IconButton053: TERDButton
            Left = 400
            Top = 692
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '053 Vastzetten'
            Icon.BuiltIn = ei053Vastzetten
            OnClick = IconClicked
          end
          object IconButton054: TERDButton
            Left = 594
            Top = 692
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '054 Losmaken'
            Icon.BuiltIn = ei054Losmaken
            OnClick = IconClicked
          end
          object IconButton055: TERDButton
            Left = 788
            Top = 692
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '055 Paneel inklappen'
            Icon.BuiltIn = ei055Paneelinklappen
            OnClick = IconClicked
          end
          object IconButton056: TERDButton
            Left = 12
            Top = 750
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '056 Paneel uitklappen'
            Icon.BuiltIn = ei056Paneeluitklappen
            OnClick = IconClicked
          end
          object IconButton057: TERDButton
            Left = 206
            Top = 750
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '057 Zijbalk tonen'
            Icon.BuiltIn = ei057Zijbalktonen
            OnClick = IconClicked
          end
          object IconButton058: TERDButton
            Left = 400
            Top = 750
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '058 Zijbalk verbergen'
            Icon.BuiltIn = ei058Zijbalkverbergen
            OnClick = IconClicked
          end
          object IconButton059: TERDButton
            Left = 594
            Top = 750
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '059 Rasterweergave'
            Icon.BuiltIn = ei059Rasterweergave
            OnClick = IconClicked
          end
          object IconButton060: TERDButton
            Left = 788
            Top = 750
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '060 Lijstweergave'
            Icon.BuiltIn = ei060Lijstweergave
            OnClick = IconClicked
          end
          object IconButton061: TERDButton
            Left = 12
            Top = 808
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '061 Detailweergave'
            Icon.BuiltIn = ei061Detailweergave
            OnClick = IconClicked
          end
          object IconButton062: TERDButton
            Left = 206
            Top = 808
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '062 Kaartweergave'
            Icon.BuiltIn = ei062Kaartweergave
            OnClick = IconClicked
          end
          object IconButton063: TERDButton
            Left = 400
            Top = 808
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '063 Alles selecteren'
            Icon.BuiltIn = ei063Allesselecteren
            OnClick = IconClicked
          end
          object IconButton064: TERDButton
            Left = 594
            Top = 808
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '064 Selectie wissen'
            Icon.BuiltIn = ei064Selectiewissen
            OnClick = IconClicked
          end
          object IconButton065: TERDButton
            Left = 788
            Top = 808
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '065 Selectie omkeren'
            Icon.BuiltIn = ei065Selectieomkeren
            OnClick = IconClicked
          end
          object IconButton066: TERDButton
            Left = 12
            Top = 866
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '066 Zoom in'
            Icon.BuiltIn = ei066Zoomin
            OnClick = IconClicked
          end
          object IconButton067: TERDButton
            Left = 206
            Top = 866
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '067 Zoom uit'
            Icon.BuiltIn = ei067Zoomuit
            OnClick = IconClicked
          end
          object IconButton068: TERDButton
            Left = 400
            Top = 866
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '068 Zoom herstellen'
            Icon.BuiltIn = ei068Zoomherstellen
            OnClick = IconClicked
          end
          object IconButton069: TERDButton
            Left = 594
            Top = 866
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '069 Nieuwe map'
            Icon.BuiltIn = ei069Nieuwemap
            OnClick = IconClicked
          end
          object IconButton070: TERDButton
            Left = 788
            Top = 866
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '070 Map hernoemen'
            Icon.BuiltIn = ei070Maphernoemen
            OnClick = IconClicked
          end
          object IconButton071: TERDButton
            Left = 12
            Top = 924
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '071 Bestand hernoemen'
            Icon.BuiltIn = ei071Bestandhernoemen
            OnClick = IconClicked
          end
          object IconButton072: TERDButton
            Left = 206
            Top = 924
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '072 Dupliceren'
            Icon.BuiltIn = ei072Dupliceren
            OnClick = IconClicked
          end
          object IconButton073: TERDButton
            Left = 400
            Top = 924
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '073 Archief maken'
            Icon.BuiltIn = ei073Archiefmaken
            OnClick = IconClicked
          end
          object IconButton074: TERDButton
            Left = 594
            Top = 924
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '074 Uitpakken'
            Icon.BuiltIn = ei074Uitpakken
            OnClick = IconClicked
          end
          object IconButton075: TERDButton
            Left = 788
            Top = 924
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '075 Alles opslaan'
            Icon.BuiltIn = ei075Allesopslaan
            OnClick = IconClicked
          end
          object IconButton076: TERDButton
            Left = 12
            Top = 982
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '076 Printvoorbeeld'
            Icon.BuiltIn = ei076Printvoorbeeld
            OnClick = IconClicked
          end
          object IconButton077: TERDButton
            Left = 206
            Top = 982
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '077 Pagina-instelling'
            Icon.BuiltIn = ei077Paginainstelling
            OnClick = IconClicked
          end
          object IconButton078: TERDButton
            Left = 400
            Top = 982
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '078 Toetsenbord'
            Icon.BuiltIn = ei078Toetsenbord
            OnClick = IconClicked
          end
          object IconButton079: TERDButton
            Left = 594
            Top = 982
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '079 Muis'
            Icon.BuiltIn = ei079Muis
            OnClick = IconClicked
          end
          object IconButton080: TERDButton
            Left = 788
            Top = 982
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '080 Touch'
            Icon.BuiltIn = ei080Touch
            OnClick = IconClicked
          end
          object IconButton081: TERDButton
            Left = 12
            Top = 1040
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '081 Database'
            Icon.BuiltIn = ei081Database
            OnClick = IconClicked
          end
          object IconButton082: TERDButton
            Left = 206
            Top = 1040
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '082 Database toevoegen'
            Icon.BuiltIn = ei082Databasetoevoegen
            OnClick = IconClicked
          end
          object IconButton083: TERDButton
            Left = 400
            Top = 1040
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '083 Database verwijderen'
            Icon.BuiltIn = ei083Databaseverwijderen
            OnClick = IconClicked
          end
          object IconButton084: TERDButton
            Left = 594
            Top = 1040
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '084 Database verbinden'
            Icon.BuiltIn = ei084Databaseverbinden
            OnClick = IconClicked
          end
          object IconButton085: TERDButton
            Left = 788
            Top = 1040
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '085 Database backup'
            Icon.BuiltIn = ei085Databasebackup
            OnClick = IconClicked
          end
          object IconButton086: TERDButton
            Left = 12
            Top = 1098
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '086 Database herstellen'
            Icon.BuiltIn = ei086Databaseherstellen
            OnClick = IconClicked
          end
          object IconButton087: TERDButton
            Left = 206
            Top = 1098
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '087 Tabel'
            Icon.BuiltIn = ei087Tabel
            OnClick = IconClicked
          end
          object IconButton088: TERDButton
            Left = 400
            Top = 1098
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '088 Rij toevoegen'
            Icon.BuiltIn = ei088Rijtoevoegen
            OnClick = IconClicked
          end
          object IconButton089: TERDButton
            Left = 594
            Top = 1098
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '089 Rij verwijderen'
            Icon.BuiltIn = ei089Rijverwijderen
            OnClick = IconClicked
          end
          object IconButton090: TERDButton
            Left = 788
            Top = 1098
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '090 Kolom toevoegen'
            Icon.BuiltIn = ei090Kolomtoevoegen
            OnClick = IconClicked
          end
          object IconButton091: TERDButton
            Left = 12
            Top = 1156
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '091 Kolom verwijderen'
            Icon.BuiltIn = ei091Kolomverwijderen
            OnClick = IconClicked
          end
          object IconButton092: TERDButton
            Left = 206
            Top = 1156
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '092 Sorteren oplopend'
            Icon.BuiltIn = ei092Sorterenoplopend
            OnClick = IconClicked
          end
          object IconButton093: TERDButton
            Left = 400
            Top = 1156
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '093 Sorteren aflopend'
            Icon.BuiltIn = ei093Sorterenaflopend
            OnClick = IconClicked
          end
          object IconButton094: TERDButton
            Left = 594
            Top = 1156
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '094 Groeperen'
            Icon.BuiltIn = ei094Groeperen
            OnClick = IconClicked
          end
          object IconButton095: TERDButton
            Left = 788
            Top = 1156
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '095 Groep opheffen'
            Icon.BuiltIn = ei095Groepopheffen
            OnClick = IconClicked
          end
          object IconButton096: TERDButton
            Left = 12
            Top = 1214
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '096 Filter toevoegen'
            Icon.BuiltIn = ei096Filtertoevoegen
            OnClick = IconClicked
          end
          object IconButton097: TERDButton
            Left = 206
            Top = 1214
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '097 Filters wissen'
            Icon.BuiltIn = ei097Filterswissen
            OnClick = IconClicked
          end
          object IconButton098: TERDButton
            Left = 400
            Top = 1214
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '098 CSV bestand'
            Icon.BuiltIn = ei098CSVbestand
            OnClick = IconClicked
          end
          object IconButton099: TERDButton
            Left = 594
            Top = 1214
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '099 JSON bestand'
            Icon.BuiltIn = ei099JSONbestand
            OnClick = IconClicked
          end
          object IconButton100: TERDButton
            Left = 788
            Top = 1214
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '100 XML bestand'
            Icon.BuiltIn = ei100XMLbestand
            OnClick = IconClicked
          end
          object IconButton101: TERDButton
            Left = 12
            Top = 1272
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '101 Spreadsheet'
            Icon.BuiltIn = ei101Spreadsheet
            OnClick = IconClicked
          end
          object IconButton102: TERDButton
            Left = 206
            Top = 1272
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '102 Tekstdocument'
            Icon.BuiltIn = ei102Tekstdocument
            OnClick = IconClicked
          end
          object IconButton103: TERDButton
            Left = 400
            Top = 1272
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '103 Presentatie'
            Icon.BuiltIn = ei103Presentatie
            OnClick = IconClicked
          end
          object IconButton104: TERDButton
            Left = 594
            Top = 1272
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '104 Document controleren'
            Icon.BuiltIn = ei104Documentcontroleren
            OnClick = IconClicked
          end
          object IconButton105: TERDButton
            Left = 788
            Top = 1272
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '105 Document vergelijken'
            Icon.BuiltIn = ei105Documentvergelijken
            OnClick = IconClicked
          end
          object IconButton106: TERDButton
            Left = 12
            Top = 1330
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '106 Staafgrafiek'
            Icon.BuiltIn = ei106Staafgrafiek
            OnClick = IconClicked
          end
          object IconButton107: TERDButton
            Left = 206
            Top = 1330
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '107 Lijngrafiek'
            Icon.BuiltIn = ei107Lijngrafiek
            OnClick = IconClicked
          end
          object IconButton108: TERDButton
            Left = 400
            Top = 1330
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '108 Cirkeldiagram'
            Icon.BuiltIn = ei108Cirkeldiagram
            OnClick = IconClicked
          end
          object IconButton109: TERDButton
            Left = 594
            Top = 1330
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '109 Vlakgrafiek'
            Icon.BuiltIn = ei109Vlakgrafiek
            OnClick = IconClicked
          end
          object IconButton110: TERDButton
            Left = 788
            Top = 1330
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '110 Spreidingsgrafiek'
            Icon.BuiltIn = ei110Spreidingsgrafiek
            OnClick = IconClicked
          end
          object IconButton111: TERDButton
            Left = 12
            Top = 1388
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '111 Histogram'
            Icon.BuiltIn = ei111Histogram
            OnClick = IconClicked
          end
          object IconButton112: TERDButton
            Left = 206
            Top = 1388
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '112 Dashboard'
            Icon.BuiltIn = ei112Dashboard
            OnClick = IconClicked
          end
          object IconButton113: TERDButton
            Left = 400
            Top = 1388
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '113 Formule'
            Icon.BuiltIn = ei113Formule
            OnClick = IconClicked
          end
          object IconButton114: TERDButton
            Left = 594
            Top = 1388
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '114 Berekenen'
            Icon.BuiltIn = ei114Berekenen
            OnClick = IconClicked
          end
          object IconButton115: TERDButton
            Left = 788
            Top = 1388
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '115 Statistieken'
            Icon.BuiltIn = ei115Statistieken
            OnClick = IconClicked
          end
          object IconButton116: TERDButton
            Left = 12
            Top = 1446
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '116 Tag'
            Icon.BuiltIn = ei116Tag
            OnClick = IconClicked
          end
          object IconButton117: TERDButton
            Left = 206
            Top = 1446
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '117 Tags beheren'
            Icon.BuiltIn = ei117Tagsbeheren
            OnClick = IconClicked
          end
          object IconButton118: TERDButton
            Left = 400
            Top = 1446
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '118 Link toevoegen'
            Icon.BuiltIn = ei118Linktoevoegen
            OnClick = IconClicked
          end
          object IconButton119: TERDButton
            Left = 594
            Top = 1446
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '119 Link verwijderen'
            Icon.BuiltIn = ei119Linkverwijderen
            OnClick = IconClicked
          end
          object IconButton120: TERDButton
            Left = 788
            Top = 1446
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '120 Kleurenpalet'
            Icon.BuiltIn = ei120Kleurenpalet
            OnClick = IconClicked
          end
          object IconButton121: TERDButton
            Left = 12
            Top = 1504
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '121 Aanmelden'
            Icon.BuiltIn = ei121Aanmelden
            OnClick = IconClicked
          end
          object IconButton122: TERDButton
            Left = 206
            Top = 1504
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '122 Afmelden'
            Icon.BuiltIn = ei122Afmelden
            OnClick = IconClicked
          end
          object IconButton123: TERDButton
            Left = 400
            Top = 1504
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '123 Profiel bewerken'
            Icon.BuiltIn = ei123Profielbewerken
            OnClick = IconClicked
          end
          object IconButton124: TERDButton
            Left = 594
            Top = 1504
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '124 Gebruiker toevoegen'
            Icon.BuiltIn = ei124Gebruikertoevoegen
            OnClick = IconClicked
          end
          object IconButton125: TERDButton
            Left = 788
            Top = 1504
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '125 Gebruiker verwijderen'
            Icon.BuiltIn = ei125Gebruikerverwijderen
            OnClick = IconClicked
          end
          object IconButton126: TERDButton
            Left = 12
            Top = 1562
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '126 Rollen'
            Icon.BuiltIn = ei126Rollen
            OnClick = IconClicked
          end
          object IconButton127: TERDButton
            Left = 206
            Top = 1562
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '127 Rechten'
            Icon.BuiltIn = ei127Rechten
            OnClick = IconClicked
          end
          object IconButton128: TERDButton
            Left = 400
            Top = 1562
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '128 Wachtwoord'
            Icon.BuiltIn = ei128Wachtwoord
            OnClick = IconClicked
          end
          object IconButton129: TERDButton
            Left = 594
            Top = 1562
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '129 Sleutel'
            Icon.BuiltIn = ei129Sleutel
            OnClick = IconClicked
          end
          object IconButton130: TERDButton
            Left = 788
            Top = 1562
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '130 Certificaat'
            Icon.BuiltIn = ei130Certificaat
            OnClick = IconClicked
          end
          object IconButton131: TERDButton
            Left = 12
            Top = 1620
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '131 Certificaat geldig'
            Icon.BuiltIn = ei131Certificaatgeldig
            OnClick = IconClicked
          end
          object IconButton132: TERDButton
            Left = 206
            Top = 1620
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '132 Certificaat verlopen'
            Icon.BuiltIn = ei132Certificaatverlopen
            OnClick = IconClicked
          end
          object IconButton133: TERDButton
            Left = 400
            Top = 1620
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '133 Bescherming'
            Icon.BuiltIn = ei133Bescherming
            OnClick = IconClicked
          end
          object IconButton134: TERDButton
            Left = 594
            Top = 1620
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '134 Privacy'
            Icon.BuiltIn = ei134Privacy
            OnClick = IconClicked
          end
          object IconButton135: TERDButton
            Left = 788
            Top = 1620
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '135 Versleutelen'
            Icon.BuiltIn = ei135Versleutelen
            OnClick = IconClicked
          end
          object IconButton136: TERDButton
            Left = 12
            Top = 1678
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '136 Ontsleutelen'
            Icon.BuiltIn = ei136Ontsleutelen
            OnClick = IconClicked
          end
          object IconButton137: TERDButton
            Left = 206
            Top = 1678
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '137 Firewall'
            Icon.BuiltIn = ei137Firewall
            OnClick = IconClicked
          end
          object IconButton138: TERDButton
            Left = 400
            Top = 1678
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '138 Proxy'
            Icon.BuiltIn = ei138Proxy
            OnClick = IconClicked
          end
          object IconButton139: TERDButton
            Left = 594
            Top = 1678
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '139 Ethernet'
            Icon.BuiltIn = ei139Ethernet
            OnClick = IconClicked
          end
          object IconButton140: TERDButton
            Left = 788
            Top = 1678
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '140 Wi-Fi'
            Icon.BuiltIn = ei140WiFi
            OnClick = IconClicked
          end
          object IconButton141: TERDButton
            Left = 12
            Top = 1736
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '141 Bluetooth'
            Icon.BuiltIn = ei141Bluetooth
            OnClick = IconClicked
          end
          object IconButton142: TERDButton
            Left = 206
            Top = 1736
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '142 USB'
            Icon.BuiltIn = ei142USB
            OnClick = IconClicked
          end
          object IconButton143: TERDButton
            Left = 400
            Top = 1736
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '143 Cloud'
            Icon.BuiltIn = ei143Cloud
            OnClick = IconClicked
          end
          object IconButton144: TERDButton
            Left = 594
            Top = 1736
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '144 Cloud downloaden'
            Icon.BuiltIn = ei144Clouddownloaden
            OnClick = IconClicked
          end
          object IconButton145: TERDButton
            Left = 788
            Top = 1736
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '145 Cloud uploaden'
            Icon.BuiltIn = ei145Clouduploaden
            OnClick = IconClicked
          end
          object IconButton146: TERDButton
            Left = 12
            Top = 1794
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '146 Online'
            Icon.BuiltIn = ei146Online
            OnClick = IconClicked
          end
          object IconButton147: TERDButton
            Left = 206
            Top = 1794
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '147 Offline'
            Icon.BuiltIn = ei147Offline
            OnClick = IconClicked
          end
          object IconButton148: TERDButton
            Left = 400
            Top = 1794
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '148 Verbinding verbroken'
            Icon.BuiltIn = ei148Verbindingverbroken
            OnClick = IconClicked
          end
          object IconButton149: TERDButton
            Left = 594
            Top = 1794
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '149 Synchroniseren'
            Icon.BuiltIn = ei149Synchroniseren
            OnClick = IconClicked
          end
          object IconButton150: TERDButton
            Left = 788
            Top = 1794
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '150 Sync conflict'
            Icon.BuiltIn = ei150Syncconflict
            OnClick = IconClicked
          end
          object IconButton151: TERDButton
            Left = 12
            Top = 1852
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '151 Takenlijst'
            Icon.BuiltIn = ei151Takenlijst
            OnClick = IconClicked
          end
          object IconButton152: TERDButton
            Left = 206
            Top = 1852
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '152 Taak toevoegen'
            Icon.BuiltIn = ei152Taaktoevoegen
            OnClick = IconClicked
          end
          object IconButton153: TERDButton
            Left = 400
            Top = 1852
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '153 Taak voltooid'
            Icon.BuiltIn = ei153Taakvoltooid
            OnClick = IconClicked
          end
          object IconButton154: TERDButton
            Left = 594
            Top = 1852
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '154 Taak gepland'
            Icon.BuiltIn = ei154Taakgepland
            OnClick = IconClicked
          end
          object IconButton155: TERDButton
            Left = 788
            Top = 1852
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '155 Kalender'
            Icon.BuiltIn = ei155Kalender
            OnClick = IconClicked
          end
          object IconButton156: TERDButton
            Left = 12
            Top = 1910
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '156 Klok'
            Icon.BuiltIn = ei156Klok
            OnClick = IconClicked
          end
          object IconButton157: TERDButton
            Left = 206
            Top = 1910
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '157 Timer'
            Icon.BuiltIn = ei157Timer
            OnClick = IconClicked
          end
          object IconButton158: TERDButton
            Left = 400
            Top = 1910
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '158 Stopwatch'
            Icon.BuiltIn = ei158Stopwatch
            OnClick = IconClicked
          end
          object IconButton159: TERDButton
            Left = 594
            Top = 1910
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '159 Alarm'
            Icon.BuiltIn = ei159Alarm
            OnClick = IconClicked
          end
          object IconButton160: TERDButton
            Left = 788
            Top = 1910
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '160 Meldingen uit'
            Icon.BuiltIn = ei160Meldingenuit
            OnClick = IconClicked
          end
          object IconButton161: TERDButton
            Left = 12
            Top = 1968
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '161 Afspelen'
            Icon.BuiltIn = ei161Afspelen
            OnClick = IconClicked
          end
          object IconButton162: TERDButton
            Left = 206
            Top = 1968
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '162 Pauzeren'
            Icon.BuiltIn = ei162Pauzeren
            OnClick = IconClicked
          end
          object IconButton163: TERDButton
            Left = 400
            Top = 1968
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '163 Stoppen'
            Icon.BuiltIn = ei163Stoppen
            OnClick = IconClicked
          end
          object IconButton164: TERDButton
            Left = 594
            Top = 1968
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '164 Volgende track'
            Icon.BuiltIn = ei164Volgendetrack
            OnClick = IconClicked
          end
          object IconButton165: TERDButton
            Left = 788
            Top = 1968
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '165 Vorige track'
            Icon.BuiltIn = ei165Vorigetrack
            OnClick = IconClicked
          end
          object IconButton166: TERDButton
            Left = 12
            Top = 2026
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '166 Vooruitspoelen'
            Icon.BuiltIn = ei166Vooruitspoelen
            OnClick = IconClicked
          end
          object IconButton167: TERDButton
            Left = 206
            Top = 2026
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '167 Terugspoelen'
            Icon.BuiltIn = ei167Terugspoelen
            OnClick = IconClicked
          end
          object IconButton168: TERDButton
            Left = 400
            Top = 2026
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '168 Opnemen'
            Icon.BuiltIn = ei168Opnemen
            OnClick = IconClicked
          end
          object IconButton169: TERDButton
            Left = 594
            Top = 2026
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '169 Uitwerpen'
            Icon.BuiltIn = ei169Uitwerpen
            OnClick = IconClicked
          end
          object IconButton170: TERDButton
            Left = 788
            Top = 2026
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '170 Dempen'
            Icon.BuiltIn = ei170Dempen
            OnClick = IconClicked
          end
          object IconButton171: TERDButton
            Left = 12
            Top = 2084
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '171 Volume lager'
            Icon.BuiltIn = ei171Volumelager
            OnClick = IconClicked
          end
          object IconButton172: TERDButton
            Left = 206
            Top = 2084
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '172 Volume hoger'
            Icon.BuiltIn = ei172Volumehoger
            OnClick = IconClicked
          end
          object IconButton173: TERDButton
            Left = 400
            Top = 2084
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '173 Herhaal één'
            Icon.BuiltIn = ei173Herhaaleen
            OnClick = IconClicked
          end
          object IconButton174: TERDButton
            Left = 594
            Top = 2084
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '174 Herhaal alles'
            Icon.BuiltIn = ei174Herhaalalles
            OnClick = IconClicked
          end
          object IconButton175: TERDButton
            Left = 788
            Top = 2084
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '175 Herhalen uit'
            Icon.BuiltIn = ei175Herhalenuit
            OnClick = IconClicked
          end
          object IconButton176: TERDButton
            Left = 12
            Top = 2142
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '176 Shuffle aan'
            Icon.BuiltIn = ei176Shuffleaan
            OnClick = IconClicked
          end
          object IconButton177: TERDButton
            Left = 206
            Top = 2142
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '177 Shuffle uit'
            Icon.BuiltIn = ei177Shuffleuit
            OnClick = IconClicked
          end
          object IconButton178: TERDButton
            Left = 400
            Top = 2142
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '178 Mediabibliotheek'
            Icon.BuiltIn = ei178Mediabibliotheek
            OnClick = IconClicked
          end
          object IconButton179: TERDButton
            Left = 594
            Top = 2142
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '179 Album'
            Icon.BuiltIn = ei179Album
            OnClick = IconClicked
          end
          object IconButton180: TERDButton
            Left = 788
            Top = 2142
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '180 Artiest'
            Icon.BuiltIn = ei180Artiest
            OnClick = IconClicked
          end
          object IconButton181: TERDButton
            Left = 12
            Top = 2200
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '181 Muziektrack'
            Icon.BuiltIn = ei181Muziektrack
            OnClick = IconClicked
          end
          object IconButton182: TERDButton
            Left = 206
            Top = 2200
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '182 Audiobestand'
            Icon.BuiltIn = ei182Audiobestand
            OnClick = IconClicked
          end
          object IconButton183: TERDButton
            Left = 400
            Top = 2200
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '183 Videobestand'
            Icon.BuiltIn = ei183Videobestand
            OnClick = IconClicked
          end
          object IconButton184: TERDButton
            Left = 594
            Top = 2200
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '184 Afbeelding'
            Icon.BuiltIn = ei184Afbeelding
            OnClick = IconClicked
          end
          object IconButton185: TERDButton
            Left = 788
            Top = 2200
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '185 Muzieknoot'
            Icon.BuiltIn = ei185Muzieknoot
            OnClick = IconClicked
          end
          object IconButton186: TERDButton
            Left = 12
            Top = 2258
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '186 Film'
            Icon.BuiltIn = ei186Film
            OnClick = IconClicked
          end
          object IconButton187: TERDButton
            Left = 206
            Top = 2258
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '187 Camera'
            Icon.BuiltIn = ei187Camera
            OnClick = IconClicked
          end
          object IconButton188: TERDButton
            Left = 400
            Top = 2258
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '188 Equalizer'
            Icon.BuiltIn = ei188Equalizer
            OnClick = IconClicked
          end
          object IconButton189: TERDButton
            Left = 594
            Top = 2258
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '189 Audio-uitgang'
            Icon.BuiltIn = ei189Audiouitgang
            OnClick = IconClicked
          end
          object IconButton190: TERDButton
            Left = 788
            Top = 2258
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '190 Hoofdtelefoon'
            Icon.BuiltIn = ei190Hoofdtelefoon
            OnClick = IconClicked
          end
          object IconButton191: TERDButton
            Left = 12
            Top = 2316
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '191 Luidspreker'
            Icon.BuiltIn = ei191Luidspreker
            OnClick = IconClicked
          end
          object IconButton192: TERDButton
            Left = 206
            Top = 2316
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '192 Ondertiteling'
            Icon.BuiltIn = ei192Ondertiteling
            OnClick = IconClicked
          end
          object IconButton193: TERDButton
            Left = 400
            Top = 2316
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '193 Ondertiteling uit'
            Icon.BuiltIn = ei193Ondertitelinguit
            OnClick = IconClicked
          end
          object IconButton194: TERDButton
            Left = 594
            Top = 2316
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '194 Audiotrack kiezen'
            Icon.BuiltIn = ei194Audiotrackkiezen
            OnClick = IconClicked
          end
          object IconButton195: TERDButton
            Left = 788
            Top = 2316
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '195 Videokwaliteit'
            Icon.BuiltIn = ei195Videokwaliteit
            OnClick = IconClicked
          end
          object IconButton196: TERDButton
            Left = 12
            Top = 2374
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '196 Afspeelsnelheid'
            Icon.BuiltIn = ei196Afspeelsnelheid
            OnClick = IconClicked
          end
          object IconButton197: TERDButton
            Left = 206
            Top = 2374
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '197 Scene markeren'
            Icon.BuiltIn = ei197Scenemarkeren
            OnClick = IconClicked
          end
          object IconButton198: TERDButton
            Left = 400
            Top = 2374
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '198 Picture-in-picture'
            Icon.BuiltIn = ei198Pictureinpicture
            OnClick = IconClicked
          end
          object IconButton199: TERDButton
            Left = 594
            Top = 2374
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '199 Casten'
            Icon.BuiltIn = ei199Casten
            OnClick = IconClicked
          end
          object IconButton200: TERDButton
            Left = 788
            Top = 2374
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '200 Live uitzending'
            Icon.BuiltIn = ei200Liveuitzending
            OnClick = IconClicked
          end
          object IconButton201: TERDButton
            Left = 12
            Top = 2432
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '201 Nieuwe playlist'
            Icon.BuiltIn = ei201Nieuweplaylist
            OnClick = IconClicked
          end
          object IconButton202: TERDButton
            Left = 206
            Top = 2432
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '202 Playlist openen'
            Icon.BuiltIn = ei202Playlistopenen
            OnClick = IconClicked
          end
          object IconButton203: TERDButton
            Left = 400
            Top = 2432
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '203 Playlist opslaan'
            Icon.BuiltIn = ei203Playlistopslaan
            OnClick = IconClicked
          end
          object IconButton204: TERDButton
            Left = 594
            Top = 2432
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '204 Playlist hernoemen'
            Icon.BuiltIn = ei204Playlisthernoemen
            OnClick = IconClicked
          end
          object IconButton205: TERDButton
            Left = 788
            Top = 2432
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '205 Playlist dupliceren'
            Icon.BuiltIn = ei205Playlistdupliceren
            OnClick = IconClicked
          end
          object IconButton206: TERDButton
            Left = 12
            Top = 2490
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '206 Playlists samenvoegen'
            Icon.BuiltIn = ei206Playlistssamenvoegen
            OnClick = IconClicked
          end
          object IconButton207: TERDButton
            Left = 206
            Top = 2490
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '207 Playlist splitsen'
            Icon.BuiltIn = ei207Playlistsplitsen
            OnClick = IconClicked
          end
          object IconButton208: TERDButton
            Left = 400
            Top = 2490
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '208 Track toevoegen'
            Icon.BuiltIn = ei208Tracktoevoegen
            OnClick = IconClicked
          end
          object IconButton209: TERDButton
            Left = 594
            Top = 2490
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '209 Track verwijderen'
            Icon.BuiltIn = ei209Trackverwijderen
            OnClick = IconClicked
          end
          object IconButton210: TERDButton
            Left = 788
            Top = 2490
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '210 Playlist sorteren'
            Icon.BuiltIn = ei210Playlistsorteren
            OnClick = IconClicked
          end
          object IconButton211: TERDButton
            Left = 12
            Top = 2548
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '211 Track omhoog'
            Icon.BuiltIn = ei211Trackomhoog
            OnClick = IconClicked
          end
          object IconButton212: TERDButton
            Left = 206
            Top = 2548
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '212 Track omlaag'
            Icon.BuiltIn = ei212Trackomlaag
            OnClick = IconClicked
          end
          object IconButton213: TERDButton
            Left = 400
            Top = 2548
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '213 Queue'
            Icon.BuiltIn = ei213Queue
            OnClick = IconClicked
          end
          object IconButton214: TERDButton
            Left = 594
            Top = 2548
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '214 Aan queue toevoegen'
            Icon.BuiltIn = ei214Aanqueuetoevoegen
            OnClick = IconClicked
          end
          object IconButton215: TERDButton
            Left = 788
            Top = 2548
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '215 Uit queue verwijderen'
            Icon.BuiltIn = ei215Uitqueueverwijderen
            OnClick = IconClicked
          end
          object IconButton216: TERDButton
            Left = 12
            Top = 2606
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '216 Queue leegmaken'
            Icon.BuiltIn = ei216Queueleegmaken
            OnClick = IconClicked
          end
          object IconButton217: TERDButton
            Left = 206
            Top = 2606
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '217 Stream URL'
            Icon.BuiltIn = ei217StreamURL
            OnClick = IconClicked
          end
          object IconButton218: TERDButton
            Left = 400
            Top = 2606
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '218 Stream testen'
            Icon.BuiltIn = ei218Streamtesten
            OnClick = IconClicked
          end
          object IconButton219: TERDButton
            Left = 594
            Top = 2606
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '219 Stream actief'
            Icon.BuiltIn = ei219Streamactief
            OnClick = IconClicked
          end
          object IconButton220: TERDButton
            Left = 788
            Top = 2606
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '220 Stream offline'
            Icon.BuiltIn = ei220Streamoffline
            OnClick = IconClicked
          end
          object IconButton221: TERDButton
            Left = 12
            Top = 2664
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '221 TV-kanaal'
            Icon.BuiltIn = ei221TVkanaal
            OnClick = IconClicked
          end
          object IconButton222: TERDButton
            Left = 206
            Top = 2664
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '222 Radiozender'
            Icon.BuiltIn = ei222Radiozender
            OnClick = IconClicked
          end
          object IconButton223: TERDButton
            Left = 400
            Top = 2664
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '223 Programmagids'
            Icon.BuiltIn = ei223Programmagids
            OnClick = IconClicked
          end
          object IconButton224: TERDButton
            Left = 594
            Top = 2664
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '224 Favoriet toevoegen'
            Icon.BuiltIn = ei224Favoriettoevoegen
            OnClick = IconClicked
          end
          object IconButton225: TERDButton
            Left = 788
            Top = 2664
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '225 Favoriet verwijderen'
            Icon.BuiltIn = ei225Favorietverwijderen
            OnClick = IconClicked
          end
          object IconButton226: TERDButton
            Left = 12
            Top = 2722
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '226 Download starten'
            Icon.BuiltIn = ei226Downloadstarten
            OnClick = IconClicked
          end
          object IconButton227: TERDButton
            Left = 206
            Top = 2722
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '227 Download pauzeren'
            Icon.BuiltIn = ei227Downloadpauzeren
            OnClick = IconClicked
          end
          object IconButton228: TERDButton
            Left = 400
            Top = 2722
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '228 Download hervatten'
            Icon.BuiltIn = ei228Downloadhervatten
            OnClick = IconClicked
          end
          object IconButton229: TERDButton
            Left = 594
            Top = 2722
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '229 Download annuleren'
            Icon.BuiltIn = ei229Downloadannuleren
            OnClick = IconClicked
          end
          object IconButton230: TERDButton
            Left = 788
            Top = 2722
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '230 Download opnieuw'
            Icon.BuiltIn = ei230Downloadopnieuw
            OnClick = IconClicked
          end
          object IconButton231: TERDButton
            Left = 12
            Top = 2780
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '231 Download wachtend'
            Icon.BuiltIn = ei231Downloadwachtend
            OnClick = IconClicked
          end
          object IconButton232: TERDButton
            Left = 206
            Top = 2780
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '232 Download actief'
            Icon.BuiltIn = ei232Downloadactief
            OnClick = IconClicked
          end
          object IconButton233: TERDButton
            Left = 400
            Top = 2780
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '233 Download klaar'
            Icon.BuiltIn = ei233Downloadklaar
            OnClick = IconClicked
          end
          object IconButton234: TERDButton
            Left = 594
            Top = 2780
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '234 Download mislukt'
            Icon.BuiltIn = ei234Downloadmislukt
            OnClick = IconClicked
          end
          object IconButton235: TERDButton
            Left = 788
            Top = 2780
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '235 Downloadinstellingen'
            Icon.BuiltIn = ei235Downloadinstellingen
            OnClick = IconClicked
          end
          object IconButton236: TERDButton
            Left = 12
            Top = 2838
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '236 Bandbreedtelimiet'
            Icon.BuiltIn = ei236Bandbreedtelimiet
            OnClick = IconClicked
          end
          object IconButton237: TERDButton
            Left = 206
            Top = 2838
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '237 Downloadmap'
            Icon.BuiltIn = ei237Downloadmap
            OnClick = IconClicked
          end
          object IconButton238: TERDButton
            Left = 400
            Top = 2838
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '238 Voltooide wissen'
            Icon.BuiltIn = ei238Voltooidewissen
            OnClick = IconClicked
          end
          object IconButton239: TERDButton
            Left = 594
            Top = 2838
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '239 Batch downloaden'
            Icon.BuiltIn = ei239Batchdownloaden
            OnClick = IconClicked
          end
          object IconButton240: TERDButton
            Left = 788
            Top = 2838
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '240 Downloadgeschiedenis'
            Icon.BuiltIn = ei240Downloadgeschiedenis
            OnClick = IconClicked
          end
          object IconButton241: TERDButton
            Left = 12
            Top = 2896
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '241 VOD bibliotheek'
            Icon.BuiltIn = ei241VODbibliotheek
            OnClick = IconClicked
          end
          object IconButton242: TERDButton
            Left = 206
            Top = 2896
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '242 Film details'
            Icon.BuiltIn = ei242Filmdetails
            OnClick = IconClicked
          end
          object IconButton243: TERDButton
            Left = 400
            Top = 2896
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '243 Series'
            Icon.BuiltIn = ei243Series
            OnClick = IconClicked
          end
          object IconButton244: TERDButton
            Left = 594
            Top = 2896
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '244 Seizoenen'
            Icon.BuiltIn = ei244Seizoenen
            OnClick = IconClicked
          end
          object IconButton245: TERDButton
            Left = 788
            Top = 2896
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '245 Afleveringen'
            Icon.BuiltIn = ei245Afleveringen
            OnClick = IconClicked
          end
          object IconButton246: TERDButton
            Left = 12
            Top = 2954
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '246 Verder kijken'
            Icon.BuiltIn = ei246Verderkijken
            OnClick = IconClicked
          end
          object IconButton247: TERDButton
            Left = 206
            Top = 2954
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '247 Laatst bekeken'
            Icon.BuiltIn = ei247Laatstbekeken
            OnClick = IconClicked
          end
          object IconButton248: TERDButton
            Left = 400
            Top = 2954
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '248 Kijklijst'
            Icon.BuiltIn = ei248Kijklijst
            OnClick = IconClicked
          end
          object IconButton249: TERDButton
            Left = 594
            Top = 2954
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '249 Teletext'
            Icon.BuiltIn = ei249Teletext
            OnClick = IconClicked
          end
          object IconButton250: TERDButton
            Left = 788
            Top = 2954
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '250 Teletextpagina'
            Icon.BuiltIn = ei250Teletextpagina
            OnClick = IconClicked
          end
          object IconButton251: TERDButton
            Left = 12
            Top = 3012
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '251 Teletext volgende'
            Icon.BuiltIn = ei251Teletextvolgende
            OnClick = IconClicked
          end
          object IconButton252: TERDButton
            Left = 206
            Top = 3012
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '252 Teletext vorige'
            Icon.BuiltIn = ei252Teletextvorige
            OnClick = IconClicked
          end
          object IconButton253: TERDButton
            Left = 400
            Top = 3012
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '253 Teletext vergroten'
            Icon.BuiltIn = ei253Teletextvergroten
            OnClick = IconClicked
          end
          object IconButton254: TERDButton
            Left = 594
            Top = 3012
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '254 Teletext transparant'
            Icon.BuiltIn = ei254Teletexttransparant
            OnClick = IconClicked
          end
          object IconButton255: TERDButton
            Left = 788
            Top = 3012
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '255 Ondertiteltaal'
            Icon.BuiltIn = ei255Ondertiteltaal
            OnClick = IconClicked
          end
          object IconButton256: TERDButton
            Left = 12
            Top = 3070
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '256 Ondertitelbestand'
            Icon.BuiltIn = ei256Ondertitelbestand
            OnClick = IconClicked
          end
          object IconButton257: TERDButton
            Left = 206
            Top = 3070
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '257 Ondertitels synchroniseren'
            Icon.BuiltIn = ei257Ondertitelssynchroniseren
            OnClick = IconClicked
          end
          object IconButton258: TERDButton
            Left = 400
            Top = 3070
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '258 Ondertitels vroeger'
            Icon.BuiltIn = ei258Ondertitelsvroeger
            OnClick = IconClicked
          end
          object IconButton259: TERDButton
            Left = 594
            Top = 3070
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '259 Ondertitels later'
            Icon.BuiltIn = ei259Ondertitelslater
            OnClick = IconClicked
          end
          object IconButton260: TERDButton
            Left = 788
            Top = 3070
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '260 Audiotaal'
            Icon.BuiltIn = ei260Audiotaal
            OnClick = IconClicked
          end
          object IconButton261: TERDButton
            Left = 12
            Top = 3128
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '261 Stereo'
            Icon.BuiltIn = ei261Stereo
            OnClick = IconClicked
          end
          object IconButton262: TERDButton
            Left = 206
            Top = 3128
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '262 Mono'
            Icon.BuiltIn = ei262Mono
            OnClick = IconClicked
          end
          object IconButton263: TERDButton
            Left = 400
            Top = 3128
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '263 Surround'
            Icon.BuiltIn = ei263Surround
            OnClick = IconClicked
          end
          object IconButton264: TERDButton
            Left = 594
            Top = 3128
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '264 Audiobalans'
            Icon.BuiltIn = ei264Audiobalans
            OnClick = IconClicked
          end
          object IconButton265: TERDButton
            Left = 788
            Top = 3128
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '265 Equalizer presets'
            Icon.BuiltIn = ei265Equalizerpresets
            OnClick = IconClicked
          end
          object IconButton266: TERDButton
            Left = 12
            Top = 3186
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '266 Bas'
            Icon.BuiltIn = ei266Bas
            OnClick = IconClicked
          end
          object IconButton267: TERDButton
            Left = 206
            Top = 3186
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '267 Hoge tonen'
            Icon.BuiltIn = ei267Hogetonen
            OnClick = IconClicked
          end
          object IconButton268: TERDButton
            Left = 400
            Top = 3186
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '268 Programmadetails'
            Icon.BuiltIn = ei268Programmadetails
            OnClick = IconClicked
          end
          object IconButton269: TERDButton
            Left = 594
            Top = 3186
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '269 Volgend programma'
            Icon.BuiltIn = ei269Volgendprogramma
            OnClick = IconClicked
          end
          object IconButton270: TERDButton
            Left = 788
            Top = 3186
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '270 Opname plannen'
            Icon.BuiltIn = ei270Opnameplannen
            OnClick = IconClicked
          end
          object IconButton271: TERDButton
            Left = 12
            Top = 3244
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '271 Opnamen'
            Icon.BuiltIn = ei271Opnamen
            OnClick = IconClicked
          end
          object IconButton272: TERDButton
            Left = 206
            Top = 3244
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '272 Timeshift'
            Icon.BuiltIn = ei272Timeshift
            OnClick = IconClicked
          end
          object IconButton273: TERDButton
            Left = 400
            Top = 3244
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '273 Timeshift pauze'
            Icon.BuiltIn = ei273Timeshiftpauze
            OnClick = IconClicked
          end
          object IconButton274: TERDButton
            Left = 594
            Top = 3244
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '274 Naar live'
            Icon.BuiltIn = ei274Naarlive
            OnClick = IconClicked
          end
          object IconButton275: TERDButton
            Left = 788
            Top = 3244
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '275 Catch-up TV'
            Icon.BuiltIn = ei275CatchupTV
            OnClick = IconClicked
          end
          object IconButton276: TERDButton
            Left = 12
            Top = 3302
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '276 Vanaf begin'
            Icon.BuiltIn = ei276Vanafbegin
            OnClick = IconClicked
          end
          object IconButton277: TERDButton
            Left = 206
            Top = 3302
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '277 Kanaalgroepen'
            Icon.BuiltIn = ei277Kanaalgroepen
            OnClick = IconClicked
          end
          object IconButton278: TERDButton
            Left = 400
            Top = 3302
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '278 Kanaalnummer'
            Icon.BuiltIn = ei278Kanaalnummer
            OnClick = IconClicked
          end
          object IconButton279: TERDButton
            Left = 594
            Top = 3302
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '279 Kanaallogo'
            Icon.BuiltIn = ei279Kanaallogo
            OnClick = IconClicked
          end
          object IconButton280: TERDButton
            Left = 788
            Top = 3302
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '280 M3U playlist'
            Icon.BuiltIn = ei280M3Uplaylist
            OnClick = IconClicked
          end
          object IconButton281: TERDButton
            Left = 12
            Top = 3360
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '281 Playlist naar cloud'
            Icon.BuiltIn = ei281Playlistnaarcloud
            OnClick = IconClicked
          end
          object IconButton282: TERDButton
            Left = 206
            Top = 3360
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '282 Playlist uit cloud'
            Icon.BuiltIn = ei282Playlistuitcloud
            OnClick = IconClicked
          end
          object IconButton283: TERDButton
            Left = 400
            Top = 3360
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '283 Cloudplaylist delen'
            Icon.BuiltIn = ei283Cloudplaylistdelen
            OnClick = IconClicked
          end
          object IconButton284: TERDButton
            Left = 594
            Top = 3360
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '284 Cloudplaylist sync'
            Icon.BuiltIn = ei284Cloudplaylistsync
            OnClick = IconClicked
          end
          object IconButton285: TERDButton
            Left = 788
            Top = 3360
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '285 Xtream Codes'
            Icon.BuiltIn = ei285XtreamCodes
            OnClick = IconClicked
          end
          object IconButton286: TERDButton
            Left = 12
            Top = 3418
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '286 Xtream server'
            Icon.BuiltIn = ei286Xtreamserver
            OnClick = IconClicked
          end
          object IconButton287: TERDButton
            Left = 206
            Top = 3418
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '287 Xtream aanmelden'
            Icon.BuiltIn = ei287Xtreamaanmelden
            OnClick = IconClicked
          end
          object IconButton288: TERDButton
            Left = 400
            Top = 3418
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '288 Xtream verversen'
            Icon.BuiltIn = ei288Xtreamverversen
            OnClick = IconClicked
          end
          object IconButton289: TERDButton
            Left = 594
            Top = 3418
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '289 M3U importeren'
            Icon.BuiltIn = ei289M3Uimporteren
            OnClick = IconClicked
          end
          object IconButton290: TERDButton
            Left = 788
            Top = 3418
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '290 M3U exporteren'
            Icon.BuiltIn = ei290M3Uexporteren
            OnClick = IconClicked
          end
          object IconButton291: TERDButton
            Left = 12
            Top = 3476
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '291 M3U8 bestand'
            Icon.BuiltIn = ei291M3U8bestand
            OnClick = IconClicked
          end
          object IconButton292: TERDButton
            Left = 206
            Top = 3476
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '292 PLS bestand'
            Icon.BuiltIn = ei292PLSbestand
            OnClick = IconClicked
          end
          object IconButton293: TERDButton
            Left = 400
            Top = 3476
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '293 XSPF bestand'
            Icon.BuiltIn = ei293XSPFbestand
            OnClick = IconClicked
          end
          object IconButton294: TERDButton
            Left = 594
            Top = 3476
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '294 ASX bestand'
            Icon.BuiltIn = ei294ASXbestand
            OnClick = IconClicked
          end
          object IconButton295: TERDButton
            Left = 788
            Top = 3476
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '295 JSON playlist'
            Icon.BuiltIn = ei295JSONplaylist
            OnClick = IconClicked
          end
          object IconButton296: TERDButton
            Left = 12
            Top = 3534
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '296 Playlist converteren'
            Icon.BuiltIn = ei296Playlistconverteren
            OnClick = IconClicked
          end
          object IconButton297: TERDButton
            Left = 206
            Top = 3534
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '297 Playlist valideren'
            Icon.BuiltIn = ei297Playlistvalideren
            OnClick = IconClicked
          end
          object IconButton298: TERDButton
            Left = 400
            Top = 3534
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '298 Playlist repareren'
            Icon.BuiltIn = ei298Playlistrepareren
            OnClick = IconClicked
          end
          object IconButton299: TERDButton
            Left = 594
            Top = 3534
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '299 Dubbele kanalen'
            Icon.BuiltIn = ei299Dubbelekanalen
            OnClick = IconClicked
          end
          object IconButton300: TERDButton
            Left = 788
            Top = 3534
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '300 Duplicaten verwijderen'
            Icon.BuiltIn = ei300Duplicatenverwijderen
            OnClick = IconClicked
          end
          object IconButton301: TERDButton
            Left = 12
            Top = 3592
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '301 Kanalen zoeken'
            Icon.BuiltIn = ei301Kanalenzoeken
            OnClick = IconClicked
          end
          object IconButton302: TERDButton
            Left = 206
            Top = 3592
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '302 Playlist filteren'
            Icon.BuiltIn = ei302Playlistfilteren
            OnClick = IconClicked
          end
          object IconButton303: TERDButton
            Left = 400
            Top = 3592
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '303 Zoeken en vervangen'
            Icon.BuiltIn = ei303Zoekenenvervangen
            OnClick = IconClicked
          end
          object IconButton304: TERDButton
            Left = 594
            Top = 3592
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '304 Alles vervangen'
            Icon.BuiltIn = ei304Allesvervangen
            OnClick = IconClicked
          end
          object IconButton305: TERDButton
            Left = 788
            Top = 3592
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '305 HD bovenaan'
            Icon.BuiltIn = ei305HDbovenaan
            OnClick = IconClicked
          end
          object IconButton306: TERDButton
            Left = 12
            Top = 3650
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '306 SD onderaan'
            Icon.BuiltIn = ei306SDonderaan
            OnClick = IconClicked
          end
          object IconButton307: TERDButton
            Left = 206
            Top = 3650
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '307 Resolutie sorteren'
            Icon.BuiltIn = ei307Resolutiesorteren
            OnClick = IconClicked
          end
          object IconButton308: TERDButton
            Left = 400
            Top = 3650
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '308 HD kanaal'
            Icon.BuiltIn = ei308HDkanaal
            OnClick = IconClicked
          end
          object IconButton309: TERDButton
            Left = 594
            Top = 3650
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '309 Full HD kanaal'
            Icon.BuiltIn = ei309FullHDkanaal
            OnClick = IconClicked
          end
          object IconButton310: TERDButton
            Left = 788
            Top = 3650
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '310 4K kanaal'
            Icon.BuiltIn = ei3104Kkanaal
            OnClick = IconClicked
          end
          object IconButton311: TERDButton
            Left = 12
            Top = 3708
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '311 Kanaal verplaatsen'
            Icon.BuiltIn = ei311Kanaalverplaatsen
            OnClick = IconClicked
          end
          object IconButton312: TERDButton
            Left = 206
            Top = 3708
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '312 Groep toevoegen'
            Icon.BuiltIn = ei312Groeptoevoegen
            OnClick = IconClicked
          end
          object IconButton313: TERDButton
            Left = 400
            Top = 3708
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '313 Groep verwijderen'
            Icon.BuiltIn = ei313Groepverwijderen
            OnClick = IconClicked
          end
          object IconButton314: TERDButton
            Left = 594
            Top = 3708
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '314 Groep hernoemen'
            Icon.BuiltIn = ei314Groephernoemen
            OnClick = IconClicked
          end
          object IconButton315: TERDButton
            Left = 788
            Top = 3708
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '315 Groepen sorteren'
            Icon.BuiltIn = ei315Groepensorteren
            OnClick = IconClicked
          end
          object IconButton316: TERDButton
            Left = 12
            Top = 3766
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '316 Kanalen samenvoegen'
            Icon.BuiltIn = ei316Kanalensamenvoegen
            OnClick = IconClicked
          end
          object IconButton317: TERDButton
            Left = 206
            Top = 3766
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '317 Kanaal hernoemen'
            Icon.BuiltIn = ei317Kanaalhernoemen
            OnClick = IconClicked
          end
          object IconButton318: TERDButton
            Left = 400
            Top = 3766
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '318 Kanalen vergelijken'
            Icon.BuiltIn = ei318Kanalenvergelijken
            OnClick = IconClicked
          end
          object IconButton319: TERDButton
            Left = 594
            Top = 3766
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '319 Dode streams vinden'
            Icon.BuiltIn = ei319Dodestreamsvinden
            OnClick = IconClicked
          end
          object IconButton320: TERDButton
            Left = 788
            Top = 3766
            Width = 184
            Height = 48
            AutoSize = False
            Caption = '320 Dode streams verwijderen'
            Icon.BuiltIn = ei320Dodestreamsverwijderen
            OnClick = IconClicked
          end
        end
    end
    object InputsPage: TERDPanel
      Align = alClient
      BorderWidth = 0
      Visible = False
      object InputsScroll: TERDScrollBox
        Align = alClient
        object LabelCheckBox: TLabel
          Left = 16
          Top = 16
          Caption = 'TERDCheckBox'
        end
        object DemoCheckBox: TERDCheckBox
          Align = alNone
          Left = 16
          Top = 42
          Width = 460
          Height = 220
          Caption = 'Enabled option'
        end
        object LabelRadioButton: TLabel
          Left = 506
          Top = 16
          Caption = 'TERDRadioButton'
        end
        object DemoRadioButton: TERDRadioButton
          Align = alNone
          Left = 506
          Top = 42
          Width = 460
          Height = 220
          Caption = 'Radio choice'
        end
        object LabelChip: TLabel
          Left = 16
          Top = 286
          Caption = 'TERDChip'
        end
        object DemoChip: TERDChip
          Align = alNone
          Left = 16
          Top = 312
          Width = 460
          Height = 220
          Caption = 'Status chip'
        end
        object LabelBanner: TLabel
          Left = 506
          Top = 286
          Caption = 'TERDBanner'
        end
        object DemoBanner: TERDBanner
          ShowClose = True
          ActionCaption = 'Details'
          OnAction = ShowDialog
          Align = alNone
          Left = 506
          Top = 312
          Width = 460
          Height = 220
          Text = 'Example notification'
        end
        object LabelEdit: TLabel
          Left = 16
          Top = 556
          Caption = 'TERDEdit'
        end
        object DemoEdit: TERDEdit
          Align = alNone
          Left = 16
          Top = 582
          Width = 460
          Height = 220
          Text = 'Editable text'
        end
        object LabelSegmented: TLabel
          Left = 506
          Top = 556
          Caption = 'TERDSegmented'
        end
        object DemoSegmented: TERDSegmented
          Align = alNone
          Left = 506
          Top = 582
          Width = 460
          Height = 220
          Items.Strings = (
            'One'
            'Two'
            'Three')
        end
        object LabelRangeBar: TLabel
          Left = 16
          Top = 826
          Caption = 'TERDRangeBar'
        end
        object DemoRangeBar: TERDRangeBar
          Align = alNone
          Left = 16
          Top = 852
          Width = 460
          Height = 220
          Value = 45
        end
        object LabelSpinEdit: TLabel
          Left = 506
          Top = 826
          Caption = 'TERDSpinEdit'
        end
        object DemoSpinEdit: TERDSpinEdit
          Align = alNone
          Left = 506
          Top = 852
          Width = 460
          Height = 220
          Value = 12
        end
        object LabelTrackBar: TLabel
          Left = 16
          Top = 1096
          Caption = 'TERDTrackBar'
        end
        object DemoTrackBar: TERDTrackBar
          Align = alNone
          Left = 16
          Top = 1122
          Width = 460
          Height = 220
          Position = 35
        end
        object LabelMemo: TLabel
          Left = 506
          Top = 1096
          Caption = 'TERDMemo'
        end
        object DemoMemo: TERDMemo
          Align = alNone
          Left = 506
          Top = 1122
          Width = 460
          Height = 220
          Lines.Strings = (
            'Editable memo text'
            'Second line')
        end
        object LabelCalendar: TLabel
          Left = 16
          Top = 1366
          Caption = 'TERDCalendar'
        end
        object DemoCalendar: TERDCalendar
          Align = alNone
          Left = 16
          Top = 1392
          Width = 460
          Height = 220
        end
        object LabelDatePicker: TLabel
          Left = 506
          Top = 1366
          Caption = 'TERDDatePicker'
        end
        object DemoDatePicker: TERDDatePicker
          Align = alNone
          Left = 506
          Top = 1392
          Width = 460
          Height = 220
        end
        object LabelListBox: TLabel
          Left = 16
          Top = 1636
          Caption = 'TERDListBox'
        end
        object DemoListBox: TERDListBox
          Align = alNone
          Left = 16
          Top = 1662
          Width = 460
          Height = 220
          Items.Strings = (
            'First item'
            'Second item'
            'Third item')
        end
        object CommonControlsLabel: TLabel
          Left = 16
          Top = 1906
          Caption = 'Search / split button / tags / expander / vertical slider'
        end
        object DemoSearchEdit: TERDSearchEdit
          Left = 16
          Top = 1932
          Width = 300
          TextHint = 'Search channels'
          OnSearch = SearchChannels
        end
        object DemoSplitButton: TERDSplitButton
          Left = 336
          Top = 1932
          Caption = 'Open'
          DropDownMenu = DemoPopupMenu
          OnClick = OpenFile
        end
        object DemoTagEdit: TERDTagEdit
          Left = 16
          Top = 1990
          Width = 460
          Height = 120
          Tags.Strings = (
            'Live TV'
            'HD'
            'Favorites')
        end
        object DemoExpander: TERDExpander
          Left = 506
          Top = 1990
          Width = 360
          Height = 160
          Caption = 'Advanced filters'
          ExpandedHeight = 160
          object ExpanderText: TLabel
            Left = 16
            Top = 54
            Caption = 'Editable content; collapse using the header.'
          end
        end
        object DemoSeekBar: TERDSeekBar
          Left = 16
          Top = 2190
          Width = 460
          Height = 32
          Max = 3600
          Position = 900
          BufferedPosition = 1800
          ShowValue = True
          Suffix = ' s'
        end
        object DemoRangeSlider: TERDRangeSlider
          Left = 506
          Top = 2190
          Width = 460
          Height = 32
          Position = 20
          UpperValue = 80
          ShowValue = True
        end
        object DemoVerticalTrackBar: TERDTrackBar
          Left = 896
          Top = 1932
          Width = 60
          Height = 220
          AutoSize = False
          Orientation = toVertical
          Position = 65
          ShowValue = True
        end
        object LabelCheckListBox: TLabel
          Left = 16
          Top = 2260
          Caption = 'TERDCheckListBox - Space or checkbox to check; caption selects'
        end
        object DemoCheckListBox: TERDCheckListBox
          Left = 16
          Top = 2286
          Width = 460
          Height = 220
          AllowGrayed = True
          OnClickCheck = CheckListChanged
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
        end
        object LabelTimePicker: TLabel
          Left = 16
          Top = 2530
          Caption = 'TERDTimePicker - type / Left-Right / Up-Down; 24h and 12h'
        end
        object DemoTimePicker: TERDTimePicker
          Left = 16
          Top = 2556
          Width = 220
          Time = 0.5625
          ShowSeconds = True
          OnChange = TimeChanged
        end
        object DemoTimePicker12: TERDTimePicker
          Left = 256
          Top = 2556
          Width = 220
          Time = 0.5625
          TimeFormat = tf12Hour
          OnChange = TimeChanged
        end
        object LabelDateTimePicker: TLabel
          Left = 16
          Top = 2620
          Caption = 'TERDDateTimePicker - editable date segments + calendar + time'
        end
        object DemoDateTimePicker: TERDDateTimePicker
          Left = 16
          Top = 2646
          Width = 460
          DateFormat = 'dd/mm/yyyy'
          ShowSeconds = True
          OnChange = DateTimeChanged
        end
        object LabelLinkLabel: TLabel
          Left = 16
          Top = 2710
          Caption = 'TERDLinkLabel - mouse / Enter / Space, visited state and optional icon'
        end
        object DemoLinkLabel: TERDLinkLabel
          Left = 16
          Top = 2736
          Caption = 'ERDesigns website'
          URL = 'https://erdesigns.be'
          Icon.BuiltIn = ei028Info
          OnClick = LinkClicked
        end
        object LabelLoadingIndicator: TLabel
          Left = 16
          Top = 2810
          Caption = 'TERDLoadingIndicator - follows Windows animation settings'
        end
        object DemoLoadingIndicator: TERDLoadingIndicator
          Left = 16
          Top = 2836
          Caption = 'Loading playlists...'
        end
        object LoadingToggle: TERDCheckBox
          Left = 256
          Top = 2836
          Caption = 'Active'
          Style = csSwitch
          Checked = True
          OnChange = LoadingChanged
        end
      end
    end
    object LayoutPage: TERDPanel
      Align = alClient
      BorderWidth = 0
      Visible = False
      object LayoutScroll: TERDScrollBox
        Align = alClient
        object LabelCard: TLabel
          Left = 16
          Top = 16
          Caption = 'TERDCard'
        end
        object DemoCard: TERDCard
          Align = alNone
          Left = 16
          Top = 42
          Width = 460
          Height = 220
          Title = 'Card container'
        end
        object LabelMenuBar: TLabel
          Left = 506
          Top = 16
          Caption = 'TERDMenuBar'
        end
        object DemoMenuBar: TERDMenuBar
          Align = alNone
          Left = 506
          Top = 42
          Width = 460
          Height = 220
          Menu = MainMenu
        end
        object LabelBackstage: TLabel
          Left = 16
          Top = 286
          Caption = 'TERDBackstage'
        end
        object OpenBackstageButton: TERDButton
          Left = 16
          Top = 312
          Width = 280
          Height = 44
          AutoSize = False
          Caption = 'Open backstage (File)'
          Icon.BuiltIn = ei005Bestand
          OnClick = ShowBackstage
        end
        object LabelTabs: TLabel
          Left = 506
          Top = 286
          Caption = 'TERDTabs'
        end
        object DemoTabs: TERDTabs
          Align = alNone
          Left = 506
          Top = 312
          Width = 460
          Height = 220
          Tabs = <
            item
              Caption = 'First tab'
            end
            item
              Caption = 'Second tab'
            end
          >
        end
        object LabelToolBar: TLabel
          Left = 16
          Top = 556
          Caption = 'TERDToolBar'
        end
        object DemoToolBar: TERDToolBar
          Align = alNone
          Left = 16
          Top = 582
          Width = 460
          Height = 220
          Items = <
            item
              Caption = 'Open'
              Icon.BuiltIn = ei002Openen
              OnClick = OpenFile
            end
            item
              Caption = 'Dialog'
              Icon.BuiltIn = ei028Info
              OnClick = ShowDialog
            end
          >
        end
        object LabelProgressBar: TLabel
          Left = 506
          Top = 556
          Caption = 'TERDProgressBar'
        end
        object DemoProgressBar: TERDProgressBar
          Align = alNone
          Left = 506
          Top = 582
          Width = 460
          Height = 220
          Position = 65
        end
        object LabelScrollBar: TLabel
          Left = 16
          Top = 826
          Caption = 'TERDScrollBar'
        end
        object DemoScrollBar: TERDScrollBar
          Align = alNone
          Left = 16
          Top = 852
          Width = 460
          Height = 220
          Position = 25
        end
        object LabelSplitter: TLabel
          Left = 506
          Top = 826
          Caption = 'TERDSplitter'
        end
        object DemoSplitter: TERDSplitter
          Align = alNone
          Left = 506
          Top = 852
          Width = 460
          Height = 220
        end
      end
    end
    object DataPage: TERDPanel
      Align = alClient
      BorderWidth = 0
      Visible = False
      object DataScroll: TERDScrollBox
        Align = alClient
        object LabelInspector: TLabel
          Left = 506
          Top = 16
          Caption = 'TERDInspector'
        end
        object DemoInspector: TERDInspector
          Align = alNone
          Left = 506
          Top = 42
          Width = 460
          Height = 220
          Categories = <
            item Caption = 'General'
              Properties = <
                item
                  Name = 'Title'
                  Value = 'Example'
                end
                item Name = 'Mode' Value = 'Light' Kind = ivPickList
                  PickList.Strings = ('Light' 'Dark') end
              >
            end
          >
        end
        object LabelDataGrid: TLabel
          Left = 16
          Top = 286
          Caption = 'TERDDataGrid'
        end
        object DemoDataGrid: TERDDataGrid
          Align = alNone
          Left = 16
          Top = 312
          Width = 460
          Height = 220
          DataColumns = <
            item
              Caption = 'Name'
              Width = 220
            end
            item
              Caption = 'Status'
              Width = 120
            end
          >
        end
        object LabelTileGrid: TLabel
          Left = 506
          Top = 286
          Caption = 'TERDTileGrid'
        end
        object DemoTileGrid: TERDTileGrid
          Align = alNone
          Left = 506
          Top = 312
          Width = 460
          Height = 220
          Items = <
            item
              Caption = 'Files'
              Icon.BuiltIn = ei005Bestand
            end
            item
              Caption = 'Settings'
              Icon.BuiltIn = ei027Instellingen
            end
          >
        end
        object LabelKeyValueList: TLabel
          Left = 16
          Top = 556
          Caption = 'TERDKeyValueList'
        end
        object DemoKeyValueList: TERDKeyValueList
          Align = alNone
          Left = 16
          Top = 582
          Width = 460
          Height = 220
          Items = <
            item
              Key = 'Application'
              Value = 'Demo'
            end
            item
              Key = 'Theme'
              Value = 'ERDesigns Light'
            end
          >
        end
        object LabelHistogram: TLabel
          Left = 506
          Top = 556
          Caption = 'TERDHistogram'
        end
        object DemoHistogram: TERDHistogram
          Align = alNone
          Left = 506
          Top = 582
          Width = 460
          Height = 220
          Title = 'Sample distribution'
        end
        object LabelVirtualStringTree: TLabel
          Left = 16
          Top = 826
          Caption = 'TERDVirtualStringTree - lazy playlist groups'
        end
        object DemoVirtualStringTree: TERDVirtualStringTree
          Left = 16
          Top = 852
          Width = 950
          Height = 300
          Theme = Theme
          Header.AutoSizeIndex = 1
          Header.Columns = <
            item
              Text = 'Playlist / channel'
              Width = 380
            end
            item
              Text = 'Type'
              Width = 130
            end
            item
              Text = 'Quality'
              Width = 100
            end>
          TreeOptions.MiscOptions = [toCheckSupport, toFullRepaintOnResize, toInitOnSave, toWheelPanning]
          TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect]
          NodeDataSize = 8
          RootNodeCount = 20
          OnGetText = TreeGetText
          OnInitNode = TreeInitNode
          OnInitChildren = TreeInitChildren
          OnCompareNodes = TreeCompareNodes
          OnHeaderClick = TreeHeaderClick
        end
      end
    end
    object FilesColorsPage: TERDPanel
      Align = alClient
      BorderWidth = 0
      Visible = False
      object FilesColorsScroll: TERDScrollBox
        Align = alClient
        object LabelBreadcrumb: TLabel
          Left = 16
          Top = 16
          Caption = 'TERDBreadcrumb'
        end
        object DemoBreadcrumb: TERDBreadcrumb
          OnDropDown = BreadcrumbChoices
          OnNavigate = BreadcrumbNavigate
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
          Align = alNone
          Left = 16
          Top = 42
          Width = 460
          Height = 220
        end
        object LabelPlacesPane: TLabel
          Left = 506
          Top = 16
          Caption = 'TERDPlacesPane'
        end
        object DemoPlacesPane: TERDPlacesPane
          Align = alNone
          Left = 506
          Top = 42
          Width = 460
          Height = 220
        end
        object LabelFolderTree: TLabel
          Left = 16
          Top = 286
          Caption = 'TERDFolderTree'
        end
        object DemoFolderTree: TERDFolderTree
          Align = alNone
          Left = 16
          Top = 312
          Width = 460
          Height = 220
        end
        object LabelFileList: TLabel
          Left = 506
          Top = 286
          Caption = 'TERDFileList'
        end
        object DemoFileList: TERDFileList
          Align = alNone
          Left = 506
          Top = 312
          Width = 460
          Height = 220
        end
        object LabelColorSwatches: TLabel
          Left = 16
          Top = 556
          Caption = 'TERDColorSwatches'
        end
        object DemoColorSwatches: TERDColorSwatches
          Align = alNone
          Left = 16
          Top = 582
          Width = 460
          Height = 220
        end
        object LabelColorSpectrum: TLabel
          Left = 506
          Top = 556
          Caption = 'TERDColorSpectrum'
        end
        object DemoColorSpectrum: TERDColorSpectrum
          Align = alNone
          Left = 506
          Top = 582
          Width = 460
          Height = 220
        end
      end
    end
    object DialogsPage: TERDPanel
      Align = alClient
      BorderWidth = 0
      Visible = False
      object DialogsScroll: TERDScrollBox
        Align = alClient
        object LaunchDialog: TERDButton
          Left = 16
          Top = 16
          Width = 290
          Height = 44
          AutoSize = False
          Caption = 'TERDDialog'
          Tag = 0
          OnClick = ExecuteDemoDialog
        end
        object LaunchOpenDialog: TERDButton
          Left = 326
          Top = 16
          Width = 290
          Height = 44
          AutoSize = False
          Caption = 'TERDOpenDialog'
          Tag = 1
          OnClick = ExecuteDemoDialog
        end
        object LaunchSaveDialog: TERDButton
          Left = 636
          Top = 16
          Width = 290
          Height = 44
          AutoSize = False
          Caption = 'TERDSaveDialog'
          Tag = 2
          OnClick = ExecuteDemoDialog
        end
        object LaunchOpenPictureDialog: TERDButton
          Left = 16
          Top = 74
          Width = 290
          Height = 44
          AutoSize = False
          Caption = 'TERDOpenPictureDialog'
          Tag = 3
          OnClick = ExecuteDemoDialog
        end
        object LaunchSavePictureDialog: TERDButton
          Left = 326
          Top = 74
          Width = 290
          Height = 44
          AutoSize = False
          Caption = 'TERDSavePictureDialog'
          Tag = 4
          OnClick = ExecuteDemoDialog
        end
        object LaunchFolderDialog: TERDButton
          Left = 636
          Top = 74
          Width = 290
          Height = 44
          AutoSize = False
          Caption = 'TERDFolderDialog'
          Tag = 5
          OnClick = ExecuteDemoDialog
        end
        object LaunchPrintDialog: TERDButton
          Left = 16
          Top = 132
          Width = 290
          Height = 44
          AutoSize = False
          Caption = 'TERDPrintDialog'
          Tag = 6
          OnClick = ExecuteDemoDialog
        end
        object LaunchPageSetupDialog: TERDButton
          Left = 326
          Top = 132
          Width = 290
          Height = 44
          AutoSize = False
          Caption = 'TERDPageSetupDialog'
          Tag = 7
          OnClick = ExecuteDemoDialog
        end
        object LaunchFontDialog: TERDButton
          Left = 636
          Top = 132
          Width = 290
          Height = 44
          AutoSize = False
          Caption = 'TERDFontDialog'
          Tag = 8
          OnClick = ExecuteDemoDialog
        end
        object LaunchColorDialog: TERDButton
          Left = 16
          Top = 190
          Width = 290
          Height = 44
          AutoSize = False
          Caption = 'TERDColorDialog'
          Tag = 9
          OnClick = ExecuteDemoDialog
        end
        object LaunchFindDialog: TERDButton
          Left = 326
          Top = 190
          Width = 290
          Height = 44
          AutoSize = False
          Caption = 'TERDFindDialog'
          Tag = 10
          OnClick = ExecuteDemoDialog
        end
        object LaunchReplaceDialog: TERDButton
          Left = 636
          Top = 190
          Width = 290
          Height = 44
          AutoSize = False
          Caption = 'TERDReplaceDialog'
          Tag = 11
          OnClick = ExecuteDemoDialog
        end
        object LaunchTaskDialog: TERDButton
          Left = 16
          Top = 248
          Width = 290
          Height = 44
          AutoSize = False
          Caption = 'TERDTaskDialog'
          Tag = 12
          OnClick = ExecuteDemoDialog
        end
        object LaunchShowToast: TERDButton
          Left = 16
          Top = 330
          Width = 290
          Height = 44
          AutoSize = False
          Caption = 'Show toast'
          OnClick = ShowToast
        end
        object LaunchShowPopup: TERDButton
          Left = 326
          Top = 330
          Width = 290
          Height = 44
          AutoSize = False
          Caption = 'Open popup menu'
          OnClick = ShowPopup
        end
        object HintSample: TLabel
          Left = 16
          Top = 395
          Caption = 'Hover here to preview TERDHintStyle'
          Hint = 'ERD hint preview|A general tooltip with more detail.'
          ShowHint = True
        end
      end
    end
  end
  object StatusHost: TERDPanel
    Left = 0
    Top = 716
    Width = 300
    Height = 32
    BorderWidth = 0
    object BadgeOne: TERDBadge
      Left = 8
      Top = 7
      Count = 5
    end
    object BadgeTwo: TERDBadge
      Left = 46
      Top = 7
      Count = 12
      Kind = skSuccess
    end
    object DownloadButton: TERDButton
      Left = 96
      Top = 2
      AutoSize = False
      Caption = 'Downloads'
      Icon.BuiltIn = ei017Download
      Width = 170
      Height = 30
      Kind = bkGhost
      OnClick = ShowDialog
    end
  end
  object StatusBar: TERDStatusBar
    Left = 0
    Top = 716
    Width = 1280
    Height = 44
    Align = alBottom
    Panels = <
      item
      Kind = spkStatus
      Text = 'Ready'
      StatusKind = skSuccess
      Width = 120
    end
      item
      Kind = spkProgress
      Position = 35
      Text = 'Example progress'
      Width = 280
    end
      item
      Width = 310
      Alignment = paRight
    end
    >
  end
  object Theme: TERDTheme
    Left = 880
    Top = 8
    Mode = tmERDesignsLight
  end
  object Images: TImageList
    Left = 920
    Top = 8
    Width = 24
    Height = 24
  end
  object MainMenu: TMainMenu
    Left = 960
    Top = 8
    object FileMenu: TERDMenuItem
      Caption = 'File'
      object OpenMenu: TERDMenuItem
        Caption = 'Open'
        Icon.BuiltIn = ei002Openen
        OnClick = OpenFile
      end
      object DialogMenu: TERDMenuItem
        Caption = 'Dialog'
        Icon.BuiltIn = ei028Info
        OnClick = ShowDialog
      end
    end
  end
  object DemoDialog: TERDDialog
    Theme = Theme
    Left = 16
    Top = 650
    Title = 'General dialog'
    Text = 'Example dialog content.'
    Buttons = [mbOK]
  end
  object DemoOpenDialog: TERDOpenDialog
    Theme = Theme
    Left = 56
    Top = 650
  end
  object DemoSaveDialog: TERDSaveDialog
    Theme = Theme
    Left = 96
    Top = 650
  end
  object DemoOpenPictureDialog: TERDOpenPictureDialog
    Theme = Theme
    Left = 136
    Top = 650
  end
  object DemoSavePictureDialog: TERDSavePictureDialog
    Theme = Theme
    Left = 176
    Top = 650
  end
  object DemoFolderDialog: TERDFolderDialog
    Theme = Theme
    Left = 216
    Top = 650
  end
  object DemoPrintDialog: TERDPrintDialog
    Theme = Theme
    Left = 256
    Top = 650
  end
  object DemoPageSetupDialog: TERDPageSetupDialog
    Theme = Theme
    Left = 296
    Top = 650
  end
  object DemoFontDialog: TERDFontDialog
    Theme = Theme
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
    Left = 416
    Top = 650
  end
  object DemoReplaceDialog: TERDReplaceDialog
    Theme = Theme
    Left = 456
    Top = 650
  end
  object DemoTaskDialog: TERDTaskDialog
    Theme = Theme
    Left = 16
    Top = 690
    Title = 'Task dialog'
    Caption = 'Choose an action'
    Text = 'General application task.'
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
      Icon.BuiltIn = ei028Info
      OnClick = ShowDialog
    end
  end
  object DemoBackstage: TERDBackstage
    Visible = False
    Align = alClient
    Left = 0
    Top = 48
    Width = 1280
    Height = 668
    OnClose = BackstageClosed
    ItemIndex = 0
    Items = <
      item
        Caption = 'Information'
        Page = BackstageInfo
      end
      item
        Caption = 'Settings'
        Page = BackstageSettings
      end
    >
    object BackstageInfo: TERDPanel
      HeaderHeight = 32
      Caption = 'Application information'
      object BackstageInfoText: TLabel
        Left = 12
        Top = 48
        Caption = 'A page supplied by your application.'
      end
    end
    object BackstageSettings: TERDPanel
      HeaderHeight = 32
      Caption = 'Application settings'
    end
  end
end
