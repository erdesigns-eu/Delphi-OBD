"""Render the implemented vector catalog to SVGs, HTML and Delphi palette bitmaps.

No mockup PNG is used as a production asset. The preview and resources are
rendered from exactly the vector primitives encoded into the Pascal unit.
"""
import html, json, math, struct
from pathlib import Path
from io import BytesIO
from PIL import Image, ImageDraw
import catalog_vectors
ROOT=Path(__file__).resolve().parents[1]

def svg(prims):
    elements=[]
    for kind,*args in prims:
        if kind in ('line','loop','fill'):
            pts=' '.join(f'{x+8:g},{y+8:g}' for x,y in args[0]);tag='polyline' if kind=='line' else 'polygon'
            elements.append(f'<{tag} points="{pts}"'+(' fill="currentColor" stroke="none"' if kind=='fill' else '')+'/>')
        elif kind in ('ring','disc'):
            x,y,r=args;elements.append(f'<circle cx="{x+8:g}" cy="{y+8:g}" r="{r:g}"'+(' fill="currentColor" stroke="none"' if kind=='disc' else '')+'/>')
        elif kind=='ellipse':
            x,y,w,h=args;elements.append(f'<ellipse cx="{x+w/2+8:g}" cy="{y+h/2+8:g}" rx="{w/2:g}" ry="{h/2:g}"/>')
        elif kind=='arc':
            x,y,r,start,sweep=args;a=math.radians(start);b=math.radians(start+sweep)
            xx=x+8;yy=y+8;elements.append(f'<path d="M {xx+r*math.cos(a):g},{yy+r*math.sin(a):g} A {r:g},{r:g} 0 {int(abs(sweep)>180)} {int(sweep>=0)} {xx+r*math.cos(b):g},{yy+r*math.sin(b):g}"/>')
        else:raise ValueError(kind)
    return '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1" stroke-linecap="round" stroke-linejoin="round">'+''.join(elements)+'</svg>'

def bitmap(prims,size=24):
    # Supersampling gives crisp antialiasing for the IDE's 24px palette glyphs.
    factor=4;scale=size*factor/16;im=Image.new('RGB',(size*factor,size*factor),(255,0,255));d=ImageDraw.Draw(im);ink=(35,42,46);width=max(1,round(scale))
    def xy(x,y):return ((x+8)*scale,(y+8)*scale)
    for kind,*args in prims:
        if kind in ('line','loop','fill'):
            pts=[xy(x,y) for x,y in args[0]]
            if kind=='fill':d.polygon(pts,fill=ink)
            else:
                if kind=='loop':pts.append(pts[0])
                d.line(pts,fill=ink,width=width,joint='curve')
                for x,y in pts:d.ellipse((x-width/2,y-width/2,x+width/2,y+width/2),fill=ink)
        elif kind in ('ring','disc','arc'):
            x,y,r,*angles=args;x1,y1=xy(x-r,y-r);x2,y2=xy(x+r,y+r)
            if kind=='disc':d.ellipse((x1,y1,x2,y2),fill=ink)
            elif kind=='ring':d.ellipse((x1,y1,x2,y2),outline=ink,width=width)
            else:
                a,sweep=angles;d.arc((x1,y1,x2,y2),start=min(a,a+sweep),end=max(a,a+sweep),fill=ink,width=width)
        elif kind=='ellipse':
            x,y,w,h=args;d.ellipse((*xy(x,y),*xy(x+w,y+h)),outline=ink,width=width)
        else:raise ValueError(kind)
    out=BytesIO();im.resize((size,size),Image.Resampling.LANCZOS).save(out,format='BMP')
    return out.getvalue()[14:]  # RT_BITMAP payload is a DIB without the BMP file header.

def resource(name,payload):
    def align(raw):return raw+b'\0'*(-len(raw)%4)
    names=align(struct.pack('<HH',0xffff,2)+name.upper().encode('utf-16le')+b'\0\0')
    header=names+struct.pack('<IHHII',0,0x30,0,0,0)
    return align(struct.pack('<II',len(payload),len(header)+8)+header+payload)

# Per-class icons, not one placeholder shared by every component.
COMPONENT_ICONS={
    'TERDDebugModel': 151,
    'TERDValueTree': 69,
    'TERDFiberList': 69,
    'TERDDisassemblyView': 69,
    'TERDWatchPanel': 69,
    'TERDLocalsPanel': 69,
    'TERDCallStack': 69,
    'TERDBreakpointPanel': 151,
    'TERDTimeSeriesChart': 111,
    'TERDPCBComponentPalette': 87,
    'TERDPCBObjectTree': 69,
    'TERDPCBValidationPanel': 151,
    'TERDPCBDesigner': -1,  # Dedicated board glyph, not a catalog placeholder.
    'TERDRating': 224,
    'TERDCommandPalette': 14,
    'TERDDropZone': 18,
    'TERDLoadingIndicator': 16,
    'TERDLinkLabel': 148,
    'TERDDateTimePicker': 156,
    'TERDTimePicker': 156,
    'TERDCheckListBox': 32,
 'TERDSeekBar':223,'TERDRangeSlider':264,
 'TERDSearchEdit':14,'TERDSplitButton':161,'TERDTagEdit':116,'TERDExpander':48,
 'TERDColorSpectrum':120,'TERDColorSwatches':120,'TERDRangeBar':111,
 'TERDTheme':27,'TERDCard':62,'TERDVirtualStringTree':87,'TERDPanel':48,
 'TERDButton':161,'TERDCheckBox':63,'TERDRadioButton':261,'TERDChip':116,
 'TERDBadge':40,'TERDBanner':30,'TERDEdit':13,'TERDComboBox':36,
 'TERDListBox':60,'TERDSegmented':48,'TERDSidebar':57,'TERDScrollBar':36,
 'TERDScrollBox':48,'TERDSplitter':48,'TERDSpinEdit':307,'TERDTrackBar':264,
 'TERDMemo':102,'TERDCalendar':155,'TERDDatePicker':154,
 'TERDTitleBar':41,'TERDMenuBar':57,'TERDPopupMenu':60,'TERDMenuItem':40,
 'TERDRibbon':112,'TERDBackstage':5,'TERDTabs':49,
 'TERDToolBar':48,'TERDStatusBar':153,'TERDProgressBar':232,'TERDDialog':28,
 'TERDToastManager':39,'TERDDataGrid':87,'TERDTileGrid':59,'TERDKeyValueList':61,
 'TERDInspector':27,'TERDHistogram':111,'TERDHintStyle':29,'TERDTaskDialog':151,
 'TERDOpenDialog':2,'TERDSaveDialog':3,'TERDOpenPictureDialog':184,
 'TERDSavePictureDialog':187,'TERDFolderDialog':6,'TERDFindDialog':14,
 'TERDReplaceDialog':303,'TERDColorDialog':120,'TERDFontDialog':102,
 'TERDPrintDialog':21,'TERDPageSetupDialog':77,'TERDBreadcrumb':33,
 'TERDPlacesPane':6,'TERDFolderTree':69,'TERDFileList':60}

def component_vectors(name):
    if name == 'TERDPCBDesigner':
        return [
            ('loop', [(-6,-5),(6,-5),(6,5),(-6,5)]),
            ('loop', [(-2,-2),(2,-2),(2,2),(-2,2)]),
            ('line', [(-5,-3),(-4,-3),(-4,0),(-2,0)]),
            ('line', [(2,0),(4,0),(4,3),(5,3)]),
            ('disc', -5,3,0.6), ('disc', 5,-3,0.6),
        ]
    return catalog_vectors.vectors(COMPONENT_ICONS[name])


def build():
    catalog=json.loads((ROOT/'docs/iconset-catalog.json').read_text())
    folder=ROOT/'assets/svg';folder.mkdir(exist_ok=True)
    cards=[]
    for icon in catalog['icons']:
        drawing=svg(catalog_vectors.vectors(icon['id']));(folder/f"{icon['id']:03}.svg").write_text(drawing+'\n')
        label=html.escape(icon['label']);cards.append(f'<article><div class="samples"><span>{drawing}</span><span class="dark">{drawing}</span></div><p>{icon["id"]:03} {label}</p></article>')
    (ROOT/'docs/vector-preview.html').write_text('<!doctype html><html lang="nl"><meta charset="utf-8"><title>ERD vectorcatalogus</title><style>body{font:14px system-ui;margin:24px;color:#232a2e;background:white}main{display:grid;grid-template-columns:repeat(auto-fill,minmax(160px,1fr));gap:20px}.samples{display:flex;gap:8px}span{padding:12px;background:#edf0f2;border-radius:6px}.dark{color:#fafafa;background:#232a2e}svg{display:block;width:32px;height:32px}p{max-width:180px}</style><h1>ERD Components — geïmplementeerde vectorcatalogus</h1><p>320 catalogusiconen. Licht/donker gebruiken dezelfde vectorgegevens. Visuele verfijning volgt apart.</p><main>'+''.join(cards)+'</main></html>\n')
    components=json.loads((ROOT/'assets/components.json').read_text())
    missing=set(components)-COMPONENT_ICONS.keys()
    if missing:raise ValueError('Assign class icons: '+', '.join(sorted(missing)))
    result=struct.pack('<IIHHHHIHHII',0,32,0xffff,0,0xffff,0,0,0,0,0,0)
    for name in components:result+=resource(name,bitmap(component_vectors(name)))
    (ROOT/'src/DesignTime/ERD.Components.Palette.res').write_bytes(result)
    print(len(catalog['icons']),'SVG icons and',len(components),'class palette resources exported.')
if __name__=='__main__':build()
