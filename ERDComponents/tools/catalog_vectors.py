"""General application vector vocabulary; coordinates use a 16-unit grid.

The semantic mapping is explicit: every catalog ID has a base and optional
operation badge/text. No generated placeholders or automotive drawings.
"""
import math

def line(*p): return ('line', list(p))
def loop(*p): return ('loop', list(p))
def rect(x,y,w,h): return loop((x,y),(x+w,y),(x+w,y+h),(x,y+h))
def ring(x,y,r): return ('ring',x,y,r)
def arc(x,y,r,start,sweep): return ('arc',x,y,r,start,sweep)
def disc(x,y,r): return ('disc',x,y,r)
FILE=[loop((-5,-7),(2,-7),(5,-4),(5,7),(-5,7)),line((1,-7),(1,-3),(5,-3))]
FOLDER=[loop((-7,-4),(-2,-4),(0,-2),(7,-2),(7,5),(-7,5))]
LIST=[line((-5,y),(5,y)) for y in (-4,0,4)]
PLAY=[loop((-4,-6),(6,0),(-4,6))]
PAUSE=[line((-3,-5),(-3,5)),line((3,-5),(3,5))]
STOP=[rect(-5,-5,10,10)]
SCREEN=[rect(-7,-5,14,9),line((0,4),(0,7)),line((-3,7),(3,7))]
USER=[ring(0,-3,3),arc(0,7,6,180,180)]
CLOUD=[arc(-3,1,4,90,180),arc(1,-1,5,180,180),arc(4,1,3,270,180),line((5,4),(-3,5))]
DB=[('ellipse',-6,-6,12,4),arc(0,0,6,0,180),arc(0,4,6,0,180),line((-6,-4),(-6,4)),line((6,-4),(6,4))]
CLOCK=[ring(0,0,6),line((0,-4),(0,0),(3,2))]
CAL=[rect(-6,-5,12,11),line((-6,-1),(6,-1)),line((-3,-7),(-3,-3)),line((3,-7),(3,-3))]
GEAR=[loop(*[(round((7 if i%2==0 else 5)*math.cos(i*math.pi/8),3),round((7 if i%2==0 else 5)*math.sin(i*math.pi/8),3)) for i in range(16)]),ring(0,0,2)]
SEARCH=[ring(-1,-1,4),line((2,2),(7,7))]
CHECK=[line((-5,0),(-1,4),(6,-5))]
X=[line((-5,-5),(5,5)),line((5,-5),(-5,5))]
PLUS=[line((-5,0),(5,0)),line((0,-5),(0,5))]
MINUS=[line((-5,0),(5,0))]
UP=[line((0,6),(0,-6),(-4,-2)),line((0,-6),(4,-2))]
DOWN=[line((0,-6),(0,6),(-4,2)),line((0,6),(4,2))]
LEFT=[line((6,0),(-6,0),(-2,-4)),line((-6,0),(-2,4))]
RIGHT=[line((-6,0),(6,0),(2,-4)),line((6,0),(2,4))]
REFRESH=[arc(0,0,6,30,280),line((4,-5),(6,-1),(2,-1))]
PLAYLIST=LIST+[loop((-6,-6),(-3,-4),(-6,-2))]
TRACK=[line((-3,4),(-3,-5),(5,-7),(5,2)),disc(-5,4,2),disc(3,2,2)]
LOCK=[rect(-5,-1,10,8),arc(0,-1,4,180,180),line((-4,-1),(-4,-4)),line((4,-1),(4,-4))]
UNLOCK=[rect(-5,-1,10,8),arc(1,-4,4,180,180),line((-3,-1),(-3,-4))]
SPEAKER=[loop((-7,-2),(-4,-2),(0,-6),(0,6),(-4,2),(-7,2)),arc(0,0,4,-55,110),arc(0,0,7,-55,110)]
TV=SCREEN
BASE={
 'file':FILE,'folder':FOLDER,'list':LIST,'playlist':PLAYLIST,'screen':SCREEN,'tv':TV,
 'user':USER,'users':[ring(-3,-3,2.5),ring(4,-2,2),arc(-3,7,4,180,180),arc(4,7,3,180,180)],
 'cloud':CLOUD,'database':DB,'clock':CLOCK,'calendar':CAL,'settings':GEAR,'search':SEARCH,
 'check':CHECK,'error':X,'plus':PLUS,'minus':MINUS,'up':UP,'down':DOWN,'left':LEFT,'right':RIGHT,
 'refresh':REFRESH,'play':PLAY,'pause':PAUSE,'stop':STOP,'track':TRACK,'lock':LOCK,'unlock':UNLOCK,
 'speaker':SPEAKER,'table':[rect(-6,-6,12,12),line((-6,-2),(6,-2)),line((-6,2),(6,2)),line((-2,-6),(-2,6)),line((2,-6),(2,6))],
 'filter':[loop((-7,-6),(7,-6),(2,0),(2,6),(-2,6),(-2,0))],
 'mail':[rect(-7,-5,14,10),line((-7,-5),(0,1),(7,-5))],
 'printer':[rect(-7,-2,14,7),rect(-4,-7,8,5),rect(-4,1,8,6),disc(4,0,.7)],
 'copy':[rect(-6,-6,9,10),rect(-3,-3,9,10)],
 'paste':[rect(-5,-5,10,12),rect(-2,-7,4,3)],
 'cut':[ring(-4,4,2),ring(4,4,2),line((-3,2),(5,-7)),line((3,2),(-5,-7))],
 'edit':[loop((-6,5),(3,-6),(6,-3),(-3,7),(-6,7)),line((1,-4),(4,-1))],
 'trash':[rect(-5,-3,10,10),line((-7,-4),(7,-4)),rect(-2,-7,4,3),line((-2,-1),(-2,5)),line((2,-1),(2,5))],
 'save':[loop((-6,-7),(4,-7),(6,-5),(6,7),(-6,7)),rect(-3,-7,5,5),rect(-3,1,6,6)],
 'download':DOWN+[line((-6,3),(-6,7),(6,7),(6,3))],
 'upload':UP+[line((-6,3),(-6,7),(6,7),(6,3))],
 'import':RIGHT+[line((1,-7),(7,-7),(7,7),(1,7))],
 'export':RIGHT+[line((1,-7),(-7,-7),(-7,7),(1,7))],
 'alert':[loop((0,-7),(7,6),(-7,6)),line((0,-2),(0,1)),disc(0,4,.6)],
 'info':[ring(0,0,7),line((0,-1),(0,4)),disc(0,-4,.7)],
 'help':[ring(0,0,7),arc(0,-2,2.5,180,270),line((0,0),(0,2)),disc(0,4,.7)],
 'bell':[arc(0,-1,4,180,180),line((-4,-1),(-4,3),(-6,5),(6,5),(4,3),(4,-1)),arc(0,5,2,0,180)],
 'more':[disc(x,0,1) for x in (-5,0,5)],
 'window':[rect(-7,-6,14,12),line((-7,-3),(7,-3))],
 'tabs':[rect(-6,-5,12,10),line((-6,-2),(0,-2),(0,-5))],
 'grid':[rect(x,y,5,5) for x,y in [(-6,-6),(1,-6),(-6,1),(1,1)]],
 'cards':[rect(-6,-5,5,10),rect(1,-5,5,10)],
 'zip':FILE+[line((0,-4),(0,5))]+[disc(0,y,.6) for y in (-3,0,3)],
 'keyboard':[rect(-7,-4,14,8),line((-4,1),(4,1))]+[disc(x,-1,.5) for x in (-4,-1,2,5)],
 'mouse':[('ellipse',-4,-7,8,14),line((0,-7),(0,-1))],
 'touch':[ring(0,-3,4),line((0,-5),(0,1),(-3,-1),(-4,1),(0,6),(4,6),(6,1),(2,-1))],
 'chartbar':[rect(-6,1,3,6),rect(-1,-3,3,10),rect(4,-6,3,13)],
 'chartline':[line((-7,7),(-7,-7)),line((-7,7),(7,7)),line((-6,4),(-2,0),(1,2),(6,-5))],
 'chartpie':[ring(0,0,6),line((0,-6),(0,0),(6,0))],
 'chartarea':[loop((-6,6),(-6,2),(-2,-3),(1,1),(6,-5),(6,6))],
 'chartscatter':[disc(x,y,.7) for x,y in [(-5,5),(-4,-1),(-2,3),(1,0),(3,-5),(5,-2),(6,4)]],
 'histogram':[rect(-6,1,3,6),rect(-3,-5,3,12),rect(0,-2,3,9),rect(3,3,3,4)],
 'dashboard':[rect(-7,-6,14,12),rect(-5,-4,4,3),rect(1,-4,4,8),rect(-5,1,4,3)],
 'tag':[loop((-7,0),(0,-7),(7,-7),(7,0),(0,7)),ring(4,-4,1)],
 'link':[arc(-3,2,3,90,270),arc(3,-2,3,270,270),line((-2,2),(2,-2))],
 'palette':[ring(0,0,6)]+[ring(x,y,.8) for x,y in [(-3,-2),(1,-3),(3,0),(-3,2)]],
 'key':[ring(-3,-3,3),line((-1,-1),(6,6),(6,3)),line((4,4),(4,1))],
 'certificate':FILE+[ring(2,2,2.5),line((0,4),(0,7),(2,5),(4,7),(4,4))],
 'shield':[loop((-6,-5),(0,-7),(6,-5),(5,3),(0,7),(-5,3))],
 'server':[rect(-6,-6,12,5),rect(-6,1,12,5),disc(-3,-3,.6),disc(-3,3,.6)],
 'ethernet':[rect(-2,-7,4,4),line((0,-3),(0,1)),line((-5,1),(5,1)),rect(-7,3,4,4),rect(3,3,4,4),line((-5,1),(-5,3)),line((5,1),(5,3))],
 'wifi':[arc(0,6,9,225,90),arc(0,6,6,225,90),arc(0,6,3,225,90),disc(0,6,1)],
 'bluetooth':[line((-3,-4),(4,3),(0,7),(0,-7),(4,-3),(-3,4))],
 'usb':[line((0,7),(0,-7),(-2,-4)),line((0,-7),(2,-4)),line((0,3),(-4,-1),(-4,-4)),line((0,0),(4,-2),(4,-5)),disc(-4,-5,1),rect(3,-7,2,2)],
 'globe':[ring(0,0,7),('ellipse',-3,-7,6,14),line((-7,0),(7,0)),line((-5,-4),(5,-4)),line((-5,4),(5,4))],
 'tasks':[rect(-6,y,3,3)+() for y in (-6,-1,4)]+[line((0,y),(6,y)) for y in (-5,0,5)],
 'timer':CLOCK+[line((-2,-7),(2,-7))],
 'next':PLAY+[line((6,-6),(6,6))],
 'previous':[loop((4,-6),(-6,0),(4,6)),line((-6,-6),(-6,6))],
 'fastforward':[loop((-7,-5),(0,0),(-7,5)),loop((0,-5),(7,0),(0,5))],
 'rewind':[loop((7,-5),(0,0),(7,5)),loop((0,-5),(-7,0),(0,5))],
 'record':[ring(0,0,7),disc(0,0,3)],
 'eject':[loop((0,-6),(6,2),(-6,2)),line((-6,6),(6,6))],
 'repeat':[line((-6,-2),(-6,-5),(6,-5),(3,-7)),line((6,-5),(3,-2)),line((6,2),(6,5),(-6,5),(-3,7)),line((-6,5),(-3,2))],
 'shuffle':[line((-7,-5),(-4,-5),(4,5),(7,5),(5,3)),line((7,5),(5,7)),line((-7,5),(-4,5),(4,-5),(7,-5),(5,-7)),line((7,-5),(5,-3))],
 'album':[ring(0,0,7),ring(0,0,2)],
 'film':[rect(-7,-6,14,12),line((-4,-6),(-4,6)),line((4,-6),(4,6))]+[line((-7,y),(-4,y)) for y in (-3,0,3)]+[line((4,y),(7,y)) for y in (-3,0,3)],
 'camera':[loop((-7,-4),(-3,-4),(-2,-7),(2,-7),(3,-4),(7,-4),(7,6),(-7,6)),ring(0,1,3)],
 'image':[rect(-7,-6,14,12),disc(3,-3,1),line((-6,5),(-2,0),(1,3),(3,1),(6,5))],
 'equalizer':[line((x,-7),(x,7)) for x in (-5,0,5)]+[ring(x,y,1.5) for x,y in [(-5,-3),(0,2),(5,-1)]],
 'headphones':[arc(0,0,6,180,180),rect(-7,-1,3,7),rect(4,-1,3,7)],
 'subtitles':[rect(-7,-5,14,10),line((-5,1),(-1,1)),line((1,1),(5,1)),line((-5,3),(5,3))],
 'pip':[rect(-7,-5,14,10),rect(0,0,6,4)],
 'cast':[line((-7,0),(-7,-6),(7,-6),(7,6),(0,6)),arc(-7,7,7,270,90),arc(-7,7,4,270,90),disc(-7,7,.7)],
 'broadcast':[disc(0,0,1),arc(0,0,4,-65,130),arc(0,0,4,115,130),arc(0,0,7,-65,130),arc(0,0,7,115,130)],
 'radio':[rect(-7,-3,14,10),line((-5,-3),(5,-7)),ring(-3,2,2),line((2,0),(5,0)),line((2,3),(5,3))],
 'bookmark':[loop((-4,-7),(4,-7),(4,7),(0,4),(-4,7))],
}
# Stencil alphabet used for format/quality badges. These are vector shapes,
# not fonts whose availability changes across systems.
FONT={
 'A':['010','101','111','101','101'],'B':['110','101','110','101','110'],
 'C':['011','100','100','100','011'],'D':['110','101','101','101','110'],
 'E':['111','100','110','100','111'],'F':['111','100','110','100','100'],
 'G':['011','100','101','101','011'],'H':['101','101','111','101','101'],
 'I':['111','010','010','010','111'],'J':['001','001','001','101','010'],
 'K':['101','101','110','101','101'],'L':['100','100','100','100','111'],
 'M':['101','111','111','101','101'],'N':['101','111','111','111','101'],
 'O':['010','101','101','101','010'],'P':['110','101','110','100','100'],
 'Q':['010','101','101','111','011'],'R':['110','101','110','101','101'],
 'S':['011','100','010','001','110'],'T':['111','010','010','010','010'],
 'U':['101','101','101','101','111'],'V':['101','101','101','101','010'],
 'W':['101','101','111','111','101'],'X':['101','101','010','101','101'],
 'Y':['101','101','010','010','010'],'Z':['111','001','010','100','111'],
 '0':['111','101','101','101','111'],'1':['010','110','010','010','111'],
 '2':['110','001','010','100','111'],'3':['110','001','010','001','110'],
 '4':['101','101','111','001','001'],'5':['111','100','110','001','110'],
 '6':['011','100','111','101','111'],'7':['111','001','010','010','010'],
 '8':['111','101','111','101','111'],'9':['111','101','111','001','110'],
 '#':['101','111','101','111','101'],'?':['110','001','010','000','010']}

def transform(prims,scale,dx=0,dy=0):
    result=[]
    for kind,*args in prims:
        if kind in ('line','loop','fill'):result.append((kind,[(x*scale+dx,y*scale+dy) for x,y in args[0]]))
        elif kind in ('ring','disc'):result.append((kind,args[0]*scale+dx,args[1]*scale+dy,args[2]*scale))
        elif kind in ('arc','pie'):result.append((kind,args[0]*scale+dx,args[1]*scale+dy,args[2]*scale,*args[3:]))
        elif kind=='ellipse':result.append((kind,args[0]*scale+dx,args[1]*scale+dy,args[2]*scale,args[3]*scale))
        else:raise ValueError(kind)
    return result

def text(code,x=0,y=1,size=1):
    result=[];start=x-(len(code)*4-1)*size/2
    for n,c in enumerate(code):
        for row,bits in enumerate(FONT[c]):
            for col,on in enumerate(bits):
                if on=='1':
                    xx=start+(n*4+col)*size;yy=y+(row-2)*size
                    result.append(('fill',[(xx,yy),(xx+size*.8,yy),(xx+size*.8,yy+size*.8),(xx,yy+size*.8)]))
    return result

def make(base,badge=None,code=None):
    prims=list(BASE[base])
    if code:prims+=text(code,size=min(1,12/(len(code)*4-1)))
    if badge:
        # The base is reduced to leave a clear operation area at bottom right.
        prims=transform(prims,.72,-1.5,-1.5)
        prims+=transform(BASE[badge],.42,4.5,4.5)
    return prims

# IDs remain the approved catalog order, with explicit semantic vocabulary.
SPECS={}
def group(first,items):
    assert len(items)==40,(first,len(items))
    for i,item in enumerate(items):
        parts=item.split(':');SPECS[first+i]=(parts[0],parts[1] or None if len(parts)>1 else None,parts[2] if len(parts)>2 else None)

group(1, ['file:plus','folder','save','save:edit','file','folder','copy','paste','cut','left','right','trash','edit','search','filter','refresh','download','upload','import','export','printer','file::PDF','file:chartbar','mail','user','users','settings','info','help','alert','error','check','left','right','up','down','lock','unlock','bell','more'])
group(41,['minus','window','copy','window:plus','window:minus','window:error','copy:refresh','cards','tabs:plus','tabs:error','tabs:right','tabs:left','bookmark','bookmark:minus','cards:left','cards:right','window:cards','window:minus','grid','list','window:list','cards','table:check','table:error','table:refresh','search:plus','search:minus','search:refresh','folder:plus','folder:edit','file:edit','copy','zip','zip:upload','save:check','printer:search','file:settings','keyboard','mouse','touch'])
group(81,['database','database:plus','database:minus','database:link','database:download','database:upload','table','table:plus','table:minus','table:right','table:left','list:up','list:down','copy:plus','copy:minus','filter:plus','filter:error','file::CSV','file::JSON','file::XML','file:table','file:list','file:chartpie','file:check','file:copy','chartbar','chartline','chartpie','chartarea','chartscatter','histogram','dashboard','file::FX','file::123','chartbar:chartpie','tag','tag:copy','link:plus','link:minus','palette'])
group(121,['import','export','user:edit','user:plus','user:minus','users:shield','shield:check','lock:key','key','certificate','certificate:check','certificate:error','shield','shield:user','file:lock','file:unlock','server:shield','server:globe','ethernet','wifi','bluetooth','usb','cloud','cloud:download','cloud:upload','globe:check','globe:minus','link:error','refresh','refresh:alert','tasks','tasks:plus','tasks:check','tasks:calendar','calendar','clock','clock:play','timer','bell:clock','bell:error'])
group(161,['play','pause','stop','next','previous','fastforward','rewind','record','eject','speaker:error','speaker:minus','speaker:plus','repeat::1','repeat','repeat:error','shuffle','shuffle:error','folder:track','album','user:track','track','file:track','file:play','image','track:plus','film','camera','equalizer','speaker:usb','headphones','speaker','subtitles','subtitles:error','track:list','screen:settings','clock:play','bookmark:plus','pip','cast','broadcast'])
group(201,['playlist:plus','folder:playlist','save:playlist','playlist:edit','playlist:copy','playlist:import','playlist:export','track:plus','track:minus','playlist:up','track:up','track:down','list:play','list:plus','list:minus','list:trash','link:broadcast','broadcast:search','broadcast:check','broadcast:error','tv','radio','calendar:tv','bookmark:plus','bookmark:minus','download:play','download:pause','download:right','download:error','download:refresh','download:clock','download:timer','download:check','download:alert','download:settings','clock:down','folder:download','tasks:trash','copy:download','clock:download'])
group(241,['film:folder','file:film','copy:play','screen::S1','screen::E1','screen:play','screen:clock','screen:bookmark','screen::TXT','screen:list','screen:right','screen:left','screen:search','screen:copy','subtitles:globe','file:subtitles','subtitles:refresh','subtitles:left','subtitles:right','speaker:globe','speaker::2','speaker::1','speaker::5','equalizer:speaker','equalizer:save','speaker::B','speaker::T','tv:info','tv:next','calendar:record','tv:record','clock:refresh','clock:pause','tv::LIVE','tv:refresh','tv:previous','copy:tv','tv::#','tv:image','file::M3U'])
group(281,['playlist:upload','playlist:download','cloud:link','cloud:refresh','server::XC','server:tv:XC','user:server:XC','server:refresh:XC','file:import:M3U','file:export:M3U','file::M3U8','file::PLS','file::XSPF','file::ASX','playlist::JSON','file:refresh','playlist:check','playlist:settings','copy:tv','copy:trash','tv:search','playlist:filter','search:refresh','list:refresh','list:up:HD','list:down:SD','list:up:4K','tv::HD','tv::FHD','tv::4K','tv:right','list:plus','list:minus','list:edit','list:up','tv:import','tv:edit','tv:copy','broadcast:search','broadcast:trash'])

def vectors(icon_id):
    return make(*SPECS[icon_id])
