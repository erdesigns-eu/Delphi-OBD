"""What the VCL declares, by the class that declares it.

Each layer adds to the one it descends from. Names only: what matters is
that the member is there, not what type it has. Shared by the checkers that
have to know whether a name a method reads is already something the class
inherited.
"""

# What the VCL declares, by the class that declares it, each layer adding to
# the one it descends from. Names only: what matters is that the member is
# there, not what type it has.
PERSISTENT = {'assign', 'assignto', 'getnamepath', 'getowner'}
COMPONENT = PERSISTENT | {
    'name', 'tag', 'owner', 'components', 'componentcount', 'componentindex',
    'componentstate', 'componentstyle', 'designinfo', 'destroying',
    'findcomponent', 'insertcomponent', 'removecomponent', 'hasparent',
    'getparentcomponent', 'setsubcomponent', 'notification', 'loaded',
    'updateregistry', 'observers'}
CONTROL = COMPONENT | {
    'action', 'align', 'alignwithmargins', 'anchors', 'bidimode', 'boundsrect',
    'caption', 'clientheight', 'clientorigin', 'clientrect', 'clientwidth',
    'color', 'constraints', 'controlstate', 'controlstyle', 'cursor',
    'customhint', 'dragcursor', 'dragkind', 'dragmode', 'enabled',
    'explicitheight', 'explicitleft', 'explicittop', 'explicitwidth',
    'floating', 'font', 'height', 'helpcontext', 'helpkeyword', 'helptype',
    'hint', 'hostdocksite', 'left', 'margins', 'parent', 'parentbidimode',
    'parentcolor', 'parentcustomhint', 'parentfont', 'parentshowhint',
    'popupmenu', 'scalingflags', 'showhint', 'styleelements', 'stylename',
    'text', 'top', 'touch', 'visible', 'width', 'windowproc',
    'bringtofront', 'sendtoback', 'clienttoscreen', 'screentoclient',
    'begindrag', 'enddrag', 'dragging', 'hide', 'show', 'invalidate',
    'refresh', 'repaint', 'update', 'setbounds', 'perform', 'gettextlen',
    'gettext', 'settext', 'click', 'dblclick', 'resize', 'changescale',
    'mousedown', 'mousemove', 'mouseup', 'mousewheel', 'mouseenter',
    'mouseleave', 'dragover', 'dragdrop', 'currentppi', 'scalevalue',
    'getdesigninfo', 'defaulthandler', 'stylerservices'}
WINCONTROL = CONTROL | {
    'brush', 'controls', 'controlcount', 'doublebuffered', 'handle',
    'handleallocated', 'padding', 'parentdoublebuffered', 'parentwindow',
    'showing', 'tabstop', 'taborder', 'usedockmanager', 'focused', 'setfocus',
    'canfocus', 'disablealign', 'enablealign', 'realign', 'insertcontrol',
    'removecontrol', 'scaleby', 'updatecontrolstate', 'createparams',
    'createwnd', 'destroywnd', 'createhandle', 'destroyhandle', 'paintwindow',
    'painthandler', 'wndproc', 'doenter', 'doexit', 'keydown', 'keyup',
    'keypress', 'alignposition', 'aligncontrols', 'paintto', 'flipchildren',
    'selectfirst', 'selectnext', 'dockmanager', 'docksite', 'ctl3d'}
CUSTOMCONTROL = WINCONTROL | {'canvas', 'paint'}
GRAPHIC = CONTROL | {'canvas', 'paint'}
PANEL = CUSTOMCONTROL | {
    'alignment', 'bevelinner', 'bevelouter', 'bevelkind', 'bevelwidth',
    'borderstyle', 'borderwidth', 'fullrepaint', 'locked', 'parentbackground',
    'verticalalignment'}
FORM = CUSTOMCONTROL | {
    'activecontrol', 'bordericons', 'borderstyle', 'close', 'closequery',
    'formstyle', 'icon', 'keypreview', 'menu', 'modalresult', 'monitor',
    'position', 'release', 'showmodal', 'windowstate', 'print', 'pixelsperinch',
    'oncreate', 'onclose', 'onshow', 'onhide', 'onshortcut', 'defocuscontrol'}
COLLECTIONITEM = PERSISTENT | {
    'collection', 'displayname', 'id', 'index', 'changed', 'getdisplayname',
    'setindex'}
COLLECTION = PERSISTENT | {
    'add', 'clear', 'count', 'delete', 'insert', 'items', 'owner', 'update',
    'notify', 'beginupdate', 'endupdate', 'getattr', 'getitem', 'setitem'}
THREAD = {
    'execute', 'terminate', 'terminated', 'start', 'suspended', 'priority',
    'freeonterminate', 'handle', 'threadid', 'returnvalue', 'waitfor',
    'synchronize', 'queue', 'checkterminated', 'donterminate', 'namethreadfordebugging'}
STRINGS = PERSISTENT | {
    'add', 'addobject', 'addstrings', 'append', 'clear', 'count', 'delete',
    'exchange', 'indexof', 'insert', 'objects', 'strings', 'text', 'values',
    'names', 'beginupdate', 'endupdate', 'loadfromfile', 'savetofile',
    'commatext', 'delimiter', 'delimitedtext', 'sorted', 'capacity'}

BASES = {
    'tpersistent': PERSISTENT,
    'tcomponent': COMPONENT, 'tdatamodule': COMPONENT,
    'tcontrol': CONTROL,
    'twincontrol': WINCONTROL,
    'tcustomcontrol': CUSTOMCONTROL,
    'tgraphiccontrol': GRAPHIC,
    'tcustompanel': PANEL, 'tpanel': PANEL,
    'tcustomform': FORM, 'tform': FORM,
    'tcustomframe': CUSTOMCONTROL, 'tframe': CUSTOMCONTROL,
    'tcollectionitem': COLLECTIONITEM, 'townedcollection': COLLECTION,
    'tcollection': COLLECTION,
    'tthread': THREAD,
    'tstrings': STRINGS, 'tstringlist': STRINGS,
}

# Virtual methods the VCL and the RTL declare public, so that an override of
# one put under protected or private is a hint on every build (H2269) and out
# of reach of whatever called it from outside. Only names that are public
# wherever they are declared belong here: a name a VCL class declares
# protected would turn every honest override of it into a finding.
# 'update' is deliberately absent: TControl declares it public and TCollection
# declares one of its own protected, so the name says nothing on its own.
PUBLIC_VIRTUALS = {
    'assign', 'invalidate', 'repaint', 'show', 'hide', 'setbounds',
    'bringtofront', 'sendtoback', 'setfocus', 'paintto', 'flipchildren',
    'defaulthandler', 'dispatch', 'afterconstruction', 'beforedestruction',
    'safecallexception', 'tostring', 'equals', 'gethashcode', 'getnamepath',
    'getparentcomponent', 'hasparent', 'setsubcomponent',
    'loadfromfile', 'savetofile', 'loadfromstream', 'savetostream',
    'close', 'closequery',
}
