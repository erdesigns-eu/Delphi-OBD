# Titlebar maximize/restore tracking

The custom maximize button keeps `HTMAXBUTTON` for Windows hover/Snap support,
but owns mouse capture and release. It does not pass button-down to the native
caption tracking loop, whose button geometry differs from the painted caption.
A release over the painted button sends one maximize/restore system command;
release outside or loss of capture cancels. Custom titlebar hit targets take
precedence over the hidden native caption geometry.

The Windows smoke suite exercises client and non-client release, restore,
release outside and capture loss. Native Windows 11 Snap-hover behaviour still
requires interactive verification; the Linux checks do not establish it.
