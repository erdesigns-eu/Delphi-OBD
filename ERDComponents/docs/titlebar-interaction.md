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

Maximize/restore hover is tracked on the window under the pointer, including the
titlebar child. Client/non-client leave transitions refresh against the pointer
position instead of clearing the painted button's hover while it is still under
the pointer. Hover and press tint the actual caption surface at distinct strengths.
