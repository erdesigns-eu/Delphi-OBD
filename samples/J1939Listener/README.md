# J1939Listener — J1939 bus-client demo

Demonstrates `TOBDJ1939` — SourceAddress, NAME, address-claim
payload assembly, request payload, and `DispatchInbound` event
routing.

```sh
dcc32 -B J1939Listener.dpr
J1939Listener
```

To drive a real bus: wire `J.Sessions.OnFrameSend` to your CAN
transmit path and call `J.DispatchInbound(canId, payload)` for
every received frame.
