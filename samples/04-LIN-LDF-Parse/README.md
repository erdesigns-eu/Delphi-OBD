# 04-LIN-LDF-Parse

Loads a LIN Description File and prints the structural contents
(protocol version, bus speed, master / slave nodes, signals, frames
with signal placement, schedule tables with microsecond delays).

Demonstrates the standalone LDF parser
(`OBD.Protocol.LIN.LDF`). No hardware or bus access required.

## Build

```
dcc32 -B ParseLDF.dpr
```

## Run

```
ParseLDF path\to\my_cluster.ldf
```

The included `sample.ldf` is a minimal but valid LIN 2.2A file you
can use to smoke-test the parser:

```
ParseLDF sample.ldf
```
