//------------------------------------------------------------------------------
//  J1939Flash — sample 29
//
//  Heavy-duty (J1939-73) flash template. Builds DM14 / DM15 / DM16
//  frames in memory and prints them so the host can route them
//  through their CAN driver. NO wire access in this sample;
//  hosts add the bus binding when they integrate.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

program J1939Flash;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  OBD.Types in '..\..\src\Core\OBD.Types.pas',
  OBD.J1939.MemoryAccess in '..\..\src\Flashing\OBD.J1939.MemoryAccess.pas';

procedure DumpHex(const ALabel: string; const ABytes: TBytes);
var
  I: Integer;
begin
  Write(ALabel: 16, ': ');
  for I := 0 to High(ABytes) do
  begin
    if I > 0 then Write(' ');
    Write(IntToHex(ABytes[I], 2));
  end;
  Writeln;
end;

var
  ImagePath: string;
  TargetAddr: UInt64;
  Image: TBytes;
  Req: TOBDJ1939DM14Request;
  Bytes: TBytes;
  ChunkSize: Integer;
  Off: Integer;
begin
  Writeln('29-J1939Flash');
  Writeln('=============');
  Writeln('Heavy-duty memory-access framing demo. NO wire access.');
  Writeln;
  if ParamCount < 3 then
  begin
    Writeln(StdErr,
      'Usage: J1939Flash <source-sa-hex> <target-addr-hex> <image.bin>');
    Halt(2);
  end;

  TargetAddr := StrToInt64('$' + ParamStr(2));
  ImagePath := ParamStr(3);
  Image := TFile.ReadAllBytes(ImagePath);
  Writeln(Format('Source SA          : 0x%s', [ParamStr(1)]));
  Writeln(Format('Target address     : 0x%x', [TargetAddr]));
  Writeln(Format('Image size         : %d bytes', [Length(Image)]));

  // 1. DM14 RequestForMemoryAccess (write).
  Req := Default(TOBDJ1939DM14Request);
  Req.Length_ := Word(Length(Image));
  Req.Command := DM14_CMD_WRITE;
  Req.Pointer_ := UInt32(TargetAddr);
  Bytes := TOBDJ1939MemoryAccess.EncodeDM14(Req);
  Writeln;
  Writeln('Step 1 — DM14 RequestForMemoryAccess (write)');
  DumpHex('  PGN 0xD900', Bytes);
  Writeln('  → host sends this through the J1939 transport;');
  Writeln('    waits for DM15 with status = PROCEED');

  // 2. DM16 chunks (the host's CAN driver routes each).
  Writeln;
  Writeln('Step 2 — DM16 BinaryDataTransfer chunks');
  Off := 0;
  ChunkSize := 7; // single-frame J1939 limit; TP/ETP for larger
  while Off < Length(Image) do
  begin
    var Chunk: TBytes;
    var Take := Length(Image) - Off;
    if Take > ChunkSize then Take := ChunkSize;
    SetLength(Chunk, Take);
    Move(Image[Off], Chunk[0], Take);
    Bytes := TOBDJ1939MemoryAccess.EncodeDM16(Chunk);
    DumpHex(Format('  off=%4d', [Off]), Bytes);
    Inc(Off, Take);
  end;

  // 3. DM14 OperationCompleted.
  Req := Default(TOBDJ1939DM14Request);
  Req.Command := DM14_CMD_OPERATION_COMPLETED;
  Bytes := TOBDJ1939MemoryAccess.EncodeDM14(Req);
  Writeln;
  Writeln('Step 3 — DM14 OperationCompleted');
  DumpHex('  PGN 0xD900', Bytes);

  Writeln;
  Writeln('Done — template only. Host integrates the J1939 transport.');
end.
