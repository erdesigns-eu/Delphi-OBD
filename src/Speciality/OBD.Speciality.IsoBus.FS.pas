//------------------------------------------------------------------------------
//  OBD.Speciality.IsoBus.FS
//
//  TOBDIsoBusFS — IsoBus File Server framing helpers
//  (ISO 11783-13). The File Server is the on-bus filesystem
//  endpoint that receives recorded process data, log files and
//  configuration blobs from agricultural implements.
//
//  v1 ships the request frames every host needs:
//
//    - Get_File_Server_Properties          (function 0x00)
//    - Open_File                           (0x20)
//    - Seek_File                           (0x21)
//    - Read_File                           (0x22)
//    - Write_File                          (0x23)
//    - Close_File                          (0x24)
//    - Get_Current_Directory               (0x10)
//    - Change_Current_Directory            (0x11)
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 11783-13:2011 (File Server)
//
//  History     :
//    2026-05-09  ERD  Follow-up.
//------------------------------------------------------------------------------

unit OBD.Speciality.IsoBus.FS;

interface

uses
  System.SysUtils,
  OBD.Types;

const
  ISOBUS_FS_PGN_TO_FS  = $00AA00;
  ISOBUS_FS_PGN_FROM_FS= $00AB00;

  // ---- File Server function codes ----
  FS_FN_GET_PROPERTIES = $00;
  FS_FN_GET_CWD        = $10;
  FS_FN_CHANGE_CWD     = $11;
  FS_FN_OPEN_FILE      = $20;
  FS_FN_SEEK_FILE      = $21;
  FS_FN_READ_FILE      = $22;
  FS_FN_WRITE_FILE     = $23;
  FS_FN_CLOSE_FILE     = $24;

  // ---- Open-flags ----
  FS_OPEN_READ_ONLY        = $00;
  FS_OPEN_WRITE_REPLACE    = $01;
  FS_OPEN_WRITE_APPEND     = $02;
  FS_OPEN_READ_WRITE       = $03;
  FS_OPEN_TARGET_FILE_BIT  = $00;
  FS_OPEN_TARGET_DIR_BIT   = $40;

type
  /// <summary>IsoBus File Server framing helpers (stateless).</summary>
  TOBDIsoBusFS = class
  public
    /// <summary>Builds a Get_File_Server_Properties frame
    /// (request).</summary>
    class function BuildGetProperties(ATransactionID: Byte): TBytes; static;
    /// <summary>Builds an Open_File frame. <c>APath</c> is UTF-8;
    /// the host TP-fragments if it spans multiple frames.</summary>
    class function BuildOpenFile(ATransactionID, AOpenFlags: Byte;
      const APath: string): TBytes; static;
    /// <summary>Builds a Read_File frame.</summary>
    class function BuildReadFile(ATransactionID: Byte; AHandle: Byte;
      ACount: Word): TBytes; static;
    /// <summary>Builds a Write_File frame. The host concatenates
    /// the returned header with the data bytes.</summary>
    class function BuildWriteFile(ATransactionID, AHandle: Byte;
      const AData: TBytes): TBytes; static;
    /// <summary>Builds a Seek_File frame. <c>AOrigin</c>: 0 =
    /// from-start, 1 = from-current, 2 = from-end.</summary>
    class function BuildSeekFile(ATransactionID, AHandle, AOrigin: Byte;
      AOffset: Int32): TBytes; static;
    /// <summary>Builds a Close_File frame.</summary>
    class function BuildCloseFile(ATransactionID, AHandle: Byte): TBytes; static;
    /// <summary>Builds a Get_Current_Directory frame.</summary>
    class function BuildGetCWD(ATransactionID: Byte): TBytes; static;
    /// <summary>Builds a Change_Current_Directory frame.</summary>
    class function BuildChangeCWD(ATransactionID: Byte;
      const APath: string): TBytes; static;
  end;

implementation

class function TOBDIsoBusFS.BuildGetProperties(ATransactionID: Byte): TBytes;
var
  I: Integer;
begin
  SetLength(Result, 8);
  Result[0] := FS_FN_GET_PROPERTIES;
  Result[1] := ATransactionID;
  for I := 2 to 7 do Result[I] := $FF;
end;

class function TOBDIsoBusFS.BuildOpenFile(ATransactionID, AOpenFlags: Byte;
  const APath: string): TBytes;
var
  PathBytes: TBytes;
  Len, I: Integer;
begin
  PathBytes := TEncoding.UTF8.GetBytes(APath);
  Len := Length(PathBytes);
  SetLength(Result, 5 + Len);
  Result[0] := FS_FN_OPEN_FILE;
  Result[1] := ATransactionID;
  Result[2] := AOpenFlags;
  Result[3] := Byte(Len and $FF);
  Result[4] := Byte((Len shr 8) and $FF);
  for I := 0 to Len - 1 do Result[5 + I] := PathBytes[I];
end;

class function TOBDIsoBusFS.BuildReadFile(ATransactionID, AHandle: Byte;
  ACount: Word): TBytes;
var
  I: Integer;
begin
  SetLength(Result, 8);
  Result[0] := FS_FN_READ_FILE;
  Result[1] := ATransactionID;
  Result[2] := AHandle;
  Result[3] := Byte(ACount and $FF);
  Result[4] := Byte((ACount shr 8) and $FF);
  for I := 5 to 7 do Result[I] := $FF;
end;

class function TOBDIsoBusFS.BuildWriteFile(ATransactionID, AHandle: Byte;
  const AData: TBytes): TBytes;
var
  Len, I: Integer;
begin
  Len := Length(AData);
  SetLength(Result, 5 + Len);
  Result[0] := FS_FN_WRITE_FILE;
  Result[1] := ATransactionID;
  Result[2] := AHandle;
  Result[3] := Byte(Len and $FF);
  Result[4] := Byte((Len shr 8) and $FF);
  for I := 0 to Len - 1 do Result[5 + I] := AData[I];
end;

class function TOBDIsoBusFS.BuildSeekFile(ATransactionID, AHandle,
  AOrigin: Byte; AOffset: Int32): TBytes;
var
  V: Cardinal;
begin
  SetLength(Result, 8);
  Result[0] := FS_FN_SEEK_FILE;
  Result[1] := ATransactionID;
  Result[2] := AHandle;
  Result[3] := AOrigin;
  V := Cardinal(AOffset);
  Result[4] := Byte(V and $FF);
  Result[5] := Byte((V shr 8) and $FF);
  Result[6] := Byte((V shr 16) and $FF);
  Result[7] := Byte((V shr 24) and $FF);
end;

class function TOBDIsoBusFS.BuildCloseFile(ATransactionID,
  AHandle: Byte): TBytes;
var
  I: Integer;
begin
  SetLength(Result, 8);
  Result[0] := FS_FN_CLOSE_FILE;
  Result[1] := ATransactionID;
  Result[2] := AHandle;
  for I := 3 to 7 do Result[I] := $FF;
end;

class function TOBDIsoBusFS.BuildGetCWD(ATransactionID: Byte): TBytes;
var
  I: Integer;
begin
  SetLength(Result, 8);
  Result[0] := FS_FN_GET_CWD;
  Result[1] := ATransactionID;
  for I := 2 to 7 do Result[I] := $FF;
end;

class function TOBDIsoBusFS.BuildChangeCWD(ATransactionID: Byte;
  const APath: string): TBytes;
var
  PathBytes: TBytes;
  Len, I: Integer;
begin
  PathBytes := TEncoding.UTF8.GetBytes(APath);
  Len := Length(PathBytes);
  SetLength(Result, 4 + Len);
  Result[0] := FS_FN_CHANGE_CWD;
  Result[1] := ATransactionID;
  Result[2] := Byte(Len and $FF);
  Result[3] := Byte((Len shr 8) and $FF);
  for I := 0 to Len - 1 do Result[4 + I] := PathBytes[I];
end;

end.
