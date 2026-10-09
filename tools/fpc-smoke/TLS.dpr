program TLSRegression;
{$MODE DELPHI}
uses cthreads, cwstring, SysUtils, ERD.Protocol.DoIP.TLS.OpenSSL;
var Options: TOBDDoIPTLSOptions; Transport: TOBDDoIPOpenSSLTransport; Accepted: Boolean;
begin
  Options := DefaultDoIPTLSOptions;
  Options.VerifyMode := TOBDDoIPTLSVerifyMode(StrToInt(ParamStr(2)));
  if ParamStr(4) <> '-' then Options.CAFile := ParamStr(4);
  Transport := TOBDDoIPOpenSSLTransport.Create(Options);
  Accepted := False;
  try
    try Transport.Connect(ParamStr(1), StrToInt(ParamStr(3)), 2000); Accepted := True;
    except on E: Exception do Writeln('Handshake rejected: ', E.Message) end;
    if Accepted <> (ParamStr(5) = 'accept') then
      raise Exception.Create('Unexpected certificate verification result');
  finally Transport.Free end;
end.
