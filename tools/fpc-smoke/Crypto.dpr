program Crypto;
{$MODE DELPHI}
uses cthreads, cwstring, SysUtils, System.IOUtils, ERD.Signature,
  ERD.Signature.HSM, ERD.Signature.OpenSSL;
var HSM: TOBDSignatureHSM; Backend, Ref: IOBDSignatureVerifier;
  Args: TOBDSignatureVerifyArgs;
procedure Check(Value: Boolean; const Text: string);
begin if not Value then raise Exception.Create(Text) end;
begin
  HSM := TOBDSignatureHSM.Create; Ref := HSM;
  HSM.LibraryPath := ParamStr(2); // An existing file is not a functional token driver.
  Check(not HSM.IsAvailable, 'Unconfigured HSM claims availability');
  Check(not Ref.Supports(saRSA_PKCS1_SHA256), 'Unconfigured HSM claims RSA support');
  Backend := TOBDSignatureOpenSSL.Create;
  HSM.Driver := Backend;
  Check(HSM.IsAvailable and Ref.Supports(saRSA_PKCS1_SHA256), 'Configured verifier capability missing');
  Args := Default(TOBDSignatureVerifyArgs); Args.Algorithm := saRSA_PKCS1_SHA256;
  Args.Message := TFile.ReadAllBytes(ParamStr(1));
  Args.PublicKey := TFile.ReadAllBytes(ParamStr(2));
  Args.Signature := TFile.ReadAllBytes(ParamStr(3));
  Check(Ref.Verify(Args), 'Real RSA signature rejected');
  Args.Message[0] := Args.Message[0] xor 1;
  Check(not Ref.Verify(Args), 'Tampered message accepted');
  HSM.Driver := nil;
  Check(not Ref.Supports(saRSA_PKCS1_SHA256), 'Removed driver retains capability');
  Ref := nil; Backend := nil;
  Writeln('6 signature driver checks passed (native OpenSSL RSA; no hardware token).');
end.
