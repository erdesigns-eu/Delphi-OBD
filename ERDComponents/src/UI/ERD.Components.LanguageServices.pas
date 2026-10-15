unit ERD.Components.LanguageServices;
interface
uses System.SysUtils;
type
  TERDLanguageRequestKind = (lrCompletion, lrHover, lrDefinition, lrSignatureHelp);
  TERDLanguageRequest = record
    Kind: TERDLanguageRequestKind;
    ID, Version: Int64;
    DocumentUri, Text: UnicodeString;
    Caret, Anchor, Position: Integer;
    TriggerCharacter: WideChar; // #0 for manual invocation, dot for member access.
  end;
  TERDCompletionItem = record
    LabelText, InsertText, Detail, Documentation: UnicodeString;
    Kind: Integer; // LSP CompletionItemKind; zero means unspecified.
    Start, Count: Integer; // Zero-based UTF-16 replacement range; plain text insertion.
  end;
  TERDDefinitionTarget = record
    Found: Boolean;
    DocumentUri: UnicodeString; // Empty URI means the current document.
    Start, Count: Integer;
  end;
  TERDSignatureParameter = record
    LabelText, Documentation: UnicodeString;
    Start, Count: Integer; // UTF-16 range within the signature label; -1 if absent.
  end;
  TERDSignature = record
    LabelText, Documentation: UnicodeString;
    Parameters: TArray<TERDSignatureParameter>;
    ActiveParameter: Integer; // -1 uses the help-level active parameter.
  end;
  TERDSignatureHelp = record
    Signatures: TArray<TERDSignature>;
    ActiveSignature, ActiveParameter: Integer;
  end;
  // Optional response extension preserves existing provider/response interface GUIDs.
  IERDSignatureHelpResponses = interface
    ['{71A04CC3-A05E-44DA-9225-8F78D233A26E}']
    function SignatureHelp(const Request: TERDLanguageRequest; const Help: TERDSignatureHelp): Boolean;
  end;
  IERDLanguageResponses = interface
    ['{E9C2C509-2931-498B-AD69-A058386B41C2}']
    function Completion(const Request: TERDLanguageRequest; const Items: TArray<TERDCompletionItem>): Boolean;
    function Hover(const Request: TERDLanguageRequest; const Text: UnicodeString): Boolean;
    function Definition(const Request: TERDLanguageRequest; const Target: TERDDefinitionTarget): Boolean;
  end;
  IERDCodeLanguageService = interface
    ['{23D6CA65-A0B7-43D0-BEF6-54D36DF192C5}']
    procedure Request(const Snapshot: TERDLanguageRequest; const Responses: IERDLanguageResponses);
    procedure Cancel(RequestID: Int64);
  end;
  // Optional document notifications, independent of caret-triggered queries.
  IERDCodeDocumentService = interface
    ['{A2F64377-887F-45B5-8189-C9406A9E8570}']
    procedure DocumentChanged(const Uri, Text: UnicodeString; Version: Int64);
  end;
function ERDCompletionKindText(Kind: Integer): string;
function ERDCopySignatureHelp(const Value: TERDSignatureHelp): TERDSignatureHelp;
implementation
function ERDCopySignatureHelp(const Value: TERDSignatureHelp): TERDSignatureHelp;
var I: Integer;
begin
  Result := Value; Result.Signatures := Copy(Value.Signatures);
  for I := 0 to High(Result.Signatures) do Result.Signatures[I].Parameters := Copy(Value.Signatures[I].Parameters);
end;
function ERDCompletionKindText(Kind: Integer): string;
begin
  case Kind of
    1: Result := 'text'; 2: Result := 'method'; 3: Result := 'function';
    4: Result := 'constructor'; 5: Result := 'field'; 6: Result := 'variable';
    7: Result := 'class'; 8: Result := 'interface'; 9: Result := 'module';
    10: Result := 'property'; 11: Result := 'unit'; 12: Result := 'value';
    13: Result := 'enum'; 14: Result := 'keyword'; 15: Result := 'snippet';
    16: Result := 'colour'; 17: Result := 'file'; 18: Result := 'reference';
    19: Result := 'folder'; 20: Result := 'enum member'; 21: Result := 'constant';
    22: Result := 'struct'; 23: Result := 'event'; 24: Result := 'operator';
    25: Result := 'type parameter';
  else Result := 'symbol'; end;
end;
end.
