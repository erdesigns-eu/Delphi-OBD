unit ERD.Components.LanguageServices;
interface
uses System.SysUtils;
type
  TERDLanguageRequestKind = (lrCompletion, lrHover, lrDefinition);
  TERDLanguageRequest = record
    Kind: TERDLanguageRequestKind;
    ID, Version: Int64;
    DocumentUri, Text: UnicodeString;
    Caret, Anchor, Position: Integer;
    TriggerCharacter: WideChar; // #0 for manual invocation, dot for member access.
  end;
  TERDCompletionItem = record
    LabelText, InsertText, Detail, Documentation: UnicodeString;
    Start, Count: Integer; // Zero-based UTF-16 replacement range; plain text insertion.
  end;
  TERDDefinitionTarget = record
    Found: Boolean;
    DocumentUri: UnicodeString; // Empty URI means the current document.
    Start, Count: Integer;
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
end.
