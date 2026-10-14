unit ERD.Components.Syntax;
{$B-}
interface
uses System.SysUtils, System.Generics.Collections;
type
  TERDCodeLanguage = (clPlainText, clPascal, clJSON, clTypeScript);
  TERDSyntaxKind = (skText, skKeyword, skIdentifier, skNumber, skString,
    skComment, skSymbol, skDirective, skInterfaceKeyword, skClassKeyword, skModuleKeyword);
  TERDSyntaxSpan = record
    Start, Count: Integer;
    Kind: TERDSyntaxKind;
  end;
  TERDSyntaxFold = record
    Start, Finish: Integer;
  end;
  TERDSyntaxAnalysis = record
    Spans: TArray<TERDSyntaxSpan>;
    Folds: TArray<TERDSyntaxFold>;
  end;
  IERDSyntaxProvider = interface
    ['{5B779F2D-C1F2-462C-84FB-8015095E600D}']
    function Analyze(const Text: UnicodeString): TERDSyntaxAnalysis;
  end;
  TERDBasicSyntaxProvider = class(TInterfacedObject, IERDSyntaxProvider)
  private
    FLanguage: TERDCodeLanguage;
  public
    constructor Create(Language: TERDCodeLanguage);
    function Analyze(const Text: UnicodeString): TERDSyntaxAnalysis;
  end;
function ERDTypeScriptKeywords: TArray<string>;
implementation
const
  PASCAL_KEYWORDS = '|and|array|as|asm|begin|case|class|const|constructor|destructor|'+
    'div|do|downto|else|end|except|exports|file|finalization|finally|for|function|'+
    'generic|goto|if|implementation|in|inherited|initialization|inline|interface|'+
    'is|label|library|mod|nil|not|object|of|operator|or|out|packed|private|'+
    'procedure|program|property|protected|public|published|raise|record|repeat|'+
    'resourcestring|set|shl|shr|specialize|string|then|threadvar|to|try|type|'+
    'unit|until|uses|var|virtual|while|with|xor|true|false|';
  TYPESCRIPT_KEYWORDS = '|abstract|accessor|any|as|assert|asserts|async|await|bigint|boolean|break|'+
    'case|catch|class|const|constructor|continue|debugger|declare|default|delete|'+
    'do|else|enum|export|extends|false|finally|for|from|function|get|if|implements|'+
    'global|import|in|infer|instanceof|interface|intrinsic|is|keyof|let|module|namespace|never|new|'+
    'null|number|object|of|out|override|package|private|protected|public|readonly|require|'+
    'return|satisfies|set|static|string|super|switch|symbol|this|throw|true|try|'+
    'type|typeof|undefined|unique|unknown|using|var|void|while|with|yield|';
function ERDTypeScriptKeywords: TArray<string>;
begin Result := Copy(TYPESCRIPT_KEYWORDS, 2, Length(TYPESCRIPT_KEYWORDS) - 2).Split(['|']); end;
constructor TERDBasicSyntaxProvider.Create(Language: TERDCodeLanguage);
begin inherited Create; FLanguage := Language; end;
function TERDBasicSyntaxProvider.Analyze(const Text: UnicodeString): TERDSyntaxAnalysis;
var Spans: TList<TERDSyntaxSpan>; Folds: TList<TERDSyntaxFold>;
    Stack: TList<TERDSyntaxFold>; Span: TERDSyntaxSpan; Fold: TERDSyntaxFold;
    I, Start, N: Integer; C: WideChar; Word: UnicodeString;
    Kind: TERDSyntaxKind; ExpectExpression, RegexClass: Boolean;
  function Identifier(C: WideChar): Boolean;
  begin Result := (C = '_') or ((FLanguage = clTypeScript) and (C = '$')) or ((C >= 'a') and (C <= 'z')) or
    ((C >= 'A') and (C <= 'Z')) or (Ord(C) >= $80); end;
  function Digit(C: WideChar): Boolean;
  begin Result := (C >= '0') and (C <= '9'); end;
  procedure AddFold(AStart, AFinish: Integer);
  begin
    if Pos(#10, Copy(Text, AStart + 1, AFinish - AStart)) = 0 then Exit;
    Fold.Start := AStart; Fold.Finish := AFinish; Folds.Add(Fold);
  end;
  procedure OpenFold(APosition: Integer; AKind: Integer);
  var Entry: TERDSyntaxFold;
  begin Entry.Start := APosition; Entry.Finish := AKind; Stack.Add(Entry); end;
  procedure CloseFold(AFinish, AKind: Integer);
  var Entry: TERDSyntaxFold;
  begin
    if (Stack.Count = 0) or (Stack.Last.Finish <> AKind) then Exit;
    Entry := Stack.Last; Stack.Delete(Stack.Count - 1); AddFold(Entry.Start, AFinish);
  end;
begin
  Result.Spans := nil; Result.Folds := nil;
  if FLanguage = clPlainText then Exit;
  Spans := TList<TERDSyntaxSpan>.Create; Folds := TList<TERDSyntaxFold>.Create;
  Stack := TList<TERDSyntaxFold>.Create;
  try
    I := 0; N := Length(Text); ExpectExpression := True;
    while I < N do
    begin
      Start := I; C := Text[I + 1]; Kind := skSymbol;
      if C <= ' ' then begin Inc(I); Continue; end;
      if (FLanguage in [clPascal, clTypeScript]) and (C = '/') and (I + 1 < N) and (Text[I + 2] = '/') then
      begin
        Kind := skComment; Inc(I, 2);
        while (I < N) and not CharInSet(Text[I + 1], [#13,#10]) do Inc(I);
      end
      else if (FLanguage = clTypeScript) and (C = '/') and
        (I + 1 < N) and (Text[I + 2] = '*') then
      begin
        Kind := skComment; Inc(I, 2);
        while (I + 1 < N) and not ((Text[I + 1] = '*') and (Text[I + 2] = '/')) do Inc(I);
        if I + 1 < N then Inc(I, 2) else I := N; AddFold(Start, I);
      end
      else if (FLanguage = clTypeScript) and (C = '/') and ExpectExpression then
      begin
        Kind := skString; RegexClass := False; Inc(I);
        while (I < N) and not CharInSet(Text[I + 1], [#13,#10]) do
        begin
          C := Text[I + 1]; Inc(I);
          if C = #92 then begin if I < N then Inc(I); Continue; end;
          if C = '[' then RegexClass := True
          else if C = ']' then RegexClass := False
          else if (C = '/') and not RegexClass then Break;
        end;
        while (I < N) and CharInSet(Text[I + 1], ['a'..'z','A'..'Z']) do Inc(I);
      end
      else if (FLanguage = clPascal) and (C = '{') then
      begin
        Kind := skComment; if (I + 1 < N) and (Text[I + 2] = '$') then Kind := skDirective;
        Inc(I); while (I < N) and (Text[I + 1] <> '}') do Inc(I);
        if I < N then Inc(I); AddFold(Start, I);
      end
      else if (FLanguage = clPascal) and (C = '(') and (I + 1 < N) and (Text[I + 2] = '*') then
      begin
        Kind := skComment; Inc(I, 2);
        while (I + 1 < N) and not ((Text[I + 1] = '*') and (Text[I + 2] = ')')) do Inc(I);
        if I + 1 < N then Inc(I, 2) else I := N; AddFold(Start, I);
      end
      else if ((FLanguage = clPascal) and (C = #39)) or ((FLanguage = clJSON) and (C = '"')) or
        ((FLanguage = clTypeScript) and CharInSet(C, [#39, '"', '`'])) then
      begin
        Kind := skString; Inc(I);
        while I < N do
        begin
          if (FLanguage in [clJSON, clTypeScript]) and (Text[I + 1] = #92) then
          begin Inc(I); if I < N then Inc(I); Continue; end;
          if Text[I + 1] = C then
          begin
            Inc(I);
            if (FLanguage = clPascal) and (I < N) and (Text[I + 1] = C) then
            begin Inc(I); Continue; end;
            Break;
          end;
          if (FLanguage <> clPascal) and (C <> '`') and
            CharInSet(Text[I + 1], [#13,#10]) then Break;
          Inc(I);
        end;
        if C = '`' then AddFold(Start, I);
      end
      else if Digit(C) or ((FLanguage = clPascal) and CharInSet(C, ['$', '#'])) or
        ((FLanguage = clJSON) and (C = '-') and (I + 1 < N) and Digit(Text[I + 2])) then
      begin
        Kind := skNumber; Inc(I);
        if (C = '$') or ((C = '0') and (FLanguage = clTypeScript) and
          (I < N) and CharInSet(Text[I + 1], ['x','X'])) then
        begin
          if C = '0' then Inc(I);
          while (I < N) and CharInSet(Text[I + 1], ['0'..'9','a'..'f','A'..'F','_']) do Inc(I);
        end
        else
        begin
          if (C = '0') and (FLanguage = clTypeScript) and (I < N) and
            CharInSet(Text[I + 1], ['b','B','o','O']) then Inc(I);
          while (I < N) and (Digit(Text[I + 1]) or
            ((FLanguage = clTypeScript) and (Text[I + 1] = '_'))) do Inc(I);
          if (I + 1 < N) and (Text[I + 1] = '.') and Digit(Text[I + 2]) then
          begin Inc(I); while (I < N) and Digit(Text[I + 1]) do Inc(I); end;
          if (I < N) and CharInSet(Text[I + 1], ['e','E']) then
          begin
            Inc(I); if (I < N) and CharInSet(Text[I + 1], ['+','-']) then Inc(I);
            while (I < N) and Digit(Text[I + 1]) do Inc(I);
          end;
          if (FLanguage = clTypeScript) and (I < N) and (Text[I + 1] = 'n') then Inc(I);
        end;
      end
      else if Identifier(C) then
      begin
        Inc(I); while (I < N) and (Identifier(Text[I + 1]) or Digit(Text[I + 1])) do Inc(I);
        Word := Copy(Text, Start + 1, I - Start);
        if FLanguage = clPascal then Word := LowerCase(Word); Kind := skIdentifier;
        if ((FLanguage = clPascal) and (Pos('|' + Word + '|', PASCAL_KEYWORDS) > 0)) or
          ((FLanguage = clTypeScript) and (Pos('|' + Word + '|', TYPESCRIPT_KEYWORDS) > 0)) or
          ((FLanguage = clJSON) and ((Word = 'true') or (Word = 'false') or (Word = 'null'))) then Kind := skKeyword;
        if (FLanguage = clTypeScript) and (Kind = skKeyword) then
        begin
          if Word = 'interface' then Kind := skInterfaceKeyword
          else if Word = 'class' then Kind := skClassKeyword
          else if (Word = 'export') or (Word = 'import') or (Word = 'from') then Kind := skModuleKeyword;
        end;
        if FLanguage = clPascal then
        begin
          // Lightweight structural folding; exact semantic folds can be supplied by a provider.
          if (Word = 'begin') or (Word = 'try') or (Word = 'case') or (Word = 'asm') then OpenFold(Start, 0)
          else if Word = 'repeat' then OpenFold(Start, 1)
          else if Word = 'end' then CloseFold(I, 0)
          else if Word = 'until' then CloseFold(I, 1);
        end;
      end
      else
      begin
        Inc(I);
        if FLanguage in [clJSON, clTypeScript] then
          case C of
            '{': OpenFold(Start, 2);
            '[': OpenFold(Start, 3);
            '}': CloseFold(I, 2);
            ']': CloseFold(I, 3);
          end;
      end;
      if FLanguage = clTypeScript then
        case Kind of
          skIdentifier, skNumber, skString: ExpectExpression := False;
          skKeyword, skInterfaceKeyword, skClassKeyword, skModuleKeyword: ExpectExpression := (Word = 'return') or (Word = 'throw') or
            (Word = 'case') or (Word = 'yield') or (Word = 'await') or
            (Word = 'typeof') or (Word = 'void') or (Word = 'delete') or (Word = 'new');
          skSymbol: ExpectExpression := not CharInSet(Text[Start + 1], [')',']','}']);
        end;
      Span.Start := Start; Span.Count := I - Start; Span.Kind := Kind; Spans.Add(Span);
    end;
    Result.Spans := Spans.ToArray; Result.Folds := Folds.ToArray;
  finally Stack.Free; Folds.Free; Spans.Free; end;
end;
end.
