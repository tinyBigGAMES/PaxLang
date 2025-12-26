{===============================================================================
  Pax™ Programming Language.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://paxlang.org

  See LICENSE for license information
===============================================================================}

unit Pax.Lexer;

{$I Pax.Defines.inc}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Pax.Utils,
  Pax.Errors,
  Pax.Resources;

type

  { TTokenKind }
  TTokenKind = (
    // Literals
    tkInteger,
    tkFloat,
    tkString,
    tkWideString,
    tkChar,
    tkWideChar,

    // Identifier
    tkIdentifier,

    // Module keywords
    tkModule,
    tkExe,
    tkDll,
    tkLib,
    tkImport,
    tkPublic,
    tkExternal,

    // Declaration keywords
    tkConst,
    tkType,
    tkVar,
    tkRoutine,

    // Type keywords
    tkRecord,
    tkArray,
    tkPointer,
    tkSet,
    tkPacked,
    tkUnion,
    tkAlign,
    tkAddress,
    tkTo,
    tkOf,

    // Block keywords
    tkBegin,
    tkEnd,
    tkTest,

    // Control flow keywords
    tkIf,
    tkThen,
    tkElse,
    tkWhile,
    tkDo,
    tkFor,
    tkDownto,
    tkRepeat,
    tkUntil,
    tkCase,
    tkReturn,

    // Built-in statements
    tkNew,
    tkDispose,
    tkSetLength,
    tkLen,
    tkSizeOf,

    // GC intrinsics
    tkGcCollect,
    tkGcHeapSize,
    tkGcUsedSize,
    tkGcCollectCount,
    tkGcDump,

    // Command line intrinsics
    tkParamCount,
    tkParamStr,

    // Logical operators (keywords)
    tkAnd,
    tkOr,
    tkNot,
    tkDiv,
    tkMod,
    tkIn,

    // Boolean literals
    tkTrue,
    tkFalse,
    tkNil,

    // Built-in type keywords
    tkInt8,
    tkInt16,
    tkInt32,
    tkInt64,
    tkUInt8,
    tkUInt16,
    tkUInt32,
    tkUInt64,
    tkFloat32,
    tkFloat64,
    tkBoolean,
    tkCharType,
    tkUCharType,
    tkWCharType,
    tkUWCharType,
    tkStringType,
    tkWStringType,

    // Operators
    tkPlus,
    tkMinus,
    tkStar,
    tkSlash,
    tkEq,
    tkNe,
    tkLt,
    tkGt,
    tkLe,
    tkGe,
    tkAssign,
    tkPlusAssign,
    tkMinusAssign,
    tkStarAssign,
    tkSlashAssign,

    // Punctuation
    tkColon,
    tkSemicolon,
    tkComma,
    tkDot,
    tkDotDot,
    tkEllipsis,
    tkCaret,
    tkLParen,
    tkRParen,
    tkLBracket,
    tkRBracket,
    tkLBrace,
    tkRBrace,

    // Directives
    tkDirective,

    // Special
    tkEOF,
    tkError
  );

  { TToken }
  TToken = record
    Kind: TTokenKind;
    Lexeme: string;
    Range: TSourceRange;
    IntValue: Int64;
    FloatValue: Double;

    function IsKeyword(): Boolean;
    function IsLiteral(): Boolean;
    function IsOperator(): Boolean;
  end;

  { TPaxLexer }
  TPaxLexer = class(TBaseObject)
  private
    FSource: string;
    FFilename: string;
    FPos: Integer;
    FLine: Integer;
    FColumn: Integer;
    FTokenStart: Integer;
    FTokenStartLine: Integer;
    FTokenStartColumn: Integer;
    FTokens: TList<TToken>;
    FErrors: TErrors;
    FKeywords: TDictionary<string, TTokenKind>;

    // Stats
    FKeywordCount: Integer;
    FIdentifierCount: Integer;
    FLiteralCount: Integer;
    FOperatorCount: Integer;

    procedure InitKeywords();

    // Character helpers
    function IsAtEnd(): Boolean;
    function Peek(): Char;
    function PeekNext(): Char;
    function Advance(): Char;
    function Match(const AChar: Char): Boolean;

    // Character classification
    function IsDigit(const AChar: Char): Boolean;
    function IsHexDigit(const AChar: Char): Boolean;
    function IsAlpha(const AChar: Char): Boolean;
    function IsAlphaNumeric(const AChar: Char): Boolean;

    // Position helpers
    function MakeRange(): TSourceRange;
    procedure MarkTokenStart();
    procedure SkipWhitespace();
    function SkipLineComment(): Boolean;
    function SkipBlockComment(): Boolean;

    // Token scanners
    procedure ScanToken();
    procedure ScanIdentifierOrKeyword();
    procedure ScanNumber();
    procedure ScanHexNumber();
    procedure ScanString();
    procedure ScanRawString();
    procedure ScanWideStringOrChar();
    procedure ScanRawWideString();
    procedure ScanDirective();

    // Token creation
    procedure AddToken(const AKind: TTokenKind);
    procedure AddTokenWithLexeme(const AKind: TTokenKind; const ALexeme: string);
    procedure AddTokenInt(const AValue: Int64);
    procedure AddTokenFloat(const AValue: Double);
    procedure AddError(const AMessage: string); overload;
    procedure AddError(const AMessage: string; const AArgs: array of const); overload;

  public
    constructor Create(); override;
    destructor Destroy(); override;

    procedure SetErrors(const AErrors: TErrors);
    procedure Tokenize(const ASource: string; const AFilename: string);
    procedure Clear();

    function GetToken(const AIndex: Integer): TToken;
    function GetTokenCount(): Integer;

    // Stats properties
    property KeywordCount: Integer read FKeywordCount;
    property IdentifierCount: Integer read FIdentifierCount;
    property LiteralCount: Integer read FLiteralCount;
    property OperatorCount: Integer read FOperatorCount;

    property Tokens: TList<TToken> read FTokens;
  end;

function TokenKindToString(const AKind: TTokenKind): string;

implementation

const
  CHAR_NULL = #0;
  CHAR_TAB = #9;
  CHAR_LF = #10;
  CHAR_CR = #13;
  CHAR_SPACE = ' ';

function TokenKindToString(const AKind: TTokenKind): string;
begin
  case AKind of
    tkInteger:     Result := 'integer';
    tkFloat:       Result := 'float';
    tkString:      Result := 'string';
    tkWideString:  Result := 'wide string';
    tkChar:        Result := 'char';
    tkWideChar:    Result := 'wide char';
    tkIdentifier:  Result := 'identifier';
    tkModule:      Result := '''module''';
    tkExe:         Result := '''exe''';
    tkDll:         Result := '''dll''';
    tkLib:         Result := '''lib''';
    tkImport:      Result := '''import''';
    tkPublic:      Result := '''public''';
    tkExternal:    Result := '''external''';
    tkConst:       Result := '''const''';
    tkType:        Result := '''type''';
    tkVar:         Result := '''var''';
    tkRoutine:     Result := '''routine''';
    tkRecord:      Result := '''record''';
    tkArray:       Result := '''array''';
    tkPointer:     Result := '''pointer''';
    tkSet:         Result := '''set''';
    tkPacked:      Result := '''packed''';
    tkUnion:       Result := '''union''';
    tkAlign:       Result := '''align''';
    tkAddress:     Result := '''address''';
    tkTo:          Result := '''to''';
    tkOf:          Result := '''of''';
    tkBegin:       Result := '''begin''';
    tkEnd:         Result := '''end''';
    tkTest:        Result := '''test''';
    tkIf:          Result := '''if''';
    tkThen:        Result := '''then''';
    tkElse:        Result := '''else''';
    tkWhile:       Result := '''while''';
    tkDo:          Result := '''do''';
    tkFor:         Result := '''for''';
    tkDownto:      Result := '''downto''';
    tkRepeat:      Result := '''repeat''';
    tkUntil:       Result := '''until''';
    tkCase:        Result := '''case''';
    tkReturn:      Result := '''return''';
    tkNew:         Result := '''new''';
    tkDispose:     Result := '''dispose''';
    tkSetLength:   Result := '''setlength''';
    tkLen:         Result := '''len''';
    tkSizeOf:      Result := '''sizeof''';
    tkGcCollect:   Result := '''gc_collect''';
    tkGcHeapSize:  Result := '''gc_heapsize''';
    tkGcUsedSize:  Result := '''gc_usedsize''';
    tkGcCollectCount: Result := '''gc_collectcount''';
    tkGcDump:      Result := '''gc_dump''';
    tkParamCount:  Result := '''paramcount''';
    tkParamStr:    Result := '''paramstr''';
    tkAnd:         Result := '''and''';
    tkOr:          Result := '''or''';
    tkNot:         Result := '''not''';
    tkDiv:         Result := '''div''';
    tkMod:         Result := '''mod''';
    tkIn:          Result := '''in''';
    tkTrue:        Result := '''true''';
    tkFalse:       Result := '''false''';
    tkNil:         Result := '''nil''';
    tkInt8:        Result := '''int8''';
    tkInt16:       Result := '''int16''';
    tkInt32:       Result := '''int32''';
    tkInt64:       Result := '''int64''';
    tkUInt8:       Result := '''uint8''';
    tkUInt16:      Result := '''uint16''';
    tkUInt32:      Result := '''uint32''';
    tkUInt64:      Result := '''uint64''';
    tkFloat32:     Result := '''float32''';
    tkFloat64:     Result := '''float64''';
    tkBoolean:     Result := '''boolean''';
    tkCharType:    Result := '''char''';
    tkUCharType:   Result := '''uchar''';
    tkWCharType:   Result := '''wchar''';
    tkUWCharType:  Result := '''uwchar''';
    tkStringType:  Result := '''string''';
    tkWStringType: Result := '''wstring''';
    tkPlus:        Result := '''+''';
    tkMinus:       Result := '''-''';
    tkStar:        Result := '''*''';
    tkSlash:       Result := '''/''';
    tkEq:          Result := '''=''';
    tkNe:          Result := '''<>''';
    tkLt:          Result := '''<''';
    tkGt:          Result := '''>''';
    tkLe:          Result := '''<=''';
    tkGe:          Result := '''>=''';
    tkAssign:      Result := ''':=''';
    tkPlusAssign:  Result := '''+=''';
    tkMinusAssign: Result := '''-=''';
    tkStarAssign:  Result := '''*=''';
    tkSlashAssign: Result := '''/=''';
    tkColon:       Result := ''':''';
    tkSemicolon:   Result := ''';''';
    tkComma:       Result := ''',''';
    tkDot:         Result := '''.''';
    tkDotDot:      Result := '''..''';
    tkEllipsis:    Result := '''...''';
    tkCaret:       Result := '''^''';
    tkLParen:      Result := '''(''';
    tkRParen:      Result := ''')''';
    tkLBracket:    Result := '''[''';
    tkRBracket:    Result := ''']''';
    tkLBrace:      Result := '''{''';
    tkRBrace:      Result := '''}''';
    tkDirective:   Result := 'directive';
    tkEOF:         Result := 'end of file';
    tkError:       Result := 'error';
  else
    Result := 'unknown';
  end;
end;

{ TToken }

function TToken.IsKeyword(): Boolean;
begin
  Result := Kind in [
    tkModule, tkExe, tkDll, tkLib, tkImport, tkPublic, tkExternal,
    tkConst, tkType, tkVar, tkRoutine,
    tkRecord, tkArray, tkPointer, tkSet, tkPacked, tkUnion, tkAlign, tkAddress, tkTo, tkOf,
    tkBegin, tkEnd, tkTest,
    tkIf, tkThen, tkElse, tkWhile, tkDo, tkFor, tkDownto,
    tkRepeat, tkUntil, tkCase, tkReturn,
    tkNew, tkDispose, tkSetLength, tkLen, tkSizeOf,
    tkGcCollect, tkGcHeapSize, tkGcUsedSize, tkGcCollectCount, tkGcDump,
    tkParamCount, tkParamStr,
    tkAnd, tkOr, tkNot, tkDiv, tkMod, tkIn,
    tkTrue, tkFalse, tkNil,
    tkInt8, tkInt16, tkInt32, tkInt64,
    tkUInt8, tkUInt16, tkUInt32, tkUInt64,
    tkFloat32, tkFloat64, tkBoolean,
    tkCharType, tkUCharType, tkWCharType, tkUWCharType,
    tkStringType, tkWStringType
  ];
end;

function TToken.IsLiteral(): Boolean;
begin
  Result := Kind in [
    tkInteger, tkFloat, tkString, tkWideString, tkChar, tkWideChar,
    tkTrue, tkFalse, tkNil
  ];
end;

function TToken.IsOperator(): Boolean;
begin
  Result := Kind in [
    tkPlus, tkMinus, tkStar, tkSlash,
    tkEq, tkNe, tkLt, tkGt, tkLe, tkGe,
    tkAnd, tkOr, tkNot, tkDiv, tkMod, tkIn
  ];
end;

{ TPaxLexer }

constructor TPaxLexer.Create();
begin
  inherited;

  FTokens := TList<TToken>.Create();
  FKeywords := TDictionary<string, TTokenKind>.Create();
  FErrors := nil;

  InitKeywords();
end;

destructor TPaxLexer.Destroy();
begin
  FKeywords.Free();
  FTokens.Free();

  inherited;
end;

procedure TPaxLexer.InitKeywords();
begin
  // Module keywords
  FKeywords.Add('module', tkModule);
  FKeywords.Add('exe', tkExe);
  FKeywords.Add('dll', tkDll);
  FKeywords.Add('lib', tkLib);
  FKeywords.Add('import', tkImport);
  FKeywords.Add('public', tkPublic);
  FKeywords.Add('external', tkExternal);

  // Declaration keywords
  FKeywords.Add('const', tkConst);
  FKeywords.Add('type', tkType);
  FKeywords.Add('var', tkVar);
  FKeywords.Add('routine', tkRoutine);

  // Type keywords
  FKeywords.Add('record', tkRecord);
  FKeywords.Add('array', tkArray);
  FKeywords.Add('pointer', tkPointer);
  FKeywords.Add('set', tkSet);
  FKeywords.Add('packed', tkPacked);
  FKeywords.Add('union', tkUnion);
  FKeywords.Add('align', tkAlign);
  FKeywords.Add('address', tkAddress);
  FKeywords.Add('to', tkTo);
  FKeywords.Add('of', tkOf);

  // Block keywords
  FKeywords.Add('begin', tkBegin);
  FKeywords.Add('end', tkEnd);
  FKeywords.Add('test', tkTest);

  // Control flow keywords
  FKeywords.Add('if', tkIf);
  FKeywords.Add('then', tkThen);
  FKeywords.Add('else', tkElse);
  FKeywords.Add('while', tkWhile);
  FKeywords.Add('do', tkDo);
  FKeywords.Add('for', tkFor);
  FKeywords.Add('downto', tkDownto);
  FKeywords.Add('repeat', tkRepeat);
  FKeywords.Add('until', tkUntil);
  FKeywords.Add('case', tkCase);
  FKeywords.Add('return', tkReturn);

  // Built-in statements
  FKeywords.Add('new', tkNew);
  FKeywords.Add('dispose', tkDispose);
  FKeywords.Add('setlength', tkSetLength);
  FKeywords.Add('len', tkLen);
  FKeywords.Add('sizeof', tkSizeOf);

  // GC intrinsics
  FKeywords.Add('gc_collect', tkGcCollect);
  FKeywords.Add('gc_heapsize', tkGcHeapSize);
  FKeywords.Add('gc_usedsize', tkGcUsedSize);
  FKeywords.Add('gc_collectcount', tkGcCollectCount);
  FKeywords.Add('gc_dump', tkGcDump);

  // Command line intrinsics
  FKeywords.Add('paramcount', tkParamCount);
  FKeywords.Add('paramstr', tkParamStr);

  // Logical operators
  FKeywords.Add('and', tkAnd);
  FKeywords.Add('or', tkOr);
  FKeywords.Add('not', tkNot);
  FKeywords.Add('div', tkDiv);
  FKeywords.Add('mod', tkMod);
  FKeywords.Add('in', tkIn);

  // Boolean literals
  FKeywords.Add('true', tkTrue);
  FKeywords.Add('false', tkFalse);
  FKeywords.Add('nil', tkNil);

  // Built-in type keywords
  FKeywords.Add('int8', tkInt8);
  FKeywords.Add('int16', tkInt16);
  FKeywords.Add('int32', tkInt32);
  FKeywords.Add('int64', tkInt64);
  FKeywords.Add('uint8', tkUInt8);
  FKeywords.Add('uint16', tkUInt16);
  FKeywords.Add('uint32', tkUInt32);
  FKeywords.Add('uint64', tkUInt64);
  FKeywords.Add('float32', tkFloat32);
  FKeywords.Add('float64', tkFloat64);
  FKeywords.Add('boolean', tkBoolean);
  FKeywords.Add('char', tkCharType);
  FKeywords.Add('uchar', tkUCharType);
  FKeywords.Add('wchar', tkWCharType);
  FKeywords.Add('uwchar', tkUWCharType);
  FKeywords.Add('string', tkStringType);
  FKeywords.Add('wstring', tkWStringType);
end;

function TPaxLexer.IsAtEnd(): Boolean;
begin
  Result := FPos > Length(FSource);
end;

function TPaxLexer.Peek(): Char;
begin
  if IsAtEnd() then
    Result := CHAR_NULL
  else
    Result := FSource[FPos];
end;

function TPaxLexer.PeekNext(): Char;
begin
  if FPos + 1 > Length(FSource) then
    Result := CHAR_NULL
  else
    Result := FSource[FPos + 1];
end;

function TPaxLexer.Advance(): Char;
begin
  Result := FSource[FPos];
  Inc(FPos);

  if Result = CHAR_LF then
  begin
    Inc(FLine);
    FColumn := 1;
  end
  else if Result <> CHAR_CR then
  begin
    Inc(FColumn);
  end;
end;

function TPaxLexer.Match(const AChar: Char): Boolean;
begin
  if IsAtEnd() then
    Exit(False);

  if FSource[FPos] <> AChar then
    Exit(False);

  Advance();
  Result := True;
end;

function TPaxLexer.IsDigit(const AChar: Char): Boolean;
begin
  Result := (AChar >= '0') and (AChar <= '9');
end;

function TPaxLexer.IsHexDigit(const AChar: Char): Boolean;
begin
  Result := IsDigit(AChar) or
            ((AChar >= 'a') and (AChar <= 'f')) or
            ((AChar >= 'A') and (AChar <= 'F'));
end;

function TPaxLexer.IsAlpha(const AChar: Char): Boolean;
begin
  Result := ((AChar >= 'a') and (AChar <= 'z')) or
            ((AChar >= 'A') and (AChar <= 'Z')) or
            (AChar = '_');
end;

function TPaxLexer.IsAlphaNumeric(const AChar: Char): Boolean;
begin
  Result := IsAlpha(AChar) or IsDigit(AChar);
end;

function TPaxLexer.MakeRange(): TSourceRange;
begin
  Result.Filename := FFilename;
  Result.StartLine := FTokenStartLine;
  Result.StartColumn := FTokenStartColumn;
  Result.EndLine := FLine;
  Result.EndColumn := FColumn;
end;

procedure TPaxLexer.MarkTokenStart();
begin
  FTokenStart := FPos;
  FTokenStartLine := FLine;
  FTokenStartColumn := FColumn;
end;

procedure TPaxLexer.SkipWhitespace();
var
  LChar: Char;
begin
  while not IsAtEnd() do
  begin
    LChar := Peek();

    if (LChar = CHAR_SPACE) or (LChar = CHAR_TAB) or
       (LChar = CHAR_CR) or (LChar = CHAR_LF) then
    begin
      Advance();
    end
    else if (LChar = '/') and (PeekNext() = '/') then
    begin
      if not SkipLineComment() then
        Exit;
    end
    else if (LChar = '(') and (PeekNext() = '*') then
    begin
      if not SkipBlockComment() then
        Exit;
    end
    else
    begin
      Exit;
    end;
  end;
end;

function TPaxLexer.SkipLineComment(): Boolean;
begin
  // Skip //
  Advance();
  Advance();

  while not IsAtEnd() and (Peek() <> CHAR_LF) do
    Advance();

  Result := True;
end;

function TPaxLexer.SkipBlockComment(): Boolean;
var
  LStartLine: Integer;
  LStartCol: Integer;
begin
  LStartLine := FLine;
  LStartCol := FColumn;

  // Skip (*
  Advance();
  Advance();

  while not IsAtEnd() do
  begin
    if (Peek() = '*') and (PeekNext() = ')') then
    begin
      Advance();
      Advance();
      Exit(True);
    end;

    Advance();
  end;

  // Unterminated comment
  if FErrors <> nil then
  begin
    FErrors.Add(
      FFilename,
      LStartLine,
      LStartCol,
      esError,
      'E010',
      RSLexerUnterminatedComment
    );
  end;

  Result := False;
end;

procedure TPaxLexer.ScanToken();
var
  LChar: Char;
begin
  SkipWhitespace();

  if IsAtEnd() then
  begin
    MarkTokenStart();
    AddToken(tkEOF);
    Exit;
  end;

  MarkTokenStart();
  LChar := Advance();

  // Single-character tokens (or compound assignments)
  if LChar = '+' then
  begin
    if Match('=') then
      AddToken(tkPlusAssign)
    else
      AddToken(tkPlus);
  end
  else if LChar = '-' then
  begin
    if Match('=') then
      AddToken(tkMinusAssign)
    else
      AddToken(tkMinus);
  end
  else if LChar = '*' then
  begin
    if Match('=') then
      AddToken(tkStarAssign)
    else
      AddToken(tkStar);
  end
  else if LChar = '/' then
  begin
    if Match('=') then
      AddToken(tkSlashAssign)
    else
      AddToken(tkSlash);
  end
  else if LChar = '=' then
    AddToken(tkEq)
  else if LChar = '^' then
    AddToken(tkCaret)
  else if LChar = '(' then
    AddToken(tkLParen)
  else if LChar = ')' then
    AddToken(tkRParen)
  else if LChar = '[' then
    AddToken(tkLBracket)
  else if LChar = ']' then
    AddToken(tkRBracket)
  else if LChar = '{' then
    AddToken(tkLBrace)
  else if LChar = '}' then
    AddToken(tkRBrace)
  else if LChar = ';' then
    AddToken(tkSemicolon)
  else if LChar = ',' then
    AddToken(tkComma)

  // Two-character tokens
  else if LChar = ':' then
  begin
    if Match('=') then
      AddToken(tkAssign)
    else
      AddToken(tkColon);
  end
  else if LChar = '.' then
  begin
    if Match('.') then
    begin
      if Match('.') then
        AddToken(tkEllipsis)
      else
        AddToken(tkDotDot);
    end
    else
      AddToken(tkDot);
  end
  else if LChar = '<' then
  begin
    if Match('=') then
      AddToken(tkLe)
    else if Match('>') then
      AddToken(tkNe)
    else
      AddToken(tkLt);
  end
  else if LChar = '>' then
  begin
    if Match('=') then
      AddToken(tkGe)
    else
      AddToken(tkGt);
  end

  // Hex number with $
  else if LChar = '$' then
    ScanHexNumber()

  // Directive or invalid #NNN char literal
  else if LChar = '#' then
  begin
    if IsDigit(Peek()) then
    begin
      // Skip past the digits
      while IsDigit(Peek()) do
        Advance();
      AddError(RSLexerCharLiteralNotSupported);
    end
    else
      ScanDirective();
  end

  // String literal
  else if LChar = '''' then
    ScanString()

  // Raw string literal @'...'
  else if LChar = '@' then
  begin
    if Peek() = '''' then
      ScanRawString()
    else if (Peek() = 'L') and (PeekNext() = '''') then
      ScanRawWideString()
    else
      AddError(RSLexerUnexpectedChar, [LChar]);
  end

  // Wide string/char with L prefix
  else if (LChar = 'L') and ((Peek() = '''') or (Peek() = '"')) then
    ScanWideStringOrChar()

  // Number
  else if IsDigit(LChar) then
    ScanNumber()

  // Identifier or keyword
  else if IsAlpha(LChar) then
    ScanIdentifierOrKeyword()

  else
    AddError(RSLexerUnexpectedChar, [LChar]);
end;

procedure TPaxLexer.ScanIdentifierOrKeyword();
var
  LLexeme: string;
  LLower: string;
  LKind: TTokenKind;
begin
  while IsAlphaNumeric(Peek()) do
    Advance();

  LLexeme := Copy(FSource, FTokenStart, FPos - FTokenStart);
  LLower := LowerCase(LLexeme);

  if FKeywords.TryGetValue(LLower, LKind) then
    AddTokenWithLexeme(LKind, LLexeme)
  else
    AddTokenWithLexeme(tkIdentifier, LLexeme);
end;

procedure TPaxLexer.ScanNumber();
var
  LLexeme: string;
  LIntVal: Int64;
  LFloatVal: Double;
  LIsFloat: Boolean;
begin
  LIsFloat := False;

  // Check for 0x hex prefix
  if (FSource[FTokenStart] = '0') and (Peek() = 'x') then
  begin
    Advance(); // skip 'x'
    ScanHexNumber();
    Exit;
  end;

  // Integer part
  while IsDigit(Peek()) do
    Advance();

  // Fractional part
  if (Peek() = '.') and IsDigit(PeekNext()) then
  begin
    LIsFloat := True;
    Advance(); // consume '.'

    while IsDigit(Peek()) do
      Advance();
  end;

  // Exponent part
  if (Peek() = 'e') or (Peek() = 'E') then
  begin
    LIsFloat := True;
    Advance(); // consume 'e'

    if (Peek() = '+') or (Peek() = '-') then
      Advance();

    while IsDigit(Peek()) do
      Advance();
  end;

  LLexeme := Copy(FSource, FTokenStart, FPos - FTokenStart);

  if LIsFloat then
  begin
    if TryStrToFloat(LLexeme, LFloatVal) then
      AddTokenFloat(LFloatVal)
    else
      AddError(RSLexerInvalidNumber, [LLexeme]);
  end
  else
  begin
    if TryStrToInt64(LLexeme, LIntVal) then
      AddTokenInt(LIntVal)
    else
      AddError(RSLexerInvalidNumber, [LLexeme]);
  end;
end;

procedure TPaxLexer.ScanHexNumber();
var
  LLexeme: string;
  LHexStr: string;
  LIntVal: Int64;
begin
  // Already consumed $ or 0x
  while IsHexDigit(Peek()) do
    Advance();

  LLexeme := Copy(FSource, FTokenStart, FPos - FTokenStart);

  // Extract just the hex digits
  if LLexeme[1] = '$' then
    LHexStr := '$' + Copy(LLexeme, 2, Length(LLexeme) - 1)
  else // 0x prefix
    LHexStr := '$' + Copy(LLexeme, 3, Length(LLexeme) - 2);

  if TryStrToInt64(LHexStr, LIntVal) then
    AddTokenInt(LIntVal)
  else
    AddError(RSLexerInvalidNumber, [LLexeme]);
end;

procedure TPaxLexer.ScanString();
var
  LBuilder: TStringBuilder;
  LChar: Char;
  LValue: string;
  LHexStr: string;
  LHexVal: Integer;
begin
  LBuilder := TStringBuilder.Create();
  try
    while not IsAtEnd() do
    begin
      LChar := Peek();

      if LChar = '''' then
      begin
        Advance(); // consume quote

        // Check for escaped quote ''
        if Peek() = '''' then
        begin
          LBuilder.Append('''');
          Advance();
        end
        else
        begin
          // End of string - always emit as string
          // Type system handles implicit conversion to char based on context
          LValue := LBuilder.ToString();
          AddTokenWithLexeme(tkString, LValue);
          Exit;
        end;
      end
      else if LChar = '\' then
      begin
        // Escape sequence
        Advance(); // consume backslash

        if IsAtEnd() then
        begin
          AddError(RSLexerUnterminatedString);
          Exit;
        end;

        LChar := Peek();
        Advance();

        if LChar = 'n' then
          LBuilder.Append(#10)  // newline
        else if LChar = 'r' then
          LBuilder.Append(#13)  // carriage return
        else if LChar = 't' then
          LBuilder.Append(#9)   // tab
        else if LChar = '0' then
          LBuilder.Append(#0)   // null
        else if LChar = '\' then
          LBuilder.Append('\')  // backslash
        else if LChar = '''' then
          LBuilder.Append('''') // single quote
        else if LChar = '"' then
          LBuilder.Append('"')  // double quote
        else if (LChar = 'x') or (LChar = 'X') then
        begin
          // Hex escape \xHH
          if IsHexDigit(Peek()) then
          begin
            LHexStr := Peek();
            Advance();

            if IsHexDigit(Peek()) then
            begin
              LHexStr := LHexStr + Peek();
              Advance();
            end;

            LHexVal := StrToIntDef('$' + LHexStr, 0);
            LBuilder.Append(Chr(LHexVal));
          end
          else
            AddError(RSLexerInvalidHexEscape);
        end
        else
        begin
          // Unknown escape, keep as-is
          LBuilder.Append('\');
          LBuilder.Append(LChar);
        end;
      end
      else if (LChar = CHAR_CR) or (LChar = CHAR_LF) then
      begin
        AddError(RSLexerUnterminatedString);
        Exit;
      end
      else
      begin
        LBuilder.Append(LChar);
        Advance();
      end;
    end;

    AddError(RSLexerUnterminatedString);
  finally
    LBuilder.Free();
  end;
end;

procedure TPaxLexer.ScanRawString();
var
  LBuilder: TStringBuilder;
  LChar: Char;
  LValue: string;
begin
  // Already consumed '@', now consume the quote
  Advance(); // consume '

  LBuilder := TStringBuilder.Create();
  try
    while not IsAtEnd() do
    begin
      LChar := Peek();

      if LChar = '''' then
      begin
        Advance(); // consume quote

        // Check for escaped quote ''
        if Peek() = '''' then
        begin
          LBuilder.Append('''');
          Advance();
        end
        else
        begin
          // End of string
          LValue := LBuilder.ToString();
          AddTokenWithLexeme(tkString, LValue);
          Exit;
        end;
      end
      else if (LChar = CHAR_CR) or (LChar = CHAR_LF) then
      begin
        AddError(RSLexerUnterminatedString);
        Exit;
      end
      else
      begin
        // Raw - no escape processing, just append character as-is
        LBuilder.Append(LChar);
        Advance();
      end;
    end;

    AddError(RSLexerUnterminatedString);
  finally
    LBuilder.Free();
  end;
end;

procedure TPaxLexer.ScanWideStringOrChar();
var
  LBuilder: TStringBuilder;
  LQuoteChar: Char;
  LChar: Char;
  LValue: string;
  LHexStr: string;
  LHexVal: Integer;
begin
  // Already consumed 'L', now get quote type
  LQuoteChar := Advance();

  LBuilder := TStringBuilder.Create();
  try
    while not IsAtEnd() do
    begin
      LChar := Peek();

      if LChar = LQuoteChar then
      begin
        Advance(); // consume quote

        // Check for escaped quote (only for single quotes)
        if (LQuoteChar = '''') and (Peek() = '''') then
        begin
          LBuilder.Append('''');
          Advance();
        end
        else
        begin
          // End of string - always emit as wide string
          // Type system handles implicit conversion to wchar based on context
          LValue := LBuilder.ToString();
          AddTokenWithLexeme(tkWideString, LValue);
          Exit;
        end;
      end
      else if LChar = '\' then
      begin
        // Escape sequence
        Advance(); // consume backslash

        if IsAtEnd() then
        begin
          AddError(RSLexerUnterminatedString);
          Exit;
        end;

        LChar := Peek();
        Advance();

        if LChar = 'n' then
          LBuilder.Append(#10)
        else if LChar = 'r' then
          LBuilder.Append(#13)
        else if LChar = 't' then
          LBuilder.Append(#9)
        else if LChar = '0' then
          LBuilder.Append(#0)
        else if LChar = '\' then
          LBuilder.Append('\')
        else if LChar = '''' then
          LBuilder.Append('''')
        else if LChar = '"' then
          LBuilder.Append('"')
        else if (LChar = 'x') or (LChar = 'X') then
        begin
          if IsHexDigit(Peek()) then
          begin
            LHexStr := Peek();
            Advance();

            if IsHexDigit(Peek()) then
            begin
              LHexStr := LHexStr + Peek();
              Advance();
            end;

            LHexVal := StrToIntDef('$' + LHexStr, 0);
            LBuilder.Append(Chr(LHexVal));
          end
          else
            AddError(RSLexerInvalidHexEscape);
        end
        else
        begin
          LBuilder.Append('\');
          LBuilder.Append(LChar);
        end;
      end
      else if (LChar = CHAR_CR) or (LChar = CHAR_LF) then
      begin
        AddError(RSLexerUnterminatedString);
        Exit;
      end
      else
      begin
        LBuilder.Append(LChar);
        Advance();
      end;
    end;

    AddError(RSLexerUnterminatedString);
  finally
    LBuilder.Free();
  end;
end;

procedure TPaxLexer.ScanRawWideString();
var
  LBuilder: TStringBuilder;
  LChar: Char;
  LValue: string;
begin
  // Already consumed '@', now consume 'L' and quote
  Advance(); // consume 'L'
  Advance(); // consume '

  LBuilder := TStringBuilder.Create();
  try
    while not IsAtEnd() do
    begin
      LChar := Peek();

      if LChar = '''' then
      begin
        Advance(); // consume quote

        // Check for escaped quote ''
        if Peek() = '''' then
        begin
          LBuilder.Append('''');
          Advance();
        end
        else
        begin
          // End of string
          LValue := LBuilder.ToString();
          AddTokenWithLexeme(tkWideString, LValue);
          Exit;
        end;
      end
      else if (LChar = CHAR_CR) or (LChar = CHAR_LF) then
      begin
        AddError(RSLexerUnterminatedString);
        Exit;
      end
      else
      begin
        // Raw - no escape processing, just append character as-is
        LBuilder.Append(LChar);
        Advance();
      end;
    end;

    AddError(RSLexerUnterminatedString);
  finally
    LBuilder.Free();
  end;
end;

procedure TPaxLexer.ScanDirective();
var
  LLexeme: string;
begin
  // Already consumed #
  while IsAlphaNumeric(Peek()) do
    Advance();

  LLexeme := Copy(FSource, FTokenStart, FPos - FTokenStart);
  AddTokenWithLexeme(tkDirective, LLexeme);
end;

procedure TPaxLexer.AddToken(const AKind: TTokenKind);
var
  LToken: TToken;
begin
  LToken.Kind := AKind;
  LToken.Lexeme := Copy(FSource, FTokenStart, FPos - FTokenStart);
  LToken.Range := MakeRange();
  LToken.IntValue := 0;
  LToken.FloatValue := 0;

  FTokens.Add(LToken);

  // Update stats
  if LToken.IsKeyword() then
    Inc(FKeywordCount)
  else if LToken.IsLiteral() then
    Inc(FLiteralCount)
  else if LToken.IsOperator() then
    Inc(FOperatorCount);
end;

procedure TPaxLexer.AddTokenWithLexeme(const AKind: TTokenKind; const ALexeme: string);
var
  LToken: TToken;
begin
  LToken.Kind := AKind;
  LToken.Lexeme := ALexeme;
  LToken.Range := MakeRange();
  LToken.IntValue := 0;
  LToken.FloatValue := 0;

  FTokens.Add(LToken);

  // Update stats
  if LToken.IsKeyword() then
    Inc(FKeywordCount)
  else if AKind = tkIdentifier then
    Inc(FIdentifierCount)
  else if LToken.IsLiteral() then
    Inc(FLiteralCount)
  else if LToken.IsOperator() then
    Inc(FOperatorCount);
end;

procedure TPaxLexer.AddTokenInt(const AValue: Int64);
var
  LToken: TToken;
begin
  LToken.Kind := tkInteger;
  LToken.Lexeme := Copy(FSource, FTokenStart, FPos - FTokenStart);
  LToken.Range := MakeRange();
  LToken.IntValue := AValue;
  LToken.FloatValue := 0;

  FTokens.Add(LToken);

  // Update stats
  Inc(FLiteralCount);
end;

procedure TPaxLexer.AddTokenFloat(const AValue: Double);
var
  LToken: TToken;
begin
  LToken.Kind := tkFloat;
  LToken.Lexeme := Copy(FSource, FTokenStart, FPos - FTokenStart);
  LToken.Range := MakeRange();
  LToken.IntValue := 0;
  LToken.FloatValue := AValue;

  FTokens.Add(LToken);

  // Update stats
  Inc(FLiteralCount);
end;

procedure TPaxLexer.AddError(const AMessage: string);
var
  LRange: TSourceRange;
begin
  if FErrors = nil then
    Exit;

  LRange := MakeRange();
  FErrors.Add(LRange, esError, 'E001', AMessage);
end;

procedure TPaxLexer.AddError(const AMessage: string; const AArgs: array of const);
begin
  AddError(Format(AMessage, AArgs));
end;

procedure TPaxLexer.SetErrors(const AErrors: TErrors);
begin
  FErrors := AErrors;
end;

procedure TPaxLexer.Tokenize(const ASource: string; const AFilename: string);
begin
  Clear();

  FSource := ASource;
  FFilename := AFilename;
  FPos := 1;
  FLine := 1;
  FColumn := 1;

  while not IsAtEnd() do
    ScanToken();

  // Add final EOF if not already added
  if (FTokens.Count = 0) or (FTokens[FTokens.Count - 1].Kind <> tkEOF) then
  begin
    MarkTokenStart();
    AddToken(tkEOF);
  end;
end;

procedure TPaxLexer.Clear();
begin
  FTokens.Clear();
  FSource := '';
  FFilename := '';
  FPos := 1;
  FLine := 1;
  FColumn := 1;

  // Reset stats
  FKeywordCount := 0;
  FIdentifierCount := 0;
  FLiteralCount := 0;
  FOperatorCount := 0;
end;

function TPaxLexer.GetToken(const AIndex: Integer): TToken;
begin
  if (AIndex >= 0) and (AIndex < FTokens.Count) then
    Result := FTokens[AIndex]
  else
  begin
    Result.Kind := tkEOF;
    Result.Lexeme := '';
    Result.Range.Clear();
    Result.IntValue := 0;
    Result.FloatValue := 0;
  end;
end;

function TPaxLexer.GetTokenCount(): Integer;
begin
  Result := FTokens.Count;
end;

end.
