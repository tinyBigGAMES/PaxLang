{===============================================================================
  Pax™ Programming Language.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://paxlang.org

  See LICENSE for license information

  ----------------------------------------------------------------------------
  Pax.CImporter - C Header to Pax Module Converter

  This unit converts C headers into Pax module source code. It uses TCC
  for preprocessing (expanding macros, includes) then parses the result
  to generate Pax declarations.

  Usage:
    LImporter := TPaxCImporter.Create();
    LImporter.SetModuleName('raylib');           // optional, defaults to header name
    LImporter.SetDllName('raylib.dll');          // optional, defaults to modulename.dll
    LImporter.SetOutputPath('output');           // optional, defaults to header folder
    LImporter.AddIncludePath('path/to/headers'); // optional, user include paths
    LImporter.AddLibraryPath('thirdparty/lib');  // optional, DLL search paths
    LImporter.AddExcludedType('va_list');        // optional, skip unwanted types
    LImporter.InsertFileBefore('end.', 'colors.txt'); // optional, insert content
    LImporter.ImportHeader('raylib.h');          // preprocesses, parses, writes .pax
    LImporter.Free();

  Limitations:
    - Win64 target only (LP64 data model assumptions)
    - Does not handle function bodies (declarations only)
===============================================================================}

unit Pax.CImporter;

{$I Pax.Defines.inc}

interface

uses
  WinApi.Windows,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  Pax.Utils,
  Pax.LibTCC;

type
  { TCTokenKind }
  TCTokenKind = (
    ctkEOF,
    ctkError,
    ctkIdentifier,
    ctkIntLiteral,
    ctkFloatLiteral,
    ctkStringLiteral,
    ctkTypedef,
    ctkStruct,
    ctkUnion,
    ctkEnum,
    ctkConst,
    ctkVoid,
    ctkChar,
    ctkShort,
    ctkInt,
    ctkLong,
    ctkFloat,
    ctkDouble,
    ctkSigned,
    ctkUnsigned,
    ctkBool,
    ctkExtern,
    ctkStatic,
    ctkInline,
    ctkRestrict,
    ctkVolatile,
    ctkAtomic,
    ctkBuiltin,
    ctkLBrace,
    ctkRBrace,
    ctkLParen,
    ctkRParen,
    ctkLBracket,
    ctkRBracket,
    ctkSemicolon,
    ctkComma,
    ctkStar,
    ctkEquals,
    ctkColon,
    ctkEllipsis,
    ctkDot,
    ctkHash
  );

  { TCToken }
  TCToken = record
    Kind: TCTokenKind;
    Lexeme: string;
    IntValue: Int64;
    FloatValue: Double;
    Line: Integer;
    Column: Integer;
  end;

  { TCLexer }
  TCLexer = class(TBaseObject)
  private
    FSource: string;
    FPos: Integer;
    FLine: Integer;
    FColumn: Integer;
    FTokens: TList<TCToken>;
    FCurrentChar: Char;

    procedure Advance();
    function Peek(): Char;
    function PeekNext(): Char;
    procedure SkipWhitespace();
    procedure SkipLineComment();
    procedure SkipBlockComment();
    procedure SkipLineMarker();
    function IsAlpha(const AChar: Char): Boolean;
    function IsDigit(const AChar: Char): Boolean;
    function IsAlphaNumeric(const AChar: Char): Boolean;
    function IsHexDigit(const AChar: Char): Boolean;
    function ScanIdentifier(): TCToken;
    function ScanNumber(): TCToken;
    function ScanString(): TCToken;
    function MakeToken(const AKind: TCTokenKind): TCToken;
    function GetKeywordKind(const AIdent: string): TCTokenKind;

  public
    constructor Create(); override;
    destructor Destroy(); override;

    procedure Tokenize(const ASource: string);
    function GetTokenCount(): Integer;
    function GetToken(const AIndex: Integer): TCToken;
    procedure Clear();
  end;

  { TCFieldInfo }
  TCFieldInfo = record
    FieldName: string;
    TypeName: string;
    IsPointer: Boolean;
    PointerDepth: Integer;
    ArraySize: Integer;
    BitWidth: Integer;
  end;

  { TCStructInfo }
  TCStructInfo = record
    StructName: string;
    IsUnion: Boolean;
    Fields: TArray<TCFieldInfo>;
  end;

  { TCEnumValue }
  TCEnumValue = record
    ValueName: string;
    Value: Int64;
    HasExplicitValue: Boolean;
  end;

  { TCEnumInfo }
  TCEnumInfo = record
    EnumName: string;
    Values: TArray<TCEnumValue>;
  end;

  { TCParamInfo }
  TCParamInfo = record
    ParamName: string;
    TypeName: string;
    IsPointer: Boolean;
    PointerDepth: Integer;
    IsConst: Boolean;
  end;

  { TCFunctionInfo }
  TCFunctionInfo = record
    FuncName: string;
    ReturnType: string;
    ReturnIsPointer: Boolean;
    ReturnPointerDepth: Integer;
    Params: TArray<TCParamInfo>;
    IsVariadic: Boolean;
  end;

  { TCTypedefInfo }
  TCTypedefInfo = record
    AliasName: string;
    TargetType: string;
    IsPointer: Boolean;
    PointerDepth: Integer;
    IsFunctionPointer: Boolean;
    FuncInfo: TCFunctionInfo;
  end;

  { TInsertionInfo }
  TInsertionInfo = record
    TargetLine: string;
    Content: string;
    InsertBefore: Boolean;
    Occurrence: Integer;
  end;

  { TPaxCImporter }
  TPaxCImporter = class(TBaseObject)
  private
    FLexer: TCLexer;
    FPos: Integer;
    FCurrentToken: TCToken;
    FModuleName: string;
    FDllName: string;
    FOutput: TStringBuilder;
    FIndent: Integer;
    FIncludePaths: TList<string>;
    FLibraryPaths: TList<string>;
    FCopyDLLs: TList<string>;
    FExcludedTypes: TList<string>;
    FOutputPath: string;
    FLastError: string;

    FStructs: TList<TCStructInfo>;
    FEnums: TList<TCEnumInfo>;
    FTypedefs: TList<TCTypedefInfo>;
    FFunctions: TList<TCFunctionInfo>;
    FForwardDecls: TList<string>;
    FInsertions: TList<TInsertionInfo>;

    function PreprocessHeader(const AHeaderFile: string; out APreprocessedSource: string): Boolean;

    function IsAtEnd(): Boolean;
    function Peek(): TCToken;
    function PeekNext(): TCToken;
    procedure Advance();
    function Check(const AKind: TCTokenKind): Boolean;
    function Match(const AKind: TCTokenKind): Boolean;
    function MatchAny(const AKinds: array of TCTokenKind): Boolean;

    procedure SkipToSemicolon();
    procedure SkipToRBrace();
    function IsTypeKeyword(): Boolean;
    function ParseBaseType(): string;
    function ParsePointerDepth(): Integer;

    procedure ParseTopLevel();
    procedure ParseTypedef();
    procedure ParseStruct(const AIsUnion: Boolean; out AInfo: TCStructInfo);
    procedure ParseEnum(out AInfo: TCEnumInfo);
    procedure ParseFunction(const AReturnType: string; const AReturnPtrDepth: Integer; const AFuncName: string);
    procedure ParseStructField(const AStruct: TCStructInfo; var AFields: TArray<TCFieldInfo>);

    procedure EmitLn(const AText: string = '');
    procedure EmitFmt(const AFormat: string; const AArgs: array of const);
    function MapCTypeToPax(const ACType: string; const AIsPointer: Boolean; const APtrDepth: Integer): string;
    function ResolveTypedefAlias(const ATypeName: string): string;
    function SanitizeIdentifier(const AName: string): string;
    procedure GenerateModule();
    procedure GenerateForwardDecls();
    procedure GenerateAllTypes();
    procedure GenerateFunctions();
    procedure ProcessInsertions();

    procedure ParsePreprocessed(const APreprocessedSource: string);

  public
    constructor Create(); override;
    destructor Destroy(); override;

    procedure AddIncludePath(const APath: string);
    procedure AddLibraryPath(const APath: string);
    procedure AddCopyDLL(const APath: string);
    procedure AddExcludedType(const ATypeName: string);
    procedure InsertTextAfter(const ATargetLine: string; const AText: string; const AOccurrence: Integer = 1);
    procedure InsertFileAfter(const ATargetLine: string; const AFilePath: string; const AOccurrence: Integer = 1);
    procedure InsertTextBefore(const ATargetLine: string; const AText: string; const AOccurrence: Integer = 1);
    procedure InsertFileBefore(const ATargetLine: string; const AFilePath: string; const AOccurrence: Integer = 1);
    procedure SetOutputPath(const APath: string);
    function ImportHeader(const AHeaderFile: string): Boolean;
    procedure SetModuleName(const AName: string);
    procedure SetDllName(const ADllName: string);
    function GetLastError(): string;
    procedure Clear();
  end;

implementation

{ TCLexer }

constructor TCLexer.Create();
begin
  inherited;
  FTokens := TList<TCToken>.Create();
  Clear();
end;

destructor TCLexer.Destroy();
begin
  FTokens.Free();
  inherited;
end;

procedure TCLexer.Advance();
begin
  if FPos <= Length(FSource) then
  begin
    if FCurrentChar = #10 then
    begin
      Inc(FLine);
      FColumn := 1;
    end
    else
      Inc(FColumn);
    Inc(FPos);
  end;

  if FPos <= Length(FSource) then
    FCurrentChar := FSource[FPos]
  else
    FCurrentChar := #0;
end;

function TCLexer.Peek(): Char;
begin
  Result := FCurrentChar;
end;

function TCLexer.PeekNext(): Char;
begin
  if FPos + 1 <= Length(FSource) then
    Result := FSource[FPos + 1]
  else
    Result := #0;
end;

procedure TCLexer.SkipWhitespace();
begin
  while (FCurrentChar <> #0) and (FCurrentChar <= ' ') do
    Advance();
end;

procedure TCLexer.SkipLineComment();
begin
  while (FCurrentChar <> #0) and (FCurrentChar <> #10) do
    Advance();
end;

procedure TCLexer.SkipBlockComment();
begin
  Advance();
  while FCurrentChar <> #0 do
  begin
    if (FCurrentChar = '*') and (PeekNext() = '/') then
    begin
      Advance();
      Advance();
      Exit;
    end;
    Advance();
  end;
end;

procedure TCLexer.SkipLineMarker();
begin
  while (FCurrentChar <> #0) and (FCurrentChar <> #10) do
    Advance();
end;

function TCLexer.IsAlpha(const AChar: Char): Boolean;
begin
  Result := ((AChar >= 'a') and (AChar <= 'z')) or
            ((AChar >= 'A') and (AChar <= 'Z')) or
            (AChar = '_');
end;

function TCLexer.IsDigit(const AChar: Char): Boolean;
begin
  Result := (AChar >= '0') and (AChar <= '9');
end;

function TCLexer.IsAlphaNumeric(const AChar: Char): Boolean;
begin
  Result := IsAlpha(AChar) or IsDigit(AChar);
end;

function TCLexer.IsHexDigit(const AChar: Char): Boolean;
begin
  Result := IsDigit(AChar) or
            ((AChar >= 'a') and (AChar <= 'f')) or
            ((AChar >= 'A') and (AChar <= 'F'));
end;

function TCLexer.GetKeywordKind(const AIdent: string): TCTokenKind;
begin
  if AIdent = 'typedef' then Result := ctkTypedef
  else if AIdent = 'struct' then Result := ctkStruct
  else if AIdent = 'union' then Result := ctkUnion
  else if AIdent = 'enum' then Result := ctkEnum
  else if AIdent = 'const' then Result := ctkConst
  else if AIdent = 'void' then Result := ctkVoid
  else if AIdent = 'char' then Result := ctkChar
  else if AIdent = 'short' then Result := ctkShort
  else if AIdent = 'int' then Result := ctkInt
  else if AIdent = 'long' then Result := ctkLong
  else if AIdent = 'float' then Result := ctkFloat
  else if AIdent = 'double' then Result := ctkDouble
  else if AIdent = 'signed' then Result := ctkSigned
  else if AIdent = 'unsigned' then Result := ctkUnsigned
  else if AIdent = '_Bool' then Result := ctkBool
  else if AIdent = 'extern' then Result := ctkExtern
  else if AIdent = 'static' then Result := ctkStatic
  else if AIdent = 'inline' then Result := ctkInline
  else if AIdent = '__inline' then Result := ctkInline
  else if AIdent = '__inline__' then Result := ctkInline
  else if AIdent = 'restrict' then Result := ctkRestrict
  else if AIdent = '__restrict' then Result := ctkRestrict
  else if AIdent = '__restrict__' then Result := ctkRestrict
  else if AIdent = 'volatile' then Result := ctkVolatile
  else if AIdent = '_Atomic' then Result := ctkAtomic
  else if AIdent.StartsWith('__builtin') then Result := ctkBuiltin
  else if AIdent.StartsWith('__attribute') then Result := ctkBuiltin
  else if AIdent.StartsWith('__declspec') then Result := ctkBuiltin
  else Result := ctkIdentifier;
end;

function TCLexer.ScanIdentifier(): TCToken;
var
  LStart: Integer;
  LIdent: string;
begin
  LStart := FPos;
  while IsAlphaNumeric(FCurrentChar) do
    Advance();
  LIdent := Copy(FSource, LStart, FPos - LStart);
  Result.Kind := GetKeywordKind(LIdent);
  Result.Lexeme := LIdent;
  Result.IntValue := 0;
  Result.FloatValue := 0;
  Result.Line := FLine;
  Result.Column := FColumn - Length(LIdent);
end;

function TCLexer.ScanNumber(): TCToken;
var
  LStart: Integer;
  LNumStr: string;
  LIsHex: Boolean;
  LIsFloat: Boolean;
begin
  LStart := FPos;
  LIsHex := False;
  LIsFloat := False;

  if (FCurrentChar = '0') and ((PeekNext() = 'x') or (PeekNext() = 'X')) then
  begin
    LIsHex := True;
    Advance();
    Advance();
    while IsHexDigit(FCurrentChar) do
      Advance();
  end
  else
  begin
    while IsDigit(FCurrentChar) do
      Advance();
    if (FCurrentChar = '.') and IsDigit(PeekNext()) then
    begin
      LIsFloat := True;
      Advance();
      while IsDigit(FCurrentChar) do
        Advance();
    end;
    if (FCurrentChar = 'e') or (FCurrentChar = 'E') then
    begin
      LIsFloat := True;
      Advance();
      if (FCurrentChar = '+') or (FCurrentChar = '-') then
        Advance();
      while IsDigit(FCurrentChar) do
        Advance();
    end;
  end;

  while CharInSet(FCurrentChar, ['u', 'U', 'l', 'L', 'f', 'F']) do
    Advance();

  LNumStr := Copy(FSource, LStart, FPos - LStart);

  if LIsFloat then
  begin
    Result.Kind := ctkFloatLiteral;
    Result.FloatValue := StrToFloatDef(LNumStr, 0);
    Result.IntValue := 0;
  end
  else
  begin
    Result.Kind := ctkIntLiteral;
    if LIsHex then
      Result.IntValue := StrToInt64Def('$' + Copy(LNumStr, 3, Length(LNumStr)), 0)
    else
      Result.IntValue := StrToInt64Def(LNumStr, 0);
    Result.FloatValue := 0;
  end;
  Result.Lexeme := LNumStr;
  Result.Line := FLine;
  Result.Column := FColumn - Length(LNumStr);
end;

function TCLexer.ScanString(): TCToken;
var
  LStart: Integer;
  LQuote: Char;
begin
  LQuote := FCurrentChar;
  LStart := FPos;
  Advance();
  while (FCurrentChar <> #0) and (FCurrentChar <> LQuote) do
  begin
    if FCurrentChar = '\' then
      Advance();
    Advance();
  end;
  if FCurrentChar = LQuote then
    Advance();
  Result.Kind := ctkStringLiteral;
  Result.Lexeme := Copy(FSource, LStart, FPos - LStart);
  Result.IntValue := 0;
  Result.FloatValue := 0;
  Result.Line := FLine;
  Result.Column := FColumn - Length(Result.Lexeme);
end;

function TCLexer.MakeToken(const AKind: TCTokenKind): TCToken;
begin
  Result.Kind := AKind;
  Result.Lexeme := FCurrentChar;
  Result.IntValue := 0;
  Result.FloatValue := 0;
  Result.Line := FLine;
  Result.Column := FColumn;
  Advance();
end;

procedure TCLexer.Tokenize(const ASource: string);
var
  LToken: TCToken;
begin
  Clear();
  FSource := ASource;
  FPos := 1;
  FLine := 1;
  FColumn := 1;
  if Length(FSource) > 0 then
    FCurrentChar := FSource[1]
  else
    FCurrentChar := #0;

  while FCurrentChar <> #0 do
  begin
    SkipWhitespace();
    if FCurrentChar = #0 then
      Break;

    if (FCurrentChar = '#') and (FColumn = 1) then
    begin
      SkipLineMarker();
      Continue;
    end;

    if (FCurrentChar = '/') and (PeekNext() = '/') then
    begin
      SkipLineComment();
      Continue;
    end;

    if (FCurrentChar = '/') and (PeekNext() = '*') then
    begin
      Advance();
      SkipBlockComment();
      Continue;
    end;

    if IsAlpha(FCurrentChar) then
    begin
      LToken := ScanIdentifier();
      FTokens.Add(LToken);
      Continue;
    end;

    if IsDigit(FCurrentChar) then
    begin
      LToken := ScanNumber();
      FTokens.Add(LToken);
      Continue;
    end;

    if (FCurrentChar = '"') or (FCurrentChar = '''') then
    begin
      LToken := ScanString();
      FTokens.Add(LToken);
      Continue;
    end;

    case FCurrentChar of
      '{': FTokens.Add(MakeToken(ctkLBrace));
      '}': FTokens.Add(MakeToken(ctkRBrace));
      '(': FTokens.Add(MakeToken(ctkLParen));
      ')': FTokens.Add(MakeToken(ctkRParen));
      '[': FTokens.Add(MakeToken(ctkLBracket));
      ']': FTokens.Add(MakeToken(ctkRBracket));
      ';': FTokens.Add(MakeToken(ctkSemicolon));
      ',': FTokens.Add(MakeToken(ctkComma));
      '*': FTokens.Add(MakeToken(ctkStar));
      '=': FTokens.Add(MakeToken(ctkEquals));
      ':': FTokens.Add(MakeToken(ctkColon));
      '#': FTokens.Add(MakeToken(ctkHash));
      '.':
        begin
          if PeekNext() = '.' then
          begin
            Advance();
            Advance();
            if FCurrentChar = '.' then
            begin
              LToken.Kind := ctkEllipsis;
              LToken.Lexeme := '...';
              LToken.Line := FLine;
              LToken.Column := FColumn - 2;
              Advance();
              FTokens.Add(LToken);
            end;
          end
          else
            FTokens.Add(MakeToken(ctkDot));
        end;
    else
      Advance();
    end;
  end;

  LToken.Kind := ctkEOF;
  LToken.Lexeme := '';
  LToken.IntValue := 0;
  LToken.FloatValue := 0;
  LToken.Line := FLine;
  LToken.Column := FColumn;
  FTokens.Add(LToken);
end;

function TCLexer.GetTokenCount(): Integer;
begin
  Result := FTokens.Count;
end;

function TCLexer.GetToken(const AIndex: Integer): TCToken;
begin
  if (AIndex >= 0) and (AIndex < FTokens.Count) then
    Result := FTokens[AIndex]
  else
  begin
    Result.Kind := ctkEOF;
    Result.Lexeme := '';
  end;
end;

procedure TCLexer.Clear();
begin
  FTokens.Clear();
  FSource := '';
  FPos := 1;
  FLine := 1;
  FColumn := 1;
  FCurrentChar := #0;
end;

{ TPaxCImporter }

constructor TPaxCImporter.Create();
begin
  inherited;
  FLexer := TCLexer.Create();
  FOutput := TStringBuilder.Create();
  FStructs := TList<TCStructInfo>.Create();
  FEnums := TList<TCEnumInfo>.Create();
  FTypedefs := TList<TCTypedefInfo>.Create();
  FFunctions := TList<TCFunctionInfo>.Create();
  FForwardDecls := TList<string>.Create();
  FInsertions := TList<TInsertionInfo>.Create();
  FIncludePaths := TList<string>.Create();
  FLibraryPaths := TList<string>.Create();
  FCopyDLLs := TList<string>.Create();
  FExcludedTypes := TList<string>.Create();
  FModuleName := '';
  FDllName := '';
  FOutputPath := '';
  FLastError := '';
  FIndent := 0;
  FPos := 0;
end;

destructor TPaxCImporter.Destroy();
begin
  FExcludedTypes.Free();
  FCopyDLLs.Free();
  FLibraryPaths.Free();
  FIncludePaths.Free();
  FInsertions.Free();
  FForwardDecls.Free();
  FFunctions.Free();
  FTypedefs.Free();
  FEnums.Free();
  FStructs.Free();
  FOutput.Free();
  FLexer.Free();
  inherited;
end;

function TPaxCImporter.PreprocessHeader(const AHeaderFile: string; out APreprocessedSource: string): Boolean;
type
  TDup = function(fd: Integer): Integer; cdecl;
  TDup2 = function(fd1, fd2: Integer): Integer; cdecl;
  TOpen = function(filename: PAnsiChar; oflag: Integer; pmode: Integer): Integer; cdecl;
  TClose = function(fd: Integer): Integer; cdecl;
  TFflush = function(stream: Pointer): Integer; cdecl;
const
  O_WRONLY = $0001;
  O_CREAT  = $0100;
  O_TRUNC  = $0200;
  S_IWRITE = $0080;
var
  LTCC: TLibTCC;
  LTempFile: string;
  LCompileResult: Boolean;
  LTCCBasePath: string;
  LPath: string;
  LTCCErrors: TStringList;
  LMsvcrt: THandle;
  LDup: TDup;
  LDup2: TDup2;
  LOpen: TOpen;
  LClose: TClose;
  LFflush: TFflush;
  LSavedStdout: Integer;
  LTempFd: Integer;
begin
  Result := False;
  APreprocessedSource := '';

  // Load msvcrt functions for file descriptor manipulation
  LMsvcrt := GetModuleHandle('msvcrt.dll');
  if LMsvcrt = 0 then
    LMsvcrt := LoadLibrary('msvcrt.dll');
  if LMsvcrt = 0 then
  begin
    FLastError := 'Failed to load msvcrt.dll';
    Exit;
  end;

  @LDup := GetProcAddress(LMsvcrt, '_dup');
  @LDup2 := GetProcAddress(LMsvcrt, '_dup2');
  @LOpen := GetProcAddress(LMsvcrt, '_open');
  @LClose := GetProcAddress(LMsvcrt, '_close');
  @LFflush := GetProcAddress(LMsvcrt, 'fflush');

  if not Assigned(LDup) or not Assigned(LDup2) or not Assigned(LOpen) or
     not Assigned(LClose) or not Assigned(LFflush) then
  begin
    FLastError := 'Failed to get msvcrt functions';
    Exit;
  end;

  // Flush Delphi output before redirecting
  Flush(Output);

  LTCCErrors := TStringList.Create();
  try
    LTCC := TLibTCC.Create();
    try
      // Set up error callback to capture TCC errors
      LTCC.SetPrintCallback(LTCCErrors,
        procedure(const AError: string; const AUserData: Pointer)
        begin
          TStringList(AUserData).Add(AError);
        end);

      if not LTCC.SetOuput(opPreProcess) then
      begin
        FLastError := 'Failed to set TCC output type to PreProcess';
        Exit;
      end;

      LTCCBasePath := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'thirdparty\tcc');
      LTCC.AddIncludePath(TPath.Combine(LTCCBasePath, 'include'));
      LTCC.AddIncludePath(TPath.Combine(LTCCBasePath, 'include\winapi'));
      LTCC.AddIncludePath(TPath.Combine(LTCCBasePath, 'libtcc'));

      for LPath in FIncludePaths do
        LTCC.AddIncludePath(LPath);

      // Create temp file and redirect stdout using file descriptors
      LTempFile := TPath.GetTempFileName();

      // Save current stdout (fd 1)
      LSavedStdout := LDup(1);
      if LSavedStdout = -1 then
      begin
        FLastError := 'Failed to duplicate stdout';
        Exit;
      end;

      // Open temp file for writing
      LTempFd := LOpen(PAnsiChar(AnsiString(LTempFile)), O_WRONLY or O_CREAT or O_TRUNC, S_IWRITE);
      if LTempFd = -1 then
      begin
        LClose(LSavedStdout);
        FLastError := 'Failed to open temp file';
        Exit;
      end;

      // Redirect stdout to temp file
      LDup2(LTempFd, 1);
      LClose(LTempFd);

      try
        LCompileResult := LTCC.CompileFile(AHeaderFile);
        LFflush(nil); // Flush all streams
      finally
        // Restore stdout
        LDup2(LSavedStdout, 1);
        LClose(LSavedStdout);
      end;

      if not LCompileResult then
      begin
        FLastError := 'Failed to preprocess header file: ' + AHeaderFile;
        if LTCCErrors.Count > 0 then
          FLastError := FLastError + sLineBreak + LTCCErrors.Text;
        TFile.Delete(LTempFile);
        Exit;
      end;

      if TFile.Exists(LTempFile) then
      begin
        APreprocessedSource := TFile.ReadAllText(LTempFile);
        TFile.Delete(LTempFile);
        Result := True;
      end
      else
      begin
        FLastError := 'Preprocessor output file not created';
        if LTCCErrors.Count > 0 then
          FLastError := FLastError + sLineBreak + LTCCErrors.Text;
      end;

    finally
      LTCC.Free();
    end;
  finally
    LTCCErrors.Free();
  end;
end;

function TPaxCImporter.IsAtEnd(): Boolean;
begin
  Result := FCurrentToken.Kind = ctkEOF;
end;

function TPaxCImporter.Peek(): TCToken;
begin
  Result := FCurrentToken;
end;

function TPaxCImporter.PeekNext(): TCToken;
begin
  Result := FLexer.GetToken(FPos + 1);
end;

procedure TPaxCImporter.Advance();
begin
  Inc(FPos);
  FCurrentToken := FLexer.GetToken(FPos);
end;

function TPaxCImporter.Check(const AKind: TCTokenKind): Boolean;
begin
  Result := FCurrentToken.Kind = AKind;
end;

function TPaxCImporter.Match(const AKind: TCTokenKind): Boolean;
begin
  if Check(AKind) then
  begin
    Advance();
    Result := True;
  end
  else
    Result := False;
end;

function TPaxCImporter.MatchAny(const AKinds: array of TCTokenKind): Boolean;
var
  LKind: TCTokenKind;
begin
  for LKind in AKinds do
  begin
    if Check(LKind) then
    begin
      Advance();
      Exit(True);
    end;
  end;
  Result := False;
end;

procedure TPaxCImporter.SkipToSemicolon();
begin
  while not IsAtEnd() and not Check(ctkSemicolon) do
    Advance();
  if Check(ctkSemicolon) then
    Advance();
end;

procedure TPaxCImporter.SkipToRBrace();
var
  LDepth: Integer;
begin
  LDepth := 1;
  while not IsAtEnd() and (LDepth > 0) do
  begin
    if Check(ctkLBrace) then
      Inc(LDepth)
    else if Check(ctkRBrace) then
      Dec(LDepth);
    Advance();
  end;
end;

function TPaxCImporter.IsTypeKeyword(): Boolean;
begin
  Result := FCurrentToken.Kind in [
    ctkVoid, ctkChar, ctkShort, ctkInt, ctkLong,
    ctkFloat, ctkDouble, ctkSigned, ctkUnsigned, ctkBool,
    ctkStruct, ctkUnion, ctkEnum, ctkConst
  ];
end;

function TPaxCImporter.ParseBaseType(): string;
var
  LHasUnsigned: Boolean;
  LHasSigned: Boolean;
  LHasLong: Boolean;
  LLongCount: Integer;
  LHasShort: Boolean;
begin
  Result := '';
  LHasUnsigned := False;
  LHasSigned := False;
  LHasLong := False;
  LLongCount := 0;
  LHasShort := False;

  while True do
  begin
    case FCurrentToken.Kind of
      ctkConst, ctkVolatile, ctkRestrict, ctkAtomic, ctkInline, ctkStatic:
        Advance();
      ctkUnsigned:
        begin
          LHasUnsigned := True;
          Advance();
        end;
      ctkSigned:
        begin
          LHasSigned := True;
          Advance();
        end;
      ctkLong:
        begin
          LHasLong := True;
          Inc(LLongCount);
          Advance();
        end;
      ctkShort:
        begin
          LHasShort := True;
          Advance();
        end;
      ctkVoid:
        begin
          Result := 'void';
          Advance();
          Exit;
        end;
      ctkChar:
        begin
          if LHasUnsigned then
            Result := 'unsigned char'
          else if LHasSigned then
            Result := 'signed char'
          else
            Result := 'char';
          Advance();
          Exit;
        end;
      ctkInt:
        begin
          if LHasShort then
          begin
            if LHasUnsigned then Result := 'unsigned short' else Result := 'short';
          end
          else if LHasLong then
          begin
            if LLongCount >= 2 then
            begin
              if LHasUnsigned then Result := 'unsigned long long' else Result := 'long long';
            end
            else
            begin
              if LHasUnsigned then Result := 'unsigned long' else Result := 'long';
            end;
          end
          else
          begin
            if LHasUnsigned then Result := 'unsigned int' else Result := 'int';
          end;
          Advance();
          Exit;
        end;
      ctkFloat:
        begin
          Result := 'float';
          Advance();
          Exit;
        end;
      ctkDouble:
        begin
          if LHasLong then Result := 'long double' else Result := 'double';
          Advance();
          Exit;
        end;
      ctkBool:
        begin
          Result := '_Bool';
          Advance();
          Exit;
        end;
      ctkStruct, ctkUnion:
        begin
          if FCurrentToken.Kind = ctkStruct then Result := 'struct ' else Result := 'union ';
          Advance();
          if Check(ctkIdentifier) then
          begin
            Result := Result + FCurrentToken.Lexeme;
            Advance();
          end;
          if Check(ctkLBrace) then
          begin
            Advance();
            SkipToRBrace();
          end;
          Exit;
        end;
      ctkEnum:
        begin
          Result := 'enum ';
          Advance();
          if Check(ctkIdentifier) then
          begin
            Result := Result + FCurrentToken.Lexeme;
            Advance();
          end;
          if Check(ctkLBrace) then
          begin
            Advance();
            SkipToRBrace();
          end;
          Exit;
        end;
      ctkIdentifier:
        begin
          Result := FCurrentToken.Lexeme;
          Advance();
          Exit;
        end;
      ctkBuiltin:
        begin
          Result := FCurrentToken.Lexeme;
          Advance();
          if Check(ctkLParen) then
          begin
            var LDepth := 1;
            Advance();
            while not IsAtEnd() and (LDepth > 0) do
            begin
              if Check(ctkLParen) then Inc(LDepth)
              else if Check(ctkRParen) then Dec(LDepth);
              Advance();
            end;
          end;
          Exit;
        end;
    else
      Break;
    end;
  end;

  if (Result = '') and (LHasUnsigned or LHasSigned or LHasLong or LHasShort) then
  begin
    if LHasShort then
    begin
      if LHasUnsigned then Result := 'unsigned short' else Result := 'short';
    end
    else if LHasLong then
    begin
      if LLongCount >= 2 then
      begin
        if LHasUnsigned then Result := 'unsigned long long' else Result := 'long long';
      end
      else
      begin
        if LHasUnsigned then Result := 'unsigned long' else Result := 'long';
      end;
    end
    else
    begin
      if LHasUnsigned then Result := 'unsigned int' else Result := 'int';
    end;
  end;
end;

function TPaxCImporter.ParsePointerDepth(): Integer;
begin
  Result := 0;
  while Check(ctkStar) do
  begin
    Inc(Result);
    Advance();
    while MatchAny([ctkConst, ctkVolatile, ctkRestrict]) do
      ;
  end;
end;

procedure TPaxCImporter.ParseTopLevel();
var
  LBaseType: string;
  LPtrDepth: Integer;
  LName: string;
begin
  while not IsAtEnd() do
  begin
    while MatchAny([ctkExtern, ctkStatic, ctkInline]) do
      ;

    if Check(ctkTypedef) then
      ParseTypedef()
    else if IsTypeKeyword() or Check(ctkIdentifier) then
    begin
      LBaseType := ParseBaseType();
      LPtrDepth := ParsePointerDepth();

      if Check(ctkIdentifier) then
      begin
        LName := FCurrentToken.Lexeme;
        Advance();

        if Check(ctkLParen) then
          ParseFunction(LBaseType, LPtrDepth, LName)
        else
          SkipToSemicolon();
      end
      else
        SkipToSemicolon();
    end
    else if Check(ctkSemicolon) then
      Advance()
    else
      Advance();
  end;
end;

procedure TPaxCImporter.ParseTypedef();
var
  LInfo: TCTypedefInfo;
  LStructInfo: TCStructInfo;
  LEnumInfo: TCEnumInfo;
  LBaseType: string;
  LPtrDepth: Integer;
  LIsUnion: Boolean;
  LTagName: string;
  LAliasName: string;
  LDepth: Integer;
begin
  Advance();

  if Check(ctkStruct) or Check(ctkUnion) then
  begin
    LIsUnion := Check(ctkUnion);
    Advance();

    LTagName := '';
    if Check(ctkIdentifier) then
    begin
      LTagName := FCurrentToken.Lexeme;
      Advance();
    end;

    if Check(ctkLBrace) then
    begin
      ParseStruct(LIsUnion, LStructInfo);

      if Check(ctkIdentifier) then
      begin
        LStructInfo.StructName := FCurrentToken.Lexeme;
        Advance();
      end
      else if LTagName <> '' then
        LStructInfo.StructName := LTagName;

      if LStructInfo.StructName <> '' then
        FStructs.Add(LStructInfo);
    end
    else
    begin
      if Check(ctkIdentifier) then
      begin
        LAliasName := FCurrentToken.Lexeme;
        Advance();

        if (LTagName <> '') and (LTagName = LAliasName) then
        begin
          if not FForwardDecls.Contains(LAliasName) then
            FForwardDecls.Add(LAliasName);
        end
        else if LTagName <> '' then
        begin
          LInfo.AliasName := LAliasName;
          LInfo.TargetType := LTagName;
          LInfo.IsPointer := False;
          LInfo.PointerDepth := 0;
          LInfo.IsFunctionPointer := False;
          FTypedefs.Add(LInfo);
        end;
      end;
    end;
  end
  else if Check(ctkEnum) then
  begin
    Advance();
    if Check(ctkIdentifier) then
      Advance();

    if Check(ctkLBrace) then
    begin
      ParseEnum(LEnumInfo);

      if Check(ctkIdentifier) then
      begin
        LEnumInfo.EnumName := FCurrentToken.Lexeme;
        Advance();
      end;

      if LEnumInfo.EnumName <> '' then
        FEnums.Add(LEnumInfo);
    end;
  end
  else
  begin
    LBaseType := ParseBaseType();
    LPtrDepth := ParsePointerDepth();

    if Check(ctkLParen) then
    begin
      Advance();
      if Check(ctkStar) then
      begin
        Advance();
        if Check(ctkIdentifier) then
        begin
          LInfo.AliasName := FCurrentToken.Lexeme;
          LInfo.IsFunctionPointer := True;
          LInfo.TargetType := LBaseType;
          LInfo.IsPointer := LPtrDepth > 0;
          LInfo.PointerDepth := LPtrDepth;
          Advance();

          if Check(ctkRParen) then
            Advance();

          // Parse function pointer parameters into FuncInfo
          LInfo.FuncInfo.FuncName := LInfo.AliasName;
          LInfo.FuncInfo.ReturnType := LBaseType;
          LInfo.FuncInfo.ReturnIsPointer := LPtrDepth > 0;
          LInfo.FuncInfo.ReturnPointerDepth := LPtrDepth;
          LInfo.FuncInfo.IsVariadic := False;
          SetLength(LInfo.FuncInfo.Params, 0);

          if Check(ctkLParen) then
          begin
            Advance();

            while not IsAtEnd() and not Check(ctkRParen) do
            begin
              // Check for void parameter list
              if Check(ctkVoid) and (PeekNext().Kind = ctkRParen) then
              begin
                Advance();
                Break;
              end;

              // Check for variadic
              if Check(ctkEllipsis) then
              begin
                LInfo.FuncInfo.IsVariadic := True;
                Advance();
                Break;
              end;

              var LParam: TCParamInfo;
              LParam.IsConst := False;
              if Check(ctkConst) then
              begin
                LParam.IsConst := True;
                Advance();
              end;

              LParam.TypeName := ParseBaseType();
              LParam.PointerDepth := ParsePointerDepth();
              LParam.IsPointer := LParam.PointerDepth > 0;

              if Check(ctkConst) then
              begin
                LParam.IsConst := True;
                Advance();
              end;

              if Check(ctkIdentifier) then
              begin
                LParam.ParamName := FCurrentToken.Lexeme;
                Advance();
              end
              else
                LParam.ParamName := '';

              // Handle array parameters as pointers
              if Check(ctkLBracket) then
              begin
                LParam.IsPointer := True;
                Inc(LParam.PointerDepth);
                while not IsAtEnd() and not Check(ctkRBracket) do
                  Advance();
                Match(ctkRBracket);
              end;

              SetLength(LInfo.FuncInfo.Params, Length(LInfo.FuncInfo.Params) + 1);
              LInfo.FuncInfo.Params[High(LInfo.FuncInfo.Params)] := LParam;

              if not Match(ctkComma) then
                Break;
            end;

            Match(ctkRParen);
          end;

          FTypedefs.Add(LInfo);
        end;
      end
      else
      begin
        SkipToSemicolon();
        Exit;
      end;
    end
    else if Check(ctkIdentifier) then
    begin
      LInfo.AliasName := FCurrentToken.Lexeme;
      LInfo.TargetType := LBaseType;
      LInfo.IsPointer := LPtrDepth > 0;
      LInfo.PointerDepth := LPtrDepth;
      LInfo.IsFunctionPointer := False;
      Advance();
      FTypedefs.Add(LInfo);
    end;
  end;

  if Check(ctkSemicolon) then
    Advance();
end;

procedure TPaxCImporter.ParseStruct(const AIsUnion: Boolean; out AInfo: TCStructInfo);
begin
  AInfo.StructName := '';
  AInfo.IsUnion := AIsUnion;
  SetLength(AInfo.Fields, 0);

  if not Match(ctkLBrace) then
    Exit;

  while not IsAtEnd() and not Check(ctkRBrace) do
    ParseStructField(AInfo, AInfo.Fields);

  Match(ctkRBrace);
end;

procedure TPaxCImporter.ParseStructField(const AStruct: TCStructInfo; var AFields: TArray<TCFieldInfo>);
var
  LField: TCFieldInfo;
  LBaseType: string;
begin
  if Check(ctkStruct) or Check(ctkUnion) then
  begin
    Advance();
    if Check(ctkIdentifier) then
      Advance();
    if Check(ctkLBrace) then
    begin
      Advance();
      SkipToRBrace();
    end;
  end;

  LBaseType := ParseBaseType();
  if LBaseType = '' then
  begin
    SkipToSemicolon();
    Exit;
  end;

  repeat
    LField.TypeName := LBaseType;
    LField.PointerDepth := ParsePointerDepth();
    LField.IsPointer := LField.PointerDepth > 0;
    LField.ArraySize := 0;
    LField.BitWidth := 0;

    if Check(ctkIdentifier) then
    begin
      LField.FieldName := FCurrentToken.Lexeme;
      Advance();

      if Check(ctkLBracket) then
      begin
        Advance();
        if Check(ctkIntLiteral) then
        begin
          LField.ArraySize := FCurrentToken.IntValue;
          Advance();
        end;
        Match(ctkRBracket);
      end;

      if Check(ctkColon) then
      begin
        Advance();
        if Check(ctkIntLiteral) then
        begin
          LField.BitWidth := FCurrentToken.IntValue;
          Advance();
        end;
      end;

      SetLength(AFields, Length(AFields) + 1);
      AFields[High(AFields)] := LField;
    end;
  until not Match(ctkComma);

  Match(ctkSemicolon);
end;

procedure TPaxCImporter.ParseEnum(out AInfo: TCEnumInfo);
var
  LValue: TCEnumValue;
  LNextVal: Int64;
  LNegative: Boolean;
begin
  AInfo.EnumName := '';
  SetLength(AInfo.Values, 0);
  LNextVal := 0;

  if not Match(ctkLBrace) then
    Exit;

  while not IsAtEnd() and not Check(ctkRBrace) do
  begin
    if Check(ctkIdentifier) then
    begin
      LValue.ValueName := FCurrentToken.Lexeme;
      LValue.HasExplicitValue := False;
      LValue.Value := LNextVal;
      Advance();

      if Match(ctkEquals) then
      begin
        LValue.HasExplicitValue := True;
        LNegative := False;
        if Check(ctkIdentifier) and (FCurrentToken.Lexeme = '-') then
        begin
          LNegative := True;
          Advance();
        end;

        if Check(ctkIntLiteral) then
        begin
          LValue.Value := FCurrentToken.IntValue;
          if LNegative then
            LValue.Value := -LValue.Value;
          Advance();
        end;
      end;

      LNextVal := LValue.Value + 1;
      SetLength(AInfo.Values, Length(AInfo.Values) + 1);
      AInfo.Values[High(AInfo.Values)] := LValue;
    end;

    if not Match(ctkComma) then
      Break;
  end;

  Match(ctkRBrace);
end;

procedure TPaxCImporter.ParseFunction(const AReturnType: string; const AReturnPtrDepth: Integer; const AFuncName: string);
var
  LFunc: TCFunctionInfo;
  LParam: TCParamInfo;
begin
  LFunc.FuncName := AFuncName;
  LFunc.ReturnType := AReturnType;
  LFunc.ReturnIsPointer := AReturnPtrDepth > 0;
  LFunc.ReturnPointerDepth := AReturnPtrDepth;
  LFunc.IsVariadic := False;
  SetLength(LFunc.Params, 0);

  if not Match(ctkLParen) then
  begin
    SkipToSemicolon();
    Exit;
  end;

  while not IsAtEnd() and not Check(ctkRParen) do
  begin
    if Check(ctkVoid) and (PeekNext().Kind = ctkRParen) then
    begin
      Advance();
      Break;
    end;

    if Check(ctkEllipsis) then
    begin
      LFunc.IsVariadic := True;
      Advance();
      Break;
    end;

    LParam.IsConst := False;
    if Check(ctkConst) then
    begin
      LParam.IsConst := True;
      Advance();
    end;

    LParam.TypeName := ParseBaseType();
    LParam.PointerDepth := ParsePointerDepth();
    LParam.IsPointer := LParam.PointerDepth > 0;

    if Check(ctkConst) then
    begin
      LParam.IsConst := True;
      Advance();
    end;

    if Check(ctkIdentifier) then
    begin
      LParam.ParamName := FCurrentToken.Lexeme;
      Advance();
    end
    else
      LParam.ParamName := '';

    if Check(ctkLBracket) then
    begin
      LParam.IsPointer := True;
      Inc(LParam.PointerDepth);
      while not IsAtEnd() and not Check(ctkRBracket) do
        Advance();
      Match(ctkRBracket);
    end;

    SetLength(LFunc.Params, Length(LFunc.Params) + 1);
    LFunc.Params[High(LFunc.Params)] := LParam;

    if not Match(ctkComma) then
      Break;
  end;

  Match(ctkRParen);
  Match(ctkSemicolon);

  FFunctions.Add(LFunc);
end;

procedure TPaxCImporter.EmitLn(const AText: string);
begin
  FOutput.AppendLine(StringOfChar(' ', FIndent * 2) + AText);
end;

procedure TPaxCImporter.EmitFmt(const AFormat: string; const AArgs: array of const);
begin
  EmitLn(Format(AFormat, AArgs));
end;

function TPaxCImporter.MapCTypeToPax(const ACType: string; const AIsPointer: Boolean; const APtrDepth: Integer): string;
var
  LBaseType: string;
  LI: Integer;
begin
  LBaseType := ACType;

  if LBaseType = 'void' then LBaseType := 'Pointer'
  else if LBaseType = '_Bool' then LBaseType := 'Boolean'
  else if LBaseType = 'char' then LBaseType := 'Char'
  else if LBaseType = 'signed char' then LBaseType := 'Int8'
  else if LBaseType = 'unsigned char' then LBaseType := 'UInt8'
  else if LBaseType = 'short' then LBaseType := 'Int16'
  else if LBaseType = 'unsigned short' then LBaseType := 'UInt16'
  else if LBaseType = 'int' then LBaseType := 'Int32'
  else if LBaseType = 'unsigned int' then LBaseType := 'UInt32'
  else if LBaseType = 'long' then LBaseType := 'Int32'
  else if LBaseType = 'unsigned long' then LBaseType := 'UInt32'
  else if LBaseType = 'long long' then LBaseType := 'Int64'
  else if LBaseType = 'unsigned long long' then LBaseType := 'UInt64'
  else if LBaseType = 'float' then LBaseType := 'Float32'
  else if LBaseType = 'double' then LBaseType := 'Float64'
  else if LBaseType = 'long double' then LBaseType := 'Float64'
  else if LBaseType = 'va_list' then LBaseType := 'Pointer'  // C variadic args list - opaque pointer
  else if LBaseType = '__va_list_tag' then LBaseType := 'Pointer'  // GCC internal va_list type
  else if LBaseType.StartsWith('struct ') then LBaseType := Copy(LBaseType, 8, Length(LBaseType))
  else if LBaseType.StartsWith('union ') then LBaseType := Copy(LBaseType, 7, Length(LBaseType))
  else if LBaseType.StartsWith('enum ') then LBaseType := Copy(LBaseType, 6, Length(LBaseType));

  if AIsPointer or (APtrDepth > 0) then
  begin
    Result := '';
    for LI := 1 to APtrDepth do
      Result := Result + 'pointer to ';
    Result := Result + LBaseType;
  end
  else
    Result := LBaseType;
end;

function TPaxCImporter.ResolveTypedefAlias(const ATypeName: string): string;
var
  LTypedef: TCTypedefInfo;
begin
  // Check if this type is a typedef alias and return the target type
  for LTypedef in FTypedefs do
  begin
    if (not LTypedef.IsFunctionPointer) and (LTypedef.AliasName = ATypeName) then
      Exit(LTypedef.TargetType);
  end;
  // Not a typedef alias, return as-is
  Result := ATypeName;
end;

function TPaxCImporter.SanitizeIdentifier(const AName: string): string;
const
  CPaxKeywords: array[0..31] of string = (
    'and', 'array', 'begin', 'case', 'const', 'div', 'do', 'else', 'end',
    'for', 'function', 'if', 'import', 'in', 'mod', 'module', 'nil', 'not',
    'of', 'or', 'pointer', 'procedure', 'record', 'repeat', 'return', 'routine',
    'set', 'then', 'to', 'type', 'var', 'while'
  );
var
  LLower: string;
  LKeyword: string;
begin
  Result := AName;
  LLower := LowerCase(AName);
  for LKeyword in CPaxKeywords do
  begin
    if LLower = LKeyword then
    begin
      Result := AName + '_';
      Exit;
    end;
  end;
end;

procedure TPaxCImporter.GenerateModule();
var
  LPath: string;
begin
  FOutput.Clear();
  EmitFmt('module lib %s;', [FModuleName]);
  EmitLn();

  // Emit library path directives
  for LPath in FLibraryPaths do
    EmitFmt('#librarypath ''%s''', [LPath]);
  if FLibraryPaths.Count > 0 then
    EmitLn();

  // Emit copy DLL directives
  for LPath in FCopyDLLs do
    EmitFmt('#copydll ''%s''', [LPath]);
  if FCopyDLLs.Count > 0 then
    EmitLn();

  GenerateForwardDecls();  // Only truly opaque types
  GenerateAllTypes();      // Structs, enums, and typedefs in one block
  GenerateFunctions();
  EmitLn('end.');
end;

procedure TPaxCImporter.GenerateForwardDecls();
var
  LName: string;
begin
  if FForwardDecls.Count = 0 then
    Exit;

  EmitLn('(* Forward declarations (opaque types) *)');
  EmitLn('public type');
  Inc(FIndent);
  for LName in FForwardDecls do
    EmitFmt('%s = record end;', [SanitizeIdentifier(LName)]);
  Dec(FIndent);
  EmitLn();
end;

procedure TPaxCImporter.GenerateAllTypes();
var
  LStruct: TCStructInfo;
  LEnum: TCEnumInfo;
  LTypedef: TCTypedefInfo;
  LField: TCFieldInfo;
  LValue: TCEnumValue;
  LFieldType: string;
  LTargetType: string;
  LI: Integer;
  LLine: string;
  LHasTypes: Boolean;
begin
  LHasTypes := (FStructs.Count > 0) or (FEnums.Count > 0) or (FTypedefs.Count > 0);
  if not LHasTypes then
    Exit;

  EmitLn('(* Types *)');
  EmitLn('public type');
  Inc(FIndent);

  // 1. Emit function pointer typedefs FIRST as proper routine types
  for LTypedef in FTypedefs do
  begin
    if FExcludedTypes.Contains(LTypedef.AliasName) or
       FExcludedTypes.Contains(LTypedef.TargetType) then
      Continue;

    if LTypedef.IsFunctionPointer then
    begin
      // Build parameter list for routine type
      var LParamStr := '';
      for LI := 0 to High(LTypedef.FuncInfo.Params) do
      begin
        var LParam := LTypedef.FuncInfo.Params[LI];
        var LParamType := MapCTypeToPax(LParam.TypeName, LParam.IsPointer, LParam.PointerDepth);
        var LParamName: string;
        if LParam.ParamName <> '' then
          LParamName := 'A' + LParam.ParamName
        else
          LParamName := Format('AParam%d', [LI]);
        if LI > 0 then
          LParamStr := LParamStr + '; ';
        LParamStr := LParamStr + Format('const %s: %s', [SanitizeIdentifier(LParamName), LParamType]);
      end;

      // Handle variadic
      if LTypedef.FuncInfo.IsVariadic then
      begin
        if LParamStr <> '' then
          LParamStr := LParamStr + '; ...';
      end;

      // Generate routine type
      if (LTypedef.FuncInfo.ReturnType = 'void') and not LTypedef.FuncInfo.ReturnIsPointer then
        EmitFmt('%s = routine(%s);', [SanitizeIdentifier(LTypedef.AliasName), LParamStr])
      else
      begin
        var LReturnType := MapCTypeToPax(LTypedef.FuncInfo.ReturnType, LTypedef.FuncInfo.ReturnIsPointer, LTypedef.FuncInfo.ReturnPointerDepth);
        EmitFmt('%s = routine(%s): %s;', [SanitizeIdentifier(LTypedef.AliasName), LParamStr, LReturnType]);
      end;
    end;
  end;
  EmitLn();

  // 2. Emit all structs
  for LStruct in FStructs do
  begin
    if LStruct.IsUnion then
      EmitFmt('%s = union', [SanitizeIdentifier(LStruct.StructName)])
    else
      EmitFmt('%s = record', [SanitizeIdentifier(LStruct.StructName)]);

    Inc(FIndent);
    for LField in LStruct.Fields do
    begin
      // Resolve typedef aliases to base types (e.g., Texture2D -> Texture)
      LFieldType := MapCTypeToPax(ResolveTypedefAlias(LField.TypeName), LField.IsPointer, LField.PointerDepth);
      if LField.ArraySize > 0 then
        EmitFmt('%s: array[0..%d] of %s;', [SanitizeIdentifier(LField.FieldName), LField.ArraySize - 1, LFieldType])
      else if LField.BitWidth > 0 then
        EmitFmt('%s: %s : %d;', [SanitizeIdentifier(LField.FieldName), LFieldType, LField.BitWidth])
      else
        EmitFmt('%s: %s;', [SanitizeIdentifier(LField.FieldName), LFieldType]);
    end;
    Dec(FIndent);
    EmitLn('end;');
    EmitLn();
  end;

  // 3. Emit type alias typedefs AFTER structs (they reference struct names)
  for LTypedef in FTypedefs do
  begin
    if FExcludedTypes.Contains(LTypedef.AliasName) or
       FExcludedTypes.Contains(LTypedef.TargetType) then
      Continue;

    // Skip function pointers - already emitted above
    if LTypedef.IsFunctionPointer then
      Continue;

    LTargetType := MapCTypeToPax(LTypedef.TargetType, LTypedef.IsPointer, LTypedef.PointerDepth);
    EmitFmt('%s = %s;', [SanitizeIdentifier(LTypedef.AliasName), LTargetType]);
  end;
  EmitLn();

  // 4. Emit all enums
  for LEnum in FEnums do
  begin
    EmitFmt('%s = (', [SanitizeIdentifier(LEnum.EnumName)]);
    Inc(FIndent);
    for LI := 0 to High(LEnum.Values) do
    begin
      LValue := LEnum.Values[LI];
      // Only emit explicit value if it has one, otherwise let C/Pax auto-increment
      if LValue.HasExplicitValue then
        LLine := Format('%s = %d', [SanitizeIdentifier(LValue.ValueName), LValue.Value])
      else
        LLine := SanitizeIdentifier(LValue.ValueName);
      if LI < High(LEnum.Values) then
        LLine := LLine + ',';
      EmitLn(LLine);
    end;
    Dec(FIndent);
    EmitLn(');');
    EmitLn();
  end;

  Dec(FIndent);
  EmitLn();
end;

procedure TPaxCImporter.GenerateFunctions();
var
  LFunc: TCFunctionInfo;
  LParam: TCParamInfo;
  LReturnType: string;
  LParamStr: string;
  LI: Integer;
  LParamType: string;
  LParamName: string;
  LDll: string;
begin
  if FFunctions.Count = 0 then
    Exit;

  EmitLn('(* External routines *)');
  if FDllName <> '' then
    LDll := FDllName
  else
    LDll := FModuleName + '.dll';

  for LFunc in FFunctions do
  begin
    LParamStr := '';
    for LI := 0 to High(LFunc.Params) do
    begin
      LParam := LFunc.Params[LI];
      LParamType := MapCTypeToPax(LParam.TypeName, LParam.IsPointer, LParam.PointerDepth);
      if LParam.ParamName <> '' then
        LParamName := 'A' + LParam.ParamName
      else
        LParamName := Format('AParam%d', [LI]);
      if LI > 0 then
        LParamStr := LParamStr + '; ';
      LParamStr := LParamStr + Format('const %s: %s', [SanitizeIdentifier(LParamName), LParamType]);
    end;

    if LFunc.IsVariadic then
    begin
      if LParamStr <> '' then
        LParamStr := LParamStr + '; ...';
    end;

    if (LFunc.ReturnType = 'void') and not LFunc.ReturnIsPointer then
    begin
      EmitFmt('public routine %s(%s); external ''%s'';', [SanitizeIdentifier(LFunc.FuncName), LParamStr, LDll]);
    end
    else
    begin
      LReturnType := MapCTypeToPax(LFunc.ReturnType, LFunc.ReturnIsPointer, LFunc.ReturnPointerDepth);
      EmitFmt('public routine %s(%s): %s; external ''%s'';', [SanitizeIdentifier(LFunc.FuncName), LParamStr, LReturnType, LDll]);
    end;
  end;
  EmitLn();
end;

procedure TPaxCImporter.ParsePreprocessed(const APreprocessedSource: string);
begin
  FLexer.Tokenize(APreprocessedSource);
  FPos := 0;
  FCurrentToken := FLexer.GetToken(0);
  ParseTopLevel();
  GenerateModule();
  ProcessInsertions();
end;

procedure TPaxCImporter.AddIncludePath(const APath: string);
begin
  if not FIncludePaths.Contains(APath) then
    FIncludePaths.Add(APath);
end;

procedure TPaxCImporter.AddLibraryPath(const APath: string);
var
  LPath: string;
begin
  LPath := APath.Replace('\', '/');
  if not FLibraryPaths.Contains(LPath) then
    FLibraryPaths.Add(LPath);
end;

procedure TPaxCImporter.AddCopyDLL(const APath: string);
var
  LPath: string;
begin
  LPath := APath.Replace('\', '/');
  if not FCopyDLLs.Contains(LPath) then
    FCopyDLLs.Add(LPath);
end;

procedure TPaxCImporter.AddExcludedType(const ATypeName: string);
begin
  if not FExcludedTypes.Contains(ATypeName) then
    FExcludedTypes.Add(ATypeName);
end;

procedure TPaxCImporter.InsertTextAfter(const ATargetLine: string; const AText: string; const AOccurrence: Integer);
var
  LInfo: TInsertionInfo;
begin
  LInfo.TargetLine := ATargetLine;
  LInfo.Content := AText;
  LInfo.InsertBefore := False;
  LInfo.Occurrence := AOccurrence;
  FInsertions.Add(LInfo);
end;

procedure TPaxCImporter.InsertFileAfter(const ATargetLine: string; const AFilePath: string; const AOccurrence: Integer);
var
  LContent: string;
begin
  if TFile.Exists(AFilePath) then
  begin
    LContent := TFile.ReadAllText(AFilePath, TEncoding.UTF8);
    InsertTextAfter(ATargetLine, LContent, AOccurrence);
  end;
end;

procedure TPaxCImporter.InsertTextBefore(const ATargetLine: string; const AText: string; const AOccurrence: Integer);
var
  LInfo: TInsertionInfo;
begin
  LInfo.TargetLine := ATargetLine;
  LInfo.Content := AText;
  LInfo.InsertBefore := True;
  LInfo.Occurrence := AOccurrence;
  FInsertions.Add(LInfo);
end;

procedure TPaxCImporter.InsertFileBefore(const ATargetLine: string; const AFilePath: string; const AOccurrence: Integer);
var
  LContent: string;
begin
  if TFile.Exists(AFilePath) then
  begin
    LContent := TFile.ReadAllText(AFilePath, TEncoding.UTF8);
    InsertTextBefore(ATargetLine, LContent, AOccurrence);
  end;
end;

procedure TPaxCImporter.ProcessInsertions();
var
  LOutput: string;
  LInsertion: TInsertionInfo;
  LOccurrenceCount: Integer;
  LTargetTrimmed: string;
  LLines: TArray<string>;
  LLine: string;
  LTrimmedLine: string;
  LResult: TStringBuilder;
  LI: Integer;
  LInserted: Boolean;
begin
  if FInsertions.Count = 0 then
    Exit;

  LOutput := FOutput.ToString();

  for LInsertion in FInsertions do
  begin
    LTargetTrimmed := LowerCase(Trim(LInsertion.TargetLine));
    LOccurrenceCount := 0;
    LInserted := False;

    // Split preserving empty lines
    LLines := LOutput.Split([#13#10, #10], TStringSplitOptions.None);

    LResult := TStringBuilder.Create();
    try
      for LI := 0 to High(LLines) do
      begin
        LLine := LLines[LI];
        LTrimmedLine := LowerCase(Trim(LLine));

        if (not LInserted) and (LTrimmedLine = LTargetTrimmed) then
        begin
          Inc(LOccurrenceCount);
          if LOccurrenceCount = LInsertion.Occurrence then
          begin
            if LInsertion.InsertBefore then
            begin
              LResult.Append(LInsertion.Content);
              LResult.AppendLine(LLine);
            end
            else
            begin
              LResult.AppendLine(LLine);
              LResult.Append(LInsertion.Content);
            end;
            LInserted := True;
          end
          else
          begin
            LResult.AppendLine(LLine);
          end;
        end
        else
        begin
          // Don't add newline after last line
          if LI < High(LLines) then
            LResult.AppendLine(LLine)
          else
            LResult.Append(LLine);
        end;
      end;

      LOutput := LResult.ToString();
    finally
      LResult.Free();
    end;
  end;

  // Update output
  FOutput.Clear();
  FOutput.Append(LOutput);
end;

procedure TPaxCImporter.SetOutputPath(const APath: string);
begin
  FOutputPath := APath.Replace('\', '/');
end;

function TPaxCImporter.ImportHeader(const AHeaderFile: string): Boolean;
var
  LPreprocessedSource: string;
  LHeaderName: string;
  LOutputFile: string;
  LOutputDir: string;
begin
  Result := False;
  FLastError := '';

  FLexer.Clear();
  FOutput.Clear();
  FStructs.Clear();
  FEnums.Clear();
  FTypedefs.Clear();
  FFunctions.Clear();
  FForwardDecls.Clear();
  FPos := 0;
  FIndent := 0;

  LHeaderName := TPath.GetFileNameWithoutExtension(AHeaderFile);

  if FModuleName = '' then
    FModuleName := LHeaderName;

  if FOutputPath <> '' then
    LOutputDir := FOutputPath
  else
    LOutputDir := TPath.GetDirectoryName(TPath.GetFullPath(AHeaderFile));

  LOutputFile := TPath.Combine(LOutputDir, LHeaderName + '.pax');

  if not PreprocessHeader(AHeaderFile, LPreprocessedSource) then
    Exit;

  ParsePreprocessed(LPreprocessedSource);

  try
    TUtils.CreateDirInPath(LOutputFile);
    TFile.WriteAllText(LOutputFile, FOutput.ToString(), TEncoding.UTF8);
  except
    on E: Exception do
    begin
      FLastError := 'Failed to write output file: ' + E.Message;
      Exit;
    end;
  end;

  Result := True;
end;

procedure TPaxCImporter.SetModuleName(const AName: string);
begin
  FModuleName := AName;
end;

procedure TPaxCImporter.SetDllName(const ADllName: string);
begin
  FDllName := ADllName;
end;

function TPaxCImporter.GetLastError(): string;
begin
  Result := FLastError;
end;

procedure TPaxCImporter.Clear();
begin
  FLexer.Clear();
  FOutput.Clear();
  FStructs.Clear();
  FEnums.Clear();
  FTypedefs.Clear();
  FFunctions.Clear();
  FForwardDecls.Clear();
  FInsertions.Clear();
  FIncludePaths.Clear();
  FLibraryPaths.Clear();
  FCopyDLLs.Clear();
  FExcludedTypes.Clear();
  FModuleName := '';
  FDllName := '';
  FOutputPath := '';
  FLastError := '';
  FPos := 0;
  FIndent := 0;
end;

end.
