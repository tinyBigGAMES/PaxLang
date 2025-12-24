{===============================================================================
  Pax™ Programming Language.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://paxlang.org

  See LICENSE for license information
===============================================================================}

unit Pax.Parser;

{$I Pax.Defines.inc}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Pax.Utils,
  Pax.Errors,
  Pax.Resources,
  Pax.Lexer,
  Pax.AST;

type

  { TPaxParser }
  TPaxParser = class(TBaseObject)
  private
    FLexer: TPaxLexer;
    FErrors: TErrors;
    FPos: Integer;
    FCurrentToken: TToken;

    // Stats
    FImportCount: Integer;
    FConstCount: Integer;
    FTypeCount: Integer;
    FVarCount: Integer;
    FRoutineCount: Integer;

    // Token navigation
    function IsAtEnd(): Boolean;
    function Peek(): TToken;
    function PeekNext(): TToken;
    function Previous(): TToken;
    procedure Advance();
    function Check(const AKind: TTokenKind): Boolean;
    function Match(const AKind: TTokenKind): Boolean;
    function MatchAny(const AKinds: array of TTokenKind): Boolean;
    function Expect(const AKind: TTokenKind): TToken;
    function ExpectIdentifier(): TToken;

    // Error handling
    procedure Error(const AMessage: string); overload;
    procedure Error(const AMessage: string; const AArgs: array of const); overload;
    procedure ErrorAt(const AToken: TToken; const AMessage: string); overload;
    procedure ErrorAt(const AToken: TToken; const AMessage: string; const AArgs: array of const); overload;
    procedure Synchronize();

    // Module level
    function ParseModule(): PASTNode;
    function ParseModuleKind(): TTokenKind;
    function ParseImportClause(): PASTNode;
    function ParseDirective(): PASTNode;
    function ParseDirectiveValue(): string;

    // Declarations
    function ParseDeclaration(): PASTNode;
    function ParseConstSection(const AIsPublic: Boolean): PASTNode;
    function ParseConstDecl(const AIsPublic: Boolean): PASTNode;
    function ParseTypeSection(const AIsPublic: Boolean): PASTNode;
    function ParseTypeDecl(const AIsPublic: Boolean): PASTNode;
    function ParseVarSection(const AIsPublic: Boolean): PASTNode;
    function ParseVarDecl(const AIsPublic: Boolean): PASTNode;
    function ParseRoutineDecl(const AIsPublic: Boolean): PASTNode;
    function ParseParamList(var AIsVariadic: Boolean): TArray<PASTNode>;
    function ParseParam(): PASTNode;
    function ParseLocalVarSection(): TArray<PASTNode>;
    function ParseTestBlock(): PASTNode;

    // Types
    function ParseTypeDef(): PASTNode;
    function ParseRecordType(): PASTNode;
    function ParseFieldDecl(): PASTNode;
    function ParseArrayType(): PASTNode;
    function ParsePointerType(): PASTNode;
    function ParseSetType(): PASTNode;
    function ParseRoutineType(): PASTNode;
    function ParseTypeName(): PASTNode;

    // Statements
    function ParseBlock(): PASTNode;
    function ParseStatement(): PASTNode;
    function ParseIfStmt(): PASTNode;
    function ParseWhileStmt(): PASTNode;
    function ParseForStmt(): PASTNode;
    function ParseRepeatStmt(): PASTNode;
    function ParseCaseStmt(): PASTNode;
    function ParseCaseArm(): PASTNode;
    function ParseReturnStmt(): PASTNode;
    function ParseNewStmt(): PASTNode;
    function ParseDisposeStmt(): PASTNode;
    function ParseSetLengthStmt(): PASTNode;
    function ParseGcCollectStmt(): PASTNode;
    function ParseGcDumpStmt(): PASTNode;
    function ParseAssignmentOrCall(): PASTNode;

    // Expressions
    function ParseExpression(): PASTNode;
    function ParseSimpleExpr(): PASTNode;
    function ParseTerm(): PASTNode;
    function ParseFactor(): PASTNode;
    function ParsePrimary(): PASTNode;
    function ParseDesignator(): PASTNode;
    function ParseDesignatorSuffix(const ALeft: PASTNode): PASTNode;
    function ParseSetLiteral(): PASTNode;
    function ParseArgList(): TArray<PASTNode>;

    // Type casting
    function IsBuiltInType(): Boolean;
    function ParseTypeCast(): PASTNode;

  public
    constructor Create(); override;
    destructor Destroy(); override;

    procedure SetLexer(const ALexer: TPaxLexer);
    procedure SetErrors(const AErrors: TErrors);

    function Parse(): PASTNode;

    // Stats properties
    property ImportCount: Integer read FImportCount;
    property ConstCount: Integer read FConstCount;
    property TypeCount: Integer read FTypeCount;
    property VarCount: Integer read FVarCount;
    property RoutineCount: Integer read FRoutineCount;
  end;

implementation

{ TPaxParser }

constructor TPaxParser.Create();
begin
  inherited;

  FLexer := nil;
  FErrors := nil;
  FPos := 0;
end;

destructor TPaxParser.Destroy();
begin
  inherited;
end;

procedure TPaxParser.SetLexer(const ALexer: TPaxLexer);
begin
  FLexer := ALexer;
end;

procedure TPaxParser.SetErrors(const AErrors: TErrors);
begin
  FErrors := AErrors;
end;

function TPaxParser.IsAtEnd(): Boolean;
begin
  Result := FCurrentToken.Kind = tkEOF;
end;

function TPaxParser.Peek(): TToken;
begin
  Result := FCurrentToken;
end;

function TPaxParser.PeekNext(): TToken;
begin
  if FPos + 1 < FLexer.GetTokenCount() then
    Result := FLexer.GetToken(FPos + 1)
  else
  begin
    Result.Kind := tkEOF;
    Result.Lexeme := '';
    Result.Range.Clear();
  end;
end;

function TPaxParser.Previous(): TToken;
begin
  if FPos > 0 then
    Result := FLexer.GetToken(FPos - 1)
  else
    Result := FCurrentToken;
end;

procedure TPaxParser.Advance();
begin
  if not IsAtEnd() then
  begin
    Inc(FPos);
    FCurrentToken := FLexer.GetToken(FPos);
  end;
end;

function TPaxParser.Check(const AKind: TTokenKind): Boolean;
begin
  Result := FCurrentToken.Kind = AKind;
end;

function TPaxParser.Match(const AKind: TTokenKind): Boolean;
begin
  if Check(AKind) then
  begin
    Advance();
    Result := True;
  end
  else
    Result := False;
end;

function TPaxParser.MatchAny(const AKinds: array of TTokenKind): Boolean;
var
  LKind: TTokenKind;
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

function TPaxParser.Expect(const AKind: TTokenKind): TToken;
begin
  if Check(AKind) then
  begin
    Result := FCurrentToken;
    Advance();
  end
  else
  begin
    Error(RSParserExpectedToken, [TokenKindToString(AKind), TokenKindToString(FCurrentToken.Kind)]);
    Result := FCurrentToken;
    // Advance on error to prevent infinite loops
    if not IsAtEnd() then
      Advance();
  end;
end;

function TPaxParser.ExpectIdentifier(): TToken;
begin
  if Check(tkIdentifier) then
  begin
    Result := FCurrentToken;
    Advance();
  end
  else
  begin
    Error(RSParserExpectedIdentifier);
    Result := FCurrentToken;
    // Advance on error to prevent infinite loops
    if not IsAtEnd() then
      Advance();
  end;
end;

procedure TPaxParser.Error(const AMessage: string);
begin
  ErrorAt(FCurrentToken, AMessage);
end;

procedure TPaxParser.Error(const AMessage: string; const AArgs: array of const);
begin
  ErrorAt(FCurrentToken, Format(AMessage, AArgs));
end;

procedure TPaxParser.ErrorAt(const AToken: TToken; const AMessage: string);
begin
  if FErrors = nil then
    Exit;

  if FErrors.ReachedMaxErrors() then
    Exit;

  FErrors.Add(AToken.Range, esError, 'E050', AMessage);
end;

procedure TPaxParser.ErrorAt(const AToken: TToken; const AMessage: string; const AArgs: array of const);
begin
  ErrorAt(AToken, Format(AMessage, AArgs));
end;

procedure TPaxParser.Synchronize();
begin
  Advance();

  while not IsAtEnd() do
  begin
    // Stop at statement boundaries
    if Previous().Kind = tkSemicolon then
      Exit;

    // Stop at declaration/statement keywords
    if FCurrentToken.Kind in [tkConst, tkType, tkVar, tkRoutine, tkBegin, tkEnd,
                              tkIf, tkWhile, tkFor, tkRepeat, tkCase, tkReturn,
                              tkTest, tkPublic] then
      Exit;

    Advance();
  end;
end;

function TPaxParser.Parse(): PASTNode;
begin
  if FLexer = nil then
  begin
    if FErrors <> nil then
      FErrors.Add(esError, ERR_PARSER_LEXER_NOT_SET, RSParserLexerNotSet);
    Exit(nil);
  end;

  if FLexer.GetTokenCount() = 0 then
  begin
    if FErrors <> nil then
      FErrors.Add(esError, ERR_PARSER_FAILED, RSParserNoTokens);
    Exit(nil);
  end;

  // Reset stats
  FImportCount := 0;
  FConstCount := 0;
  FTypeCount := 0;
  FVarCount := 0;
  FRoutineCount := 0;

  FPos := 0;
  FCurrentToken := FLexer.GetToken(0);

  Result := ParseModule();
end;

function TPaxParser.ParseModule(): PASTNode;
var
  LModuleToken: TToken;
  LModuleKind: TTokenKind;
  LNameToken: TToken;
  LDirective: PASTNode;
  LImports: PASTNode;
  LDecl: PASTNode;
  LBlock: PASTNode;
  LTest: PASTNode;
begin
  // "module" [ModuleKind] Identifier ";"
  LModuleToken := Expect(tkModule);
  Result := CreateASTNodeWithToken(nkModule, LModuleToken);

  // Optional module kind
  LModuleKind := ParseModuleKind();
  if LModuleKind <> tkEOF then
    Result^.StrVal := TokenKindToString(LModuleKind);

  // Module name
  LNameToken := ExpectIdentifier();
  Result^.NodeName := LNameToken.Lexeme;

  Expect(tkSemicolon);

  // Directives
  while Check(tkDirective) do
  begin
    LDirective := ParseDirective();
    if LDirective <> nil then
      AddASTChild(Result, LDirective);
  end;

  // Import clause
  if Check(tkImport) then
  begin
    LImports := ParseImportClause();
    if LImports <> nil then
      AddASTChild(Result, LImports);
  end;

  // Declarations
  while not IsAtEnd() and not Check(tkBegin) and not Check(tkEnd) and not Check(tkTest) do
  begin
    // Bail out if too many errors
    if (FErrors <> nil) and FErrors.ReachedMaxErrors() then
      Break;

    LDecl := ParseDeclaration();
    if LDecl <> nil then
      AddASTChild(Result, LDecl)
    else
      Synchronize();
  end;

  // Optional initialization block
  if Match(tkBegin) then
  begin
    LBlock := ParseBlock();
    if LBlock <> nil then
      AddASTChild(Result, LBlock);

    Expect(tkEnd);
  end
  else
  begin
    Expect(tkEnd);
  end;

  Expect(tkDot);

  // Test blocks
  while Check(tkTest) do
  begin
    LTest := ParseTestBlock();
    if LTest <> nil then
      AddASTChild(Result, LTest);
  end;
end;

function TPaxParser.ParseModuleKind(): TTokenKind;
begin
  if Check(tkExe) then
  begin
    Advance();
    Result := tkExe;
  end
  else if Check(tkDll) then
  begin
    Advance();
    Result := tkDll;
  end
  else if Check(tkLib) then
  begin
    Advance();
    Result := tkLib;
  end
  else
    Result := tkEOF;
end;

function TPaxParser.ParseImportClause(): PASTNode;
var
  LToken: TToken;
  LImportNode: PASTNode;
begin
  // "import" Identifier {"," Identifier} ";"
  LToken := Expect(tkImport);
  Result := CreateASTNodeWithToken(nkBlock, LToken);
  Result^.NodeName := 'imports';

  repeat
    LToken := ExpectIdentifier();
    LImportNode := CreateASTNodeWithToken(nkImport, LToken);
    LImportNode^.NodeName := LToken.Lexeme;
    AddASTChild(Result, LImportNode);
    Inc(FImportCount);
  until not Match(tkComma);

  Expect(tkSemicolon);
end;

function TPaxParser.ParseDirective(): PASTNode;
var
  LToken: TToken;
  LValue: string;
begin
  // Directive = "#name" [value]
  LToken := FCurrentToken;
  Advance();

  Result := CreateASTNodeWithToken(nkDirective, LToken);
  Result^.NodeName := LToken.Lexeme;

  // Parse directive value if present
  LValue := ParseDirectiveValue();
  if LValue <> '' then
    Result^.StrVal := LValue;
end;

function TPaxParser.ParseDirectiveValue(): string;
var
  LLower: string;
begin
  Result := '';

  // Handle different directive types
  LLower := LowerCase(Previous().Lexeme);

  if (LLower = '#apptype') or (LLower = '#subsystem') then
  begin
    // Expects: console | gui
    if Check(tkIdentifier) then
    begin
      Result := FCurrentToken.Lexeme;
      Advance();
    end
    else
    begin
      Error(RSParserExpectedConsoleOrGui);
      Exit;
    end;
  end
  else if (LLower = '#debug') or (LLower = '#unittestmode') then
  begin
    // Expects: on | off
    if Check(tkIdentifier) then
    begin
      Result := FCurrentToken.Lexeme;
      Advance();
    end
    else
    begin
      Error(RSParserExpectedOnOrOff);
      Exit;
    end;
  end
  else if (LLower = '#define') then
  begin
    // Expects: Identifier [value]
    if Check(tkIdentifier) then
    begin
      Result := FCurrentToken.Lexeme;
      Advance();

      // Optional value - consume tokens until end of directive context
      while Check(tkString) or Check(tkIdentifier) or Check(tkInteger) or
            Check(tkFloat) or Check(tkPlus) or Check(tkMinus) or
            Check(tkStar) or Check(tkSlash) or Check(tkLParen) or Check(tkRParen) do
      begin
        Result := Result + ' ' + FCurrentToken.Lexeme;
        Advance();
      end;
    end
    else
    begin
      Error(RSParserExpectedIdentifier);
      Exit;
    end;
  end
  else if (LLower = '#undef') or (LLower = '#ifdef') or (LLower = '#ifndef') then
  begin
    // Expects: Identifier
    if Check(tkIdentifier) then
    begin
      Result := FCurrentToken.Lexeme;
      Advance();
    end
    else
    begin
      Error(RSParserExpectedIdentifier);
      Exit;
    end;
  end
  else if (LLower = '#if') or (LLower = '#elif') then
  begin
    // Expects: expression - consume tokens until end of directive context
    while Check(tkIdentifier) or Check(tkInteger) or Check(tkFloat) or
          Check(tkPlus) or Check(tkMinus) or Check(tkStar) or Check(tkSlash) or
          Check(tkLParen) or Check(tkRParen) or Check(tkEq) or Check(tkNe) or
          Check(tkLt) or Check(tkGt) or Check(tkLe) or Check(tkGe) or
          Check(tkAnd) or Check(tkOr) or Check(tkNot) do
    begin
      if Result <> '' then
        Result := Result + ' ';
      Result := Result + FCurrentToken.Lexeme;
      Advance();
    end;
    if Result = '' then
    begin
      Error(RSParserExpectedExpression);
      Exit;
    end;
  end
  else if (LLower = '#else') or (LLower = '#endif') then
  begin
    // No value expected
    Result := '';
  end
  else if LLower = '#maxerrors' then
  begin
    // Expects: integer
    if Check(tkInteger) then
    begin
      Result := FCurrentToken.Lexeme;
      Advance();
    end;
  end
  else
  begin
    // String value directives: #modulepath, #includepath, #librarypath, #library, #addfile, #option
    if Check(tkString) then
    begin
      Result := FCurrentToken.Lexeme;
      Advance();
    end;
  end;
end;

function TPaxParser.ParseDeclaration(): PASTNode;
var
  LIsPublic: Boolean;
begin
  Result := nil;
  LIsPublic := False;

  // Optional "public"
  if Match(tkPublic) then
    LIsPublic := True;

  if Check(tkConst) then
    Result := ParseConstSection(LIsPublic)
  else if Check(tkType) then
    Result := ParseTypeSection(LIsPublic)
  else if Check(tkVar) then
    Result := ParseVarSection(LIsPublic)
  else if Check(tkRoutine) then
    Result := ParseRoutineDecl(LIsPublic)
  else
  begin
    Error(RSParserExpectedDeclaration, [TokenKindToString(FCurrentToken.Kind)]);
    Exit;
  end;
end;

function TPaxParser.ParseConstSection(const AIsPublic: Boolean): PASTNode;
var
  LToken: TToken;
  LDecl: PASTNode;
  LDeclPublic: Boolean;
begin
  // "const" {["public"] ConstDecl}
  LToken := Expect(tkConst);
  Result := CreateASTNodeWithToken(nkBlock, LToken);
  Result^.NodeName := 'consts';
  Result^.IsPublic := AIsPublic;

  while Check(tkIdentifier) or Check(tkPublic) do
  begin
    LDeclPublic := AIsPublic;

    if Match(tkPublic) then
      LDeclPublic := True;

    LDecl := ParseConstDecl(LDeclPublic);
    if LDecl <> nil then
      AddASTChild(Result, LDecl);
  end;
end;

function TPaxParser.ParseConstDecl(const AIsPublic: Boolean): PASTNode;
var
  LToken: TToken;
  LTypeNode: PASTNode;
  LValueNode: PASTNode;
begin
  // Identifier [":" TypeName] "=" Expression ";"
  LToken := ExpectIdentifier();
  Result := CreateASTNodeWithToken(nkConstDecl, LToken);
  Result^.NodeName := LToken.Lexeme;
  Result^.IsPublic := AIsPublic;

  Inc(FConstCount);

  // Optional type
  if Match(tkColon) then
  begin
    LTypeNode := ParseTypeName();
    if LTypeNode <> nil then
      AddASTChild(Result, LTypeNode);
  end;

  Expect(tkEq);

  // Value expression
  LValueNode := ParseExpression();
  if LValueNode <> nil then
    AddASTChild(Result, LValueNode);

  Expect(tkSemicolon);
end;

function TPaxParser.ParseTypeSection(const AIsPublic: Boolean): PASTNode;
var
  LToken: TToken;
  LDecl: PASTNode;
  LDeclPublic: Boolean;
begin
  // "type" {["public"] TypeDecl}
  LToken := Expect(tkType);
  Result := CreateASTNodeWithToken(nkBlock, LToken);
  Result^.NodeName := 'types';
  Result^.IsPublic := AIsPublic;

  while Check(tkIdentifier) or Check(tkPublic) do
  begin
    LDeclPublic := AIsPublic;

    if Match(tkPublic) then
      LDeclPublic := True;

    LDecl := ParseTypeDecl(LDeclPublic);
    if LDecl <> nil then
      AddASTChild(Result, LDecl);
  end;
end;

function TPaxParser.ParseTypeDecl(const AIsPublic: Boolean): PASTNode;
var
  LToken: TToken;
  LTypeDefNode: PASTNode;
begin
  // Identifier "=" TypeDef ";"
  LToken := ExpectIdentifier();
  Result := CreateASTNodeWithToken(nkTypeDecl, LToken);
  Result^.NodeName := LToken.Lexeme;
  Result^.IsPublic := AIsPublic;

  Inc(FTypeCount);

  Expect(tkEq);

  LTypeDefNode := ParseTypeDef();
  if LTypeDefNode <> nil then
    AddASTChild(Result, LTypeDefNode);

  Expect(tkSemicolon);
end;

function TPaxParser.ParseVarSection(const AIsPublic: Boolean): PASTNode;
var
  LToken: TToken;
  LDecl: PASTNode;
  LDeclPublic: Boolean;
begin
  // "var" {["public"] VarDecl}
  LToken := Expect(tkVar);
  Result := CreateASTNodeWithToken(nkBlock, LToken);
  Result^.NodeName := 'vars';
  Result^.IsPublic := AIsPublic;

  while Check(tkIdentifier) or Check(tkPublic) do
  begin
    LDeclPublic := AIsPublic;

    if Match(tkPublic) then
      LDeclPublic := True;

    LDecl := ParseVarDecl(LDeclPublic);
    if LDecl <> nil then
      AddASTChild(Result, LDecl);
  end;
end;

function TPaxParser.ParseVarDecl(const AIsPublic: Boolean): PASTNode;
var
  LToken: TToken;
  LTypeNode: PASTNode;
  LValueNode: PASTNode;
begin
  // Identifier ":" TypeName ["=" Expression] ";"
  LToken := ExpectIdentifier();
  Result := CreateASTNodeWithToken(nkVarDecl, LToken);
  Result^.NodeName := LToken.Lexeme;
  Result^.IsPublic := AIsPublic;

  Inc(FVarCount);

  Expect(tkColon);

  LTypeNode := ParseTypeName();
  if LTypeNode <> nil then
    AddASTChild(Result, LTypeNode);

  // Optional initializer
  if Match(tkEq) then
  begin
    LValueNode := ParseExpression();
    if LValueNode <> nil then
      AddASTChild(Result, LValueNode);
  end;

  Expect(tkSemicolon);
end;

function TPaxParser.ParseRoutineDecl(const AIsPublic: Boolean): PASTNode;
var
  LToken: TToken;
  LNameToken: TToken;
  LParams: TArray<PASTNode>;
  LReturnType: PASTNode;
  LLocalVars: TArray<PASTNode>;
  LBody: PASTNode;
  LParamsBlock: PASTNode;
  LLocalsBlock: PASTNode;
  LI: Integer;
  LIsVariadic: Boolean;
begin
  // "routine" Identifier ["(" [ParamList] ")"] [":" TypeName] ";"
  // ("external" StringLiteral ";" | [LocalVarSection] "begin" Block "end" ";")
  LToken := Expect(tkRoutine);
  Result := CreateASTNodeWithToken(nkRoutineDecl, LToken);
  Result^.IsPublic := AIsPublic;

  Inc(FRoutineCount);

  LNameToken := ExpectIdentifier();
  Result^.NodeName := LNameToken.Lexeme;

  // Parameters
  LIsVariadic := False;
  if Match(tkLParen) then
  begin
    if not Check(tkRParen) then
      LParams := ParseParamList(LIsVariadic)
    else
      SetLength(LParams, 0);

    Expect(tkRParen);

    // Store params as block
    if Length(LParams) > 0 then
    begin
      LParamsBlock := CreateASTNode(nkBlock);
      LParamsBlock^.NodeName := 'params';
      for LI := 0 to High(LParams) do
        AddASTChild(LParamsBlock, LParams[LI]);
      AddASTChild(Result, LParamsBlock);
    end;
  end;

  // Set variadic flag
  Result^.IsVariadic := LIsVariadic;

  // Return type
  if Match(tkColon) then
  begin
    LReturnType := ParseTypeName();
    if LReturnType <> nil then
      AddASTChild(Result, LReturnType);
  end;

  Expect(tkSemicolon);

  // External or body
  if Match(tkExternal) then
  begin
    Result^.IsExternal := True;

    if Check(tkString) then
    begin
      Result^.ExternalLib := FCurrentToken.Lexeme;
      Advance();
    end;
    // If no string, ExternalLib remains empty - routine is extern without specific DLL

    Expect(tkSemicolon);
  end
  else
  begin
    // Local variables
    if Check(tkVar) then
    begin
      Advance();
      LLocalVars := ParseLocalVarSection();

      if Length(LLocalVars) > 0 then
      begin
        LLocalsBlock := CreateASTNode(nkBlock);
        LLocalsBlock^.NodeName := 'locals';
        for LI := 0 to High(LLocalVars) do
          AddASTChild(LLocalsBlock, LLocalVars[LI]);
        AddASTChild(Result, LLocalsBlock);
      end;
    end;

    // Body
    Expect(tkBegin);
    LBody := ParseBlock();
    if LBody <> nil then
      AddASTChild(Result, LBody);
    Expect(tkEnd);
    Expect(tkSemicolon);
  end;
end;

function TPaxParser.ParseParamList(var AIsVariadic: Boolean): TArray<PASTNode>;
var
  LParam: PASTNode;
begin
  SetLength(Result, 0);
  AIsVariadic := False;

  repeat
    // Check for ellipsis (varargs)
    if Check(tkEllipsis) then
    begin
      Advance();
      AIsVariadic := True;
      Break; // ... must be last
    end;

    LParam := ParseParam();
    if LParam <> nil then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := LParam;
    end;
  until not Match(tkSemicolon);
end;

function TPaxParser.ParseParam(): PASTNode;
var
  LToken: TToken;
  LTypeNode: PASTNode;
begin
  // ["var" | "const"] Identifier ":" TypeName
  Result := nil;

  // Check for var/const modifier
  if Match(tkVar) then
  begin
    LToken := ExpectIdentifier();
    Result := CreateASTNodeWithToken(nkParamDecl, LToken);
    Result^.NodeName := LToken.Lexeme;
    Result^.StrVal := 'var';
  end
  else if Match(tkConst) then
  begin
    LToken := ExpectIdentifier();
    Result := CreateASTNodeWithToken(nkParamDecl, LToken);
    Result^.NodeName := LToken.Lexeme;
    Result^.StrVal := 'const';
  end
  else
  begin
    LToken := ExpectIdentifier();
    Result := CreateASTNodeWithToken(nkParamDecl, LToken);
    Result^.NodeName := LToken.Lexeme;
    Result^.StrVal := '';
  end;

  Expect(tkColon);

  LTypeNode := ParseTypeName();
  if LTypeNode <> nil then
    AddASTChild(Result, LTypeNode);
end;

function TPaxParser.ParseLocalVarSection(): TArray<PASTNode>;
var
  LDecl: PASTNode;
begin
  // {VarDecl}
  SetLength(Result, 0);

  while Check(tkIdentifier) do
  begin
    LDecl := ParseVarDecl(False);
    if LDecl <> nil then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := LDecl;
    end;
  end;
end;

function TPaxParser.ParseTestBlock(): PASTNode;
var
  LToken: TToken;
  LLocalVars: TArray<PASTNode>;
  LLocalsBlock: PASTNode;
  LBody: PASTNode;
  LI: Integer;
begin
  // "test" StringLiteral [LocalVarSection] "begin" Block "end" ";"
  LToken := Expect(tkTest);
  Result := CreateASTNodeWithToken(nkTestBlock, LToken);

  if Check(tkString) then
  begin
    Result^.NodeName := FCurrentToken.Lexeme;
    Advance();
  end
  else
  begin
    Error(RSParserExpectedTestName);
    Exit;
  end;

  // Local variables
  if Check(tkVar) then
  begin
    Advance();
    LLocalVars := ParseLocalVarSection();

    if Length(LLocalVars) > 0 then
    begin
      LLocalsBlock := CreateASTNode(nkBlock);
      LLocalsBlock^.NodeName := 'locals';
      for LI := 0 to High(LLocalVars) do
        AddASTChild(LLocalsBlock, LLocalVars[LI]);
      AddASTChild(Result, LLocalsBlock);
    end;
  end;

  Expect(tkBegin);
  LBody := ParseBlock();
  if LBody <> nil then
    AddASTChild(Result, LBody);
  Expect(tkEnd);
  Expect(tkSemicolon);
end;

function TPaxParser.ParseTypeDef(): PASTNode;
begin
  // RecordType | ArrayType | PointerType | SetType | RoutineType | TypeName
  if Check(tkRecord) then
    Result := ParseRecordType()
  else if Check(tkArray) then
    Result := ParseArrayType()
  else if Check(tkPointer) then
    Result := ParsePointerType()
  else if Check(tkSet) then
    Result := ParseSetType()
  else if Check(tkRoutine) then
    Result := ParseRoutineType()
  else
    Result := ParseTypeName();
end;

function TPaxParser.ParseRecordType(): PASTNode;
var
  LToken: TToken;
  LParentType: PASTNode;
  LField: PASTNode;
begin
  // "record" ["(" TypeName ")"] {FieldDecl} "end"
  LToken := Expect(tkRecord);
  Result := CreateASTNodeWithToken(nkRecordType, LToken);

  // Optional parent type
  if Match(tkLParen) then
  begin
    LParentType := ParseTypeName();
    if LParentType <> nil then
    begin
      LParentType^.NodeName := 'parent';
      AddASTChild(Result, LParentType);
    end;
    Expect(tkRParen);
  end;

  // Fields
  while Check(tkIdentifier) do
  begin
    LField := ParseFieldDecl();
    if LField <> nil then
      AddASTChild(Result, LField);
  end;

  Expect(tkEnd);
end;

function TPaxParser.ParseFieldDecl(): PASTNode;
var
  LToken: TToken;
  LTypeNode: PASTNode;
begin
  // Identifier ":" TypeName ";"
  LToken := ExpectIdentifier();
  Result := CreateASTNodeWithToken(nkFieldDecl, LToken);
  Result^.NodeName := LToken.Lexeme;

  Expect(tkColon);

  LTypeNode := ParseTypeName();
  if LTypeNode <> nil then
    AddASTChild(Result, LTypeNode);

  Expect(tkSemicolon);
end;

function TPaxParser.ParseArrayType(): PASTNode;
var
  LToken: TToken;
  LLowBound: PASTNode;
  LHighBound: PASTNode;
  LElementType: PASTNode;
begin
  // "array" ["[" [ArrayBounds] "]"] "of" TypeName
  LToken := Expect(tkArray);
  Result := CreateASTNodeWithToken(nkArrayType, LToken);

  // Optional bounds
  if Match(tkLBracket) then
  begin
    if Check(tkInteger) then
    begin
      // Static array bounds
      LLowBound := CreateASTNodeWithToken(nkIntLiteral, FCurrentToken);
      LLowBound^.IntVal := FCurrentToken.IntValue;
      AddASTChild(Result, LLowBound);
      Advance();

      Expect(tkDotDot);

      if Check(tkInteger) then
      begin
        LHighBound := CreateASTNodeWithToken(nkIntLiteral, FCurrentToken);
        LHighBound^.IntVal := FCurrentToken.IntValue;
        AddASTChild(Result, LHighBound);
        Advance();
      end
      else
      begin
        Error(RSParserInvalidArrayBounds);
        Exit;
      end;
    end;
    // Empty brackets = dynamic array

    Expect(tkRBracket);
  end;

  Expect(tkOf);

  LElementType := ParseTypeName();
  if LElementType <> nil then
    AddASTChild(Result, LElementType);
end;

function TPaxParser.ParsePointerType(): PASTNode;
var
  LToken: TToken;
  LTargetType: PASTNode;
begin
  // "pointer" ["to" TypeName]
  LToken := Expect(tkPointer);
  Result := CreateASTNodeWithToken(nkPointerType, LToken);

  if Match(tkTo) then
  begin
    LTargetType := ParseTypeName();
    if LTargetType <> nil then
      AddASTChild(Result, LTargetType);
  end;
end;

function TPaxParser.ParseSetType(): PASTNode;
var
  LToken: TToken;
  LLowBound: PASTNode;
  LHighBound: PASTNode;
  LElementType: PASTNode;
begin
  // "set" "of" (IntegerLiteral ".." IntegerLiteral | TypeName)
  LToken := Expect(tkSet);
  Result := CreateASTNodeWithToken(nkSetType, LToken);

  Expect(tkOf);

  // Check if it's a range or a type name
  if Check(tkInteger) then
  begin
    LLowBound := CreateASTNodeWithToken(nkIntLiteral, FCurrentToken);
    LLowBound^.IntVal := FCurrentToken.IntValue;
    AddASTChild(Result, LLowBound);
    Advance();

    Expect(tkDotDot);

    if Check(tkInteger) then
    begin
      LHighBound := CreateASTNodeWithToken(nkIntLiteral, FCurrentToken);
      LHighBound^.IntVal := FCurrentToken.IntValue;
      AddASTChild(Result, LHighBound);
      Advance();
    end
    else
    begin
      Error(RSParserExpectedSetRange);
      Exit;
    end;
  end
  else
  begin
    LElementType := ParseTypeName();
    if LElementType <> nil then
      AddASTChild(Result, LElementType);
  end;
end;

function TPaxParser.ParseRoutineType(): PASTNode;
var
  LToken: TToken;
  LParams: TArray<PASTNode>;
  LParamsBlock: PASTNode;
  LReturnType: PASTNode;
  LI: Integer;
  LIsVariadic: Boolean;
begin
  // "routine" "(" [ParamList] ")" [":" TypeName]
  LToken := Expect(tkRoutine);
  Result := CreateASTNodeWithToken(nkRoutineType, LToken);

  Expect(tkLParen);

  LIsVariadic := False;
  if not Check(tkRParen) then
    LParams := ParseParamList(LIsVariadic)
  else
    SetLength(LParams, 0);

  Expect(tkRParen);

  // Set variadic flag
  Result^.IsVariadic := LIsVariadic;

  if Length(LParams) > 0 then
  begin
    LParamsBlock := CreateASTNode(nkBlock);
    LParamsBlock^.NodeName := 'params';
    for LI := 0 to High(LParams) do
      AddASTChild(LParamsBlock, LParams[LI]);
    AddASTChild(Result, LParamsBlock);
  end;

  if Match(tkColon) then
  begin
    LReturnType := ParseTypeName();
    if LReturnType <> nil then
      AddASTChild(Result, LReturnType);
  end;
end;

function TPaxParser.ParseTypeName(): PASTNode;
var
  LToken: TToken;
  LTargetType: PASTNode;
begin
  // "pointer" ["to" TypeName]
  // | "array" ["[" [ArrayBounds] "]"] "of" TypeName
  // | "set" "of" (...)
  // | QualifiedIdent

  if Check(tkPointer) then
  begin
    LToken := FCurrentToken;
    Advance();
    Result := CreateASTNodeWithToken(nkPointerType, LToken);

    if Match(tkTo) then
    begin
      LTargetType := ParseTypeName();
      if LTargetType <> nil then
        AddASTChild(Result, LTargetType);
    end;
  end
  else if Check(tkArray) then
  begin
    Result := ParseArrayType();
  end
  else if Check(tkSet) then
  begin
    Result := ParseSetType();
  end
  else if IsBuiltInType() then
  begin
    // Built-in type
    LToken := FCurrentToken;
    Advance();
    Result := CreateASTNodeWithToken(nkTypeRef, LToken);
    Result^.NodeName := LToken.Lexeme;
  end
  else if Check(tkIdentifier) then
  begin
    // Qualified identifier
    LToken := FCurrentToken;
    Advance();
    Result := CreateASTNodeWithToken(nkTypeRef, LToken);
    Result^.NodeName := LToken.Lexeme;

    while Match(tkDot) do
    begin
      if Check(tkIdentifier) then
      begin
        Result^.NodeName := Result^.NodeName + '.' + FCurrentToken.Lexeme;
        Advance();
      end
      else
      begin
        Error(RSParserExpectedIdentifierAfterDot);
        Exit;
      end;
    end;
  end
  else
  begin
    Error(RSParserExpectedTypeRef);
    Result := nil;
    Exit;
  end;
end;

function TPaxParser.ParseBlock(): PASTNode;
var
  LToken: TToken;
  LStmt: PASTNode;
begin
  // {Statement}
  LToken := Previous();
  Result := CreateASTNodeWithToken(nkBlock, LToken);
  Result^.NodeName := 'block';

  while not IsAtEnd() and not Check(tkEnd) and not Check(tkElse) and not Check(tkUntil) and
        not Check(tkPublic) and not Check(tkConst) and not Check(tkType) and
        not Check(tkVar) and not Check(tkRoutine) and not Check(tkTest) do
  begin
    // Bail out if too many errors
    if (FErrors <> nil) and FErrors.ReachedMaxErrors() then
      Break;

    LStmt := ParseStatement();
    if LStmt <> nil then
      AddASTChild(Result, LStmt);
  end;
end;

function TPaxParser.ParseStatement(): PASTNode;
begin
  // Empty statement
  if Match(tkSemicolon) then
  begin
    Result := CreateASTNodeWithToken(nkEmptyStmt, Previous());
    Exit;
  end;

  if Check(tkIf) then
    Result := ParseIfStmt()
  else if Check(tkWhile) then
    Result := ParseWhileStmt()
  else if Check(tkFor) then
    Result := ParseForStmt()
  else if Check(tkRepeat) then
    Result := ParseRepeatStmt()
  else if Check(tkCase) then
    Result := ParseCaseStmt()
  else if Check(tkReturn) then
    Result := ParseReturnStmt()
  else if Check(tkNew) then
    Result := ParseNewStmt()
  else if Check(tkDispose) then
    Result := ParseDisposeStmt()
  else if Check(tkSetLength) then
    Result := ParseSetLengthStmt()
  else if Check(tkGcCollect) then
    Result := ParseGcCollectStmt()
  else if Check(tkGcDump) then
    Result := ParseGcDumpStmt()
  else
    Result := ParseAssignmentOrCall();
end;

function TPaxParser.ParseIfStmt(): PASTNode;
var
  LToken: TToken;
  LCondition: PASTNode;
  LThenBlock: PASTNode;
  LElseBlock: PASTNode;
begin
  // "if" Expression "then" Block ["else" Block] "end" [";"]
  LToken := Expect(tkIf);
  Result := CreateASTNodeWithToken(nkIfStmt, LToken);

  LCondition := ParseExpression();
  if LCondition <> nil then
    AddASTChild(Result, LCondition);

  Expect(tkThen);

  LThenBlock := ParseBlock();
  if LThenBlock <> nil then
    AddASTChild(Result, LThenBlock);

  if Match(tkElse) then
  begin
    LElseBlock := ParseBlock();
    if LElseBlock <> nil then
      AddASTChild(Result, LElseBlock);
  end;

  Expect(tkEnd);
  Match(tkSemicolon);

  // Check if we likely consumed the wrong 'end' (belonged to outer scope)
  if Check(tkPublic) or Check(tkConst) or Check(tkType) or
     Check(tkVar) or Check(tkRoutine) or Check(tkTest) or Check(tkDot) then
  begin
    ErrorAt(LToken, 'This ''if'' statement is missing ''end'' or ''else''');
    // Don't synchronize - let the outer parser handle recovery
  end;
end;

function TPaxParser.ParseWhileStmt(): PASTNode;
var
  LToken: TToken;
  LCondition: PASTNode;
  LBody: PASTNode;
begin
  // "while" Expression "do" Block "end" [";"]
  LToken := Expect(tkWhile);
  Result := CreateASTNodeWithToken(nkWhileStmt, LToken);

  LCondition := ParseExpression();
  if LCondition <> nil then
    AddASTChild(Result, LCondition);

  Expect(tkDo);

  LBody := ParseBlock();
  if LBody <> nil then
    AddASTChild(Result, LBody);

  Expect(tkEnd);
  Match(tkSemicolon);

  // Check if we likely consumed the wrong 'end' (belonged to outer scope)
  if Check(tkPublic) or Check(tkConst) or Check(tkType) or
     Check(tkVar) or Check(tkRoutine) or Check(tkTest) then
  begin
    ErrorAt(LToken, 'This ''while'' statement is missing its ''end'' keyword');
  end;
end;

function TPaxParser.ParseForStmt(): PASTNode;
var
  LToken: TToken;
  LVarToken: TToken;
  LVarNode: PASTNode;
  LFromExpr: PASTNode;
  LToExpr: PASTNode;
  LBody: PASTNode;
begin
  // "for" Identifier ":=" Expression ("to" | "downto") Expression "do" Block "end" [";"]
  LToken := Expect(tkFor);
  Result := CreateASTNodeWithToken(nkForStmt, LToken);

  LVarToken := ExpectIdentifier();
  LVarNode := CreateASTNodeWithToken(nkIdentifier, LVarToken);
  LVarNode^.NodeName := LVarToken.Lexeme;
  AddASTChild(Result, LVarNode);

  Expect(tkAssign);

  LFromExpr := ParseExpression();
  if LFromExpr <> nil then
    AddASTChild(Result, LFromExpr);

  if Match(tkTo) then
    Result^.BoolVal := True  // to = ascending
  else if Match(tkDownto) then
    Result^.BoolVal := False // downto = descending
  else
  begin
    Error(RSParserExpectedToOrDownto);
    Exit;
  end;

  LToExpr := ParseExpression();
  if LToExpr <> nil then
    AddASTChild(Result, LToExpr);

  Expect(tkDo);

  LBody := ParseBlock();
  if LBody <> nil then
    AddASTChild(Result, LBody);

  Expect(tkEnd);
  Match(tkSemicolon);

  // Check if we likely consumed the wrong 'end' (belonged to outer scope)
  if Check(tkPublic) or Check(tkConst) or Check(tkType) or
     Check(tkVar) or Check(tkRoutine) or Check(tkTest) then
  begin
    ErrorAt(LToken, 'This ''for'' statement is missing its ''end'' keyword');
  end;
end;

function TPaxParser.ParseRepeatStmt(): PASTNode;
var
  LToken: TToken;
  LBody: PASTNode;
  LCondition: PASTNode;
begin
  // "repeat" Block "until" Expression [";"]
  LToken := Expect(tkRepeat);
  Result := CreateASTNodeWithToken(nkRepeatStmt, LToken);

  LBody := ParseBlock();
  if LBody <> nil then
    AddASTChild(Result, LBody);

  Expect(tkUntil);

  LCondition := ParseExpression();
  if LCondition <> nil then
    AddASTChild(Result, LCondition);

  Match(tkSemicolon);
end;

function TPaxParser.ParseCaseStmt(): PASTNode;
var
  LToken: TToken;
  LSelector: PASTNode;
  LArm: PASTNode;
  LElseBlock: PASTNode;
begin
  // "case" Expression "of" {CaseArm} ["else" Block] "end" [";"]
  LToken := Expect(tkCase);
  Result := CreateASTNodeWithToken(nkCaseStmt, LToken);

  LSelector := ParseExpression();
  if LSelector <> nil then
    AddASTChild(Result, LSelector);

  Expect(tkOf);

  // Case arms
  while not Check(tkEnd) and not Check(tkElse) and not IsAtEnd() do
  begin
    LArm := ParseCaseArm();
    if LArm <> nil then
      AddASTChild(Result, LArm);
  end;

  if Match(tkElse) then
  begin
    LElseBlock := ParseBlock();
    if LElseBlock <> nil then
      AddASTChild(Result, LElseBlock);
  end;

  Expect(tkEnd);
  Match(tkSemicolon);

  // Check if we likely consumed the wrong 'end' (belonged to outer scope)
  if Check(tkPublic) or Check(tkConst) or Check(tkType) or
     Check(tkVar) or Check(tkRoutine) or Check(tkTest) then
  begin
    ErrorAt(LToken, 'This ''case'' statement is missing its ''end'' keyword');
  end;
end;

function TPaxParser.ParseCaseArm(): PASTNode;
var
  LToken: TToken;
  LLabel: PASTNode;
  LHighLabel: PASTNode;
  LRangeNode: PASTNode;
  LBody: PASTNode;
begin
  // CaseLabel {"," CaseLabel} ":" Block
  LToken := FCurrentToken;
  Result := CreateASTNodeWithToken(nkCaseArm, LToken);

  // Parse labels
  repeat
    LLabel := ParseExpression();

    // Check for range
    if Match(tkDotDot) then
    begin
      LHighLabel := ParseExpression();
      LRangeNode := CreateASTNode(nkSetRange);
      AddASTChild(LRangeNode, LLabel);
      AddASTChild(LRangeNode, LHighLabel);
      AddASTChild(Result, LRangeNode);
    end
    else if LLabel <> nil then
      AddASTChild(Result, LLabel);
  until not Match(tkComma);

  Expect(tkColon);

  LBody := ParseBlock();
  if LBody <> nil then
    AddASTChild(Result, LBody);
end;

function TPaxParser.ParseReturnStmt(): PASTNode;
var
  LToken: TToken;
  LExpr: PASTNode;
begin
  // "return" [Expression] [";"]
  LToken := Expect(tkReturn);
  Result := CreateASTNodeWithToken(nkReturnStmt, LToken);

  // Optional return value
  if not Check(tkSemicolon) and not Check(tkEnd) and not Check(tkElse) then
  begin
    LExpr := ParseExpression();
    if LExpr <> nil then
      AddASTChild(Result, LExpr);
  end;

  Match(tkSemicolon);
end;

function TPaxParser.ParseNewStmt(): PASTNode;
var
  LToken: TToken;
  LExpr: PASTNode;
begin
  // "new" "(" Expression ")" [";"]
  LToken := Expect(tkNew);
  Result := CreateASTNodeWithToken(nkNewStmt, LToken);

  Expect(tkLParen);

  LExpr := ParseExpression();
  if LExpr <> nil then
    AddASTChild(Result, LExpr);

  Expect(tkRParen);
  Match(tkSemicolon);
end;

function TPaxParser.ParseDisposeStmt(): PASTNode;
var
  LToken: TToken;
  LExpr: PASTNode;
begin
  // "dispose" "(" Expression ")" [";"]
  LToken := Expect(tkDispose);
  Result := CreateASTNodeWithToken(nkDisposeStmt, LToken);

  Expect(tkLParen);

  LExpr := ParseExpression();
  if LExpr <> nil then
    AddASTChild(Result, LExpr);

  Expect(tkRParen);
  Match(tkSemicolon);
end;

function TPaxParser.ParseSetLengthStmt(): PASTNode;
var
  LToken: TToken;
  LArrayExpr: PASTNode;
  LLengthExpr: PASTNode;
begin
  // "setlength" "(" Expression "," Expression ")" [";"]
  LToken := Expect(tkSetLength);
  Result := CreateASTNodeWithToken(nkSetLengthStmt, LToken);

  Expect(tkLParen);

  LArrayExpr := ParseExpression();
  if LArrayExpr <> nil then
    AddASTChild(Result, LArrayExpr);

  Expect(tkComma);

  LLengthExpr := ParseExpression();
  if LLengthExpr <> nil then
    AddASTChild(Result, LLengthExpr);

  Expect(tkRParen);
  Match(tkSemicolon);
end;

function TPaxParser.ParseGcCollectStmt(): PASTNode;
var
  LToken: TToken;
begin
  // "gc_collect" "(" ")" [";"]
  LToken := Expect(tkGcCollect);
  Result := CreateASTNodeWithToken(nkGcCollectStmt, LToken);

  Expect(tkLParen);
  Expect(tkRParen);
  Match(tkSemicolon);
end;

function TPaxParser.ParseGcDumpStmt(): PASTNode;
var
  LToken: TToken;
begin
  // "gc_dump" "(" ")" [";"]
  LToken := Expect(tkGcDump);
  Result := CreateASTNodeWithToken(nkGcDumpStmt, LToken);

  Expect(tkLParen);
  Expect(tkRParen);
  Match(tkSemicolon);
end;

function TPaxParser.ParseAssignmentOrCall(): PASTNode;
var
  LDesignator: PASTNode;
  LExpr: PASTNode;
  LToken: TToken;
begin
  // Assignment = Designator ":=" Expression [";"]
  // CallStmt = Designator [";"]
  LDesignator := ParseDesignator();

  if Match(tkAssign) then
  begin
    LToken := Previous();
    Result := CreateASTNodeWithToken(nkAssignment, LToken);
    AddASTChild(Result, LDesignator);

    LExpr := ParseExpression();
    if LExpr <> nil then
      AddASTChild(Result, LExpr);

    Match(tkSemicolon);
  end
  else
  begin
    // Just a call statement
    Result := CreateASTNodeWithToken(nkCallStmt, LDesignator^.Token);
    AddASTChild(Result, LDesignator);
    Match(tkSemicolon);
  end;
end;

function TPaxParser.ParseExpression(): PASTNode;
var
  LLeft: PASTNode;
  LRight: PASTNode;
  LOpToken: TToken;
  LOp: TOperator;
  LBinaryNode: PASTNode;
begin
  // Expression = SimpleExpr [RelOp SimpleExpr]
  LLeft := ParseSimpleExpr();

  // RelOp = "=" | "<>" | "<" | ">" | "<=" | ">=" | "in"
  if Check(tkEq) or Check(tkNe) or Check(tkLt) or Check(tkGt) or
     Check(tkLe) or Check(tkGe) or Check(tkIn) then
  begin
    LOpToken := FCurrentToken;

    case FCurrentToken.Kind of
      tkEq: LOp := opEq;
      tkNe: LOp := opNe;
      tkLt: LOp := opLt;
      tkGt: LOp := opGt;
      tkLe: LOp := opLe;
      tkGe: LOp := opGe;
      tkIn: LOp := opIn;
    else
      LOp := opNone;
    end;

    Advance();

    LRight := ParseSimpleExpr();

    LBinaryNode := CreateASTNodeWithToken(nkBinaryOp, LOpToken);
    LBinaryNode^.Op := LOp;
    AddASTChild(LBinaryNode, LLeft);
    AddASTChild(LBinaryNode, LRight);

    Result := LBinaryNode;
  end
  else
    Result := LLeft;
end;

function TPaxParser.ParseSimpleExpr(): PASTNode;
var
  LLeft: PASTNode;
  LRight: PASTNode;
  LOpToken: TToken;
  LOp: TOperator;
  LBinaryNode: PASTNode;
begin
  // SimpleExpr = Term {AddOp Term}
  LLeft := ParseTerm();

  // AddOp = "+" | "-" | "or"
  while Check(tkPlus) or Check(tkMinus) or Check(tkOr) do
  begin
    LOpToken := FCurrentToken;

    case FCurrentToken.Kind of
      tkPlus:  LOp := opAdd;
      tkMinus: LOp := opSub;
      tkOr:    LOp := opOr;
    else
      LOp := opNone;
    end;

    Advance();

    LRight := ParseTerm();

    LBinaryNode := CreateASTNodeWithToken(nkBinaryOp, LOpToken);
    LBinaryNode^.Op := LOp;
    AddASTChild(LBinaryNode, LLeft);
    AddASTChild(LBinaryNode, LRight);

    LLeft := LBinaryNode;
  end;

  Result := LLeft;
end;

function TPaxParser.ParseTerm(): PASTNode;
var
  LLeft: PASTNode;
  LRight: PASTNode;
  LOpToken: TToken;
  LOp: TOperator;
  LBinaryNode: PASTNode;
begin
  // Term = Factor {MulOp Factor}
  LLeft := ParseFactor();

  // MulOp = "*" | "/" | "div" | "mod" | "and"
  while Check(tkStar) or Check(tkSlash) or Check(tkDiv) or Check(tkMod) or Check(tkAnd) do
  begin
    LOpToken := FCurrentToken;

    case FCurrentToken.Kind of
      tkStar:  LOp := opMul;
      tkSlash: LOp := opDiv;
      tkDiv:   LOp := opDivInt;
      tkMod:   LOp := opMod;
      tkAnd:   LOp := opAnd;
    else
      LOp := opNone;
    end;

    Advance();

    LRight := ParseFactor();

    LBinaryNode := CreateASTNodeWithToken(nkBinaryOp, LOpToken);
    LBinaryNode^.Op := LOp;
    AddASTChild(LBinaryNode, LLeft);
    AddASTChild(LBinaryNode, LRight);

    LLeft := LBinaryNode;
  end;

  Result := LLeft;
end;

function TPaxParser.ParseFactor(): PASTNode;
var
  LOpToken: TToken;
  LOperand: PASTNode;
begin
  // Factor = "not" Factor | "-" Factor | "+" Factor | Primary

  if Check(tkNot) then
  begin
    LOpToken := FCurrentToken;
    Advance();
    LOperand := ParseFactor();

    Result := CreateASTNodeWithToken(nkUnaryOp, LOpToken);
    Result^.Op := opNot;
    AddASTChild(Result, LOperand);
  end
  else if Check(tkMinus) then
  begin
    LOpToken := FCurrentToken;
    Advance();
    LOperand := ParseFactor();

    Result := CreateASTNodeWithToken(nkUnaryOp, LOpToken);
    Result^.Op := opNeg;
    AddASTChild(Result, LOperand);
  end
  else if Check(tkPlus) then
  begin
    LOpToken := FCurrentToken;
    Advance();
    LOperand := ParseFactor();

    Result := CreateASTNodeWithToken(nkUnaryOp, LOpToken);
    Result^.Op := opPos;
    AddASTChild(Result, LOperand);
  end
  else
    Result := ParsePrimary();
end;

function TPaxParser.ParsePrimary(): PASTNode;
var
  LToken: TToken;
  LExpr: PASTNode;
begin
  LToken := FCurrentToken;

  // Literals
  if Check(tkInteger) then
  begin
    Advance();
    Result := CreateASTNodeWithToken(nkIntLiteral, LToken);
    Result^.IntVal := LToken.IntValue;
  end
  else if Check(tkFloat) then
  begin
    Advance();
    Result := CreateASTNodeWithToken(nkFloatLiteral, LToken);
    Result^.FloatVal := LToken.FloatValue;
  end
  else if Check(tkString) then
  begin
    Advance();
    Result := CreateASTNodeWithToken(nkStrLiteral, LToken);
    Result^.StrVal := LToken.Lexeme;
  end
  else if Check(tkWideString) then
  begin
    Advance();
    Result := CreateASTNodeWithToken(nkWideStrLiteral, LToken);
    Result^.StrVal := LToken.Lexeme;
  end
  else if Check(tkChar) then
  begin
    Advance();
    Result := CreateASTNodeWithToken(nkCharLiteral, LToken);
    Result^.StrVal := LToken.Lexeme;
  end
  else if Check(tkWideChar) then
  begin
    Advance();
    Result := CreateASTNodeWithToken(nkWideCharLiteral, LToken);
    Result^.StrVal := LToken.Lexeme;
  end
  else if Check(tkTrue) then
  begin
    Advance();
    Result := CreateASTNodeWithToken(nkBoolLiteral, LToken);
    Result^.BoolVal := True;
  end
  else if Check(tkFalse) then
  begin
    Advance();
    Result := CreateASTNodeWithToken(nkBoolLiteral, LToken);
    Result^.BoolVal := False;
  end
  else if Check(tkNil) then
  begin
    Advance();
    Result := CreateASTNodeWithToken(nkNilLiteral, LToken);
  end

  // Set literal
  else if Check(tkLBrace) then
  begin
    Result := ParseSetLiteral();
  end

  // Parenthesized expression
  else if Match(tkLParen) then
  begin
    LExpr := ParseExpression();
    Expect(tkRParen);
    Result := LExpr;
  end

  // len(expr)
  else if Check(tkLen) then
  begin
    Advance();
    Expect(tkLParen);
    LExpr := ParseExpression();
    Expect(tkRParen);

    Result := CreateASTNodeWithToken(nkLen, LToken);
    AddASTChild(Result, LExpr);
  end

  // sizeof(typename)
  else if Check(tkSizeOf) then
  begin
    Advance();
    Expect(tkLParen);
    LExpr := ParseTypeName();
    Expect(tkRParen);

    Result := CreateASTNodeWithToken(nkSizeOf, LToken);
    AddASTChild(Result, LExpr);
  end

  // gc_heapsize()
  else if Check(tkGcHeapSize) then
  begin
    Advance();
    Expect(tkLParen);
    Expect(tkRParen);
    Result := CreateASTNodeWithToken(nkGcHeapSize, LToken);
  end

  // gc_usedsize()
  else if Check(tkGcUsedSize) then
  begin
    Advance();
    Expect(tkLParen);
    Expect(tkRParen);
    Result := CreateASTNodeWithToken(nkGcUsedSize, LToken);
  end

  // gc_collectcount()
  else if Check(tkGcCollectCount) then
  begin
    Advance();
    Expect(tkLParen);
    Expect(tkRParen);
    Result := CreateASTNodeWithToken(nkGcCollectCount, LToken);
  end

  // paramcount()
  else if Check(tkParamCount) then
  begin
    Advance();
    Expect(tkLParen);
    Expect(tkRParen);
    Result := CreateASTNodeWithToken(nkParamCount, LToken);
  end

  // paramstr(index)
  else if Check(tkParamStr) then
  begin
    Advance();
    Expect(tkLParen);
    LExpr := ParseExpression();
    Expect(tkRParen);
    Result := CreateASTNodeWithToken(nkParamStr, LToken);
    AddASTChild(Result, LExpr);
  end

  // Type cast: BuiltInType(expr)
  else if IsBuiltInType() then
  begin
    Result := ParseTypeCast();
  end

  // Designator (identifier with optional suffixes)
  else if Check(tkIdentifier) then
  begin
    Result := ParseDesignator();
  end

  else
  begin
    Error(RSParserExpectedExpression);
    Result := nil;
    Exit;
  end;
end;

function TPaxParser.ParseDesignator(): PASTNode;
var
  LToken: TToken;
begin
  // Designator = Identifier {Suffix}
  LToken := ExpectIdentifier();

  Result := CreateASTNodeWithToken(nkIdentifier, LToken);
  Result^.NodeName := LToken.Lexeme;

  // Parse suffixes
  Result := ParseDesignatorSuffix(Result);
end;

function TPaxParser.ParseDesignatorSuffix(const ALeft: PASTNode): PASTNode;
var
  LToken: TToken;
  LFieldNode: PASTNode;
  LIndexExpr: PASTNode;
  LDerefNode: PASTNode;
  LCallNode: PASTNode;
  LArgs: TArray<PASTNode>;
  LI: Integer;
begin
  Result := ALeft;

  while True do
  begin
    // Field access: .identifier
    if Match(tkDot) then
    begin
      LToken := FCurrentToken;

      if Check(tkIdentifier) then
      begin
        LFieldNode := CreateASTNodeWithToken(nkFieldAccess, LToken);
        LFieldNode^.NodeName := FCurrentToken.Lexeme;
        AddASTChild(LFieldNode, Result);
        Advance();
        Result := LFieldNode;
      end
      else
      begin
        Error(RSParserExpectedIdentifierAfterDot);
        Exit;
      end;
    end

    // Array index: [expr]
    else if Match(tkLBracket) then
    begin
      LToken := Previous();
      LIndexExpr := ParseExpression();
      Expect(tkRBracket);

      LFieldNode := CreateASTNodeWithToken(nkArrayAccess, LToken);
      AddASTChild(LFieldNode, Result);
      AddASTChild(LFieldNode, LIndexExpr);
      Result := LFieldNode;
    end

    // Dereference: ^
    else if Match(tkCaret) then
    begin
      LToken := Previous();
      LDerefNode := CreateASTNodeWithToken(nkDeref, LToken);
      AddASTChild(LDerefNode, Result);
      Result := LDerefNode;
    end

    // Call: (args)
    else if Match(tkLParen) then
    begin
      LToken := Previous();

      if Check(tkRParen) then
        SetLength(LArgs, 0)
      else
        LArgs := ParseArgList();

      Expect(tkRParen);

      LCallNode := CreateASTNodeWithToken(nkCall, LToken);
      AddASTChild(LCallNode, Result);

      for LI := 0 to High(LArgs) do
        AddASTChild(LCallNode, LArgs[LI]);

      Result := LCallNode;
    end
    else
      Break;
  end;
end;

function TPaxParser.ParseSetLiteral(): PASTNode;
var
  LToken: TToken;
  LElement: PASTNode;
  LHighElement: PASTNode;
  LRangeNode: PASTNode;
begin
  // SetLiteral = "{" [SetElement {"," SetElement}] "}"
  LToken := Expect(tkLBrace);
  Result := CreateASTNodeWithToken(nkSetLiteral, LToken);

  if not Check(tkRBrace) then
  begin
    repeat
      LElement := ParseExpression();

      // Check for range
      if Match(tkDotDot) then
      begin
        LHighElement := ParseExpression();
        LRangeNode := CreateASTNode(nkSetRange);
        AddASTChild(LRangeNode, LElement);
        AddASTChild(LRangeNode, LHighElement);
        AddASTChild(Result, LRangeNode);
      end
      else if LElement <> nil then
        AddASTChild(Result, LElement);
    until not Match(tkComma);
  end;

  Expect(tkRBrace);
end;

function TPaxParser.ParseArgList(): TArray<PASTNode>;
var
  LExpr: PASTNode;
begin
  // ArgList = Expression {"," Expression}
  SetLength(Result, 0);

  repeat
    LExpr := ParseExpression();
    if LExpr <> nil then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := LExpr;
    end;
  until not Match(tkComma);
end;

function TPaxParser.IsBuiltInType(): Boolean;
begin
  Result := FCurrentToken.Kind in [
    tkInt8, tkInt16, tkInt32, tkInt64,
    tkUInt8, tkUInt16, tkUInt32, tkUInt64,
    tkFloat32, tkFloat64,
    tkBoolean,
    tkCharType, tkUCharType, tkWCharType, tkUWCharType,
    tkStringType, tkWStringType,
    tkPointer
  ];
end;

function TPaxParser.ParseTypeCast(): PASTNode;
var
  LToken: TToken;
  LTypeNode: PASTNode;
  LExpr: PASTNode;
begin
  // TypeCast = TypeName "(" Expression ")"
  LToken := FCurrentToken;

  Result := CreateASTNodeWithToken(nkTypeCast, LToken);

  // Parse the full type (handles compound types like "pointer to char")
  LTypeNode := ParseTypeName();
  if LTypeNode <> nil then
    AddASTChild(Result, LTypeNode);

  Expect(tkLParen);
  LExpr := ParseExpression();
  if LExpr <> nil then
    AddASTChild(Result, LExpr);
  Expect(tkRParen);
end;

end.
