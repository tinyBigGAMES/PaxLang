{===============================================================================
  Pax™ Programming Language.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://paxlang.org

  See LICENSE for license information
===============================================================================}

unit Pax.ModuleLoader;

{$I Pax.Defines.inc}

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  System.Generics.Collections,
  Pax.Utils,
  Pax.Errors,
  Pax.AST,
  Pax.Types,
  Pax.Symbols,
  Pax.Checker;

type

  { TModuleState }
  TModuleState = (
    msNotLoaded,
    msLoading,      // Currently being compiled (for circular dependency detection)
    msLoaded        // Successfully compiled and cached
  );

  { TModuleInfo }
  TModuleInfo = class
  private
    FChecker: TPaxChecker;
    function GetTypes(): TPaxTypeRegistry;
    function GetSymbols(): TSymbolTable;
  public
    ModuleName: string;
    FilePath: string;
    SourcePath: string;
    State: TModuleState;
    ASTRoot: PASTNode;
    CHeader: string;
    CSource: string;
    IsLibrary: Boolean;
    PublicSymbolCount: Integer;

    constructor Create();
    destructor Destroy(); override;

    property Checker: TPaxChecker read FChecker write FChecker;
    property Types: TPaxTypeRegistry read GetTypes;
    property Symbols: TSymbolTable read GetSymbols;
  end;

  { TPaxModuleLoader }
  TPaxModuleLoader = class(TBaseObject)
  private
    FErrors: TErrors;
    FSearchPaths: TList<string>;
    FModules: TObjectDictionary<string, TModuleInfo>;
    FLoadStack: TList<string>;  // For circular dependency detection
    FVerbose: Boolean;
    FOutputCallback: TOutputCallback;

    function FindModuleFile(const AModuleName: string): string;
    function DoCompileModule(const AFilePath: string): TModuleInfo;
    {$HINTS OFF}
    procedure Output(const AMsg: string); overload;
    {$HINTS ON}
    procedure Output(const AMsg: string; const AArgs: array of const); overload;

  public
    constructor Create(); override;
    destructor Destroy(); override;

    procedure SetErrors(const AErrors: TErrors);
    procedure AddSearchPath(const APath: string);
    procedure ClearSearchPaths();

    function LoadModule(const AModuleName: string): TModuleInfo;
    function GetModule(const AModuleName: string): TModuleInfo;
    function IsModuleLoaded(const AModuleName: string): Boolean;
    function IsModuleLoading(const AModuleName: string): Boolean;

    procedure RegisterPublicSymbols(const AModule: TModuleInfo; const ATargetSymbols: TSymbolTable);
    
    procedure Clear();

    property SearchPaths: TList<string> read FSearchPaths;
    property Modules: TObjectDictionary<string, TModuleInfo> read FModules;
    property Verbose: Boolean read FVerbose write FVerbose;
    property OutputCallback: TOutputCallback read FOutputCallback write FOutputCallback;
  end;

implementation

uses
  Pax.Lexer,
  Pax.Parser,
  Pax.CodeGen;

{ TModuleInfo }

constructor TModuleInfo.Create();
begin
  inherited Create();
  
  ModuleName := '';
  FilePath := '';
  State := msNotLoaded;
  ASTRoot := nil;
  FChecker := nil;
  CHeader := '';
  CSource := '';
  IsLibrary := False;
end;

destructor TModuleInfo.Destroy();
begin
  if ASTRoot <> nil then
    FreeASTNode(ASTRoot);
    
  FChecker.Free();
  
  inherited;
end;

function TModuleInfo.GetTypes(): TPaxTypeRegistry;
begin
  if FChecker <> nil then
    Result := FChecker.Types
  else
    Result := nil;
end;

function TModuleInfo.GetSymbols(): TSymbolTable;
begin
  if FChecker <> nil then
    Result := FChecker.Symbols
  else
    Result := nil;
end;

{ TPaxModuleLoader }

constructor TPaxModuleLoader.Create();
begin
  inherited;
  
  FErrors := nil;
  FSearchPaths := TList<string>.Create();
  FModules := TObjectDictionary<string, TModuleInfo>.Create([doOwnsValues]);
  FLoadStack := TList<string>.Create();
  FVerbose := False;
  FOutputCallback := nil;
end;

destructor TPaxModuleLoader.Destroy();
begin
  FLoadStack.Free();
  FModules.Free();
  FSearchPaths.Free();
  
  inherited;
end;

procedure TPaxModuleLoader.SetErrors(const AErrors: TErrors);
begin
  FErrors := AErrors;
end;

procedure TPaxModuleLoader.Output(const AMsg: string);
begin
  if not FVerbose then
    Exit;

  if Assigned(FOutputCallback) then
    FOutputCallback(AMsg)
  else
    TUtils.PrintLn(AMsg);
end;

procedure TPaxModuleLoader.Output(const AMsg: string; const AArgs: array of const);
begin
  if not FVerbose then
    Exit;

  if Assigned(FOutputCallback) then
    FOutputCallback(Format(AMsg, AArgs))
  else
    TUtils.PrintLn(AMsg, AArgs);
end;

procedure TPaxModuleLoader.AddSearchPath(const APath: string);
var
  LPath: string;
begin
  LPath := TPath.GetFullPath(APath);
  
  if not FSearchPaths.Contains(LPath) then
    FSearchPaths.Add(LPath);
end;

procedure TPaxModuleLoader.ClearSearchPaths();
begin
  FSearchPaths.Clear();
end;

function TPaxModuleLoader.FindModuleFile(const AModuleName: string): string;
var
  LPath: string;
  LFilePath: string;
begin
  Result := '';
  
  // Try each search path
  for LPath in FSearchPaths do
  begin
    LFilePath := TPath.Combine(LPath, AModuleName + '.pax');
    if TFile.Exists(LFilePath) then
      Exit(LFilePath);
  end;
  
  // Try current directory as fallback
  LFilePath := AModuleName + '.pax';
  if TFile.Exists(LFilePath) then
    Exit(TPath.GetFullPath(LFilePath));
end;

function TPaxModuleLoader.DoCompileModule(const AFilePath: string): TModuleInfo;
var
  LLexer: TPaxLexer;
  LParser: TPaxParser;
  LChecker: TPaxChecker;
  LCodeGen: TPaxCodeGen;
  LSource: string;
  LModuleKind: string;
  LSym: TSymbol;
begin
  Result := TModuleInfo.Create();
  Result.FilePath := AFilePath;
  Result.SourcePath := AFilePath;
  Result.State := msLoading;
  Result.PublicSymbolCount := 0;

  Output('  Compiling ''%s''...', [TPath.GetFileNameWithoutExtension(AFilePath)]);
  
  LLexer := TPaxLexer.Create();
  LParser := TPaxParser.Create();
  LChecker := TPaxChecker.Create();
  LCodeGen := TPaxCodeGen.Create();
  try
    // Wire up errors
    LLexer.SetErrors(FErrors);
    LParser.SetErrors(FErrors);
    LChecker.SetErrors(FErrors);
    LCodeGen.SetErrors(FErrors);
    
    // Load source
    try
      LSource := TFile.ReadAllText(AFilePath, TEncoding.UTF8);
    except
      on E: Exception do
      begin
        if FErrors <> nil then
          FErrors.Add(esFatal, 'E950', Format('Cannot read module file: %s', [AFilePath]));
        Result.State := msNotLoaded;
        LChecker.Free();
        LChecker := nil;
        Exit;
      end;
    end;
    
    // Lex
    LLexer.Tokenize(LSource, AFilePath);
    if (FErrors <> nil) and FErrors.HasErrors() then
    begin
      Result.State := msNotLoaded;
      LChecker.Free();
      LChecker := nil;
      Exit;
    end;
    
    // Parse
    LParser.SetLexer(LLexer);
    Result.ASTRoot := LParser.Parse();
    if (Result.ASTRoot = nil) or ((FErrors <> nil) and FErrors.HasErrors()) then
    begin
      Result.State := msNotLoaded;
      LChecker.Free();
      LChecker := nil;
      Exit;
    end;
    
    Result.ModuleName := Result.ASTRoot^.NodeName;
    
    // Check if it's a library module
    LModuleKind := Result.ASTRoot^.StrVal;
    Result.IsLibrary := (LModuleKind = '''lib''') or (LModuleKind = 'lib');
    
    // Check
    LChecker.Check(Result.ASTRoot);
    if (FErrors <> nil) and FErrors.HasErrors() then
    begin
      Result.State := msNotLoaded;
      LChecker.Free();
      LChecker := nil;
      Exit;
    end;
    
    // Transfer ownership of checker to Result
    Result.Checker := LChecker;
    LChecker := nil;  // Prevent finally from freeing it

    // Count public symbols
    for LSym in Result.Symbols.GlobalScope.Symbols do
    begin
      if LSym.IsPublic then
        Inc(Result.PublicSymbolCount);
    end;

    // Generate C code
    LCodeGen.SetTypes(Result.Types);
    LCodeGen.SetSymbols(Result.Symbols);
    LCodeGen.Generate(Result.ASTRoot);
    
    Result.CHeader := LCodeGen.GetHeader();
    Result.CSource := LCodeGen.GetSource();
    
    Result.State := msLoaded;

    // Output condensed stats
    Output('    Tokens: ' + COLOR_BOLD + '%d' + COLOR_RESET + ', Decls: ' + COLOR_BOLD + '%d' + COLOR_RESET + ', Public: ' + COLOR_BOLD + '%d' + COLOR_RESET + ', C: ' + COLOR_BOLD + '%d' + COLOR_RESET + ' lines',
      [LLexer.GetTokenCount(),
       LParser.ConstCount + LParser.TypeCount + LParser.VarCount + LParser.RoutineCount,
       Result.PublicSymbolCount,
       LCodeGen.HeaderLines + LCodeGen.SourceLines]);
  finally
    LCodeGen.Free();
    LChecker.Free();  // Will be nil if ownership was transferred
    LParser.Free();
    LLexer.Free();
  end;
end;

function TPaxModuleLoader.LoadModule(const AModuleName: string): TModuleInfo;
var
  LFilePath: string;
  LLowerName: string;
begin
  LLowerName := LowerCase(AModuleName);
  
  // Check if already loaded
  if FModules.TryGetValue(LLowerName, Result) then
  begin
    if Result.State = msLoading then
    begin
      // Circular dependency detected
      if FErrors <> nil then
        FErrors.Add(esError, 'E951', Format('Circular dependency detected for module: %s', [AModuleName]));
      Exit(nil);
    end;
    Exit;
  end;
  
  // Find the module file
  LFilePath := FindModuleFile(AModuleName);
  if LFilePath = '' then
  begin
    if FErrors <> nil then
      FErrors.Add(esError, 'E952', Format('Module not found: %s', [AModuleName]));
    Exit(nil);
  end;
  
  // Track loading for circular dependency detection
  FLoadStack.Add(LLowerName);
  try
    // Compile the module
    Result := DoCompileModule(LFilePath);
    
    if Result <> nil then
    begin
      Result.ModuleName := AModuleName;
      FModules.Add(LLowerName, Result);
    end;
  finally
    FLoadStack.Remove(LLowerName);
  end;
end;

function TPaxModuleLoader.GetModule(const AModuleName: string): TModuleInfo;
begin
  if not FModules.TryGetValue(LowerCase(AModuleName), Result) then
    Result := nil;
end;

function TPaxModuleLoader.IsModuleLoaded(const AModuleName: string): Boolean;
var
  LModule: TModuleInfo;
begin
  Result := FModules.TryGetValue(LowerCase(AModuleName), LModule) and 
            (LModule.State = msLoaded);
end;

function TPaxModuleLoader.IsModuleLoading(const AModuleName: string): Boolean;
begin
  Result := FLoadStack.Contains(LowerCase(AModuleName));
end;

procedure TPaxModuleLoader.RegisterPublicSymbols(const AModule: TModuleInfo; 
  const ATargetSymbols: TSymbolTable);
var
  LSym: TSymbol;
  LNewSym: TSymbol;
  LQualifiedName: string;
begin
  if (AModule = nil) or (AModule.Symbols = nil) or (ATargetSymbols = nil) then
    Exit;
    
  // Register all public symbols with qualified names (ModuleName.SymbolName)
  for LSym in AModule.Symbols.GlobalScope.Symbols do
  begin
    if LSym.IsPublic then
    begin
      // Create a qualified name entry
      LQualifiedName := AModule.ModuleName + '.' + LSym.SymbolName;
      
      if not ATargetSymbols.ContainsLocal(LQualifiedName) then
      begin
        LNewSym := ATargetSymbols.Define(LQualifiedName, LSym.Kind);
        LNewSym.SymbolType := LSym.SymbolType;
        LNewSym.IsPublic := True;
        LNewSym.IsExternal := True;  // Mark as coming from another module
        LNewSym.DeclNode := LSym.DeclNode;
      end;
    end;
  end;
end;

procedure TPaxModuleLoader.Clear();
begin
  FModules.Clear();
  FLoadStack.Clear();
end;

end.
