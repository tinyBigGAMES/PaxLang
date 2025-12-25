{===============================================================================
  Pax™ Programming Language.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://paxlang.org

  See LICENSE for license information
===============================================================================}

unit Pax.Compiler;

{$I Pax.Defines.inc}

interface

uses
  WinAPI.Windows,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  System.Diagnostics,
  Pax.Utils,
  Pax.Errors,
  Pax.Resources,
  Pax.Lexer,
  Pax.AST,
  Pax.Parser,
  Pax.Types,
  Pax.Symbols,
  Pax.Checker,
  Pax.CodeGen,
  Pax.LibTCC,
  Pax.ModuleLoader,
  Pax.ArArchive,
  Pax.ZipVFS;

type

  { TPaxCompiler }
  TPaxCompiler = class(TBaseObject)
  private
    FErrors: TErrors;
    FLexer: TPaxLexer;
    FParser: TPaxParser;
    FChecker: TPaxChecker;
    FCodeGen: TPaxCodeGen;
    FTCC: TLibTCC;
    FModuleLoader: TPaxModuleLoader;
    FASTRoot: PASTNode;
    FModuleName: string;
    FSourceFilename: string;
    FTCCBasePath: string;
    FGeneratedPath: string;
    FOutputPath: string;
    FMainSourceFile: string;
    FOutputFilename: string;
    FSubsystem: TLibTCCSubsystem;
    FOutputCallback: TOutputCallback;
    FVerbose: Boolean;
    FUnitTestMode: Boolean;

    // Version info and post-build settings
    FAddVersionInfo: Boolean;
    FVIMajor: Word;
    FVIMinor: Word;
    FVIPatch: Word;
    FVIProductName: string;
    FVIDescription: string;
    FVIFilename: string;
    FVICompanyName: string;
    FVICopyright: string;
    FExeIcon: string;

    function SafeLoadFile(const AFilename: string): string;
    function SafeSaveFile(const AFilename: string; const AContent: string): Boolean;
    function GetTypes(): TPaxTypeRegistry;
    function GetSymbols(): TSymbolTable;

    function GetMaxErrors(): Integer;
    procedure SetMaxErrors(const AValue: Integer);

    procedure Reset();
    function RunLexer(const ASource: string; const AFilename: string): Boolean;
    function RunParser(): Boolean;
    function ProcessImports(): Boolean;
    procedure ProcessEarlyDirectives();
    function RunChecker(): Boolean;
    function RunCodeGen(): Boolean;

    function GetTCCBasePath(): string;
    function GetGeneratedPath(): string;
    function GetOutputPath(): string;
    procedure SetOutputPath(const APath: string);
    function GetOutputTypeFromAST(): TOutputType;
    function CopyGCRuntime(const AOutputDir: string): Boolean;
    function SetupTCC(const AOutput: TLibTCCOutput): Boolean;
    function CompileImportedModules(): Boolean;
    function SaveGeneratedFiles(): Boolean;
    procedure ProcessDirectivesPre();
    procedure ProcessDirectivesPost();
    procedure ProcessVersionInfoDirectives();
    procedure ApplyPostBuildResources(const AExePath: string);
    function BuildEXE(const AOutputPath: string): Boolean;
    function BuildDLL(const AOutputPath: string): Boolean;
    function BuildLIB(const AOutputPath: string): Boolean;


    function GetCHeader(): string;
    function GetCSource(): string;
    function SaveCFiles(const AOutputDir: string): Boolean;
    procedure SetTCCBasePath(const APath: string);
    procedure SetGeneratedPath(const APath: string);
    procedure Output(const AMsg: string); overload;
    procedure Output(const AMsg: string; const AArgs: array of const); overload;

  public
    constructor Create(); override;
    destructor Destroy(); override;

    function  GetVersionStr(): string;

    // LSP: Compile/check without building
    function CompileSource(const ASource: string; const AFilename: string): Boolean;
    function CompileFile(const AFilename: string): Boolean;

    // CLI: Build and run
    function Build(const AAutoRun: Boolean=True): Boolean;
    function Run(): Cardinal;

    // Module paths (LSP + CLI)
    procedure AddModuleSearchPath(const APath: string);
    procedure ClearModuleSearchPaths();

    // LSP properties
    property Errors: TErrors read FErrors;
    property ModuleName: string read FModuleName;
    property SourceFilename: string read FSourceFilename;
    property ASTRoot: PASTNode read FASTRoot;
    property Types: TPaxTypeRegistry read GetTypes;
    property Symbols: TSymbolTable read GetSymbols;
    property ModuleLoader: TPaxModuleLoader read FModuleLoader;

    // CLI/Build properties
    property TCCBasePath: string read GetTCCBasePath write SetTCCBasePath;
    property GeneratedPath: string read GetGeneratedPath write SetGeneratedPath;
    property OutputPath: string read GetOutputPath write SetOutputPath;
    property MainSourceFile: string read FMainSourceFile write FMainSourceFile;
    property OutputFilename: string read FOutputFilename;
    property Subsystem: TLibTCCSubsystem read FSubsystem write FSubsystem;
    property OutputCallback: TOutputCallback read FOutputCallback write FOutputCallback;
    property Verbose: Boolean read FVerbose write FVerbose;
    property MaxErrors: Integer read GetMaxErrors write SetMaxErrors;
  end;

implementation

{ TPaxCompiler }

constructor TPaxCompiler.Create();
begin
  inherited;

  FErrors := TErrors.Create();
  FLexer := TPaxLexer.Create();
  FParser := TPaxParser.Create();
  FChecker := TPaxChecker.Create();
  FCodeGen := TPaxCodeGen.Create();
  FTCC := TLibTCC.Create();
  FModuleLoader := TPaxModuleLoader.Create();

  // Wire up shared error collector
  FLexer.SetErrors(FErrors);
  FParser.SetErrors(FErrors);
  FChecker.SetErrors(FErrors);
  FCodeGen.SetErrors(FErrors);
  FTCC.SetErrors(FErrors);
  FModuleLoader.SetErrors(FErrors);

  FASTRoot := nil;
  FModuleName := '';
  FSourceFilename := '';
  FTCCBasePath := '';
  FGeneratedPath := '';
  FOutputPath := '';
  FMainSourceFile := '';
  FOutputFilename := '';
  FSubsystem := ssConsole;
  FOutputCallback := nil;
  FVerbose := False;
  FUnitTestMode := False;
end;

destructor TPaxCompiler.Destroy();
begin
  if FASTRoot <> nil then
    FreeASTNode(FASTRoot);

  FModuleLoader.Free();
  FTCC.Free();
  FCodeGen.Free();
  FChecker.Free();
  FParser.Free();
  FLexer.Free();
  FErrors.Free();

  inherited;
end;

function  TPaxCompiler.GetVersionStr(): string;
begin
  Result := Format('%d.%d.%d', [PAX_MAJOR_VERSION, PAX_MINOR_VERSION, PAX_PATCH_VERSION]);
end;

function TPaxCompiler.SafeLoadFile(const AFilename: string): string;
begin
  Result := '';

  try
    if not TFile.Exists(AFilename) then
    begin
      FErrors.Add(esFatal, 'E900', Format(RSFatalFileNotFound, [AFilename]));
      Exit;
    end;

    Result := TFile.ReadAllText(AFilename, TEncoding.UTF8);
  except
    on E: Exception do
      FErrors.Add(esFatal, 'E901', Format(RSFatalFileReadError, [AFilename, E.Message]));
  end;
end;

function TPaxCompiler.SafeSaveFile(const AFilename: string; const AContent: string): Boolean;
begin
  Result := False;

  try
    TUtils.CreateDirInPath(AFilename);
    TFile.WriteAllText(AFilename, AContent, TEncoding.UTF8);
    Result := True;
  except
    on E: Exception do
      FErrors.Add(esFatal, 'E902', Format(RSFatalFileWriteError, [AFilename, E.Message]));
  end;
end;

function TPaxCompiler.GetTypes(): TPaxTypeRegistry;
begin
  Result := FChecker.Types;
end;

function TPaxCompiler.GetSymbols(): TSymbolTable;
begin
  Result := FChecker.Symbols;
end;

function TPaxCompiler.GetMaxErrors(): Integer;
begin
  Result := FErrors.MaxErrors;
end;

procedure TPaxCompiler.SetMaxErrors(const AValue: Integer);
begin
  FErrors.MaxErrors := AValue;
end;

procedure TPaxCompiler.Reset();
begin
  FErrors.Clear();
  FLexer.Clear();

  if FASTRoot <> nil then
  begin
    FreeASTNode(FASTRoot);
    FASTRoot := nil;
  end;

  FCodeGen.Clear();
  FModuleName := '';
  FUnitTestMode := False;
end;

function TPaxCompiler.RunLexer(const ASource: string; const AFilename: string): Boolean;
begin
  FLexer.Tokenize(ASource, AFilename);
  Result := not FErrors.HasErrors();
end;

function TPaxCompiler.RunParser(): Boolean;
begin
  FParser.SetLexer(FLexer);
  FASTRoot := FParser.Parse();

  Result := (FASTRoot <> nil) and (not FErrors.HasErrors());

  if FASTRoot <> nil then
    FModuleName := FASTRoot^.NodeName;
end;

procedure TPaxCompiler.ProcessEarlyDirectives();
var
  LI: Integer;
  LChild: PASTNode;
  LName: string;
  LValue: string;
  LIntValue: Integer;
begin
  if FASTRoot = nil then
    Exit;

  for LI := 0 to GetASTChildCount(FASTRoot) - 1 do
  begin
    LChild := GetASTChild(FASTRoot, LI);
    if LChild^.Kind = nkDirective then
    begin
      LName := LowerCase(LChild^.NodeName);
      LValue := LChild^.StrVal;

      if LName = '#modulepath' then
        FModuleLoader.AddSearchPath(LValue)
      else if LName = '#maxerrors' then
      begin
        if TryStrToInt(LValue, LIntValue) and (LIntValue > 0) then
          FErrors.MaxErrors := LIntValue;
      end
      else if LName = '#outputpath' then
        FOutputPath := LValue
      else if LName = '#generatedpath' then
        FGeneratedPath := LValue
      else if (LName = '#subsystem') or (LName = '#apptype') then
      begin
        // Validate: #subsystem only valid for EXE modules
        if (FASTRoot^.StrVal <> '') and (LowerCase(FASTRoot^.StrVal) <> 'exe') then
          FErrors.Add(LChild^.Token.Range, esError, 'E080', '#subsystem directive is only valid for EXE modules')
        else if LowerCase(LValue) = 'gui' then
          FSubsystem := ssGUI
        else
          FSubsystem := ssConsole;
      end
      else if LName = '#unittestmode' then
      begin
        if LowerCase(LValue) = 'on' then
          FUnitTestMode := True
        else
          FUnitTestMode := False;
      end;
    end;
  end;
end;

function TPaxCompiler.ProcessImports(): Boolean;
var
  LI: Integer;
  LJ: Integer;
  LChild: PASTNode;
  LImportNode: PASTNode;
  LModuleName: string;
  LModule: TModuleInfo;
begin
  Result := True;

  if FASTRoot = nil then
    Exit;

  // Find and process import nodes
  for LI := 0 to GetASTChildCount(FASTRoot) - 1 do
  begin
    LChild := GetASTChild(FASTRoot, LI);

    // Check for imports block
    if (LChild^.Kind = nkBlock) and (LChild^.NodeName = 'imports') then
    begin
      for LJ := 0 to GetASTChildCount(LChild) - 1 do
      begin
        LImportNode := GetASTChild(LChild, LJ);
        if LImportNode^.Kind = nkImport then
        begin
          LModuleName := LImportNode^.NodeName;

          // Load the imported module
          LModule := FModuleLoader.LoadModule(LModuleName);
          if LModule = nil then
          begin
            Result := False;
            Continue;
          end;

          if LModule.State <> msLoaded then
          begin
            FErrors.Add(LImportNode^.Token.Range, esError, 'E960',
              Format('Failed to load module: %s', [LModuleName]));
            Result := False;
            Continue;
          end;

          // Register public symbols from the imported module
          FModuleLoader.RegisterPublicSymbols(LModule, FChecker.Symbols);
        end;
      end;
    end
    // Single import node (not in block)
    else if LChild^.Kind = nkImport then
    begin
      LModuleName := LChild^.NodeName;

      LModule := FModuleLoader.LoadModule(LModuleName);
      if LModule = nil then
      begin
        Result := False;
        Continue;
      end;

      if LModule.State <> msLoaded then
      begin
        FErrors.Add(LChild^.Token.Range, esError, 'E960',
          Format('Failed to load module: %s', [LModuleName]));
        Result := False;
        Continue;
      end;

      FModuleLoader.RegisterPublicSymbols(LModule, FChecker.Symbols);
    end;
  end;
end;

function TPaxCompiler.RunChecker(): Boolean;
begin
  FChecker.Check(FASTRoot);
  Result := not FErrors.HasErrors();
end;

function TPaxCompiler.RunCodeGen(): Boolean;
begin
  FCodeGen.SetTypes(FChecker.Types);
  FCodeGen.SetSymbols(FChecker.Symbols);
  FCodeGen.SetSourceFile(FSourceFilename);
  FCodeGen.Generate(FASTRoot);

  Result := not FErrors.HasErrors();
end;

function TPaxCompiler.CompileSource(const ASource: string; const AFilename: string): Boolean;
var
  LSourceDir: string;
  LStopwatch: TStopwatch;
  LModuleKind: string;
  LDeclCount: Integer;
begin
  Result := False;
  FSourceFilename := AFilename;

  Reset();

  LStopwatch := TStopwatch.StartNew();

  // Add source directory to module search paths
  LSourceDir := TPath.GetDirectoryName(TPath.GetFullPath(AFilename));
  if LSourceDir <> '' then
    FModuleLoader.AddSearchPath(LSourceDir);

  // Wire up verbose settings to module loader
  FModuleLoader.Verbose := FVerbose;
  FModuleLoader.OutputCallback := FOutputCallback;

  // Phase 1: Lexing
  if not RunLexer(ASource, AFilename) then
    Exit;

  // Phase 2: Parsing
  if not RunParser() then
    Exit;

  // Determine module kind
  if FASTRoot <> nil then
  begin
    if FASTRoot^.StrVal <> '' then
      LModuleKind := Copy(FASTRoot^.StrVal, 2, Length(FASTRoot^.StrVal) - 2)
    else
      LModuleKind := 'exe';
  end
  else
    LModuleKind := 'unknown';

  Output(COLOR_GREEN + '=== Pax Compiler v%s ===' + COLOR_RESET, [GetVersionStr()]);
  Output('');

  // Phase 2.5: Process early directives (#modulepath)
  ProcessEarlyDirectives();

  // Phase 3: Process imports (load dependencies)
  Output(COLOR_CYAN + '[Imports]' + COLOR_RESET);
  if not ProcessImports() then
    Exit;
  if FModuleLoader.Modules.Count = 0 then
    Output('  (none)');
  Output('');

  // Phase 4: Semantic analysis
  if not RunChecker() then
    Exit;

  // Phase 5: Code generation
  if not RunCodeGen() then
    Exit;

  // Output condensed stats for main module
  LDeclCount := FParser.ConstCount + FParser.TypeCount + FParser.VarCount + FParser.RoutineCount;
  Output(COLOR_CYAN + '[Main]' + COLOR_RESET + ' %s ''%s''', [LModuleKind, FModuleName]);
  Output('  Tokens: ' + COLOR_BOLD + '%d' + COLOR_RESET + ', Decls: ' + COLOR_BOLD + '%d' + COLOR_RESET + ', Checks: ' + COLOR_BOLD + '%d' + COLOR_RESET + ', C: ' + COLOR_BOLD + '%d' + COLOR_RESET + ' lines',
    [FLexer.GetTokenCount(),
     LDeclCount,
     FChecker.TypeChecks,
     FCodeGen.HeaderLines + FCodeGen.SourceLines]);
  if FChecker.WarningCount > 0 then
    Output('  Warnings: ' + COLOR_YELLOW + '%d' + COLOR_RESET, [FChecker.WarningCount]);
  Output('');

  LStopwatch.Stop();
  Output(COLOR_GREEN + '=== Transpile Successful ===' + COLOR_RESET);
  Output('Total time: ' + COLOR_BOLD + '%.0f' + COLOR_RESET + 'ms', [LStopwatch.Elapsed.TotalMilliseconds]);

  Result := True;
end;

function TPaxCompiler.CompileFile(const AFilename: string): Boolean;
var
  LSource: string;
begin
  Result := False;
  FSourceFilename := AFilename;

  Reset();

  // Load source file
  LSource := SafeLoadFile(AFilename);
  if FErrors.HasErrors() then
    Exit;

  if LSource = '' then
  begin
    FErrors.Add(esFatal, 'E903', Format(RSFatalFileReadError, [AFilename, 'Empty file']));
    Exit;
  end;

  Result := CompileSource(LSource, AFilename);
end;

function TPaxCompiler.GetCHeader(): string;
begin
  Result := FCodeGen.GetHeader();
end;

function TPaxCompiler.GetCSource(): string;
begin
  Result := FCodeGen.GetSource();
end;

function TPaxCompiler.SaveCFiles(const AOutputDir: string): Boolean;
var
  LHeaderPath: string;
  LSourcePath: string;
  LDir: string;
begin
  Result := False;

  if FModuleName = '' then
  begin
    FErrors.Add(esError, 'E904', 'No module compiled');
    Exit;
  end;

  // Determine output directory
  if AOutputDir <> '' then
    LDir := AOutputDir
  else
    LDir := TPath.GetDirectoryName(FSourceFilename);

  if LDir = '' then
    LDir := '.';

  LHeaderPath := TPath.Combine(LDir, FModuleName + '.h');
  LSourcePath := TPath.Combine(LDir, FModuleName + '.c');

  // Save header
  if not SafeSaveFile(LHeaderPath, GetCHeader()) then
    Exit;

  // Save source
  if not SafeSaveFile(LSourcePath, GetCSource()) then
    Exit;

  Result := True;
end;

function TPaxCompiler.GetTCCBasePath(): string;
begin
  if FTCCBasePath <> '' then
    Result := FTCCBasePath
  else
    Result := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'thirdparty\tcc');
end;

procedure TPaxCompiler.SetTCCBasePath(const APath: string);
begin
  FTCCBasePath := APath;
end;

function TPaxCompiler.GetGeneratedPath(): string;
begin
  if FGeneratedPath <> '' then
    Result := FGeneratedPath
  else
    Result := TPath.Combine(GetOutputPath(), 'generated');
end;

procedure TPaxCompiler.SetGeneratedPath(const APath: string);
begin
  FGeneratedPath := APath;
end;

procedure TPaxCompiler.Output(const AMsg: string);
begin
  if not FVerbose then
    Exit;

  if Assigned(FOutputCallback) then
    FOutputCallback(AMsg)
  else
    TUtils.PrintLn(AMsg);
end;

procedure TPaxCompiler.Output(const AMsg: string; const AArgs: array of const);
begin
  if not FVerbose then
    Exit;

  if Assigned(FOutputCallback) then
    FOutputCallback(Format(AMsg, AArgs))
  else
    TUtils.PrintLn(AMsg, AArgs);
end;

function TPaxCompiler.GetOutputPath(): string;
var
  LSourceDir: string;
begin
  if FOutputPath <> '' then
    Result := FOutputPath
  else
  begin
    LSourceDir := TPath.GetDirectoryName(TPath.GetFullPath(FSourceFilename));
    if LSourceDir <> '' then
      Result := LSourceDir
    else
      Result := '.';
  end;
end;

procedure TPaxCompiler.SetOutputPath(const APath: string);
begin
  FOutputPath := APath;
end;

function TPaxCompiler.GetOutputTypeFromAST(): TOutputType;
var
  LKindStr: string;
begin
  Result := otEXE; // Default

  if FASTRoot = nil then
    Exit;

  // Strip quotes from module kind string (e.g., 'dll' -> dll)
  LKindStr := FASTRoot^.StrVal;
  if (Length(LKindStr) >= 2) and (LKindStr[1] = '''') then
    LKindStr := Copy(LKindStr, 2, Length(LKindStr) - 2);
  LKindStr := LowerCase(LKindStr);

  if LKindStr = 'exe' then
    Result := otEXE
  else if LKindStr = 'dll' then
    Result := otDLL
  else if LKindStr = 'lib' then
    Result := otLIB;
end;

function TPaxCompiler.CopyGCRuntime(const AOutputDir: string): Boolean;
var
  LGCDestPath: string;
begin
  Result := True;

  LGCDestPath := TPath.Combine(AOutputDir, 'gc.dll');

  { Extract gc.dll from ZIP to output directory }
  if not TZipVFS.ExtractFile('thirdparty\gc\bin\gc.dll', LGCDestPath, True) then
  begin
    FErrors.Add(esWarning, 'W950', 'gc.dll not found in embedded resources');
    Exit(False);
  end;
end;

function TPaxCompiler.Build(const AAutoRun: Boolean): Boolean;
var
  LOutputType: TOutputType;
  LOutputDir: string;
  LExtension: string;
  LStopwatch: TStopwatch;
  LOutputKind: string;
begin
  Result := False;
  FOutputFilename := '';

  // Check MainSourceFile is set
  if FMainSourceFile = '' then
  begin
    FErrors.Add(esError, 'E979', 'MainSourceFile not set');
    Exit;
  end;

  LStopwatch := TStopwatch.StartNew();

  // Compile the source file
  if not CompileFile(FMainSourceFile) then
    Exit;

  // Process version info directives
  ProcessVersionInfoDirectives();

  LOutputType := GetOutputTypeFromAST();
  LOutputDir := GetOutputPath();

  // Determine extension based on output type
  case LOutputType of
    otEXE:
    begin
      LExtension := '.exe';
      if FSubsystem = ssGUI then
        LOutputKind := 'EXE (gui)'
      else
        LOutputKind := 'EXE (console)';
    end;
    otDLL:
    begin
      LExtension := '.dll';
      LOutputKind := 'DLL';
    end;
    otLIB:
    begin
      LExtension := '.a';
      LOutputKind := 'LIB (static)';
      // LIB files go in outputpath/lib/ subdirectory
      LOutputDir := TPath.Combine(LOutputDir, 'lib');
    end;
  end;

  FOutputFilename := TPath.Combine(LOutputDir, FModuleName + LExtension);

  Output('');
  Output(COLOR_CYAN + '[Build]' + COLOR_RESET);
  Output('  Target: ' + COLOR_BOLD + '%s' + COLOR_RESET, [LOutputKind]);
  Output('  Setting up TCC...');
  Output('    Include: ' + COLOR_BOLD + '%s' + COLOR_RESET, [TPath.Combine(GetTCCBasePath(), 'include')]);
  Output('    Library path: ' + COLOR_BOLD + '%s' + COLOR_RESET, [TPath.Combine(GetTCCBasePath(), 'lib')]);
  Output('    Library: gc');

  // Build based on type
  case LOutputType of
    otEXE:
      begin
        Result := BuildEXE(FOutputFilename);
        if Result then
        begin
          Output('  Linking...');
          CopyGCRuntime(LOutputDir);
          Output('  Output: ' + COLOR_BOLD + '%s' + COLOR_RESET, [TPath.GetFullPath(FOutputFilename)]);
        end;
      end;
    otDLL:
      begin
        Result := BuildDLL(FOutputFilename);
        if Result then
        begin
          Output('  Linking...');
          CopyGCRuntime(LOutputDir);
          Output('  Output: ' + COLOR_BOLD + '%s' + COLOR_RESET, [TPath.GetFullPath(FOutputFilename)]);
        end;
      end;
    otLIB:
      begin
        Result := BuildLIB(FOutputFilename);
        if Result then
        begin
          Output('  Linking...');
          Output('  Output: ' + COLOR_BOLD + '%s' + COLOR_RESET, [TPath.GetFullPath(FOutputFilename)]);
        end;
      end;
  end;

  LStopwatch.Stop();

  FTCC.ShowStats(Round(LStopwatch.Elapsed.TotalMilliseconds), '  ');

  // Clear OutputFilename on failure
  if not Result then
    FOutputFilename := ''
  else
  begin
    Output('');
    Output(COLOR_GREEN + '=== Build Successful ===' + COLOR_RESET);
    Output('Total time: ' + COLOR_BOLD + '%.0f' + COLOR_RESET + 'ms', [LStopwatch.Elapsed.TotalMilliseconds]);
    Output('', []);
    if AAutoRun then
      Run();
  end;
end;

function TPaxCompiler.Run(): Cardinal;
var
  LOutputDir: string;
begin
  Result := Cardinal(-1);

  // Check OutputFilename is set
  if FOutputFilename = '' then
  begin
    FErrors.Add(esError, 'E990', 'No output file - call Build first');
    Exit;
  end;

  // Check file exists
  if not TFile.Exists(FOutputFilename) then
  begin
    FErrors.Add(esError, 'E991', Format('Output file not found: %s', [FOutputFilename]));
    Exit;
  end;

  // Check it's an EXE (not DLL or LIB)
  if not FOutputFilename.EndsWith('.exe', True) then
  begin
    //FErrors.Add(esError, 'E992', 'Cannot run: only executables can be run');
    Exit;
  end;

  LOutputDir := TPath.GetDirectoryName(FOutputFilename);

  Result := TUtils.RunExe(FOutputFilename, '', LOutputDir, True, SW_SHOW);
end;

procedure TPaxCompiler.AddModuleSearchPath(const APath: string);
begin
  FModuleLoader.AddSearchPath(APath);
end;

procedure TPaxCompiler.ClearModuleSearchPaths();
begin
  FModuleLoader.ClearSearchPaths();
end;

function TPaxCompiler.SetupTCC(const AOutput: TLibTCCOutput): Boolean;
var
  LBasePath: string;
  LIncludePath: string;
  LLibPath: string;
  LGCIncludePath: string;
begin
  Result := False;

  FTCC.Reset();

  LBasePath := GetTCCBasePath();
  LIncludePath := TPath.Combine(LBasePath, 'include');
  LLibPath := TPath.Combine(LBasePath, 'lib');

  // Set output type
  if not FTCC.SetOuput(AOutput) then
  begin
    FErrors.Add(esFatal, 'E912', 'Failed to set TCC output type');
    Exit;
  end;

  // Add include paths
  if not FTCC.AddIncludePath(LIncludePath) then
  begin
    FErrors.Add(esFatal, 'E913', 'Failed to add TCC include path');
    Exit;
  end;

  // Add winapi include path for Windows headers
  FTCC.AddIncludePath(TPath.Combine(LIncludePath, 'winapi'));

  // Add gc include path and link to gc.dll
  LGCIncludePath := TPath.Combine(TPath.GetDirectoryName(LBasePath), 'gc\include');
  FTCC.AddIncludePath(LGCIncludePath);

  // Add gc library path (for gc.dll)
  FTCC.AddLibraryPath(TPath.Combine(TPath.GetDirectoryName(LBasePath), 'gc\bin'));

  // Link to gc.dll
  FTCC.AddLibrary('gc');

  // Add library path
  if not FTCC.AddLibraryPath(LLibPath) then
  begin
    FErrors.Add(esFatal, 'E914', 'Failed to add TCC library path');
    Exit;
  end;

  Result := True;
end;

function TPaxCompiler.SaveGeneratedFiles(): Boolean;
var
  LGenPath: string;
  LHeaderPath: string;
  LSourcePath: string;
begin
  Result := False;

  LGenPath := GetGeneratedPath();
  LHeaderPath := TPath.Combine(LGenPath, FModuleName + '.h');
  LSourcePath := TPath.Combine(LGenPath, FModuleName + '.c');

  // Save header
  if not SafeSaveFile(LHeaderPath, GetCHeader()) then
    Exit;

  // Save source
  if not SafeSaveFile(LSourcePath, GetCSource()) then
    Exit;

  Result := True;
end;

procedure TPaxCompiler.ProcessDirectivesPre();
var
  LI: Integer;
  LChild: PASTNode;
  LName: string;
  LValue: string;
begin
  if FASTRoot = nil then
    Exit;

  for LI := 0 to GetASTChildCount(FASTRoot) - 1 do
  begin
    LChild := GetASTChild(FASTRoot, LI);

    // Process directives that must run BEFORE main source is added
    if LChild^.Kind = nkDirective then
    begin
      LName := LowerCase(LChild^.NodeName);
      LValue := LChild^.StrVal;

      if LName = '#library' then
        FTCC.AddLibrary(LValue)
      else if LName = '#librarypath' then
        FTCC.AddLibraryPath(LValue);
    end
    // Process external routines with DLL names
    else if (LChild^.Kind = nkRoutineDecl) and LChild^.IsExternal and (LChild^.ExternalLib <> '') then
    begin
      FTCC.AddLibrary(LChild^.ExternalLib);
    end;
  end;
end;

procedure TPaxCompiler.ProcessDirectivesPost();
var
  LI: Integer;
  LChild: PASTNode;
  LName: string;
  LValue: string;
begin
  if FASTRoot = nil then
    Exit;

  for LI := 0 to GetASTChildCount(FASTRoot) - 1 do
  begin
    LChild := GetASTChild(FASTRoot, LI);

    // Process directives that must run AFTER main source is added
    // (these need undefined symbols to exist for proper linking)
    if LChild^.Kind = nkDirective then
    begin
      LName := LowerCase(LChild^.NodeName);
      LValue := LChild^.StrVal;

      if LName = '#addfile' then
        FTCC.AddFile(LValue);
    end;
  end;
end;

procedure TPaxCompiler.ProcessVersionInfoDirectives();
var
  LI: Integer;
  LChild: PASTNode;
  LName: string;
  LValue: string;
  LIntValue: Integer;
begin
  // Reset to defaults
  FAddVersionInfo := False;
  FVIMajor := 0;
  FVIMinor := 0;
  FVIPatch := 0;
  FVIProductName := '';
  FVIDescription := '';
  FVIFilename := '';
  FVICompanyName := '';
  FVICopyright := '';
  FExeIcon := '';

  if FASTRoot = nil then
    Exit;

  for LI := 0 to GetASTChildCount(FASTRoot) - 1 do
  begin
    LChild := GetASTChild(FASTRoot, LI);
    if LChild^.Kind = nkDirective then
    begin
      LName := LowerCase(LChild^.NodeName);
      LValue := LChild^.StrVal;

      if LName = '#addverinfo' then
        FAddVersionInfo := (LowerCase(LValue) = 'yes')
      else if LName = '#vimajor' then
      begin
        if TryStrToInt(LValue, LIntValue) then
          FVIMajor := Word(LIntValue);
      end
      else if LName = '#viminor' then
      begin
        if TryStrToInt(LValue, LIntValue) then
          FVIMinor := Word(LIntValue);
      end
      else if LName = '#vipatch' then
      begin
        if TryStrToInt(LValue, LIntValue) then
          FVIPatch := Word(LIntValue);
      end
      else if LName = '#viproductname' then
        FVIProductName := LValue
      else if LName = '#videscription' then
        FVIDescription := LValue
      else if LName = '#vifilename' then
        FVIFilename := LValue
      else if LName = '#vicompanyname' then
        FVICompanyName := LValue
      else if LName = '#vicopyright' then
        FVICopyright := LValue
      else if LName = '#exeicon' then
      begin
        FExeIcon := LValue;
        FExeIcon := FExeIcon.Replace('\', '/');
      end;
    end;
  end;
end;

procedure TPaxCompiler.ApplyPostBuildResources(const AExePath: string);
var
  LIconPath: string;
  LIsExe: Boolean;
  LIsDll: Boolean;
begin
  LIsExe := AExePath.EndsWith('.exe', True);
  LIsDll := AExePath.EndsWith('.dll', True);

  // Only applies to EXE and DLL files
  if not LIsExe and not LIsDll then
    Exit;

  // 1. Add manifest (EXE only)
  if LIsExe then
  begin
    if TUtils.ResourceExist('EXE_MANIFEST') then
    begin
      if not TUtils.AddResManifestFromResource('EXE_MANIFEST', AExePath) then
        FErrors.Add(esWarning, 'W980', 'Failed to add manifest to executable');
    end;
  end;

  // 2. Add icon if specified (EXE only)
  if LIsExe and (FExeIcon <> '') then
  begin
    try
      LIconPath := FExeIcon;
      // Resolve relative paths against source file directory
      if not TPath.IsPathRooted(LIconPath) then
        LIconPath := TPath.Combine(TPath.GetDirectoryName(FSourceFilename), LIconPath);

      if TFile.Exists(LIconPath) then
        TUtils.UpdateIconResource(AExePath, LIconPath)
      else
        FErrors.Add(esWarning, 'W982', Format('Icon file not found: %s', [LIconPath]));
    except
      on E: Exception do
        FErrors.Add(esWarning, 'W981', Format('Failed to add icon: %s', [E.Message]));
    end;
  end;

  // 3. Add version info if enabled (EXE and DLL)
  if FAddVersionInfo then
  begin
    try
      TUtils.UpdateVersionInfoResource(
        AExePath,
        FVIMajor,
        FVIMinor,
        FVIPatch,
        FVIProductName,
        FVIDescription,
        FVIFilename,
        FVICompanyName,
        FVICopyright
      );
    except
      on E: Exception do
        FErrors.Add(esWarning, 'W983', Format('Failed to add version info: %s', [E.Message]));
    end;
  end;
end;

function TPaxCompiler.CompileImportedModules(): Boolean;
var
  LPair: TPair<string, TModuleInfo>;
  LModule: TModuleInfo;
  LGenPath: string;
  LHeaderPath: string;
  LSourcePath: string;
begin
  Result := True;
  LGenPath := GetGeneratedPath();

  // Save and compile all imported modules' C code
  for LPair in FModuleLoader.Modules do
  begin
    LModule := LPair.Value;
    if (LModule.State = msLoaded) and (LModule.CSource <> '') then
    begin
      // Save imported module's files to generated path
      LHeaderPath := TPath.Combine(LGenPath, LModule.ModuleName + '.h');
      LSourcePath := TPath.Combine(LGenPath, LModule.ModuleName + '.c');

      if not SafeSaveFile(LHeaderPath, LModule.CHeader) then
      begin
        Result := False;
        Exit;
      end;

      if not SafeSaveFile(LSourcePath, LModule.CSource) then
      begin
        Result := False;
        Exit;
      end;

      // Add the source file to TCC
      if not FTCC.AddFile(LSourcePath) then
      begin
        //FErrors.Add(esFatal, 'E970', Format('Failed to compile imported module: %s', [LModule.ModuleName]));
        Result := False;
        Exit;
      end;
    end;
  end;
end;

function TPaxCompiler.BuildEXE(const AOutputPath: string): Boolean;
var
  LRuntimePath: string;
  LGenPath: string;
  LSourcePath: string;
begin
  Result := False;

  if FModuleName = '' then
  begin
    FErrors.Add(esError, 'E920', 'No module compiled - call CompileSource or CompileFile first');
    Exit;
  end;

  // Save generated files to disk
  if not SaveGeneratedFiles() then
    Exit;

  LGenPath := GetGeneratedPath();
  LSourcePath := TPath.Combine(LGenPath, FModuleName + '.c');

  // Setup TCC for EXE output
  if not SetupTCC(opEXE) then
    Exit;

  // Process pre-directives (#library, #librarypath)
  ProcessDirectivesPre();

  // Add generated path to TCC include path
  if not FTCC.AddIncludePath(LGenPath) then
  begin
    FErrors.Add(esFatal, 'E924', 'Failed to add generated include path');
    Exit;
  end;

  // Set subsystem
  if not FTCC.SetSubsystem(FSubsystem) then
  begin
    FErrors.Add(esWarning, 'W920', 'Failed to set subsystem');
  end;

  // Define PAX_UNITTESTING if unit test mode is enabled
  if FUnitTestMode then
  begin
    if not FTCC.DefineSymbol('PAX_UNITTESTING', '1') then
      FErrors.Add(esWarning, 'W921', 'Failed to define PAX_UNITTESTING symbol');
  end;

  // Compile the runtime library
  LRuntimePath := TPath.Combine(GetTCCBasePath(), 'lib\pax_runtime.c');
  if not FTCC.AddFile(LRuntimePath) then
  begin
    FErrors.Add(esFatal, 'E921', 'Failed to compile Pax runtime library');
    Exit;
  end;

  // Compile the unit test runtime if unit test mode is enabled
  if FUnitTestMode then
  begin
    LRuntimePath := TPath.Combine(GetTCCBasePath(), 'lib\pax_unittest.c');
    if not FTCC.AddFile(LRuntimePath) then
    begin
      FErrors.Add(esFatal, 'E922', 'Failed to compile Pax unit test library');
      Exit;
    end;
  end;

  // Compile imported modules
  if not CompileImportedModules() then
    Exit;

  // Add the main source file
  if not FTCC.AddFile(LSourcePath) then
  begin
    //FErrors.Add(esFatal, 'E925', 'Failed to compile main module');
    Exit;
  end;

  // Process post-directives (#addfile) - must come after main source
  // so that external declarations create undefined symbols for archives to resolve
  ProcessDirectivesPost();

  // Output the executable
  if not FTCC.OutputFile(AOutputPath) then
  begin
    FErrors.Add(esFatal, 'E923', Format('Failed to write executable: %s', [AOutputPath]));
    Exit;
  end;

  // Apply post-build resources (manifest, icon, version info)
  ApplyPostBuildResources(AOutputPath);

  Result := True;
end;

function TPaxCompiler.BuildDLL(const AOutputPath: string): Boolean;
var
  LRuntimePath: string;
  LGenPath: string;
  LSourcePath: string;
begin
  Result := False;

  if FModuleName = '' then
  begin
    FErrors.Add(esError, 'E930', 'No module compiled - call CompileSource or CompileFile first');
    Exit;
  end;

  // Save generated files to disk
  if not SaveGeneratedFiles() then
    Exit;

  LGenPath := GetGeneratedPath();
  LSourcePath := TPath.Combine(LGenPath, FModuleName + '.c');

  // Setup TCC for DLL output
  if not SetupTCC(opDLL) then
    Exit;

  // Process pre-directives (#library, #librarypath)
  ProcessDirectivesPre();

  // Add generated path to TCC include path
  if not FTCC.AddIncludePath(LGenPath) then
  begin
    FErrors.Add(esFatal, 'E934', 'Failed to add generated include path');
    Exit;
  end;

  // Compile the runtime library
  LRuntimePath := TPath.Combine(GetTCCBasePath(), 'lib\pax_runtime.c');
  if not FTCC.AddFile(LRuntimePath) then
  begin
    FErrors.Add(esFatal, 'E931', 'Failed to compile Pax runtime library');
    Exit;
  end;


  // Compile imported modules
  if not CompileImportedModules() then
    Exit;

  // Add the main source file
  if not FTCC.AddFile(LSourcePath) then
  begin
    //FErrors.Add(esFatal, 'E935', 'Failed to compile main module');
    Exit;
  end;

  // Process post-directives (#addfile) - must come after main source
  // so that external declarations create undefined symbols for archives to resolve
  ProcessDirectivesPost();

  // Output the DLL
  if not FTCC.OutputFile(AOutputPath) then
  begin
    FErrors.Add(esFatal, 'E933', Format('Failed to write DLL: %s', [AOutputPath]));
    Exit;
  end;

  // Apply post-build resources (version info only for DLLs)
  ApplyPostBuildResources(AOutputPath);

  Result := True;
end;

function TPaxCompiler.BuildLIB(const AOutputPath: string): Boolean;
var
  LGenPath: string;
  LSourcePath: string;
  LObjPath: string;
  LArchive: TArArchiveWriter;
begin
  Result := False;

  if FModuleName = '' then
  begin
    FErrors.Add(esError, 'E940', 'No module compiled - call CompileSource or CompileFile first');
    Exit;
  end;

  // Save generated files to disk
  if not SaveGeneratedFiles() then
    Exit;

  LGenPath := GetGeneratedPath();
  LSourcePath := TPath.Combine(LGenPath, FModuleName + '.c');
  LObjPath := TPath.Combine(LGenPath, FModuleName + '.o');

  // Setup TCC for OBJ output
  if not SetupTCC(opOBJ) then
    Exit;

  // Process pre-directives (#library, #librarypath)
  ProcessDirectivesPre();

  // Add generated path to TCC include path
  if not FTCC.AddIncludePath(LGenPath) then
  begin
    FErrors.Add(esFatal, 'E944', 'Failed to add generated include path');
    Exit;
  end;

  // NOTE: For static libs, we do NOT compile runtime into the .o
  // Runtime is linked when the final EXE/DLL is built

  // Compile imported modules (their code goes into the .o)
  if not CompileImportedModules() then
    Exit;

  // Add the main source file
  if not FTCC.AddFile(LSourcePath) then
    Exit;

  // Process post-directives (#addfile) - must come after main source
  // so that external declarations create undefined symbols for archives to resolve
  ProcessDirectivesPost();

  // Output the object file (to generated folder as intermediate)
  if not FTCC.OutputFile(LObjPath) then
  begin
    FErrors.Add(esFatal, 'E943', Format('Failed to write object file: %s', [LObjPath]));
    Exit;
  end;

  // Ensure output directory exists (outputpath/lib/)
  TUtils.CreateDirInPath(AOutputPath);

  // Create .a archive using TArArchiveWriter (native Delphi, no shell-out)
  LArchive := TArArchiveWriter.Create();
  try
    try
      LArchive.AddFile(LObjPath);
      LArchive.SaveToFile(AOutputPath);
    except
      on E: Exception do
      begin
        FErrors.Add(esFatal, 'E946', Format('Failed to create archive: %s', [E.Message]));
        Exit;
      end;
    end;
  finally
    LArchive.Free();
  end;

  {
  // [COMMENTED OUT - Old tcc -ar approach]
  // Convert .o to .a archive using tcc -ar
  LTccExe := TPath.Combine(GetTCCBasePath(), 'tcc.exe');
  if not TFile.Exists(LTccExe) then
  begin
    FErrors.Add(esFatal, 'E945', Format('TCC executable not found: %s', [LTccExe]));
    Exit;
  end;

  // tcc -ar rcs output.a input.o (use absolute paths)
  LParams := Format('-ar rcs "%s" "%s"', [TPath.GetFullPath(AOutputPath), TPath.GetFullPath(LObjPath)]);
  
  try
    LExitCode := TUtils.RunExe(LTccExe, LParams, LGenPath, True, SW_HIDE);
    if LExitCode <> 0 then
    begin
      FErrors.Add(esFatal, 'E946', Format('Failed to create archive (exit code %d)', [LExitCode]));
      Exit;
    end;
  except
    on E: Exception do
    begin
      FErrors.Add(esFatal, 'E947', Format('Failed to run tcc -ar: %s', [E.Message]));
      Exit;
    end;
  end;
  }

  // Clean up intermediate .o file (optional - keep for debugging)
  // TFile.Delete(LObjPath);

  Result := True;
end;

end.
