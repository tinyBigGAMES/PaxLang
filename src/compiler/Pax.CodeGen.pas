{===============================================================================
  Pax™ Programming Language.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://paxlang.org

  See LICENSE for license information
===============================================================================}

unit Pax.CodeGen;

{$I Pax.Defines.inc}

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.Generics.Collections,
  Pax.Utils,
  Pax.Errors,
  Pax.Lexer,
  Pax.AST,
  Pax.Types,
  Pax.Symbols;

type

  { TSourceFile }
  TSourceFile = (
    sfHeader,
    sfSource
  );

  { TPaxCodeGen }
  TPaxCodeGen = class(TBaseObject)
  private
    FErrors: TErrors;
    FTypes: TPaxTypeRegistry;
    FSymbols: TSymbolTable;
    FBuilders: array[TSourceFile] of TStringBuilder;
    FIndent: Integer;
    FModuleName: string;
    FSourceFile: string;
    FCurrentReturnType: TPaxType;
    FIsDLL: Boolean;

    // Stats
    FHeaderLines: Integer;
    FSourceLines: Integer;

    // Output helpers
    procedure Emit(const AFile: TSourceFile; const AText: string);
    procedure EmitLn(const AFile: TSourceFile; const AText: string); overload;
    procedure EmitLn(const AFile: TSourceFile); overload;
    procedure EmitFmt(const AFile: TSourceFile; const AFormat: string; const AArgs: array of const);
    procedure EmitLnFmt(const AFile: TSourceFile; const AFormat: string; const AArgs: array of const);
    procedure EmitLineDirective(const ANode: PASTNode);
    procedure IncIndent();
    procedure DecIndent();
    function GetIndentStr(): string;

    // Type conversion
    function TypeToC(const AType: TPaxType): string;
    function TypeToCDecl(const AType: TPaxType; const AName: string): string;

    // Module generation
    procedure GenerateModule(const ANode: PASTNode);
    procedure GenerateHeaderPreamble();
    procedure GenerateSourcePreamble();
    procedure GenerateHeaderPostamble();

    // Declaration generation
    procedure GenerateDirective(const ANode: PASTNode);
    procedure GenerateConstDecl(const AFile: TSourceFile; const ANode: PASTNode);
    procedure GenerateTypeDecl(const AFile: TSourceFile; const ANode: PASTNode);
    procedure GenerateRecordType(const AFile: TSourceFile; const ANode: PASTNode; const AName: string);
    procedure GenerateUnionType(const AFile: TSourceFile; const ANode: PASTNode; const AName: string);
    procedure GenerateAnonymousRecord(const AFile: TSourceFile; const ANode: PASTNode);
    procedure GenerateAnonymousUnion(const AFile: TSourceFile; const ANode: PASTNode);
    procedure GenerateVarDecl(const AFile: TSourceFile; const ANode: PASTNode; const AIsLocal: Boolean);
    procedure GenerateRoutineDecl(const ANode: PASTNode);
    procedure GenerateExternDecl(const AFile: TSourceFile; const ANode: PASTNode);
    procedure GenerateRoutinePrototype(const AFile: TSourceFile; const ANode: PASTNode);
    procedure GenerateParamList(const AFile: TSourceFile; const ANode: PASTNode);
    procedure GenerateTestBlock(const ANode: PASTNode; const AIndex: Integer);

    // Statement generation
    procedure GenerateBlock(const ANode: PASTNode);
    procedure GenerateStatement(const ANode: PASTNode);
    procedure GenerateIfStmt(const ANode: PASTNode);
    procedure GenerateWhileStmt(const ANode: PASTNode);
    procedure GenerateForStmt(const ANode: PASTNode);
    procedure GenerateRepeatStmt(const ANode: PASTNode);
    procedure GenerateCaseStmt(const ANode: PASTNode);
    procedure GenerateReturnStmt(const ANode: PASTNode);
    procedure GenerateAssignment(const ANode: PASTNode);
    procedure GenerateCallStmt(const ANode: PASTNode);
    procedure GenerateNewStmt(const ANode: PASTNode);
    procedure GenerateDisposeStmt(const ANode: PASTNode);
    procedure GenerateSetLengthStmt(const ANode: PASTNode);
    procedure GenerateGcCollectStmt(const ANode: PASTNode);
    procedure GenerateGcDumpStmt(const ANode: PASTNode);

    // Expression generation
    function GenerateExpression(const ANode: PASTNode): string;
    function GenerateBinaryOp(const ANode: PASTNode): string;
    function GenerateUnaryOp(const ANode: PASTNode): string;
    function GenerateIdentifier(const ANode: PASTNode): string;
    function GenerateFieldAccess(const ANode: PASTNode): string;
    function GenerateArrayAccess(const ANode: PASTNode): string;
    function GenerateDeref(const ANode: PASTNode): string;
    function GenerateCall(const ANode: PASTNode): string;
    function GenerateTypeCast(const ANode: PASTNode): string;
    function GenerateSizeOf(const ANode: PASTNode): string;
    function GenerateLen(const ANode: PASTNode): string;
    function GenerateGcHeapSize(const ANode: PASTNode): string;
    function GenerateGcUsedSize(const ANode: PASTNode): string;
    function GenerateGcCollectCount(const ANode: PASTNode): string;
    function GenerateParamCount(const ANode: PASTNode): string;
    function GenerateParamStr(const ANode: PASTNode): string;
    function GenerateSetLiteral(const ANode: PASTNode): string;
    function GenerateLiteral(const ANode: PASTNode): string;

    // Utility
    function EscapeString(const AStr: string): string;
    function OperatorToC(const AOp: TOperator): string;

    // Type inference helpers
    function GetExpressionType(const ANode: PASTNode): TPaxType;
    function ResolveTypeNode(const ANode: PASTNode): TPaxType;
    function TypeNodeToC(const ANode: PASTNode): string;

  public
    constructor Create(); override;
    destructor Destroy(); override;

    procedure SetErrors(const AErrors: TErrors);
    procedure SetTypes(const ATypes: TPaxTypeRegistry);
    procedure SetSymbols(const ASymbols: TSymbolTable);
    procedure SetSourceFile(const ASourceFile: string);

    procedure Generate(const ARoot: PASTNode);
    procedure Clear();

    function GetHeader(): string;
    function GetSource(): string;

    property ModuleName: string read FModuleName;
    property SourceFile: string read FSourceFile;

    // Stats properties
    property HeaderLines: Integer read FHeaderLines;
    property SourceLines: Integer read FSourceLines;
  end;

implementation

{ TPaxCodeGen }

constructor TPaxCodeGen.Create();
var
  LFile: TSourceFile;
begin
  inherited;

  FErrors := nil;
  FTypes := nil;
  FSymbols := nil;

  for LFile := Low(TSourceFile) to High(TSourceFile) do
    FBuilders[LFile] := TStringBuilder.Create();

  FIndent := 0;
  FModuleName := '';
  FSourceFile := '';
  FCurrentReturnType := nil;
  FIsDLL := False;
end;

destructor TPaxCodeGen.Destroy();
var
  LFile: TSourceFile;
begin
  for LFile := Low(TSourceFile) to High(TSourceFile) do
    FBuilders[LFile].Free();

  inherited;
end;

procedure TPaxCodeGen.SetErrors(const AErrors: TErrors);
begin
  FErrors := AErrors;
end;

procedure TPaxCodeGen.SetTypes(const ATypes: TPaxTypeRegistry);
begin
  FTypes := ATypes;
end;

procedure TPaxCodeGen.SetSymbols(const ASymbols: TSymbolTable);
begin
  FSymbols := ASymbols;
end;

procedure TPaxCodeGen.SetSourceFile(const ASourceFile: string);
begin
  if ASourceFile <> '' then
    FSourceFile := TPath.GetFullPath(ASourceFile).Replace('\', '/')
  else
    FSourceFile := '';
end;

procedure TPaxCodeGen.Emit(const AFile: TSourceFile; const AText: string);
begin
  FBuilders[AFile].Append(AText);
end;

procedure TPaxCodeGen.EmitLn(const AFile: TSourceFile; const AText: string);
begin
  FBuilders[AFile].Append(GetIndentStr()).AppendLine(AText);

  // Track line count
  if AFile = sfHeader then
    Inc(FHeaderLines)
  else
    Inc(FSourceLines);
end;

procedure TPaxCodeGen.EmitLn(const AFile: TSourceFile);
begin
  FBuilders[AFile].AppendLine();

  // Track line count
  if AFile = sfHeader then
    Inc(FHeaderLines)
  else
    Inc(FSourceLines);
end;

procedure TPaxCodeGen.EmitFmt(const AFile: TSourceFile; const AFormat: string; const AArgs: array of const);
begin
  FBuilders[AFile].Append(Format(AFormat, AArgs));
end;

procedure TPaxCodeGen.EmitLnFmt(const AFile: TSourceFile; const AFormat: string; const AArgs: array of const);
begin
  FBuilders[AFile].Append(GetIndentStr()).AppendLine(Format(AFormat, AArgs));
end;

procedure TPaxCodeGen.EmitLineDirective(const ANode: PASTNode);
begin
  if (ANode <> nil) and (FSourceFile <> '') and (ANode^.Token.Range.StartLine > 0) then
    EmitLnFmt(sfSource, '#line %d "%s"', [ANode^.Token.Range.StartLine, FSourceFile]);
end;

procedure TPaxCodeGen.IncIndent();
begin
  Inc(FIndent);
end;

procedure TPaxCodeGen.DecIndent();
begin
  if FIndent > 0 then
    Dec(FIndent);
end;

function TPaxCodeGen.GetIndentStr(): string;
begin
  Result := StringOfChar(' ', FIndent * 4);
end;

function TPaxCodeGen.TypeToC(const AType: TPaxType): string;
begin
  if AType = nil then
    Exit('void');

  case AType.Kind of
    tkVoid:     Result := 'void';
    tkInt8:     Result := 'int8_t';
    tkInt16:    Result := 'int16_t';
    tkInt32:    Result := 'int32_t';
    tkInt64:    Result := 'int64_t';
    tkUInt8:    Result := 'uint8_t';
    tkUInt16:   Result := 'uint16_t';
    tkUInt32:   Result := 'uint32_t';
    tkUInt64:   Result := 'uint64_t';
    tkFloat32:  Result := 'float';
    tkFloat64:  Result := 'double';
    tkBoolean:  Result := 'bool';
    tkChar:     Result := 'char';
    tkUChar:    Result := 'unsigned char';
    tkWChar:    Result := 'wchar_t';
    tkUWChar:   Result := 'unsigned short';
    tkString:   Result := 'PaxString*';
    tkWString:  Result := 'PaxWString*';
    tkPointer:
      begin
        if AType.ElementType <> nil then
          Result := TypeToC(AType.ElementType) + '*'
        else
          Result := 'void*';
      end;
    tkArray:
      begin
        if AType.IsDynamic then
          Result := 'PaxArray*'
        else if AType.ElementType <> nil then
          Result := TypeToC(AType.ElementType) + '*'
        else
          Result := 'void*';
      end;
    tkRecord:
      begin
        if AType.TypeName <> '' then
          Result := AType.TypeName
        else
          Result := 'struct';
      end;
    tkUnion:
      begin
        if AType.TypeName <> '' then
          Result := AType.TypeName
        else
          Result := 'union';
      end;
    tkSet:      Result := 'uint64_t';
    tkRoutine:  Result := 'void*'; // Function pointer simplified
  else
    Result := 'void';
  end;
end;

function TPaxCodeGen.TypeToCDecl(const AType: TPaxType; const AName: string): string;
var
  LElementType: string;
  LCount: Int64;
begin
  if AType = nil then
    Exit('void ' + AName);

  if AType.Kind = tkArray then
  begin
    if AType.IsFlexibleArray then
    begin
      // Flexible array member (C99): type name[]
      LElementType := TypeToC(AType.ElementType);
      Result := Format('%s %s[]', [LElementType, AName]);
    end
    else if not AType.IsDynamic then
    begin
      // Static array: type name[size]
      LElementType := TypeToC(AType.ElementType);
      LCount := AType.HighBound - AType.LowBound + 1;
      Result := Format('%s %s[%d]', [LElementType, AName, LCount]);
    end
    else
      Result := TypeToC(AType) + ' ' + AName;
  end
  else
    Result := TypeToC(AType) + ' ' + AName;
end;

procedure TPaxCodeGen.Generate(const ARoot: PASTNode);
begin
  Clear();

  if ARoot = nil then
    Exit;

  if ARoot^.Kind = nkModule then
    GenerateModule(ARoot);
end;

procedure TPaxCodeGen.Clear();
var
  LFile: TSourceFile;
begin
  for LFile := Low(TSourceFile) to High(TSourceFile) do
    FBuilders[LFile].Clear();

  FIndent := 0;
  FModuleName := '';
  // Note: FSourceFile is NOT cleared here - it's set by caller before Generate()

  // Reset stats
  FHeaderLines := 0;
  FSourceLines := 0;
end;

function TPaxCodeGen.GetHeader(): string;
begin
  Result := FBuilders[sfHeader].ToString();
end;

function TPaxCodeGen.GetSource(): string;
begin
  Result := FBuilders[sfSource].ToString();
end;

procedure TPaxCodeGen.GenerateModule(const ANode: PASTNode);
var
  LI: Integer;
  LJ: Integer;
  LChild: PASTNode;
  LImportNode: PASTNode;
  LTypeDecl: PASTNode;
  LHasMain: Boolean;
  LModuleKind: string;
  LImportName: string;
  LTestBlocks: TList<PASTNode>;
  LTestIndex: Integer;
  LTestName: string;
  LTestFuncName: string;
begin
  FModuleName := ANode^.NodeName;
  LModuleKind := LowerCase(ANode^.StrVal);
  LHasMain := (LModuleKind = 'exe') or (LModuleKind = '''exe''') or (LModuleKind = '');
  FIsDLL := (LModuleKind = 'dll') or (LModuleKind = '''dll''');

  // Generate preambles
  GenerateHeaderPreamble();
  GenerateSourcePreamble();

  // Generate #include for imported modules
  for LI := 0 to GetASTChildCount(ANode) - 1 do
  begin
    LChild := GetASTChild(ANode, LI);

    if (LChild^.Kind = nkBlock) and (LChild^.NodeName = 'imports') then
    begin
      for LJ := 0 to GetASTChildCount(LChild) - 1 do
      begin
        LImportNode := GetASTChild(LChild, LJ);
        if LImportNode^.Kind = nkImport then
        begin
          LImportName := LImportNode^.NodeName;
          EmitLnFmt(sfSource, '#include "%s.h"', [LImportName]);
        end;
      end;
    end
    else if LChild^.Kind = nkImport then
    begin
      LImportName := LChild^.NodeName;
      EmitLnFmt(sfSource, '#include "%s.h"', [LImportName]);
    end;
  end;
  EmitLn(sfSource);

  // First pass: collect public type declarations for header
  for LI := 0 to GetASTChildCount(ANode) - 1 do
  begin
    LChild := GetASTChild(ANode, LI);

    if LChild^.Kind = nkBlock then
    begin
      if LChild^.NodeName = 'types' then
      begin
        for LJ := 0 to GetASTChildCount(LChild) - 1 do
        begin
          LTypeDecl := GetASTChild(LChild, LJ);
          if LTypeDecl^.IsPublic then
            GenerateTypeDecl(sfHeader, LTypeDecl);
        end;
      end;
    end
    else if LChild^.Kind = nkTypeDecl then
    begin
      if LChild^.IsPublic then
        GenerateTypeDecl(sfHeader, LChild);
    end;
  end;

  // Generate public routine prototypes in header
  for LI := 0 to GetASTChildCount(ANode) - 1 do
  begin
    LChild := GetASTChild(ANode, LI);
    if (LChild^.Kind = nkRoutineDecl) and LChild^.IsPublic and (not LChild^.IsExternal) then
      GenerateRoutinePrototype(sfHeader, LChild);
  end;

  // Generate extern declarations in header for external routines
  for LI := 0 to GetASTChildCount(ANode) - 1 do
  begin
    LChild := GetASTChild(ANode, LI);
    if (LChild^.Kind = nkRoutineDecl) and LChild^.IsExternal then
      GenerateExternDecl(sfHeader, LChild);
  end;

  GenerateHeaderPostamble();

  // Second pass: generate all declarations in source
  for LI := 0 to GetASTChildCount(ANode) - 1 do
  begin
    LChild := GetASTChild(ANode, LI);

    case LChild^.Kind of
      nkDirective:    GenerateDirective(LChild);
      nkConstDecl:    GenerateConstDecl(sfSource, LChild);
      nkTypeDecl:     GenerateTypeDecl(sfSource, LChild);
      nkVarDecl:      GenerateVarDecl(sfSource, LChild, False);
      nkRoutineDecl:  GenerateRoutineDecl(LChild);
      nkBlock:
        begin
          if LChild^.NodeName = 'consts' then
          begin
            for LJ := 0 to GetASTChildCount(LChild) - 1 do
              GenerateConstDecl(sfSource, GetASTChild(LChild, LJ));
          end
          else if LChild^.NodeName = 'types' then
          begin
            for LJ := 0 to GetASTChildCount(LChild) - 1 do
              GenerateTypeDecl(sfSource, GetASTChild(LChild, LJ));
          end
          else if LChild^.NodeName = 'vars' then
          begin
            for LJ := 0 to GetASTChildCount(LChild) - 1 do
              GenerateVarDecl(sfSource, GetASTChild(LChild, LJ), False);
          end;
        end;
    end;
  end;

  // Collect and generate test blocks
  LTestBlocks := TList<PASTNode>.Create();
  try
    for LI := 0 to GetASTChildCount(ANode) - 1 do
    begin
      LChild := GetASTChild(ANode, LI);
      if LChild^.Kind = nkTestBlock then
        LTestBlocks.Add(LChild);
    end;

    // Generate test functions
    for LTestIndex := 0 to LTestBlocks.Count - 1 do
      GenerateTestBlock(LTestBlocks[LTestIndex], LTestIndex);

    // Generate module's test registration function
    EmitLn(sfSource);
    EmitLn(sfSource, '#ifdef PAX_UNITTESTING');
    EmitLnFmt(sfSource, 'void %s_register_tests(void) {', [FModuleName]);
    IncIndent();
    for LTestIndex := 0 to LTestBlocks.Count - 1 do
    begin
      LTestName := LTestBlocks[LTestIndex]^.NodeName;
      if (Length(LTestName) >= 2) and (LTestName[1] = '''') then
        LTestName := Copy(LTestName, 2, Length(LTestName) - 2);
      LTestFuncName := Format('pax_test_%d', [LTestIndex]);
      EmitLnFmt(sfSource, 'pax_test_register("%s", %s, "%s", %d);', 
        [EscapeString(LTestName), LTestFuncName, FSourceFile, LTestBlocks[LTestIndex]^.Token.Range.StartLine]);
    end;
    DecIndent();
    EmitLn(sfSource, '}');
    EmitLn(sfSource, '#endif // PAX_UNITTESTING');

    // Generate main function for exe modules
    if LHasMain then
    begin
      EmitLn(sfSource);
      EmitLn(sfSource, 'int main(int argc, char** argv) {');
      IncIndent();

      // Initialize runtime (including GC)
      EmitLn(sfSource, 'pax_init(argc, argv);');
      EmitLn(sfSource);

      // If tests exist (local or imported), add conditional test execution block
      EmitLn(sfSource, '#ifdef PAX_UNITTESTING');

      // Call imported modules' registration functions
      for LI := 0 to GetASTChildCount(ANode) - 1 do
      begin
        LChild := GetASTChild(ANode, LI);
        if (LChild^.Kind = nkBlock) and (LChild^.NodeName = 'imports') then
        begin
          for LJ := 0 to GetASTChildCount(LChild) - 1 do
          begin
            LImportNode := GetASTChild(LChild, LJ);
            if LImportNode^.Kind = nkImport then
              EmitLnFmt(sfSource, '%s_register_tests();', [LImportNode^.NodeName]);
          end;
        end
        else if LChild^.Kind = nkImport then
          EmitLnFmt(sfSource, '%s_register_tests();', [LChild^.NodeName]);
      end;

      // Register local tests
      EmitLnFmt(sfSource, '%s_register_tests();', [FModuleName]);

      EmitLn(sfSource, 'return pax_test_run_all();');
      EmitLn(sfSource, '#endif');
      EmitLn(sfSource);

      // Find and generate initialization block
      for LI := 0 to GetASTChildCount(ANode) - 1 do
      begin
        LChild := GetASTChild(ANode, LI);
        if (LChild^.Kind = nkBlock) and (LChild^.NodeName = 'block') then
          GenerateBlock(LChild);
      end;

      EmitLn(sfSource, 'return 0;');
      DecIndent();
      EmitLn(sfSource, '}');
    end;
  finally
    LTestBlocks.Free();
  end;
end;

procedure TPaxCodeGen.GenerateHeaderPreamble();
var
  LGuard: string;
begin
  LGuard := UpperCase(FModuleName) + '_H';

  EmitLnFmt(sfHeader, '// Generated from %s.pax', [FModuleName]);
  EmitLn(sfHeader, '#ifndef ' + LGuard);
  EmitLn(sfHeader, '#define ' + LGuard);
  EmitLn(sfHeader);
  EmitLn(sfHeader, '#include <stdint.h>');
  EmitLn(sfHeader, '#include <stdbool.h>');
  EmitLn(sfHeader, '#include <wchar.h>');
  EmitLn(sfHeader, '#include "pax_runtime.h"');
  EmitLn(sfHeader);
end;

procedure TPaxCodeGen.GenerateSourcePreamble();
begin
  EmitLnFmt(sfSource, '// Generated from %s.pax', [FModuleName]);
  EmitLn(sfSource);
  EmitLn(sfSource, '#include <stdint.h>');
  EmitLn(sfSource, '#include <stdbool.h>');
  EmitLn(sfSource, '#include <wchar.h>');
  EmitLn(sfSource, '#include "pax_runtime.h"');
  EmitLn(sfSource, '#ifdef PAX_UNITTESTING');
  EmitLn(sfSource, '#include "pax_unittest.h"');
  EmitLn(sfSource, '#endif');
  EmitLnFmt(sfSource, '#include "%s.h"', [FModuleName]);
end;

procedure TPaxCodeGen.GenerateHeaderPostamble();
begin
  // Declare test registration function (always present, may be empty)
  EmitLn(sfHeader);
  EmitLn(sfHeader, '#ifdef PAX_UNITTESTING');
  EmitLnFmt(sfHeader, 'void %s_register_tests(void);', [FModuleName]);
  EmitLn(sfHeader, '#endif');

  EmitLn(sfHeader);
  EmitLn(sfHeader, '#endif');
end;

procedure TPaxCodeGen.GenerateDirective(const ANode: PASTNode);
var
  LName: string;
  LValue: string;
  LSpacePos: Integer;
begin
  LName := LowerCase(ANode^.NodeName);
  LValue := ANode^.StrVal;

  if LName = '#define' then
  begin
    // Split name and value
    LSpacePos := Pos(' ', LValue);
    if LSpacePos > 0 then
      EmitLnFmt(sfSource, '#define %s %s', [Copy(LValue, 1, LSpacePos - 1), Copy(LValue, LSpacePos + 1, Length(LValue))])
    else
      EmitLnFmt(sfSource, '#define %s', [LValue]);
  end
  else if LName = '#undef' then
    EmitLnFmt(sfSource, '#undef %s', [LValue])
  else if LName = '#ifdef' then
    EmitLnFmt(sfSource, '#ifdef %s', [LValue])
  else if LName = '#ifndef' then
    EmitLnFmt(sfSource, '#ifndef %s', [LValue])
  else if LName = '#if' then
    EmitLnFmt(sfSource, '#if %s', [LValue])
  else if LName = '#elif' then
    EmitLnFmt(sfSource, '#elif %s', [LValue])
  else if LName = '#else' then
    EmitLn(sfSource, '#else')
  else if LName = '#endif' then
    EmitLn(sfSource, '#endif');
  // Other directives (#library, #modulepath, etc.) are handled during compilation, not code generation
end;

procedure TPaxCodeGen.GenerateConstDecl(const AFile: TSourceFile; const ANode: PASTNode);
var
  LType: TPaxType;
  LValue: string;
  LSym: TSymbol;
  LValueNode: PASTNode;
begin
  LSym := nil;
  if FSymbols <> nil then
    LSym := FSymbols.Lookup(ANode^.NodeName);

  if LSym <> nil then
    LType := LSym.SymbolType
  else
    LType := nil;

  // Get value expression
  if GetASTChildCount(ANode) > 0 then
  begin
    LValueNode := GetASTChild(ANode, GetASTChildCount(ANode) - 1);
    LValue := GenerateExpression(LValueNode);
  end
  else
    LValue := '0';

  if LType <> nil then
    EmitLnFmt(AFile, 'const %s = %s;', [TypeToCDecl(LType, ANode^.NodeName), LValue])
  else
    EmitLnFmt(AFile, '#define %s %s', [ANode^.NodeName, LValue]);
end;

procedure TPaxCodeGen.GenerateTypeDecl(const AFile: TSourceFile; const ANode: PASTNode);
var
  LTypeNode: PASTNode;
  LSym: TSymbol;
begin
  if GetASTChildCount(ANode) = 0 then
    Exit;

  LTypeNode := GetASTChild(ANode, 0);

  case LTypeNode^.Kind of
    nkRecordType:
      GenerateRecordType(AFile, LTypeNode, ANode^.NodeName);
    nkUnionType:
      GenerateUnionType(AFile, LTypeNode, ANode^.NodeName);
    nkArrayType, nkPointerType, nkSetType:
      begin
        LSym := nil;
        if FSymbols <> nil then
          LSym := FSymbols.Lookup(ANode^.NodeName);

        if LSym <> nil then
          EmitLnFmt(AFile, 'typedef %s %s;', [TypeToC(LSym.SymbolType), ANode^.NodeName]);
      end;
  end;
end;

procedure TPaxCodeGen.GenerateRecordType(const AFile: TSourceFile; const ANode: PASTNode; const AName: string);
var
  LI: Integer;
  LChild: PASTNode;
  LTypeNode: PASTNode;
  LFieldType: TPaxType;
  LIsPacked: Boolean;
  LAlignment: Integer;
begin
  // Check if this is a packed record
  LIsPacked := ANode^.IsPacked;
  LAlignment := ANode^.Alignment;

  // Emit pragma pack for packed records
  if LIsPacked then
    EmitLn(AFile, '#pragma pack(push, 1)');

  // Emit struct with alignment attribute if specified
  if LAlignment > 0 then
    EmitLnFmt(AFile, 'typedef struct __attribute__((aligned(%d))) %s {', [LAlignment, AName])
  else
    EmitLnFmt(AFile, 'typedef struct %s {', [AName]);
  IncIndent();

  for LI := 0 to GetASTChildCount(ANode) - 1 do
  begin
    LChild := GetASTChild(ANode, LI);

    if LChild^.Kind = nkFieldDecl then
    begin
      LFieldType := nil;

      // Resolve field type from AST node
      if GetASTChildCount(LChild) > 0 then
      begin
        LTypeNode := GetASTChild(LChild, 0);
        LFieldType := ResolveTypeNode(LTypeNode);
      end;

      if LFieldType <> nil then
      begin
        // Check for bit field
        if LChild^.BitWidth > 0 then
          EmitLnFmt(AFile, '%s : %d;', [TypeToCDecl(LFieldType, LChild^.NodeName), LChild^.BitWidth])
        else
          EmitLnFmt(AFile, '%s;', [TypeToCDecl(LFieldType, LChild^.NodeName)]);
      end
      else
        EmitLnFmt(AFile, 'void* %s; // unknown type', [LChild^.NodeName]);
    end
    else if LChild^.Kind = nkUnionType then
    begin
      // Anonymous union inside record
      GenerateAnonymousUnion(AFile, LChild);
    end;
  end;

  DecIndent();
  EmitLnFmt(AFile, '} %s;', [AName]);

  // Close pragma pack for packed records
  if LIsPacked then
    EmitLn(AFile, '#pragma pack(pop)');

  EmitLn(AFile);
end;

procedure TPaxCodeGen.GenerateUnionType(const AFile: TSourceFile; const ANode: PASTNode; const AName: string);
var
  LI: Integer;
  LChild: PASTNode;
  LTypeNode: PASTNode;
  LFieldType: TPaxType;
begin
  EmitLnFmt(AFile, 'typedef union %s {', [AName]);
  IncIndent();

  for LI := 0 to GetASTChildCount(ANode) - 1 do
  begin
    LChild := GetASTChild(ANode, LI);

    if LChild^.Kind = nkFieldDecl then
    begin
      LFieldType := nil;

      // Resolve field type from AST node
      if GetASTChildCount(LChild) > 0 then
      begin
        LTypeNode := GetASTChild(LChild, 0);
        LFieldType := ResolveTypeNode(LTypeNode);
      end;

      if LFieldType <> nil then
      begin
        // Check for bit field
        if LChild^.BitWidth > 0 then
          EmitLnFmt(AFile, '%s : %d;', [TypeToCDecl(LFieldType, LChild^.NodeName), LChild^.BitWidth])
        else
          EmitLnFmt(AFile, '%s;', [TypeToCDecl(LFieldType, LChild^.NodeName)]);
      end
      else
        EmitLnFmt(AFile, 'void* %s; // unknown type', [LChild^.NodeName]);
    end
    else if LChild^.Kind = nkRecordType then
    begin
      // Anonymous record inside union
      GenerateAnonymousRecord(AFile, LChild);
    end;
  end;

  DecIndent();
  EmitLnFmt(AFile, '} %s;', [AName]);
  EmitLn(AFile);
end;

procedure TPaxCodeGen.GenerateAnonymousRecord(const AFile: TSourceFile; const ANode: PASTNode);
var
  LI: Integer;
  LChild: PASTNode;
  LTypeNode: PASTNode;
  LFieldType: TPaxType;
begin
  // Anonymous struct inside union
  EmitLn(AFile, 'struct {');
  IncIndent();

  for LI := 0 to GetASTChildCount(ANode) - 1 do
  begin
    LChild := GetASTChild(ANode, LI);

    if LChild^.Kind = nkFieldDecl then
    begin
      LFieldType := nil;

      if GetASTChildCount(LChild) > 0 then
      begin
        LTypeNode := GetASTChild(LChild, 0);
        LFieldType := ResolveTypeNode(LTypeNode);
      end;

      if LFieldType <> nil then
      begin
        // Check for bit field
        if LChild^.BitWidth > 0 then
          EmitLnFmt(AFile, '%s : %d;', [TypeToCDecl(LFieldType, LChild^.NodeName), LChild^.BitWidth])
        else
          EmitLnFmt(AFile, '%s;', [TypeToCDecl(LFieldType, LChild^.NodeName)]);
      end
      else
        EmitLnFmt(AFile, 'void* %s; // unknown type', [LChild^.NodeName]);
    end
    else if LChild^.Kind = nkUnionType then
    begin
      // Nested anonymous union
      GenerateAnonymousUnion(AFile, LChild);
    end;
  end;

  DecIndent();
  EmitLn(AFile, '};');
end;

procedure TPaxCodeGen.GenerateAnonymousUnion(const AFile: TSourceFile; const ANode: PASTNode);
var
  LI: Integer;
  LChild: PASTNode;
  LTypeNode: PASTNode;
  LFieldType: TPaxType;
begin
  // Anonymous union inside record
  EmitLn(AFile, 'union {');
  IncIndent();

  for LI := 0 to GetASTChildCount(ANode) - 1 do
  begin
    LChild := GetASTChild(ANode, LI);

    if LChild^.Kind = nkFieldDecl then
    begin
      LFieldType := nil;

      if GetASTChildCount(LChild) > 0 then
      begin
        LTypeNode := GetASTChild(LChild, 0);
        LFieldType := ResolveTypeNode(LTypeNode);
      end;

      if LFieldType <> nil then
      begin
        // Check for bit field
        if LChild^.BitWidth > 0 then
          EmitLnFmt(AFile, '%s : %d;', [TypeToCDecl(LFieldType, LChild^.NodeName), LChild^.BitWidth])
        else
          EmitLnFmt(AFile, '%s;', [TypeToCDecl(LFieldType, LChild^.NodeName)]);
      end
      else
        EmitLnFmt(AFile, 'void* %s; // unknown type', [LChild^.NodeName]);
    end
    else if LChild^.Kind = nkRecordType then
    begin
      // Nested anonymous record
      GenerateAnonymousRecord(AFile, LChild);
    end;
  end;

  DecIndent();
  EmitLn(AFile, '};');
end;

procedure TPaxCodeGen.GenerateVarDecl(const AFile: TSourceFile; const ANode: PASTNode; const AIsLocal: Boolean);
var
  LSym: TSymbol;
  LType: TPaxType;
  LInit: string;
begin
  LSym := nil;
  if FSymbols <> nil then
    LSym := FSymbols.Lookup(ANode^.NodeName);

  if LSym <> nil then
    LType := LSym.SymbolType
  else
    LType := nil;

  // Check for initializer
  LInit := '';
  if GetASTChildCount(ANode) > 1 then
    LInit := ' = ' + GenerateExpression(GetASTChild(ANode, 1));

  if LType <> nil then
    EmitLnFmt(AFile, '%s%s;', [TypeToCDecl(LType, ANode^.NodeName), LInit])
  else
    EmitLnFmt(AFile, 'void* %s%s; // unknown type', [ANode^.NodeName, LInit]);
end;

procedure TPaxCodeGen.GenerateRoutineDecl(const ANode: PASTNode);
var
  LSym: TSymbol;
  LReturnType: TPaxType;
  LI: Integer;
  LJ: Integer;
  LChild: PASTNode;
  LParam: PASTNode;
  LParamSym: TSymbol;
  LParamType: TPaxType;
  LTypeNode: PASTNode;
begin
  // External routines are handled via GenerateExternDecl in header
  if ANode^.IsExternal then
    Exit;

  // Emit #line directive for source mapping
  EmitLineDirective(ANode);

  LSym := nil;
  if FSymbols <> nil then
    LSym := FSymbols.Lookup(ANode^.NodeName);

  if (LSym <> nil) and (LSym.SymbolType <> nil) then
    LReturnType := LSym.SymbolType.ReturnType
  else
    LReturnType := nil;

  // Store return type for GenerateReturnStmt
  FCurrentReturnType := LReturnType;

  // Push scope and register parameters for type lookup during code gen
  if FSymbols <> nil then
  begin
    FSymbols.PushScope(ANode^.NodeName);

    // Register parameters in scope
    for LI := 0 to GetASTChildCount(ANode) - 1 do
    begin
      LChild := GetASTChild(ANode, LI);
      if (LChild^.Kind = nkBlock) and (LChild^.NodeName = 'params') then
      begin
        for LJ := 0 to GetASTChildCount(LChild) - 1 do
        begin
          LParam := GetASTChild(LChild, LJ);
          if LParam^.Kind = nkParamDecl then
          begin
            LParamType := nil;
            if GetASTChildCount(LParam) > 0 then
            begin
              LTypeNode := GetASTChild(LParam, 0);
              if (FTypes <> nil) and (LTypeNode^.Kind = nkTypeRef) then
                LParamType := FTypes.GetType(LTypeNode^.NodeName);
            end;
            if LParamType <> nil then
            begin
              LParamSym := FSymbols.Define(LParam^.NodeName, skParam);
              LParamSym.SymbolType := LParamType;
            end;
          end;
        end;
      end;
    end;

    // Register local variables in scope
    for LI := 0 to GetASTChildCount(ANode) - 1 do
    begin
      LChild := GetASTChild(ANode, LI);
      if (LChild^.Kind = nkBlock) and (LChild^.NodeName = 'locals') then
      begin
        for LJ := 0 to GetASTChildCount(LChild) - 1 do
        begin
          LParam := GetASTChild(LChild, LJ);
          if LParam^.Kind = nkVarDecl then
          begin
            LParamType := nil;
            if GetASTChildCount(LParam) > 0 then
            begin
              LTypeNode := GetASTChild(LParam, 0);
              if (FTypes <> nil) and (LTypeNode^.Kind = nkTypeRef) then
                LParamType := FTypes.GetType(LTypeNode^.NodeName);
            end;
            if LParamType <> nil then
            begin
              LParamSym := FSymbols.Define(LParam^.NodeName, skVar);
              LParamSym.SymbolType := LParamType;
            end;
          end;
        end;
      end;
    end;
  end;

  // Function signature
  Emit(sfSource, GetIndentStr());

  // Add dllexport for public routines in DLL modules
  if FIsDLL and ANode^.IsPublic then
    Emit(sfSource, '__declspec(dllexport) ');

  // Non-public routines are static (internal linkage)
  if not ANode^.IsPublic then
    Emit(sfSource, 'static ');

  if LReturnType <> nil then
    Emit(sfSource, TypeToC(LReturnType))
  else
    Emit(sfSource, 'void');

  EmitFmt(sfSource, ' %s(', [ANode^.NodeName]);

  // Parameters
  GenerateParamList(sfSource, ANode);

  Emit(sfSource, ') {');
  EmitLn(sfSource);
  IncIndent();

  // Local variables
  for LI := 0 to GetASTChildCount(ANode) - 1 do
  begin
    LChild := GetASTChild(ANode, LI);

    if (LChild^.Kind = nkBlock) and (LChild^.NodeName = 'locals') then
    begin
      for LJ := 0 to GetASTChildCount(LChild) - 1 do
        GenerateVarDecl(sfSource, GetASTChild(LChild, LJ), True);

      if GetASTChildCount(LChild) > 0 then
        EmitLn(sfSource);
    end;
  end;

  // Body
  for LI := 0 to GetASTChildCount(ANode) - 1 do
  begin
    LChild := GetASTChild(ANode, LI);

    if (LChild^.Kind = nkBlock) and (LChild^.NodeName = 'block') then
      GenerateBlock(LChild);
  end;

  DecIndent();
  EmitLn(sfSource, '}');
  EmitLn(sfSource);

  // Pop scope and clear return type
  FCurrentReturnType := nil;
  if FSymbols <> nil then
    FSymbols.PopScope();
end;

procedure TPaxCodeGen.GenerateExternDecl(const AFile: TSourceFile; const ANode: PASTNode);
var
  LSym: TSymbol;
  LReturnType: TPaxType;
begin
  LSym := nil;
  if FSymbols <> nil then
    LSym := FSymbols.Lookup(ANode^.NodeName);

  if (LSym <> nil) and (LSym.SymbolType <> nil) then
    LReturnType := LSym.SymbolType.ReturnType
  else
    LReturnType := nil;

  Emit(AFile, GetIndentStr());
  Emit(AFile, 'extern ');
  if LReturnType <> nil then
    Emit(AFile, TypeToC(LReturnType))
  else
    Emit(AFile, 'void');

  EmitFmt(AFile, ' %s(', [ANode^.NodeName]);
  GenerateParamList(AFile, ANode);
  Emit(AFile, ');');
  EmitLn(AFile);
end;

procedure TPaxCodeGen.GenerateRoutinePrototype(const AFile: TSourceFile; const ANode: PASTNode);
var
  LSym: TSymbol;
  LReturnType: TPaxType;
begin
  if ANode^.IsExternal then
    Exit;

  LSym := nil;
  if FSymbols <> nil then
    LSym := FSymbols.Lookup(ANode^.NodeName);

  if (LSym <> nil) and (LSym.SymbolType <> nil) then
    LReturnType := LSym.SymbolType.ReturnType
  else
    LReturnType := nil;

  Emit(AFile, GetIndentStr());

  // Add dllexport for public routines in DLL modules
  if FIsDLL and ANode^.IsPublic then
    Emit(AFile, '__declspec(dllexport) ');

  if LReturnType <> nil then
    Emit(AFile, TypeToC(LReturnType))
  else
    Emit(AFile, 'void');

  EmitFmt(AFile, ' %s(', [ANode^.NodeName]);
  GenerateParamList(AFile, ANode);
  Emit(AFile, ');');
  EmitLn(AFile);
end;

procedure TPaxCodeGen.GenerateParamList(const AFile: TSourceFile; const ANode: PASTNode);
var
  LI, LJ: Integer;
  LChild, LParam: PASTNode;
  LTypeNode: PASTNode;
  LFirst: Boolean;
  LParamType: TPaxType;
begin
  LFirst := True;

  for LI := 0 to GetASTChildCount(ANode) - 1 do
  begin
    LChild := GetASTChild(ANode, LI);

    if (LChild^.Kind = nkBlock) and (LChild^.NodeName = 'params') then
    begin
      for LJ := 0 to GetASTChildCount(LChild) - 1 do
      begin
        LParam := GetASTChild(LChild, LJ);

        if LParam^.Kind = nkParamDecl then
        begin
          if not LFirst then
            Emit(AFile, ', ');
          LFirst := False;

          LParamType := nil;
          if GetASTChildCount(LParam) > 0 then
          begin
            LTypeNode := GetASTChild(LParam, 0);
            if (FTypes <> nil) and (LTypeNode^.Kind = nkTypeRef) then
              LParamType := FTypes.GetType(LTypeNode^.NodeName);
          end;

          if LParamType <> nil then
          begin
            // var parameters become pointers
            if LParam^.StrVal = 'var' then
              EmitFmt(AFile, '%s* %s', [TypeToC(LParamType), LParam^.NodeName])
            else
              EmitFmt(AFile, '%s %s', [TypeToC(LParamType), LParam^.NodeName]);
          end
          else
            EmitFmt(AFile, 'void* %s', [LParam^.NodeName]);
        end;
      end;
    end;
  end;

  // Handle variadic routines
  if ANode^.IsVariadic then
  begin
    if not LFirst then
      Emit(AFile, ', ...')
    else
      Emit(AFile, '...');
  end
  else if LFirst then
    Emit(AFile, 'void');
end;

procedure TPaxCodeGen.GenerateTestBlock(const ANode: PASTNode; const AIndex: Integer);
var
  LI: Integer;
  LJ: Integer;
  LChild: PASTNode;
  LTestName: string;
  LFuncName: string;
begin
  // Get test name (strip quotes)
  LTestName := ANode^.NodeName;
  if (Length(LTestName) >= 2) and (LTestName[1] = '''') then
    LTestName := Copy(LTestName, 2, Length(LTestName) - 2);

  // Generate sanitized function name
  LFuncName := Format('pax_test_%d', [AIndex]);

  // Emit #line directive for source mapping
  EmitLineDirective(ANode);

  // Generate test function
  EmitLn(sfSource);
  EmitLn(sfSource, '#ifdef PAX_UNITTESTING');
  EmitLnFmt(sfSource, 'static void %s(void) {', [LFuncName]);
  IncIndent();

  // Register local variables in scope if needed
  if FSymbols <> nil then
    FSymbols.PushScope('test:' + LTestName);

  // Local variables
  for LI := 0 to GetASTChildCount(ANode) - 1 do
  begin
    LChild := GetASTChild(ANode, LI);

    if (LChild^.Kind = nkBlock) and (LChild^.NodeName = 'locals') then
    begin
      for LJ := 0 to GetASTChildCount(LChild) - 1 do
        GenerateVarDecl(sfSource, GetASTChild(LChild, LJ), True);

      if GetASTChildCount(LChild) > 0 then
        EmitLn(sfSource);
    end;
  end;

  // Body
  for LI := 0 to GetASTChildCount(ANode) - 1 do
  begin
    LChild := GetASTChild(ANode, LI);

    if (LChild^.Kind = nkBlock) and (LChild^.NodeName = 'block') then
      GenerateBlock(LChild);
  end;

  if FSymbols <> nil then
    FSymbols.PopScope();

  DecIndent();
  EmitLn(sfSource, '}');
  EmitLn(sfSource, '#endif // PAX_UNITTESTING');
end;

procedure TPaxCodeGen.GenerateBlock(const ANode: PASTNode);
var
  LI: Integer;
begin
  for LI := 0 to GetASTChildCount(ANode) - 1 do
    GenerateStatement(GetASTChild(ANode, LI));
end;

procedure TPaxCodeGen.GenerateStatement(const ANode: PASTNode);
begin
  if ANode = nil then
    Exit;

  // Emit #line directive for source mapping
  EmitLineDirective(ANode);

  case ANode^.Kind of
    nkIfStmt:         GenerateIfStmt(ANode);
    nkWhileStmt:      GenerateWhileStmt(ANode);
    nkForStmt:        GenerateForStmt(ANode);
    nkRepeatStmt:     GenerateRepeatStmt(ANode);
    nkCaseStmt:       GenerateCaseStmt(ANode);
    nkReturnStmt:     GenerateReturnStmt(ANode);
    nkAssignment:     GenerateAssignment(ANode);
    nkCallStmt:       GenerateCallStmt(ANode);
    nkNewStmt:        GenerateNewStmt(ANode);
    nkDisposeStmt:    GenerateDisposeStmt(ANode);
    nkSetLengthStmt:  GenerateSetLengthStmt(ANode);
    nkGcCollectStmt:  GenerateGcCollectStmt(ANode);
    nkGcDumpStmt:     GenerateGcDumpStmt(ANode);
    nkEmptyStmt:      ; // Nothing to emit
    nkBlock:          GenerateBlock(ANode);
  end;
end;

procedure TPaxCodeGen.GenerateIfStmt(const ANode: PASTNode);
var
  LCond: string;
begin
  if GetASTChildCount(ANode) = 0 then
    Exit;

  LCond := GenerateExpression(GetASTChild(ANode, 0));

  EmitLnFmt(sfSource, 'if (%s) {', [LCond]);
  IncIndent();

  if GetASTChildCount(ANode) > 1 then
    GenerateBlock(GetASTChild(ANode, 1));

  DecIndent();

  if GetASTChildCount(ANode) > 2 then
  begin
    EmitLn(sfSource, '} else {');
    IncIndent();
    GenerateBlock(GetASTChild(ANode, 2));
    DecIndent();
  end;

  EmitLn(sfSource, '}');
end;

procedure TPaxCodeGen.GenerateWhileStmt(const ANode: PASTNode);
var
  LCond: string;
begin
  if GetASTChildCount(ANode) = 0 then
    Exit;

  LCond := GenerateExpression(GetASTChild(ANode, 0));

  EmitLnFmt(sfSource, 'while (%s) {', [LCond]);
  IncIndent();

  if GetASTChildCount(ANode) > 1 then
    GenerateBlock(GetASTChild(ANode, 1));

  DecIndent();
  EmitLn(sfSource, '}');
end;

procedure TPaxCodeGen.GenerateForStmt(const ANode: PASTNode);
var
  LVarName: string;
  LFrom, LTo: string;
  LIsDownto: Boolean;
  LCmpOp, LIncOp: string;
begin
  if GetASTChildCount(ANode) < 4 then
    Exit;

  LVarName := GetASTChild(ANode, 0)^.NodeName;
  LFrom := GenerateExpression(GetASTChild(ANode, 1));
  LTo := GenerateExpression(GetASTChild(ANode, 2));
  LIsDownto := not ANode^.BoolVal;

  if LIsDownto then
  begin
    LCmpOp := '>=';
    LIncOp := '--';
  end
  else
  begin
    LCmpOp := '<=';
    LIncOp := '++';
  end;

  EmitLnFmt(sfSource, 'for (%s = %s; %s %s %s; %s%s) {',
    [LVarName, LFrom, LVarName, LCmpOp, LTo, LVarName, LIncOp]);
  IncIndent();

  GenerateBlock(GetASTChild(ANode, 3));

  DecIndent();
  EmitLn(sfSource, '}');
end;

procedure TPaxCodeGen.GenerateRepeatStmt(const ANode: PASTNode);
var
  LCond: string;
begin
  EmitLn(sfSource, 'do {');
  IncIndent();

  if GetASTChildCount(ANode) > 0 then
    GenerateBlock(GetASTChild(ANode, 0));

  DecIndent();

  if GetASTChildCount(ANode) > 1 then
    LCond := GenerateExpression(GetASTChild(ANode, 1))
  else
    LCond := 'true';

  EmitLnFmt(sfSource, '} while (!(%s));', [LCond]);
end;

procedure TPaxCodeGen.GenerateCaseStmt(const ANode: PASTNode);
var
  LI, LJ: Integer;
  LChild, LArmChild: PASTNode;
  LSelector: string;
  LFirst: Boolean;
  LLabelFirst: Boolean;
  LLabelExpr: string;
  LLow: string;
  LHigh: string;
begin
  if GetASTChildCount(ANode) = 0 then
    Exit;

  LSelector := GenerateExpression(GetASTChild(ANode, 0));

  // Generate as if-else chain for safety
  LFirst := True;

  for LI := 1 to GetASTChildCount(ANode) - 1 do
  begin
    LChild := GetASTChild(ANode, LI);

    if LChild^.Kind = nkCaseArm then
    begin
      if LFirst then
        Emit(sfSource, GetIndentStr() + 'if (')
      else
        Emit(sfSource, ' else if (');

      // Generate label conditions
      LLabelFirst := True;
      for LJ := 0 to GetASTChildCount(LChild) - 1 do
      begin
        LArmChild := GetASTChild(LChild, LJ);

        if LArmChild^.Kind = nkBlock then
          Continue;

        if not LLabelFirst then
          Emit(sfSource, ' || ');
        LLabelFirst := False;

        if LArmChild^.Kind = nkSetRange then
        begin
          // Range label
          LLow := GenerateExpression(GetASTChild(LArmChild, 0));
          LHigh := GenerateExpression(GetASTChild(LArmChild, 1));
          EmitFmt(sfSource, '(%s >= %s && %s <= %s)', [LSelector, LLow, LSelector, LHigh]);
        end
        else
        begin
          LLabelExpr := GenerateExpression(LArmChild);
          EmitFmt(sfSource, '(%s == %s)', [LSelector, LLabelExpr]);
        end;
      end;

      Emit(sfSource, ') {');
      EmitLn(sfSource);
      IncIndent();

      // Generate body
      for LJ := 0 to GetASTChildCount(LChild) - 1 do
      begin
        LArmChild := GetASTChild(LChild, LJ);
        if LArmChild^.Kind = nkBlock then
          GenerateBlock(LArmChild);
      end;

      DecIndent();
      Emit(sfSource, GetIndentStr() + '}');

      LFirst := False;
    end
    else if LChild^.Kind = nkBlock then
    begin
      // Else block
      Emit(sfSource, ' else {');
      EmitLn(sfSource);
      IncIndent();
      GenerateBlock(LChild);
      DecIndent();
      Emit(sfSource, GetIndentStr() + '}');
    end;
  end;

  EmitLn(sfSource);
end;

procedure TPaxCodeGen.GenerateReturnStmt(const ANode: PASTNode);
var
  LExpr: string;
  LValueNode: PASTNode;
begin
  if GetASTChildCount(ANode) > 0 then
  begin
    LValueNode := GetASTChild(ANode, 0);
    LExpr := GenerateExpression(LValueNode);

    // Special handling: returning string literal when return type is string
    if (FCurrentReturnType <> nil) then
    begin
      // string return with string literal -> wrap with pax_string_new
      if (FCurrentReturnType.Kind = tkString) and (LValueNode^.Kind = nkStrLiteral) then
      begin
        EmitLnFmt(sfSource, 'return pax_string_new(%s);', [LExpr]);
        Exit;
      end;

      // wstring return with wide string literal -> wrap with pax_wstring_new
      if (FCurrentReturnType.Kind = tkWString) and (LValueNode^.Kind = nkWideStrLiteral) then
      begin
        EmitLnFmt(sfSource, 'return pax_wstring_new(%s);', [LExpr]);
        Exit;
      end;

      // char return with string literal -> dereference to get first char
      if (FCurrentReturnType.Kind = tkChar) and (LValueNode^.Kind = nkStrLiteral) then
      begin
        EmitLnFmt(sfSource, 'return %s[0];', [LExpr]);
        Exit;
      end;

      // wchar return with wide string literal -> dereference to get first wchar
      if (FCurrentReturnType.Kind = tkWChar) and (LValueNode^.Kind = nkWideStrLiteral) then
      begin
        EmitLnFmt(sfSource, 'return %s[0];', [LExpr]);
        Exit;
      end;
    end;

    EmitLnFmt(sfSource, 'return %s;', [LExpr]);
  end
  else
    EmitLn(sfSource, 'return;');
end;

procedure TPaxCodeGen.GenerateAssignment(const ANode: PASTNode);
var
  LTarget, LValue: string;
  LTargetType: TPaxType;
  LValueNode: PASTNode;
begin
  if GetASTChildCount(ANode) < 2 then
    Exit;

  LValueNode := GetASTChild(ANode, 1);
  LTarget := GenerateExpression(GetASTChild(ANode, 0));
  LValue := GenerateExpression(LValueNode);

  // Get target type for special handling
  LTargetType := GetExpressionType(GetASTChild(ANode, 0));

  // Special handling: assigning string literal to string variable
  if (LTargetType <> nil) then
  begin
    // string := 'literal' -> wrap with pax_string_new
    if (LTargetType.Kind = tkString) and (LValueNode^.Kind = nkStrLiteral) then
    begin
      EmitLnFmt(sfSource, '%s = pax_string_new(%s);', [LTarget, LValue]);
      Exit;
    end;

    // wstring := L'literal' -> wrap with pax_wstring_new
    if (LTargetType.Kind = tkWString) and (LValueNode^.Kind = nkWideStrLiteral) then
    begin
      EmitLnFmt(sfSource, '%s = pax_wstring_new(%s);', [LTarget, LValue]);
      Exit;
    end;

    // char := 'x' -> dereference string literal to get first char
    if (LTargetType.Kind = tkChar) and (LValueNode^.Kind = nkStrLiteral) then
    begin
      EmitLnFmt(sfSource, '%s = %s[0];', [LTarget, LValue]);
      Exit;
    end;

    // wchar := L'x' -> dereference wide string literal to get first wchar
    if (LTargetType.Kind = tkWChar) and (LValueNode^.Kind = nkWideStrLiteral) then
    begin
      EmitLnFmt(sfSource, '%s = %s[0];', [LTarget, LValue]);
      Exit;
    end;
  end;

  // Default assignment
  EmitLnFmt(sfSource, '%s = %s;', [LTarget, LValue]);
end;

procedure TPaxCodeGen.GenerateCallStmt(const ANode: PASTNode);
var
  LExpr: string;
begin
  if GetASTChildCount(ANode) > 0 then
  begin
    LExpr := GenerateExpression(GetASTChild(ANode, 0));
    EmitLnFmt(sfSource, '%s;', [LExpr]);
  end;
end;

procedure TPaxCodeGen.GenerateNewStmt(const ANode: PASTNode);
var
  LExpr: string;
begin
  if GetASTChildCount(ANode) > 0 then
  begin
    LExpr := GenerateExpression(GetASTChild(ANode, 0));
    EmitLnFmt(sfSource, '%s = pax_new(sizeof(*%s));', [LExpr, LExpr]);
  end;
end;

procedure TPaxCodeGen.GenerateDisposeStmt(const ANode: PASTNode);
var
  LExpr: string;
begin
  if GetASTChildCount(ANode) > 0 then
  begin
    LExpr := GenerateExpression(GetASTChild(ANode, 0));
    EmitLnFmt(sfSource, 'pax_dispose(%s);', [LExpr]);
  end;
end;

procedure TPaxCodeGen.GenerateSetLengthStmt(const ANode: PASTNode);
var
  LArray, LLen: string;
begin
  if GetASTChildCount(ANode) < 2 then
    Exit;

  LArray := GenerateExpression(GetASTChild(ANode, 0));
  LLen := GenerateExpression(GetASTChild(ANode, 1));

  EmitLnFmt(sfSource, 'pax_setlength(&%s, %s);', [LArray, LLen]);
end;

procedure TPaxCodeGen.GenerateGcCollectStmt(const ANode: PASTNode);
begin
  EmitLn(sfSource, 'pax_gc_collect();');
end;

procedure TPaxCodeGen.GenerateGcDumpStmt(const ANode: PASTNode);
begin
  EmitLn(sfSource, 'pax_gc_dump();');
end;

function TPaxCodeGen.GenerateExpression(const ANode: PASTNode): string;
begin
  Result := '';

  if ANode = nil then
    Exit;

  case ANode^.Kind of
    nkBinaryOp:       Result := GenerateBinaryOp(ANode);
    nkUnaryOp:        Result := GenerateUnaryOp(ANode);
    nkIdentifier:     Result := GenerateIdentifier(ANode);
    nkFieldAccess:    Result := GenerateFieldAccess(ANode);
    nkArrayAccess:    Result := GenerateArrayAccess(ANode);
    nkDeref:          Result := GenerateDeref(ANode);
    nkAddressOf:      Result := '(&' + GenerateExpression(GetASTChild(ANode, 0)) + ')';
    nkCall:           Result := GenerateCall(ANode);
    nkTypeCast:       Result := GenerateTypeCast(ANode);
    nkSizeOf:         Result := GenerateSizeOf(ANode);
    nkLen:            Result := GenerateLen(ANode);
    nkGcHeapSize:     Result := GenerateGcHeapSize(ANode);
    nkGcUsedSize:     Result := GenerateGcUsedSize(ANode);
    nkGcCollectCount: Result := GenerateGcCollectCount(ANode);
    nkParamCount:     Result := GenerateParamCount(ANode);
    nkParamStr:       Result := GenerateParamStr(ANode);
    nkSetLiteral:     Result := GenerateSetLiteral(ANode);

    nkIntLiteral, nkFloatLiteral, nkStrLiteral, nkWideStrLiteral,
    nkCharLiteral, nkWideCharLiteral, nkBoolLiteral, nkNilLiteral:
      Result := GenerateLiteral(ANode);
  end;
end;

function TPaxCodeGen.GenerateBinaryOp(const ANode: PASTNode): string;
var
  LLeft, LRight, LOp: string;
  LLeftType, LRightType: TPaxType;
  LIsLeftPCharPtr, LIsRightPCharPtr: Boolean;
  LIsLeftPWCharPtr, LIsRightPWCharPtr: Boolean;
begin
  if GetASTChildCount(ANode) < 2 then
    Exit('');

  LLeft := GenerateExpression(GetASTChild(ANode, 0));
  LRight := GenerateExpression(GetASTChild(ANode, 1));

  // Check for string concatenation
  if ANode^.Op = opAdd then
  begin
    LLeftType := GetExpressionType(GetASTChild(ANode, 0));
    LRightType := GetExpressionType(GetASTChild(ANode, 1));

    if (LLeftType <> nil) and (LRightType <> nil) then
    begin
      // Helper: check if type is pointer to char
      LIsLeftPCharPtr := (LLeftType.Kind = tkPointer) and
                         (LLeftType.ElementType <> nil) and
                         (LLeftType.ElementType.Kind = tkChar);
      LIsRightPCharPtr := (LRightType.Kind = tkPointer) and
                          (LRightType.ElementType <> nil) and
                          (LRightType.ElementType.Kind = tkChar);
      LIsLeftPWCharPtr := (LLeftType.Kind = tkPointer) and
                          (LLeftType.ElementType <> nil) and
                          (LLeftType.ElementType.Kind = tkWChar);
      LIsRightPWCharPtr := (LRightType.Kind = tkPointer) and
                           (LRightType.ElementType <> nil) and
                           (LRightType.ElementType.Kind = tkWChar);

      // string + string
      if (LLeftType.Kind = tkString) and (LRightType.Kind = tkString) then
      begin
        Result := Format('pax_string_concat(%s, %s)', [LLeft, LRight]);
        Exit;
      end;

      // string + char
      if (LLeftType.Kind = tkString) and (LRightType.Kind = tkChar) then
      begin
        Result := Format('pax_string_concat_char(%s, %s)', [LLeft, LRight]);
        Exit;
      end;

      // char + string
      if (LLeftType.Kind = tkChar) and (LRightType.Kind = tkString) then
      begin
        Result := Format('pax_string_prepend_char(%s, %s)', [LLeft, LRight]);
        Exit;
      end;

      // string + pointer to char (C string literal)
      if (LLeftType.Kind = tkString) and LIsRightPCharPtr then
      begin
        Result := Format('pax_string_concat_cstr(%s, %s)', [LLeft, LRight]);
        Exit;
      end;

      // pointer to char + string
      if LIsLeftPCharPtr and (LRightType.Kind = tkString) then
      begin
        Result := Format('pax_string_prepend_cstr(%s, %s)', [LLeft, LRight]);
        Exit;
      end;

      // wstring + wstring
      if (LLeftType.Kind = tkWString) and (LRightType.Kind = tkWString) then
      begin
        Result := Format('pax_wstring_concat(%s, %s)', [LLeft, LRight]);
        Exit;
      end;

      // wstring + wchar
      if (LLeftType.Kind = tkWString) and (LRightType.Kind = tkWChar) then
      begin
        Result := Format('pax_wstring_concat_char(%s, %s)', [LLeft, LRight]);
        Exit;
      end;

      // wchar + wstring
      if (LLeftType.Kind = tkWChar) and (LRightType.Kind = tkWString) then
      begin
        Result := Format('pax_wstring_prepend_char(%s, %s)', [LLeft, LRight]);
        Exit;
      end;

      // wstring + pointer to wchar (C wide string literal)
      if (LLeftType.Kind = tkWString) and LIsRightPWCharPtr then
      begin
        Result := Format('pax_wstring_concat_cstr(%s, %s)', [LLeft, LRight]);
        Exit;
      end;

      // pointer to wchar + wstring
      if LIsLeftPWCharPtr and (LRightType.Kind = tkWString) then
      begin
        Result := Format('pax_wstring_prepend_cstr(%s, %s)', [LLeft, LRight]);
        Exit;
      end;
    end;
  end;

  LOp := OperatorToC(ANode^.Op);
  Result := Format('(%s %s %s)', [LLeft, LOp, LRight]);
end;

function TPaxCodeGen.GenerateUnaryOp(const ANode: PASTNode): string;
var
  LOperand, LOp: string;
begin
  if GetASTChildCount(ANode) = 0 then
    Exit('');

  LOperand := GenerateExpression(GetASTChild(ANode, 0));

  case ANode^.Op of
    opNot: LOp := '!';
    opNeg: LOp := '-';
    opPos: LOp := '+';
  else
    LOp := '';
  end;

  Result := Format('(%s%s)', [LOp, LOperand]);
end;

function TPaxCodeGen.GenerateIdentifier(const ANode: PASTNode): string;
begin
  Result := ANode^.NodeName;
end;

function TPaxCodeGen.GenerateFieldAccess(const ANode: PASTNode): string;
var
  LBase: string;
  LBaseNode: PASTNode;
  LSym: TSymbol;
begin
  if GetASTChildCount(ANode) = 0 then
    Exit('');

  LBaseNode := GetASTChild(ANode, 0);
  
  // Check if base is a module identifier - if so, just use the field name directly
  if (LBaseNode^.Kind = nkIdentifier) and (FSymbols <> nil) then
  begin
    LSym := FSymbols.Lookup(LBaseNode^.NodeName);
    if (LSym <> nil) and (LSym.Kind = skModule) then
    begin
      // Module-qualified access: just use the function/symbol name
      Result := ANode^.NodeName;
      Exit;
    end;
  end;

  LBase := GenerateExpression(LBaseNode);
  Result := Format('%s.%s', [LBase, ANode^.NodeName]);
end;

function TPaxCodeGen.GenerateArrayAccess(const ANode: PASTNode): string;
var
  LBase, LIndex: string;
  LBaseType: TPaxType;
  LElementType: string;
begin
  if GetASTChildCount(ANode) < 2 then
    Exit('');

  LBase := GenerateExpression(GetASTChild(ANode, 0));
  LIndex := GenerateExpression(GetASTChild(ANode, 1));

  // Check if base is a dynamic array
  LBaseType := GetExpressionType(GetASTChild(ANode, 0));
  if (LBaseType <> nil) and (LBaseType.Kind = tkArray) and LBaseType.IsDynamic then
  begin
    // Dynamic array access: ((ElementType*)arr->data)[index]
    if LBaseType.ElementType <> nil then
      LElementType := TypeToC(LBaseType.ElementType)
    else
      LElementType := 'void';
    Result := Format('((%s*)%s->data)[%s]', [LElementType, LBase, LIndex]);
  end
  else
    Result := Format('%s[%s]', [LBase, LIndex]);
end;

function TPaxCodeGen.GenerateDeref(const ANode: PASTNode): string;
var
  LBase: string;
begin
  if GetASTChildCount(ANode) = 0 then
    Exit('');

  LBase := GenerateExpression(GetASTChild(ANode, 0));
  Result := Format('(*%s)', [LBase]);
end;

function TPaxCodeGen.GenerateCall(const ANode: PASTNode): string;
var
  LCallee: string;
  LArgs: string;
  LI: Integer;
  LCalleeType: TPaxType;
  LArgNode: PASTNode;
  LArgExpr: string;
  LParamType: TPaxType;
  LCalleeNode: PASTNode;
  LSym: TSymbol;
  LTargetType: TPaxType;
  LSourceType: TPaxType;
  LLine: Integer;
  LIsTestAssert: Boolean;
  LImplName: string;
begin
  if GetASTChildCount(ANode) = 0 then
    Exit('');

  LCalleeNode := GetASTChild(ANode, 0);

  // Check if this is actually a type cast (type alias used as function-style cast)
  // e.g., pchar(s) where pchar = pointer to char
  if (LCalleeNode^.Kind = nkIdentifier) and (FSymbols <> nil) then
  begin
    LSym := FSymbols.Lookup(LCalleeNode^.NodeName);
    if (LSym <> nil) and (LSym.Kind = skType) then
    begin
      // This is a type cast, not a call
      if GetASTChildCount(ANode) <> 2 then
        Exit('/* invalid cast */');

      LTargetType := LSym.SymbolType;
      LArgExpr := GenerateExpression(GetASTChild(ANode, 1));
      LSourceType := GetExpressionType(GetASTChild(ANode, 1));

      // Special handling: string -> pointer to char
      if (LSourceType <> nil) and (LTargetType <> nil) then
      begin
        // string -> pointer to char: extract data member
        if (LSourceType.Kind = tkString) and (LTargetType.Kind = tkPointer) and
           (LTargetType.ElementType <> nil) and (LTargetType.ElementType.Kind = tkChar) then
        begin
          Result := Format('%s->data', [LArgExpr]);
          Exit;
        end;

        // wstring -> pointer to wchar: extract data member
        if (LSourceType.Kind = tkWString) and (LTargetType.Kind = tkPointer) and
           (LTargetType.ElementType <> nil) and (LTargetType.ElementType.Kind = tkWChar) then
        begin
          Result := Format('%s->data', [LArgExpr]);
          Exit;
        end;
      end;

      // Default cast
      Result := Format('((%s)%s)', [TypeToC(LTargetType), LArgExpr]);
      Exit;
    end;
  end;

  LCallee := GenerateExpression(LCalleeNode);

  // Check if this is a TestAssert* builtin call - needs special handling
  LIsTestAssert := False;
  LImplName := '';
  if LCalleeNode^.Kind = nkIdentifier then
  begin
    if LCallee = 'TestAssert' then
    begin
      LIsTestAssert := True;
      LImplName := 'pax_test_assert_impl';
    end
    else if LCallee = 'TestAssertTrue' then
    begin
      LIsTestAssert := True;
      LImplName := 'pax_test_assert_true_impl';
    end
    else if LCallee = 'TestAssertFalse' then
    begin
      LIsTestAssert := True;
      LImplName := 'pax_test_assert_false_impl';
    end
    else if LCallee = 'TestAssertEqualInt' then
    begin
      LIsTestAssert := True;
      LImplName := 'pax_test_assert_equal_int_impl';
    end
    else if LCallee = 'TestAssertEqualUInt' then
    begin
      LIsTestAssert := True;
      LImplName := 'pax_test_assert_equal_uint_impl';
    end
    else if LCallee = 'TestAssertEqualFloat' then
    begin
      LIsTestAssert := True;
      LImplName := 'pax_test_assert_equal_float_impl';
    end
    else if LCallee = 'TestAssertEqualStr' then
    begin
      LIsTestAssert := True;
      LImplName := 'pax_test_assert_equal_str_impl';
    end
    else if LCallee = 'TestAssertEqualBool' then
    begin
      LIsTestAssert := True;
      LImplName := 'pax_test_assert_equal_bool_impl';
    end
    else if LCallee = 'TestAssertEqualPtr' then
    begin
      LIsTestAssert := True;
      LImplName := 'pax_test_assert_equal_ptr_impl';
    end
    else if LCallee = 'TestAssertNil' then
    begin
      LIsTestAssert := True;
      LImplName := 'pax_test_assert_nil_impl';
    end
    else if LCallee = 'TestAssertNotNil' then
    begin
      LIsTestAssert := True;
      LImplName := 'pax_test_assert_not_nil_impl';
    end
    else if LCallee = 'TestFail' then
    begin
      LIsTestAssert := True;
      LImplName := 'pax_test_fail_impl';
    end;
  end;

  // Get callee type to check parameter types
  LCalleeType := GetExpressionType(LCalleeNode);

  LArgs := '';
  for LI := 1 to GetASTChildCount(ANode) - 1 do
  begin
    if LI > 1 then
      LArgs := LArgs + ', ';

    LArgNode := GetASTChild(ANode, LI);
    LArgExpr := GenerateExpression(LArgNode);

    // Check for implicit string literal -> char conversion
    LParamType := nil;
    if (LCalleeType <> nil) and (LCalleeType.Kind = tkRoutine) and
       (LI - 1 < LCalleeType.Params.Count) then
    begin
      LParamType := LCalleeType.Params[LI - 1].ParamType;
    end;

    // If parameter expects char and we have a string literal, dereference
    if (LParamType <> nil) and (LParamType.Kind = tkChar) and
       (LArgNode^.Kind = nkStrLiteral) then
    begin
      LArgExpr := LArgExpr + '[0]';
    end
    else if (LParamType <> nil) and (LParamType.Kind = tkWChar) and
            (LArgNode^.Kind = nkWideStrLiteral) then
    begin
      LArgExpr := LArgExpr + '[0]';
    end
    // If parameter expects string and we have a string literal, wrap with pax_string_new
    else if (LParamType <> nil) and (LParamType.Kind = tkString) and
            (LArgNode^.Kind = nkStrLiteral) then
    begin
      LArgExpr := Format('pax_string_new(%s)', [LArgExpr]);
    end
    else if (LParamType <> nil) and (LParamType.Kind = tkWString) and
            (LArgNode^.Kind = nkWideStrLiteral) then
    begin
      LArgExpr := Format('pax_wstring_new(%s)', [LArgExpr]);
    end;

    LArgs := LArgs + LArgExpr;
  end;

  // For TestAssert* calls, append file and line parameters
  if LIsTestAssert then
  begin
    LLine := ANode^.Token.Range.StartLine;
    if LArgs <> '' then
      LArgs := LArgs + ', ';
    LArgs := LArgs + Format('"%s", %d', [FSourceFile, LLine]);
    Result := Format('%s(%s)', [LImplName, LArgs]);
  end
  else
    Result := Format('%s(%s)', [LCallee, LArgs]);
end;

function TPaxCodeGen.GenerateTypeCast(const ANode: PASTNode): string;
var
  LType, LExpr: string;
  LTypeNode: PASTNode;
  LTargetType, LSourceType: TPaxType;
  LChildCount: Integer;
begin
  LChildCount := GetASTChildCount(ANode);

  if LChildCount < 2 then
  begin
    // Malformed type cast - try to recover
    if LChildCount = 1 then
    begin
      // Only expression, no type - just return the expression
      LExpr := GenerateExpression(GetASTChild(ANode, 0));
      Exit(LExpr);
    end;
    Exit('/* invalid cast */');
  end;

  LTypeNode := GetASTChild(ANode, 0);

  // Resolve target type from AST node (handles compound types like pointer to char)
  LTargetType := ResolveTypeNode(LTypeNode);

  // Generate C type string
  if LTargetType <> nil then
    LType := TypeToC(LTargetType)
  else
    LType := TypeNodeToC(LTypeNode);

  // Fallback if type is still empty
  if LType = '' then
    LType := 'void*';

  LExpr := GenerateExpression(GetASTChild(ANode, 1));

  // Get source expression type for special handling
  LSourceType := GetExpressionType(GetASTChild(ANode, 1));

  // Special handling: string -> pointer to char
  if (LSourceType <> nil) and (LTargetType <> nil) then
  begin
    // string -> pointer to char: extract data member
    if (LSourceType.Kind = tkString) and (LTargetType.Kind = tkPointer) and
       (LTargetType.ElementType <> nil) and (LTargetType.ElementType.Kind = tkChar) then
    begin
      Result := Format('%s->data', [LExpr]);
      Exit;
    end;

    // wstring -> pointer to wchar: extract data member
    if (LSourceType.Kind = tkWString) and (LTargetType.Kind = tkPointer) and
       (LTargetType.ElementType <> nil) and (LTargetType.ElementType.Kind = tkWChar) then
    begin
      Result := Format('%s->data', [LExpr]);
      Exit;
    end;
  end;

  // Default cast
  Result := Format('((%s)%s)', [LType, LExpr]);
end;

function TPaxCodeGen.GenerateSizeOf(const ANode: PASTNode): string;
var
  LTypeNode: PASTNode;
  LType: string;
  LTargetType: TPaxType;
begin
  if GetASTChildCount(ANode) = 0 then
    Exit('0');

  LTypeNode := GetASTChild(ANode, 0);
  LTargetType := nil;

  if (FTypes <> nil) and (LTypeNode^.Kind = nkTypeRef) then
    LTargetType := FTypes.GetType(LTypeNode^.NodeName);

  if LTargetType <> nil then
    LType := TypeToC(LTargetType)
  else
    LType := LTypeNode^.NodeName;

  Result := Format('sizeof(%s)', [LType]);
end;

function TPaxCodeGen.GenerateLen(const ANode: PASTNode): string;
var
  LExpr: string;
begin
  if GetASTChildCount(ANode) = 0 then
    Exit('0');

  LExpr := GenerateExpression(GetASTChild(ANode, 0));
  Result := Format('pax_len(%s)', [LExpr]);
end;

function TPaxCodeGen.GenerateGcHeapSize(const ANode: PASTNode): string;
begin
  Result := 'pax_gc_heapsize()';
end;

function TPaxCodeGen.GenerateGcUsedSize(const ANode: PASTNode): string;
begin
  Result := 'pax_gc_usedsize()';
end;

function TPaxCodeGen.GenerateGcCollectCount(const ANode: PASTNode): string;
begin
  Result := 'pax_gc_collectcount()';
end;

function TPaxCodeGen.GenerateParamCount(const ANode: PASTNode): string;
begin
  Result := 'pax_paramcount()';
end;

function TPaxCodeGen.GenerateParamStr(const ANode: PASTNode): string;
var
  LIndex: string;
begin
  if GetASTChildCount(ANode) > 0 then
    LIndex := GenerateExpression(GetASTChild(ANode, 0))
  else
    LIndex := '0';

  Result := Format('pax_paramstr(%s)', [LIndex]);
end;

function TPaxCodeGen.GenerateSetLiteral(const ANode: PASTNode): string;
var
  LI: Integer;
  LChild: PASTNode;
  LExpr: string;
  LLow: string;
  LHigh: string;
begin
  Result := '0ULL';

  for LI := 0 to GetASTChildCount(ANode) - 1 do
  begin
    LChild := GetASTChild(ANode, LI);

    if LChild^.Kind = nkSetRange then
    begin
      // Range - generate bitmask
      LLow := GenerateExpression(GetASTChild(LChild, 0));
      LHigh := GenerateExpression(GetASTChild(LChild, 1));
      Result := Format('(%s | pax_set_range(%s, %s))', [Result, LLow, LHigh]);
    end
    else
    begin
      LExpr := GenerateExpression(LChild);
      Result := Format('(%s | (1ULL << %s))', [Result, LExpr]);
    end;
  end;
end;

function TPaxCodeGen.GenerateLiteral(const ANode: PASTNode): string;
begin
  case ANode^.Kind of
    nkIntLiteral:
      Result := IntToStr(ANode^.IntVal);

    nkFloatLiteral:
      Result := FloatToStr(ANode^.FloatVal);

    nkStrLiteral:
      Result := '"' + EscapeString(ANode^.StrVal) + '"';

    nkWideStrLiteral:
      Result := 'L"' + EscapeString(ANode^.StrVal) + '"';

    nkCharLiteral:
      begin
        if Length(ANode^.StrVal) > 0 then
          Result := '''' + EscapeString(ANode^.StrVal) + ''''
        else
          Result := '''\0''';
      end;

    nkWideCharLiteral:
      begin
        if Length(ANode^.StrVal) > 0 then
          Result := 'L''' + EscapeString(ANode^.StrVal) + ''''
        else
          Result := 'L''\0''';
      end;

    nkBoolLiteral:
      if ANode^.BoolVal then
        Result := 'true'
      else
        Result := 'false';

    nkNilLiteral:
      Result := 'NULL';
  else
    Result := '0';
  end;
end;

function TPaxCodeGen.EscapeString(const AStr: string): string;
var
  LI: Integer;
  LChar: Char;
  LCodePoint: UInt32;
begin
  Result := '';

  LI := 1;
  while LI <= Length(AStr) do
  begin
    LChar := AStr[LI];

    case LChar of
      #0:  Result := Result + '\0';
      #9:  Result := Result + '\t';
      #10: Result := Result + '\n';
      #13: Result := Result + '\r';
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
      '''': Result := Result + '\''';
    else
      if Ord(LChar) < 128 then
        // ASCII printable
        Result := Result + LChar
      else if (Ord(LChar) >= $D800) and (Ord(LChar) <= $DBFF) then
      begin
        // UTF-16 high surrogate - combine with next char to get code point
        if (LI < Length(AStr)) then
        begin
          LCodePoint := $10000 + ((Ord(LChar) - $D800) shl 10) + (Ord(AStr[LI + 1]) - $DC00);
          // Convert to UTF-8 (4 bytes for code points >= $10000)
          Result := Result + Format('\x%02X\x%02X\x%02X\x%02X', [
            $F0 or (LCodePoint shr 18),
            $80 or ((LCodePoint shr 12) and $3F),
            $80 or ((LCodePoint shr 6) and $3F),
            $80 or (LCodePoint and $3F)
          ]);
          Inc(LI); // Skip the low surrogate
        end;
      end
      else if Ord(LChar) < 256 then
        // Single byte (already UTF-8 byte from raw file read)
        Result := Result + Format('\x%02X', [Ord(LChar)])
      else
      begin
        // BMP character (U+0080 to U+FFFF, excluding surrogates)
        // Convert to UTF-8 (2 or 3 bytes)
        if Ord(LChar) < $800 then
        begin
          Result := Result + Format('\x%02X\x%02X', [
            $C0 or (Ord(LChar) shr 6),
            $80 or (Ord(LChar) and $3F)
          ]);
        end
        else
        begin
          Result := Result + Format('\x%02X\x%02X\x%02X', [
            $E0 or (Ord(LChar) shr 12),
            $80 or ((Ord(LChar) shr 6) and $3F),
            $80 or (Ord(LChar) and $3F)
          ]);
        end;
      end;
    end;

    Inc(LI);
  end;
end;

function TPaxCodeGen.OperatorToC(const AOp: TOperator): string;
begin
  case AOp of
    opAdd:    Result := '+';
    opSub:    Result := '-';
    opMul:    Result := '*';
    opDiv:    Result := '/';
    opDivInt: Result := '/';
    opMod:    Result := '%';
    opEq:     Result := '==';
    opNe:     Result := '!=';
    opLt:     Result := '<';
    opGt:     Result := '>';
    opLe:     Result := '<=';
    opGe:     Result := '>=';
    opAnd:    Result := '&&';
    opOr:     Result := '||';
    opIn:     Result := '&'; // Bitwise for set membership
  else
    Result := '?';
  end;
end;

function TPaxCodeGen.GetExpressionType(const ANode: PASTNode): TPaxType;
var
  LSym: TSymbol;
  LBaseType: TPaxType;
  LField: TPaxField;
  LLeftType, LRightType: TPaxType;
begin
  Result := nil;

  if ANode = nil then
    Exit;

  case ANode^.Kind of
    nkIdentifier:
      begin
        if FSymbols <> nil then
        begin
          LSym := FSymbols.Lookup(ANode^.NodeName);
          if LSym <> nil then
            Result := LSym.SymbolType;
        end;
      end;

    nkBinaryOp:
      begin
        // Infer result type of binary operation
        if GetASTChildCount(ANode) >= 2 then
        begin
          LLeftType := GetExpressionType(GetASTChild(ANode, 0));
          LRightType := GetExpressionType(GetASTChild(ANode, 1));

          if (LLeftType <> nil) and (LRightType <> nil) then
          begin
            case ANode^.Op of
              opAdd:
                begin
                  // String concatenation returns string
                  if (LLeftType.Kind = tkString) then
                    Result := FTypes.StringType
                  else if (LRightType.Kind = tkString) then
                    Result := FTypes.StringType
                  else if (LLeftType.Kind = tkWString) then
                    Result := FTypes.WStringType
                  else if (LRightType.Kind = tkWString) then
                    Result := FTypes.WStringType
                  // Pointer to char + anything involving string = string
                  else if (LLeftType.Kind = tkPointer) and (LLeftType.ElementType <> nil) and
                          (LLeftType.ElementType.Kind = tkChar) then
                    Result := FTypes.StringType
                  else if (LRightType.Kind = tkPointer) and (LRightType.ElementType <> nil) and
                          (LRightType.ElementType.Kind = tkChar) then
                    Result := FTypes.StringType
                  else if (LLeftType.Kind = tkPointer) and (LLeftType.ElementType <> nil) and
                          (LLeftType.ElementType.Kind = tkWChar) then
                    Result := FTypes.WStringType
                  else if (LRightType.Kind = tkPointer) and (LRightType.ElementType <> nil) and
                          (LRightType.ElementType.Kind = tkWChar) then
                    Result := FTypes.WStringType
                  else if LLeftType.IsNumeric() and LRightType.IsNumeric() then
                  begin
                    // Return larger numeric type
                    if LLeftType.Size >= LRightType.Size then
                      Result := LLeftType
                    else
                      Result := LRightType;
                  end;
                end;

              opSub, opMul, opDiv, opDivInt, opMod:
                begin
                  if LLeftType.IsNumeric() and LRightType.IsNumeric() then
                  begin
                    if LLeftType.Size >= LRightType.Size then
                      Result := LLeftType
                    else
                      Result := LRightType;
                  end;
                end;

              opEq, opNe, opLt, opGt, opLe, opGe, opAnd, opOr, opIn:
                Result := FTypes.BooleanType;
            end;
          end;
        end;
      end;

    nkIntLiteral:
      if FTypes <> nil then Result := FTypes.Int64Type;

    nkFloatLiteral:
      if FTypes <> nil then Result := FTypes.Float64Type;

    nkStrLiteral:
      if FTypes <> nil then Result := FTypes.PCharType;

    nkWideStrLiteral:
      if FTypes <> nil then Result := FTypes.PWCharType;

    nkCharLiteral:
      if FTypes <> nil then Result := FTypes.CharType;

    nkWideCharLiteral:
      if FTypes <> nil then Result := FTypes.WCharType;

    nkBoolLiteral:
      if FTypes <> nil then Result := FTypes.BooleanType;

    nkNilLiteral:
      if FTypes <> nil then Result := FTypes.PointerType;

    nkFieldAccess:
      begin
        // Get base type and look up field
        if GetASTChildCount(ANode) > 0 then
        begin
          LBaseType := GetExpressionType(GetASTChild(ANode, 0));
          if (LBaseType <> nil) and (LBaseType.IsRecord() or LBaseType.IsUnion()) then
          begin
            LField := LBaseType.GetField(ANode^.NodeName);
            if LField <> nil then
              Result := LField.FieldType;
          end;
        end;
      end;

    nkArrayAccess:
      begin
        if GetASTChildCount(ANode) > 0 then
        begin
          LBaseType := GetExpressionType(GetASTChild(ANode, 0));
          if LBaseType <> nil then
          begin
            if LBaseType.Kind in [tkArray, tkPointer] then
              Result := LBaseType.ElementType
            else if LBaseType.Kind = tkString then
              Result := FTypes.CharType
            else if LBaseType.Kind = tkWString then
              Result := FTypes.WCharType;
          end;
        end;
      end;

    nkDeref:
      begin
        if GetASTChildCount(ANode) > 0 then
        begin
          LBaseType := GetExpressionType(GetASTChild(ANode, 0));
          if (LBaseType <> nil) and (LBaseType.Kind = tkPointer) then
            Result := LBaseType.ElementType;
        end;
      end;

    nkCall:
      begin
        if GetASTChildCount(ANode) > 0 then
        begin
          LBaseType := GetExpressionType(GetASTChild(ANode, 0));
          if (LBaseType <> nil) and (LBaseType.Kind = tkRoutine) then
            Result := LBaseType.ReturnType;
        end;
      end;

    nkTypeCast:
      begin
        // For type cast, return the target type
        if GetASTChildCount(ANode) > 0 then
          Result := ResolveTypeNode(GetASTChild(ANode, 0));
      end;
  end;
end;

function TPaxCodeGen.ResolveTypeNode(const ANode: PASTNode): TPaxType;
var
  LElementType: TPaxType;
  LChildCount: Integer;
  LLowBound: Int64;
  LHighBound: Int64;
begin
  Result := nil;

  if ANode = nil then
    Exit;

  case ANode^.Kind of
    nkTypeRef:
      begin
        if FTypes <> nil then
          Result := FTypes.GetType(ANode^.NodeName);
      end;

    nkPointerType:
      begin
        if (FTypes <> nil) and (GetASTChildCount(ANode) > 0) then
        begin
          LElementType := ResolveTypeNode(GetASTChild(ANode, 0));
          if LElementType <> nil then
            Result := FTypes.CreatePointerType(LElementType)
          else
            Result := FTypes.PointerType;
        end
        else if FTypes <> nil then
          Result := FTypes.PointerType;
      end;

    nkArrayType:
      begin
        if (FTypes <> nil) and (GetASTChildCount(ANode) > 0) then
        begin
          LChildCount := GetASTChildCount(ANode);
          // Last child is element type
          LElementType := ResolveTypeNode(GetASTChild(ANode, LChildCount - 1));
          if LElementType <> nil then
          begin
            // Check for static array bounds (3 children: low, high, elementtype)
            if (LChildCount = 3) and
               (GetASTChild(ANode, 0)^.Kind = nkIntLiteral) and
               (GetASTChild(ANode, 1)^.Kind = nkIntLiteral) then
            begin
              LLowBound := GetASTChild(ANode, 0)^.IntVal;
              LHighBound := GetASTChild(ANode, 1)^.IntVal;
              Result := FTypes.CreateArrayType(LElementType, LLowBound, LHighBound);
            end
            else
              Result := FTypes.CreateDynamicArrayType(LElementType);
          end;
        end;
      end;
  end;
end;

function TPaxCodeGen.TypeNodeToC(const ANode: PASTNode): string;
var
  LType: TPaxType;
begin
  if ANode = nil then
    Exit('void');

  case ANode^.Kind of
    nkTypeRef:
      begin
        if FTypes <> nil then
        begin
          LType := FTypes.GetType(ANode^.NodeName);
          if LType <> nil then
            Exit(TypeToC(LType));
        end;
        Result := ANode^.NodeName;
      end;

    nkPointerType:
      begin
        if GetASTChildCount(ANode) > 0 then
          Result := TypeNodeToC(GetASTChild(ANode, 0)) + '*'
        else
          Result := 'void*';
      end;

    nkArrayType:
      begin
        if GetASTChildCount(ANode) > 0 then
          Result := TypeNodeToC(GetASTChild(ANode, GetASTChildCount(ANode) - 1)) + '*'
        else
          Result := 'void*';
      end;
  else
    Result := 'void';
  end;
end;

end.
