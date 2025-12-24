{===============================================================================
  Pax™ Programming Language.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://paxlang.org

  See LICENSE for license information
===============================================================================}

unit Pax.Checker;

{$I Pax.Defines.inc}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Pax.Utils,
  Pax.Errors,
  Pax.Resources,
  Pax.Lexer,
  Pax.AST,
  Pax.Types,
  Pax.Symbols;

type

  { TPaxChecker }
  TPaxChecker = class(TBaseObject)
  private
    FErrors: TErrors;
    FTypes: TPaxTypeRegistry;
    FSymbols: TSymbolTable;
    FCurrentRoutine: TSymbol;
    FModuleKind: string;

    // Stats
    FSymbolsResolved: Integer;
    FTypeChecks: Integer;
    FWarningCount: Integer;

    // Error helpers
    procedure Error(const ANode: PASTNode; const ACode: string; const AMessage: string); overload;
    procedure Error(const ANode: PASTNode; const ACode: string; const AMessage: string; const AArgs: array of const); overload;
    procedure Warning(const ANode: PASTNode; const ACode: string; const AMessage: string); overload;
    procedure Warning(const ANode: PASTNode; const ACode: string; const AMessage: string; const AArgs: array of const); overload;

    // Type resolution
    function ResolveType(const ANode: PASTNode): TPaxType;
    function ResolveTypeRef(const ANode: PASTNode): TPaxType;
    function ResolveRecordType(const ANode: PASTNode): TPaxType;
    function ResolveUnionType(const ANode: PASTNode): TPaxType;
    function ResolveArrayType(const ANode: PASTNode): TPaxType;
    function ResolvePointerType(const ANode: PASTNode): TPaxType;
    function ResolveSetType(const ANode: PASTNode): TPaxType;
    function ResolveRoutineType(const ANode: PASTNode): TPaxType;

    // Declaration processing
    procedure CheckModule(const ANode: PASTNode);
    procedure CheckDirective(const ANode: PASTNode);
    procedure CheckImport(const ANode: PASTNode);
    procedure CheckConstDecl(const ANode: PASTNode);
    procedure CheckTypeDecl(const ANode: PASTNode);
    procedure CheckVarDecl(const ANode: PASTNode);
    procedure CheckRoutineDecl(const ANode: PASTNode);
    procedure CheckParamDecl(const ANode: PASTNode; const ARoutineSym: TSymbol);
    procedure CheckTestBlock(const ANode: PASTNode);

    // Statement checking
    procedure CheckBlock(const ANode: PASTNode);
    procedure CheckStatement(const ANode: PASTNode);
    procedure CheckIfStmt(const ANode: PASTNode);
    procedure CheckWhileStmt(const ANode: PASTNode);
    procedure CheckForStmt(const ANode: PASTNode);
    procedure CheckRepeatStmt(const ANode: PASTNode);
    procedure CheckCaseStmt(const ANode: PASTNode);
    procedure CheckReturnStmt(const ANode: PASTNode);
    procedure CheckAssignment(const ANode: PASTNode);
    procedure CheckCallStmt(const ANode: PASTNode);
    procedure CheckNewStmt(const ANode: PASTNode);
    procedure CheckDisposeStmt(const ANode: PASTNode);
    procedure CheckSetLengthStmt(const ANode: PASTNode);

    // Expression checking
    function CheckExpression(const ANode: PASTNode): TPaxType;
    function CheckBinaryOp(const ANode: PASTNode): TPaxType;
    function CheckUnaryOp(const ANode: PASTNode): TPaxType;
    function CheckIdentifier(const ANode: PASTNode): TPaxType;
    function CheckFieldAccess(const ANode: PASTNode): TPaxType;
    function CheckArrayAccess(const ANode: PASTNode): TPaxType;
    function CheckDeref(const ANode: PASTNode): TPaxType;
    function CheckAddressOf(const ANode: PASTNode): TPaxType;
    function CheckCall(const ANode: PASTNode): TPaxType;
    function CheckTypeCast(const ANode: PASTNode): TPaxType;
    function CheckSizeOf(const ANode: PASTNode): TPaxType;
    function CheckLen(const ANode: PASTNode): TPaxType;
    function CheckSetLiteral(const ANode: PASTNode): TPaxType;
    function CheckParamStr(const ANode: PASTNode): TPaxType;

    // Utility
    function GetTypeName(const AType: TPaxType): string;
    function IsLValue(const ANode: PASTNode): Boolean;

    // Built-in registration
    procedure RegisterTestBuiltins();

  public
    constructor Create(); override;
    destructor Destroy(); override;

    procedure SetErrors(const AErrors: TErrors);
    procedure Check(const ARoot: PASTNode);

    property Types: TPaxTypeRegistry read FTypes;
    property Symbols: TSymbolTable read FSymbols;

    // Stats properties
    property SymbolsResolved: Integer read FSymbolsResolved;
    property TypeChecks: Integer read FTypeChecks;
    property WarningCount: Integer read FWarningCount;
  end;

implementation

{ TPaxChecker }

constructor TPaxChecker.Create();
begin
  inherited;

  FErrors := nil;
  FTypes := TPaxTypeRegistry.Create();
  FSymbols := TSymbolTable.Create();
  FCurrentRoutine := nil;
end;

destructor TPaxChecker.Destroy();
begin
  FSymbols.Free();
  FTypes.Free();

  inherited;
end;

procedure TPaxChecker.SetErrors(const AErrors: TErrors);
begin
  FErrors := AErrors;
end;

procedure TPaxChecker.Error(const ANode: PASTNode; const ACode: string; const AMessage: string);
begin
  if FErrors = nil then
    Exit;

  if ANode <> nil then
    FErrors.Add(ANode^.Token.Range, esError, ACode, AMessage)
  else
    FErrors.Add(esError, ACode, AMessage);
end;

procedure TPaxChecker.Error(const ANode: PASTNode; const ACode: string; const AMessage: string; const AArgs: array of const);
begin
  Error(ANode, ACode, Format(AMessage, AArgs));
end;

procedure TPaxChecker.Warning(const ANode: PASTNode; const ACode: string; const AMessage: string);
begin
  if FErrors = nil then
    Exit;

  Inc(FWarningCount);

  if ANode <> nil then
    FErrors.Add(ANode^.Token.Range, esWarning, ACode, AMessage)
  else
    FErrors.Add(esWarning, ACode, AMessage);
end;

procedure TPaxChecker.Warning(const ANode: PASTNode; const ACode: string; const AMessage: string; const AArgs: array of const);
begin
  Warning(ANode, ACode, Format(AMessage, AArgs));
end;

procedure TPaxChecker.Check(const ARoot: PASTNode);
begin
  // Reset stats
  FSymbolsResolved := 0;
  FTypeChecks := 0;
  FWarningCount := 0;

  if ARoot = nil then
    Exit;

  // Register test assertion built-ins
  RegisterTestBuiltins();

  if ARoot^.Kind = nkModule then
    CheckModule(ARoot);
end;

function TPaxChecker.ResolveType(const ANode: PASTNode): TPaxType;
begin
  Result := nil;

  if ANode = nil then
    Exit;

  case ANode^.Kind of
    nkTypeRef:      Result := ResolveTypeRef(ANode);
    nkRecordType:   Result := ResolveRecordType(ANode);
    nkUnionType:    Result := ResolveUnionType(ANode);
    nkArrayType:    Result := ResolveArrayType(ANode);
    nkPointerType:  Result := ResolvePointerType(ANode);
    nkSetType:      Result := ResolveSetType(ANode);
    nkRoutineType:  Result := ResolveRoutineType(ANode);
  else
    Error(ANode, ERR_SEMANTIC_UNKNOWN_TYPE, RSTypeUnknown, ['<invalid>']);
    Result := FTypes.ErrorType;
  end;
end;

function TPaxChecker.ResolveTypeRef(const ANode: PASTNode): TPaxType;
var
  LName: string;
  LSym: TSymbol;
begin
  LName := ANode^.NodeName;

  // Check built-in types first
  Result := FTypes.GetType(LName);
  if Result <> nil then
  begin
    Inc(FSymbolsResolved);
    Exit;
  end;

  // Look up in symbol table
  LSym := FSymbols.Lookup(LName);
  if LSym <> nil then
  begin
    if LSym.Kind = skType then
      Result := LSym.SymbolType
    else
    begin
      Error(ANode, ERR_SEMANTIC_UNKNOWN_TYPE, RSTypeUnknown, [LName]);
      Result := FTypes.ErrorType;
    end;
  end
  else
  begin
    Error(ANode, ERR_SEMANTIC_UNKNOWN_TYPE, RSTypeUnknown, [LName]);
    Result := FTypes.ErrorType;
  end;
end;

function TPaxChecker.ResolveRecordType(const ANode: PASTNode): TPaxType;
var
  LI: Integer;
  LJ: Integer;
  LChild: PASTNode;
  LFieldType: TPaxType;
  LParentType: TPaxType;
  LAnonType: TPaxType;
  LAnonField: TPaxField;
begin
  Result := FTypes.CreateRecordType('');

  // Propagate packed flag from AST
  Result.IsPacked := ANode^.IsPacked;

  // Propagate alignment from AST (TCC only supports up to 16-byte alignment)
  if ANode^.Alignment > 0 then
  begin
    if ANode^.Alignment > 16 then
      Error(ANode, ERR_SEMANTIC_INVALID_ALIGNMENT, RSParserAlignmentExceedsMax, [ANode^.Alignment, 16])
    else
      Result.Alignment := ANode^.Alignment;
  end;

  for LI := 0 to GetASTChildCount(ANode) - 1 do
  begin
    LChild := GetASTChild(ANode, LI);

    if LChild^.Kind = nkTypeRef then
    begin
      // Parent type
      if LChild^.NodeName = 'parent' then
      begin
        LParentType := ResolveTypeRef(GetASTChild(LChild, 0));
        Result.ParentType := LParentType;
      end;
    end
    else if LChild^.Kind = nkFieldDecl then
    begin
      // Regular field
      if GetASTChildCount(LChild) > 0 then
        LFieldType := ResolveType(GetASTChild(LChild, 0))
      else
        LFieldType := FTypes.ErrorType;

      // Add field and propagate bit width if present
      LAnonField := Result.AddField(LChild^.NodeName, LFieldType);

      // Validate and propagate bit field width
      if LChild^.BitWidth > 0 then
      begin
        // Bit fields require integer or boolean types
        if not LFieldType.IsInteger() and not LFieldType.IsBoolean() and not LFieldType.IsError() then
          Error(LChild, ERR_SEMANTIC_BITFIELD_TYPE, RSSemanticBitFieldType, [GetTypeName(LFieldType)])
        else if LChild^.BitWidth < 1 then
          Error(LChild, ERR_SEMANTIC_BITFIELD_POSITIVE, RSSemanticBitFieldPositive)
        else
        begin
          // Check bit width doesn't exceed type size
          if LFieldType.IsBoolean() then
          begin
            // Boolean bit fields should be 1 bit max
            if LChild^.BitWidth > 8 then
              Error(LChild, ERR_SEMANTIC_BITFIELD_WIDTH, RSSemanticBitFieldWidth, [LChild^.BitWidth, 8, GetTypeName(LFieldType)])
            else
              LAnonField.BitWidth := LChild^.BitWidth;
          end
          else if LFieldType.IsInteger() then
          begin
            // Integer bit fields limited to type size in bits
            if LChild^.BitWidth > LFieldType.Size * 8 then
              Error(LChild, ERR_SEMANTIC_BITFIELD_WIDTH, RSSemanticBitFieldWidth, [LChild^.BitWidth, LFieldType.Size * 8, GetTypeName(LFieldType)])
            else
              LAnonField.BitWidth := LChild^.BitWidth;
          end;
        end;
      end;
    end
    else if LChild^.Kind = nkUnionType then
    begin
      // Anonymous union - flatten its fields into this record
      LAnonType := ResolveUnionType(LChild);
      if LAnonType <> nil then
      begin
        for LJ := 0 to LAnonType.Fields.Count - 1 do
        begin
          LAnonField := LAnonType.Fields[LJ];
          Result.AddField(LAnonField.FieldName, LAnonField.FieldType);
        end;
      end;
    end;
  end;

  // Validate flexible array member rules
  for LI := 0 to Result.Fields.Count - 1 do
  begin
    LFieldType := Result.Fields[LI].FieldType;
    if (LFieldType <> nil) and LFieldType.IsArray() and LFieldType.IsFlexibleArray then
    begin
      // Flexible array must be the last field
      if LI < Result.Fields.Count - 1 then
        Error(ANode, ERR_SEMANTIC_FLEXIBLE_ARRAY_LAST, RSSemanticFlexibleArrayLast);

      // Record must have at least one other field
      if Result.Fields.Count = 1 then
        Error(ANode, ERR_SEMANTIC_FLEXIBLE_ARRAY_ONLY_FIELD, RSSemanticFlexibleArrayOnlyField);
    end;
  end;
end;

function TPaxChecker.ResolveUnionType(const ANode: PASTNode): TPaxType;
var
  LI: Integer;
  LJ: Integer;
  LChild: PASTNode;
  LFieldType: TPaxType;
  LMaxSize: Integer;
  LAnonType: TPaxType;
  LAnonField: TPaxField;
begin
  Result := FTypes.CreateUnionType('');
  LMaxSize := 0;

  for LI := 0 to GetASTChildCount(ANode) - 1 do
  begin
    LChild := GetASTChild(ANode, LI);

    if LChild^.Kind = nkFieldDecl then
    begin
      // Regular field - all fields at offset 0 in union
      if GetASTChildCount(LChild) > 0 then
        LFieldType := ResolveType(GetASTChild(LChild, 0))
      else
        LFieldType := FTypes.ErrorType;

      Result.AddField(LChild^.NodeName, LFieldType);

      // Track max size for union (size = largest field)
      if (LFieldType <> nil) and (LFieldType.Size > LMaxSize) then
        LMaxSize := LFieldType.Size;
    end
    else if LChild^.Kind = nkRecordType then
    begin
      // Anonymous record - flatten its fields into this union
      LAnonType := ResolveRecordType(LChild);
      if LAnonType <> nil then
      begin
        for LJ := 0 to LAnonType.Fields.Count - 1 do
        begin
          LAnonField := LAnonType.Fields[LJ];
          Result.AddField(LAnonField.FieldName, LAnonField.FieldType);
        end;

        // Track max size (anonymous struct size)
        if LAnonType.Size > LMaxSize then
          LMaxSize := LAnonType.Size;
      end;
    end;
  end;

  // Union size is the size of its largest member
  Result.Size := LMaxSize;

  // Validate: flexible arrays not allowed in unions
  for LI := 0 to Result.Fields.Count - 1 do
  begin
    LFieldType := Result.Fields[LI].FieldType;
    if (LFieldType <> nil) and LFieldType.IsArray() and LFieldType.IsFlexibleArray then
      Error(ANode, ERR_SEMANTIC_FLEXIBLE_ARRAY_UNION, RSSemanticFlexibleArrayUnion);
  end;
end;

function TPaxChecker.ResolveArrayType(const ANode: PASTNode): TPaxType;
var
  LChildCount: Integer;
  LElementType: TPaxType;
  LLow, LHigh: Int64;
begin
  LChildCount := GetASTChildCount(ANode);

  if LChildCount = 0 then
  begin
    Result := FTypes.ErrorType;
    Exit;
  end;

  // Check if static or dynamic array
  if LChildCount >= 3 then
  begin
    // Static array: [0]=low, [1]=high, [2]=element type
    LLow := GetASTChild(ANode, 0)^.IntVal;
    LHigh := GetASTChild(ANode, 1)^.IntVal;
    LElementType := ResolveType(GetASTChild(ANode, 2));
    Result := FTypes.CreateArrayType(LElementType, LLow, LHigh);
  end
  else
  begin
    // Dynamic or flexible array: [0]=element type
    LElementType := ResolveType(GetASTChild(ANode, 0));
    if ANode^.IsFlexibleArray then
      Result := FTypes.CreateFlexibleArrayType(LElementType)
    else
      Result := FTypes.CreateDynamicArrayType(LElementType);
  end;
end;

function TPaxChecker.ResolvePointerType(const ANode: PASTNode): TPaxType;
var
  LElementType: TPaxType;
begin
  if GetASTChildCount(ANode) > 0 then
  begin
    LElementType := ResolveType(GetASTChild(ANode, 0));
    Result := FTypes.CreatePointerType(LElementType);
  end
  else
    Result := FTypes.PointerType; // Untyped pointer
end;

function TPaxChecker.ResolveSetType(const ANode: PASTNode): TPaxType;
var
  LChildCount: Integer;
  LElementType: TPaxType;
  LLow, LHigh: Int64;
begin
  LChildCount := GetASTChildCount(ANode);

  if LChildCount >= 2 then
  begin
    // Range-based set
    LLow := GetASTChild(ANode, 0)^.IntVal;
    LHigh := GetASTChild(ANode, 1)^.IntVal;
    Result := FTypes.CreateSetType(nil, LLow, LHigh);
  end
  else if LChildCount = 1 then
  begin
    // Type-based set
    LElementType := ResolveType(GetASTChild(ANode, 0));
    Result := FTypes.CreateSetType(LElementType, 0, 255);
  end
  else
    Result := FTypes.ErrorType;
end;

function TPaxChecker.ResolveRoutineType(const ANode: PASTNode): TPaxType;
var
  LI: Integer;
  LChild: PASTNode;
  LParamType: TPaxType;
  LReturnType: TPaxType;
  LMode: TPaxParamMode;
begin
  LReturnType := FTypes.VoidType;

  // Find return type (last child if it's a type ref)
  for LI := GetASTChildCount(ANode) - 1 downto 0 do
  begin
    LChild := GetASTChild(ANode, LI);
    if (LChild^.Kind = nkTypeRef) and (LChild^.NodeName <> 'params') then
    begin
      LReturnType := ResolveType(LChild);
      Break;
    end;
  end;

  Result := FTypes.CreateRoutineType(LReturnType);

  // Process parameters
  for LI := 0 to GetASTChildCount(ANode) - 1 do
  begin
    LChild := GetASTChild(ANode, LI);

    if (LChild^.Kind = nkBlock) and (LChild^.NodeName = 'params') then
    begin
      // Params block
      for var LJ := 0 to GetASTChildCount(LChild) - 1 do
      begin
        var LParam := GetASTChild(LChild, LJ);
        if LParam^.Kind = nkParamDecl then
        begin
          if GetASTChildCount(LParam) > 0 then
            LParamType := ResolveType(GetASTChild(LParam, 0))
          else
            LParamType := FTypes.ErrorType;

          if LParam^.StrVal = 'var' then
            LMode := pmVar
          else if LParam^.StrVal = 'const' then
            LMode := pmConst
          else
            LMode := pmValue;

          Result.AddParam(LParam^.NodeName, LParamType, LMode);
        end;
      end;
    end;
  end;
end;

procedure TPaxChecker.CheckModule(const ANode: PASTNode);
var
  LI: Integer;
  LChild: PASTNode;
  LModuleSym: TSymbol;
begin
  // Register module symbol
  LModuleSym := FSymbols.Define(ANode^.NodeName, skModule);
  LModuleSym.DeclNode := ANode;

  // Extract module kind (strip quotes, lowercase)
  FModuleKind := ANode^.StrVal;
  if (Length(FModuleKind) >= 2) and (FModuleKind[1] = '''') then
    FModuleKind := Copy(FModuleKind, 2, Length(FModuleKind) - 2);
  FModuleKind := LowerCase(FModuleKind);
  if FModuleKind = '' then
    FModuleKind := 'exe';  // Default is exe

  // Process children
  for LI := 0 to GetASTChildCount(ANode) - 1 do
  begin
    LChild := GetASTChild(ANode, LI);

    case LChild^.Kind of
      nkDirective:    CheckDirective(LChild);
      nkImport:       CheckImport(LChild);
      nkConstDecl:    CheckConstDecl(LChild);
      nkTypeDecl:     CheckTypeDecl(LChild);
      nkVarDecl:      CheckVarDecl(LChild);
      nkRoutineDecl:  CheckRoutineDecl(LChild);
      nkBlock:
        begin
          if LChild^.NodeName = 'imports' then
          begin
            for var LJ := 0 to GetASTChildCount(LChild) - 1 do
              CheckImport(GetASTChild(LChild, LJ));
          end
          else if LChild^.NodeName = 'consts' then
          begin
            for var LJ := 0 to GetASTChildCount(LChild) - 1 do
              CheckConstDecl(GetASTChild(LChild, LJ));
          end
          else if LChild^.NodeName = 'types' then
          begin
            for var LJ := 0 to GetASTChildCount(LChild) - 1 do
              CheckTypeDecl(GetASTChild(LChild, LJ));
          end
          else if LChild^.NodeName = 'vars' then
          begin
            for var LJ := 0 to GetASTChildCount(LChild) - 1 do
              CheckVarDecl(GetASTChild(LChild, LJ));
          end
          else if LChild^.NodeName = 'block' then
          begin
            // DLL and lib modules cannot have initialization blocks
            if (ANode^.StrVal = '''dll''') or (ANode^.StrVal = '''lib''') then
              Error(LChild, ERR_SEMANTIC_DLL_INIT_BLOCK, RSSemanticDllInitBlock)
            else
              CheckBlock(LChild);
          end;
        end;
      nkTestBlock:    CheckTestBlock(LChild);
    end;
  end;
end;

procedure TPaxChecker.CheckDirective(const ANode: PASTNode);
var
  LName: string;
begin
  LName := LowerCase(ANode^.NodeName);

  // Validate #unittestmode is only valid for EXE modules
  if LName = '#unittestmode' then
  begin
    if FModuleKind <> 'exe' then
      Error(ANode, 'E081', '#unittestmode directive is only valid for EXE modules');
  end;
end;

procedure TPaxChecker.CheckImport(const ANode: PASTNode);
var
  LModuleSym: TSymbol;
begin
  // For now, just register the import as a module symbol
  // Full module resolution would happen during compilation
  if not FSymbols.ContainsLocal(ANode^.NodeName) then
  begin
    LModuleSym := FSymbols.Define(ANode^.NodeName, skModule);
    LModuleSym.DeclNode := ANode;
  end;
end;

procedure TPaxChecker.CheckConstDecl(const ANode: PASTNode);
var
  LSym: TSymbol;
  LType: TPaxType;
  LValueType: TPaxType;
  LValueNode: PASTNode;
  LTypeNode: PASTNode;
  LChildCount: Integer;
begin
  // Check for duplicate
  if FSymbols.ContainsLocal(ANode^.NodeName) then
  begin
    Error(ANode, ERR_SEMANTIC_DUPLICATE_IDENT, RSNameDuplicate, [ANode^.NodeName]);
    Exit;
  end;

  LSym := FSymbols.Define(ANode^.NodeName, skConst);
  LSym.DeclNode := ANode;
  LSym.IsPublic := ANode^.IsPublic;

  LChildCount := GetASTChildCount(ANode);

  // Determine type and value
  if LChildCount >= 2 then
  begin
    // Has explicit type and value
    LTypeNode := GetASTChild(ANode, 0);
    LValueNode := GetASTChild(ANode, 1);
    LType := ResolveType(LTypeNode);
    LValueType := CheckExpression(LValueNode);

    if not TypesCompatible(LType, LValueType) then
      Error(ANode, ERR_SEMANTIC_TYPE_MISMATCH, RSTypeMismatch, [GetTypeName(LValueType), GetTypeName(LType)]);
  end
  else if LChildCount = 1 then
  begin
    // Value only, infer type
    LValueNode := GetASTChild(ANode, 0);
    LType := CheckExpression(LValueNode);
  end
  else
  begin
    LType := FTypes.ErrorType;
  end;

  LSym.SymbolType := LType;
end;

procedure TPaxChecker.CheckTypeDecl(const ANode: PASTNode);
var
  LSym: TSymbol;
  LType: TPaxType;
begin
  // Check for duplicate
  if FSymbols.ContainsLocal(ANode^.NodeName) then
  begin
    Error(ANode, ERR_SEMANTIC_DUPLICATE_IDENT, RSNameDuplicate, [ANode^.NodeName]);
    Exit;
  end;

  LSym := FSymbols.Define(ANode^.NodeName, skType);
  LSym.DeclNode := ANode;
  LSym.IsPublic := ANode^.IsPublic;

  if GetASTChildCount(ANode) > 0 then
  begin
    LType := ResolveType(GetASTChild(ANode, 0));
    LType.TypeName := ANode^.NodeName;
    FTypes.RegisterType(LType);
  end
  else
    LType := FTypes.ErrorType;

  LSym.SymbolType := LType;
end;

procedure TPaxChecker.CheckVarDecl(const ANode: PASTNode);
var
  LSym: TSymbol;
  LType: TPaxType;
  LInitType: TPaxType;
  LChildCount: Integer;
begin
  // Check for duplicate
  if FSymbols.ContainsLocal(ANode^.NodeName) then
  begin
    Error(ANode, ERR_SEMANTIC_DUPLICATE_IDENT, RSNameDuplicate, [ANode^.NodeName]);
    Exit;
  end;

  LSym := FSymbols.Define(ANode^.NodeName, skVar);
  LSym.DeclNode := ANode;
  LSym.IsPublic := ANode^.IsPublic;

  LChildCount := GetASTChildCount(ANode);

  if LChildCount > 0 then
  begin
    LType := ResolveType(GetASTChild(ANode, 0));

    // Check initializer if present
    if LChildCount > 1 then
    begin
      LInitType := CheckExpression(GetASTChild(ANode, 1));
      if not TypesCompatible(LType, LInitType) then
        Error(ANode, ERR_SEMANTIC_TYPE_MISMATCH, RSTypeMismatch, [GetTypeName(LInitType), GetTypeName(LType)]);
    end;
  end
  else
    LType := FTypes.ErrorType;

  LSym.SymbolType := LType;
end;

procedure TPaxChecker.CheckRoutineDecl(const ANode: PASTNode);
var
  LSym: TSymbol;
  LRoutineType: TPaxType;
  LReturnType: TPaxType;
  LI: Integer;
  LChild: PASTNode;
begin
  // Check for duplicate
  if FSymbols.ContainsLocal(ANode^.NodeName) then
  begin
    Error(ANode, ERR_SEMANTIC_DUPLICATE_IDENT, RSNameDuplicate, [ANode^.NodeName]);
    Exit;
  end;

  LSym := FSymbols.Define(ANode^.NodeName, skRoutine);
  LSym.DeclNode := ANode;
  LSym.IsPublic := ANode^.IsPublic;
  LSym.IsExternal := ANode^.IsExternal;

  // Determine return type
  LReturnType := FTypes.VoidType;
  for LI := 0 to GetASTChildCount(ANode) - 1 do
  begin
    LChild := GetASTChild(ANode, LI);
    if (LChild^.Kind = nkTypeRef) or
       (LChild^.Kind = nkPointerType) or
       (LChild^.Kind = nkArrayType) then
    begin
      LReturnType := ResolveType(LChild);
      Break;
    end;
  end;

  LRoutineType := FTypes.CreateRoutineType(LReturnType);
  LRoutineType.IsVariadic := ANode^.IsVariadic;
  LSym.SymbolType := LRoutineType;

  // Enter routine scope
  FSymbols.PushScope(ANode^.NodeName);
  FCurrentRoutine := LSym;

  // Process parameters
  for LI := 0 to GetASTChildCount(ANode) - 1 do
  begin
    LChild := GetASTChild(ANode, LI);

    if (LChild^.Kind = nkBlock) and (LChild^.NodeName = 'params') then
    begin
      for var LJ := 0 to GetASTChildCount(LChild) - 1 do
        CheckParamDecl(GetASTChild(LChild, LJ), LSym);
    end;
  end;

  // Process local variables and body
  for LI := 0 to GetASTChildCount(ANode) - 1 do
  begin
    LChild := GetASTChild(ANode, LI);

    if (LChild^.Kind = nkBlock) and (LChild^.NodeName = 'locals') then
    begin
      for var LJ := 0 to GetASTChildCount(LChild) - 1 do
        CheckVarDecl(GetASTChild(LChild, LJ));
    end
    else if (LChild^.Kind = nkBlock) and (LChild^.NodeName = 'block') then
    begin
      CheckBlock(LChild);
    end;
  end;

  FCurrentRoutine := nil;
  FSymbols.PopScope();
end;

procedure TPaxChecker.CheckParamDecl(const ANode: PASTNode; const ARoutineSym: TSymbol);
var
  LSym: TSymbol;
  LType: TPaxType;
  LMode: TPaxParamMode;
begin
  if ANode^.Kind <> nkParamDecl then
    Exit;

  // Check for duplicate
  if FSymbols.ContainsLocal(ANode^.NodeName) then
  begin
    Error(ANode, ERR_SEMANTIC_DUPLICATE_IDENT, RSNameDuplicate, [ANode^.NodeName]);
    Exit;
  end;

  LSym := FSymbols.Define(ANode^.NodeName, skParam);
  LSym.DeclNode := ANode;

  if GetASTChildCount(ANode) > 0 then
    LType := ResolveType(GetASTChild(ANode, 0))
  else
    LType := FTypes.ErrorType;

  LSym.SymbolType := LType;

  // Add to routine type
  if ANode^.StrVal = 'var' then
    LMode := pmVar
  else if ANode^.StrVal = 'const' then
    LMode := pmConst
  else
    LMode := pmValue;

  if (ARoutineSym <> nil) and (ARoutineSym.SymbolType <> nil) then
    ARoutineSym.SymbolType.AddParam(ANode^.NodeName, LType, LMode);
end;

procedure TPaxChecker.CheckTestBlock(const ANode: PASTNode);
var
  LI: Integer;
  LChild: PASTNode;
begin
  // Enter test scope
  FSymbols.PushScope('test:' + ANode^.NodeName);

  for LI := 0 to GetASTChildCount(ANode) - 1 do
  begin
    LChild := GetASTChild(ANode, LI);

    if (LChild^.Kind = nkBlock) and (LChild^.NodeName = 'locals') then
    begin
      for var LJ := 0 to GetASTChildCount(LChild) - 1 do
        CheckVarDecl(GetASTChild(LChild, LJ));
    end
    else if (LChild^.Kind = nkBlock) and (LChild^.NodeName = 'block') then
    begin
      CheckBlock(LChild);
    end;
  end;

  FSymbols.PopScope();
end;

procedure TPaxChecker.CheckBlock(const ANode: PASTNode);
var
  LI: Integer;
begin
  for LI := 0 to GetASTChildCount(ANode) - 1 do
    CheckStatement(GetASTChild(ANode, LI));
end;

procedure TPaxChecker.CheckStatement(const ANode: PASTNode);
begin
  if ANode = nil then
    Exit;

  case ANode^.Kind of
    nkIfStmt:         CheckIfStmt(ANode);
    nkWhileStmt:      CheckWhileStmt(ANode);
    nkForStmt:        CheckForStmt(ANode);
    nkRepeatStmt:     CheckRepeatStmt(ANode);
    nkCaseStmt:       CheckCaseStmt(ANode);
    nkReturnStmt:     CheckReturnStmt(ANode);
    nkAssignment:     CheckAssignment(ANode);
    nkCallStmt:       CheckCallStmt(ANode);
    nkNewStmt:        CheckNewStmt(ANode);
    nkDisposeStmt:    CheckDisposeStmt(ANode);
    nkSetLengthStmt:  CheckSetLengthStmt(ANode);
    nkEmptyStmt:      ; // Nothing to check
    nkBlock:          CheckBlock(ANode);
  end;
end;

procedure TPaxChecker.CheckIfStmt(const ANode: PASTNode);
var
  LCondType: TPaxType;
begin
  // Check condition
  if GetASTChildCount(ANode) > 0 then
  begin
    LCondType := CheckExpression(GetASTChild(ANode, 0));
    if not LCondType.IsBoolean() and not LCondType.IsError() then
      Error(ANode, ERR_SEMANTIC_CONDITION_NOT_BOOLEAN, RSSemanticConditionNotBoolean);
  end;

  // Check then block
  if GetASTChildCount(ANode) > 1 then
    CheckBlock(GetASTChild(ANode, 1));

  // Check else block
  if GetASTChildCount(ANode) > 2 then
    CheckBlock(GetASTChild(ANode, 2));
end;

procedure TPaxChecker.CheckWhileStmt(const ANode: PASTNode);
var
  LCondType: TPaxType;
begin
  // Check condition
  if GetASTChildCount(ANode) > 0 then
  begin
    LCondType := CheckExpression(GetASTChild(ANode, 0));
    if not LCondType.IsBoolean() and not LCondType.IsError() then
      Error(ANode, ERR_SEMANTIC_CONDITION_NOT_BOOLEAN, RSSemanticConditionNotBoolean);
  end;

  // Check body
  if GetASTChildCount(ANode) > 1 then
    CheckBlock(GetASTChild(ANode, 1));
end;

procedure TPaxChecker.CheckForStmt(const ANode: PASTNode);
var
  LVarNode: PASTNode;
  LVarType: TPaxType;
  LFromType, LToType: TPaxType;
begin
  if GetASTChildCount(ANode) < 4 then
    Exit;

  // Check loop variable
  LVarNode := GetASTChild(ANode, 0);
  LVarType := CheckExpression(LVarNode);
  if not LVarType.IsInteger() and not LVarType.IsError() then
    Error(LVarNode, ERR_SEMANTIC_FOR_VAR_NOT_INTEGER, RSSemanticForVarNotInteger);

  // Check from expression
  LFromType := CheckExpression(GetASTChild(ANode, 1));
  if not LFromType.IsInteger() and not LFromType.IsError() then
    Error(GetASTChild(ANode, 1), ERR_SEMANTIC_FOR_VAR_NOT_INTEGER, RSSemanticForBoundsNotInteger);

  // Check to expression
  LToType := CheckExpression(GetASTChild(ANode, 2));
  if not LToType.IsInteger() and not LToType.IsError() then
    Error(GetASTChild(ANode, 2), ERR_SEMANTIC_FOR_VAR_NOT_INTEGER, RSSemanticForBoundsNotInteger);

  // Check body
  CheckBlock(GetASTChild(ANode, 3));
end;

procedure TPaxChecker.CheckRepeatStmt(const ANode: PASTNode);
var
  LCondType: TPaxType;
begin
  // Check body
  if GetASTChildCount(ANode) > 0 then
    CheckBlock(GetASTChild(ANode, 0));

  // Check condition
  if GetASTChildCount(ANode) > 1 then
  begin
    LCondType := CheckExpression(GetASTChild(ANode, 1));
    if not LCondType.IsBoolean() and not LCondType.IsError() then
      Error(ANode, ERR_SEMANTIC_CONDITION_NOT_BOOLEAN, RSSemanticConditionNotBoolean);
  end;
end;

procedure TPaxChecker.CheckCaseStmt(const ANode: PASTNode);
var
  LSelectorType: TPaxType;
  LI: Integer;
  LChild: PASTNode;
begin
  if GetASTChildCount(ANode) = 0 then
    Exit;

  // Check selector
  LSelectorType := CheckExpression(GetASTChild(ANode, 0));
  if not LSelectorType.IsOrdinal() and not LSelectorType.IsError() then
    Error(ANode, ERR_SEMANTIC_CASE_NOT_ORDINAL, RSSemanticCaseSelectorNotOrdinal);

  // Check case arms
  for LI := 1 to GetASTChildCount(ANode) - 1 do
  begin
    LChild := GetASTChild(ANode, LI);

    if LChild^.Kind = nkCaseArm then
    begin
      // Check labels and body
      for var LJ := 0 to GetASTChildCount(LChild) - 1 do
      begin
        var LArmChild := GetASTChild(LChild, LJ);
        if LArmChild^.Kind = nkBlock then
          CheckBlock(LArmChild)
        else
          CheckExpression(LArmChild);
      end;
    end
    else if LChild^.Kind = nkBlock then
    begin
      // Else block
      CheckBlock(LChild);
    end;
  end;
end;

procedure TPaxChecker.CheckReturnStmt(const ANode: PASTNode);
var
  LExprType: TPaxType;
  LReturnType: TPaxType;
begin
  if FCurrentRoutine = nil then
    Exit;

  if FCurrentRoutine.SymbolType <> nil then
    LReturnType := FCurrentRoutine.SymbolType.ReturnType
  else
    LReturnType := FTypes.VoidType;

  if GetASTChildCount(ANode) > 0 then
  begin
    LExprType := CheckExpression(GetASTChild(ANode, 0));

    if LReturnType.IsVoid() then
      Error(ANode, ERR_SEMANTIC_RETURN_VALUE_IN_VOID, RSTypeReturnInVoid)
    else if not TypesCompatible(LReturnType, LExprType) then
      Error(ANode, ERR_SEMANTIC_RETURN_TYPE_MISMATCH, RSTypeIncompatibleReturn, [GetTypeName(LReturnType), GetTypeName(LExprType)]);
  end;
end;

procedure TPaxChecker.CheckAssignment(const ANode: PASTNode);
var
  LTargetType, LValueType: TPaxType;
  LTarget: PASTNode;
begin
  if GetASTChildCount(ANode) < 2 then
    Exit;

  LTarget := GetASTChild(ANode, 0);
  LTargetType := CheckExpression(LTarget);
  LValueType := CheckExpression(GetASTChild(ANode, 1));

  // Check if target is an lvalue
  if not IsLValue(LTarget) then
    Error(LTarget, ERR_SEMANTIC_TYPE_MISMATCH, RSSemanticAssignToReadOnly, [LTarget^.NodeName]);

  // Type compatibility
  if not TypesCompatible(LTargetType, LValueType) then
    Error(ANode, ERR_SEMANTIC_TYPE_MISMATCH, RSTypeMismatch, [GetTypeName(LValueType), GetTypeName(LTargetType)]);
end;

procedure TPaxChecker.CheckCallStmt(const ANode: PASTNode);
begin
  if GetASTChildCount(ANode) > 0 then
    CheckExpression(GetASTChild(ANode, 0));
end;

procedure TPaxChecker.CheckNewStmt(const ANode: PASTNode);
var
  LArgType: TPaxType;
begin
  if GetASTChildCount(ANode) > 0 then
  begin
    LArgType := CheckExpression(GetASTChild(ANode, 0));
    if not LArgType.IsPointer() and not LArgType.IsError() then
      Error(ANode, ERR_SEMANTIC_NEW_NON_POINTER, RSSemanticNewNonPointer);
  end;
end;

procedure TPaxChecker.CheckDisposeStmt(const ANode: PASTNode);
var
  LArgType: TPaxType;
begin
  if GetASTChildCount(ANode) > 0 then
  begin
    LArgType := CheckExpression(GetASTChild(ANode, 0));
    if not LArgType.IsPointer() and not LArgType.IsError() then
      Error(ANode, ERR_SEMANTIC_DISPOSE_NON_POINTER, RSSemanticDisposeNonPointer);
  end;
end;

procedure TPaxChecker.CheckSetLengthStmt(const ANode: PASTNode);
var
  LArrayType, LLenType: TPaxType;
begin
  if GetASTChildCount(ANode) < 2 then
    Exit;

  LArrayType := CheckExpression(GetASTChild(ANode, 0));
  LLenType := CheckExpression(GetASTChild(ANode, 1));

  if not LArrayType.IsArray() and not LArrayType.IsError() then
    Error(ANode, ERR_SEMANTIC_SETLENGTH_NON_ARRAY, RSSemanticSetLengthNonArray);

  if not LLenType.IsInteger() and not LLenType.IsError() then
    Error(ANode, ERR_SEMANTIC_SETLENGTH_NON_INTEGER, RSSemanticSetLengthNonInteger);
end;

function TPaxChecker.CheckExpression(const ANode: PASTNode): TPaxType;
begin
  Result := FTypes.ErrorType;

  if ANode = nil then
    Exit;

  Inc(FTypeChecks);

  case ANode^.Kind of
    nkBinaryOp:       Result := CheckBinaryOp(ANode);
    nkUnaryOp:        Result := CheckUnaryOp(ANode);
    nkIdentifier:     Result := CheckIdentifier(ANode);
    nkFieldAccess:    Result := CheckFieldAccess(ANode);
    nkArrayAccess:    Result := CheckArrayAccess(ANode);
    nkDeref:          Result := CheckDeref(ANode);
    nkAddressOf:      Result := CheckAddressOf(ANode);
    nkCall:           Result := CheckCall(ANode);
    nkTypeCast:       Result := CheckTypeCast(ANode);
    nkSizeOf:         Result := CheckSizeOf(ANode);
    nkLen:            Result := CheckLen(ANode);
    nkSetLiteral:     Result := CheckSetLiteral(ANode);

    nkGcHeapSize:     Result := FTypes.Int64Type;
    nkGcUsedSize:     Result := FTypes.Int64Type;
    nkGcCollectCount: Result := FTypes.Int64Type;
    nkParamCount:     Result := FTypes.Int32Type;
    nkParamStr:       Result := CheckParamStr(ANode);

    nkIntLiteral:     Result := FTypes.Int64Type;
    nkFloatLiteral:   Result := FTypes.Float64Type;
    nkStrLiteral:     Result := FTypes.PCharType;      // String literals are pointer to char
    nkWideStrLiteral: Result := FTypes.PWCharType;     // Wide string literals are pointer to wchar
    nkCharLiteral:    Result := FTypes.CharType;
    nkWideCharLiteral:Result := FTypes.WCharType;
    nkBoolLiteral:    Result := FTypes.BooleanType;
    nkNilLiteral:     Result := FTypes.PointerType;
  end;
end;

function TPaxChecker.CheckBinaryOp(const ANode: PASTNode): TPaxType;
var
  LLeftType, LRightType: TPaxType;
  LOp: TOperator;
begin
  Result := FTypes.ErrorType;

  if GetASTChildCount(ANode) < 2 then
    Exit;

  LLeftType := CheckExpression(GetASTChild(ANode, 0));
  LRightType := CheckExpression(GetASTChild(ANode, 1));
  LOp := ANode^.Op;

  case LOp of
    opAdd, opSub, opMul, opDiv:
      begin
        if LLeftType.IsNumeric() and LRightType.IsNumeric() then
          Result := GetCommonType(LLeftType, LRightType)
        else if LLeftType.IsString() and LRightType.IsString() and (LOp = opAdd) then
          Result := LLeftType
        else if LLeftType.IsString() and LRightType.IsChar() and (LOp = opAdd) then
          Result := LLeftType
        else if LLeftType.IsChar() and LRightType.IsString() and (LOp = opAdd) then
          Result := LRightType
        // string + pointer to char (C string literal)
        else if LLeftType.IsString() and LRightType.IsPointer() and
                (LRightType.ElementType <> nil) and LRightType.ElementType.IsChar() and (LOp = opAdd) then
          Result := LLeftType
        else if LLeftType.IsPointer() and (LLeftType.ElementType <> nil) and
                LLeftType.ElementType.IsChar() and LRightType.IsString() and (LOp = opAdd) then
          Result := LRightType
        else if LLeftType.IsWString() and LRightType.IsWString() and (LOp = opAdd) then
          Result := LLeftType
        else if LLeftType.IsWString() and LRightType.IsWChar() and (LOp = opAdd) then
          Result := LLeftType
        else if LLeftType.IsWChar() and LRightType.IsWString() and (LOp = opAdd) then
          Result := LRightType
        // wstring + pointer to wchar (C wide string literal)
        else if LLeftType.IsWString() and LRightType.IsPointer() and
                (LRightType.ElementType <> nil) and LRightType.ElementType.IsWChar() and (LOp = opAdd) then
          Result := LLeftType
        else if LLeftType.IsPointer() and (LLeftType.ElementType <> nil) and
                LLeftType.ElementType.IsWChar() and LRightType.IsWString() and (LOp = opAdd) then
          Result := LRightType
        else if not LLeftType.IsError() and not LRightType.IsError() then
          Error(ANode, ERR_SEMANTIC_INVALID_OPERATOR, RSTypeMismatchBinary, [OperatorToString(LOp), GetTypeName(LLeftType), GetTypeName(LRightType)]);
      end;

    opDivInt, opMod:
      begin
        if LLeftType.IsInteger() and LRightType.IsInteger() then
          Result := GetCommonType(LLeftType, LRightType)
        else if not LLeftType.IsError() and not LRightType.IsError() then
          Error(ANode, ERR_SEMANTIC_INVALID_OPERATOR, RSTypeMismatchBinary, [OperatorToString(LOp), GetTypeName(LLeftType), GetTypeName(LRightType)]);
      end;

    opEq, opNe, opLt, opGt, opLe, opGe:
      begin
        if TypesCompatible(LLeftType, LRightType) then
          Result := FTypes.BooleanType
        else if not LLeftType.IsError() and not LRightType.IsError() then
          Error(ANode, ERR_SEMANTIC_INVALID_OPERATOR, RSTypeMismatchBinary, [OperatorToString(LOp), GetTypeName(LLeftType), GetTypeName(LRightType)]);
      end;

    opAnd, opOr:
      begin
        if LLeftType.IsBoolean() and LRightType.IsBoolean() then
          Result := FTypes.BooleanType
        else if LLeftType.IsInteger() and LRightType.IsInteger() then
          Result := GetCommonType(LLeftType, LRightType)
        else if not LLeftType.IsError() and not LRightType.IsError() then
          Error(ANode, ERR_SEMANTIC_INVALID_OPERATOR, RSTypeMismatchBinary, [OperatorToString(LOp), GetTypeName(LLeftType), GetTypeName(LRightType)]);
      end;

    opIn:
      begin
        if LLeftType.IsOrdinal() and LRightType.IsSet() then
          Result := FTypes.BooleanType
        else
        begin
          if not LLeftType.IsOrdinal() and not LLeftType.IsError() then
            Error(ANode, ERR_SEMANTIC_IN_NOT_ORDINAL, RSSemanticInNotOrdinal);
          if not LRightType.IsSet() and not LRightType.IsError() then
            Error(ANode, ERR_SEMANTIC_IN_NOT_SET, RSSemanticInNotSet);
        end;
      end;
  end;
end;

function TPaxChecker.CheckUnaryOp(const ANode: PASTNode): TPaxType;
var
  LOperandType: TPaxType;
  LOp: TOperator;
begin
  Result := FTypes.ErrorType;

  if GetASTChildCount(ANode) = 0 then
    Exit;

  LOperandType := CheckExpression(GetASTChild(ANode, 0));
  LOp := ANode^.Op;

  case LOp of
    opNot:
      begin
        if LOperandType.IsBoolean() then
          Result := FTypes.BooleanType
        else if LOperandType.IsInteger() then
          Result := LOperandType
        else if not LOperandType.IsError() then
          Error(ANode, ERR_SEMANTIC_INVALID_UNARY_OPERATOR, RSTypeMismatchUnary, [OperatorToString(LOp), GetTypeName(LOperandType)]);
      end;

    opNeg, opPos:
      begin
        if LOperandType.IsNumeric() then
          Result := LOperandType
        else if not LOperandType.IsError() then
          Error(ANode, ERR_SEMANTIC_INVALID_UNARY_OPERATOR, RSTypeMismatchUnary, [OperatorToString(LOp), GetTypeName(LOperandType)]);
      end;
  end;
end;

function TPaxChecker.CheckIdentifier(const ANode: PASTNode): TPaxType;
var
  LSym: TSymbol;
begin
  LSym := FSymbols.Lookup(ANode^.NodeName);

  if LSym = nil then
  begin
    Error(ANode, ERR_SEMANTIC_UNDECLARED_IDENT, RSNameUndeclared, [ANode^.NodeName]);
    Result := FTypes.ErrorType;
  end
  else
  begin
    Inc(FSymbolsResolved);
    Result := LSym.SymbolType;
  end;
end;

function TPaxChecker.CheckFieldAccess(const ANode: PASTNode): TPaxType;
var
  LBaseNode: PASTNode;
  LBaseType: TPaxType;
  LField: TPaxField;
  LQualifiedName: string;
  LSym: TSymbol;
begin
  Result := FTypes.ErrorType;

  if GetASTChildCount(ANode) = 0 then
    Exit;

  LBaseNode := GetASTChild(ANode, 0);

  // Try qualified module lookup first (e.g., mathlib.Add)
  if LBaseNode^.Kind = nkIdentifier then
  begin
    LQualifiedName := LBaseNode^.NodeName + '.' + ANode^.NodeName;
    LSym := FSymbols.Lookup(LQualifiedName);
    if LSym <> nil then
    begin
      Result := LSym.SymbolType;
      Exit;
    end;
  end;

  LBaseType := CheckExpression(LBaseNode);

  if LBaseType.IsRecord() or LBaseType.IsUnion() then
  begin
    LField := LBaseType.GetField(ANode^.NodeName);
    if LField <> nil then
      Result := LField.FieldType
    else
      Error(ANode, ERR_SEMANTIC_UNKNOWN_FIELD, RSNameUnknownField, [ANode^.NodeName, GetTypeName(LBaseType)]);
  end
  else if not LBaseType.IsError() then
    Error(ANode, ERR_SEMANTIC_UNKNOWN_FIELD, RSTypeNotRecord, [GetTypeName(LBaseType)]);
end;

function TPaxChecker.CheckArrayAccess(const ANode: PASTNode): TPaxType;
var
  LBaseType, LIndexType: TPaxType;
begin
  Result := FTypes.ErrorType;

  if GetASTChildCount(ANode) < 2 then
    Exit;

  LBaseType := CheckExpression(GetASTChild(ANode, 0));
  LIndexType := CheckExpression(GetASTChild(ANode, 1));

  if not LIndexType.IsInteger() and not LIndexType.IsError() then
    Error(ANode, ERR_SEMANTIC_INDEX_NOT_INTEGER, RSSemanticIndexNotInteger);

  if LBaseType.IsArray() then
    Result := LBaseType.ElementType
  else if LBaseType.IsString() then
    Result := FTypes.CharType
  else if LBaseType.IsPointer() then
    Result := LBaseType.ElementType
  else if not LBaseType.IsError() then
    Error(ANode, ERR_SEMANTIC_TYPE_MISMATCH, RSTypeCannotIndex, [GetTypeName(LBaseType)]);
end;

function TPaxChecker.CheckDeref(const ANode: PASTNode): TPaxType;
var
  LBaseType: TPaxType;
begin
  Result := FTypes.ErrorType;

  if GetASTChildCount(ANode) = 0 then
    Exit;

  LBaseType := CheckExpression(GetASTChild(ANode, 0));

  if LBaseType.IsPointer() then
  begin
    if LBaseType.ElementType <> nil then
      Result := LBaseType.ElementType
    else
      Result := FTypes.ErrorType; // Untyped pointer dereference
  end
  else if not LBaseType.IsError() then
    Error(ANode, ERR_SEMANTIC_DEREF_NON_POINTER, RSTypeCannotDeref, [GetTypeName(LBaseType)]);
end;

function TPaxChecker.CheckAddressOf(const ANode: PASTNode): TPaxType;
var
  LOperand: PASTNode;
  LOperandType: TPaxType;
begin
  Result := FTypes.ErrorType;

  if GetASTChildCount(ANode) = 0 then
    Exit;

  LOperand := GetASTChild(ANode, 0);
  LOperandType := CheckExpression(LOperand);

  // Operand must be an lvalue
  if not IsLValue(LOperand) then
  begin
    Error(ANode, ERR_SEMANTIC_TYPE_MISMATCH, 'Cannot take address of non-lvalue');
    Exit;
  end;

  // Return pointer to the operand's type
  Result := FTypes.CreatePointerType(LOperandType);
end;

function TPaxChecker.CheckCall(const ANode: PASTNode): TPaxType;
var
  LCalleeNode: PASTNode;
  LCalleeType: TPaxType;
  LSym: TSymbol;
  LI: Integer;
  LArgType: TPaxType;
  LParam: TPaxParam;
  LArgCount, LParamCount: Integer;
  LTargetType, LSourceType: TPaxType;
begin
  Result := FTypes.ErrorType;

  if GetASTChildCount(ANode) = 0 then
    Exit;

  LCalleeNode := GetASTChild(ANode, 0);

  // Check if this is actually a type cast (type alias used as function-style cast)
  // e.g., pchar(s) where pchar = pointer to char
  if LCalleeNode^.Kind = nkIdentifier then
  begin
    LSym := FSymbols.Lookup(LCalleeNode^.NodeName);
    if (LSym <> nil) and (LSym.Kind = skType) then
    begin
      // This is a type cast, not a call
      if GetASTChildCount(ANode) <> 2 then
      begin
        Error(ANode, ERR_SEMANTIC_ARG_COUNT_MISMATCH, RSSemanticArgCountMismatch, [1, GetASTChildCount(ANode) - 1]);
        Exit;
      end;

      LTargetType := LSym.SymbolType;
      LSourceType := CheckExpression(GetASTChild(ANode, 1));

      // Validate cast (same logic as CheckTypeCast)
      if LTargetType.IsNumeric() and LSourceType.IsNumeric() then
        Result := LTargetType
      else if LTargetType.IsPointer() and LSourceType.IsPointer() then
        Result := LTargetType
      else if LTargetType.IsPointer() and LSourceType.IsInteger() then
        Result := LTargetType
      else if LTargetType.IsInteger() and LSourceType.IsPointer() then
        Result := LTargetType
      else if LTargetType.IsChar() and LSourceType.IsInteger() then
        Result := LTargetType
      else if LTargetType.IsInteger() and LSourceType.IsChar() then
        Result := LTargetType
      // string -> pointer to char: extract data pointer
      else if (LSourceType.Kind = tkString) and LTargetType.IsPointer() and
              (LTargetType.ElementType <> nil) and (LTargetType.ElementType.Kind = tkChar) then
        Result := LTargetType
      // wstring -> pointer to wchar: extract data pointer
      else if (LSourceType.Kind = tkWString) and LTargetType.IsPointer() and
              (LTargetType.ElementType <> nil) and (LTargetType.ElementType.Kind = tkWChar) then
        Result := LTargetType
      else if not LTargetType.IsError() and not LSourceType.IsError() then
        Error(ANode, ERR_SEMANTIC_INVALID_CAST, RSTypeCannotCast, [GetTypeName(LSourceType), GetTypeName(LTargetType)])
      else
        Result := LTargetType;

      Exit;
    end;
  end;

  LCalleeType := CheckExpression(LCalleeNode);

  if LCalleeType.IsRoutine() then
  begin
    Result := LCalleeType.ReturnType;
    if Result = nil then
      Result := FTypes.VoidType;

    // Check arguments
    LArgCount := GetASTChildCount(ANode) - 1;
    LParamCount := LCalleeType.Params.Count;

    // For variadic routines, we need at least as many args as fixed params
    if LCalleeType.IsVariadic then
    begin
      if LArgCount < LParamCount then
        Error(ANode, ERR_SEMANTIC_ARG_COUNT_MISMATCH, RSSemanticArgCountMismatch, [LParamCount, LArgCount]);
    end
    else if LArgCount <> LParamCount then
      Error(ANode, ERR_SEMANTIC_ARG_COUNT_MISMATCH, RSSemanticArgCountMismatch, [LParamCount, LArgCount]);

    // Check fixed parameter types (skip variadic args)
    for LI := 0 to LParamCount - 1 do
    begin
      if LI < LArgCount then
      begin
        LArgType := CheckExpression(GetASTChild(ANode, LI + 1));
        LParam := LCalleeType.Params[LI];

        if not TypesCompatible(LParam.ParamType, LArgType) then
          Error(GetASTChild(ANode, LI + 1), ERR_SEMANTIC_ARG_TYPE_MISMATCH, RSSemanticArgTypeMismatch, [LI + 1, GetTypeName(LParam.ParamType), GetTypeName(LArgType)]);
      end;
    end;

    // For variadic routines, still type-check the variadic arguments (but accept any type)
    if LCalleeType.IsVariadic then
    begin
      for LI := LParamCount to LArgCount - 1 do
        CheckExpression(GetASTChild(ANode, LI + 1));
    end;
  end
  else if not LCalleeType.IsError() then
    Error(ANode, ERR_SEMANTIC_CALL_NON_ROUTINE, RSTypeCannotCall, [GetTypeName(LCalleeType)]);
end;

function TPaxChecker.CheckTypeCast(const ANode: PASTNode): TPaxType;
var
  LTargetType, LSourceType: TPaxType;
begin
  Result := FTypes.ErrorType;

  if GetASTChildCount(ANode) < 2 then
    Exit;

  LTargetType := ResolveType(GetASTChild(ANode, 0));
  LSourceType := CheckExpression(GetASTChild(ANode, 1));

  // Basic cast validation
  if LTargetType.IsNumeric() and LSourceType.IsNumeric() then
    Result := LTargetType
  else if LTargetType.IsPointer() and LSourceType.IsPointer() then
    Result := LTargetType
  else if LTargetType.IsPointer() and LSourceType.IsInteger() then
    Result := LTargetType
  else if LTargetType.IsInteger() and LSourceType.IsPointer() then
    Result := LTargetType
  else if LTargetType.IsChar() and LSourceType.IsInteger() then
    Result := LTargetType
  else if LTargetType.IsInteger() and LSourceType.IsChar() then
    Result := LTargetType
  // string -> pointer to char: extract data pointer
  else if (LSourceType.Kind = tkString) and LTargetType.IsPointer() and
          (LTargetType.ElementType <> nil) and (LTargetType.ElementType.Kind = tkChar) then
    Result := LTargetType
  // wstring -> pointer to wchar: extract data pointer
  else if (LSourceType.Kind = tkWString) and LTargetType.IsPointer() and
          (LTargetType.ElementType <> nil) and (LTargetType.ElementType.Kind = tkWChar) then
    Result := LTargetType
  else if not LTargetType.IsError() and not LSourceType.IsError() then
    Error(ANode, ERR_SEMANTIC_INVALID_CAST, RSTypeCannotCast, [GetTypeName(LSourceType), GetTypeName(LTargetType)]);
end;

function TPaxChecker.CheckSizeOf(const ANode: PASTNode): TPaxType;
begin
  // sizeof always returns an integer
  if GetASTChildCount(ANode) > 0 then
    ResolveType(GetASTChild(ANode, 0));

  Result := FTypes.Int64Type;
end;

function TPaxChecker.CheckLen(const ANode: PASTNode): TPaxType;
var
  LArgType: TPaxType;
begin
  Result := FTypes.Int64Type;

  if GetASTChildCount(ANode) > 0 then
  begin
    LArgType := CheckExpression(GetASTChild(ANode, 0));

    if not LArgType.IsArray() and not LArgType.IsString() and not LArgType.IsError() then
      Error(ANode, ERR_SEMANTIC_TYPE_MISMATCH, RSTypeNotArray, [GetTypeName(LArgType)]);
  end;
end;

function TPaxChecker.CheckSetLiteral(const ANode: PASTNode): TPaxType;
var
  LI: Integer;
begin
  // Check all elements
  for LI := 0 to GetASTChildCount(ANode) - 1 do
    CheckExpression(GetASTChild(ANode, LI));

  // Return a generic set type
  Result := FTypes.CreateSetType(nil, 0, 255);
end;

function TPaxChecker.CheckParamStr(const ANode: PASTNode): TPaxType;
var
  LArgType: TPaxType;
begin
  Result := FTypes.StringType;

  if GetASTChildCount(ANode) > 0 then
  begin
    LArgType := CheckExpression(GetASTChild(ANode, 0));

    if not LArgType.IsInteger() and not LArgType.IsError() then
      Error(ANode, ERR_SEMANTIC_TYPE_MISMATCH, RSSemanticParamStrNotInteger);
  end;
end;

function TPaxChecker.GetTypeName(const AType: TPaxType): string;
begin
  if AType = nil then
    Result := '<nil>'
  else if AType.TypeName <> '' then
    Result := AType.TypeName
  else
    Result := TypeKindToString(AType.Kind);
end;

function TPaxChecker.IsLValue(const ANode: PASTNode): Boolean;
var
  LSym: TSymbol;
begin
  Result := False;

  if ANode = nil then
    Exit;

  case ANode^.Kind of
    nkIdentifier:
      begin
        LSym := FSymbols.Lookup(ANode^.NodeName);
        if LSym <> nil then
          Result := LSym.Kind in [skVar, skParam];
      end;

    nkFieldAccess, nkArrayAccess, nkDeref:
      Result := True;
  end;
end;

procedure TPaxChecker.RegisterTestBuiltins();
var
  LSym: TSymbol;
  LRoutineType: TPaxType;
begin
  // TestAssert(condition: boolean)
  LSym := FSymbols.Define('TestAssert', skRoutine);
  LRoutineType := FTypes.CreateRoutineType(FTypes.VoidType);
  LRoutineType.AddParam('condition', FTypes.BooleanType, pmConst);
  LSym.SymbolType := LRoutineType;

  // TestAssertTrue(condition: boolean)
  LSym := FSymbols.Define('TestAssertTrue', skRoutine);
  LRoutineType := FTypes.CreateRoutineType(FTypes.VoidType);
  LRoutineType.AddParam('condition', FTypes.BooleanType, pmConst);
  LSym.SymbolType := LRoutineType;

  // TestAssertFalse(condition: boolean)
  LSym := FSymbols.Define('TestAssertFalse', skRoutine);
  LRoutineType := FTypes.CreateRoutineType(FTypes.VoidType);
  LRoutineType.AddParam('condition', FTypes.BooleanType, pmConst);
  LSym.SymbolType := LRoutineType;

  // TestAssertEqualInt(expected, actual: int64)
  LSym := FSymbols.Define('TestAssertEqualInt', skRoutine);
  LRoutineType := FTypes.CreateRoutineType(FTypes.VoidType);
  LRoutineType.AddParam('expected', FTypes.Int64Type, pmConst);
  LRoutineType.AddParam('actual', FTypes.Int64Type, pmConst);
  LSym.SymbolType := LRoutineType;

  // TestAssertEqualUInt(expected, actual: uint64)
  LSym := FSymbols.Define('TestAssertEqualUInt', skRoutine);
  LRoutineType := FTypes.CreateRoutineType(FTypes.VoidType);
  LRoutineType.AddParam('expected', FTypes.UInt64Type, pmConst);
  LRoutineType.AddParam('actual', FTypes.UInt64Type, pmConst);
  LSym.SymbolType := LRoutineType;

  // TestAssertEqualFloat(expected, actual: float64)
  LSym := FSymbols.Define('TestAssertEqualFloat', skRoutine);
  LRoutineType := FTypes.CreateRoutineType(FTypes.VoidType);
  LRoutineType.AddParam('expected', FTypes.Float64Type, pmConst);
  LRoutineType.AddParam('actual', FTypes.Float64Type, pmConst);
  LSym.SymbolType := LRoutineType;

  // TestAssertEqualStr(expected, actual: pchar)
  LSym := FSymbols.Define('TestAssertEqualStr', skRoutine);
  LRoutineType := FTypes.CreateRoutineType(FTypes.VoidType);
  LRoutineType.AddParam('expected', FTypes.PCharType, pmConst);
  LRoutineType.AddParam('actual', FTypes.PCharType, pmConst);
  LSym.SymbolType := LRoutineType;

  // TestAssertEqualBool(expected, actual: boolean)
  LSym := FSymbols.Define('TestAssertEqualBool', skRoutine);
  LRoutineType := FTypes.CreateRoutineType(FTypes.VoidType);
  LRoutineType.AddParam('expected', FTypes.BooleanType, pmConst);
  LRoutineType.AddParam('actual', FTypes.BooleanType, pmConst);
  LSym.SymbolType := LRoutineType;

  // TestAssertEqualPtr(expected, actual: pointer)
  LSym := FSymbols.Define('TestAssertEqualPtr', skRoutine);
  LRoutineType := FTypes.CreateRoutineType(FTypes.VoidType);
  LRoutineType.AddParam('expected', FTypes.PointerType, pmConst);
  LRoutineType.AddParam('actual', FTypes.PointerType, pmConst);
  LSym.SymbolType := LRoutineType;

  // TestAssertNil(ptr: pointer)
  LSym := FSymbols.Define('TestAssertNil', skRoutine);
  LRoutineType := FTypes.CreateRoutineType(FTypes.VoidType);
  LRoutineType.AddParam('ptr', FTypes.PointerType, pmConst);
  LSym.SymbolType := LRoutineType;

  // TestAssertNotNil(ptr: pointer)
  LSym := FSymbols.Define('TestAssertNotNil', skRoutine);
  LRoutineType := FTypes.CreateRoutineType(FTypes.VoidType);
  LRoutineType.AddParam('ptr', FTypes.PointerType, pmConst);
  LSym.SymbolType := LRoutineType;

  // TestFail(message: pchar)
  LSym := FSymbols.Define('TestFail', skRoutine);
  LRoutineType := FTypes.CreateRoutineType(FTypes.VoidType);
  LRoutineType.AddParam('message', FTypes.PCharType, pmConst);
  LSym.SymbolType := LRoutineType;
end;

end.
