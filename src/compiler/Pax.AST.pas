{===============================================================================
  Pax™ Programming Language.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://paxlang.org

  See LICENSE for license information
===============================================================================}

unit Pax.AST;

{$I Pax.Defines.inc}

interface

uses
  System.SysUtils,
  Pax.Utils,
  Pax.Lexer;

type

  { TASTNodeKind }
  TASTNodeKind = (
    // Module level
    nkModule,
    nkImport,
    nkDirective,

    // Declarations
    nkConstDecl,
    nkTypeDecl,
    nkVarDecl,
    nkRoutineDecl,
    nkParamDecl,
    nkFieldDecl,

    // Types
    nkRecordType,
    nkUnionType,
    nkArrayType,
    nkPointerType,
    nkSetType,
    nkRoutineType,
    nkTypeRef,

    // Statements
    nkBlock,
    nkIfStmt,
    nkWhileStmt,
    nkForStmt,
    nkRepeatStmt,
    nkCaseStmt,
    nkCaseArm,
    nkReturnStmt,
    nkAssignment,
    nkCallStmt,
    nkNewStmt,
    nkDisposeStmt,
    nkSetLengthStmt,
    nkGcCollectStmt,
    nkGcDumpStmt,
    nkEmptyStmt,

    // Expressions
    nkBinaryOp,
    nkUnaryOp,
    nkIdentifier,
    nkQualifiedIdent,
    nkIntLiteral,
    nkFloatLiteral,
    nkStrLiteral,
    nkWideStrLiteral,
    nkCharLiteral,
    nkWideCharLiteral,
    nkBoolLiteral,
    nkNilLiteral,
    nkSetLiteral,
    nkSetRange,
    nkArrayAccess,
    nkFieldAccess,
    nkDeref,
    nkAddressOf,
    nkCall,
    nkTypeCast,
    nkSizeOf,
    nkLen,
    nkGcHeapSize,
    nkGcUsedSize,
    nkGcCollectCount,
    nkParamCount,
    nkParamStr,

    // Test
    nkTestBlock
  );

  { TOperator }
  TOperator = (
    opNone,

    // Binary arithmetic
    opAdd,
    opSub,
    opMul,
    opDiv,
    opDivInt,
    opMod,

    // Binary comparison
    opEq,
    opNe,
    opLt,
    opGt,
    opLe,
    opGe,

    // Binary logical
    opAnd,
    opOr,
    opIn,

    // Unary
    opNot,
    opNeg,
    opPos
  );

  { Forward declaration }
  PASTNode = ^TASTNode;

  { TASTNode }
  TASTNode = record
    Kind: TASTNodeKind;
    Token: TToken;

    // Common fields
    NodeName: string;
    Children: TArray<PASTNode>;

    // Kind-specific data
    IntVal: Int64;
    FloatVal: Double;
    StrVal: string;
    BoolVal: Boolean;
    Op: TOperator;

    // Type info (filled by Checker)
    TypeNode: PASTNode;

    // Flags
    IsPublic: Boolean;
    IsExternal: Boolean;
    IsForwardTo: Boolean;
    IsVariadic: Boolean;
    IsPacked: Boolean;
    IsFlexibleArray: Boolean;  // For flexible array members (array[] of T)
    Alignment: Integer;
    BitWidth: Integer;  // For bit fields (0 = not a bit field)

    // External routine info
    ExternalLib: string;
  end;

// AST helper functions
function CreateASTNode(const AKind: TASTNodeKind): PASTNode;
function CreateASTNodeWithToken(const AKind: TASTNodeKind; const AToken: TToken): PASTNode;
procedure FreeASTNode(var ANode: PASTNode);
procedure AddASTChild(const AParent: PASTNode; const AChild: PASTNode);
function GetASTChildCount(const ANode: PASTNode): Integer;
function GetASTChild(const ANode: PASTNode; const AIndex: Integer): PASTNode;
function ASTNodeKindToString(const AKind: TASTNodeKind): string;
function OperatorToString(const AOp: TOperator): string;

implementation

function CreateASTNode(const AKind: TASTNodeKind): PASTNode;
begin
  New(Result);

  Result^.Kind := AKind;
  Result^.Token.Kind := tkEOF;
  Result^.Token.Lexeme := '';
  Result^.Token.Range.Clear();
  Result^.Token.IntValue := 0;
  Result^.Token.FloatValue := 0;

  Result^.NodeName := '';
  SetLength(Result^.Children, 0);

  Result^.IntVal := 0;
  Result^.FloatVal := 0;
  Result^.StrVal := '';
  Result^.BoolVal := False;
  Result^.Op := opNone;

  Result^.TypeNode := nil;

  Result^.IsPublic := False;
  Result^.IsExternal := False;
  Result^.IsForwardTo := False;
  Result^.IsVariadic := False;
  Result^.IsPacked := False;
  Result^.IsFlexibleArray := False;
  Result^.Alignment := 0;
  Result^.BitWidth := 0;

  Result^.ExternalLib := '';
end;

function CreateASTNodeWithToken(const AKind: TASTNodeKind; const AToken: TToken): PASTNode;
begin
  Result := CreateASTNode(AKind);
  Result^.Token := AToken;
end;

procedure FreeASTNode(var ANode: PASTNode);
var
  LI: Integer;
begin
  if ANode = nil then
    Exit;

  // Recursively free children
  for LI := 0 to High(ANode^.Children) do
    FreeASTNode(ANode^.Children[LI]);

  SetLength(ANode^.Children, 0);

  // Note: TypeNode is not owned, don't free it

  Dispose(ANode);
  ANode := nil;
end;

procedure AddASTChild(const AParent: PASTNode; const AChild: PASTNode);
var
  LLen: Integer;
begin
  if AParent = nil then
    Exit;

  LLen := Length(AParent^.Children);
  SetLength(AParent^.Children, LLen + 1);
  AParent^.Children[LLen] := AChild;
end;

function GetASTChildCount(const ANode: PASTNode): Integer;
begin
  if ANode = nil then
    Result := 0
  else
    Result := Length(ANode^.Children);
end;

function GetASTChild(const ANode: PASTNode; const AIndex: Integer): PASTNode;
begin
  if ANode = nil then
    Exit(nil);

  if (AIndex < 0) or (AIndex >= Length(ANode^.Children)) then
    Exit(nil);

  Result := ANode^.Children[AIndex];
end;

function ASTNodeKindToString(const AKind: TASTNodeKind): string;
begin
  case AKind of
    nkModule:         Result := 'Module';
    nkImport:         Result := 'Import';
    nkDirective:      Result := 'Directive';
    nkConstDecl:      Result := 'ConstDecl';
    nkTypeDecl:       Result := 'TypeDecl';
    nkVarDecl:        Result := 'VarDecl';
    nkRoutineDecl:    Result := 'RoutineDecl';
    nkParamDecl:      Result := 'ParamDecl';
    nkFieldDecl:      Result := 'FieldDecl';
    nkRecordType:     Result := 'RecordType';
    nkUnionType:      Result := 'UnionType';
    nkArrayType:      Result := 'ArrayType';
    nkPointerType:    Result := 'PointerType';
    nkSetType:        Result := 'SetType';
    nkRoutineType:    Result := 'RoutineType';
    nkTypeRef:        Result := 'TypeRef';
    nkBlock:          Result := 'Block';
    nkIfStmt:         Result := 'IfStmt';
    nkWhileStmt:      Result := 'WhileStmt';
    nkForStmt:        Result := 'ForStmt';
    nkRepeatStmt:     Result := 'RepeatStmt';
    nkCaseStmt:       Result := 'CaseStmt';
    nkCaseArm:        Result := 'CaseArm';
    nkReturnStmt:     Result := 'ReturnStmt';
    nkAssignment:     Result := 'Assignment';
    nkCallStmt:       Result := 'CallStmt';
    nkNewStmt:        Result := 'NewStmt';
    nkDisposeStmt:    Result := 'DisposeStmt';
    nkSetLengthStmt:  Result := 'SetLengthStmt';
    nkGcCollectStmt:  Result := 'GcCollectStmt';
    nkGcDumpStmt:     Result := 'GcDumpStmt';
    nkEmptyStmt:      Result := 'EmptyStmt';
    nkBinaryOp:       Result := 'BinaryOp';
    nkUnaryOp:        Result := 'UnaryOp';
    nkIdentifier:     Result := 'Identifier';
    nkQualifiedIdent: Result := 'QualifiedIdent';
    nkIntLiteral:     Result := 'IntLiteral';
    nkFloatLiteral:   Result := 'FloatLiteral';
    nkStrLiteral:     Result := 'StrLiteral';
    nkWideStrLiteral: Result := 'WideStrLiteral';
    nkCharLiteral:    Result := 'CharLiteral';
    nkWideCharLiteral:Result := 'WideCharLiteral';
    nkBoolLiteral:    Result := 'BoolLiteral';
    nkNilLiteral:     Result := 'NilLiteral';
    nkSetLiteral:     Result := 'SetLiteral';
    nkSetRange:       Result := 'SetRange';
    nkArrayAccess:    Result := 'ArrayAccess';
    nkFieldAccess:    Result := 'FieldAccess';
    nkDeref:          Result := 'Deref';
    nkAddressOf:      Result := 'AddressOf';
    nkCall:           Result := 'Call';
    nkTypeCast:       Result := 'TypeCast';
    nkSizeOf:         Result := 'SizeOf';
    nkLen:            Result := 'Len';
    nkGcHeapSize:     Result := 'GcHeapSize';
    nkGcUsedSize:     Result := 'GcUsedSize';
    nkGcCollectCount: Result := 'GcCollectCount';
    nkTestBlock:      Result := 'TestBlock';
  else
    Result := 'Unknown';
  end;
end;

function OperatorToString(const AOp: TOperator): string;
begin
  case AOp of
    opNone:   Result := 'none';
    opAdd:    Result := '+';
    opSub:    Result := '-';
    opMul:    Result := '*';
    opDiv:    Result := '/';
    opDivInt: Result := 'div';
    opMod:    Result := 'mod';
    opEq:     Result := '=';
    opNe:     Result := '<>';
    opLt:     Result := '<';
    opGt:     Result := '>';
    opLe:     Result := '<=';
    opGe:     Result := '>=';
    opAnd:    Result := 'and';
    opOr:     Result := 'or';
    opIn:     Result := 'in';
    opNot:    Result := 'not';
    opNeg:    Result := '-';
    opPos:    Result := '+';
  else
    Result := 'unknown';
  end;
end;

end.
