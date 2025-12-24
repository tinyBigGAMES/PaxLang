{===============================================================================
  Pax™ Programming Language.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://paxlang.org

  See LICENSE for license information
===============================================================================}

unit Pax.Types;

{$I Pax.Defines.inc}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Pax.Utils;

const
  PAX_MAJOR_VERSION = 0;
  PAX_MINOR_VERSION = 1;
  PAX_PATCH_VERSION = 0;

type

  { Forward declarations }
  TPaxType = class;
  TPaxField = class;

  { TPaxTypeKind }
  TPaxTypeKind = (
    tkUnknown,
    tkVoid,
    tkError,

    // Integer types
    tkInt8,
    tkInt16,
    tkInt32,
    tkInt64,
    tkUInt8,
    tkUInt16,
    tkUInt32,
    tkUInt64,

    // Float types
    tkFloat32,
    tkFloat64,

    // Boolean
    tkBoolean,

    // Character types
    tkChar,
    tkUChar,
    tkWChar,
    tkUWChar,

    // String types
    tkString,
    tkWString,

    // Composite types
    tkPointer,
    tkArray,
    tkRecord,
    tkSet,
    tkRoutine
  );

  { TOutputType }
  TOutputType = (
    otEXE,
    otLIB,
    otDLL
  );

  { TOutputCallback }
  TOutputCallback = reference to procedure(const AText: string);

  { TPaxParamMode }
  TPaxParamMode = (
    pmValue,
    pmVar,
    pmConst
  );

  { TPaxParam }
  TPaxParam = class(TBaseObject)
  private
    FName: string;
    FParamType: TPaxType;
    FMode: TPaxParamMode;

  public
    constructor Create(); override;
    destructor Destroy(); override;

    property ParamName: string read FName write FName;
    property ParamType: TPaxType read FParamType write FParamType;
    property Mode: TPaxParamMode read FMode write FMode;
  end;

  { TPaxField }
  TPaxField = class(TBaseObject)
  private
    FName: string;
    FFieldType: TPaxType;
    FOffset: Integer;

  public
    constructor Create(); override;
    destructor Destroy(); override;

    property FieldName: string read FName write FName;
    property FieldType: TPaxType read FFieldType write FFieldType;
    property Offset: Integer read FOffset write FOffset;
  end;

  { TPaxType }
  TPaxType = class(TBaseObject)
  private
    FKind: TPaxTypeKind;
    FName: string;
    FSize: Integer;
    FAlignment: Integer;

    // For pointer and array types
    FElementType: TPaxType;

    // For static arrays
    FLowBound: Int64;
    FHighBound: Int64;
    FIsDynamic: Boolean;

    // For record types
    FFields: TObjectList<TPaxField>;
    FParentType: TPaxType;

    // For routine types
    FParams: TObjectList<TPaxParam>;
    FReturnType: TPaxType;
    FIsVariadic: Boolean;

  public
    constructor Create(); override;
    destructor Destroy(); override;

    function IsInteger(): Boolean;
    function IsSignedInteger(): Boolean;
    function IsUnsignedInteger(): Boolean;
    function IsFloat(): Boolean;
    function IsNumeric(): Boolean;
    function IsOrdinal(): Boolean;
    function IsBoolean(): Boolean;
    function IsChar(): Boolean;
    function IsWChar(): Boolean;
    function IsString(): Boolean;
    function IsWString(): Boolean;
    function IsPointer(): Boolean;
    function IsArray(): Boolean;
    function IsRecord(): Boolean;
    function IsSet(): Boolean;
    function IsRoutine(): Boolean;
    function IsVoid(): Boolean;
    function IsError(): Boolean;

    function GetField(const AName: string): TPaxField;
    function AddField(const AName: string; const AFieldType: TPaxType): TPaxField;
    function AddParam(const AName: string; const AParamType: TPaxType; const AMode: TPaxParamMode): TPaxParam;

    property Kind: TPaxTypeKind read FKind write FKind;
    property TypeName: string read FName write FName;
    property Size: Integer read FSize write FSize;
    property Alignment: Integer read FAlignment write FAlignment;
    property ElementType: TPaxType read FElementType write FElementType;
    property LowBound: Int64 read FLowBound write FLowBound;
    property HighBound: Int64 read FHighBound write FHighBound;
    property IsDynamic: Boolean read FIsDynamic write FIsDynamic;
    property Fields: TObjectList<TPaxField> read FFields;
    property ParentType: TPaxType read FParentType write FParentType;
    property Params: TObjectList<TPaxParam> read FParams;
    property ReturnType: TPaxType read FReturnType write FReturnType;
    property IsVariadic: Boolean read FIsVariadic write FIsVariadic;
  end;

  { TPaxTypeRegistry }
  TPaxTypeRegistry = class(TBaseObject)
  private
    FTypes: TObjectList<TPaxType>;
    FTypeMap: TDictionary<string, TPaxType>;

    // Built-in types (cached)
    FVoidType: TPaxType;
    FErrorType: TPaxType;
    FInt8Type: TPaxType;
    FInt16Type: TPaxType;
    FInt32Type: TPaxType;
    FInt64Type: TPaxType;
    FUInt8Type: TPaxType;
    FUInt16Type: TPaxType;
    FUInt32Type: TPaxType;
    FUInt64Type: TPaxType;
    FFloat32Type: TPaxType;
    FFloat64Type: TPaxType;
    FBooleanType: TPaxType;
    FCharType: TPaxType;
    FUCharType: TPaxType;
    FWCharType: TPaxType;
    FUWCharType: TPaxType;
    FStringType: TPaxType;
    FWStringType: TPaxType;
    FPointerType: TPaxType;
    FPCharType: TPaxType;
    FPWCharType: TPaxType;

    procedure RegisterBuiltInTypes();
    function CreateBuiltInType(const AKind: TPaxTypeKind; const AName: string; const ASize: Integer): TPaxType;

  public
    constructor Create(); override;
    destructor Destroy(); override;

    function GetType(const AName: string): TPaxType;
    function RegisterType(const AType: TPaxType): TPaxType;

    function CreatePointerType(const AElementType: TPaxType): TPaxType;
    function CreateArrayType(const AElementType: TPaxType; const ALow, AHigh: Int64): TPaxType;
    function CreateDynamicArrayType(const AElementType: TPaxType): TPaxType;
    function CreateRecordType(const AName: string): TPaxType;
    function CreateSetType(const AElementType: TPaxType; const ALow, AHigh: Int64): TPaxType;
    function CreateRoutineType(const AReturnType: TPaxType): TPaxType;

    // Built-in type accessors
    property VoidType: TPaxType read FVoidType;
    property ErrorType: TPaxType read FErrorType;
    property Int8Type: TPaxType read FInt8Type;
    property Int16Type: TPaxType read FInt16Type;
    property Int32Type: TPaxType read FInt32Type;
    property Int64Type: TPaxType read FInt64Type;
    property UInt8Type: TPaxType read FUInt8Type;
    property UInt16Type: TPaxType read FUInt16Type;
    property UInt32Type: TPaxType read FUInt32Type;
    property UInt64Type: TPaxType read FUInt64Type;
    property Float32Type: TPaxType read FFloat32Type;
    property Float64Type: TPaxType read FFloat64Type;
    property BooleanType: TPaxType read FBooleanType;
    property CharType: TPaxType read FCharType;
    property UCharType: TPaxType read FUCharType;
    property WCharType: TPaxType read FWCharType;
    property UWCharType: TPaxType read FUWCharType;
    property StringType: TPaxType read FStringType;
    property WStringType: TPaxType read FWStringType;
    property PointerType: TPaxType read FPointerType;
    property PCharType: TPaxType read FPCharType;
    property PWCharType: TPaxType read FPWCharType;
  end;

// Type comparison and utility functions
function TypesEqual(const AType1, AType2: TPaxType): Boolean;
function TypesCompatible(const ATarget, ASource: TPaxType): Boolean;
function TypesAssignmentCompatible(const ATarget, ASource: TPaxType): Boolean;
function GetCommonType(const AType1, AType2: TPaxType): TPaxType;
function TypeKindToString(const AKind: TPaxTypeKind): string;

implementation

{ TPaxParam }

constructor TPaxParam.Create();
begin
  inherited;

  FName := '';
  FParamType := nil;
  FMode := pmValue;
end;

destructor TPaxParam.Destroy();
begin
  inherited;
end;

{ TPaxField }

constructor TPaxField.Create();
begin
  inherited;

  FName := '';
  FFieldType := nil;
  FOffset := 0;
end;

destructor TPaxField.Destroy();
begin
  inherited;
end;

{ TPaxType }

constructor TPaxType.Create();
begin
  inherited;

  FKind := tkUnknown;
  FName := '';
  FSize := 0;
  FAlignment := 1;
  FElementType := nil;
  FLowBound := 0;
  FHighBound := 0;
  FIsDynamic := False;
  FFields := TObjectList<TPaxField>.Create(True);
  FParentType := nil;
  FParams := TObjectList<TPaxParam>.Create(True);
  FReturnType := nil;
  FIsVariadic := False;
end;

destructor TPaxType.Destroy();
begin
  FParams.Free();
  FFields.Free();

  inherited;
end;

function TPaxType.IsInteger(): Boolean;
begin
  Result := FKind in [tkInt8, tkInt16, tkInt32, tkInt64,
                      tkUInt8, tkUInt16, tkUInt32, tkUInt64];
end;

function TPaxType.IsSignedInteger(): Boolean;
begin
  Result := FKind in [tkInt8, tkInt16, tkInt32, tkInt64];
end;

function TPaxType.IsUnsignedInteger(): Boolean;
begin
  Result := FKind in [tkUInt8, tkUInt16, tkUInt32, tkUInt64];
end;

function TPaxType.IsFloat(): Boolean;
begin
  Result := FKind in [tkFloat32, tkFloat64];
end;

function TPaxType.IsNumeric(): Boolean;
begin
  Result := IsInteger() or IsFloat();
end;

function TPaxType.IsOrdinal(): Boolean;
begin
  Result := IsInteger() or IsBoolean() or IsChar();
end;

function TPaxType.IsBoolean(): Boolean;
begin
  Result := FKind = tkBoolean;
end;

function TPaxType.IsChar(): Boolean;
begin
  Result := FKind in [tkChar, tkUChar, tkWChar, tkUWChar];
end;

function TPaxType.IsWChar(): Boolean;
begin
  Result := FKind in [tkWChar, tkUWChar];
end;

function TPaxType.IsString(): Boolean;
begin
  Result := FKind in [tkString, tkWString];
end;

function TPaxType.IsWString(): Boolean;
begin
  Result := FKind = tkWString;
end;

function TPaxType.IsPointer(): Boolean;
begin
  Result := FKind = tkPointer;
end;

function TPaxType.IsArray(): Boolean;
begin
  Result := FKind = tkArray;
end;

function TPaxType.IsRecord(): Boolean;
begin
  Result := FKind = tkRecord;
end;

function TPaxType.IsSet(): Boolean;
begin
  Result := FKind = tkSet;
end;

function TPaxType.IsRoutine(): Boolean;
begin
  Result := FKind = tkRoutine;
end;

function TPaxType.IsVoid(): Boolean;
begin
  Result := FKind = tkVoid;
end;

function TPaxType.IsError(): Boolean;
begin
  Result := FKind = tkError;
end;

function TPaxType.GetField(const AName: string): TPaxField;
var
  LField: TPaxField;
  LLowerName: string;
begin
  Result := nil;
  LLowerName := LowerCase(AName);

  for LField in FFields do
  begin
    if LowerCase(LField.FieldName) = LLowerName then
      Exit(LField);
  end;

  // Check parent type
  if FParentType <> nil then
    Result := FParentType.GetField(AName);
end;

function TPaxType.AddField(const AName: string; const AFieldType: TPaxType): TPaxField;
begin
  Result := TPaxField.Create();
  Result.FieldName := AName;
  Result.FieldType := AFieldType;
  Result.Offset := FSize;

  FFields.Add(Result);

  // Update size (simple sequential layout)
  if AFieldType <> nil then
    FSize := FSize + AFieldType.Size;
end;

function TPaxType.AddParam(const AName: string; const AParamType: TPaxType; const AMode: TPaxParamMode): TPaxParam;
begin
  Result := TPaxParam.Create();
  Result.ParamName := AName;
  Result.ParamType := AParamType;
  Result.Mode := AMode;

  FParams.Add(Result);
end;

{ TPaxTypeRegistry }

constructor TPaxTypeRegistry.Create();
begin
  inherited;

  FTypes := TObjectList<TPaxType>.Create(True);
  FTypeMap := TDictionary<string, TPaxType>.Create();

  RegisterBuiltInTypes();
end;

destructor TPaxTypeRegistry.Destroy();
begin
  FTypeMap.Free();
  FTypes.Free();

  inherited;
end;

procedure TPaxTypeRegistry.RegisterBuiltInTypes();
begin
  // Special types
  FVoidType := CreateBuiltInType(tkVoid, 'void', 0);
  FErrorType := CreateBuiltInType(tkError, '<error>', 0);

  // Integer types
  FInt8Type := CreateBuiltInType(tkInt8, 'int8', 1);
  FInt16Type := CreateBuiltInType(tkInt16, 'int16', 2);
  FInt32Type := CreateBuiltInType(tkInt32, 'int32', 4);
  FInt64Type := CreateBuiltInType(tkInt64, 'int64', 8);
  FUInt8Type := CreateBuiltInType(tkUInt8, 'uint8', 1);
  FUInt16Type := CreateBuiltInType(tkUInt16, 'uint16', 2);
  FUInt32Type := CreateBuiltInType(tkUInt32, 'uint32', 4);
  FUInt64Type := CreateBuiltInType(tkUInt64, 'uint64', 8);

  // Float types
  FFloat32Type := CreateBuiltInType(tkFloat32, 'float32', 4);
  FFloat64Type := CreateBuiltInType(tkFloat64, 'float64', 8);

  // Boolean
  FBooleanType := CreateBuiltInType(tkBoolean, 'boolean', 1);

  // Character types
  FCharType := CreateBuiltInType(tkChar, 'char', 1);
  FUCharType := CreateBuiltInType(tkUChar, 'uchar', 1);
  FWCharType := CreateBuiltInType(tkWChar, 'wchar', 2);
  FUWCharType := CreateBuiltInType(tkUWChar, 'uwchar', 2);

  // String types (pointers to managed strings)
  FStringType := CreateBuiltInType(tkString, 'string', 8);
  FWStringType := CreateBuiltInType(tkWString, 'wstring', 8);

  // Untyped pointer
  FPointerType := CreateBuiltInType(tkPointer, 'pointer', 8);

  // Pointer to char types (for string literals)
  FPCharType := CreatePointerType(FCharType);
  FPWCharType := CreatePointerType(FWCharType);
end;

function TPaxTypeRegistry.CreateBuiltInType(const AKind: TPaxTypeKind; const AName: string; const ASize: Integer): TPaxType;
begin
  Result := TPaxType.Create();
  Result.Kind := AKind;
  Result.TypeName := AName;
  Result.Size := ASize;
  Result.Alignment := ASize;

  if Result.Alignment = 0 then
    Result.Alignment := 1;

  FTypes.Add(Result);
  FTypeMap.Add(LowerCase(AName), Result);
end;

function TPaxTypeRegistry.GetType(const AName: string): TPaxType;
begin
  if not FTypeMap.TryGetValue(LowerCase(AName), Result) then
    Result := nil;
end;

function TPaxTypeRegistry.RegisterType(const AType: TPaxType): TPaxType;
begin
  Result := AType;

  if AType = nil then
    Exit;

  if AType.TypeName <> '' then
  begin
    if not FTypeMap.ContainsKey(LowerCase(AType.TypeName)) then
    begin
      // Only add to FTypes if not already there (may have been added by Create*Type)
      if FTypes.IndexOf(AType) < 0 then
        FTypes.Add(AType);
      FTypeMap.Add(LowerCase(AType.TypeName), AType);
    end;
  end
  else
  begin
    // Only add to FTypes if not already there
    if FTypes.IndexOf(AType) < 0 then
      FTypes.Add(AType);
  end;
end;

function TPaxTypeRegistry.CreatePointerType(const AElementType: TPaxType): TPaxType;
begin
  Result := TPaxType.Create();
  Result.Kind := tkPointer;
  Result.Size := 8;
  Result.Alignment := 8;
  Result.ElementType := AElementType;

  if AElementType <> nil then
    Result.TypeName := 'pointer to ' + AElementType.TypeName
  else
    Result.TypeName := 'pointer';

  FTypes.Add(Result);
end;

function TPaxTypeRegistry.CreateArrayType(const AElementType: TPaxType; const ALow, AHigh: Int64): TPaxType;
var
  LCount: Int64;
begin
  Result := TPaxType.Create();
  Result.Kind := tkArray;
  Result.ElementType := AElementType;
  Result.LowBound := ALow;
  Result.HighBound := AHigh;
  Result.IsDynamic := False;

  LCount := AHigh - ALow + 1;

  if AElementType <> nil then
  begin
    Result.Size := LCount * AElementType.Size;
    Result.Alignment := AElementType.Alignment;
    Result.TypeName := Format('array[%d..%d] of %s', [ALow, AHigh, AElementType.TypeName]);
  end
  else
  begin
    Result.Size := 0;
    Result.Alignment := 1;
    Result.TypeName := Format('array[%d..%d] of <unknown>', [ALow, AHigh]);
  end;

  FTypes.Add(Result);
end;

function TPaxTypeRegistry.CreateDynamicArrayType(const AElementType: TPaxType): TPaxType;
begin
  Result := TPaxType.Create();
  Result.Kind := tkArray;
  Result.ElementType := AElementType;
  Result.LowBound := 0;
  Result.HighBound := -1;
  Result.IsDynamic := True;
  Result.Size := 8; // Pointer to array structure
  Result.Alignment := 8;

  if AElementType <> nil then
    Result.TypeName := 'array[] of ' + AElementType.TypeName
  else
    Result.TypeName := 'array[] of <unknown>';

  FTypes.Add(Result);
end;

function TPaxTypeRegistry.CreateRecordType(const AName: string): TPaxType;
begin
  Result := TPaxType.Create();
  Result.Kind := tkRecord;
  Result.TypeName := AName;
  Result.Size := 0;
  Result.Alignment := 1;

  FTypes.Add(Result);

  if AName <> '' then
    FTypeMap.Add(LowerCase(AName), Result);
end;

function TPaxTypeRegistry.CreateSetType(const AElementType: TPaxType; const ALow, AHigh: Int64): TPaxType;
begin
  Result := TPaxType.Create();
  Result.Kind := tkSet;
  Result.ElementType := AElementType;
  Result.LowBound := ALow;
  Result.HighBound := AHigh;
  Result.Size := 8; // Use 64-bit bitset
  Result.Alignment := 8;

  if AElementType <> nil then
    Result.TypeName := 'set of ' + AElementType.TypeName
  else
    Result.TypeName := Format('set of %d..%d', [ALow, AHigh]);

  FTypes.Add(Result);
end;

function TPaxTypeRegistry.CreateRoutineType(const AReturnType: TPaxType): TPaxType;
begin
  Result := TPaxType.Create();
  Result.Kind := tkRoutine;
  Result.ReturnType := AReturnType;
  Result.Size := 8; // Pointer to routine
  Result.Alignment := 8;
  Result.TypeName := 'routine';

  FTypes.Add(Result);
end;

{ Utility functions }

function TypesEqual(const AType1, AType2: TPaxType): Boolean;
begin
  if (AType1 = nil) or (AType2 = nil) then
    Exit(AType1 = AType2);

  if AType1 = AType2 then
    Exit(True);

  if AType1.Kind <> AType2.Kind then
    Exit(False);

  // For pointer types, compare element types
  if AType1.Kind = tkPointer then
  begin
    // Untyped pointer matches any pointer
    if (AType1.ElementType = nil) or (AType2.ElementType = nil) then
      Exit(True);
    Exit(TypesEqual(AType1.ElementType, AType2.ElementType));
  end;

  // For array types
  if AType1.Kind = tkArray then
  begin
    if AType1.IsDynamic <> AType2.IsDynamic then
      Exit(False);

    if not AType1.IsDynamic then
    begin
      if (AType1.LowBound <> AType2.LowBound) or (AType1.HighBound <> AType2.HighBound) then
        Exit(False);
    end;

    Exit(TypesEqual(AType1.ElementType, AType2.ElementType));
  end;

  // For built-in types, same kind means equal
  Result := True;
end;

function TypesCompatible(const ATarget, ASource: TPaxType): Boolean;
begin
  if (ATarget = nil) or (ASource = nil) then
    Exit(False);

  if ATarget.IsError() or ASource.IsError() then
    Exit(True); // Error type compatible with anything to reduce noise

  if TypesEqual(ATarget, ASource) then
    Exit(True);

  // Integer types are compatible with each other
  if ATarget.IsInteger() and ASource.IsInteger() then
    Exit(True);

  // Float types are compatible with integers and each other
  if ATarget.IsFloat() and (ASource.IsNumeric()) then
    Exit(True);

  // Char types are compatible with each other
  if ATarget.IsChar() and ASource.IsChar() then
    Exit(True);

  // Pointer to char (string literal) can be implicitly converted to char
  // This allows: var c: char; c := 'A';
  if ATarget.IsChar() and ASource.IsPointer() then
  begin
    if (ASource.ElementType <> nil) and ASource.ElementType.IsChar() then
      Exit(True);
  end;

  // String types
  if ATarget.IsString() and ASource.IsString() then
    Exit(True);

  // Pointer to char can be assigned to string (for string literals)
  if ATarget.IsString() and ASource.IsPointer() then
  begin
    if ASource.ElementType <> nil then
    begin
      // pointer to char -> string
      if (ATarget.Kind = tkString) and (ASource.ElementType.Kind = tkChar) then
        Exit(True);
      // pointer to wchar -> wstring
      if (ATarget.Kind = tkWString) and (ASource.ElementType.Kind = tkWChar) then
        Exit(True);
    end;
  end;

  // Pointer compatibility
  if ATarget.IsPointer() and ASource.IsPointer() then
  begin
    // Untyped pointer compatible with any pointer
    if (ATarget.ElementType = nil) or (ASource.ElementType = nil) then
      Exit(True);

    Exit(TypesEqual(ATarget.ElementType, ASource.ElementType));
  end;

  // nil is compatible with any pointer
  if ATarget.IsPointer() and (ASource.Kind = tkPointer) and (ASource.ElementType = nil) then
    Exit(True);

  Result := False;
end;

function TypesAssignmentCompatible(const ATarget, ASource: TPaxType): Boolean;
begin
  Result := TypesCompatible(ATarget, ASource);
end;

function GetCommonType(const AType1, AType2: TPaxType): TPaxType;
begin
  if (AType1 = nil) or (AType2 = nil) then
    Exit(nil);

  if TypesEqual(AType1, AType2) then
    Exit(AType1);

  // Integer promotion
  if AType1.IsInteger() and AType2.IsInteger() then
  begin
    // Return the larger type
    if AType1.Size >= AType2.Size then
      Exit(AType1)
    else
      Exit(AType2);
  end;

  // Float promotion
  if AType1.IsNumeric() and AType2.IsNumeric() then
  begin
    if AType1.IsFloat() and AType2.IsFloat() then
    begin
      if AType1.Size >= AType2.Size then
        Exit(AType1)
      else
        Exit(AType2);
    end;

    // Integer + float = float
    if AType1.IsFloat() then
      Exit(AType1);
    if AType2.IsFloat() then
      Exit(AType2);
  end;

  Result := nil;
end;

function TypeKindToString(const AKind: TPaxTypeKind): string;
begin
  case AKind of
    tkUnknown:  Result := 'unknown';
    tkVoid:     Result := 'void';
    tkError:    Result := '<error>';
    tkInt8:     Result := 'int8';
    tkInt16:    Result := 'int16';
    tkInt32:    Result := 'int32';
    tkInt64:    Result := 'int64';
    tkUInt8:    Result := 'uint8';
    tkUInt16:   Result := 'uint16';
    tkUInt32:   Result := 'uint32';
    tkUInt64:   Result := 'uint64';
    tkFloat32:  Result := 'float32';
    tkFloat64:  Result := 'float64';
    tkBoolean:  Result := 'boolean';
    tkChar:     Result := 'char';
    tkUChar:    Result := 'uchar';
    tkWChar:    Result := 'wchar';
    tkUWChar:   Result := 'uwchar';
    tkString:   Result := 'string';
    tkWString:  Result := 'wstring';
    tkPointer:  Result := 'pointer';
    tkArray:    Result := 'array';
    tkRecord:   Result := 'record';
    tkSet:      Result := 'set';
    tkRoutine:  Result := 'routine';
  else
    Result := 'unknown';
  end;
end;

end.
