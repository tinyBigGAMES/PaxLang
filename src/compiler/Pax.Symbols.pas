{===============================================================================
  Pax™ Programming Language.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://paxlang.org

  See LICENSE for license information
===============================================================================}

unit Pax.Symbols;

{$I Pax.Defines.inc}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Pax.Utils,
  Pax.Errors,
  Pax.AST,
  Pax.Types;

type

  { Forward declarations }
  TSymbol = class;
  TScope = class;

  { TSymbolKind }
  TSymbolKind = (
    skUnknown,
    skModule,
    skConst,
    skType,
    skVar,
    skRoutine,
    skParam,
    skField
  );

  { TSymbol }
  TSymbol = class(TBaseObject)
  private
    FName: string;
    FKind: TSymbolKind;
    FSymbolType: TPaxType;
    FScope: TScope;
    FIsPublic: Boolean;
    FIsExternal: Boolean;
    FDeclNode: PASTNode;
    FValue: Int64;
    FFloatValue: Double;
    FStringValue: string;

  public
    constructor Create(); override;
    destructor Destroy(); override;

    property SymbolName: string read FName write FName;
    property Kind: TSymbolKind read FKind write FKind;
    property SymbolType: TPaxType read FSymbolType write FSymbolType;
    property Scope: TScope read FScope write FScope;
    property IsPublic: Boolean read FIsPublic write FIsPublic;
    property IsExternal: Boolean read FIsExternal write FIsExternal;
    property DeclNode: PASTNode read FDeclNode write FDeclNode;
    property Value: Int64 read FValue write FValue;
    property FloatValue: Double read FFloatValue write FFloatValue;
    property StringValue: string read FStringValue write FStringValue;
  end;

  { TScope }
  TScope = class(TBaseObject)
  private
    FName: string;
    FParent: TScope;
    FSymbols: TObjectList<TSymbol>;
    FSymbolMap: TDictionary<string, TSymbol>;
    FChildren: TObjectList<TScope>;

  public
    constructor Create(); override;
    destructor Destroy(); override;

    function Define(const AName: string; const AKind: TSymbolKind): TSymbol;
    function Lookup(const AName: string): TSymbol;
    function LookupLocal(const AName: string): TSymbol;
    function Contains(const AName: string): Boolean;
    function ContainsLocal(const AName: string): Boolean;

    property ScopeName: string read FName write FName;
    property Parent: TScope read FParent write FParent;
    property Symbols: TObjectList<TSymbol> read FSymbols;
    property Children: TObjectList<TScope> read FChildren;
  end;

  { TSymbolTable }
  TSymbolTable = class(TBaseObject)
  private
    FGlobalScope: TScope;
    FCurrentScope: TScope;
    FAllScopes: TObjectList<TScope>;

  public
    constructor Create(); override;
    destructor Destroy(); override;

    procedure PushScope(const AName: string);
    procedure PopScope();

    function Define(const AName: string; const AKind: TSymbolKind): TSymbol;
    function Lookup(const AName: string): TSymbol;
    function LookupLocal(const AName: string): TSymbol;
    function Contains(const AName: string): Boolean;
    function ContainsLocal(const AName: string): Boolean;

    property GlobalScope: TScope read FGlobalScope;
    property CurrentScope: TScope read FCurrentScope;
  end;

function SymbolKindToString(const AKind: TSymbolKind): string;

implementation

{ TSymbol }

constructor TSymbol.Create();
begin
  inherited;

  FName := '';
  FKind := skUnknown;
  FSymbolType := nil;
  FScope := nil;
  FIsPublic := False;
  FIsExternal := False;
  FDeclNode := nil;
  FValue := 0;
  FFloatValue := 0;
  FStringValue := '';
end;

destructor TSymbol.Destroy();
begin
  inherited;
end;

{ TScope }

constructor TScope.Create();
begin
  inherited;

  FName := '';
  FParent := nil;
  FSymbols := TObjectList<TSymbol>.Create(True);
  FSymbolMap := TDictionary<string, TSymbol>.Create();
  FChildren := TObjectList<TScope>.Create(False); // Don't own children
end;

destructor TScope.Destroy();
begin
  FChildren.Free();
  FSymbolMap.Free();
  FSymbols.Free();

  inherited;
end;

function TScope.Define(const AName: string; const AKind: TSymbolKind): TSymbol;
begin
  // Identifiers are case-sensitive
  Result := TSymbol.Create();
  Result.SymbolName := AName;
  Result.Kind := AKind;
  Result.Scope := Self;

  FSymbols.Add(Result);
  FSymbolMap.AddOrSetValue(AName, Result);
end;

function TScope.Lookup(const AName: string): TSymbol;
begin
  // Case-sensitive lookup
  if FSymbolMap.TryGetValue(AName, Result) then
    Exit;

  // Search parent scope
  if FParent <> nil then
    Result := FParent.Lookup(AName)
  else
    Result := nil;
end;

function TScope.LookupLocal(const AName: string): TSymbol;
begin
  // Case-sensitive lookup
  if not FSymbolMap.TryGetValue(AName, Result) then
    Result := nil;
end;

function TScope.Contains(const AName: string): Boolean;
begin
  Result := Lookup(AName) <> nil;
end;

function TScope.ContainsLocal(const AName: string): Boolean;
begin
  Result := LookupLocal(AName) <> nil;
end;

{ TSymbolTable }

constructor TSymbolTable.Create();
begin
  inherited;

  FAllScopes := TObjectList<TScope>.Create(True);

  FGlobalScope := TScope.Create();
  FGlobalScope.ScopeName := 'global';
  FAllScopes.Add(FGlobalScope);

  FCurrentScope := FGlobalScope;
end;

destructor TSymbolTable.Destroy();
begin
  FAllScopes.Free();

  inherited;
end;

procedure TSymbolTable.PushScope(const AName: string);
var
  LNewScope: TScope;
begin
  LNewScope := TScope.Create();
  LNewScope.ScopeName := AName;
  LNewScope.Parent := FCurrentScope;

  FCurrentScope.Children.Add(LNewScope);
  FAllScopes.Add(LNewScope);

  FCurrentScope := LNewScope;
end;

procedure TSymbolTable.PopScope();
begin
  if FCurrentScope.Parent <> nil then
    FCurrentScope := FCurrentScope.Parent;
end;

function TSymbolTable.Define(const AName: string; const AKind: TSymbolKind): TSymbol;
begin
  Result := FCurrentScope.Define(AName, AKind);
end;

function TSymbolTable.Lookup(const AName: string): TSymbol;
begin
  Result := FCurrentScope.Lookup(AName);
end;

function TSymbolTable.LookupLocal(const AName: string): TSymbol;
begin
  Result := FCurrentScope.LookupLocal(AName);
end;

function TSymbolTable.Contains(const AName: string): Boolean;
begin
  Result := FCurrentScope.Contains(AName);
end;

function TSymbolTable.ContainsLocal(const AName: string): Boolean;
begin
  Result := FCurrentScope.ContainsLocal(AName);
end;

function SymbolKindToString(const AKind: TSymbolKind): string;
begin
  case AKind of
    skUnknown:  Result := 'unknown';
    skModule:   Result := 'module';
    skConst:    Result := 'constant';
    skType:     Result := 'type';
    skVar:      Result := 'variable';
    skRoutine:  Result := 'routine';
    skParam:    Result := 'parameter';
    skField:    Result := 'field';
  else
    Result := 'unknown';
  end;
end;

end.
