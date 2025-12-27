{===============================================================================
  Pax™ Programming Language.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://paxlang.org

  See LICENSE for license information
===============================================================================}

unit Pax.Errors;

{$I Pax.Defines.inc}

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.Generics.Collections,
  Pax.Utils,
  Pax.Resources;

const
  DEFAULT_MAX_ERRORS = 1;

  // TCC/C backend error codes
  ERR_TCC = 'C001';
  WRN_TCC = 'C002';
  HNT_TCC = 'C003';

  // Lexer error codes
  ERR_LEXER_CONFIG_NOT_FOUND = 'L001';
  ERR_LEXER_CONFIG_FAILED    = 'L002';

  // Parser error codes
  ERR_PARSER_EXPECTED_TOKEN = 'P001';
  ERR_PARSER_UNKNOWN_RULE   = 'P002';
  ERR_PARSER_CONFIG_FAILED  = 'P003';
  ERR_PARSER_LEXER_NOT_SET  = 'P004';
  ERR_PARSER_NO_START_RULE  = 'P005';
  ERR_PARSER_FAILED         = 'P006';

  // Semantic error codes
  ERR_SEMANTIC_UNKNOWN_TYPE            = 'S001';
  ERR_SEMANTIC_DUPLICATE_IDENT         = 'S002';
  ERR_SEMANTIC_UNDECLARED_IDENT        = 'S003';
  ERR_SEMANTIC_TYPE_MISMATCH           = 'S004';
  ERR_SEMANTIC_UNKNOWN_FIELD           = 'S005';
  ERR_SEMANTIC_CONDITION_NOT_BOOLEAN   = 'S006';
  ERR_SEMANTIC_ARG_COUNT_MISMATCH      = 'S007';
  ERR_SEMANTIC_CALL_NON_ROUTINE        = 'S008';
  ERR_SEMANTIC_RETURN_TYPE_MISMATCH    = 'S009';
  ERR_SEMANTIC_RETURN_VALUE_IN_VOID    = 'S010';
  ERR_SEMANTIC_DEREF_NON_POINTER       = 'S011';
  ERR_SEMANTIC_ARG_TYPE_MISMATCH       = 'S012';
  ERR_SEMANTIC_NEW_NON_POINTER         = 'S013';
  ERR_SEMANTIC_DISPOSE_NON_POINTER     = 'S014';
  ERR_SEMANTIC_SETLENGTH_NON_ARRAY     = 'S015';
  ERR_SEMANTIC_SETLENGTH_NON_INTEGER   = 'S016';
  ERR_SEMANTIC_INVALID_CAST            = 'S017';
  ERR_SEMANTIC_FOR_VAR_NOT_INTEGER     = 'S018';
  ERR_SEMANTIC_CASE_NOT_ORDINAL        = 'S019';
  ERR_SEMANTIC_INDEX_NOT_INTEGER       = 'S020';
  ERR_SEMANTIC_INVALID_OPERATOR        = 'S021';
  ERR_SEMANTIC_INVALID_UNARY_OPERATOR  = 'S022';
  ERR_SEMANTIC_IN_NOT_ORDINAL          = 'S023';
  ERR_SEMANTIC_IN_NOT_SET              = 'S024';
  ERR_SEMANTIC_DLL_INIT_BLOCK          = 'S025';
  ERR_SEMANTIC_INVALID_ALIGNMENT        = 'S026';
  ERR_SEMANTIC_BITFIELD_WIDTH           = 'S027';
  ERR_SEMANTIC_BITFIELD_POSITIVE        = 'S028';
  ERR_SEMANTIC_BITFIELD_TYPE            = 'S029';
  ERR_SEMANTIC_FLEXIBLE_ARRAY_LAST       = 'S030';
  ERR_SEMANTIC_FLEXIBLE_ARRAY_UNION      = 'S031';
  ERR_SEMANTIC_FLEXIBLE_ARRAY_ONLY_FIELD = 'S032';
  ERR_SEMANTIC_CONST_EXPR_REQUIRED       = 'S033';

  // Semantic warning codes
  WRN_SEMANTIC_UNUSED_VAR              = 'W001';

type

  { TErrorSeverity }
  TErrorSeverity = (
    esHint,
    esWarning,
    esError,
    esFatal
  );

  { TSourceRange }
  TSourceRange = record
    Filename: string;
    StartLine: Integer;
    StartColumn: Integer;
    EndLine: Integer;
    EndColumn: Integer;
    
    procedure Clear();
    function IsEmpty(): Boolean;
    function ToPointString(): string;
    function ToRangeString(): string;
  end;

  { TErrorRelated }
  TErrorRelated = record
    Range: TSourceRange;
    Message: string;
  end;

  { TError }
  TError = record
    Range: TSourceRange;
    Severity: TErrorSeverity;
    Code: string;
    Message: string;
    Related: TArray<TErrorRelated>;

    function GetSeverityString(): string;
    function ToIDEString(): string;
    function ToFullString(): string;
  end;

  { TErrors }
  TErrors = class(TBaseObject)
  private
    FItems: TList<TError>;
    FMaxErrors: Integer;

    function CountErrors(): Integer;

  public
    constructor Create(); override;
    destructor Destroy(); override;

    // Full location with range
    procedure Add(
      const ARange: TSourceRange;
      const ASeverity: TErrorSeverity;
      const ACode: string;
      const AMessage: string
    ); overload;

    procedure Add(
      const ARange: TSourceRange;
      const ASeverity: TErrorSeverity;
      const ACode: string;
      const AMessage: string;
      const AArgs: array of const
    ); overload;

    // Point location (start = end)
    procedure Add(
      const AFilename: string;
      const ALine: Integer;
      const AColumn: Integer;
      const ASeverity: TErrorSeverity;
      const ACode: string;
      const AMessage: string
    ); overload;

    procedure Add(
      const AFilename: string;
      const ALine: Integer;
      const AColumn: Integer;
      const ASeverity: TErrorSeverity;
      const ACode: string;
      const AMessage: string;
      const AArgs: array of const
    ); overload;

    // No location
    procedure Add(
      const ASeverity: TErrorSeverity;
      const ACode: string;
      const AMessage: string
    ); overload;

    procedure Add(
      const ASeverity: TErrorSeverity;
      const ACode: string;
      const AMessage: string;
      const AArgs: array of const
    ); overload;

    // Add related info to most recent error
    procedure AddRelated(
      const ARange: TSourceRange;
      const AMessage: string
    ); overload;

    procedure AddRelated(
      const ARange: TSourceRange;
      const AMessage: string;
      const AArgs: array of const
    ); overload;

    function HasHints(): Boolean;
    function HasWarnings(): Boolean;
    function HasErrors(): Boolean;
    function HasFatal(): Boolean;
    function Count(): Integer;
    function ErrorCount(): Integer;
    function WarningCount(): Integer;
    function ReachedMaxErrors(): Boolean;
    procedure Clear();

    property Items: TList<TError> read FItems;
    property MaxErrors: Integer read FMaxErrors write FMaxErrors;
  end;

implementation

{ TSourceRange }

procedure TSourceRange.Clear();
begin
  Filename := '';
  StartLine := 0;
  StartColumn := 0;
  EndLine := 0;
  EndColumn := 0;
end;

function TSourceRange.IsEmpty(): Boolean;
begin
  Result := (StartLine = 0) and (StartColumn = 0);
end;

function TSourceRange.ToPointString(): string;
begin
  if IsEmpty() then
    Result := ''
  else
    Result := Format('%s(%d,%d)', [Filename, StartLine, StartColumn]);
end;

function TSourceRange.ToRangeString(): string;
begin
  if IsEmpty() then
    Result := ''
  else if (StartLine = EndLine) and (StartColumn = EndColumn) then
    Result := Format('%s(%d,%d)', [Filename, StartLine, StartColumn])
  else if StartLine = EndLine then
    Result := Format('%s(%d,%d-%d)', [Filename, StartLine, StartColumn, EndColumn])
  else
    Result := Format('%s(%d,%d)-(%d,%d)', [Filename, StartLine, StartColumn, EndLine, EndColumn]);
end;

{ TError }

function TError.GetSeverityString(): string;
begin
  case Severity of
    esHint:    Result := RSSeverityHint;
    esWarning: Result := RSSeverityWarning;
    esError:   Result := RSSeverityError;
    esFatal:   Result := RSSeverityFatal;
  else
    Result := RSSeverityUnknown;
  end;
end;

function TError.ToIDEString(): string;
begin
  if Range.IsEmpty() then
    Result := Format(RSErrorFormatSimple, [GetSeverityString(), Code, Message])
  else
    Result := Format(RSErrorFormatWithLocation, [Range.ToPointString(), GetSeverityString(), Code, Message]);
end;

function TError.ToFullString(): string;
var
  LBuilder: TStringBuilder;
  LI: Integer;
begin
  LBuilder := TStringBuilder.Create();
  try
    LBuilder.AppendLine(ToIDEString());
    
    for LI := 0 to High(Related) do
    begin
      if Related[LI].Range.IsEmpty() then
        LBuilder.AppendFormat(RSErrorFormatRelatedSimple, [RSSeverityNote, Related[LI].Message])
      else
        LBuilder.AppendFormat(RSErrorFormatRelatedWithLocation, [Related[LI].Range.ToPointString(), RSSeverityNote, Related[LI].Message]);
      LBuilder.AppendLine();
    end;
    
    Result := LBuilder.ToString().TrimRight();
  finally
    LBuilder.Free();
  end;
end;

{ TErrors }

constructor TErrors.Create();
begin
  inherited;

  FItems := TList<TError>.Create();
  FMaxErrors := DEFAULT_MAX_ERRORS;
end;

destructor TErrors.Destroy();
begin
  FItems.Free();

  inherited;
end;

function TErrors.CountErrors(): Integer;
var
  LError: TError;
begin
  Result := 0;
  for LError in FItems do
  begin
    if LError.Severity in [esError, esFatal] then
      Inc(Result);
  end;
end;

procedure TErrors.Add(
  const ARange: TSourceRange;
  const ASeverity: TErrorSeverity;
  const ACode: string;
  const AMessage: string
);
var
  LError: TError;
  LRange: TSourceRange;
begin
  // Stop adding errors after limit reached (except fatal)
  if (ASeverity = esError) and (CountErrors() >= FMaxErrors) then
    Exit;

  // Normalize the filename to absolute path with forward slashes
  LRange := ARange;
  if LRange.Filename <> '' then
    LRange.Filename := TPath.GetFullPath(LRange.Filename).Replace('\', '/');

  LError.Range := LRange;
  LError.Severity := ASeverity;
  LError.Code := ACode;
  LError.Message := AMessage;
  SetLength(LError.Related, 0);

  FItems.Add(LError);
end;

procedure TErrors.Add(
  const ARange: TSourceRange;
  const ASeverity: TErrorSeverity;
  const ACode: string;
  const AMessage: string;
  const AArgs: array of const
);
begin
  Add(ARange, ASeverity, ACode, Format(AMessage, AArgs));
end;

procedure TErrors.Add(
  const AFilename: string;
  const ALine: Integer;
  const AColumn: Integer;
  const ASeverity: TErrorSeverity;
  const ACode: string;
  const AMessage: string
);
var
  LRange: TSourceRange;
begin
  LRange.Filename := AFilename;
  LRange.StartLine := ALine;
  LRange.StartColumn := AColumn;
  LRange.EndLine := ALine;
  LRange.EndColumn := AColumn;
  
  Add(LRange, ASeverity, ACode, AMessage);
end;

procedure TErrors.Add(
  const AFilename: string;
  const ALine: Integer;
  const AColumn: Integer;
  const ASeverity: TErrorSeverity;
  const ACode: string;
  const AMessage: string;
  const AArgs: array of const
);
begin
  Add(AFilename, ALine, AColumn, ASeverity, ACode, Format(AMessage, AArgs));
end;

procedure TErrors.Add(
  const ASeverity: TErrorSeverity;
  const ACode: string;
  const AMessage: string
);
var
  LRange: TSourceRange;
begin
  LRange.Clear();
  Add(LRange, ASeverity, ACode, AMessage);
end;

procedure TErrors.Add(
  const ASeverity: TErrorSeverity;
  const ACode: string;
  const AMessage: string;
  const AArgs: array of const
);
begin
  Add(ASeverity, ACode, Format(AMessage, AArgs));
end;

procedure TErrors.AddRelated(
  const ARange: TSourceRange;
  const AMessage: string
);
var
  LError: TError;
  LRelated: TErrorRelated;
  LLen: Integer;
begin
  if FItems.Count = 0 then
    Exit;
    
  LError := FItems[FItems.Count - 1];
  
  LRelated.Range := ARange;
  LRelated.Message := AMessage;
  
  LLen := Length(LError.Related);
  SetLength(LError.Related, LLen + 1);
  LError.Related[LLen] := LRelated;
  
  FItems[FItems.Count - 1] := LError;
end;

procedure TErrors.AddRelated(
  const ARange: TSourceRange;
  const AMessage: string;
  const AArgs: array of const
);
begin
  AddRelated(ARange, Format(AMessage, AArgs));
end;

function TErrors.HasHints(): Boolean;
var
  LError: TError;
begin
  Result := False;
  for LError in FItems do
  begin
    if LError.Severity = esHint then
      Exit(True);
  end;
end;

function TErrors.HasWarnings(): Boolean;
var
  LError: TError;
begin
  Result := False;
  for LError in FItems do
  begin
    if LError.Severity = esWarning then
      Exit(True);
  end;
end;

function TErrors.HasErrors(): Boolean;
var
  LError: TError;
begin
  Result := False;
  for LError in FItems do
  begin
    if LError.Severity in [esError, esFatal] then
      Exit(True);
  end;
end;

function TErrors.HasFatal(): Boolean;
var
  LError: TError;
begin
  Result := False;
  for LError in FItems do
  begin
    if LError.Severity = esFatal then
      Exit(True);
  end;
end;

function TErrors.Count(): Integer;
begin
  Result := FItems.Count;
end;

function TErrors.ErrorCount(): Integer;
begin
  Result := CountErrors();
end;

function TErrors.WarningCount(): Integer;
var
  LError: TError;
begin
  Result := 0;
  for LError in FItems do
  begin
    if LError.Severity = esWarning then
      Inc(Result);
  end;
end;

function TErrors.ReachedMaxErrors(): Boolean;
begin
  Result := CountErrors() >= FMaxErrors;
end;

procedure TErrors.Clear();
begin
  FItems.Clear();
end;

end.
