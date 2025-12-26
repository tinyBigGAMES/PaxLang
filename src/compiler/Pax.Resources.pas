{===============================================================================
  Pax™ Programming Language.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://paxlang.org

  See LICENSE for license information
===============================================================================}


unit Pax.Resources;

{$I Pax.Defines.inc}

interface

resourcestring

  //--------------------------------------------------------------------------
  // Severity Names
  //--------------------------------------------------------------------------
  RSSeverityHint = 'Hint';
  RSSeverityWarning = 'Warning';
  RSSeverityError = 'Error';
  RSSeverityFatal = 'Fatal';
  RSSeverityNote = 'Note';
  RSSeverityUnknown = 'Unknown';

  //--------------------------------------------------------------------------
  // Error Format Strings
  //--------------------------------------------------------------------------
  RSErrorFormatSimple = '%s %s: %s';
  RSErrorFormatWithLocation = '%s: %s %s: %s';
  RSErrorFormatRelatedSimple = '  %s: %s';
  RSErrorFormatRelatedWithLocation = '  %s: %s: %s';

  //--------------------------------------------------------------------------
  // Syntax Errors - Lexer (E001-E049)
  //--------------------------------------------------------------------------
  RSLexerUnexpectedChar = 'Unexpected character: ''%s''';
  RSLexerUnterminatedString = 'Unterminated string literal';
  RSLexerUnterminatedChar = 'Unterminated character literal';
  RSLexerInvalidNumber = 'Invalid number format: %s';
  RSLexerInvalidEscape = 'Invalid escape sequence: \%s';
  RSLexerUnterminatedComment = 'Unterminated comment';
  RSLexerInvalidDirective = 'Invalid directive: %s';
  RSLexerCharLiteralNotSupported = 'Character literal #NNN syntax not supported. Use escape sequences like \n or \xNN inside strings.';
  RSLexerInvalidHexEscape = 'Invalid hex escape sequence';

  //--------------------------------------------------------------------------
  // Syntax Errors - Parser (E050-E099)
  //--------------------------------------------------------------------------
  RSParserUnexpectedToken = 'Unexpected token: %s';
  RSParserExpectedToken = 'Expected %s but found %s';
  RSParserExpectedIdentifier = 'Expected identifier';
  RSParserExpectedIdentifierAfterDot = 'Expected identifier after ''.''';
  RSParserExpectedExpression = 'Expected expression';
  RSParserExpectedStatement = 'Expected statement';
  RSParserExpectedDeclaration = 'Expected ''const'', ''type'', ''var'', or ''routine'' but found %s';
  RSParserExpectedTypeRef = 'Expected type reference';
  RSParserExpectedToOrDownto = 'Expected ''to'' or ''downto''';
  RSParserExpectedSemicolon = 'Expected '';''';
  RSParserExpectedColon = 'Expected '':''';
  RSParserExpectedEnd = 'Expected ''end''';
  RSParserExpectedBegin = 'Expected ''begin''';
  RSParserExpectedThen = 'Expected ''then''';
  RSParserExpectedDo = 'Expected ''do''';
  RSParserExpectedOf = 'Expected ''of''';
  RSParserExpectedAssign = 'Expected '':=''';
  RSParserExpectedRParen = 'Expected '')''';
  RSParserExpectedRBracket = 'Expected '']''';
  RSParserExpectedModuleKind = 'Expected module kind: exe, dll, or lib';
  RSParserMissingModuleEnd = 'Missing ''end.'' at end of module';
  RSParserInvalidArrayBounds = 'Invalid array bounds';
  RSParserNegativeArrayBounds = 'Negative array bounds are not supported';
  RSParserExpectedConsoleOrGui = 'Expected ''console'' or ''gui''';
  RSParserExpectedOnOrOff = 'Expected ''on'' or ''off''';
  RSParserExpectedYesOrNo = 'Expected ''yes'' or ''no''';
  RSParserExpectedString = 'Expected string';
  RSParserExpectedTestName = 'Expected test name string';
  RSParserExpectedSetRange = 'Expected integer for set range';
  RSParserExpectedInteger = 'Expected integer';
  RSParserAlignmentExceedsMax = 'Alignment %d exceeds maximum supported alignment of %d bytes';
  RSParserLexerNotSet = 'Lexer not set';
  RSParserNoTokens = 'No tokens to parse';
  RSParserSetLiteralBrackets = 'Set literals use braces {}, not brackets []';

  //--------------------------------------------------------------------------
  // Type Errors (E100-E149)
  //--------------------------------------------------------------------------
  RSTypeUnknown = 'Unknown type: ''%s''';
  RSTypeMismatch = 'Type mismatch: cannot assign ''%s'' to ''%s''';
  RSTypeMismatchBinary = 'Type mismatch: cannot apply ''%s'' to ''%s'' and ''%s''';
  RSTypeMismatchUnary = 'Type mismatch: cannot apply ''%s'' to ''%s''';
  RSTypeNotOrdinal = 'Expected ordinal type but found ''%s''';
  RSTypeNotBoolean = 'Expected boolean type but found ''%s''';
  RSTypeNotInteger = 'Expected integer type but found ''%s''';
  RSTypeNotNumeric = 'Expected numeric type but found ''%s''';
  RSTypeNotPointer = 'Expected pointer type but found ''%s''';
  RSTypeNotArray = 'Expected array type but found ''%s''';
  RSTypeNotRecord = 'Expected record type but found ''%s''';
  RSTypeNotSet = 'Expected set type but found ''%s''';
  RSTypeNotRoutine = 'Expected routine type but found ''%s''';
  RSTypeCannotCast = 'Cannot cast ''%s'' to ''%s''';
  RSTypeCannotDeref = 'Cannot dereference non-pointer type ''%s''';
  RSTypeCannotIndex = 'Cannot index non-array type ''%s''';
  RSTypeCannotCall = 'Cannot call non-routine ''%s''';
  RSTypeIncompatibleReturn = 'Return type mismatch: expected ''%s'' but found ''%s''';
  RSTypeReturnInVoid = 'Cannot return a value from a procedure';
  RSTypeMissingReturn = 'Function must return a value';

  //--------------------------------------------------------------------------
  // Name Resolution Errors (E150-E199)
  //--------------------------------------------------------------------------
  RSNameUndeclared = 'Undeclared identifier: ''%s''';
  RSNameDuplicate = 'Duplicate identifier: ''%s''';
  RSNameUnknownField = 'Unknown field ''%s'' in type ''%s''';
  RSNameUnknownModule = 'Unknown module: ''%s''';
  RSNameNotVisible = '''%s'' is not visible in this scope';
  RSNameDidYouMean = 'Did you mean ''%s''?';
  RSNameFirstDeclaredHere = 'First declared here';
  RSNameImportedFrom = 'Imported from ''%s''';

  //--------------------------------------------------------------------------
  // Semantic Errors (E200-E299)
  //--------------------------------------------------------------------------
  RSSemanticArgCountMismatch = 'Expected %d argument(s) but found %d';
  RSSemanticArgTypeMismatch = 'Argument %d: expected ''%s'' but found ''%s''';
  RSSemanticConditionNotBoolean = 'Condition must be boolean';
  RSSemanticForVarNotInteger = 'For loop variable must be an integer type';
  RSSemanticForBoundsNotInteger = 'For loop bounds must be integer';
  RSSemanticCaseSelectorNotOrdinal = 'Case selector must be an ordinal type';
  RSSemanticCaseLabelNotOrdinal = 'Case label must be an ordinal type';
  RSSemanticCaseLabelDuplicate = 'Duplicate case label: %s';
  RSSemanticIndexNotInteger = 'Array index must be an integer';
  RSSemanticIndexOutOfBounds = 'Array index out of bounds: %d (range %d..%d)';
  RSSemanticNewNonPointer = '''new'' requires a pointer type';
  RSSemanticDisposeNonPointer = '''dispose'' requires a pointer type';
  RSSemanticSetLengthNonArray = '''setlength'' requires a dynamic array';
  RSSemanticSetLengthNonInteger = '''setlength'' length must be an integer';
  RSSemanticAssignToConst = 'Cannot assign to constant ''%s''';
  RSSemanticAssignToReadOnly = 'Cannot assign to read-only ''%s''';
  RSSemanticInNotOrdinal = '''in'' operator requires ordinal type on left side';
  RSSemanticInNotSet = '''in'' operator requires set type on right side';
  RSSemanticBreakOutsideLoop = '''break'' outside of loop';
  RSSemanticContinueOutsideLoop = '''continue'' outside of loop';
  RSSemanticUnreachableCode = 'Unreachable code';
  RSSemanticExternalWithBody = 'External routine cannot have a body';
  RSSemanticExternalMissingLib = 'External routine must specify library';
  RSSemanticParamStrNotInteger = 'ParamStr index must be an integer';
  RSSemanticDllInitBlock = 'DLL and library modules cannot have initialization blocks';
  RSSemanticBitFieldWidth = 'Bit field width %d exceeds maximum %d bits for type ''%s''';
  RSSemanticBitFieldPositive = 'Bit field width must be positive';
  RSSemanticBitFieldType = 'Bit fields require integer or boolean type, found ''%s''';
  RSSemanticFlexibleArrayLast = 'Flexible array member must be the last field in a record';
  RSSemanticFlexibleArrayUnion = 'Flexible array members are not allowed in unions';
  RSSemanticFlexibleArrayOnlyField = 'Flexible array member cannot be the only field in a record';

  //--------------------------------------------------------------------------
  // Warnings (W001-W099)
  //--------------------------------------------------------------------------
  RSWarnUnusedVariable = 'Unused variable: ''%s''';
  RSWarnUnusedParameter = 'Unused parameter: ''%s''';
  RSWarnUnusedRoutine = 'Unused routine: ''%s''';
  RSWarnUnusedType = 'Unused type: ''%s''';
  RSWarnUnusedConst = 'Unused constant: ''%s''';
  RSWarnUnreachableCode = 'Unreachable code';
  RSWarnDeprecated = '''%s'' is deprecated';
  RSWarnDeprecatedWithMsg = '''%s'' is deprecated: %s';
  RSWarnNarrowingConversion = 'Narrowing conversion from ''%s'' to ''%s''';
  RSWarnPossibleOverflow = 'Possible overflow in conversion';
  RSWarnUninitializedVar = 'Variable ''%s'' might not be initialized';
  RSWarnShadowedVar = 'Variable ''%s'' shadows outer declaration';
  RSWarnEmptyBlock = 'Empty statement block';

  //--------------------------------------------------------------------------
  // Hints (H001-H099)
  //--------------------------------------------------------------------------
  RSHintImplicitConversion = 'Implicit conversion from ''%s'' to ''%s''';
  RSHintRedundantCast = 'Redundant type cast';
  RSHintRedundantParens = 'Redundant parentheses';

  //--------------------------------------------------------------------------
  // I/O and Fatal Errors (E900-E999)
  //--------------------------------------------------------------------------
  RSFatalFileNotFound = 'File not found: ''%s''';
  RSFatalFileReadError = 'Cannot read file ''%s'': %s';
  RSFatalFileWriteError = 'Cannot write file ''%s'': %s';
  RSFatalModuleNotFound = 'Module not found: ''%s''';
  RSFatalCircularImport = 'Circular import detected: %s';
  RSFatalTooManyErrors = 'Too many errors, compilation stopped';
  RSFatalInternalError = 'Internal compiler error: %s';
  RSFatalOutOfMemory = 'Out of memory';

  //--------------------------------------------------------------------------
  // Compiler Messages
  //--------------------------------------------------------------------------
  RSCompilerVersion = 'Pax Compiler %s';
  RSCompilerCompiling = 'Compiling: %s';
  RSCompilerSuccess = 'Compilation successful';
  RSCompilerFailed = 'Compilation failed with %d error(s)';
  RSCompilerWarnings = '%d warning(s)';
  RSCompilerErrorsAndWarnings = '%d error(s), %d warning(s)';

implementation

end.
