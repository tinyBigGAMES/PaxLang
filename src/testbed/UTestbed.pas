{===============================================================================
  Pax™ Programming Language.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://paxlang.org

  See LICENSE for license information
===============================================================================}

unit UTestbed;

interface

procedure RunTestbed();

implementation

uses
  WinApi.Windows,
  System.TypInfo,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  Pax.Utils,
  Pax.LibTCC,
  Pax.Errors,
  Pax.Compiler,
  Pax.CImporter;


procedure ShowErrors(const AErrors: TErrors);
var
  LError: TError;
begin
  WriteLn;
  if AErrors.Count() = 0 then Exit;

  WriteLn('Captured ', AErrors.Count(), ' error(s):');
  for LError in AErrors.Items do
  begin
    WriteLn('  File: ', LError.Range.Filename);
    WriteLn('  Line: ', LError.Range.StartLine);
    WriteLn('   Col: ', LError.Range.StartColumn);
    WriteLn('  Type: ', LError.GetSeverityString());
    WriteLn('  Code: ', LError.Code);
    WriteLn('  Msg:  ', LError.Message);
    WriteLn;
  end;
end;

procedure ImportRaylib();
var
  LImporter: TPaxCImporter;
begin
  TUtils.PrintLn('=== C Header to Pax Module Converter ===');
  TUtils.PrintLn('');

  LImporter := TPaxCImporter.Create();
  try
    LImporter.AddExcludedType('__builtin_va_list');
    LImporter.AddExcludedType('va_list');
    LImporter.AddExcludedType('__gnuc_va_list');

    LImporter.AddLibraryPath('libs\raylib\bin');

    LImporter.AddCopyDLL('libs\raylib\bin\raylib.dll');

    LImporter.InsertFileBefore('(* External routines *)', 'libs\raylib\src\raylib_colors.txt');

    // Optional settings - if not set, defaults are used:
    // - ModuleName: derived from header filename
    // - DllName: modulename.dll
    // - OutputPath: same folder as header
    LImporter.SetDllName('raylib.dll');
    LImporter.SetOutputPath('libs\raylib\src');

    TUtils.PrintLn('Importing raylib.h -> raylib.pax');

    if LImporter.ImportHeader('libs\raylib\include\raylib.h') then
      TUtils.PrintLn('SUCCESS')
    else
      TUtils.PrintLn('ERROR: %s', [LImporter.GetLastError()]);

  finally
    LImporter.Free();
  end;

  TUtils.PrintLn('');
  TUtils.PrintLn('=== Done ===');
end;

procedure TestFile(const ABaseFilename: string);
var
  LCompiler: TPaxCompiler;
  LFilename: string;
begin
  LFilename := TPath.GetFileNameWithoutExtension(ABaseFilename) + '.pax';
  LFilename := TPath.Combine('tests', LFilename);

  LCompiler := TPaxCompiler.Create();
  try

    LCompiler.AddModuleSearchPath('tests');

    LCompiler.Verbose := True;
    LCompiler.MaxErrors := 1;
    LCompiler.OutputPath := 'output';

    LCompiler.MainSourceFile := LFilename;
    LCompiler.Build();
  finally
    ShowErrors(LCompiler.Errors);
    LCompiler.Free();
  end;
end;

procedure TestFiles();
var
  LNum: Integer;
begin
  LNum := 41;

  case LNum of
    // STANDALONE EXE TESTS (no dependencies, any order)
    01: TestFile('test_exe_hello');
    02: TestFile('test_exe_expressions');
    03: TestFile('test_exe_control_flow');
    04: TestFile('test_exe_case');
    05: TestFile('test_exe_records');
    06: TestFile('test_exe_record_arrays');
    07: TestFile('test_exe_packed_records');
    08: TestFile('test_exe_unions');
    09: TestFile('test_exe_anonymous');
    10: TestFile('test_exe_aligned');
    11: TestFile('test_exe_bitfields');
    12: TestFile('test_exe_arrays');
    13: TestFile('test_exe_pointers');
    14: TestFile('test_exe_routines');
    15: TestFile('test_exe_sets');
    16: TestFile('test_exe_types');
    17: TestFile('test_exe_constants');
    18: TestFile('test_exe_emoji');
    19: TestFile('test_exe_unittest');
    20: TestFile('test_exe_verinfo');
    21: TestFile('test_exe_routine_types');
    22: TestFile('test_exe_cmdline');
    23: TestFile('test_exe_conditionals');
    24: TestFile('test_exe_raw_wide');
    25: TestFile('test_exe_set_types');
    26: TestFile('test_exe_compound_assign');
    27: TestFile('test_exe_literals');
    28: TestFile('test_exe_directives');
    29: TestFile('test_exe_type_alias');
    30: TestFile('test_exe_edge_cases');
    31: TestFile('test_exe_gui');
    32: TestFile('test_exe_memory');

    // ERROR TESTS
    33: TestFile('test_exe_aligned_error');
    34: TestFile('test_exe_badtest');

    // LIB TESTS (lib auto-compiles when EXE imports it)
    35: TestFile('test_exe_imports');
    36: TestFile('test_lib_math');
    37: TestFile('test_lib_math_exe');
    38: TestFile('test_exe_unittest');

    // DLL TESTS (must build DLL first, then EXE)
    39: TestFile('test_dll_strings');      // Step 1: Build DLL
    40: TestFile('test_dll_strings_exe');  // Step 2: Build EXE

    // Library Tests
    41: TestFile('test_exe_raylib');
  end;
end;

procedure RunTestbed();
begin
  try
    TestFiles();
    //Test01();
    //Test02();
  except
    on E: Exception do
    begin
      TUtils.PrintLn('');
      TUtils.PrintLn(COLOR_RED + '  [EXCEPTION] ' + COLOR_RESET + E.ClassName);
      TUtils.PrintLn('  ' + E.Message);
    end;
  end;

  TUtils.Pause();
end;

end.
