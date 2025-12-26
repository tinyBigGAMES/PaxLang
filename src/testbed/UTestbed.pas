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
  System.TypInfo,
  System.SysUtils,
  System.IOUtils,
  Pax.Utils,
  Pax.LibTCC,
  Pax.Errors,
  Pax.Compiler;


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

procedure Test01();
var
  LCompiler: TPaxCompiler;
begin
  LCompiler := TPaxCompiler.Create();
  try
    LCompiler.AddModuleSearchPath('tests');

    LCompiler.Verbose := True;
    LCompiler.MaxErrors := 1;
    LCompiler.OutputPath := 'output';

    // =========================================================================
    // STANDALONE EXE TESTS (no dependencies, any order)
    // =========================================================================
    //LCompiler.MainSourceFile := 'tests\test_exe_hello.pax';
    //LCompiler.MainSourceFile := 'tests\test_exe_expressions.pax';
    //LCompiler.MainSourceFile := 'tests\test_exe_control_flow.pax';
    //LCompiler.MainSourceFile := 'tests\test_exe_case.pax';
    //LCompiler.MainSourceFile := 'tests\test_exe_records.pax';
    //LCompiler.MainSourceFile := 'tests\test_exe_record_arrays.pax';
    //LCompiler.MainSourceFile := 'tests\test_exe_packed_records.pax';
    //LCompiler.MainSourceFile := 'tests\test_exe_unions.pax';
    //LCompiler.MainSourceFile := 'tests\test_exe_anonymous.pax';
    //LCompiler.MainSourceFile := 'tests\test_exe_aligned.pax';
    //LCompiler.MainSourceFile := 'tests\test_exe_bitfields.pax';
    //LCompiler.MainSourceFile := 'tests\test_exe_arrays.pax';
    //LCompiler.MainSourceFile := 'tests\test_exe_pointers.pax';
    //LCompiler.MainSourceFile := 'tests\test_exe_routines.pax';
    //LCompiler.MainSourceFile := 'tests\test_exe_sets.pax';
    //LCompiler.MainSourceFile := 'tests\test_exe_types.pax';
    //LCompiler.MainSourceFile := 'tests\test_exe_constants.pax';
    //LCompiler.MainSourceFile := 'tests\test_exe_emoji.pax';
    //LCompiler.MainSourceFile := 'tests\test_exe_unittest.pax';
    LCompiler.MainSourceFile := 'tests\test_exe_verinfo.pax';

    // =========================================================================
    // ERROR TESTS
    // =========================================================================
    //LCompiler.MainSourceFile := 'tests\test_exe_aligned_error.pax';
    //LCompiler.MainSourceFile := 'tests\test_exe_badtest.pax';

    // =========================================================================
    // LIB TESTS (lib auto-imports when EXE is compiled)
    // =========================================================================
    //LCompiler.MainSourceFile := 'tests\test_exe_imports.pax';
    //LCompiler.MainSourceFile := 'tests\test_lib_math_exe.pax';
    //LCompiler.MainSourceFile := 'tests\test_lib_mathlib_exe.pax';

    // =========================================================================
    // DLL TESTS (must build DLL first, then EXE)
    // =========================================================================
    // Step 1: Build DLL
    //LCompiler.MainSourceFile := 'tests\test_dll_strings.pax';
    // Step 2: Build EXE that uses DLL
    //LCompiler.MainSourceFile := 'tests\test_dll_strings_exe.pax';

    LCompiler.Build();
  finally
    ShowErrors(LCompiler.Errors);
    LCompiler.Free();
  end;
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
  LNum := 28;
  //LNum := 1;

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

    // ERROR TESTS
    31: TestFile('test_exe_aligned_error');
    32: TestFile('test_exe_badtest');

    // LIB TESTS (lib auto-compiles when EXE imports it)
    33: TestFile('test_exe_imports');
    34: TestFile('test_lib_math');
    35: TestFile('test_lib_math_exe');
    36: TestFile('test_exe_unittest');

    // DLL TESTS (must build DLL first, then EXE)
    37: TestFile('test_dll_strings');      // Step 1: Build DLL
    38: TestFile('test_dll_strings_exe');  // Step 2: Build EXE
  end;
end;

procedure RunTestbed();
begin
  try
    TestFiles();
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
