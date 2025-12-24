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
    //LCompiler.MainSourceFile := 'tests\hello.pax';
    //LCompiler.MainSourceFile := 'tests\imports.pax';
    //LCompiler.MainSourceFile := 'tests\emoji_test.pax';
    //LCompiler.MainSourceFile := 'tests\test_dll_strings.pax';
    //LCompiler.MainSourceFile := 'tests\test_dll_strings_exe.pax';
    //LCompiler.MainSourceFile := 'tests\test_lib_math.pax';
    //LCompiler.MainSourceFile := 'tests\test_lib_math_exe.pax';
    //LCompiler.MainSourceFile := 'tests\test_unions.pax';
    //LCompiler.MainSourceFile := 'tests\test_packed_records.pax';
    //LCompiler.MainSourceFile := 'tests\test_anonymous.pax';
    //LCompiler.MainSourceFile := 'tests\test_aligned.pax';
    //LCompiler.MainSourceFile := 'tests\test_aligned_error.pax';
    //LCompiler.MainSourceFile := 'tests\test_exe_bitfields.pax';
    //LCompiler.MainSourceFile := 'tests\test_exe_record_arrays.pax';
    //LCompiler.MainSourceFile := 'tests\test_exe_arrays.pax';

    //LCompiler.MainSourceFile := 'tests\test_exe_unittest.pax';
    //LCompiler.MainSourceFile := 'tests\test_lib_mathlib.pax';
    LCompiler.MainSourceFile := 'tests\test_lib_mathlib_exe.pax';

    LCompiler.Build();
  finally
    ShowErrors(LCompiler.Errors);
    LCompiler.Free();
  end;
end;

procedure RunTestbed();
begin
  try
    Test01();
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
