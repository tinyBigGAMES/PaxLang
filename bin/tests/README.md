# Pax Compiler Test Suite

This directory contains test files for the Pax compiler.

## Test Files

### Executable Tests (EXE)

| File | Description |
|------|-------------|
| `test_exe_hello.pax` | Minimal program - printf, string basics |
| `test_exe_expressions.pax` | Arithmetic & logic operators |
| `test_exe_control_flow.pax` | if/else, while, for, repeat, case |
| `test_exe_case.pax` | Case statement variations |
| `test_exe_records.pax` | Record types, field access, nested records |
| `test_exe_record_arrays.pax` | Arrays of records |
| `test_exe_packed_records.pax` | C99 pragma pack, byte-aligned structs |
| `test_exe_unions.pax` | C99 unions, overlapping fields |
| `test_exe_anonymous.pax` | Anonymous records and unions |
| `test_exe_aligned.pax` | Custom alignment directives |
| `test_exe_aligned_error.pax` | Alignment error handling |
| `test_exe_bitfields.pax` | C99 bit field support |
| `test_exe_arrays.pax` | Static arrays, dynamic arrays, setlength, len |
| `test_exe_pointers.pax` | new, dispose, dereference, nil |
| `test_exe_routines.pax` | Functions, procedures, parameters, var params |
| `test_exe_sets.pax` | Set literals, ranges, in operator |
| `test_exe_types.pax` | Casting, sizeof, built-in types |
| `test_exe_constants.pax` | Typed/untyped constants, hex literals |
| `test_exe_imports.pax` | Module imports, qualified names |
| `test_exe_emoji.pax` | UTF-8/UTF-16 Unicode support |
| `test_exe_unittest.pax` | Unit testing framework with assertions |
| `test_exe_verinfo.pax` | Version info, icon, manifest directives |
| `test_exe_badtest.pax` | Error handling test case |

### Library Tests (LIB)

| File | Description |
|------|-------------|
| `test_lib_math.pax` | Math library module |
| `test_lib_math_exe.pax` | Test runner for math library |
| `test_lib_mathlib.pax` | Math library with unit tests |
| `test_lib_mathlib_exe.pax` | Test runner with transitive test collection |
| `test_lib_imports.pax` | Library import test module |

### DLL Tests

| File | Description |
|------|-------------|
| `test_dll_strings.pax` | DLL with string exports |
| `test_dll_strings_exe.pax` | EXE that loads string DLL |

## Running Tests

### From Delphi Code

```delphi
var
  LCompiler: TPaxCompiler;
begin
  LCompiler := TPaxCompiler.Create();
  try
    // Set the source file
    LCompiler.MainSourceFile := 'tests\test_exe_hello.pax';
    
    // Optional: enable verbose output
    LCompiler.Verbose := True;
    
    // Build and run
    if LCompiler.Build(True) then
      WriteLn('OK: Build successful')
    else
    begin
      WriteLn('FAIL: Build failed');
      for var LErr in LCompiler.Errors.Items do
        WriteLn(LErr.ToFullString());
    end;
  finally
    LCompiler.Free();
  end;
end;
```

### Testing Module Imports

```delphi
var
  LCompiler: TPaxCompiler;
begin
  LCompiler := TPaxCompiler.Create();
  try
    // Add search path for modules
    LCompiler.AddModuleSearchPath('tests');
    
    // Set the main source file
    LCompiler.MainSourceFile := 'tests\test_exe_imports.pax';
    
    // Build without auto-run
    if LCompiler.Build(False) then
    begin
      WriteLn('OK: ', LCompiler.OutputFilename);
      // Manually run if needed
      LCompiler.Run();
    end
    else
    begin
      WriteLn('FAIL: Build failed');
      for var LErr in LCompiler.Errors.Items do
        WriteLn(LErr.ToFullString());
    end;
  finally
    LCompiler.Free();
  end;
end;
```

### Compile Only (No Build)

```delphi
var
  LCompiler: TPaxCompiler;
begin
  LCompiler := TPaxCompiler.Create();
  try
    // Just compile/check without building executable
    if LCompiler.CompileFile('tests\test_exe_hello.pax') then
      WriteLn('Compile OK: ', LCompiler.ModuleName)
    else
    begin
      WriteLn('Compile FAIL');
      for var LErr in LCompiler.Errors.Items do
        WriteLn(LErr.ToFullString());
    end;
  finally
    LCompiler.Free();
  end;
end;
```

### Expected Behavior

All test files should:
1. Compile without errors (lexer, parser, checker pass)
2. Generate valid C code
3. Compile with TCC to executable
4. Run without crashing

## Key Features Tested

### Version Info & Resources (`test_exe_verinfo.pax`)
```pax
#addverinfo yes
#vimajor 1
#viminor 0
#vipatch 0
#viproductname 'My App'
#videscription 'Application description'
#vifilename 'myapp.exe'
#vicompanyname 'My Company'
#vicopyright 'Copyright © 2025'
#exeicon @'..\res\app.ico'
```

### Unit Testing (`test_exe_unittest.pax`)
```pax
#unittestmode on

test 'My test name'
begin
  TestAssertEqualInt(5, Add(2, 3));
  TestAssertTrue(IsValid());
end;
```

### Raw String Literals
```pax
// Use @'...' for paths to avoid escape processing
#exeicon @'..\res\icon.ico'

// Regular strings process escapes
printf('Hello\nWorld');
```

## Module System

### Creating a Library Module

```pax
module lib mylib;

// Public routine - exported
public
routine Add(const a: int32; const b: int32): int32;
begin
  return a + b;
end;

// Private routine - not exported
routine Helper(): int32;
begin
  return 42;
end;

end.
```

### Importing a Module

```pax
module exe main;

import mylib;

var
  result: int32;

begin
  // Use qualified name: modulename.routinename
  result := mylib.Add(10, 20);
end.
```

### Module Search Paths

The compiler searches for imported modules in:
1. Paths added via `AddModuleSearchPath()`
2. The directory containing the source file
3. Current working directory

## File Naming Convention

- `test_exe_*.pax` - Standalone executable tests
- `test_lib_*.pax` - Library module tests
- `test_lib_*_exe.pax` - Executable that uses a library
- `test_dll_*.pax` - DLL module tests
- `test_dll_*_exe.pax` - Executable that uses a DLL

## Adding New Tests

1. Create a new `.pax` file following the naming convention
2. Use `module exe testname;` for standalone tests
3. Use `module lib testname;` for library modules
4. Use `module dll testname;` for DLL modules
5. Add to the appropriate table above
6. Test with the compiler

## Notes

- Most tests are self-contained (no imports)
- Each test focuses on a specific language feature
- Complex combinations are avoided to isolate failures
- Module tests demonstrate the import system and cross-module features
