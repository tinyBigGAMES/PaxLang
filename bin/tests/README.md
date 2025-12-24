# Pax Compiler Test Suite

This directory contains test files for the Pax compiler.

## Test Files

| File | Description | Tests |
|------|-------------|-------|
| `hello.pax` | Minimal program | Empty module compiles |
| `expressions.pax` | Arithmetic & logic | +, -, *, /, div, mod, and, or, not, comparisons |
| `control_flow.pax` | Control statements | if/else, while, for, repeat, case |
| `records.pax` | Record types | Field access, nested records, extended records |
| `arrays.pax` | Array types | Static arrays, dynamic arrays, setlength, len |
| `pointers.pax` | Pointer operations | new, dispose, dereference, nil |
| `routines.pax` | Functions/procedures | Parameters, locals, return, recursion, var params |
| `sets.pax` | Set types | Set literals, ranges, in operator |
| `types.pax` | Type system | Casting, sizeof, built-in types |
| `constants.pax` | Constants | Typed/untyped constants, hex literals |
| `mathlib.pax` | Library module | Public routines, module exports |
| `imports.pax` | Module imports | Import statement, qualified names |

## Running Tests

### From Delphi Code

```delphi
var
  LCompiler: TPaxCompiler;
  LTestFile: string;
begin
  LCompiler := TPaxCompiler.Create();
  try
    LTestFile := 'tests\hello.pax';
    
    // Compile to C
    if LCompiler.CompileFile(LTestFile) then
    begin
      WriteLn('OK: ', LTestFile);
      
      // Optionally build executable
      LCompiler.BuildExecutable('tests\hello.exe');
    end
    else
    begin
      WriteLn('FAIL: ', LTestFile);
      WriteLn(LCompiler.Errors.Items[0].ToFullString());
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
    
    // Compile the main module (will auto-load mathlib)
    if LCompiler.CompileFile('tests\imports.pax') then
    begin
      WriteLn('OK: imports.pax');
      LCompiler.BuildExecutable('tests\imports.exe');
    end
    else
    begin
      WriteLn('FAIL: imports.pax');
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

## Adding New Tests

1. Create a new `.pax` file in this directory
2. Use `module exe testname;` for standalone tests
3. Use `module lib testname;` for library modules
4. Add to the table above
5. Test with the compiler

## Notes

- Most tests are self-contained (no imports)
- Each test focuses on a specific language feature
- Complex combinations are avoided to isolate failures
- Module tests (`mathlib.pax`, `imports.pax`) demonstrate the import system
