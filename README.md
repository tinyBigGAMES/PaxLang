<div align="center">

![Pax](media/logo.png)

[![Discord](https://img.shields.io/discord/754884471324672040?style=for-the-badge&logo=discord&label=Discord)](https://discord.gg/tPWjMwK)
[![Follow on Bluesky](https://img.shields.io/badge/Bluesky-tinyBigGAMES-blue?style=for-the-badge&logo=bluesky)](https://bsky.app/profile/tinybiggames.com)

A minimal systems programming language that compiles to native executables via C99.

</div>

## What is Pax?

The **Pax Programming Language** is a Pascal/Oberon-inspired systems programming language targeting Win64. It compiles to C via [TinyCC](https://github.com/TinyCC/tinycc) with a completely self-contained toolchain - no external dependencies required. Memory is automatically managed via the [Boehm-Demers-Weiser Garbage Collector](https://github.com/bdwgc/bdwgc).

```pax
module exe HelloWorld;

routine printf(const fmt: pointer to char; ...): int32; external 'msvcrt.dll';

var
  name: string;

begin
  name := 'Pax';
  printf('Hello from %s!\n', pointer to char(name));
  printf('GC heap: %lld bytes\n', gc_heapsize());
end.
```

## Key Features

- **Minimal by design** - Clean syntax, no redundancy, just what you need
- **Pascal heritage** - Readable, structured code inspired by Pascal and Oberon
- **Automatic memory** - Boehm GC handles allocation and cleanup
- **Self-contained** - Embedded TinyCC toolchain, single executable distribution
- **Windows native** - Call any DLL directly, full Windows API access
- **Multiple outputs** - Build executables, DLLs, or static libraries
- **Type extension** - Record inheritance without class complexity
- **Union types** - C-compatible unions with anonymous nesting
- **Dynamic arrays** - setlength/len with automatic memory management
- **Managed strings** - UTF-8 and UTF-16 string types with emoji support
- **Varargs support** - Full C interop with printf-style functions
- **Built-in testing** - Integrated unit test framework
- **Version info** - Embed metadata and icons in executables
- **C header import** - Convert C headers to Pax modules automatically

## Language Overview

### Built-in Types

| Type | Size | Description |
|------|------|-------------|
| int8, int16, int32, int64 | 1-8 bytes | Signed integers |
| uint8, uint16, uint32, uint64 | 1-8 bytes | Unsigned integers |
| float32, float64 | 4-8 bytes | Floating point |
| boolean | 1 byte | true / false |
| char, uchar | 1 byte | Signed/unsigned characters |
| wchar, uwchar | 2 bytes | Wide characters (UTF-16) |
| string, wstring | 8 bytes | Managed strings (UTF-8 / UTF-16) |
| pointer, pointer to T | 8 bytes | Untyped / typed pointers |

### Module Types

| Type | Description |
|------|-------------|
| module exe Name | Executable program |
| module lib Name | Static library (.a) |
| module dll Name | Dynamic library (.dll) |

### Operators

#### Arithmetic and Comparison
```pax
+ - * /           // Arithmetic
div mod           // Integer division and modulo
= <> < > <= >=    // Comparison
and or not        // Logical
in                // Set membership
```

#### Compound Assignment
```pax
var
  n: int32;
  s: string;
  days: set of 0..6;

begin
  n := 10;
  n += 5;         // n = n + 5
  n -= 3;         // n = n - 3
  n *= 2;         // n = n * 2
  n /= 4;         // n = n / 4 (integer division)
  
  s := 'Hello';
  s += ' World';  // String concatenation
  
  days := {1, 2};
  days += {3};    // Set union
  days -= {1};    // Set difference
end.
```

### Type Aliases

```pax
type
  THandle = uint64;
  TSize = int64;
  TFileHandle = THandle;  // Alias of alias
  
var
  h: THandle;
  fh: TFileHandle;

begin
  h := 12345;
  fh := h;        // Compatible assignment
end.
```

### Records and Extension

```pax
type
  TPoint = record
    x: int32;
    y: int32;
  end;

  TColorPoint = record(TPoint)  // Inherits from TPoint
    color: uint32;
  end;

var
  p: TColorPoint;

begin
  p.x := 100;         // Inherited from TPoint
  p.y := 200;         // Inherited from TPoint
  p.color := $FF0000;
end.
```

### Packed Records and Alignment

```pax
type
  // Packed record - no padding between fields
  THeader = record packed
    magic: uint16;
    version: uint8;
    flags: uint8;
  end;

  // Explicit alignment for SIMD or cache optimization
  TAlignedData = record align(16)
    values: array[0..3] of float32;
  end;

  // Bit fields for compact storage
  TFlags = record packed
    enabled: uint8 : 1;   // 1 bit
    priority: uint8 : 3;  // 3 bits
    reserved: uint8 : 4;  // 4 bits
  end;
```

### Union Types

```pax
type
  // All fields share the same memory location
  TValue = union
    asInt: int64;
    asFloat: float64;
    asPtr: pointer;
  end;

  // Record with anonymous union (C-style variant)
  TVariant = record
    kind: int32;
    union
      intVal: int64;
      floatVal: float64;
      strVal: pointer to char;
    end;
  end;
```

### Dynamic Arrays

```pax
var
  numbers: array of int32;
  i: int32;

begin
  setlength(numbers, 10);
  
  for i := 0 to len(numbers) - 1 do
    numbers[i] := i * 2;
  end;
end.
```

### Sets

```pax
type
  TDays = set of 0..6;

var
  weekdays: TDays;
  weekend: TDays;
  alldays: TDays;
  today: int32;

begin
  weekdays := {1, 2, 3, 4, 5};  // Mon-Fri
  weekend := {0, 6};            // Sun, Sat
  
  alldays := weekdays + weekend; // Union
  weekdays := alldays - weekend; // Difference
  
  today := 3;
  if today in weekdays then
    // It's a weekday
  end;
end.
```

### Enumerations

```pax
type
  TColor = (
    clRed,
    clGreen,
    clBlue
  );

  TFlags = (
    flagNone = 0,
    flagRead = 1,
    flagWrite = 2,
    flagExecute = 4
  );

var
  color: TColor;
  flags: TFlags;

begin
  color := clGreen;
  flags := flagRead;
end.
```

### Control Flow

```pax
var
  i: int32;
  sum: int32;
  n: int32;
  ch: char;

begin
  // If-then-else
  if n > 10 then
    sum := 1;
  else
    sum := 0;
  end;
  
  // While loop
  while i < 10 do
    i := i + 1;
  end;
  
  // For loop (ascending)
  for i := 1 to 10 do
    sum := sum + i;
  end;
  
  // For loop (descending)
  for i := 10 downto 1 do
    sum := sum + i;
  end;
  
  // Repeat-until
  repeat
    i := i + 1;
  until i >= 10;
  
  // Case statement with ranges
  case n of
    0: sum := 0;
    1..10: sum := 1;
    11..100: sum := 2;
  else
    sum := -1;
  end;
  
  // Case with character ranges
  case ch of
    'a'..'z': sum := 1;
    'A'..'Z': sum := 2;
    '0'..'9': sum := 3;
  else
    sum := 0;
  end;
end.
```

### Routine Types (Function Pointers)

```pax
type
  TIntFunc = routine(const a: int32; const b: int32): int32;
  TCallback = routine(const x: int32): int32;

routine Add(const a: int32; const b: int32): int32;
begin
  return a + b;
end;

routine Multiply(const a: int32; const b: int32): int32;
begin
  return a * b;
end;

// Higher-order function
routine Apply(const fn: TIntFunc; const x: int32; const y: int32): int32;
begin
  return fn(x, y);
end;

var
  mathOp: TIntFunc;
  result: int32;

begin
  mathOp := Add;
  result := mathOp(10, 5);     // 15
  
  mathOp := Multiply;
  result := mathOp(10, 5);     // 50
  
  result := Apply(Add, 3, 4);  // 7
end.
```

### Strings and Raw Strings

```pax
var
  path: string;
  msg: wstring;

begin
  // Normal strings - backslashes are escape characters
  path := 'C:\\Users\\Name\\file.txt';
  
  // Raw strings - no escape processing (great for paths)
  path := @'C:\Users\Name\file.txt';
  
  // Wide strings for Windows API (UTF-16)
  msg := L'Hello World!';
  
  // Raw wide strings
  msg := @L'C:\Path\To\File';
end.
```

### Pointers and Const Pointers

```pax
routine wprintf(const fmt: pointer to const wchar; ...): int32; external 'msvcrt.dll';

var
  value: int32;
  ptr: pointer to int32;
  constPtr: pointer to const char;

begin
  value := 42;
  ptr := address of value;  // Get pointer to value
  ptr^ := 100;              // Dereference and assign
  // value is now 100
end.
```

### External Routines

Call Windows DLL functions with simple declarations - no binding libraries required:

```pax
module exe WinAPI;

routine MessageBoxW(hwnd: pointer; text: pointer to wchar; 
  caption: pointer to wchar; utype: uint32): int32; external 'user32.dll';

begin
  MessageBoxW(nil, L'Hello from Pax!', L'Pax', 0);
end.
```

### Linking Libraries

For custom DLLs or when linking multiple functions from the same library, use #library:

```pax
module exe CustomDLL;

#library 'mylib'

// Routines declared without DLL name - linked via #library
routine MyFunction(const value: int32): int32; external;
routine MyOtherFunction(const msg: pointer to char); external;

begin
  MyOtherFunction('Hello');
end.
```

### GC Intrinsics

```pax
begin
  // Force garbage collection
  gc_collect();
  
  // Query heap statistics
  printf('Heap size: %lld bytes\n', gc_heapsize());
  printf('Used: %lld bytes\n', gc_usedsize());
  printf('GC cycles: %lld\n', gc_collectcount());
  
  // Dump detailed GC stats (debug builds)
  gc_dump();
end.
```

### Command Line Arguments

```pax
var
  i: int32;
  arg: string;

begin
  printf('Arguments: %d\n', paramcount());
  
  for i := 0 to paramcount() do
    arg := paramstr(i);
    printf('  [%d] %s\n', i, pointer to char(arg));
  end;
end.
```

### Conditional Compilation

```pax
#define DEBUG
#define VERSION 100

begin
  #ifdef DEBUG
  printf('Debug mode enabled\n');
  #endif
  
  #ifndef RELEASE
  printf('Not a release build\n');
  #endif
  
  #if VERSION >= 100
  printf('Version 100 or higher\n');
  #elif VERSION >= 50
  printf('Version 50-99\n');
  #else
  printf('Old version\n');
  #endif
  
  #undef DEBUG
end.
```

### PAX Compiler Constants

```pax
begin
  // Available as both preprocessor macros and runtime constants
  printf('Pax version: %s\n', PAX_VERSION_STR);
  printf('Major: %d\n', PAX_MAJOR_VERSION);
  printf('Minor: %d\n', PAX_MINOR_VERSION);
  printf('Patch: %d\n', PAX_PATCH_VERSION);
  printf('Combined: %d\n', PAX_VERSION);
  
  // Preprocessor detection
  #ifdef PAX
  printf('Compiled with Pax\n');
  #endif
end.
```

### Unit Testing

```pax
module exe MyTests;

#unittestmode on

routine Add(const a: int32; const b: int32): int32;
begin
  return a + b;
end;

routine IsPositive(const a: int32): boolean;
begin
  return a > 0;
end;

begin
  // Normal entry point (skipped in test mode)
end.

test 'Addition works correctly'
begin
  TestAssertEqualInt(5, Add(2, 3));
  TestAssertEqualInt(0, Add(-1, 1));
end;

test 'Boolean assertions'
begin
  TestAssertTrue(IsPositive(5));
  TestAssertFalse(IsPositive(-5));
end;

test 'Pointer and allocation assertions'
var
  p: pointer to int32;
begin
  p := nil;
  TestAssertNil(p);
  
  new(p);
  TestAssertNotNil(p);
  p^ := 42;
  TestAssertEqualInt(42, p^);
end;
```

### Version Info and Icons

```pax
module exe MyApp;

#subsystem gui
#addverinfo yes
#vimajor 1
#viminor 0
#vipatch 0
#viproductname 'My Application'
#videscription 'A sample Pax application'
#vicompanyname 'My Company'
#vicopyright 'Copyright 2025'
#exeicon @'assets\app.ico'

begin
  // Application code
end.
```

## Directives Reference

### Build Directives

| Directive | Description |
|-----------|-------------|
| #subsystem console/gui | PE subsystem (default: console) |
| #library 'name' | Link a library |
| #librarypath 'path' | Add library search path |
| #includepath 'path' | Add C include path |
| #addfile 'file' | Add .c, .obj, .lib, .dll to link |
| #modulepath 'path' | Add module search path |
| #outputpath 'path' | Set output directory |
| #generatedpath 'path' | Set generated C files directory |

### Preprocessor Directives

| Directive | Description |
|-----------|-------------|
| #define SYM [value] | Define preprocessor symbol |
| #undef SYM | Undefine symbol |
| #ifdef SYM | Conditional if defined |
| #ifndef SYM | Conditional if not defined |
| #if expr | Conditional expression |
| #elif expr | Else if |
| #else | Else branch |
| #endif | End conditional |

### Compiler Directives

| Directive | Description |
|-----------|-------------|
| #debug | Enable debug info (STABS format) |
| #unittestmode on/off | Enable unit test mode |
| #maxerrors N | Max errors before stopping |
| #option 'flag' | Pass raw TCC option |

### Version Info Directives

| Directive | Description |
|-----------|-------------|
| #addverinfo yes/no | Enable version info embedding |
| #vimajor N | Major version number |
| #viminor N | Minor version number |
| #vipatch N | Patch version number |
| #viproductname 'name' | Product name |
| #videscription 'desc' | File description |
| #vifilename 'name' | Original filename |
| #vicompanyname 'name' | Company name |
| #vicopyright 'text' | Copyright notice |
| #exeicon 'path' | Executable icon |

## C Header Importer

Pax includes a built-in C header importer that converts C headers into Pax module source code. It uses TCC for preprocessing (expanding macros, includes) then parses the result to generate Pax declarations.

```delphi
LImporter := TPaxCImporter.Create();
LImporter.SetModuleName('raylib');
LImporter.SetDllName('raylib.dll');
LImporter.AddIncludePath('path/to/headers');
LImporter.AddExcludedType('va_list');
LImporter.ImportHeader('raylib.h');
LImporter.Free();
```

The importer handles:
- Structs, unions, and typedefs
- Function declarations with varargs
- Enumerations with explicit values
- Pointer types and arrays
- Forward declarations for opaque types

## Architecture

The compiler is built in Delphi with a clean pipeline architecture:

| Unit | Purpose |
|------|---------|
| Pax.Lexer | Tokenization with full Unicode support |
| Pax.AST | Abstract syntax tree node definitions |
| Pax.Parser | Recursive descent parser with error recovery |
| Pax.Types | Type registry and type system management |
| Pax.Symbols | Symbol table with scope management |
| Pax.Checker | Semantic analysis and type checking |
| Pax.CodeGen | C99 code generation |
| Pax.Compiler | Build orchestration and TinyCC integration |

### Additional Components

| Unit | Purpose |
|------|---------|
| Pax.ArArchive | Native AR archive creation for static libraries |
| Pax.ZipVFS | Virtual file system for embedded toolchain |
| Pax.IATHook | IAT hooking for transparent file redirection |
| Pax.LibTCC | TinyCC (libtcc) integration |
| Pax.ModuleLoader | Module dependency resolution |
| Pax.Errors | Error types, codes, and diagnostic formatting |
| Pax.CImporter | C header to Pax module converter |

## Status

**Under active development.**

The core compiler is functional and can produce working executables, DLLs, and static libraries. All major language features are implemented:

- Records with inheritance
- Unions (named and anonymous)
- Packed records and alignment
- Bit fields
- Dynamic arrays
- Sets with ranges and operations
- Managed strings (UTF-8/UTF-16)
- External DLL calls with varargs
- Module system with imports
- Unit testing framework
- GC intrinsics
- Version info embedding
- Icon embedding
- Type aliases
- Routine types (function pointers)
- Compound assignment operators
- Case statement ranges
- Conditional compilation
- PAX compiler constants
- Enumeration types
- C header importer

## Building

### Get the Source

**Option 1: [Download ZIP](https://github.com/tinyBigGAMES/PaxLang/archive/refs/heads/main.zip)**

**Option 2: Git Clone**
```bash
git clone https://github.com/tinyBigGAMES/PaxLang.git
```

### Compile

1. Open src\Pax Programming Language.groupproj in Delphi
2. Build the project

That's it! Everything is included - TinyCC, Boehm GC, and runtime resources are already bundled in the repo. The compiled executable will be output to the bin folder.

## Requirements

| | Minimum | Tested |
|---|---------|--------|
| **Platform** | Windows 10 x64 | Windows 11 25H2 x64 |
| **Build** | Delphi 11 (Alexandria) | Delphi 12 (Athens) |

**Dependencies:** None for end users - TinyCC and Boehm GC are embedded.

## Contributing

Contributions are welcome! Join our [Discord](https://discord.gg/tPWjMwK) to discuss development.

## License

Pax is licensed under the **Apache License 2.0**. See [LICENSE](https://github.com/tinyBigGAMES/PaxLang/tree/main?tab=License-1-ov-file#readme) for details.

## Links

- [Discord](https://discord.gg/tPWjMwK)
- [Bluesky](https://bsky.app/profile/tinybiggames.com)
- [tinyBigGAMES](https://tinybiggames.com)

---

<div align="center">

**Pax**™ Programming Language.

Copyright © 2025-present tinyBigGAMES™ LLC  
All Rights Reserved.

</div>
