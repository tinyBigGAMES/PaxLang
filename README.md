<div align="center">

![Pax](media/logo.png)

[![Discord](https://img.shields.io/discord/754884471324672040?style=for-the-badge&logo=discord&label=Discord)](https://discord.gg/tPWjMwK)
[![Follow on Bluesky](https://img.shields.io/badge/Bluesky-tinyBigGAMES-blue?style=for-the-badge&logo=bluesky)](https://bsky.app/profile/tinybiggames.com)

A minimal systems programming language that compiles to native executables via C99.

</div>

## What is Pax?

The **Pax™ Programming Language** is a Pascal/Oberon-inspired systems programming language targeting Win64. It compiles to C via [TinyCC](https://github.com/TinyCC/tinycc) with a completely self-contained toolchain — no external dependencies required. Memory is automatically managed via the [Boehm-Demers-Weiser Garbage Collector](https://github.com/bdwgc/bdwgc).

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

## ✨ Key Features

- ✨ **Minimal by design** — Clean syntax, no redundancy, just what you need
- 🔤 **Pascal heritage** — Readable, structured code inspired by Pascal and Oberon
- 🧠 **Automatic memory** — Boehm GC handles allocation and cleanup
- 📦 **Self-contained** — Embedded TinyCC toolchain, single executable distribution
- 🔗 **Windows native** — Call any DLL directly, full Windows API access
- 🏗️ **Multiple outputs** — Build executables, DLLs, or static libraries
- 🧬 **Type extension** — Record inheritance without class complexity
- 🔀 **Union types** — C-compatible unions with anonymous nesting
- 📊 **Dynamic arrays** — `setlength`/`len` with automatic memory management
- 🛡️ **Managed strings** — UTF-8 and UTF-16 string types with emoji support
- ⚡ **Varargs support** — Full C interop with printf-style functions
- 🧪 **Built-in testing** — Integrated unit test framework
- 📋 **Version info** — Embed metadata and icons in executables

## 📖 Language Overview

### Built-in Types

| Type | Size | Description |
|------|------|-------------|
| `int8`, `int16`, `int32`, `int64` | 1-8 bytes | Signed integers |
| `uint8`, `uint16`, `uint32`, `uint64` | 1-8 bytes | Unsigned integers |
| `float32`, `float64` | 4-8 bytes | Floating point |
| `boolean` | 1 byte | `true` / `false` |
| `char`, `uchar` | 1 byte | Signed/unsigned characters |
| `wchar`, `uwchar` | 2 bytes | Wide characters (UTF-16) |
| `string`, `wstring` | 8 bytes | Managed strings (UTF-8 / UTF-16) |
| `pointer`, `pointer to T` | 8 bytes | Untyped / typed pointers |

### Module Types

| Type | Description |
|------|-------------|
| `module exe Name` | Executable program |
| `module lib Name` | Static library (.a) |
| `module dll Name` | Dynamic library (.dll) |

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
  today: int32;

begin
  weekdays := {1, 2, 3, 4, 5};  // Mon-Fri
  today := 3;
  
  if today in weekdays then
    // It's a weekday
  end;
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
  msg := L'Hello 🌍 World! 🎉';
  
  // Raw wide strings
  msg := @L'C:\Path\To\File';
end.
```

### External Routines

Call Windows DLL functions with simple declarations — no binding libraries required:

```pax
module exe WinAPI;

routine MessageBoxW(hwnd: pointer; text: pointer to wchar; 
  caption: pointer to wchar; utype: uint32): int32; external 'user32.dll';

begin
  MessageBoxW(nil, L'Hello from Pax! 🚀', L'Pax', 0);
end.
```

### Linking Libraries

For custom DLLs or when linking multiple functions from the same library, use `#library`:

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

### Pointers and Address-Of

```pax
var
  value: int32;
  ptr: pointer to int32;

begin
  value := 42;
  ptr := address of value;  // Get pointer to value
  ptr^ := 100;              // Dereference and assign
  // value is now 100
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

### Unit Testing

```pax
module exe MyTests;

#unittestmode on

routine add(const a: int32; const b: int32): int32;
begin
  return a + b;
end;

begin
  // Normal entry point (skipped in test mode)
end.

test 'add returns correct sum';
begin
  assert(add(2, 3) = 5);
  assert(add(-1, 1) = 0);
end;

test 'add handles negative numbers';
begin
  assert(add(-5, -3) = -8);
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
#vicopyright 'Copyright © 2025'
#exeicon @'assets\app.ico'

begin
  // Application code
end.
```

## 🔧 Directives Reference

### Build Directives

| Directive | Description |
|-----------|-------------|
| `#subsystem console\|gui` | PE subsystem (default: console) |
| `#library "name"` | Link a library |
| `#librarypath "path"` | Add library search path |
| `#includepath "path"` | Add C include path |
| `#addfile "file"` | Add .c, .obj, .lib, .dll to link |
| `#modulepath "path"` | Add module search path |
| `#outputpath "path"` | Set output directory |
| `#generatedpath "path"` | Set generated C files directory |

### Preprocessor Directives

| Directive | Description |
|-----------|-------------|
| `#define SYM [value]` | Define preprocessor symbol |
| `#undef SYM` | Undefine symbol |
| `#ifdef SYM` | Conditional if defined |
| `#ifndef SYM` | Conditional if not defined |
| `#if expr` | Conditional expression |
| `#elif expr` | Else if |
| `#else` | Else branch |
| `#endif` | End conditional |

### Compiler Directives

| Directive | Description |
|-----------|-------------|
| `#debug on\|off` | Enable debug info |
| `#unittestmode on\|off` | Enable unit test mode |
| `#maxerrors N` | Max errors before stopping |
| `#option "flag"` | Pass raw TCC option |

### Version Info Directives

| Directive | Description |
|-----------|-------------|
| `#addverinfo yes\|no` | Enable version info embedding |
| `#vimajor N` | Major version number |
| `#viminor N` | Minor version number |
| `#vipatch N` | Patch version number |
| `#viproductname "name"` | Product name |
| `#videscription "desc"` | File description |
| `#vifilename "name"` | Original filename |
| `#vicompanyname "name"` | Company name |
| `#vicopyright "text"` | Copyright notice |
| `#exeicon "path"` | Executable icon |

## 🏗️ Architecture

The compiler is built in Delphi with a clean pipeline architecture:

| Unit | Purpose |
|------|---------|
| `Pax.Lexer` | Tokenization with full Unicode support |
| `Pax.AST` | Abstract syntax tree node definitions |
| `Pax.Parser` | Recursive descent parser with error recovery |
| `Pax.Types` | Type registry and type system management |
| `Pax.Symbols` | Symbol table with scope management |
| `Pax.Checker` | Semantic analysis and type checking |
| `Pax.CodeGen` | C99 code generation |
| `Pax.Compiler` | Build orchestration and TinyCC integration |

### Additional Components

| Unit | Purpose |
|------|---------|
| `Pax.ArArchive` | Native AR archive creation for static libraries |
| `Pax.ZipVFS` | Virtual file system for embedded toolchain |
| `Pax.IATHook` | IAT hooking for transparent file redirection |
| `Pax.LibTCC` | TinyCC (libtcc) integration |
| `Pax.ModuleLoader` | Module dependency resolution |

## 🚧 Status

**Version 0.1.0 — Under active development.**

The core compiler is functional and can produce working executables, DLLs, and static libraries. All major language features are implemented:

- ✅ Records with inheritance
- ✅ Unions (named and anonymous)
- ✅ Packed records and alignment
- ✅ Bit fields
- ✅ Dynamic arrays
- ✅ Sets with ranges
- ✅ Managed strings (UTF-8/UTF-16)
- ✅ External DLL calls with varargs
- ✅ Module system with imports
- ✅ Unit testing framework
- ✅ GC intrinsics
- ✅ Version info embedding
- ✅ Icon embedding

## 🛠️ Building

### Get the Source

**Option 1: [Download ZIP](https://github.com/tinyBigGAMES/PaxLang/archive/refs/heads/main.zip)**

**Option 2: Git Clone**
```bash
git clone https://github.com/tinyBigGAMES/PaxLang.git
```

### Compile

1. Open `src\Pax Programming Language.groupproj` in Delphi
2. Build the project

That's it! Everything is included — TinyCC, Boehm GC, and runtime resources are already bundled in the repo. The compiled executable will be output to the `bin` folder.

## 📋 Requirements

| | Minimum | Tested |
|---|---------|--------|
| **Platform** | Windows 10 x64 | Windows 11 25H2 x64 |
| **Build** | Delphi 11 (Alexandria) | Delphi 12 (Athens) |

**Dependencies:** None for end users — TinyCC and Boehm GC are embedded.

## 🤝 Contributing

Contributions are welcome! Join our [Discord](https://discord.gg/tPWjMwK) to discuss development.

## 📄 License

Pax is licensed under the **Apache License 2.0**. See [LICENSE](https://github.com/tinyBigGAMES/PaxLang/tree/main?tab=License-1-ov-file#readme) for details.

## 🔗 Links

- [Discord](https://discord.gg/tPWjMwK)
- [Bluesky](https://bsky.app/profile/tinybiggames.com)
- [tinyBigGAMES](https://tinybiggames.com)

---

<div align="center">

**Pax**™ Programming Language.

Copyright © 2025-present tinyBigGAMES™ LLC.  
All Rights Reserved.

</div>
