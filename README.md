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

- ✨ **Minimal by design** — Clean syntax, no redundancy, just what you need.
- 🔤 **Pascal heritage** — Readable, structured code inspired by Pascal and Oberon.
- 🧠 **Automatic memory** — Boehm GC handles allocation and cleanup.
- 📦 **Self-contained** — Embedded TinyCC toolchain, single executable distribution.
- 🔗 **Windows native** — Call any DLL directly, full Windows API access.
- 🏗️ **Multiple outputs** — Build executables, DLLs, or static libraries.
- 🧬 **Type extension** — Record inheritance without class complexity.
- 📊 **Dynamic arrays** — `setlength`/`len` with automatic memory management.
- 🛡️ **Managed strings** — UTF-8 and UTF-16 string types.
- ⚡ **Varargs support** — Full C interop with printf-style functions.

## 📖 Language Overview

### Built-in Types

| Type | Size | Description |
|------|------|-------------|
| `int8`, `int16`, `int32`, `int64` | 1-8 bytes | Signed integers |
| `uint8`, `uint16`, `uint32`, `uint64` | 1-8 bytes | Unsigned integers |
| `float32`, `float64` | 4-8 bytes | Floating point |
| `boolean` | 1 byte | `true` / `false` |
| `char`, `wchar` | 1-2 bytes | Characters (ANSI / UTF-16) |
| `string`, `wstring` | 8 bytes | Managed strings (UTF-8 / UTF-16) |
| `pointer`, `pointer to T` | 8 bytes | Untyped / typed pointers |

### Module Types

| Type | Description |
|------|-------------|
| `module exe Name` | Executable program |
| `module lib Name` | Static library |
| `module dll Name` | Dynamic/shared library |

### Records and Extension

```pax
type
  TPoint = record
    x: int32;
    y: int32;
  end;

  TColorPoint = record(TPoint)
    color: uint32;
  end;

var
  p: TColorPoint;

begin
  p.x := 100;       // inherited from TPoint
  p.y := 200;       // inherited from TPoint
  p.color := $FF0000;
end.
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

### External Routines

Call any Windows DLL directly — no wrappers needed:

```pax
module exe WinAPI;

#library 'user32'

routine MessageBoxW(hwnd: pointer; text: pointer to wchar; 
  caption: pointer to wchar; utype: uint32): int32; external 'user32.dll';

begin
  MessageBoxW(nil, L'Hello from Pax!', L'Pax', 0);
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
  
  // Dump detailed GC stats
  gc_dump();
end.
```

## 🏗️ Architecture

The compiler is built in Delphi with a clean architecture:

| Unit | Purpose |
|------|---------|
| `Pax.Lexer` | Tokenization with full Unicode support |
| `Pax.AST` | Abstract syntax tree node definitions |
| `Pax.Parser` | Recursive descent parser with error recovery |
| `Pax.Types` | Type registry and type system management |
| `Pax.Symbols` | Symbol table with scope management |
| `Pax.Checker` | Semantic analysis and type checking |
| `Pax.CodeGen` | C code generation |
| `Pax.Compiler` | Build orchestration and TinyCC integration |

### Additional Components

| Unit | Purpose |
|------|---------|
| `Pax.ArArchive` | Native AR archive creation for static libraries |
| `Pax.ZipVFS` | Virtual file system for embedded toolchain |
| `Pax.IATHook` | IAT hooking for transparent file redirection |
| `Pax.LibTCC` | TinyCC (libtcc) integration |

## 🔧 Build Directives

```pax
#apptype console           // Console application (default)
#apptype gui               // Windows GUI application
#library 'user32'          // Link a library
#modulepath 'path'         // Module search path
#outputpath 'path'         // Output directory
#generatedpath 'path'      // Generated C files directory
#addfile 'file.lib'        // Add file to link
#subsystem console         // PE subsystem
```

## 🚧 Status

**Under active development.**

The core compiler is functional and can produce working executables, DLLs, and static libraries. We're actively working toward a 1.0 release.

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
