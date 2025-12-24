{===============================================================================
  Pax™ Programming Language.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://paxlang.org

  See LICENSE for license information
===============================================================================}

program Testbed;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  UTestbed in 'UTestbed.pas',
  Pax.ArArchive in '..\compiler\Pax.ArArchive.pas',
  Pax.AST in '..\compiler\Pax.AST.pas',
  Pax.Checker in '..\compiler\Pax.Checker.pas',
  Pax.CodeGen in '..\compiler\Pax.CodeGen.pas',
  Pax.Compiler in '..\compiler\Pax.Compiler.pas',
  Pax.Errors in '..\compiler\Pax.Errors.pas',
  Pax.IATHook in '..\compiler\Pax.IATHook.pas',
  Pax.Lexer in '..\compiler\Pax.Lexer.pas',
  Pax.LibTCC in '..\compiler\Pax.LibTCC.pas',
  Pax.ModuleLoader in '..\compiler\Pax.ModuleLoader.pas',
  Pax.Parser in '..\compiler\Pax.Parser.pas',
  Pax.Resources in '..\compiler\Pax.Resources.pas',
  Pax.Symbols in '..\compiler\Pax.Symbols.pas',
  Pax.Types in '..\compiler\Pax.Types.pas',
  Pax.Utils in '..\compiler\Pax.Utils.pas',
  Pax.ZipVFS in '..\compiler\Pax.ZipVFS.pas';

begin
  RunTestbed();
end.
