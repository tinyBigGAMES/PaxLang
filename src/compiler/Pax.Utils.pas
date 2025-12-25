{===============================================================================
  Pax™ Programming Language.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://paxlang.org

  See LICENSE for license information
===============================================================================}


unit Pax.Utils;

{$I Pax.Defines.inc}

interface

uses
  WinAPI.Windows,
  System.SysUtils,
  System.IOUtils,
  System.AnsiStrings,
  System.Classes,
  System.Math;

  const
    COLOR_RESET   = #27'[0m';
    COLOR_BOLD    = #27'[1m';
    COLOR_RED     = #27'[31m';
    COLOR_GREEN   = #27'[32m';
    COLOR_YELLOW  = #27'[33m';
    COLOR_BLUE    = #27'[34m';
    COLOR_CYAN    = #27'[36m';
    COLOR_WHITE   = #27'[37m';

type

  { TCallback }
  TCallback<T> = record
    Callback: T;
    UserData: Pointer;
  end;

  { TCaptureConsoleCallback }
  TCaptureConsoleCallback = reference to procedure(const ALine: string; const AUserData: Pointer);

  { TVersionInfo }
  TVersionInfo = record
    Major: Word;
    Minor: Word;
    Patch: Word;
    Build: Word;
    VersionString: string;
  end;

  { TUtils }
  TUtils = class
  private class var
    FMarshaller: TMarshaller;
  private
    class function  EnableVirtualTerminalProcessing(): Boolean; static;
    class procedure InitConsole(); static;

  public
    class procedure FailIf(const Cond: Boolean; const Msg: string; const AArgs: array of const);

    class function  GetTickCount(): DWORD; static;
    class function  GetTickCount64(): UInt64; static;

    class function  CallI64(AFunction: Pointer; const AArgs: array of const): UInt64; static;
    class function  CallF32(AFunction: Pointer; const AArgs: array of const): Single; static;
    class function  CallF64(AFunction: Pointer; const AArgs: array of const): Double; static;

    class function  HasConsole(): Boolean; static;
    class procedure ClearToEOL(); static;
    class procedure Print(); overload; static;
    class procedure PrintLn(); overload; static;
    class procedure Print(const AText: string); overload; static;
    class procedure Print(const AText: string; const AArgs: array of const); overload; static;
    class procedure PrintLn(const AText: string); overload; static;
    class procedure PrintLn(const AText: string; const AArgs: array of const); overload; static;
    class procedure Pause(); static;

    class function  AsUTF8(const AValue: string; ALength: PCardinal=nil): Pointer; static;
    class function  ToAnsi(const AValue: string): AnsiString; static;


    class procedure ProcessMessages(); static;

    class function  RunExe(const AExe, AParams, AWorkDir: string; const AWait: Boolean = True; const AShowCmd: Word = SW_SHOWNORMAL): Cardinal; static;
    class procedure CaptureConsoleOutput(const ATitle: string; const ACommand: PChar; const AParameters: PChar; const AWorkDir: string; var AExitCode: DWORD; const AUserData: Pointer; const ACallback: TCaptureConsoleCallback); static;
    class procedure CaptureZigConsoleOutput(const ATitle: string; const ACommand: PChar; const AParameters: PChar; const AWorkDir: string; var AExitCode: DWORD; const AUserData: Pointer; const ACallback: TCaptureConsoleCallback); static;

    class function  CreateDirInPath(const AFilename: string): Boolean;
    class function  GetVersionInfo(out AVersionInfo: TVersionInfo; const AFilePath: string = ''): Boolean; static;
    class function  GetZigExePath(): string; static;

    class procedure CopyFilePreservingEncoding(const ASourceFile, ADestFile: string); static;
    class function  DetectFileEncoding(const AFilePath: string): TEncoding; static;
    class function  EnsureBOM(const AText: string): string; static;
    class function  EscapeString(const AText: string): string; static;

    class function  IsValidWin64PE(const AFilePath: string): Boolean; static;
    class procedure UpdateIconResource(const AExeFilePath, AIconFilePath: string); static;
    class procedure UpdateVersionInfoResource(const PEFilePath: string; const AMajor, AMinor, APatch: Word; const AProductName, ADescription, AFilename, ACompanyName, ACopyright: string); static;
    class function  ResourceExist(const AResName: string): Boolean; static;
    class function  AddResManifestFromResource(const aResName: string; const aModuleFile: string; aLanguage: Integer=1033): Boolean; static;

  end;

  { TBaseObject }
  TBaseObject = class
  public
    constructor Create(); virtual;
    destructor Destroy(); override;
  end;

  { TCommandBuilder }
  TCommandBuilder = class(TBaseObject)
  private
    FParams: TStringList;
  public
    constructor Create(); override;
    destructor Destroy(); override;
    
    procedure Clear();
    procedure AddParam(const AParam: string); overload;
    procedure AddParam(const AFlag, AValue: string); overload;
    procedure AddQuotedParam(const AFlag, AValue: string); overload;
    procedure AddQuotedParam(const AValue: string); overload;
    procedure AddFlag(const AFlag: string);
    
    function ToString(): string; reintroduce;
    function GetParamCount(): Integer;
  end;

const
  LOAD_LIBRARY_SEARCH_DEFAULT_DIRS   = $00001000;
  LOAD_LIBRARY_SEARCH_USER_DIRS      = $00000400;
  LOAD_LIBRARY_SEARCH_APPLICATION_DIR= $00000200;
  LOAD_LIBRARY_SEARCH_SYSTEM32       = $00000800;

function AddDllDirectory(NewDirectory: LPCWSTR): Pointer; stdcall; external kernel32 name 'AddDllDirectory';
function RemoveDllDirectory(Cookie: Pointer): BOOL; stdcall; external kernel32 name 'RemoveDllDirectory';
function SetDefaultDllDirectories(DirectoryFlags: DWORD): BOOL; stdcall; external kernel32 name 'SetDefaultDllDirectories';
function GetEnvironmentStringsW(): PWideChar; stdcall; external kernel32 name 'GetEnvironmentStringsW';
function FreeEnvironmentStringsW(lpszEnvironmentBlock: PWideChar): BOOL; stdcall; external kernel32 name 'FreeEnvironmentStringsW';


implementation

{$IF DEFINED(MSWINDOWS) AND DEFINED(CPUX64)}
function ffi_call_win64_i64(AFunction: Pointer; AArgs: PUInt64; AArgCount: Cardinal): UInt64; assembler;
asm
  // Prologue with only RBX saved; compute aligned stack space so that
  // RSP is 16-byte aligned at the CALL site.
  push rbp
  mov  rbp, rsp
  push rbx

  // Volatile locals
  mov  r11, rcx        // AFunction
  mov  r10, rdx        // AArgs
  mov  eax, r8d        // AArgCount -> EAX

  // k = max(0, ArgCount-4)
  mov  ecx, eax
  sub  ecx, 4
  xor  edx, edx
  cmp  ecx, 0
  jle  @no_stack
  mov  edx, ecx
  shl  edx, 3          // edx = k * 8
@no_stack:
  // s = 32 + 8*k ; ensure s ≡ 8 (mod 16) because we've pushed RBX
  lea  ebx, [rdx + 32] // ebx = base space
  test ecx, 1          // if k even, add +8; if k odd, already ≡ 8
  jnz  @have_s
  add  ebx, 8
@have_s:
  sub  rsp, rbx        // allocate

  // Copy stack args (5..N) to [rsp+32]
  mov  ecx, eax
  cmp  ecx, 4
  jle  @load_regs
  sub  ecx, 4
  lea  rsi, [r10 + 32]   // src
  lea  rdi, [rsp + 32]   // dst
  rep  movsq

@load_regs:
  // Dual-load first 4 slots
  test eax, eax
  jz   @do_call
  mov  rcx, [r10]
  movsd xmm0, qword ptr [r10]

  cmp  eax, 1
  jle  @do_call
  mov  rdx, [r10 + 8]
  movsd xmm1, qword ptr [r10 + 8]

  cmp  eax, 2
  jle  @do_call
  mov  r8,  [r10 + 16]
  movsd xmm2, qword ptr [r10 + 16]

  cmp  eax, 3
  jle  @do_call
  mov  r9,  [r10 + 24]
  movsd xmm3, qword ptr [r10 + 24]

@do_call:
  call r11

  // Epilogue
  add  rsp, rbx
  pop  rbx
  pop  rbp
  ret
end;

procedure ffi_call_win64_f32(AFunction: Pointer; AArgs: PUInt64; AArgCount: Cardinal; AResult: PSingle); assembler;
asm
  push rbp
  mov  rbp, rsp
  push rbx

  mov  r11, rcx        // AFunction
  mov  r10, rdx        // AArgs
  mov  eax, r8d        // AArgCount
  mov  r9,  r9         // AResult already in R9 (keep)

  // k = max(0, ArgCount-4)
  mov  ecx, eax
  sub  ecx, 4
  xor  edx, edx
  cmp  ecx, 0
  jle  @no_stack
  mov  edx, ecx
  shl  edx, 3
@no_stack:
  // s = 32 + 8*k ; adjust for RBX push parity
  lea  ebx, [rdx + 32]
  test ecx, 1
  jnz  @have_s
  add  ebx, 8
@have_s:
  sub  rsp, rbx

  // Copy stack args
  mov  ecx, eax
  cmp  ecx, 4
  jle  @load_regs
  sub  ecx, 4
  lea  rsi, [r10 + 32]
  lea  rdi, [rsp + 32]
  rep  movsq

@load_regs:
  test eax, eax
  jz   @do_call
  mov  rcx, [r10]
  // For float params the low 32 bits contain the value; movsd is fine (callee reads low 32)
  movsd xmm0, qword ptr [r10]

  cmp  eax, 1
  jle  @do_call
  mov  rdx, [r10 + 8]
  movsd xmm1, qword ptr [r10 + 8]

  cmp  eax, 2
  jle  @do_call
  mov  r8,  [r10 + 16]
  movsd xmm2, qword ptr [r10 + 16]

  cmp  eax, 3
  jle  @do_call
  mov  r9,  [r10 + 24]
  movsd xmm3, qword ptr [r10 + 24]

@do_call:
  call r11

  // Store float result
  test r9, r9
  jz   @done
  movss dword ptr [r9], xmm0
@done:
  add  rsp, rbx
  pop  rbx
  pop  rbp
  ret
end;

procedure ffi_call_win64_f64(AFunction: Pointer; AArgs: PUInt64; AArgCount: Cardinal; AResult: PDouble); assembler;
asm
  push rbp
  mov  rbp, rsp
  push rbx

  mov  r11, rcx        // AFunction
  mov  r10, rdx        // AArgs
  mov  eax, r8d        // AArgCount
  mov  r9,  r9         // AResult already in R9

  // k = max(0, ArgCount-4)
  mov  ecx, eax
  sub  ecx, 4
  xor  edx, edx
  cmp  ecx, 0
  jle  @no_stack
  mov  edx, ecx
  shl  edx, 3
@no_stack:
  // s = 32 + 8*k ; adjust for RBX push parity
  lea  ebx, [rdx + 32]
  test ecx, 1
  jnz  @have_s
  add  ebx, 8
@have_s:
  sub  rsp, rbx

  // Copy stack args
  mov  ecx, eax
  cmp  ecx, 4
  jle  @load_regs
  sub  ecx, 4
  lea  rsi, [r10 + 32]
  lea  rdi, [rsp + 32]
  rep  movsq

@load_regs:
  test eax, eax
  jz   @do_call
  mov  rcx, [r10]
  movsd xmm0, qword ptr [r10]

  cmp  eax, 1
  jle  @do_call
  mov  rdx, [r10 + 8]
  movsd xmm1, qword ptr [r10 + 8]

  cmp  eax, 2
  jle  @do_call
  mov  r8,  [r10 + 16]
  movsd xmm2, qword ptr [r10 + 16]

  cmp  eax, 3
  jle  @do_call
  mov  r9,  [r10 + 24]
  movsd xmm3, qword ptr [r10 + 24]

@do_call:
  call r11

  // Store double result
  test r9, r9
  jz   @done
  movsd qword ptr [r9], xmm0
@done:
  add  rsp, rbx
  pop  rbx
  pop  rbp
  ret
end;
{$ENDIF}

{ TUtils }
class function TUtils.EnableVirtualTerminalProcessing(): Boolean;
var
  HOut: THandle;
  LMode: DWORD;
begin
  Result := False;

  HOut := GetStdHandle(STD_OUTPUT_HANDLE);
  if HOut = INVALID_HANDLE_VALUE then Exit;
  if not GetConsoleMode(HOut, LMode) then Exit;

  LMode := LMode or ENABLE_VIRTUAL_TERMINAL_PROCESSING;
  if not SetConsoleMode(HOut, LMode) then Exit;

  Result := True;
end;

class procedure TUtils.InitConsole();
begin
  {$IF DEFINED(MSWINDOWS) AND DEFINED(CPUX64)}
    EnableVirtualTerminalProcessing();
    SetConsoleCP(CP_UTF8);
    SetConsoleOutputCP(CP_UTF8);
  {$ENDIF}
end;

type
  TUInt64Array = array of UInt64;

class function TUtils.CallI64(AFunction: Pointer; const AArgs: array of const): UInt64;
var
  LSlots: TUInt64Array;
  I: Integer;
  L: UInt64;
begin
  SetLength(LSlots, Length(AArgs));
  for I := 0 to High(AArgs) do
  begin
    L := 0;
    case AArgs[I].VType of
      vtInteger:       L := UInt64(Int64(AArgs[I].VInteger));
      vtInt64:         L := UInt64(PInt64(AArgs[I].VInt64)^);
      vtBoolean:       L := Ord(AArgs[I].VBoolean);
      vtPointer:       L := UInt64(NativeUInt(AArgs[I].VPointer));
      vtPChar:         L := UInt64(NativeUInt(AArgs[I].VPChar));
      vtPWideChar:     L := UInt64(NativeUInt(AArgs[I].VPWideChar));
      vtClass:         L := UInt64(NativeUInt(AArgs[I].VClass));
      vtObject:        L := UInt64(NativeUInt(AArgs[I].VObject));
      vtWideChar:      L := UInt64(Ord(AArgs[I].VWideChar));
      vtChar:          L := UInt64(Ord(AArgs[I].VChar));
      vtAnsiString:    L := UInt64(NativeUInt(AArgs[I].VAnsiString));      // pointer to Ansi data
      vtUnicodeString: L := UInt64(NativeUInt(AArgs[I].VUnicodeString));   // pointer to UTF-16 data
      vtExtended:      Move(PExtended(AArgs[I].VExtended)^, L, 8);         // pass as double bits
      vtCurrency:      Move(PCurrency(AArgs[I].VCurrency)^, L, 8);
      vtVariant:       L := UInt64(NativeUInt(AArgs[I].VVariant));         // pointer to Variant
    else
      L := 0;
    end;
    LSlots[I] := L;
  end;

  if Length(LSlots) = 0 then
    Result := ffi_call_win64_i64(AFunction, nil, 0)
  else
    Result := ffi_call_win64_i64(AFunction, @LSlots[0], Length(LSlots));
end;

class function TUtils.CallF32(AFunction: Pointer; const AArgs: array of const): Single;
var
  LSlots: TUInt64Array;
  I: Integer;
  L: UInt64;
  S: Single;
begin
  SetLength(LSlots, Length(AArgs));
  for I := 0 to High(AArgs) do
  begin
    L := 0;
    case AArgs[I].VType of
      vtExtended:      begin S := Single(PExtended(AArgs[I].VExtended)^); Move(S, L, 4); end;
      vtInteger:       L := UInt64(Int64(AArgs[I].VInteger));
      vtInt64:         L := UInt64(PInt64(AArgs[I].VInt64)^);
      vtBoolean:       L := Ord(AArgs[I].VBoolean);
      vtPointer:       L := UInt64(NativeUInt(AArgs[I].VPointer));
      vtPChar:         L := UInt64(NativeUInt(AArgs[I].VPChar));
      vtPWideChar:     L := UInt64(NativeUInt(AArgs[I].VPWideChar));
      vtChar:          L := UInt64(Ord(AArgs[I].VChar));
      vtWideChar:      L := UInt64(Ord(AArgs[I].VWideChar));
      vtAnsiString:    L := UInt64(NativeUInt(AArgs[I].VAnsiString));
      vtUnicodeString: L := UInt64(NativeUInt(AArgs[I].VUnicodeString));
      vtCurrency:      Move(PCurrency(AArgs[I].VCurrency)^, L, 8);
      vtVariant:       L := UInt64(NativeUInt(AArgs[I].VVariant));
    else
      L := 0;
    end;
    LSlots[I] := L;
  end;

  if Length(LSlots) = 0 then
    ffi_call_win64_f32(AFunction, nil, 0, @Result)
  else
    ffi_call_win64_f32(AFunction, @LSlots[0], Length(LSlots), @Result);
end;

class function TUtils.CallF64(AFunction: Pointer; const AArgs: array of const): Double;
var
  LSlots: TUInt64Array;
  I: Integer;
  L: UInt64;
  D: Double;
begin
  SetLength(LSlots, Length(AArgs));
  for I := 0 to High(AArgs) do
  begin
    L := 0;
    case AArgs[I].VType of
      vtExtended:      begin D := Double(PExtended(AArgs[I].VExtended)^); Move(D, L, 8); end;
      vtInteger:       L := UInt64(Int64(AArgs[I].VInteger));
      vtInt64:         L := UInt64(PInt64(AArgs[I].VInt64)^);
      vtBoolean:       L := Ord(AArgs[I].VBoolean);
      vtPointer:       L := UInt64(NativeUInt(AArgs[I].VPointer));
      vtPChar:         L := UInt64(NativeUInt(AArgs[I].VPChar));
      vtPWideChar:     L := UInt64(NativeUInt(AArgs[I].VPWideChar));
      vtChar:          L := UInt64(Ord(AArgs[I].VChar));
      vtWideChar:      L := UInt64(Ord(AArgs[I].VWideChar));
      vtAnsiString:    L := UInt64(NativeUInt(AArgs[I].VAnsiString));
      vtUnicodeString: L := UInt64(NativeUInt(AArgs[I].VUnicodeString));
      vtCurrency:      Move(PCurrency(AArgs[I].VCurrency)^, L, 8);
      vtVariant:       L := UInt64(NativeUInt(AArgs[I].VVariant));
    else
      L := 0;
    end;
    LSlots[I] := L;
  end;

  if Length(LSlots) = 0 then
    ffi_call_win64_f64(AFunction, nil, 0, @Result)
  else
    ffi_call_win64_f64(AFunction, @LSlots[0], Length(LSlots), @Result);
end;

class procedure TUtils.FailIf(const Cond: Boolean; const Msg: string; const AArgs: array of const);
  begin
    if Cond then
      raise Exception.CreateFmt(Msg, AArgs);
  end;

class function TUtils.GetTickCount(): DWORD;
begin
  {$IF DEFINED(MSWINDOWS) AND DEFINED(CPUX64)}
  Result := WinApi.Windows.GetTickCount();
  {$ENDIF}
end;

class function TUtils.GetTickCount64(): UInt64;
begin
  {$IF DEFINED(MSWINDOWS) AND DEFINED(CPUX64)}
  Result := WinApi.Windows.GetTickCount64();
  {$ENDIF}
end;

class function TUtils.HasConsole(): Boolean;
begin
  {$IF DEFINED(MSWINDOWS) AND DEFINED(CPUX64)}
  Result := Boolean(GetConsoleWindow() <> 0);
  {$ENDIF}
end;

class procedure TUtils.ClearToEOL();
begin
  if not HasConsole() then Exit;
  Write(#27'[0K');
end;

class procedure  TUtils.Print();
begin
  Print('');
end;

class procedure  TUtils.PrintLn();
begin
  PrintLn('');
end;

class procedure TUtils.Print(const AText: string);
begin
  if not HasConsole() then Exit;
  Write(AText);
end;

class procedure TUtils.Print(const AText: string; const AArgs: array of const);
begin
  if not HasConsole() then Exit;
  Write(Format(AText, AArgs));
end;

class procedure TUtils.PrintLn(const AText: string);
begin
  if not HasConsole() then Exit;
  WriteLn(AText);
end;

class procedure  TUtils.PrintLn(const AText: string; const AArgs: array of const);
begin
  if not HasConsole() then Exit;
  WriteLn(Format(AText, AArgs));
end;

class procedure TUtils.Pause();
begin
  PrintLn('');
  Print('Press ENTER to continue...');
  ReadLn;
  PrintLn('');
end;

class function TUtils.AsUTF8(const AValue: string; ALength: PCardinal): Pointer;
begin
  Result := FMarshaller.AsUtf8(AValue).ToPointer;
  if Assigned(ALength) then
    ALength^ := System.AnsiStrings.StrLen(PAnsiChar(Result));

end;

class function TUtils.ToAnsi(const AValue: string): AnsiString;
var
  LBytes: TBytes;
begin
  LBytes := TEncoding.ANSI.GetBytes(AValue);
   if Length(LBytes) = 0 then
    Exit('');
  SetString(Result, PAnsiChar(@LBytes[0]), Length(LBytes));
end;

class procedure TUtils.ProcessMessages();
var
  LMsg: TMsg;
begin
  while Integer(PeekMessage(LMsg, 0, 0, 0, PM_REMOVE)) <> 0 do
  begin
    TranslateMessage(LMsg);
    DispatchMessage(LMsg);
  end;
end;

class function TUtils.RunExe(const AExe, AParams, AWorkDir: string; const AWait: Boolean; const AShowCmd: Word): Cardinal;
var
  LAppPath: string;
  LCmd: UnicodeString;
  LSI: STARTUPINFOW;
  LPI: PROCESS_INFORMATION;
  LExit: DWORD;
  LCreationFlags: DWORD;
  LWorkDirPW: PWideChar;
begin

  if AExe = '' then
    raise Exception.Create('RunExe: Executable path is empty');

  // Resolve the executable path against the workdir if only a filename was provided.
  if TPath.IsPathRooted(AExe) or (Pos('\', AExe) > 0) or (Pos('/', AExe) > 0) then
    LAppPath := AExe
  else if AWorkDir <> '' then
    LAppPath := TPath.Combine(AWorkDir, AExe)
  else
    LAppPath := AExe; // will rely on caller's current dir / PATH

  // Quote the app path and build a mutable command line.
  if AParams <> '' then
    LCmd := '"' + LAppPath + '" ' + AParams
  else
    LCmd := '"' + LAppPath + '"';
  UniqueString(LCmd);

  // Optional: ensure the exe exists when a workdir is provided.
  if (AWorkDir <> '') and (not TFile.Exists(LAppPath)) then
    raise Exception.CreateFmt('RunExe: Executable not found: %s', [LAppPath]);

  ZeroMemory(@LSI, SizeOf(LSI));
  ZeroMemory(@LPI, SizeOf(LPI));
  LSI.cb := SizeOf(LSI);
  LSI.dwFlags := STARTF_USESHOWWINDOW;
  LSI.wShowWindow := AShowCmd;

  if AWorkDir <> '' then
    LWorkDirPW := PWideChar(AWorkDir)
  else
    LWorkDirPW := nil;

  LCreationFlags := CREATE_UNICODE_ENVIRONMENT;

  // IMPORTANT: pass the resolved path in lpApplicationName so Windows won't search using the caller's current directory.
  if not CreateProcessW(
    PWideChar(LAppPath),   // lpApplicationName (explicit module path)
    PWideChar(LCmd),       // lpCommandLine (mutable, includes quoted path + params)
    nil,                   // lpProcessAttributes
    nil,                   // lpThreadAttributes
    False,                 // bInheritHandles
    LCreationFlags,        // dwCreationFlags
    nil,                   // lpEnvironment
    LWorkDirPW,            // lpCurrentDirectory (workdir for the child)
    LSI,                   // lpStartupInfo
    LPI                    // lpProcessInformation
  ) then
    raise Exception.CreateFmt('RunExe: CreateProcess failed (%d) %s', [GetLastError, SysErrorMessage(GetLastError)]);

  try
    if AWait then
    begin
      WaitForSingleObject(LPI.hProcess, INFINITE);
      LExit := 0;
      if GetExitCodeProcess(LPI.hProcess, LExit) then
        Result := LExit
      else
        raise Exception.CreateFmt('RunExe: GetExitCodeProcess failed (%d) %s', [GetLastError, SysErrorMessage(GetLastError)]);
    end
    else
      Result := 0;
  finally
    CloseHandle(LPI.hThread);
    CloseHandle(LPI.hProcess);
  end;
end;


class procedure TUtils.CaptureConsoleOutput(const ATitle: string; const ACommand: PChar; const AParameters: PChar; const AWorkDir: string; var AExitCode: DWORD; const AUserData: Pointer; const ACallback: TCaptureConsoleCallback);
const
  //CReadBuffer = 2400;
  CReadBuffer = 1024*2;
var
  saSecurity: TSecurityAttributes;
  hRead: THandle;
  hWrite: THandle;
  suiStartup: TStartupInfo;
  piProcess: TProcessInformation;
  pBuffer: array [0 .. CReadBuffer] of AnsiChar;
  dBuffer: array [0 .. CReadBuffer] of AnsiChar;
  dRead: DWORD;
  dRunning: DWORD;
  dAvailable: DWORD;
  CmdLine: string;
  LExitCode: DWORD;
  LWorkDirPtr: PChar;
  LLineAccumulator: TStringBuilder;
  LI: Integer;
  LChar: AnsiChar;
  LCurrentLine: string;
begin
  saSecurity.nLength := SizeOf(TSecurityAttributes);
  saSecurity.bInheritHandle := true;
  saSecurity.lpSecurityDescriptor := nil;
  if CreatePipe(hRead, hWrite, @saSecurity, 0) then
    try
      FillChar(suiStartup, SizeOf(TStartupInfo), #0);
      suiStartup.cb := SizeOf(TStartupInfo);
      suiStartup.hStdInput := hRead;
      suiStartup.hStdOutput := hWrite;
      suiStartup.hStdError := hWrite;
      suiStartup.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      suiStartup.wShowWindow := SW_HIDE;
      if ATitle.IsEmpty then
        suiStartup.lpTitle := nil
      else
        suiStartup.lpTitle := PChar(ATitle);
      CmdLine := ACommand + ' ' + AParameters;
      if AWorkDir <> '' then
        LWorkDirPtr := PChar(AWorkDir)
      else
        LWorkDirPtr := nil;
      if CreateProcess(nil, PChar(CmdLine), @saSecurity, @saSecurity, true, NORMAL_PRIORITY_CLASS, nil, LWorkDirPtr, suiStartup, piProcess) then
        try
          LLineAccumulator := TStringBuilder.Create;
          try
            repeat
              dRunning := WaitForSingleObject(piProcess.hProcess, 100);
              PeekNamedPipe(hRead, nil, 0, nil, @dAvailable, nil);
              if (dAvailable > 0) then
                repeat
                  dRead := 0;
                  ReadFile(hRead, pBuffer[0], CReadBuffer, dRead, nil);
                  pBuffer[dRead] := #0;
                  OemToCharA(pBuffer, dBuffer);
                  
                  // Process character-by-character to find complete lines
                  LI := 0;
                  while LI < Integer(dRead) do
                  begin
                    LChar := dBuffer[LI];
                    
                    if (LChar = #13) or (LChar = #10) then
                    begin
                      // Found line terminator - emit accumulated line if not empty
                      if LLineAccumulator.Length > 0 then
                      begin
                        LCurrentLine := LLineAccumulator.ToString();
                        LLineAccumulator.Clear();
                        
                        if Assigned(ACallback) then
                          ACallback(LCurrentLine, AUserData);
                      end;
                      
                      // Skip paired CR+LF
                      if (LChar = #13) and (LI + 1 < Integer(dRead)) and (dBuffer[LI + 1] = #10) then
                        Inc(LI);
                    end
                    else
                    begin
                      // Accumulate character
                      LLineAccumulator.Append(string(LChar));
                    end;
                    
                    Inc(LI);
                  end;
                until (dRead < CReadBuffer);
              ProcessMessages;
            until (dRunning <> WAIT_TIMEOUT);
            
            // Emit any remaining partial line
            if LLineAccumulator.Length > 0 then
            begin
              LCurrentLine := LLineAccumulator.ToString();
              if Assigned(ACallback) then
                ACallback(LCurrentLine, AUserData);
            end;

            if GetExitCodeProcess(piProcess.hProcess, LExitCode) then
            begin
              AExitCode := LExitCode;
            end;

          finally
            FreeAndNil(LLineAccumulator);
          end;
        finally
          CloseHandle(piProcess.hProcess);
          CloseHandle(piProcess.hThread);
        end;
    finally
      CloseHandle(hRead);
      CloseHandle(hWrite);
    end;
end;

class procedure TUtils.CaptureZigConsoleOutput(const ATitle: string; const ACommand: PChar; const AParameters: PChar; const AWorkDir: string; var AExitCode: DWORD; const AUserData: Pointer; const ACallback: TCaptureConsoleCallback);
const
  CReadBuffer = 1024*2;
  CProgressBuffer = 1024*16;
type
  TZigProgressNode = record
    Completed: UInt32;
    EstimatedTotal: UInt32;
    TaskName: string;
    Parent: Byte;
  end;
  TZigProgressNodeArray = array of TZigProgressNode;
  
  function IsTTY(): Boolean;
  var
    LStdOut: THandle;
    LMode: DWORD;
  begin
    LStdOut := GetStdHandle(STD_OUTPUT_HANDLE);
    Result := (LStdOut <> INVALID_HANDLE_VALUE) and GetConsoleMode(LStdOut, LMode);
  end;
  
  function BuildEnvironmentWithProgress(const AProgressHandle: THandle): Pointer;
  var
    LEnvStrings: PWideChar;
    LCurrent: PWideChar;
    LEnvList: TStringList;
    LFinalEnv: string;
    LI: Integer;
    LSize: Integer;
  begin
    Result := nil;
    LEnvList := TStringList.Create();
    try
      // Get existing environment
      LEnvStrings := GetEnvironmentStringsW();
      if LEnvStrings = nil then
        Exit;
      
      try
        LCurrent := LEnvStrings;
        while LCurrent^ <> #0 do
        begin
          LEnvList.Add(LCurrent);
          Inc(LCurrent, Length(LCurrent) + 1);
        end;
      finally
        FreeEnvironmentStringsW(LEnvStrings);
      end;
      
      // Add ZIG_PROGRESS
      LEnvList.Add('ZIG_PROGRESS=' + IntToStr(AProgressHandle));
      
      // Build double-null terminated string
      LFinalEnv := '';
      for LI := 0 to LEnvList.Count - 1 do
        LFinalEnv := LFinalEnv + LEnvList[LI] + #0;
      LFinalEnv := LFinalEnv + #0;
      
      // Allocate and copy
      LSize := Length(LFinalEnv) * SizeOf(WideChar);
      Result := AllocMem(LSize);
      Move(PWideChar(LFinalEnv)^, Result^, LSize);
    finally
      FreeAndNil(LEnvList);
    end;
  end;
  
  procedure ParseProgressMessage(const ABuffer: PByte; const ABytesRead: DWORD; var ANodes: TZigProgressNodeArray);
  var
    LLen: Byte;
    LNodeIdx: Integer;
    LOffset: Integer;
    LNameBytes: array[0..39] of Byte;
    LUtf8Bytes: TBytes;
    LNameLen: Integer;
    LI: Integer;
    LExpectedSize: DWORD;
  begin
    SetLength(ANodes, 0);
    
    if ABytesRead < 1 then
      Exit;
    
    LLen := ABuffer[0];
    if (LLen > 253) or (LLen = 0) then
      Exit;
    
    LExpectedSize := 1 + (LLen * 48) + LLen;
    if ABytesRead < LExpectedSize then
      Exit;
    
    SetLength(ANodes, LLen);
    LOffset := 1;
    
    for LNodeIdx := 0 to LLen - 1 do
    begin
      Move(ABuffer[LOffset], ANodes[LNodeIdx].Completed, 4);
      Inc(LOffset, 4);
      
      Move(ABuffer[LOffset], ANodes[LNodeIdx].EstimatedTotal, 4);
      Inc(LOffset, 4);
      
      Move(ABuffer[LOffset], LNameBytes[0], 40);
      
      LNameLen := 0;
      for LI := 0 to 39 do
      begin
        if LNameBytes[LI] = 0 then
          Break;
        Inc(LNameLen);
      end;
      
      if LNameLen > 0 then
      begin
        SetLength(LUtf8Bytes, LNameLen);
        Move(LNameBytes[0], LUtf8Bytes[0], LNameLen);
        ANodes[LNodeIdx].TaskName := TEncoding.UTF8.GetString(LUtf8Bytes);
      end
      else
        ANodes[LNodeIdx].TaskName := '';
      
      Inc(LOffset, 40);
    end;
    
    for LNodeIdx := 0 to LLen - 1 do
    begin
      ANodes[LNodeIdx].Parent := ABuffer[LOffset];
      Inc(LOffset);
    end;
  end;
  
  procedure FormatAndCallbackProgress(const ANodes: TZigProgressNodeArray; const AUserData: Pointer; const ACallback: TCaptureConsoleCallback);
  var
    LI: Integer;
    LLine: string;
    //LProgressText: string;
    LLastNode: Integer;
  begin
    if Length(ANodes) = 0 then
      Exit;
    
    if not Assigned(ACallback) then
      Exit;
    
    // Find the last non-root node with progress
    LLastNode := -1;
    for LI := High(ANodes) downto 0 do
    begin
      if (ANodes[LI].EstimatedTotal > 0) or (ANodes[LI].Completed > 0) then
      begin
        LLastNode := LI;
        Break;
      end;
    end;
    
    // If found, send just that one with progress marker
    if LLastNode >= 0 then
    begin
      if ANodes[LLastNode].EstimatedTotal > 0 then
        LLine := Format('[%d/%d] %s', [ANodes[LLastNode].Completed, ANodes[LLastNode].EstimatedTotal, ANodes[LLastNode].TaskName])
      else
        LLine := Format('[%d] %s', [ANodes[LLastNode].Completed, ANodes[LLastNode].TaskName]);
      
      // Add special marker for progress lines
      ACallback(#1 + LLine, AUserData);
    end;
  end;

var
  saSecurity: TSecurityAttributes;
  hRead: THandle;
  hWrite: THandle;
  hProgressRead: THandle;
  hProgressWrite: THandle;
  suiStartup: TStartupInfo;
  piProcess: TProcessInformation;
  pBuffer: array [0 .. CReadBuffer] of AnsiChar;
  dBuffer: array [0 .. CReadBuffer] of AnsiChar;
  progressBuffer: array [0 .. CProgressBuffer] of Byte;
  dRead: DWORD;
  dProgressRead: DWORD;
  dRunning: DWORD;
  dAvailable: DWORD;
  dProgressAvailable: DWORD;
  CmdLine: string;
  BufferList: TStringList;
  Line: string;
  LExitCode: DWORD;
  LWorkDirPtr: PChar;
  LProgressNodes: TZigProgressNodeArray;
  LEnvBlock: Pointer;
begin
  saSecurity.nLength := SizeOf(TSecurityAttributes);
  saSecurity.bInheritHandle := true;
  saSecurity.lpSecurityDescriptor := nil;
  
  hProgressRead := 0;
  hProgressWrite := 0;
  LEnvBlock := nil;
  
  if CreatePipe(hRead, hWrite, @saSecurity, 0) then
    try
      // Try to create progress pipe (optional)
      if CreatePipe(hProgressRead, hProgressWrite, @saSecurity, 0) then
      begin
        // Build environment with ZIG_PROGRESS
        LEnvBlock := BuildEnvironmentWithProgress(hProgressWrite);
      end;
      
      FillChar(suiStartup, SizeOf(TStartupInfo), #0);
      suiStartup.cb := SizeOf(TStartupInfo);
      suiStartup.hStdInput := hRead;
      suiStartup.hStdOutput := hWrite;
      suiStartup.hStdError := hWrite;
      suiStartup.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      suiStartup.wShowWindow := SW_HIDE;
      if ATitle.IsEmpty then
        suiStartup.lpTitle := nil
      else
        suiStartup.lpTitle := PChar(ATitle);
      
      // Add --color off if not TTY
      if not IsTTY() and (Pos('--color', string(AParameters)) = 0) then
        CmdLine := ACommand + ' ' + AParameters + ' --color off'
      else
        CmdLine := ACommand + ' ' + AParameters;
      
      if AWorkDir <> '' then
        LWorkDirPtr := PChar(AWorkDir)
      else
        LWorkDirPtr := nil;
      
      if CreateProcess(nil, PChar(CmdLine), @saSecurity, @saSecurity, true, NORMAL_PRIORITY_CLASS or CREATE_UNICODE_ENVIRONMENT, LEnvBlock, LWorkDirPtr, suiStartup, piProcess) then
        try
          BufferList := TStringList.Create;
          try
            repeat
              dRunning := WaitForSingleObject(piProcess.hProcess, 100);
              
              // Handle console output
              PeekNamedPipe(hRead, nil, 0, nil, @dAvailable, nil);
              if (dAvailable > 0) then
                repeat
                  dRead := 0;
                  ReadFile(hRead, pBuffer[0], CReadBuffer, dRead, nil);
                  pBuffer[dRead] := #0;
                  OemToCharA(pBuffer, dBuffer);
                  BufferList.Clear;
                  BufferList.Text := string(pBuffer);
                  for line in BufferList do
                  begin
                    if Assigned(ACallback) then
                    begin
                      ACallback(line, AUserData);
                    end;
                  end;
                until (dRead < CReadBuffer);
              
              // Handle progress pipe if available
              if hProgressRead <> 0 then
              begin
                PeekNamedPipe(hProgressRead, nil, 0, nil, @dProgressAvailable, nil);
                if (dProgressAvailable > 0) then
                begin
                  dProgressRead := 0;
                  ReadFile(hProgressRead, progressBuffer[0], CProgressBuffer, dProgressRead, nil);
                  if dProgressRead > 0 then
                  begin
                    ParseProgressMessage(@progressBuffer[0], dProgressRead, LProgressNodes);
                    FormatAndCallbackProgress(LProgressNodes, AUserData, ACallback);
                  end;
                end;
              end;
              
              ProcessMessages;
            until (dRunning <> WAIT_TIMEOUT);

            if GetExitCodeProcess(piProcess.hProcess, LExitCode) then
            begin
              AExitCode := LExitCode;
            end;

          finally
            FreeAndNil(BufferList);
          end;
        finally
          CloseHandle(piProcess.hProcess);
          CloseHandle(piProcess.hThread);
        end;
    finally
      if LEnvBlock <> nil then
        FreeMem(LEnvBlock);
      
      if hProgressRead <> 0 then
        CloseHandle(hProgressRead);
      if hProgressWrite <> 0 then
        CloseHandle(hProgressWrite);
      
      CloseHandle(hRead);
      CloseHandle(hWrite);
    end;
end;

class function TUtils.CreateDirInPath(const AFilename: string): Boolean;
var
  LPath: string;
begin
  // If AFilename is a directory, use it directly; otherwise, extract its directory part
  if TPath.HasExtension(AFilename) then
    LPath := TPath.GetDirectoryName(AFilename)
  else
    LPath := AFilename;

  if LPath.IsEmpty then
    Exit(False);

  if not TDirectory.Exists(LPath) then
    TDirectory.CreateDirectory(LPath);

  Result := True;
end;

class function TUtils.GetZigExePath(): string;
var
  LBase: string;
begin
  LBase := TPath.GetDirectoryName(ParamStr(0));
  Result := TPath.Combine(
    LBase,
    TPath.Combine('res', TPath.Combine('zig', 'zig.exe'))
  );
end;

class procedure TUtils.CopyFilePreservingEncoding(const ASourceFile, ADestFile: string);
var
  LSourceBytes: TBytes;
begin
  // Validate source file exists
  if not TFile.Exists(ASourceFile) then
    raise Exception.CreateFmt('CopyFilePreservingEncoding: Source file not found: %s', [ASourceFile]);

  // Ensure destination directory exists
  CreateDirInPath(ADestFile);

  // Read all bytes from source file
  LSourceBytes := TFile.ReadAllBytes(ASourceFile);
  
  // Write bytes to destination - this preserves EVERYTHING including BOM
  TFile.WriteAllBytes(ADestFile, LSourceBytes);
end;

class function TUtils.DetectFileEncoding(const AFilePath: string): TEncoding;
var
  LBytes: TBytes;
  LEncoding: TEncoding;
begin
  // Validate file exists
  if not TFile.Exists(AFilePath) then
    raise Exception.CreateFmt('DetectFileEncoding: File not found: %s', [AFilePath]);

  // Read a sample of bytes (first 4KB should be enough for BOM detection)
  LBytes := TFile.ReadAllBytes(AFilePath);

  if Length(LBytes) = 0 then
    Exit(TEncoding.Default);

  // Let TEncoding detect the encoding from BOM
  LEncoding := nil;
  TEncoding.GetBufferEncoding(LBytes, LEncoding, TEncoding.Default);

  Result := LEncoding;
end;

class function TUtils.EnsureBOM(const AText: string): string;
const
  UTF16_BOM = #$FEFF;
begin
  Result := AText;
  if (Length(Result) = 0) or (Result[1] <> UTF16_BOM) then
    Result := UTF16_BOM + Result;
end;

class function TUtils.EscapeString(const AText: string): string;
var
  LI: Integer;
  LChar: Char;
  LNextChar: Char;
begin
  Result := '';
  LI := 1;
  
  while LI <= Length(AText) do
  begin
    LChar := AText[LI];
    
    case LChar of
      #13: // Carriage return
        begin
          Result := Result + '\r';
          Inc(LI);
        end;
      #10: // Line feed
        begin
          Result := Result + '\n';
          Inc(LI);
        end;
      #9: // Tab
        begin
          Result := Result + '\t';
          Inc(LI);
        end;
      '"': // Quote
        begin
          Result := Result + '\"';
          Inc(LI);
        end;
      '\': // Backslash - requires look-ahead
        begin
          if LI < Length(AText) then
          begin
            LNextChar := AText[LI + 1];
            
            // Preserve valid C++ escape sequences: \x (hex), \n, \r, \t, \", \\
            if CharInSet(LNextChar, ['x', 'n', 'r', 't', '"', '\']) then
            begin
              // Valid C++ escape sequence - preserve the backslash
              Result := Result + '\';
            end
            else
            begin
              // Not a recognized escape - escape the backslash for paths etc.
              Result := Result + '\\';
            end;
          end
          else
          begin
            // Backslash at end of string - escape it
            Result := Result + '\\';
          end;
          Inc(LI);
        end;
    else
      // Regular character - append as-is
      Result := Result + LChar;
      Inc(LI);
    end;
  end;
end;

class function TUtils.GetVersionInfo(out AVersionInfo: TVersionInfo; const AFilePath: string): Boolean;
var
  LFileName: string;
  LInfoSize: DWORD;
  LHandle: DWORD;
  LBuffer: Pointer;
  LFileInfo: PVSFixedFileInfo;
  LLen: UINT;
begin
  // Initialize output
  AVersionInfo.Major := 0;
  AVersionInfo.Minor := 0;
  AVersionInfo.Patch := 0;
  AVersionInfo.Build := 0;
  AVersionInfo.VersionString := '';

  // Determine which file to query
  if AFilePath = '' then
    LFileName := ParamStr(0)
  else
    LFileName := AFilePath;

  // Get version info size
  LInfoSize := GetFileVersionInfoSize(PChar(LFileName), LHandle);
  if LInfoSize = 0 then
    Exit(False);

  // Allocate buffer and get version info
  GetMem(LBuffer, LInfoSize);
  try
    if not GetFileVersionInfo(PChar(LFileName), LHandle, LInfoSize, LBuffer) then
      Exit(False);

    // Query fixed file info
    if not VerQueryValue(LBuffer, '\', Pointer(LFileInfo), LLen) then
      Exit(False);

    // Extract version components
    AVersionInfo.Major := HiWord(LFileInfo.dwFileVersionMS);
    AVersionInfo.Minor := LoWord(LFileInfo.dwFileVersionMS);
    AVersionInfo.Patch := HiWord(LFileInfo.dwFileVersionLS);
    AVersionInfo.Build := LoWord(LFileInfo.dwFileVersionLS);

    // Format version string (Major.Minor.Patch)
    AVersionInfo.VersionString := Format('%d.%d.%d', [AVersionInfo.Major, AVersionInfo.Minor, AVersionInfo.Patch]);
    
    Result := True;
  finally
    FreeMem(LBuffer);
  end;
end;

class function TUtils.IsValidWin64PE(const AFilePath: string): Boolean;
var
  LFile: TFileStream;
  LDosHeader: TImageDosHeader;
  LPEHeaderOffset: DWORD;
  LPEHeaderSignature: DWORD;
  LFileHeader: TImageFileHeader;
begin
  Result := False;

  if not FileExists(AFilePath) then
    Exit;

  LFile := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyWrite);
  try
    // Check if file is large enough for DOS header
    if LFile.Size < SizeOf(TImageDosHeader) then
      Exit;

    // Read DOS header
    LFile.ReadBuffer(LDosHeader, SizeOf(TImageDosHeader));

    // Check DOS signature
    if LDosHeader.e_magic <> IMAGE_DOS_SIGNATURE then // 'MZ'
      Exit;

      // Validate PE header offset
    LPEHeaderOffset := LDosHeader._lfanew;
    if LFile.Size < LPEHeaderOffset + SizeOf(DWORD) + SizeOf(TImageFileHeader) then
      Exit;

    // Seek to the PE header
    LFile.Position := LPEHeaderOffset;

    // Read and validate the PE signature
    LFile.ReadBuffer(LPEHeaderSignature, SizeOf(DWORD));
    if LPEHeaderSignature <> IMAGE_NT_SIGNATURE then // 'PE\0\0'
      Exit;

   // Read the file header
    LFile.ReadBuffer(LFileHeader, SizeOf(TImageFileHeader));

    // Check if it is a 64-bit executable
    if LFileHeader.Machine <> IMAGE_FILE_MACHINE_AMD64 then   Exit;

    // If all checks pass, it's a valid Win64 PE file
    Result := True;
  finally
    LFile.Free;
  end;
end;

class procedure TUtils.UpdateIconResource(const AExeFilePath, AIconFilePath: string);
type
  TIconDir = packed record
    idReserved: Word;  // Reserved, must be 0
    idType: Word;      // Resource type, 1 for icons
    idCount: Word;     // Number of images in the file
  end;
  PIconDir = ^TIconDir;

  TGroupIconDirEntry = packed record
    bWidth: Byte;            // Width of the icon (0 means 256)
    bHeight: Byte;           // Height of the icon (0 means 256)
    bColorCount: Byte;       // Number of colors in the palette (0 if more than 256)
    bReserved: Byte;         // Reserved, must be 0
    wPlanes: Word;           // Color planes
    wBitCount: Word;         // Bits per pixel
    dwBytesInRes: Cardinal;  // Size of the image data
    nID: Word;               // Resource ID of the icon
  end;

  TGroupIconDir = packed record
    idReserved: Word;  // Reserved, must be 0
    idType: Word;      // Resource type, 1 for icons
    idCount: Word;     // Number of images in the file
    Entries: array[0..0] of TGroupIconDirEntry; // Variable-length array
  end;

  TIconResInfo = packed record
    bWidth: Byte;            // Width of the icon (0 means 256)
    bHeight: Byte;           // Height of the icon (0 means 256)
    bColorCount: Byte;       // Number of colors in the palette (0 if more than 256)
    bReserved: Byte;         // Reserved, must be 0
    wPlanes: Word;           // Color planes (should be 1)
    wBitCount: Word;         // Bits per pixel
    dwBytesInRes: Cardinal;  // Size of the image data
    dwImageOffset: Cardinal; // Offset of the image data in the file
  end;
  PIconResInfo = ^TIconResInfo;

var
  LUpdateHandle: THandle;
  LIconStream: TMemoryStream;
  LIconDir: PIconDir;
  LIconGroup: TMemoryStream;
  LIconRes: PByte;
  LIconID: Word;
  I: Integer;
  LGroupEntry: TGroupIconDirEntry;
begin

  if not FileExists(AExeFilePath) then
    raise Exception.Create('The specified executable file does not exist.');

  if not FileExists(AIconFilePath) then
    raise Exception.Create('The specified icon file does not exist.');

  LIconStream := TMemoryStream.Create;
  LIconGroup := TMemoryStream.Create;
  try
    // Load the icon file
    LIconStream.LoadFromFile(AIconFilePath);

    // Read the ICONDIR structure from the icon file
    LIconDir := PIconDir(LIconStream.Memory);
    if LIconDir^.idReserved <> 0 then
      raise Exception.Create('Invalid icon file format.');

    // Begin updating the executable's resources
    LUpdateHandle := BeginUpdateResource(PChar(AExeFilePath), False);
    if LUpdateHandle = 0 then
      raise Exception.Create('Failed to begin resource update.');

    try
      // Process each icon image in the .ico file
      LIconRes := PByte(LIconStream.Memory) + SizeOf(TIconDir);
      for I := 0 to LIconDir^.idCount - 1 do
      begin
        // Assign a unique resource ID for the RT_ICON
        LIconID := I + 1;

        // Add the icon image data as an RT_ICON resource
        if not UpdateResource(LUpdateHandle, RT_ICON, PChar(LIconID), LANG_NEUTRAL,
          Pointer(PByte(LIconStream.Memory) + PIconResInfo(LIconRes)^.dwImageOffset),
          PIconResInfo(LIconRes)^.dwBytesInRes) then
          raise Exception.CreateFmt('Failed to add RT_ICON resource for image %d.', [I]);

        // Move to the next icon entry
        Inc(LIconRes, SizeOf(TIconResInfo));
      end;

      // Create the GROUP_ICON resource
      LIconGroup.Clear;
      LIconGroup.Write(LIconDir^, SizeOf(TIconDir)); // Write ICONDIR header

      LIconRes := PByte(LIconStream.Memory) + SizeOf(TIconDir);
      // Write each GROUP_ICON entry
      for I := 0 to LIconDir^.idCount - 1 do
      begin
        // Populate the GROUP_ICON entry
        LGroupEntry.bWidth := PIconResInfo(LIconRes)^.bWidth;
        LGroupEntry.bHeight := PIconResInfo(LIconRes)^.bHeight;
        LGroupEntry.bColorCount := PIconResInfo(LIconRes)^.bColorCount;
        LGroupEntry.bReserved := 0;
        LGroupEntry.wPlanes := PIconResInfo(LIconRes)^.wPlanes;
        LGroupEntry.wBitCount := PIconResInfo(LIconRes)^.wBitCount;
        LGroupEntry.dwBytesInRes := PIconResInfo(LIconRes)^.dwBytesInRes;
        LGroupEntry.nID := I + 1; // Match resource ID for RT_ICON

        // Write the populated GROUP_ICON entry to the stream
        LIconGroup.Write(LGroupEntry, SizeOf(TGroupIconDirEntry));

        // Move to the next ICONDIRENTRY
        Inc(LIconRes, SizeOf(TIconResInfo));
      end;

      // Add the GROUP_ICON resource to the executable
      if not UpdateResource(LUpdateHandle, RT_GROUP_ICON, 'MAINICON', LANG_NEUTRAL,
        LIconGroup.Memory, LIconGroup.Size) then
        raise Exception.Create('Failed to add RT_GROUP_ICON resource.');

      // Commit the resource updates
      if not EndUpdateResource(LUpdateHandle, False) then
        raise Exception.Create('Failed to commit resource updates.');
    except
      EndUpdateResource(LUpdateHandle, True); // Discard changes on failure
      raise;
    end;
  finally
    LIconStream.Free;
    LIconGroup.Free;
  end;
end;

class procedure TUtils.UpdateVersionInfoResource(const PEFilePath: string; const AMajor, AMinor, APatch: Word; const AProductName, ADescription, AFilename, ACompanyName, ACopyright: string);
type
  { TVSFixedFileInfo }
  TVSFixedFileInfo = packed record
    dwSignature: DWORD;        // e.g. $FEEF04BD
    dwStrucVersion: DWORD;     // e.g. $00010000 for version 1.0
    dwFileVersionMS: DWORD;    // e.g. $00030075 for version 3.75
    dwFileVersionLS: DWORD;    // e.g. $00000031 for version 0.31
    dwProductVersionMS: DWORD; // Same format as dwFileVersionMS
    dwProductVersionLS: DWORD; // Same format as dwFileVersionLS
    dwFileFlagsMask: DWORD;    // = $3F for version "0011 1111"
    dwFileFlags: DWORD;        // e.g. VFF_DEBUG | VFF_PRERELEASE
    dwFileOS: DWORD;           // e.g. VOS_NT_WINDOWS32
    dwFileType: DWORD;         // e.g. VFT_APP
    dwFileSubtype: DWORD;      // e.g. VFT2_UNKNOWN
    dwFileDateMS: DWORD;       // file date
    dwFileDateLS: DWORD;       // file date
  end;

  { TStringPair }
  TStringPair = record
    Key: string;
    Value: string;
  end;

var
  LHandleUpdate: THandle;
  LVersionInfoStream: TMemoryStream;
  LFixedInfo: TVSFixedFileInfo;
  LDataPtr: Pointer;
  LDataSize: Integer;
  LStringFileInfoStart, LStringTableStart, LVarFileInfoStart: Int64;
  LStringPairs: array of TStringPair;
  LVErsion: string;
  LMajor, LMinor,LPatch: Word;
  LVSVersionInfoStart: Int64;
  LPair: TStringPair;
  LStringInfoEnd, LStringStart: Int64;
  LStringEnd, LFinalPos: Int64;
  LTranslationStart: Int64;

  procedure AlignStream(const AStream: TMemoryStream; const AAlignment: Integer);
  var
    LPadding: Integer;
    LPadByte: Byte;
  begin
    LPadding := (AAlignment - (AStream.Position mod AAlignment)) mod AAlignment;
    LPadByte := 0;
    while LPadding > 0 do
    begin
      AStream.WriteBuffer(LPadByte, 1);
      Dec(LPadding);
    end;
  end;

  procedure WriteWideString(const AStream: TMemoryStream; const AText: string);
  var
    LWideText: WideString;
  begin
    LWideText := WideString(AText);
    AStream.WriteBuffer(PWideChar(LWideText)^, (Length(LWideText) + 1) * SizeOf(WideChar));
  end;

  procedure SetFileVersionFromString(const AVersion: string; out AFileVersionMS, AFileVersionLS: DWORD);
  var
    LVersionParts: TArray<string>;
    LMajor, LMinor, LBuild, LRevision: Word;
  begin
    // Split the version string into its components
    LVersionParts := AVersion.Split(['.']);
    if Length(LVersionParts) <> 4 then
      raise Exception.Create('Invalid version string format. Expected "Major.Minor.Build.Revision".');

    // Parse each part into a Word
    LMajor := StrToIntDef(LVersionParts[0], 0);
    LMinor := StrToIntDef(LVersionParts[1], 0);
    LBuild := StrToIntDef(LVersionParts[2], 0);
    LRevision := StrToIntDef(LVersionParts[3], 0);

    // Set the high and low DWORD values
    AFileVersionMS := (DWORD(LMajor) shl 16) or DWORD(LMinor);
    AFileVersionLS := (DWORD(LBuild) shl 16) or DWORD(LRevision);
  end;

begin
  LMajor := EnsureRange(AMajor, 0, MaxWord);
  LMinor := EnsureRange(AMinor, 0, MaxWord);
  LPatch := EnsureRange(APatch, 0, MaxWord);
  LVersion := Format('%d.%d.%d.0', [LMajor, LMinor, LPatch]);

  SetLength(LStringPairs, 8);
  LStringPairs[0].Key := 'CompanyName';
  LStringPairs[0].Value := ACompanyName;
  LStringPairs[1].Key := 'FileDescription';
  LStringPairs[1].Value := ADescription;
  LStringPairs[2].Key := 'FileVersion';
  LStringPairs[2].Value := LVersion;
  LStringPairs[3].Key := 'InternalName';
  LStringPairs[3].Value := ADescription;
  LStringPairs[4].Key := 'LegalCopyright';
  LStringPairs[4].Value := ACopyright;
  LStringPairs[5].Key := 'OriginalFilename';
  LStringPairs[5].Value := AFilename;
  LStringPairs[6].Key := 'ProductName';
  LStringPairs[6].Value := AProductName;
  LStringPairs[7].Key := 'ProductVersion';
  LStringPairs[7].Value := LVersion;

  // Initialize fixed info structure
  FillChar(LFixedInfo, SizeOf(LFixedInfo), 0);
  LFixedInfo.dwSignature := $FEEF04BD;
  LFixedInfo.dwStrucVersion := $00010000;
  LFixedInfo.dwFileVersionMS := $00010000;
  LFixedInfo.dwFileVersionLS := $00000000;
  LFixedInfo.dwProductVersionMS := $00010000;
  LFixedInfo.dwProductVersionLS := $00000000;
  LFixedInfo.dwFileFlagsMask := $3F;
  LFixedInfo.dwFileFlags := 0;
  LFixedInfo.dwFileOS := VOS_NT_WINDOWS32;
  LFixedInfo.dwFileType := VFT_APP;
  LFixedInfo.dwFileSubtype := 0;
  LFixedInfo.dwFileDateMS := 0;
  LFixedInfo.dwFileDateLS := 0;

  // SEt MS and LS for FileVersion and ProductVersion
  SetFileVersionFromString(LVersion, LFixedInfo.dwFileVersionMS, LFixedInfo.dwFileVersionLS);
  SetFileVersionFromString(LVersion, LFixedInfo.dwProductVersionMS, LFixedInfo.dwProductVersionLS);

  LVersionInfoStream := TMemoryStream.Create;
  try
    // VS_VERSION_INFO
    LVSVersionInfoStart := LVersionInfoStream.Position;

    LVersionInfoStream.WriteData<Word>(0);  // Length placeholder
    LVersionInfoStream.WriteData<Word>(SizeOf(TVSFixedFileInfo));  // Value length
    LVersionInfoStream.WriteData<Word>(0);  // Type = 0
    WriteWideString(LVersionInfoStream, 'VS_VERSION_INFO');
    AlignStream(LVersionInfoStream, 4);

    // VS_FIXEDFILEINFO
    LVersionInfoStream.WriteBuffer(LFixedInfo, SizeOf(TVSFixedFileInfo));
    AlignStream(LVersionInfoStream, 4);

    // StringFileInfo
    LStringFileInfoStart := LVersionInfoStream.Position;
    LVersionInfoStream.WriteData<Word>(0);  // Length placeholder
    LVersionInfoStream.WriteData<Word>(0);  // Value length = 0
    LVersionInfoStream.WriteData<Word>(1);  // Type = 1
    WriteWideString(LVersionInfoStream, 'StringFileInfo');
    AlignStream(LVersionInfoStream, 4);

    // StringTable
    LStringTableStart := LVersionInfoStream.Position;
    LVersionInfoStream.WriteData<Word>(0);  // Length placeholder
    LVersionInfoStream.WriteData<Word>(0);  // Value length = 0
    LVersionInfoStream.WriteData<Word>(1);  // Type = 1
    WriteWideString(LVersionInfoStream, '040904B0'); // Match Delphi's default code page
    AlignStream(LVersionInfoStream, 4);

    // Write string pairs
    for LPair in LStringPairs do
    begin
      LStringStart := LVersionInfoStream.Position;

      LVersionInfoStream.WriteData<Word>(0);  // Length placeholder
      LVersionInfoStream.WriteData<Word>((Length(LPair.Value) + 1) * 2);  // Value length
      LVersionInfoStream.WriteData<Word>(1);  // Type = 1
      WriteWideString(LVersionInfoStream, LPair.Key);
      AlignStream(LVersionInfoStream, 4);
      WriteWideString(LVersionInfoStream, LPair.Value);
      AlignStream(LVersionInfoStream, 4);

      LStringEnd := LVersionInfoStream.Position;
      LVersionInfoStream.Position := LStringStart;
      LVersionInfoStream.WriteData<Word>(LStringEnd - LStringStart);
      LVersionInfoStream.Position := LStringEnd;
    end;

    LStringInfoEnd := LVersionInfoStream.Position;

    // Write StringTable length
    LVersionInfoStream.Position := LStringTableStart;
    LVersionInfoStream.WriteData<Word>(LStringInfoEnd - LStringTableStart);

    // Write StringFileInfo length
    LVersionInfoStream.Position := LStringFileInfoStart;
    LVersionInfoStream.WriteData<Word>(LStringInfoEnd - LStringFileInfoStart);

    // Start VarFileInfo where StringFileInfo ended
    LVarFileInfoStart := LStringInfoEnd;
    LVersionInfoStream.Position := LVarFileInfoStart;

    // VarFileInfo header
    LVersionInfoStream.WriteData<Word>(0);  // Length placeholder
    LVersionInfoStream.WriteData<Word>(0);  // Value length = 0
    LVersionInfoStream.WriteData<Word>(1);  // Type = 1 (text)
    WriteWideString(LVersionInfoStream, 'VarFileInfo');
    AlignStream(LVersionInfoStream, 4);

    // Translation value block
    LTranslationStart := LVersionInfoStream.Position;
    LVersionInfoStream.WriteData<Word>(0);  // Length placeholder
    LVersionInfoStream.WriteData<Word>(4);  // Value length = 4 (size of translation value)
    LVersionInfoStream.WriteData<Word>(0);  // Type = 0 (binary)
    WriteWideString(LVersionInfoStream, 'Translation');
    AlignStream(LVersionInfoStream, 4);

    // Write translation value
    LVersionInfoStream.WriteData<Word>($0409);  // Language ID (US English)
    LVersionInfoStream.WriteData<Word>($04B0);  // Unicode code page

    LFinalPos := LVersionInfoStream.Position;

    // Update VarFileInfo block length
    LVersionInfoStream.Position := LVarFileInfoStart;
    LVersionInfoStream.WriteData<Word>(LFinalPos - LVarFileInfoStart);

    // Update translation block length
    LVersionInfoStream.Position := LTranslationStart;
    LVersionInfoStream.WriteData<Word>(LFinalPos - LTranslationStart);

    // Update total version info length
    LVersionInfoStream.Position := LVSVersionInfoStart;
    LVersionInfoStream.WriteData<Word>(LFinalPos);

    LDataPtr := LVersionInfoStream.Memory;
    LDataSize := LVersionInfoStream.Size;

    // Update the resource
    LHandleUpdate := BeginUpdateResource(PChar(PEFilePath), False);
    if LHandleUpdate = 0 then
      RaiseLastOSError;

    try
      if not UpdateResourceW(LHandleUpdate, RT_VERSION, MAKEINTRESOURCE(1),
         MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL), LDataPtr, LDataSize) then
        RaiseLastOSError;

      if not EndUpdateResource(LHandleUpdate, False) then
        RaiseLastOSError;
    except
      EndUpdateResource(LHandleUpdate, True);
      raise;
    end;
  finally
    LVersionInfoStream.Free;
  end;
end;

class function  TUtils.ResourceExist(const AResName: string): Boolean;
begin
  Result := Boolean((FindResource(HInstance, PChar(AResName), RT_RCDATA) <> 0));
end;

class function TUtils.AddResManifestFromResource(const aResName: string; const aModuleFile: string; aLanguage: Integer): Boolean;
var
  LHandle: THandle;
  LManifestStream: TResourceStream;
begin
  Result := False;

  if not ResourceExist(aResName) then Exit;
  if not TFile.Exists(aModuleFile) then Exit;

  LManifestStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    LHandle := WinAPI.Windows.BeginUpdateResourceW(System.PWideChar(aModuleFile), LongBool(False));

    if LHandle <> 0 then
    begin
      Result := WinAPI.Windows.UpdateResourceW(LHandle, RT_MANIFEST, CREATEPROCESS_MANIFEST_RESOURCE_ID, aLanguage, LManifestStream.Memory, LManifestStream.Size);
      WinAPI.Windows.EndUpdateResourceW(LHandle, False);
    end;
  finally
    FreeAndNil(LManifestStream);
  end;
end;

{ TBaseObject }
constructor TBaseObject.Create();
begin
  inherited;
end;

destructor TBaseObject.Destroy();
begin
  inherited;
end;

{ TCommandBuilder }
constructor TCommandBuilder.Create();
begin
  inherited;
  
  FParams := TStringList.Create();
  FParams.Delimiter := ' ';
  FParams.StrictDelimiter := True;
end;

destructor TCommandBuilder.Destroy();
begin
  FreeAndNil(FParams);
  
  inherited;
end;

procedure TCommandBuilder.Clear();
begin
  FParams.Clear();
end;

procedure TCommandBuilder.AddParam(const AParam: string);
begin
  if AParam <> '' then
    FParams.Add(AParam);
end;

procedure TCommandBuilder.AddParam(const AFlag, AValue: string);
begin
  if AFlag <> '' then
  begin
    if AValue <> '' then
      FParams.Add(AFlag + AValue)
    else
      FParams.Add(AFlag);
  end
  else if AValue <> '' then
    FParams.Add(AValue);
end;

procedure TCommandBuilder.AddQuotedParam(const AFlag, AValue: string);
begin
  if AValue = '' then
    Exit;
  
  if AFlag <> '' then
    FParams.Add(AFlag + ' "' + AValue + '"')
  else
    FParams.Add('"' + AValue + '"');
end;

procedure TCommandBuilder.AddQuotedParam(const AValue: string);
begin
  AddQuotedParam('', AValue);
end;

procedure TCommandBuilder.AddFlag(const AFlag: string);
begin
  if AFlag <> '' then
    FParams.Add(AFlag);
end;

function TCommandBuilder.ToString(): string;
var
  LI: Integer;
begin
  if FParams.Count = 0 then
  begin
    Result := '';
    Exit;
  end;
  
  // Manually join with spaces to avoid TStringList.DelimitedText auto-quoting
  Result := FParams[0];
  for LI := 1 to FParams.Count - 1 do
    Result := Result + ' ' + FParams[LI];
end;

function TCommandBuilder.GetParamCount(): Integer;
begin
  Result := FParams.Count;
end;


procedure Startup();
var
  LPath: string;
begin
  LPath := '';
  {
  // include app dir + System32 + user dirs
  SetDefaultDllDirectories(LOAD_LIBRARY_SEARCH_DEFAULT_DIRS);

  // set custom paths
  LPath := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'res\yaml');
  AddDllDirectory(PChar(LPath));
  }
  ReportMemoryLeaksOnShutdown := True;
  TUtils.InitConsole();
end;

procedure Shutdown();
begin
end;

initialization
begin
  Startup();
end;

finalization
begin
  Shutdown();
end;

end.
