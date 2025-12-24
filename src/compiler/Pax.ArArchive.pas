{===============================================================================
  Pax™ Programming Language.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://paxlang.org

  See LICENSE for license information
===============================================================================}

unit Pax.ArArchive;

{$I Pax.Defines.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  Pax.Utils;

type
  { TArMember }
  TArMember = record
    MemberName: string;
    Data: TBytes;
  end;

  { TArSymbolEntry }
  TArSymbolEntry = record
    SymbolName: string;
    MemberIndex: Integer;
  end;

  { TArArchiveWriter }
  TArArchiveWriter = class(TBaseObject)
  private const
    AR_MAGIC = '!<arch>'#10;
    AR_HEADER_SIZE = 60;
    AR_HEADER_MAGIC = '`'#10;
    
    // ELF64 constants
    ELF_MAGIC: array[0..3] of Byte = ($7F, $45, $4C, $46); // 0x7F 'E' 'L' 'F'
    ELF_CLASS_64 = 2;
    ELF_DATA_LE = 1;
    ELF_HEADER_SIZE = 64;
    ELF_SECTION_HEADER_SIZE = 64;
    ELF_SYMBOL_SIZE = 24;
    
    SHT_SYMTAB = 2;
    STB_GLOBAL = 1;
    STB_WEAK = 2;
    SHN_UNDEF = 0;
    
  private
    FMembers: TList<TArMember>;
    FSymbols: TList<TArSymbolEntry>;
    
    function  FormatHeader(const AMemberName: string; const ASize: Int64): TBytes;
    function  PadToEven(const AData: TBytes): TBytes;
    function  ExtractElfSymbols(const AData: TBytes; const AMemberIndex: Integer): Boolean;
    function  BuildSymbolTable(const AMemberOffsets: TArray<Integer>): TBytes;
    function  ReadUInt16LE(const AData: TBytes; const AOffset: Integer): Word;
    function  ReadUInt32LE(const AData: TBytes; const AOffset: Integer): Cardinal;
    function  ReadUInt64LE(const AData: TBytes; const AOffset: Integer): UInt64;
    procedure WriteUInt32BE(var AData: TBytes; const AOffset: Integer; const AValue: Cardinal);
    function  ReadNullTermString(const AData: TBytes; const AOffset: Integer): string;
    
  public
    constructor Create(); override;
    destructor Destroy(); override;
    
    procedure Clear();
    procedure AddFile(const AFilePath: string);
    procedure AddFileFromMemory(const AMemberName: string; const AData: TBytes);
    procedure SaveToFile(const AOutputPath: string);
    function  GetMemberCount(): Integer;
    function  GetSymbolCount(): Integer;
  end;

implementation

{ TArArchiveWriter }
constructor TArArchiveWriter.Create();
begin
  inherited;
  
  FMembers := TList<TArMember>.Create();
  FSymbols := TList<TArSymbolEntry>.Create();
end;

destructor TArArchiveWriter.Destroy();
begin
  Clear();
  FreeAndNil(FSymbols);
  FreeAndNil(FMembers);
  
  inherited;
end;

procedure TArArchiveWriter.Clear();
begin
  FMembers.Clear();
  FSymbols.Clear();
end;

function TArArchiveWriter.ReadUInt16LE(const AData: TBytes; const AOffset: Integer): Word;
begin
  Result := AData[AOffset] or (AData[AOffset + 1] shl 8);
end;

function TArArchiveWriter.ReadUInt32LE(const AData: TBytes; const AOffset: Integer): Cardinal;
begin
  Result := AData[AOffset] or 
            (AData[AOffset + 1] shl 8) or 
            (AData[AOffset + 2] shl 16) or 
            (AData[AOffset + 3] shl 24);
end;

function TArArchiveWriter.ReadUInt64LE(const AData: TBytes; const AOffset: Integer): UInt64;
begin
  Result := UInt64(AData[AOffset]) or 
            (UInt64(AData[AOffset + 1]) shl 8) or 
            (UInt64(AData[AOffset + 2]) shl 16) or 
            (UInt64(AData[AOffset + 3]) shl 24) or
            (UInt64(AData[AOffset + 4]) shl 32) or
            (UInt64(AData[AOffset + 5]) shl 40) or
            (UInt64(AData[AOffset + 6]) shl 48) or
            (UInt64(AData[AOffset + 7]) shl 56);
end;

procedure TArArchiveWriter.WriteUInt32BE(var AData: TBytes; const AOffset: Integer; const AValue: Cardinal);
begin
  AData[AOffset]     := Byte((AValue shr 24) and $FF);
  AData[AOffset + 1] := Byte((AValue shr 16) and $FF);
  AData[AOffset + 2] := Byte((AValue shr 8) and $FF);
  AData[AOffset + 3] := Byte(AValue and $FF);
end;

function TArArchiveWriter.ReadNullTermString(const AData: TBytes; const AOffset: Integer): string;
var
  LI: Integer;
begin
  Result := '';
  LI := AOffset;
  while (LI < Length(AData)) and (AData[LI] <> 0) do
  begin
    Result := Result + Chr(AData[LI]);
    Inc(LI);
  end;
end;

function TArArchiveWriter.ExtractElfSymbols(const AData: TBytes; const AMemberIndex: Integer): Boolean;
var
  LElfClass: Byte;
  LElfData: Byte;
  LSectionHeaderOffset: UInt64;
  LSectionHeaderEntrySize: Word;
  LSectionHeaderCount: Word;
  LI: Integer;
  LSectionOffset: Integer;
  LSectionType: Cardinal;
  LSymTabOffset: UInt64;
  LSymTabSize: UInt64;
  LSymTabEntrySize: UInt64;
  LSymTabLink: Cardinal;
  LStrTabOffset: UInt64;
  LNumSymbols: Integer;
  LJ: Integer;
  LSymOffset: Integer;
  LStName: Cardinal;
  LStInfo: Byte;
  LStShndx: Word;
  LBinding: Byte;
  LSymbolName: string;
  LSymEntry: TArSymbolEntry;
begin
  Result := False;
  
  // Validate minimum size for ELF header
  if Length(AData) < ELF_HEADER_SIZE then
    Exit;
  
  // Check ELF magic
  if (AData[0] <> ELF_MAGIC[0]) or (AData[1] <> ELF_MAGIC[1]) or
     (AData[2] <> ELF_MAGIC[2]) or (AData[3] <> ELF_MAGIC[3]) then
    Exit;
  
  // Verify 64-bit ELF
  LElfClass := AData[4];
  if LElfClass <> ELF_CLASS_64 then
    Exit;
  
  // Verify little-endian
  LElfData := AData[5];
  if LElfData <> ELF_DATA_LE then
    Exit;
  
  // Read section header info from ELF header
  LSectionHeaderOffset := ReadUInt64LE(AData, 40);     // e_shoff
  LSectionHeaderEntrySize := ReadUInt16LE(AData, 58);  // e_shentsize
  LSectionHeaderCount := ReadUInt16LE(AData, 60);      // e_shnum
  
  // Validate
  if (LSectionHeaderOffset = 0) or (LSectionHeaderCount = 0) then
    Exit(True); // Valid ELF but no sections
  
  // Find the symbol table section
  LSymTabOffset := 0;
  LSymTabSize := 0;
  LSymTabEntrySize := 0;
  LSymTabLink := 0;
  
  for LI := 0 to LSectionHeaderCount - 1 do
  begin
    LSectionOffset := Integer(LSectionHeaderOffset) + (LI * LSectionHeaderEntrySize);
    
    if LSectionOffset + Integer(LSectionHeaderEntrySize) > Length(AData) then
      Break;
    
    LSectionType := ReadUInt32LE(AData, LSectionOffset + 4);  // sh_type
    
    if LSectionType = SHT_SYMTAB then
    begin
      LSymTabOffset := ReadUInt64LE(AData, LSectionOffset + 24);    // sh_offset
      LSymTabSize := ReadUInt64LE(AData, LSectionOffset + 32);      // sh_size
      LSymTabLink := ReadUInt32LE(AData, LSectionOffset + 40);      // sh_link (string table index)
      LSymTabEntrySize := ReadUInt64LE(AData, LSectionOffset + 56); // sh_entsize
      Break;
    end;
  end;
  
  // No symbol table found
  if (LSymTabOffset = 0) or (LSymTabEntrySize = 0) then
    Exit(True);
  
  // Get string table offset
  if LSymTabLink >= LSectionHeaderCount then
    Exit(True);
  
  LSectionOffset := Integer(LSectionHeaderOffset) + (Integer(LSymTabLink) * LSectionHeaderEntrySize);
  LStrTabOffset := ReadUInt64LE(AData, LSectionOffset + 24);  // sh_offset of string table
  
  // Parse symbols
  LNumSymbols := Integer(LSymTabSize div LSymTabEntrySize);
  
  for LJ := 0 to LNumSymbols - 1 do
  begin
    LSymOffset := Integer(LSymTabOffset) + (LJ * Integer(LSymTabEntrySize));
    
    if LSymOffset + ELF_SYMBOL_SIZE > Length(AData) then
      Break;
    
    LStName := ReadUInt32LE(AData, LSymOffset);       // st_name
    LStInfo := AData[LSymOffset + 4];                 // st_info
    LStShndx := ReadUInt16LE(AData, LSymOffset + 6);  // st_shndx
    
    // Extract binding from st_info (high 4 bits)
    LBinding := LStInfo shr 4;
    
    // We want GLOBAL or WEAK symbols that are DEFINED (section != 0)
    if ((LBinding = STB_GLOBAL) or (LBinding = STB_WEAK)) and (LStShndx <> SHN_UNDEF) then
    begin
      // Get symbol name from string table
      if LStName > 0 then
      begin
        LSymbolName := ReadNullTermString(AData, Integer(LStrTabOffset) + Integer(LStName));
        
        if LSymbolName <> '' then
        begin
          LSymEntry.SymbolName := LSymbolName;
          LSymEntry.MemberIndex := AMemberIndex;
          FSymbols.Add(LSymEntry);
        end;
      end;
    end;
  end;
  
  Result := True;
end;

function TArArchiveWriter.BuildSymbolTable(const AMemberOffsets: TArray<Integer>): TBytes;
var
  LSymCount: Integer;
  LNamesSize: Integer;
  LTotalSize: Integer;
  LOffset: Integer;
  LI: Integer;
  LNameBytes: TBytes;
begin
  LSymCount := FSymbols.Count;
  
  if LSymCount = 0 then
  begin
    SetLength(Result, 0);
    Exit;
  end;
  
  // Calculate names section size
  LNamesSize := 0;
  for LI := 0 to FSymbols.Count - 1 do
    LNamesSize := LNamesSize + Length(FSymbols[LI].SymbolName) + 1; // +1 for null terminator
  
  // Total size: 4 (count) + 4*count (offsets) + names
  LTotalSize := 4 + (4 * LSymCount) + LNamesSize;
  SetLength(Result, LTotalSize);
  
  // Write symbol count (big-endian)
  WriteUInt32BE(Result, 0, Cardinal(LSymCount));
  
  // Write offsets (big-endian) - offset to the member header for each symbol
  LOffset := 4;
  for LI := 0 to FSymbols.Count - 1 do
  begin
    WriteUInt32BE(Result, LOffset, Cardinal(AMemberOffsets[FSymbols[LI].MemberIndex]));
    Inc(LOffset, 4);
  end;
  
  // Write symbol names (null-terminated)
  for LI := 0 to FSymbols.Count - 1 do
  begin
    LNameBytes := TEncoding.ASCII.GetBytes(FSymbols[LI].SymbolName);
    if Length(LNameBytes) > 0 then
    begin
      Move(LNameBytes[0], Result[LOffset], Length(LNameBytes));
      Inc(LOffset, Length(LNameBytes));
    end;
    Result[LOffset] := 0; // Null terminator
    Inc(LOffset);
  end;
end;

function TArArchiveWriter.FormatHeader(const AMemberName: string; const ASize: Int64): TBytes;
var
  LHeader: AnsiString;
  LName: AnsiString;
  LDate: AnsiString;
  LUid: AnsiString;
  LGid: AnsiString;
  LMode: AnsiString;
  LSizeStr: AnsiString;
  LI: Integer;
begin
  // Build each field with proper padding
  // Name: 16 bytes, space-padded, ends with '/'
  LName := AnsiString(TPath.GetFileName(AMemberName));
  if Length(LName) > 15 then
    LName := Copy(LName, 1, 15);
  LName := LName + '/';
  while Length(LName) < 16 do
    LName := LName + ' ';
  
  // Date: 12 bytes, decimal seconds since epoch (use 0 for deterministic builds)
  LDate := '0';
  while Length(LDate) < 12 do
    LDate := LDate + ' ';
  
  // UID: 6 bytes
  LUid := '0';
  while Length(LUid) < 6 do
    LUid := LUid + ' ';
  
  // GID: 6 bytes
  LGid := '0';
  while Length(LGid) < 6 do
    LGid := LGid + ' ';
  
  // Mode: 8 bytes, octal (644 = rw-r--r--)
  LMode := '100644';
  while Length(LMode) < 8 do
    LMode := LMode + ' ';
  
  // Size: 10 bytes, decimal
  LSizeStr := AnsiString(IntToStr(ASize));
  while Length(LSizeStr) < 10 do
    LSizeStr := LSizeStr + ' ';
  
  // Combine: Name(16) + Date(12) + UID(6) + GID(6) + Mode(8) + Size(10) + Magic(2) = 60
  LHeader := LName + LDate + LUid + LGid + LMode + LSizeStr + AnsiString(AR_HEADER_MAGIC);
  
  // Convert to bytes
  SetLength(Result, AR_HEADER_SIZE);
  for LI := 1 to AR_HEADER_SIZE do
    Result[LI - 1] := Byte(LHeader[LI]);
end;

function TArArchiveWriter.PadToEven(const AData: TBytes): TBytes;
begin
  Result := AData;
  
  // AR format requires each member to start at an even offset
  if (Length(AData) mod 2) <> 0 then
  begin
    SetLength(Result, Length(AData) + 1);
    Move(AData[0], Result[0], Length(AData));
    Result[Length(Result) - 1] := $0A; // Newline padding
  end;
end;

procedure TArArchiveWriter.AddFile(const AFilePath: string);
var
  LData: TBytes;
  LMember: TArMember;
  LMemberIndex: Integer;
begin
  if not TFile.Exists(AFilePath) then
    raise Exception.CreateFmt('TArArchiveWriter.AddFile: File not found: %s', [AFilePath]);
  
  LData := TFile.ReadAllBytes(AFilePath);
  
  LMember.MemberName := TPath.GetFileName(AFilePath);
  LMember.Data := LData;
  
  LMemberIndex := FMembers.Count;
  FMembers.Add(LMember);
  
  // Extract ELF symbols for the symbol table
  ExtractElfSymbols(LData, LMemberIndex);
end;

procedure TArArchiveWriter.AddFileFromMemory(const AMemberName: string; const AData: TBytes);
var
  LMember: TArMember;
  LMemberIndex: Integer;
begin
  LMember.MemberName := AMemberName;
  LMember.Data := AData;
  
  LMemberIndex := FMembers.Count;
  FMembers.Add(LMember);
  
  // Extract ELF symbols for the symbol table
  ExtractElfSymbols(AData, LMemberIndex);
end;

procedure TArArchiveWriter.SaveToFile(const AOutputPath: string);
var
  LStream: TFileStream;
  LMagicBytes: TBytes;
  LHeader: TBytes;
  LPaddedData: TBytes;
  LMember: TArMember;
  LI: Integer;
  LSymTableData: TBytes;
  LSymTablePadded: TBytes;
  LMemberOffsets: TArray<Integer>;
  LCurrentOffset: Integer;
begin
  if FMembers.Count = 0 then
    raise Exception.Create('TArArchiveWriter.SaveToFile: No members to write');
  
  // Ensure output directory exists
  TUtils.CreateDirInPath(AOutputPath);
  
  // First pass: calculate member offsets (needed for symbol table)
  // Offset calculation must account for symbol table being first
  SetLength(LMemberOffsets, FMembers.Count);
  
  // Start after magic (8 bytes)
  LCurrentOffset := Length(AR_MAGIC);
  
  // If we have symbols, symbol table comes first
  if FSymbols.Count > 0 then
  begin
    // We need a temporary calculation to get symbol table size
    // Use placeholder offsets first, then recalculate
    for LI := 0 to FMembers.Count - 1 do
      LMemberOffsets[LI] := 0; // Placeholder
    
    LSymTableData := BuildSymbolTable(LMemberOffsets);
    LSymTablePadded := PadToEven(LSymTableData);
    
    // Symbol table header + data
    LCurrentOffset := LCurrentOffset + AR_HEADER_SIZE + Length(LSymTablePadded);
  end;
  
  // Calculate actual member offsets
  for LI := 0 to FMembers.Count - 1 do
  begin
    LMemberOffsets[LI] := LCurrentOffset;
    LPaddedData := PadToEven(FMembers[LI].Data);
    LCurrentOffset := LCurrentOffset + AR_HEADER_SIZE + Length(LPaddedData);
  end;
  
  // Rebuild symbol table with correct offsets
  if FSymbols.Count > 0 then
    LSymTableData := BuildSymbolTable(LMemberOffsets);
  
  // Now write the archive
  LStream := TFileStream.Create(AOutputPath, fmCreate);
  try
    // Write AR magic signature
    LMagicBytes := TEncoding.ASCII.GetBytes(AR_MAGIC);
    LStream.WriteBuffer(LMagicBytes[0], Length(LMagicBytes));
    
    // Write symbol table as first member (if we have symbols)
    if FSymbols.Count > 0 then
    begin
      LSymTablePadded := PadToEven(LSymTableData);
      
      // Symbol table uses special name '/'
      LHeader := FormatHeader('/', Length(LSymTableData));
      // Fix the name field to just '/' padded with spaces (not '//') 
      LHeader[0] := Ord('/');
      LHeader[1] := Ord(' ');
      
      LStream.WriteBuffer(LHeader[0], Length(LHeader));
      if Length(LSymTablePadded) > 0 then
        LStream.WriteBuffer(LSymTablePadded[0], Length(LSymTablePadded));
    end;
    
    // Write each member
    for LI := 0 to FMembers.Count - 1 do
    begin
      LMember := FMembers[LI];
      
      // Write 60-byte header
      LHeader := FormatHeader(LMember.MemberName, Length(LMember.Data));
      LStream.WriteBuffer(LHeader[0], Length(LHeader));
      
      // Write file data (padded to even boundary)
      LPaddedData := PadToEven(LMember.Data);
      if Length(LPaddedData) > 0 then
        LStream.WriteBuffer(LPaddedData[0], Length(LPaddedData));
    end;
  finally
    FreeAndNil(LStream);
  end;
end;

function TArArchiveWriter.GetMemberCount(): Integer;
begin
  Result := FMembers.Count;
end;

function TArArchiveWriter.GetSymbolCount(): Integer;
begin
  Result := FSymbols.Count;
end;

end.
