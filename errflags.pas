{$A+,B-,D-,I+,O-,R-,S-,V-}
{$IFDEF VER60}
   {$X-}
{$ENDIF}
{$IFDEF VER70}
   {$P-,Q-,T-,X-,Y-}
{$ENDIF}
{$IFDEF OS2}
   {$G+,P-,Q-,T-,X-,Y-}
   {$M 8192,8192}
{$ELSE}
   {$IFDEF WIN32}
      {$G+,P-,Q-,T-,X-,Y-}
      {$M 8192,8192}
   {$ELSE}
      {$M 8192,0,65536}
   {$ENDIF}
{$ENDIF}

program ErrFlags;

(* ErrFlags v2.17  //  Checks nodelist segments for flag errors.
   Copyright 1995-1997 jonny bergdahl data AB. Freeware. All rights deserved.
   Modifications (c) 1999-2006 by Johan Zwiekhorst, 2:292/100
   Modifications (c) 2017 by Niels Joncheere, 2:292/789                       *)

(* Revision history                                                         *)
(* Date     Version  Change                                                 *)
(*   960127 2.1      Increased limit of inbound files to 80                 *)
(*                   Implementation of a 'NoTouch' mode                     *)
(*   960310 2.2      Implemented stripping of duplicate flags               *)
(*   960327 2.3      Implemented support for arced files                    *)
(*                   Bugfix for user flag only nodes                        *)
(*                   Minor bugs fixed                                       *)
(*   960415 2.4      Fixed bug with inbound path not being used.            *)
(*                   Fixed bug with reports being sent to wrong recipient   *)
(*                   Removed support for REPORTALL and added support for    *)
(*                   a NOERR notification key word.                         *)
(*   960528 2.5      Fixed a minor bug regarding input path with no-arc     *)
(*                   Fixed problem with command line capitalizing           *)
(*   970129 2.6      Fixed handling of zone mail flags                      *)
(* Since Jonny Bergdahl is no longer in Fidonet, I (JZ) will take over      *)
(* starting with version 2.7 on 1999 April 5th                              *)
(*   990405 2.7 !JZ  Fixed some problems with number and valid. of flags    *)
(*   MM0717 2.8      Added check for nodelist entry length <= 157;          *)
(*                   Flag checking is now case-insensitive and flags are    *)
(*                   made uppercase while their parameters are left alone   *)
(*   MM0805 2.9      Added baudrate parameter checking                      *)
(*                   Added checking for 'Pvt' and '-unpublished-' pairing   *)
(*                   Added checking for valid nodelist entry prefix         *)
(*   MM0811 2.10     Added check for proper phonenumber                     *)
(*                   Some cosmetic changes                                  *)
(*   MM0901 2.11     Added DELENTRY feature                                 *)
(* 20010414 2.12     Added removal of spaces and tail commas                *)
(*                   Added recalculation of CRC value                       *)
(* 20010422 2.13     Header line with CRC is added if it isn't there        *)
(* 20010428 2.14     Made indications for space and tail comma removal      *)
(*                   more informative for coordinators                      *)
(* 20060525 2.15     Increase user flag length from 32 to 40 chars          *)
(* Since Johan Zwiekhorst is no longer active in FidoNet, Niels Joncheere     *)
(* will take over development from version 2.16 onwards.                      *)
(* 20170924 2.16     Removed check for presence of Pvt prefix if phone number *)
(*                   is -Unpublished-.                                        *)
(* 20171023 2.17     Added support for a realistic Linux setup.               *)

{$IFDEF LINUX}
Uses dos, process, unix;
{$ELSE}
Uses dos;
{$ENDIF}

Const
	ErrFlagsVersion = '2.17';
	DEFAULT_CTL_FILE_NAMES : Array[1..2] of String = ('ErrFlags.ctl', 'errflags.ctl');
	DEFAULT_TAB_FILE_NAMES : Array[1..2] of String = ('ErrFlags.tab', 'errflags.tab');
	DEFAULT_CMT_FILE_NAMES : Array[1..2] of String = ('ErrFlags.cmt', 'errflags.cmt');
	FILE_SEPARATOR : String = '/';

  Unpub       = '-Unpublished-';

  MaxBaudNum  = 12;
{$IFDEF OS2}
  MaxFlags    = 200;                               (* !! 2.11 MM0901 *)
{$ELSE}
  {$IFDEF WIN32}
    MaxFlags  = 200;
  {$ELSE}
    MaxFlags  = 160;                               (* !! 2.15 060625 otherwise more than 64K variables in DOS *)
  {$ENDIF}
{$ENDIF}
  MaxDelEntry = 20;

Type
  TSegmentFile = record
                   FileName : String[13];
                   RptFile  : String[255];
                   Notifier : String[24];
                 end;
  TFlag        = string[40];                      (* !! 2.7 990405, was 15 !! 2.15 060525, was 32 *)
  TConvFlag    = record
                   First    : TFlag;
                   Last     : TFlag;
                 end;

Var
  RptFile  : Text;
  ctlFileName,
  tabFileName,
  cmtFileName : String[255];
  ctlFile,
  tabFile,
  cmtFile : Text;
  DefaultZone,
  ThisZone    : Word;
  DefaultNet,
  ThisNet     : Word;
  SegmentFile : Array[1..MaxFlags] of TSegmentFile;    (* !! 2.1 960127 !! 2.7 990405: 200 iso 80 *)
  SegmentNum  : Byte;
  OldDir,
  InboundPath,
  NotifyPath,
  NotifyCmd,
  NoErrCmd,
  UnCompress  : String;
  ExecutePath,
  ExecuteCmd  : String;
  Baudrates   : Array[1..MaxBaudNum] of Word;        (* !! 2.9 MM0805 *)
  ApprFlags   : Array[1..MaxFlags] of TFlag;
  CatFlags    : Array[1..MaxFlags] of TFlag;         (* !! 2.6 970129 !! 2.7 990405 *)
  UserFlags   : Array[1..MaxFlags] of TFlag;
  ConvFlags   : Array[1..MaxFlags] of TConvFlag;
  ReduntFlags : Array[1..MaxFlags] of TConvFlag;
  DelEntry    : Array[1..MaxDelEntry] of TFlag;
  BaudDefault : Word;                                (* !! 2.9  MM0805 *)
  BaudNum,                                           (* !! 2.9  MM0805 *)
  DelEntryNum,                                       (* !! 2.11 MM0901 *)
  ApprNum,
  CatNum,                                            (* !! 2.6  970129 *)
  UserNum,
  ConvNum,
  ReduntNum   : Byte;
  TotPrefixErr,                                  (* !! 2.9  MM0805 *)
  TotPvtErr,                                     (* !! 2.9  MM0805 *)
  TotPhoneErr,                                   (* !! 2.10 MM0811 *)
  TotBaudErr,                                    (* !! 2.9  MM0805 *)
  TotFlagErr,
  TotUserErr,
  TotReduErr,
  TotDupErr,
  TotCaseConv,                                   (* !! 2.8  MM0717 *)
  TotTailCommas,                                 (* !! 2.14 20010428 *)
  TotSpaces,                                     (* !! 2.14 20010428 *)
  ThisPrefixErr,                                 (* !! 2.9  MM0805 *)
  ThisPvtErr,                                    (* !! 2.9  MM0805 *)
  ThisPhoneErr,                                  (* !! 2.10 MM0811 *)
  ThisBaudErr,                                   (* !! 2.9  MM0805 *)
  ThisFlagErr,
  ThisUserErr,
  ThisReduErr,
  ThisDupErr,
  ThisCaseConv,                                  (* !! 2.8 MM0717 *)
  ThisTailCommas,                                (* !! 2.14 20010428 *)
  ThisSpaces  : Word;                            (* !! 2.14 20010428 *)

  LastErr     : Word;
  Touch,                                         (* !! 2.1 960127 *)
  AnyProcessed: Boolean;


  (* Generic functions                                                 *)

function LastError : Word;   (* Saves the last I/O error code *)

  begin
    LastErr := IOresult;
    LastError := LastErr;
  end;

function Upper(Ins: String): String;   (* Uppercase string conversion *)
  Var
    L : Integer;
  begin
    For L := 1 to length(Ins) do
      Ins[L] := UpCase(Ins[L]);
    Upper := Ins;
  end;

function Proper(Ins: String): String;  (* Lowercase string conversion with 1st char Uppercase *)
  Var
    L : Integer;
  begin
    Ins[1] := UpCase(Ins[1]);
    for L := 2 to length(Ins) do
       Ins[L] := Char(Byte(Ins[L]) or $20);  (* quick & dirty *)
    Proper := Ins;
  end;


function StrToWord(InputString : String) : Word;  (* String to Word *)
var
	OutputWord : Word;
{$IFDEF OS2}                            (* !! 2.8 MM0717 *)
	Code : LongInt;
{$ELSE}
	{$IFDEF WIN32}                      (* !! 2.14 20010428 *)
	Code : LongInt;
	{$ELSE}
	Code : Integer;
	{$ENDIF}
{$ENDIF}
begin
	Val(InputString, OutputWord, Code);
	if Code <> 0 then
		OutputWord := 0;
	StrToWord := OutputWord;
end;

function WordToStr(InputWord : Word) : String;    (* Word to String *)
var
	OutputString : String[12];
begin
	Str(InputWord, OutputString);
	WordToStr := OutputString;
end;

function AddFileSeparator(s : String) : String;
begin
	if (s = '') or (s[length(s)] = FILE_SEPARATOR) then
		AddFileSeparator := s
	else
		AddFileSeparator := s + FILE_SEPARATOR;
end;

function FirstWord(var Instr:String):String;
                             (* This function returns the first word in a *)
                             (* string, and removes it from the original  *)
                             (* string. Used to read the setup files      *)
  Var
    Position : Byte;

  begin
    Position  := 1;
    while ((Instr[Position]<>' ') and (Instr[Position]<>#9) and
           (Position <= Length(InStr))) do Inc(Position);
    FirstWord := Copy(InStr,1,Pred(Position));
    while (((Instr[Position]=' ') or (Instr[Position]=#9)) and
           (Position<=Length(InStr))) do Inc(Position);
    InStr := Copy(InStr,Position,255);
  end;

function Extract(Var Line:String):String;
                             (* This function works as the above, but for *)
                             (* comma separated files like the nodelist   *)
  begin
    If pos(',',Line)<>0 then
      begin
        Extract := Copy(Line,1,Pred(Pos(',',Line)));
        Delete(Line,1,Pos(',',Line));
      end
    else
      begin
        Extract := Line;
        Line := '';
      end;
  end;

function StripStr(Line: String):String;

  begin
    While Line[1]=' ' do
      Delete(Line,1,1);
    While Line[Length(Line)]=' ' do
      Delete(Line,Length(Line),1);
    StripStr := Line;
  end;


procedure Calculate_CRC (var CRC: word; b: byte);       (* !! 2.12  20010414 *)

  begin
    { Polynomial = $1021  (x^16 + x^12 + x^5 + x^0) }
    CRC := Swap(CRC) xor b;
    CRC := CRC xor (Lo(CRC) shr 4);
    CRC := CRC xor (Swap(Lo(CRC)) shl 4) xor (Lo(CRC) shl 5)
  end;


function JulianDayNumber (D, M, Y: Word): LongInt;      (* !! 2.13  20010424 *)
 { convert day, month, year to JDN }

  var
    n, z    : LongInt;

  begin
   n := (integer(M) - 14) div 12;
   z := Y + 4800 + n;
   JulianDayNumber :=    1461 * z div 4
                      +   367 * (integer(M) - 2 - n * 12) div 12
                      -     3 * ((z + 100) div 100) div 4
                      - 32074 + D
  end;


procedure JulianToDate (JDN: LongInt; var Day, Month, Year: Word);   (* !! 2.13  20010424 *)
 { convert JDN to day, month, year }

 var
   M, Y, x, z : LongInt;

 begin
   x := JDN + 68568;
   z := 4 * x div 146097;
   x := x - (146097 * z + 3) div 4;
   Y := 4000 * (x + 1) div 1461001;
   x := x - 1461 * Y div 4 + 31;
   M := 80 * x div 2447;
   Day := x - 2447 * M div 80;
   x := M div 11;
   Month := M + 2 - 12 * x;
   Year := 100 * (z - 49) + Y + x
  end;


  (* Program specific functions and procedures                           *)

procedure SignOn;     (* Initialises the program and extracts parameters *)

  Var
    StdOut : File;
    Temp   : String;

  begin
    Assign(StdOut,'');
    Rewrite(StdOut);
    WriteLn('ErrFlags v', ErrFlagsVersion, '  //  Checks nodelist segments for flag errors.');
    WriteLn('Copyright 1995-1997 jonny bergdahl data AB. Freeware. All rights deserved.');
    WriteLn('Modifications (c) 1999-2006 by Johan Zwiekhorst, 2:292/100');
    WriteLn('Modifications (c) 2017 by Niels Joncheere, 2:292/789');
    WriteLn;
    Temp := Upper(ParamStr(1));
    If (Temp[2]='?') then
      begin
        WriteLn('Syntax:');
        WriteLn;
        WriteLn('ERRFLAGS [control file]');
        WriteLn;
        WriteLn('    control file  Optional control file name. Can be used when processing');
        WriteLn('                  segments in different zones.');
        WriteLn('                  Default file name is ERRFLAGS.CTL');
        Halt(0);
      end;
    If ParamStr(1)<>'' then
      CTLFileName := ParamStr(1);
    GetDir(0,OldDir);
  end;

function OpenConfigFileHelper(configFileNames : Array of String; var configFileName : String; var configFile : Text; haltOnError : Boolean) : Boolean;
	var
		i     : Integer;
		error : Boolean;
	begin
		for i := 1 to length(configFileNames) do
			begin
				configFileName := configFileNames[i - 1];
				Assign(configFile, configFileName);
				{$I-}
				Reset(configFile);
				{$I+}
				if lastError = 0 then
					begin
						error := false;
						break;
					end
				else
					error := true;
			end;
		if error and haltOnError then
			begin
				WriteLn('! Unable to open ', configFileNames[0], ' - exiting...');
				Halt(1);
			end;
		OpenConfigFileHelper := error;
	end;

function OpenConfigFile(defaultConfigFileNames : Array of String; var configFileName : String; var configFile : Text; haltOnError : Boolean) : Boolean;
	var
		error : Boolean;
	begin
		if configFileName = '' then
			error := OpenConfigFileHelper(defaultConfigFileNames, configFileName, configFile, haltOnError)
		else
			error := OpenConfigFileHelper([configFileName], configFileName, configFile, haltOnError);
		OpenConfigFile := error;
	end;

procedure ExecuteLinuxCommand(var cmd : String);
	var
		proc : TProcess;
	begin
		proc := TProcess.Create(nil);
		proc.CommandLine := ((cmd));
		proc.Options := [poUsePipes, poWaitOnExit];
		proc.Execute;
	end;

procedure ExecuteDosCommand(var cmd : String);
	begin
		Exec(GetEnv('COMSPEC'), '/C ' + cmd);
	end;

procedure ExecuteCommand(lbl : String; var cmd : String; var oldDir : String);
	begin
		WriteLn('# ', lbl, ':');
		WriteLn('  ', cmd);
{$IFDEF LINUX}
		ExecuteLinuxCommand(cmd);
{$ELSE}
		ExecuteDosCommand(cmd);
{$ENDIF}
		ChDir(oldDir);
	end;

procedure ParseCTLfile;    (* Parses the configuration file  *)

  Var
    Temp1   : String;
    Temp2   : String;

  begin
    SegmentNum := 0;
    ExecutePath := '';
    ExecuteCmd := '';
    NotifyPath := '';
    NotifyCmd := '';
    NoErrCmd := '';
    Touch := True;                          (* !! 960127 *)
	OpenConfigFile(DEFAULT_CTL_FILE_NAMES, ctlFileName, ctlFile, true);
    While not EOF(CTLFile) do
      begin
        {$I-}
        ReadLn(CTLFile,Temp1);
        {$I+}
        If LastError<>0 then
          begin
            WriteLn('! Error reading ',CTLFileName,' - exiting...');
            Halt(1);
          end;
        If Temp1[1]<>';' then
          begin
            Temp2 := Firstword(Temp1);
            If Upper(Temp2)='ZONE' then
              DefaultZone := StrToWord(StripStr(Temp1));
            If Upper(Temp2)='NET' then
              DefaultNet := StrToWord(StripStr(Temp1));
            If Upper(Temp2)='NOTOUCH' then          (* !! 960127 *)
              Touch := False;
            If Upper(Temp2)='FILE' then
              begin
                Inc(SegmentNum);
                With SegmentFile[SegmentNum] do
                  begin
                    FileName := FirstWord(Temp1);
                    Notifier := FirstWord(Temp1);
                    RptFile  := FirstWord(Temp1);
                    If RptFile='' then
                      RptFile := Copy(FileName,1,Pos('.',FileName))+'RPT';
                  end;
              end;
            If Upper(Temp2)='INBOUND' then
              InboundPath := StripStr(Temp1);
            If Upper(Temp2)='NOTIFY' then
              NotifyCmd := StripStr(Temp1);
            If Upper(Temp2)='NOERR' then
              NoErrCmd := StripStr(Temp1);
            If Upper(Temp2)='NOTIFYPATH' then
              NotifyPath := StripStr(Temp1);
            If Upper(Temp2)='EXECUTE' then
              ExecuteCmd := StripStr(Temp1);
            If Upper(Temp2)='EXECUTEPATH' then
              ExecutePath := StripStr(Temp1);
            If Upper(Temp2)='TABFILE' then
              TABFileName := StripStr(Temp1);
            If Upper(Temp2)='CMTFILE' then
              CMTFileName := StripStr(Temp1);
            If Upper(Temp2)='UNCOMPRESS' then
              UnCompress := StripStr(Temp1);
          end;
      end;
   inboundPath := AddFileSeparator(inboundPath);
   close(CTLFile);
 end;

procedure ParseTabFile;    (* Parses the approved flag file *)

  Var
    Temp1,
    Temp2,
    UTemp2  : String;

  begin
    BaudDefault := 9600;               (* !! 2.9  MM0805 *)
    BaudNum := 0;                      (* !! 2.9  MM0805 *)
    DelEntryNum := 0;                  (* !! 2.11 MM0901 *)
    ApprNum := 0;
    UserNum := 0;
    ConvNum := 0;                      (* !! 2.8  MM0717 *)
    ReduntNum := 0;
    FillChar(DelEntry,sizeof(DelEntry),0); (* !! 2.11 MM0901 *)
	OpenConfigFile(DEFAULT_TAB_FILE_NAMES, tabFileName, tabFile, true);
    While not EOF(TabFile) do
      begin
        {$I-}
        ReadLn(TabFile,Temp1);
        {$I+}
        If LastError<>0 then
          begin
            WriteLn('! Error reading ',TabFileName,' - exiting...');
            Halt(1);
          end;
        If Temp1[1]<>';' then
          begin
            Temp2 := Firstword(Temp1);
            UTemp2 := Upper(Temp2);
            if UTemp2='BAUDDEFAULT' then                 (* !! 2.9 MM0805 *)
              BaudDefault := StrToWord(FirstWord(Temp1));
            if UTemp2='BAUDRATE' then                    (* !! 2.9 MM0805 *)
              while Temp1<>'' do
                begin
                  Inc(BaudNum);
                  If BaudNum <= MaxBaudNum then
                    Baudrates[BaudNum] := StrToWord(FirstWord(Temp1))
                  else begin (* Too many baudrates specified! *)
                        WriteLn('! More than ',MaxBaudNum,' baudrates specified in TAB, ignoring the rest...');
                        BREAK
                       end
                end;
            If UTemp2='FLAGS' then
              while Temp1<>'' do
                begin
                  Inc(ApprNum);
                  ApprFlags[ApprNum] := Upper(FirstWord(Temp1));
                end;
            If UTemp2='CFLAGS' then           (* !! 2.6 970129 *)
              while Temp1<>'' do
                begin
                  Inc(CatNum);
                  CatFlags[CatNum] := Upper(FirstWord(Temp1));
                end;
            If UTemp2='USER' then
              while Temp1<>'' do
                begin
                  Inc(UserNum);
                  UserFlags[UserNum] := Upper(FirstWord(Temp1));
                end;
            If UTemp2='CONVERT' then
              while Temp1<>'' do
                begin
                  Inc(ConvNum);
                  ConvFlags[ConvNum].First := Upper(FirstWord(Temp1));
                  ConvFlags[ConvNum].Last := Upper(FirstWord(Temp1));
                end;
            If UTemp2='REDUNDANT' then
              begin
                Temp2 := Upper(FirstWord(Temp1));
                while Temp1<>'' do
                  begin
                    Inc(ReduntNum);
                    ReduntFlags[ReduntNum].First := Temp2;
                    ReduntFlags[ReduntNum].Last := Upper(FirstWord(Temp1));
                  end;
              end;
            if UTemp2='DELENTRY' then                    (* !! 2.11 MM0901 *)
              while Temp1<>'' do
                begin
                  Inc(DelEntryNum);
                  If DelEntryNum <= MaxDelEntry then
                    DelEntry[DelEntryNum] := FirstWord(Temp1)
                  else begin (* Too many names to kill specified! *)
                        WriteLn('! More than ',MaxDelEntry,' names to kill specified in TAB, ignoring the rest...');
                        BREAK
                       end
                end;
          end;
      end;
    close(TabFile);
  end;

function Equal(VAR Test, Against : String):Boolean;
                              (* Check flags for equality, supporting *)
                              (* a few macros in the process          *)
  Var
    C       : Char;
    Passed  : Boolean;
    L       : Integer;


  begin
    Passed := (Test[0]=Against[0]) or ((Test[0]>Against[0]) and (pos('*',Against)>0));
    L := 1;
    while (Passed and (L<=Byte(Test[0]))) do
      begin
        C := Test[L];                                 (* !! 2.8 MM0717 *)
        Case Against[L] of
          '$' : Passed := C in ['A'..'Z','a'..'z'];
          '@' : Passed := C in ['0'..'9'];
          '?' : Passed := True;                       (* !! 2.7 990405 *)
          '*' : begin                                 (* !! 2.7 990405 *)
                 Passed := True;
                 L := Byte(Test[0]);
                end
        else begin
              Test[L] := upcase(C);                   (* !! 2.8 MM0717 *)
              if Test[L] <> C then Inc(ThisCaseConv);
              Passed := Test[L]=upcase(Against[L]);
             end
        end;
        Inc(L);
      end;
    Equal := Passed;
  end;


function PhoneOK (var Inp: String): Boolean;         (* !! 2.10  MM0811 *)

                             (* Checks the entry's phonenumber validity *)
  Var
    i    : Byte;
    Ok   : Boolean;

  begin
    Ok := TRUE;
    if Inp[1] = '-' then
      if Upper(Inp) = '-UNPUBLISHED-' then
        Inp := Unpub
      else Ok := FALSE
    else if copy(Inp,1,3) = '000' then    (* no smuggling of IP addresses! *)
           Ok := FALSE
         else begin                       (* it must be a phonenumber! *)
                i := 1;
                repeat
                  if Inp[i] in ['0'..'9','-'] then
                    Inc(i)
                  else Ok := FALSE
                until not Ok or (i > length(Inp))
              end;
    PhoneOK := Ok
  end;


function FixFlags(Inp: String; RepNode: String): String;
                                   (* Checks the entry's nodelist flags *)
                                   (* for errors and fixes them.        *)
  Var
    L1,
    L2,
    L3,
    UserStart    : Byte;
    temp,
    NrmFlags,
    UsrFlags     : String;
    nNFlags      : Byte;
    nUFlags      : Byte;
    NFlag        : Array[1..MaxFlags] of TFlag;           (* !! 2.7 990405 *)
    UFlag        : Array[1..MaxFlags] of TFlag;           (* !! 2.7 990405 *)
    Passed       : Boolean;

  begin
                             (* First of all, make any conversions *)
                             (* as stated in the CTL file          *)
    For L1 := 1 to ConvNum do
      begin
        If Pos(ConvFlags[L1].First,Inp) <> 0 then
          Inp := Copy(Inp,1,Pred(Pos(ConvFlags[L1].First,Inp)))
                 + ConvFlags[L1].Last
                 + Copy(Inp,Pos(ConvFlags[L1].First,Inp)
                 + Length(ConvFlags[L1].First),255);
      end;
                             (* Then we will divide the flags into normal *)
                             (* and user flags *)
    NrmFlags := Inp;
    UsrFlags := '';
    UserStart := Pos(',U',Inp);
    If UserStart<>0 then
      begin
        NrmFlags := Copy(Inp,1,Pred(UserStart));
        UsrFlags := Copy(Inp,UserStart,255);
        If UsrFlags[3]=',' then
          Delete(UsrFlags,1,3)
        else
          Delete(UsrFlags,1,2);
        While Pos(',U',UsrFlags)<>0 do
          Delete(UsrFlags,Pos(',U',UsrFlags)+1,1);
      end;
                   (* Now let's check for invalid flags *)
    nNFlags := 0;
    If Length(NrmFlags)>0 then
      Delete(NrmFlags,1,1);
    While Length(NrmFlags)>0 Do
      begin
        Inc(nNFlags);
        NFlag[nNFlags] := extract(NrmFlags);
      end;
    L1 := 1;
    For L1 := 1 to nNFlags do
      begin
        Passed := False;
        For L2 := 1 to ApprNum do
          If not passed then
            Passed := Equal(NFlag[L1],ApprFlags[L2]);
        if not Passed then               (* !! jb 970129 *)
          begin                          (* Check CFLAGs *)
            temp := nFlag[L1];
            For L2 := 1 to CatNum do
              if pos(CatFlags[L2],temp)<>0 then (* match found! *)
                delete(temp,pos(CatFlags[L2],temp),length(CatFlags[L2]));
            Passed := (temp='');
          end;
        If Passed=False then
          begin
            WriteLn('# ERROR  : invalid flag "',nFlag[L1],'" for ',RepNode);
            WriteLn(RptFile,' ERROR  : invalid flag ',nFlag[L1],' for ',RepNode);
            Inc(ThisFlagErr);
            nFlag[L1] := '';
          end;
      end;
                   (* And for invalid user flags *)
    nUFlags := 0;
    While Length(UsrFlags)>0 Do
      begin
        Inc(nUFlags);
        UFlag[nUFlags] := extract(UsrFlags);
      end;
    For L1 := 1 to nUFlags do
      begin
        Passed := False;
        For L2 := 1 to UserNum
         do
          if not passed then
            Passed := Equal(UFlag[L1],UserFlags[L2]);
        If Passed=False then
          begin
            Inc(ThisUserErr);
            WriteLn('# ERROR  : invalid user flag ',UFlag[L1],' for ',RepNode);
            WriteLn(RptFile,' ERROR  : invalid user flag ',UFlag[L1],' for ',RepNode);
            UFlag[L1] := '';
          end;
      end;
              (* Now let's remove any redundant flags *)

    For L1 := 1 to nNFlags do
      begin
        For L2 := 1 to ReduntNum do
          begin
            if NFlag[L1]=ReduntFlags[L2].First then (* We got a match *)
              For L3 := 1 to (nNFLags) do           (* Now check redundancy *)
                If NFlag[L3]=ReduntFlags[L2].Last then (* Got it *)
                  begin
                    WriteLn('# WARNING: redundant flag ',NFlag[L3],' due to ', NFlag[L1],' for ',RepNode);
                    WriteLn(RptFile,' WARNING: redundant flag ',NFlag[L3],' due to ', NFlag[L1],' for ',RepNode);
                    NFLag[L3] := '';
                    Inc(ThisReduErr);
                  end;
          end;
      end;

              (* Now check for any duplicate flags *)
    For L1 := 1 to Pred(nNFlags) do
      For L2 := Succ(L1) to nNFlags do
        If (NFlag[L1]<>'') and (NFlag[L1]=NFlag[L2]) then
          begin
            WriteLn('# WARNING: duplicate flag ',NFlag[L2],' for ',RepNode);
            WriteLn(RptFile,' WARNING: duplicate flag ',NFlag[L2],' for ',RepNode);
            NFlag[L2] := '';
            Inc(ThisDupErr);
          end;

    Inp := '';
    For L1 := 1 to nUFlags do
      If uFlag[L1]<>'' then Inp := Inp+','+uFlag[L1];
    If Inp<>'' then
      Inp := ',U'+Inp;
    For L1 := NNFlags downto 1 do
      If nFlag[L1]<>'' then Inp := ','+nFlag[L1]+Inp;
    FixFlags := Inp;
  end;


function ParseNodeLine(Inp : String): String;
                               (* initial parser of segment line        *)
                               (* Since MakeNl makes checks on the      *)
                               (*  normal fields, we do not check them  *)
                               (*  but checks should go here            *)
 Const                         (* V2.9 and up will check and try to     *)
    comma = ',';               (*  correct normal fields!               *)
 Var
    CurrentNode : String[25];
    Prefix      : String[10];
    Node        : String[5];
    SystemName,
    Location,
    AdminName,
    Phonenr,
    AllFlags    : String;
    BaudrateStr : String[10];
    Baudrate    : Word;
    LenLine     : Word;
    LenToStrip  : Integer;
    DoDelEntry,
    DoDelSpaces,
    DoDelCommas : Boolean;
    i           : Byte;

begin
  i := pos(' ', Inp);
  DoDelSpaces := (i > 0);     (* !! 2.12  20010414 - remove all spaces *)
  while i > 0 do
    begin
      delete(Inp, i, 1);
      i := pos(' ', Inp)
    end;
  DoDelCommas := (Inp[length(Inp)] = ',');  (* !! 2.12  20010414 - remove all tail comma's *)
  while Inp[length(Inp)] = ',' do dec(Inp[0]);
  Prefix := Proper(Extract(Inp));
  Node := Extract(Inp);
  CurrentNode := WordToStr(ThisZone)+':'+WordToStr(ThisNet)+'/'+Node;
  If Prefix='Zone' then
    begin
      ThisZone := StrToWord(Node);
      CurrentNode := Node+':'+Node+'/0';
    end
  else If (Prefix='Region') or (Prefix='Host') then
         begin
           ThisNet := StrToWord(Node);
           CurrentNode := WordToStr(ThisZone)+':'+Node+'/0';
         end
       else If (Prefix='Hub') or (Prefix='Pvt') or
               (Prefix='Hold') or (Prefix='Down') then        (* !! 2.9  MM0805 *)
              { valid prefix: continue }
            else If Prefix<>'' then     (* !! 2.9  MM0805 *)
                   begin
                     WriteLn('# ERROR  : unknown prefix "',Prefix,'" for ', CurrentNode, '!');
                     WriteLn(RptFile,' ERROR  : unknown prefix "',Prefix,'" for ', CurrentNode, '!');
                     Prefix := '';
                     Inc(ThisPrefixErr)
                   end;
  If DoDelSpaces then
    begin
      WriteLn('# WARNING: space(s) removed for ', CurrentNode, '!');
      WriteLn(RptFile, ' WARNING: space(s) removed for ', CurrentNode, '!');
      Inc(ThisSpaces)
    end;
  If DoDelCommas then
    begin
      WriteLn('# WARNING: tail comma(s) removed for ', CurrentNode, '!');
      WriteLn(RptFile, ' WARNING: tail comma(s) removed for ', CurrentNode, '!');
      Inc(ThisTailCommas)
    end;
  SystemName := Extract(Inp);
  Location := Extract(Inp);
  AdminName := Extract(Inp);
  DoDelEntry := false;
  If DelEntryNum > 0 then                                (* !! 2.11  MM0901 *)
    For i := 1 to DelEntryNum do
      DoDelEntry := DoDelEntry OR (Upper(AdminName) = Upper(DelEntry[i]));
  If not DoDelEntry then
    begin
      PhoneNr := Extract(Inp);
      If not PhoneOK(PhoneNr) then                       (* !! 2.10  MM0811 *)
        begin
          WriteLn('# WARNING: invalid phonenumber "',PhoneNr,'" for ',CurrentNode);
          WriteLn(RptFile,' WARNING: invalid phonenumber "',PhoneNr,'" for ',CurrentNode);
          PhoneNr := Unpub;
          if Prefix = '' then Prefix := 'Pvt';
          Inc(ThisPhoneErr)
         end;
      If (Prefix = 'Pvt') and (PhoneNr <> Unpub) then
        begin
          WriteLn('# WARNING: Pvt prefix should have unpublished phonenumber for ', CurrentNode);
          WriteLn(RptFile,' WARNING: Pvt prefix should have unpublished phonenumber for ', CurrentNode);
          PhoneNr := Unpub;
          Inc(ThisPvtErr);
        end;
      BaudrateStr := Extract(Inp);
      Baudrate := StrToWord(BaudrateStr);
      i := 1;
      While (i <= MaxBaudNum) and (Baudrate <> Baudrates[i]) do Inc(i);
      If i > MaxBaudNum then
        begin
         WriteLn('# WARNING: invalid baudrate ', BaudrateStr,' for ',CurrentNode);
         WriteLn(RptFile,' WARNING: invalid baudrate ', BaudrateStr,' for ',CurrentNode);
         Baudrate := BaudDefault;
         Inc(ThisBaudErr);
        end;
      AllFlags := FixFlags(','+Inp, CurrentNode);
      LenLine := length(Prefix)+length(Node)+length(SystemName)+
            length(Location)+length(AdminName)+length(PhoneNr)+
            length(WordToStr(Baudrate))+length(AllFlags)+6;
      LenToStrip := LenLine - 157;      (* !! MM0717 v2.8 add length check *)
      If LenToStrip > 0 then
        begin
         WriteLn('# WARNING: entry for ', CurrentNode, ' of ', LenLine, ' chars cut to 157 for MAKENL');
         WriteLn(RptFile,' WARNING: entry for ', CurrentNode, ' of ', LenLine, ' chars cut to 157 for MAKENL');
         If length(SystemName)-1 >= LenToStrip then
           Delete(SystemName, length(SystemName)-LenToStrip+1, LenToStrip)
         else begin
               LenToStrip := LenToStrip - length(SystemName) + 3;
               Delete(SystemName, 2, length(SystemName)-1);
               if LenToStrip > 0 then
                 if length(Location)-1 >= LenToStrip then
                   Delete(Location, length(Location)-LenToStrip+1, LenToStrip)
                 else begin
                       LenToStrip := LenToStrip - length(Location) + 1;
                       Delete(Location, 2, length(Location)-1);
                       if LenToStrip > 0 then
                         begin
                          WriteLn('= BEWARE: cut ', LenToStrip, ' chars from Flags as well!');
                          WriteLn(RptFile,' BEWARE: cut ', LenToStrip, ' chars from Flags as well!');
                          Delete(AllFlags, length(AllFlags)-LenToStrip+1, LenToStrip);
                          AllFlags := FixFlags(AllFlags, CurrentNode)  (* check flags again *)
                         end
                      end;
              end;
        end;
      ParseNodeLine := Prefix+comma+Node+comma+SystemName+comma+Location+
            comma+AdminName+comma+PhoneNr+comma+WordToStr(Baudrate)+AllFlags
    end
  else begin
         WriteLn('# WARNING: entry for ', CurrentNode, ', SysOp ', AdminName,', removed.');
         WriteLn(RptFile,' WARNING: entry for ', CurrentNode, ', SysOp ', AdminName,', removed.');
         ParseNodeLine := ''
       end
end;


procedure CheckSegment(InP, InF, RptF, Notif : String);
                                (* Checks complete segment files, creating *)
                                (* a fixed segment file together with a    *)
                                (* report file for notification            *)
  Const
    TempFName = 'NLSEGtmp.$$$';    (* !! 2.11  MM0901 *)

  Type
    Str18 = String[18];
    Str3  = String[3];

  Var
    TmpFile,
    InFile,
    OutFile  : Text;
    Temp,
    Default_NLheader,
    CRCline  : String;
    CRC16str : String[5];
    CRC16    : Word;
    i        : Byte;
    CalcCRC  : Boolean;         (* !! 2.12  20010414 *)
    AmDate   : Str18;           (* !! 2.13  20010422 *)
    DayNum   : Str3;            (* !! 2.13  20010422 *)


  procedure NextFriday (var Datum    : Str18;    (* !! 2.13  20010422 *)
                        var Daynumber: Str3);

   const
     MonthName : array [1..12] of String[9]
             = ('January', 'February', 'March',    'April',  'May',     'June',
                 'July',   'August',   'September','October','November','December'
               );

   var
     JDN : LongInt;
     D, M, Y, WkD: Word;

    begin {NextFriday}
     DOS.GetDate(Y, M, D, WkD); {WkD: 0..6 with 0 = Sunday}
     JDN := JulianDayNumber(D, M, Y);
     if WkD < 5 then
       JDN := JDN + 5 - WkD
     else JDN := JDN + 12 - WkD;
     JulianToDate(JDN, D, M, Y);
     Datum := MonthName[M] + ' ' + WordToStr(D) + ', ' + WordToStr(Y);
     Daynumber := WordToStr(JDN - JulianDayNumber(1, 1, Y) + 1);
     while length(Daynumber) < 3 do Daynumber := '0' + Daynumber
    end; {NextFriday}


  function ExtractFile(InFile:String):String;
                                (* Find file names with wild cards, and    *)
    Var                         (* extract any ARC'ed data files           *)
      Temp,
      OldDir : String;
      S      : SearchRec;

    begin
      ExtractFile := '';
      If Pos('*',InFile)=0 then
        begin
          ExtractFile := AddFileSeparator(Inp) + InFile;
          Exit;
        end;
      If InFile[Succ(Pos('.',InFile))]='A' then  (* UnARC this one *)
        begin
          GetDir(0,OldDir);
          ChDir(InP);
          FindFirst(InFile,Archive,S);
          If DOSError=0 then
            begin
              while DOSError=0 do
                begin
                  InFile := S.Name;
                  FindNext(S);
                end;
              Temp := Upper(UnCompress);
              Temp := Copy(UnCompress,1,Pred(Pos('%FILE%',Upper(UnCompress))))+InFile+
                    Copy(UnCompress,Pos('%FILE%',Upper(UnCompress))+6,255);
              Exec(GetEnv('COMSPEC'),'/C '+Temp);
              Assign(TmpFile,InFile);
              {$I-}
              Erase(TmpFile);
              {$I+}
              If LastError<>0 then
                begin
                  WriteLn('! Unable to delete ',InFile,' - error ',WordToStr(LastErr));
                  Exit;
                end;
            end;
          ChDir(OldDir);
        end;
      FindFirst(AddFileSeparator(Inp) + Copy(InFile, 1, Pos('.', InFile)) + '*', Archive, S);
      if DOSError=0 then
        begin
          While DOSError=0 do
            begin
              ExtractFile := AddFileSeparator(Inp) + S.Name;
              FindNext(S);
            end;
        end;
    end; {ExtractFile}

  begin {CheckSegment}
    InF := ExtractFile(InF);    (* Get pure file *)
    If InF='' then Exit;
    ThisZone := DefaultZone;
    ThisNet := DefaultNet;
    ThisPrefixErr := 0;    (* !! 2.9  MM0805 *)
    ThisPvtErr := 0;       (* !! 2.9  MM0805 *)
    ThisPhoneErr := 0;     (* !! 2.10 MM0811 *)
    ThisBaudErr := 0;      (* !! 2.9  MM0805 *)
    ThisFlagErr := 0;
    ThisUserErr := 0;
    ThisReduErr := 0;
    ThisDupErr := 0;
    ThisCaseConv := 0;          (* !! 2.8 MM0717 *)
    ThisSpaces := 0;            (* !! 2.14  20010424 *)
    ThisTailCommas := 0;        (* !! 2.14  20010424 *)
    NextFriday(AmDate, DayNum); (* !! 2.13  20010422 *)
    Default_NLheader := ';A Fidonet Nodelist for Friday, '
                        + AmDate + ' -- Day number '
                        + DayNum + ' : 00000';   (* !! 2.13  20010422 *)
    If Touch then               (* !! 960127 No need to erase in NoTouch mode *)
      begin
        Assign(InFile,TempFName);
        {$I-}
        Erase(InFile);
        {$I+}
        If LastError=0 then;  (* This would be the normal *)
      end;
    Assign(InFile,InF);
    If Touch then             (* !! 960127 Don't rename in NoTouch mode *)
      begin
        {$I-}
        Rename(InFile,TempFName);
        {$I+}
        If LastError<>0 then
          begin
            If (LastErr<>2) and (LastErr<>3) then  (* file not found is normal *)
              WriteLn('! Unable to rename ',InF,' - error ',WordToStr(LastErr));
            Exit;
          end;
        Assign(OutFile,InF);   (* !! 960127 Moved outfile creation code *)
        {$I-}
        Rewrite(OutFile);
        {$I+}
        If LastError<>0 then
          begin
            WriteLn('! Unable to create ',InF,' - error ',WordToStr(LastErr));
            Close(InFile);
            Exit;
          end;
      end;
    {$I-}
    Reset(InFile);
    {$I+}
    If LastError<>0 then
      begin
        if LastErr<>2 then       (* !! 960127 *)
          WriteLn('! Unable to open ',InF,' - error ',WordToStr(LastErr));
        Exit;
      end;
    Assign(RptFile,RptF);
    {$I-}
    Rewrite(RptFile);
    {$I+}
    If LastError<>0 then
      begin
        WriteLn('! Unable to create ',InF,' - error ',WordToStr(LastErr));
        Exit;
      end;
    if not OpenConfigFile(DEFAULT_CMT_FILE_NAMES, cmtFileName, cmtFile, false) then
      begin
        while not EOF(cmtFile) do
          begin
            ReadLn(cmtFile, temp);
            WriteLn(RptFile,Temp);
          end;
        writeLn(RptFile);
        close(cmtFile);
      end;
    WriteLn(#10'# Processing file ',InF);
    WriteLn(RptFile,#10' Processing file ',InF);
    CalcCRC := false;
    CRCline := '';
    While not Eof(InFile) Do
      begin
        ReadLn(InFile,Temp);
		if temp[1] = chr(26) then (* Ignore 'soft' EOF character *)
			begin
				temp := Copy(temp, 2, length(temp) - 1);
				if temp = '' then
					continue;
			end;
        if not CalcCRC then
          begin
            CalcCRC := true;
            CRC16 := 0;
            if pos('Day number', Temp) = 0 then  (* !! 2.12  20010414 *)
              CRCline := Default_NLheader
            else CRCline := Temp
          end;
        if Temp[1] <> ';' then                   (* !! 960127 *)
          Temp := ParseNodeLine(Temp);
        If Touch and (length(Temp) > 0) then   (* !! 960127 *)  (* !! 2.11 MM0901 *)
          begin
            WriteLn(OutFile,Temp);
            if CRCline <> Temp then  (* !! 2.12  20010414 *)
              begin
                for i := 1 to length(Temp) do
                  Calculate_CRC(CRC16, Byte(Temp[i]));
                Calculate_CRC(CRC16, 13); (* CR *)
                Calculate_CRC(CRC16, 10)  (* LF *)
              end
          end;
      end;
    close(InFile);
    If Touch then    (* !! 960127 *)
      begin
        {$I-}
        Erase(InFile);
        {$I+}
        Close(OutFile);
        CRC16str := WordToStr(CRC16);  (* !! 2.12  20010114 *)
        while length(CRC16str) < 5 do CRC16str := '0' + CRC16str;
        CRCline := copy(CRCline, 1, length(CRCline) - 5) + CRC16str;
        WriteLn(CRCline);
        Rename(OutFile, TempFName);
        Assign(InFile, TempFName);
        Assign(OutFile, InF);
        {$I-}
        Rewrite(OutFile);
        {$I+}
        If LastError<>0 then
          begin
            WriteLn('! Unable to create ',InF,' - error ',WordToStr(LastErr));
            Close(InFile);
            Exit;
          end;
        Reset(InFile);
        ReadLn(InFile, Temp);
        WriteLn(OutFile, CRCline);
        If pos('Day number', Temp) = 0 then     (* !! 2.13  20010122 *)
          WriteLn(OutFile, Temp);
        while not EOF(InFile) do
          begin
            ReadLn(InFile, Temp);
            WriteLn(OutFile, Temp);
          end;
        Close(InFile);
        Close(OutFile);
        Erase(InFile);
        {$I+}
        If LastError<>0 then
          begin
            WriteLn('! Unable to delete temporary file - error ',WordToStr(LastErr));
            exit;
          end;
      end;
    WriteLn(#10'* SUMMARY REPORT FOR THIS NODELIST SEGMENT:');  (* !! 2.10  MM0811 *)
    WriteLn('> Prefix errors            : ', ThisPrefixErr);
    WriteLn('> Pvt/phone errors         : ', ThisPvtErr);
    Writeln('> Phonenumber errors       : ', ThisPhoneErr);
    WriteLn('> Baudrate errors          : ', ThisBaudErr);
    WriteLn('> Flag errors              : ', ThisFlagErr);
    WriteLn('> User flag errors         : ', ThisUserErr);
    WriteLn('> Redundant flags          : ', ThisReduErr);
    WriteLn('> Duplicate flags          : ', ThisDupErr);
    WriteLn('> Case conversions         : ', ThisCaseConv);
    WriteLn('> Entries with spaces      : ', ThisSpaces);              (* !! 2.14  20010428 *)
    WriteLn('> Entries with tail commas : ', ThisTailCommas);          (* !! 2.14  20010428 *)
    Writeln(RptFile,#10' SUMMARY REPORT FOR THIS NODELIST SEGMENT:');
    WriteLn(RptFile,' Prefix errors            : ', ThisPrefixErr);
    WriteLn(RptFile,' Pvt/phone errors         : ', ThisPvtErr);
    Writeln(RptFile,' Phonenumber errors       : ', ThisPhoneErr);
    WriteLn(RptFile,' Baudrate errors          : ', ThisBaudErr);
    WriteLn(RptFile,' Flag errors              : ', ThisFlagErr);
    WriteLn(RptFile,' User flag errors         : ', ThisUserErr);
    WriteLn(RptFile,' Redundant flags          : ', ThisReduErr);
    WriteLn(RptFile,' Duplicate flags          : ', ThisDupErr);
    WriteLn(RptFile,' Case conversions         : ', ThisCaseConv);
    WriteLn(RptFile,' Entries with spaces      : ', ThisSpaces);       (* !! 2.14  20010428 *)
    WriteLn(RptFile,' Entries with tail commas : ', ThisTailCommas);   (* !! 2.14  20010428 *)
    WriteLn(RptFile);
    WriteLn(RptFile,' // ErrFlags v', ErrFlagsVersion, ' by Jonny Bergdahl, Johan Zwiekhorst, Niels Joncheere'); (* !! 990405 *)
    Close(RptFile);
    Inc(TotPrefixErr, ThisPrefixErr);         (* !! 2.9  MM0805 *)
    Inc(TotPvtErr, ThisPvtErr);               (* !! 2.9  MM0805 *)
    Inc(TotPhoneErr, ThisPhoneErr);           (* !! 2.10 MM0811 *)
    Inc(TotBaudErr, ThisBaudErr);             (* !! 2.9  MM0805 *)
    Inc(TotFlagErr,ThisFlagErr);
    Inc(TotUserErr,ThisUserErr);
    Inc(TotReduErr,ThisReduErr);
    Inc(TotDupErr,ThisDupErr);
    Inc(TotCaseConv,ThisCaseConv);            (* !! 2.8  MM0717 *)
    Inc(TotSpaces, ThisSpaces);               (* !! 2.14  20010428 *)
    Inc(TotTailCommas, ThisTailCommas);       (* !! 2.14  20010428 *)
    AnyProcessed := True;
    If ThisPvtErr+ThisBaudErr+ThisFlagErr+ThisUserErr+ThisReduErr+ThisDupErr > 0 then      (* !! 2.9 MM0805 *)
      begin
        if (NotifyCmd='') then
          exit
        else
          Temp := NotifyCmd;
      end     (* Only run notification if defined *)
    else
      begin
        if (NoErrCmd='') then
          exit
        else
          Temp := NoErrCmd;
      end;
    {$I-}
    ChDir(NotifyPath);
    {$I+}
    If LastError<>0 then
      begin
        writeLn('! Unable to CD to path ',NotifyPath, ' - error ',LastErr);
        exit;
      end;
    If Pos('%NODE%',Temp)<>0 then
      Temp := Copy(Temp,1,Pred(Pos('%NODE%',Upper(Temp))))+Notif+
            Copy(Temp,Pos('%NODE%',Upper(Temp))+6,255);
    If Pos('%FILE%',Temp)<>0 then
      Temp := Copy(Temp,1,Pred(Pos('%FILE%',Upper(Temp))))+RptF+
            Copy(Temp,Pos('%FILE%',Upper(Temp))+6,255);
	ExecuteCommand('Notify', Temp, OldDir);
  end;

Var
  Loop : Integer;

begin                          (* Main program  *)
  AnyProcessed := False;
  SignOn;
  ParseCTLFile;
  ParseTabFile;
  TotPvtErr     := 0;
  TotPhoneErr   := 0;
  TotBaudErr    := 0;
  TotFlagErr    := 0;
  TotUserErr    := 0;
  TotReduErr    := 0;
  TotDupErr     := 0;
  TotCaseConv   := 0;
  TotSpaces     := 0;
  TotTailCommas := 0;
  For Loop := 1 to SegmentNum do
    with SegmentFile[Loop] do
      CheckSegment(InboundPath,FileName,RptFile,Notifier);
  If AnyProcessed and (NotifyCmd<>'') then
    begin
      ChDir(ExecutePath);
      {$I+}
      If LastError<>0 then
        writeLn('! Unable to CD to path ',ExecutePath, ' - error ',LastErr);
      ExecuteCommand('Execute', ExecuteCmd, OldDir);
      ChDir(OldDir);
    end;
  WriteLn(#10'* FINAL REPORT FOR ALL PROCESSED NODELIST SEGMENTS:');  (* !! 2.10  MM0811 *)
  WriteLn('* Total prefix errors            : ', TotPrefixErr);     (* !! 2.9  MM0805 *)
  WriteLn('* Total pvt/phone errors         : ', TotPvtErr);        (* !! 2.9  MM0805 *)
  Writeln('* Total phonenumber errors       : ', TotPhoneErr);      (* !! 2.10 MM0811 *)
  WriteLn('* Total baudrate errors          : ', TotBaudErr);       (* !! 2.9  MM0805 *)
  WriteLn('* Total flag errors              : ', TotFlagErr);
  WriteLn('* Total user flag errors         : ', TotUserErr);
  WriteLn('* Total redundant flags          : ', TotReduErr);
  WriteLn('* Total duplicate flags          : ', TotDupErr);
  WriteLn('* Total case conversions         : ', TotCaseConv);      (* !! 2.8  MM0717 *)
  WriteLn('* Total entries with spaces      : ', TotSpaces);        (* !! 2.14  20010428 *)
  WriteLn('* Total entries with tail commas : ', TotTailCommas);    (* !! 2.14  20010428 *)
end.
