; // sample ERRFLAGS.CTL suitable for Z2R29C //
; (The ";" character placed in the first postion of the line makes ERRFLAGS
; treat the whole line as a comment)
;
; Key word ZONE states default zone.
ZONE 2
;
; Key word NET states default net, and is only really necessary for hub
; level segment checks.
;NET 292
;
; Key word NOTOUCH can be used to instruct ErrFlags not to change the
; submitted nodelist segment, making it possible to just send notifications
; on errors in the segment, instead of fixing other peoples problems.
;NOTOUCH
;
; Key word TABFILE can be used to override the default file name of the
; approved flag file table
;TABFILE ERRFILE.TAB
;
; Key word CMTFILE can be used to override the default file name of the
; comment file.
;CMTFILE ERRFILE.CMT
;
; Key word INBOUND states the directory where inbound segment files are to
; be found. Default value is current directory
;INBOUND \In
;
; Key word FILE states the filename of the updates to check and who should
; recieve the notification for the particular file (If using the
; notification function). Up to 80 file names are supported. Wildcards
; are supported in the file name to handle day numbers etc.
; Errflags are also capable of unarcing incoming packed segments. Just
; use a .A* extension, like in NET292.A* below, to use this feature.
; It is also possible to name the report file. Default report file name
; is input file name with extension RPT.
;       Filename        Notify  Report filename
;FILE   HUB_80.*        292/80  HUB_80.RPT
FILE    NET_292.TXT     292/0   NET_292.RPT
;
; Key word UNCOMPRESS states the ARC decompression utility to use, and
; the parameters to use. The %file% macro will be expanded to the segment
; file name.
UNCOMPRESS PAKE.EXE E -WA %file%
;
; Key word NOTIFY states the robot program and parameters to mail
; the notification file. This might also be a batch file if You wish 
; to process more than one notification for each file.
; The macros %node% and %file% will be expanded to the notify address
; and report file name.
; The example below uses the AUTOMAIL program by Mats Wallin, but any
; program capable of creating a netmail from a text file will suffice.
;NOTIFY AUTOMAIL %node% -F "Flag errors" -T %file% -c
;
; Key word NOERR states the robot program and parameters to mail the
; notification file when no errors where detected. This can be used to
; send such notification routed while error reports gets crashed.
; See NOTIFY above for further information.
;NOERR AUTOMAIL %node% -F "Flag errors" -T %file%
;
; Key word NOTIFYPATH states the directory that ErrFlags should execute
; the notification command in. Note that the %file% macro above only contains
; an unqualified filename, so you'll have to add the full path of the report
; file if you use this key word. The report file is created in the program
; directory.
NOTIFYPATH \BBS\ErrFlag
;
; Key word EXECUTE states any program to execute if any files has been
; processed. This makes it possible for executing MakeNl only when files
; has actually been received.
;EXECUTE MAKENL.EXE REGION.CTL /P >> MAKENLOG.LOG
;
; Key word EXECUTEPATH states the directory that ErrFlags should execute
; the above program.
;EXECUTEPATH \BBS\MAKENL
;
; Key word MAXENTRYLENGTH specifies the maximum length for a nodelist entry; if
; a nodelist entry exceeds this value, its name (and, if necessary, flags) will
; be cut.
;MAXENTRYLENGTH 157
;
; // end of file //
