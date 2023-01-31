#!/usr/bin/python3
#
# ErrFlags v3.0  Checks nodelist segments for flag errors.
# Copyright (C) 2022  Wilfred van Velzen
#
# This version is based on the earlier pascal version developed by:
# Jonny Bergdahl later modified by Johan Zwiekhorst, and Niels Joncheere.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

import glob
import os
import os.path
import subprocess
import sys
import time

# Constants
ErrFlagsVersion = '2.99.10 beta'
codec     = 'latin_1'
comma     = ','
crlf      = '\r\n'
eof       = chr(26)
daynumstr = " -- Day number "
unpub     = "-Unpublished-"
DEFAULT_CTL_FILE_NAMES = ('ErrFlags.ctl', 'errflags.ctl')
DEFAULT_TAB_FILE_NAMES = ('ErrFlags.tab', 'errflags.tab')
DEFAULT_CMT_FILE_NAMES = ('ErrFlags.cmt', 'errflags.cmt')

cmtFileName    = ""
ctlFileName    = ""
ExecuteCmd     = ""
ExecutePath    = ""
InboundPath    = ""
NoErrCmd       = ""
NotifyCmd      = ""
NotifyPath     = ""
oldDir         = ""
tabFileName    = ""
UnCompressArc  = ""
UnCompressZip  = ""
ctlFile        = None
RptFile        = None
BaudDefault    = 300
DefaultNet     = 0
DefaultZone    = 0
MaxEntryLength = 157
SegmentNum     = 0
TotBaudErr     = 0
TotCaseConv    = 0
TotConverted   = 0
TotDelEntry    = 0
TotDupErr      = 0
TotFlagErr     = 0
TotPhoneErr    = 0
TotPrefixErr   = 0
TotPvtErr      = 0
TotReduErr     = 0
TotSpaces      = 0
TotTailCommas  = 0
TotUserErr     = 0
ThisCaseConv   = 0
ThisConverted  = 0
ThisDupErr     = 0
ThisFlagErr    = 0
ThisReduErr    = 0
ThisUserErr    = 0
ThisPrefixErr  = 0
ThisSpaces     = 0
ThisTailCommas = 0
ThisDelEntry   = 0
ThisPhoneErr   = 0
ThisPvtErr     = 0
ThisBaudErr    = 0
ThisZone       = 0
ThisNet        = 0
Touch          = True
AnyProcessed   = False


class TSegmentFile:
  def __init__(self, file_name, rpt_file, notifier):
    self.FileName = file_name
    self.RptFile  = rpt_file
    self.Notifier = notifier


class TConvFlag:
  def __init__(self, first, last):
    self.First = first
    self.Last  = last


SegmentFiles       = []
DelEntrys          = []
Baudrates          = []
ApprFlags          = []
CatFlags           = []
UserFlags          = []
ConvFlags          = []
ReduntFlags        = []
ReduntForPrefFlags = []


#
# Generic functions
#

# Converts the input string into an int.
def str2int(s):
  try:
    return int(s)
  except:
    return 0


# Returns a temporary full path name for the given path name.
def GetTempFileName(path):
  return os.path.join(os.path.dirname(path), 'NLSEGtmp.$$$')


# This function returns the first word in a string, and removes it from the original string.
# Used to read the setup files
def first_word(line):
  if not line:
    return "", ""
  r = line.split(maxsplit=1)
  return r if len(r) > 1 else (r[0], "")


# This function works as the above, but for comma separated files like the nodelist
def extract(line):
  if not line:
    return "", ""
  r = line.split(comma, maxsplit=1)
  return r if len(r) > 1 else (r[0], "")


crc_table = [
  0x0000, 0x1021, 0x2042, 0x3063, 0x4084, 0x50A5, 0x60C6, 0x70E7, 0x8108, 0x9129, 0xA14A, 0xB16B, 0xC18C, 0xD1AD, 0xE1CE, 0xF1EF,
  0x1231, 0x0210, 0x3273, 0x2252, 0x52B5, 0x4294, 0x72F7, 0x62D6, 0x9339, 0x8318, 0xB37B, 0xA35A, 0xD3BD, 0xC39C, 0xF3FF, 0xE3DE,
  0x2462, 0x3443, 0x0420, 0x1401, 0x64E6, 0x74C7, 0x44A4, 0x5485, 0xA56A, 0xB54B, 0x8528, 0x9509, 0xE5EE, 0xF5CF, 0xC5AC, 0xD58D,
  0x3653, 0x2672, 0x1611, 0x0630, 0x76D7, 0x66F6, 0x5695, 0x46B4, 0xB75B, 0xA77A, 0x9719, 0x8738, 0xF7DF, 0xE7FE, 0xD79D, 0xC7BC,
  0x48C4, 0x58E5, 0x6886, 0x78A7, 0x0840, 0x1861, 0x2802, 0x3823, 0xC9CC, 0xD9ED, 0xE98E, 0xF9AF, 0x8948, 0x9969, 0xA90A, 0xB92B,
  0x5AF5, 0x4AD4, 0x7AB7, 0x6A96, 0x1A71, 0x0A50, 0x3A33, 0x2A12, 0xDBFD, 0xCBDC, 0xFBBF, 0xEB9E, 0x9B79, 0x8B58, 0xBB3B, 0xAB1A,
  0x6CA6, 0x7C87, 0x4CE4, 0x5CC5, 0x2C22, 0x3C03, 0x0C60, 0x1C41, 0xEDAE, 0xFD8F, 0xCDEC, 0xDDCD, 0xAD2A, 0xBD0B, 0x8D68, 0x9D49,
  0x7E97, 0x6EB6, 0x5ED5, 0x4EF4, 0x3E13, 0x2E32, 0x1E51, 0x0E70, 0xFF9F, 0xEFBE, 0xDFDD, 0xCFFC, 0xBF1B, 0xAF3A, 0x9F59, 0x8F78,
  0x9188, 0x81A9, 0xB1CA, 0xA1EB, 0xD10C, 0xC12D, 0xF14E, 0xE16F, 0x1080, 0x00A1, 0x30C2, 0x20E3, 0x5004, 0x4025, 0x7046, 0x6067,
  0x83B9, 0x9398, 0xA3FB, 0xB3DA, 0xC33D, 0xD31C, 0xE37F, 0xF35E, 0x02B1, 0x1290, 0x22F3, 0x32D2, 0x4235, 0x5214, 0x6277, 0x7256,
  0xB5EA, 0xA5CB, 0x95A8, 0x8589, 0xF56E, 0xE54F, 0xD52C, 0xC50D, 0x34E2, 0x24C3, 0x14A0, 0x0481, 0x7466, 0x6447, 0x5424, 0x4405,
  0xA7DB, 0xB7FA, 0x8799, 0x97B8, 0xE75F, 0xF77E, 0xC71D, 0xD73C, 0x26D3, 0x36F2, 0x0691, 0x16B0, 0x6657, 0x7676, 0x4615, 0x5634,
  0xD94C, 0xC96D, 0xF90E, 0xE92F, 0x99C8, 0x89E9, 0xB98A, 0xA9AB, 0x5844, 0x4865, 0x7806, 0x6827, 0x18C0, 0x08E1, 0x3882, 0x28A3,
  0xCB7D, 0xDB5C, 0xEB3F, 0xFB1E, 0x8BF9, 0x9BD8, 0xABBB, 0xBB9A, 0x4A75, 0x5A54, 0x6A37, 0x7A16, 0x0AF1, 0x1AD0, 0x2AB3, 0x3A92,
  0xFD2E, 0xED0F, 0xDD6C, 0xCD4D, 0xBDAA, 0xAD8B, 0x9DE8, 0x8DC9, 0x7C26, 0x6C07, 0x5C64, 0x4C45, 0x3CA2, 0x2C83, 0x1CE0, 0x0CC1,
  0xEF1F, 0xFF3E, 0xCF5D, 0xDF7C, 0xAF9B, 0xBFBA, 0x8FD9, 0x9FF8, 0x6E17, 0x7E36, 0x4E55, 0x5E74, 0x2E93, 0x3EB2, 0x0ED1, 0x1EF0
]


def crc16(crc: int, data: bytes):
  """
  CRC-16 (CCITT) implemented with a precomputed lookup table
  """
  for byte in data:
    crc = ((crc << 8) ^ crc_table[(crc >> 8) ^ byte]) & 0xFFFF
  return crc


def crc16str(crc: int, data: str):
  return crc16(crc, data.encode(codec))


# Replace case-insensitive first occurrence only
def ireplace(old, new, text):
  i = text.lower().find(old.lower())
  if i == -1:
    return text
  return text[:i] + new + text[i + len(old):]

#
# Program specific functions and procedures
#

# len(";A * Nodelist for *, * *, 2022 -- Day number 035 : *") = 52
def is_crcline(line):
  return len(line) > 50 and line[0:3] == ';A ' and daynumstr in line


def print_and_report(msg, prechar='#'):
  global RptFile

  print(prechar + ' ' + msg)
  print(' ' + msg, file=RptFile)


# Initialises the program and extracts parameters
def SignOn():
  global ctlFileName, oldDir

  print("ErrFlags V" + ErrFlagsVersion, " Copyright (C) 2022  Wilfred van Velzen")
  print(" //  Checks nodelist segments for flag errors.")
  print("This program comes with ABSOLUTELY NO WARRANTY")
  print("This is free software, and you are welcome to redistribute it")
  print()
  if len(sys.argv) > 1:
    if sys.argv[1] == '/?':
      print('Syntax:')
      print()
      print('ERRFLAGS [control file]')
      print()
      print('    control file  Optional control file name. Can be used when processing')
      print('                  segments in different zones.')
      print('                  Default file name is ERRFLAGS.CTL')
      print()
      sys.exit(0)
    ctlFileName = sys.argv[1]
  oldDir = os.getcwd()


def openConfigFileHelper(configFileNames, haltOnError):
  error = True
  configFileName = ""
  configFile = None
  for configFileName in configFileNames:
    try:
      configFile = open(configFileName)
      error = False
      break
    except:
      error = True

  if error and haltOnError:
    print('! Unable to open ', configFileNames[0], ' - exiting...', file=sys.stderr)
    sys.exit(1)

  return error, configFileName, configFile


def openConfigFile(defaultConfigFileNames, configFileName, haltOnError):
  if configFileName:
    return openConfigFileHelper((configFileName,), haltOnError)

  return openConfigFileHelper(defaultConfigFileNames, haltOnError)


def ExecuteCommand(lbl, cmd, old_dir):
  print('#', lbl + ':', cmd)
  try:
    subprocess.call(cmd, shell=True)
    os.chdir(old_dir)
  except Exception as e:
    print("Error:", e, file=sys.stderr)


# Parses the configuration file
def ParseCTLFile():
  global ctlFileName, DefaultZone, DefaultNet, MaxEntryLength, Touch, InboundPath, NotifyPath, NotifyCmd, NoErrCmd \
       , NotifyPath, ExecuteCmd, ExecutePath, tabFileName, cmtFileName, UnCompressArc, UnCompressZip, SegmentFiles

  error, ctlFileName, ctl_file = openConfigFile(DEFAULT_CTL_FILE_NAMES, ctlFileName, True)
  for line in ctl_file:
    line = line.strip()
    if line and line[0] != ';':
      key_word, line = first_word(line)
      key_word = key_word.upper()
      if key_word == 'ZONE':
        DefaultZone = str2int(line)
      elif key_word == 'NET':
        DefaultNet = str2int(line)
      elif key_word == 'MAXENTRYLENGTH':
        MaxEntryLength = str2int(line)
      elif key_word == 'NOTOUCH':
        Touch = False
      elif key_word == 'FILE':
        fileName, line = first_word(line)
        notifier, line = first_word(line)
        rptFile , line = first_word(line)
        if not rptFile:
          rptFile = os.path.splitext(fileName)[0] + ".rpt"
        SegmentFiles.append(TSegmentFile(fileName, rptFile, notifier))
      elif key_word == 'INBOUND':
        InboundPath = line
      elif key_word == 'NOTIFY':
        NotifyCmd = line
      elif key_word == 'NOERR':
        NoErrCmd = line
      elif key_word == 'NOTIFYPATH':
        NotifyPath = line
      elif key_word == 'EXECUTE':
        ExecuteCmd = line
      elif key_word == 'EXECUTEPATH':
        ExecutePath = line
      elif key_word == 'TABFILE':
        tabFileName = line
      elif key_word == 'CMTFILE':
        cmtFileName = line
      elif key_word == 'UNCOMPRESS':
        UnCompressArc = line
      elif key_word == 'UNCOMPRESSZIP':
        UnCompressZip = line
  ctl_file.close()


# Parses the approved flag file
def ParseTabFile():
  global tabFileName, BaudDefault, Baudrates, ApprFlags, CatFlags, UserFlags, ConvFlags, ReduntFlags, ReduntForPrefFlags, DelEntrys

  error, tabFileName, tab_file = openConfigFile(DEFAULT_TAB_FILE_NAMES, tabFileName, True)
  for line in tab_file:
    line = line.strip()
    if line and line[0] != ';':
      key_word, line = first_word(line)
      key_word = key_word.upper()
      if key_word == 'BAUDDEFAULT':
        BaudDefault, line = first_word(line)
        BaudDefault = str2int(BaudDefault)
      elif key_word == 'BAUDRATE':
        while line:
          baudrate, line = first_word(line)
          Baudrates.append(str2int(baudrate))
      elif key_word == 'FLAGS':
        while line:
          appr_flag, line = first_word(line)
          ApprFlags.append(appr_flag.upper())
      elif key_word == 'CFLAGS':
        while line:
          cat_flag, line = first_word(line)
          CatFlags.append(cat_flag.upper())
      elif key_word == 'USER':
        while line:
          user_flag, line = first_word(line)
          UserFlags.append(user_flag.upper())
      elif key_word == 'CONVERT':
        while line:
          first, line = first_word(line)
          last , line = first_word(line)
          ConvFlags.append(TConvFlag(first.upper(), last.upper()))
      elif key_word == 'REDUNDANT':
        first, line = first_word(line)
        rflags = []
        while line:
          last, line = first_word(line)
          lup = last.upper()
          if lup in rflags:
            print("WARNING: Flag", last, "specified more then once in", tabFileName, "file for 'REDUNDANT", first, "...'", file=sys.stderr)
          else:
            rflags.append(lup)
        ReduntFlags.append(TConvFlag(first.upper(), rflags))
      elif key_word == 'REDUNDANTFORPREFIX':
        first, line = first_word(line)
        rflags = []
        while line:
          last, line = first_word(line)
          lup = last.upper()
          if lup in rflags:
            print("WARNING: Flag", last, "specified more then once in", tabFileName, "file for 'REDUNDANTFORPREFIX", first, "...'", file=sys.stderr)
          else:
            rflags.append(lup)
        ReduntForPrefFlags.append(TConvFlag(first, rflags))
      elif key_word == 'DELENTRY':
        while line:
          del_entry, line = first_word(line)
          DelEntrys.append(del_entry.upper())
  tab_file.close()


# Check flags for equality, supporting a few macros in the process
def is_equal(test, against):
  global ThisCaseConv

  passed = (len(test) == len(against)) or ((len(test) > len(against)) and ('*' in against))
  L = 0
  while (passed and (L < len(test))):
    C = test[L]
    if against[L] == '$':
      passed = C.isalpha()
    elif against[L] == '@':
      passed = C.isdecimal()
    elif against[L] == '?':
      passed = True
    elif against[L] == '*':
      passed = True
      L = len(test)
    else:
      test = test[:L] + test[L].upper() + test[L + 1:]
      if test[L] != C:
        ThisCaseConv += 1
      passed = test[L] == against[L].upper()
    L += 1
  return passed, test


# Checks the entry's phonenumber validity
def PhoneOK(phonenumber):
  ok = True
  if phonenumber[0] == '-':
    if phonenumber.upper() == '-UNPUBLISHED-':
      phonenumber = unpub
    else:
      ok = False
  elif phonenumber[:3] == '000':  # no smuggling of IP addresses
    ok = False
  else:                           # it must be a phone-number
    ok = all(ch in '-0123456789' for ch in phonenumber)
  return ok, phonenumber


def split_flags_str(str):
  return [flag for flag in str.split(comma) if flag]


def remove_duplicate_flags(flags, report_node):
  global ThisDupErr
  for n1, nflag1 in enumerate(flags[:-1]):
    if nflag1:
      for n2, nflag2 in enumerate(flags[n1 + 1:], start=n1 + 1):
        if nflag2 and nflag1 == nflag2:
          print_and_report("WARNING: duplicate flag " + nflag2 + " for " + report_node)
          flags[n2] = ""
          ThisDupErr += 1


def convert_flags(flags, report_node):
  global ThisConverted
  for fi, flag in enumerate(flags):
    for conv_flag in ConvFlags:
      if conv_flag.First == flag.upper():
        print_and_report("WARNING: converted flag " + flag + " to " + conv_flag.Last + " for "+ report_node)
        flags[fi] = conv_flag.Last
        ThisConverted += 1
        break


# Checks the entry's nodelist flags for errors and fixes them.
def FixFlags(flags, report_node, prefix):
  global ThisFlagErr, ThisReduErr, ThisUserErr, ThisDupErr

  # Divide the flags into normal and user flags
  user_start = flags.find(',U')
  if user_start >= 0:
    NrmFlags = flags[:user_start]
    UsrFlags = flags[user_start + 2:].replace(",U", ",")
  else:
    NrmFlags = flags
    UsrFlags = ""

  NFlags = split_flags_str(NrmFlags)
  convert_flags(NFlags, report_node)

  # Check for invalid flags
  for ni, nflag in enumerate(NFlags):
    Passed = False
    for appr_flag in ApprFlags:
      Passed, nflag = is_equal(nflag, appr_flag)
      NFlags[ni] = nflag
      if Passed:
        break
    if not Passed:
      temp = nflag
      for cat_flag in CatFlags:
        temp = temp.replace(cat_flag, "", 1)
      Passed = (temp == "")
      if not Passed:
        print_and_report('ERROR  : invalid flag ' + nflag + ' for ' + report_node)
        NFlags[ni] = ""
        ThisFlagErr += 1

  # Check for redundant combination of prefix and flag
  for redunt_for_pref_flag in ReduntForPrefFlags:
    if prefix == redunt_for_pref_flag.First:
      for nflag in NFlags:
        if nflag and nflag in redunt_for_pref_flag.Last:
          print_and_report('WARNING: incompatible flag ' + nflag + ' for ' + prefix + ' node ' + report_node)
          ThisReduErr += 1

  # Remove redundant flags
  for ni, nflag in enumerate(NFlags):
    if nflag:
      for redunt_flag in ReduntFlags:
        if redunt_flag.First in NFlags and nflag in redunt_flag.Last:
          print_and_report('WARNING: redundant flag ' + nflag + ' due to ' + redunt_flag.First + ' for ' + report_node)
          NFlags[ni] = ""
          ThisReduErr += 1
          break  # 1x reporting per flag is enough

  remove_duplicate_flags(NFlags, report_node)

  # Check user flags
  UFlags = split_flags_str(UsrFlags)
  convert_flags(UFlags, report_node)

  for ui, uflag in enumerate(UFlags):
    Passed = False
    for user_flag in UserFlags:
      Passed, uflag = is_equal(uflag, user_flag)
      UFlags[ui] = uflag
      if Passed:
        break
    if not Passed:
      print_and_report('ERROR  : invalid user flag ' + uflag + ' for ' + report_node)
      UFlags[ui] = ""
      ThisUserErr += 1

  remove_duplicate_flags(UFlags, report_node)

  # Remove deleted flags
  NFlags = [flag for flag in NFlags if flag]
  UFlags = [flag for flag in UFlags if flag]

  flags = comma + ",".join(NFlags) if NFlags else ""
  if UFlags:
    flags += ",U," + ",".join(UFlags)
  return flags


# Initial parser of segment line since MakeNl makes checks on the normal fields,
# we do not check them but checks should go here
def ParseNodeLine(line: str):
  global ThisZone, ThisNet \
       , ThisBaudErr, ThisPhoneErr, ThisPrefixErr, ThisPvtErr, ThisSpaces, ThisTailCommas, ThisDelEntry

  do_del_spaces = ' ' in line
  if do_del_spaces:
    line = line.replace(' ', '')

  Prefix, line = extract(line)
  Prefix = Prefix.capitalize()
  Node, line = extract(line)
  if Prefix == 'Zone':
    ThisZone = str2int(Node)
    CurrentNode = Node + ':' + Node + '/0'
  elif Prefix == 'Region' or Prefix == 'Host':
    ThisNet = str2int(Node)
    CurrentNode = str(ThisZone) + ':' + Node + '/0'
  else:
    CurrentNode = str(ThisZone) + ':' + str(ThisNet) + '/' + Node
    if Prefix and not (Prefix == 'Hub' or Prefix == 'Pvt' or Prefix == 'Hold' or Prefix == 'Down'):
      print_and_report('ERROR  : unknown prefix "' + Prefix + '" for ' + CurrentNode)
      Prefix = ''
      ThisPrefixErr += 1

  if do_del_spaces:
    print_and_report('WARNING: space(s) removed for ' + CurrentNode)
    ThisSpaces += 1

  if line and line[-1] == comma:
    line = line.rstrip(comma)
    print_and_report('WARNING: tail comma(s) removed for ' + CurrentNode)
    ThisTailCommas += 1

  SystemName, line = extract(line)
  Location, line = extract(line)
  AdminName, line = extract(line)

  if AdminName.upper() in DelEntrys:
    print_and_report('WARNING: entry for ' + CurrentNode + ', SysOp ' + AdminName + ', removed.')
    ThisDelEntry += 1
    return ""

  PhoneNr, line = extract(line)
  Ok, PhoneNr = PhoneOK(PhoneNr)
  if not Ok:
    print_and_report('WARNING: invalid phonenumber "' + PhoneNr + '" for ' + CurrentNode)
    PhoneNr = unpub
    if Prefix == '':
      Prefix = 'Pvt'
    ThisPhoneErr += 1

  if Prefix == 'Pvt' and PhoneNr != unpub:
    print_and_report("WARNING: Pvt prefix should have " + unpub + " phonenumber for " + CurrentNode)
    PhoneNr = unpub
    ThisPvtErr += 1

  BaudrateStr, line = extract(line)
  Baudrate = str2int(BaudrateStr)
  if Baudrate not in Baudrates:
    print_and_report('WARNING: invalid baudrate ' + BaudrateStr + ' for ' + CurrentNode)
    Baudrate = BaudDefault
    ThisBaudErr += 1

  AllFlags = FixFlags(comma + line, CurrentNode, Prefix)
  LenLine = len(Prefix) + len(Node) + len(SystemName) + len(Location) + len(AdminName) + len(PhoneNr) \
          + len(str(Baudrate)) + len(AllFlags) + 6
  LenToStrip = LenLine - MaxEntryLength
  if LenToStrip > 0:
    print_and_report('WARNING: entry for ' + CurrentNode + ' of ' + str(LenLine) + ' chars cut to ' + str(MaxEntryLength) + ' for MAKENL')
    if len(SystemName) > LenToStrip:
      SystemName = SystemName[:-LenToStrip]
    else:
      LenToStrip = LenToStrip - len(SystemName) + 3
      SystemName = SystemName[:1]
      if LenToStrip > 0:
        if len(Location) > LenToStrip:
          Location = Location[:-LenToStrip]
        else:
          LenToStrip = LenToStrip - len(Location) + 1
          Location = Location[:1]
          if LenToStrip > 0:
            print_and_report('= BEWARE: cut ' + str(LenToStrip) + ' chars from Flags as well!', prechar='=')
            AllFlags = FixFlags(AllFlags[:-LenToStrip], CurrentNode, Prefix)

  return Prefix + comma + Node + comma + SystemName + comma + Location + comma \
       + AdminName + comma + PhoneNr + comma + str(Baudrate) + AllFlags


# Set locale to English?
def NextFriday():
  t = time.time()
  lt = time.localtime(t + ((4 - time.localtime(t).tm_wday) % 7) * 86400)
  return time.strftime("%A, %B " + str(lt.tm_mday) + ", %Y", lt) \
       , time.strftime("%j", lt)


# OS independent case-insensitive pattern matching.
def iglob(path: str, pattern: str):
  def either(c):
    return '[' + c.lower() + c.upper() + ']' if c.isalpha() else c
  return glob.glob(os.path.join(path, ''.join(either(c) for c in pattern)))


# Return first file that matches pattern case-insensitive
def find_ifile(path: str, pattern: str):
  for fn in iglob(path, pattern):
    if os.path.isfile(fn):
      return fn
  return ""


def uncompress(command, inbound_path, filename):
  if not command:
    return True

  old_dir = os.getcwd()
  try:
    os.chdir(inbound_path)
    fn = find_ifile("", filename)
    if fn:
      cmd = ireplace('%FILE%', fn, command)
      print("# Uncompressing: '" + fn + "' in dir: '" + inbound_path + "' with command: '" + cmd + "'")
      subprocess.call(cmd, shell=True)
      os.remove(fn)
    else:
      return False
  except Exception as e:
    print('! Error in uncompress:', e, file=sys.stderr)
    return False
  finally:
    os.chdir(old_dir)

  return True


def uncompress_select(inbound_path, filename, ext):
  if len(ext) > 1:
    uncompress_type = ext[1].upper()
    if   uncompress_type == 'A':
      return uncompress(UnCompressArc, inbound_path, filename)
    elif uncompress_type == 'Z':
      return uncompress(UnCompressZip, inbound_path, filename)
  return True


# Find file names with wild cards, and extract any ARC'd data files
def ExtractFile(inbound_path: str, filename: str):
  if '*' not in filename:
    return find_ifile(inbound_path, filename)

  basename, ext = os.path.splitext(filename)
  if uncompress_select(inbound_path, filename, ext):
    return find_ifile(inbound_path, basename + '.*')

  return ""


# Checks complete segment files, creating a fixed segment file together with a report file for notification
def CheckSegment(inbound_path, segment_filename, report_filename, notify_node):
  global ThisZone, ThisNet, cmtFileName, RptFile, oldDir, AnyProcessed, NotifyCmd, NoErrCmd\
       , ThisPrefixErr, ThisPvtErr, ThisPhoneErr, ThisBaudErr, ThisFlagErr, ThisUserErr, ThisReduErr, ThisDupErr\
       , ThisCaseConv, ThisConverted, ThisSpaces, ThisTailCommas, ThisDelEntry\
       , TotPrefixErr, TotPvtErr, TotPhoneErr, TotBaudErr, TotFlagErr, TotUserErr, TotReduErr, TotDupErr\
       , TotCaseConv, TotConverted, TotSpaces, TotTailCommas, TotDelEntry

  segment_filename = ExtractFile(inbound_path, segment_filename)
  if not segment_filename:
    return

  ThisZone       = DefaultZone
  ThisNet        = DefaultNet
  ThisPrefixErr  = 0
  ThisPvtErr     = 0
  ThisPhoneErr   = 0
  ThisBaudErr    = 0
  ThisFlagErr    = 0
  ThisUserErr    = 0
  ThisReduErr    = 0
  ThisDupErr     = 0
  ThisCaseConv   = 0
  ThisConverted  = 0
  ThisSpaces     = 0
  ThisTailCommas = 0
  ThisDelEntry   = 0
  AmDate, DayNum = NextFriday()
  Default_NLheader = ';A Fidonet Nodelist for ' + AmDate + daynumstr + DayNum + ' : 00000'
  OutFile   = None
  TempFName = GetTempFileName(segment_filename)
  if Touch:
    infname = TempFName
    try:
      os.replace(segment_filename, TempFName)
    except OSError as e:
      if e.errno != 2:  # file not found is normal
        print('! Unable to rename', segment_filename, 'to', TempFName, '- error:', e, file=sys.stderr)
      return
    try:
      OutFile = open(segment_filename, "w", encoding=codec, newline=crlf)
    except OSError as e:
      print('! Unable to create', segment_filename, '- error:', e, file=sys.stderr)
      return
  else:
    infname = segment_filename

  try:
    InFile = open(infname, encoding=codec)
  except OSError as e:
    if e.errno != 2:  # file not found is normal
      print('! Unable to open', infname, '- error:', e, file=sys.stderr)
    return

  try:
    RptFile = open(report_filename, "w", encoding=codec)
  except OSError as e:
    print('! Unable to create', report_filename, '- error:', e, file=sys.stderr)
    return

  error, cmtFileName, cmt_file = openConfigFile(DEFAULT_CMT_FILE_NAMES, cmtFileName, False)
  if not error:
    RptFile.writelines(cmt_file.readlines())
    cmt_file.close()
    print(file=RptFile)

  print_and_report('Processing file ' + segment_filename)
  handle_first_crc_line = True
  crc_line = ''
  crc = 0

  for line in InFile:
    line = line.rstrip()    # remove CR
    if line and line[0] == eof:  # Ignore 'soft' EOF character
      line = line[1:]
      if not line:
        continue
    if handle_first_crc_line:
      handle_first_crc_line = False
      if is_crcline(line):
        crc_line = line
      else:
        crc_line = Default_NLheader
    if line and line[0] != ';':
      line = ParseNodeLine(line)
    if Touch and line:
      print(line, file=OutFile)
      if crc_line != line:
        crc = crc16str(crc, line + crlf)

  InFile.close()
  if Touch:
    OutFile.write(eof)
    OutFile.close()
    os.remove(infname)
    crc_line = crc_line.rstrip('0123456789') + ("%05d" % crc)
    print(crc_line)
    os.replace(segment_filename, TempFName)
    try:
      OutFile = open(segment_filename, "w", encoding=codec, newline=crlf)
    except OSError as e:
      print('! Unable to create', segment_filename, '- error', e, file=sys.stderr)
      return

    InFile = open(TempFName, encoding=codec)
    line = InFile.readline().rstrip()
    print(crc_line, file=OutFile)
    if not is_crcline(line):
      print(line, file=OutFile)
    OutFile.write(InFile.read())
    InFile.close()
    OutFile.close()
    try:
      os.remove(TempFName)
    except OSError as e:
      print('! Unable to delete temporary file - error:', e, file=sys.stderr)
      return

  print()
  print(file=RptFile)
  print_and_report('SUMMARY REPORT FOR THIS NODELIST SEGMENT:', prechar='*')
  print_and_report('Prefix errors            : %d' % ThisPrefixErr, prechar='>')
  print_and_report('Pvt/phone errors         : %d' % ThisPvtErr, prechar='>')
  print_and_report('Phonenumber errors       : %d' % ThisPhoneErr, prechar='>')
  print_and_report('Baudrate errors          : %d' % ThisBaudErr, prechar='>')
  print_and_report('Flag errors              : %d' % ThisFlagErr, prechar='>')
  print_and_report('User flag errors         : %d' % ThisUserErr, prechar='>')
  print_and_report('Redundant flags          : %d' % ThisReduErr, prechar='>')
  print_and_report('Duplicate flags          : %d' % ThisDupErr, prechar='>')
  print_and_report('Case conversions         : %d' % ThisCaseConv, prechar='>')
  print_and_report('Converted flags          : %d' % ThisConverted, prechar='>')
  print_and_report('Entries with spaces      : %d' % ThisSpaces, prechar='>')
  print_and_report('Entries with tail commas : %d' % ThisTailCommas, prechar='>')
  print_and_report('Deleted entries          : %d' % ThisDelEntry, prechar='>')
  print()
  print("\n // ErrFlags v" + ErrFlagsVersion, file=RptFile)
  RptFile.close()

  TotPrefixErr += ThisPrefixErr
  TotPvtErr    += ThisPvtErr
  TotPhoneErr  += ThisPhoneErr
  TotBaudErr   += ThisBaudErr
  TotFlagErr   += ThisFlagErr
  TotUserErr   += ThisUserErr
  TotReduErr   += ThisReduErr
  TotDupErr    += ThisDupErr
  TotCaseConv  += ThisCaseConv
  TotConverted += ThisConverted
  TotSpaces    += ThisSpaces
  TotTailCommas+= ThisTailCommas
  TotDelEntry  += ThisDelEntry
  AnyProcessed = True

  if ThisPrefixErr + ThisPvtErr + ThisPhoneErr + ThisBaudErr + ThisFlagErr + ThisUserErr + ThisReduErr + ThisDupErr > 0:
    if not NotifyCmd:
      return
    else:
      cmd = NotifyCmd
  else:
    if not NoErrCmd:
      return
    else:
      cmd = NoErrCmd
  try:
    os.chdir(NotifyPath)
  except OSError as e:
    print('! Unable to CD to path', NotifyPath, '- error:', e, file=sys.stderr)
    return
  cmd = ireplace("%NODE%", notify_node, cmd)
  cmd = ireplace("%FILE%", report_filename, cmd)
  ExecuteCommand('Notify', cmd, oldDir)


def init():
  SignOn()
  ParseCTLFile()
  ParseTabFile()


def handle_segment_files():
  global SegmentFiles

  for segment_file in SegmentFiles:
    CheckSegment(InboundPath, segment_file.FileName, segment_file.RptFile, segment_file.Notifier)


def executecmd_when_any_processed():
  global AnyProcessed, ExecutePath, ExecuteCmd, oldDir

  if AnyProcessed and ExecuteCmd:
    if ExecutePath:
      try:
        os.chdir(ExecutePath)
      except OSError as e:
        print('! Unable to CD to path ', ExecutePath, ' - error:', e, file=sys.stderr)
        return
    ExecuteCommand('Execute', ExecuteCmd, oldDir)


def final_report():
  global TotPrefixErr, TotPvtErr, TotPhoneErr, TotBaudErr, TotFlagErr, TotUserErr \
       , TotReduErr, TotDupErr, TotCaseConv, TotConverted, TotSpaces, TotTailCommas, TotDelEntry
  print()
  print('* FINAL REPORT FOR ALL PROCESSED NODELIST SEGMENTS:')
  print('> Total prefix errors            : %d' % TotPrefixErr)
  print('> Total Pvt/phone errors         : %d' % TotPvtErr)
  print('> Total phonenumber errors       : %d' % TotPhoneErr)
  print('> Total baudrate errors          : %d' % TotBaudErr)
  print('> Total flag errors              : %d' % TotFlagErr)
  print('> Total user flag errors         : %d' % TotUserErr)
  print('> Total redundant flags          : %d' % TotReduErr)
  print('> Total duplicate flags          : %d' % TotDupErr)
  print('> Total case conversions         : %d' % TotCaseConv)
  print('> Total converted flags          : %d' % TotConverted)
  print('> Total entries with spaces      : %d' % TotSpaces)
  print('> Total entries with tail commas : %d' % TotTailCommas)
  print('> Total deleted entries          : %d' % TotDelEntry)


def main():
  init()
  handle_segment_files()
  executecmd_when_any_processed()
  final_report()


if __name__ == "__main__":
  main()
