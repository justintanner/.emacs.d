; Emacs AutoHotkey Script
; Author: Justin Tanner
; Email: work@jwtanner.com
; License: MIT

; What does this script do?
; Allows you to have *most* Emacs keybindings in other apps.
; Ctrl-Space space be used to mark and cut text just like Emacs. Also enables Emacs prefix keys such as Ctrl-xs (save).

; Installation
; 1) Download and Install AutoHotkey https://autohotkey.com/
; 2) Launch emacs_autohotkey.ahk
; 3) Add emacs_authotkey to your Windows startup script.

; Customization
; To customize the keybindings modfiy the global "keys" below. AutoHotkey is a little fussy about whitespace, so please
; follow the syntax of the existing data structure exactly without adding whitespace or newlines.

; Namespaces
; This script uses namespaces to send different commands to different apps.
; "globalOverride" contains keybindings that override all other apps (including Emacs)
; "globalEmacs" brings Emacs style keybindings to all other apps
; "chrome.exe" (and other EXE names) specifies app specific keybindings taking precendence over "globalEmacs"

; Syntax Example
; "globalEmacs" : { "ctrl" { "a": ["{Home}", False, ""] } }
; This keybinding states, for all apps other than Emacs map Ctrl+a to Home and maintain any mark previously set while editing text.

; "globalEmacs" : { "ctrl" { "k": ["", False, "MacroKillLine"] } }
; This keybinding states, for all apps other than Emacs map Ctrl+k to a macro called "MacroKillLine" (defined in a function) and unset
; any mark previously set.

#SingleInstance
#Installkeybdhook
#UseHook
SetKeyDelay 0

global keys
:= {"globalOverride"
  : {"ctrl"
    : {"t": ["!{Esc}", False, ""]
      ,"j": ["{Ctrl up}{LWin}", False, ""] }}
 , "globalEmacs"
    : {"ctrl"
      : {"a": ["{Home}", True, ""]
        ,"b": ["{Left}", True, ""]
        ,"d": ["{Del}", False, ""]
        ,"e": ["{End}", True, ""]
        ,"f": ["{Right}", True, ""]
        ,"g": ["{Escape}", False, ""]
        ,"k": ["", False, "MacroKillLine"]
        ,"n": ["{Down}", True, ""]
        ,"o": ["{Enter}", False, ""]
        ,"p": ["{Up}", True, ""]
        ,"r": ["^f", False, ""]
        ,"s": ["^f", False, ""]
        ,"v": ["{PgDown}", True, ""]
        ,"w": ["^x", False, ""]
        ,"x": ["", False, "MacroStartCtrlX"]
        ,"y": ["^v", False, ""]
        ,"space": ["", True, "MacroCtrlSpace"] }
    , "ctrlXPrefix"
      : {"f": ["^o", False, ""]
        ,"g": ["^f", False, ""]
        ,"h": ["^a", False, ""]
        ,"k": ["!{F4}", False, ""]
        ,"r": ["{F5}", False, ""]
        ,"s": ["^s", False, ""]
        ,"u": ["^z", False, ""]
        ,"w": ["{F12}", False, ""] }
    , "alt"
      : {"f": ["^{Right}", True, ""]
        ,"n": ["^n", False, ""]
        ,"v": ["{PgUp}", True, ""]
        ,"w": ["^c", False, ""]
        ,"y": ["^v", False, ""] }
   , "altShift"
      : {".": ["^{End}", True, ""]
       , ",": ["^{Home}", True, ""] }}
 , "chrome.exe"
   : {"ctrlXPrefix"
     : {"b": ["^o", False, ""]
      , "d": ["^+j", False, ""]
      , "f": ["^l", False, ""] }}}

global appsWithNativeEmacsKeybindings = ["emacs.exe", "rubymine64.exe", "conemu64.exe"]
global ctrlXActive := False
global ctrlSpaceActive := False

^a::
^b::
^c::
^d::
^e::
^f::
^g::
^h::
^i::
^j::
^k::
^l::
^m::
^n::
^o::
^p::
^q::
^r::
^s::
^t::
^u::
^v::
^w::
^x::
^y::
^z::
!a::
!b::
!c::
!d::
!e::
!f::
!g::
!h::
!i::
!j::
!k::
!l::
!m::
!n::
!o::
!p::
!q::
!r::
!s::
!t::
!u::
!v::
!w::
!x::
!y::
!z::
^Space::
!+,::
!+.::
ProcessKeystrokes(A_ThisHotkey)
Return

ProcessKeystrokes(keystrokes)
{
  mods := ParseMods(keystrokes)
  key := ParseKey(keystrokes)
  namespace := CurrentNamespace(mods, key)

  If (IsEmacs() && namespace != "globalOverride")
  {
    Passthrough(keystrokes)
    Return
  }

  If KeybindingExists(namespace, mods, key)
  {
    LookupAndTranslate(namespace, mods, key)
  }
  Else
  {
    Passthrough(keystrokes)
  }

  Return
}

LookupAndTranslate(namespace, mods, key)
{
  config := keys[namespace][mods][key]
  toKey := config[1]
  ctrlSpaceSensitive := config[2]
  toMacro := config[3]

  If (toMacro && (toMacro != ""))
  {
    ; MsgBox Executing a macro: %toMacro%
    %toMacro%()
  }
  Else
  {
    toKey := AddShift(toKey, ctrlSpaceSensitive)
    ; MsgBox Sending: %toMods%%toKey%
    Send %toKey%
  }

  If !ctrlSpaceSensitive
  {
    ctrlSpaceActive := False
  }

  If (toMacro != "MacroStartCtrlX")
  {
    ctrlXActive := False
  }
}

KeybindingExists(namespace, mods, key)
{
  Return (keys[namespace] && keys[namespace][mods] && keys[namespace][mods][key])
}

ParseMods(keystrokes)
{
  If InStr(keystrokes, "!+")
  {
    Return "altShift"
  }
  Else If InStr(keystrokes, "^")
  {
    If (ctrlXActive)
    {
      Return "ctrlXPrefix"
    }
    Else
    {
      Return "ctrl"
    }
  }
  Else If InStr(keystrokes, "!")
  {
    Return "alt"
  }

  Return keystrokes
}

ParseKey(keystrokes)
{
  If InStr(keystrokes, "Space")
  {
    Return "space"
  }

  StringRight, letter, keystrokes, 1
  Return letter
}

AddShift(mods, ctrlSpaceSensitive)
{
  holdShift := (ctrlSpaceSensitive && ctrlSpaceActive)

  If (holdShift)
  {
    Return "+" . mods
  }

  Return mods
}

Passthrough(keystrokes)
{
    If InStr(keystrokes, "Space")
    {
      Send ^{Space}
    }
    Else
    {
      Send %keystrokes%
    }
}

IsEmacs()
{
  For index, appName in appsWithNativeEmacsKeybindings
  {
    StringLower appNameLower, appName

    If IsProgram(appNameLower)
    {
      Return True
    }
  }

  Return False
}

IsProgram(classOrExec)
{
  IfWinActive, ahk_class %classOrExec%
  {
    Return True
  }

  IfWinActive, ahk_exe %classOrExec%
  {
    Return True
  }

  Return False
}

CurrentNamespace(mods, key)
{
  currentApp := CurrentApp()
    
  If KeybindingExists("globalOverride", mods, key)
  {
    Return "globalOverride"   
  }
  Else If (currentApp && KeybindingExists(currentApp, mods, key))
  {
    Return currentApp      
  }

  Return "globalEmacs"
}

CurrentApp()
{
  WinGet, exeName, ProcessName, A
  StringLower, exeName, exeName

  Return exeName
}

MacroCtrlSpace()
{
  ctrlSpaceActive := True
  Send {Shift}
}

MacroStartCtrlX()
{
  ctrlXActive := True
  SetTimer, ClearCtrlX, -750
}

ClearCtrlX()
{
  ctrlXActive := False
}

MacroKillLine()
{
  Send {ShiftDown}{END}{ShiftUp}
  Sleep 50
  Send ^x
  Send {Del}
}
