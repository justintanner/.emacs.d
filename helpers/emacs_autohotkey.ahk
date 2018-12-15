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
; 3) Set emacs_authotkey.ahk to start on boot.

; Customization
; To customize the keybindings modfiy the global "keys" below. AutoHotkey is a little fussy about whitespace, so please
; follow the syntax of the existing data structure exactly without adding whitespace or newlines.

; Namespaces
; This script orangizes its keybindings into namespaces to send different commands to different apps.
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
:= {"globalEmacs"
    : {"ctrl"
      : {"a": ["{Home}", True, ""]
        ,"b": ["{Left}", True, ""]
        ,"d": ["{Del}", False, ""]
        ,"e": ["{End}", True, ""]
        ,"f": ["{Right}", True, ""]
        ,"g": ["{Escape}", False, ""]
        ,"h": ["", False, ""]
        ,"k": ["", False, "MacroKillLine"]
        ,"j": ["", True, ""]
        ,"n": ["{Down}", True, ""]
        ,"o": ["{Enter}", False, ""]
        ,"p": ["{Up}", True, ""]
        ,"r": ["^f", False, ""]
        ,"s": ["^f", False, ""]
        ,"t": ["", True, ""]
        ,"v": ["{PgDn}", True, ""]
        ,"w": ["^x", False, ""]
        ,"y": ["^v", False, ""]
        ,"/": ["^z", False, ""]
        ,"Space": ["", True, "MacroCtrlSpace"]
        ,"Backspace": ["^+{Left}^x", False,""] }
    , "ctrlXPrefix"
      : {"c": ["!{F4}", False, ""]
        ,"f": ["^o", False, ""]
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
        ,"y": ["^v", False, ""]
        ,"Backspace": ["^z", False, ""] }
   , "altShift"
      : {".": ["^{End}", True, ""]
       , ",": ["^{Home}", True, ""] } } }

; Add Evernote
keys["chrome.exe"]
:= {"ctrlXPrefix"
   : {"b": ["^o", False, ""]
    , "d": ["^+j", False, ""]
    , "f": ["^l", False, ""]
    , "k": ["^w", False, ""] }
  , "alt"
    : {"n": ["^t", False, ""] } }

keys["globalOverride"]
:= {"ctrl"
    : {"x": ["", False, "MacroStartCtrlX"] }
  , "ctrlXPrefix"
    : {"j": ["^{Esc}", False, ""]
     , "t": ["!{Space}", False, ""]
     , "]": ["^#{Right}", False, ""]
     , "[": ["^#{Left}", False, ""] }
  , "alt"
    : {"m": ["{LWin down}{Up}{LWin up}", False, ""] } }

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
^[::
^]::
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
^/::
^Space::
^Backspace::
!Backspace::
!+,::
!+.::
!^t::
ProcessKeystrokes(A_ThisHotkey)
Return

; Entry point for processing keystrokes and taking the appropriate action.
; @param keystrokes String keystrokes pressed by the user (usually A_ThisHotkey)
ProcessKeystrokes(keystrokes)
{
  mods := ParseMods(keystrokes)
  key := ParseKey(keystrokes)
  namespace := CurrentNamespace(mods, key)

  OutputDebug Mods: %mods% key: %key% namespace: %namespace%

  ; TODO: Allow app specific keybinding to override emacs, like hammerspoon.
  If (IsEmacs() && namespace != "globalOverride")
  {
    SendOriginal(keystrokes)
    Return
  }

  If KeybindingExists(namespace, mods, key)
  {
    LookupAndTranslate(namespace, mods, key)
  }
  Else
  {
    SendOriginal(keystrokes)
  }

  Return
}

; Looks up a keybinding in the global keybindings table and translates that keybinding or runs a macro.
; @param namespace String the namespace to lookup a key (eg Google Chrome or GlobalEmacs)
; @param mods String modifiers key such as ctrl or alt.
; @param key String keys such as: a, b or c
LookupAndTranslate(namespace, mods, key)
{
  config := keys[namespace][mods][key]
  toKey := config[1]
  ctrlSpaceSensitive := config[2]
  toMacro := config[3]

  If (toMacro && (toMacro != ""))
  {
    OutputDebug Executing a macro: %toMacro%
    %toMacro%()
  }
  Else
  {
    toKey := AddShift(toKey, ctrlSpaceSensitive)
    OutputDebug Sending: %toKey%
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

; Checks the global keys table for a keybinding
; @param namespace String namespace of the keybinding (eg globalEmacs, chrome.exe, etc)
; @param mods String modifier keys such as alt, ctrl, ctrlXPrefix, etc
; @param key String key such as a, b, c, etc
KeybindingExists(namespace, mods, key)
{
  Return (keys[namespace] && keys[namespace][mods] && keys[namespace][mods][key])
}

; Parses out the modifiers from a keystrokes strings ommiting the key
; @param keystroke String contains autohotkey keystrokes such as ^c
; @return String translated modifiers such as ctrl or alt
ParseMods(keystrokes)
{
  If InStr(keystrokes, "!^t")
  {
    Return "ctrlXPrefix"
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
  Else If InStr(keystrokes, "!+")
  {
    Return "altShift"
  }
  Else If InStr(keystrokes, "!")
  {
    Return "alt"
  }

  Return keystrokes
}

; Parses out the key from a keystrokes string without the modifier
; @param keystroke String contains autohotkey keystrokes such as ^c
; @return String keys such as c
ParseKey(keystrokes)
{
  If InStr(keystrokes, "& t")
  {
    Return "t"
  }
  Else If InStr(keystrokes, "Backspace")
  {
    Return "Backspace"
  }
  Else If InStr(keystrokes, "Space")
  {
    Return "Space"
  }

  StringRight, letter, keystrokes, 1
  Return letter
}

; Appends a shift modifier key (if needed) to the existing mods.
; @param mods String modifier keys such as alt, ctrl, ctrlXPrefix, etc
; @param ctrlSpaceSensitive Boolean current keybinding is amenable to holding shift
AddShift(mods, ctrlSpaceSensitive)
{
  holdShift := (ctrlSpaceSensitive && ctrlSpaceActive)

  If (holdShift)
  {
    Return "+" . mods
  }

  Return mods
}

; Execute original keystrokes without translating
; @param keystrokes String original keystrokes
SendOriginal(keystrokes)
{
    ; Forgot why these exceptions exist!
    If InStr(keystrokes, "Backspace")
    {
      Send ^{Backspace}
    }
    Else If InStr(keystrokes, "Space")
    {
      Send ^{Space}
    }
    Else
    {
      Send %keystrokes%
    }
}

; Does the current app already have Emacs keybinings
; @return Boolean true if Emacs
IsEmacs()
{
  For index, appName in appsWithNativeEmacsKeybindings
  {
    StringLower appNameLower, appName

    If IsProgramActive(appNameLower)
    {
      Return True
    }
  }

  Return False
}

; Does the current foreground application match a class or exec name
; @param classOrExec String class name (eg Google Chrome) or exe name (eg chrome.exe)
; @return Boolean true on match
IsProgramActive(classOrExec)
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

; Current namespace based on the current app in the foreground and matching keybindings
; @param mods String modifier keys such as alt, ctrl, ctrlXPrefix, etc
; @param key String key such as a, b, c, etc
; @return String current namespace such as globalOverride, globalEmacs, etc
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

; Current foreground process name
CurrentApp()
{
  WinGet, exeName, ProcessName, A
  StringLower, exeName, exeName

  Return exeName
}

; Start text selection in a non Emacs app
MacroCtrlSpace()
{
  ctrlSpaceActive := True
  Send {Shift}
}

; Activate the Ctrl-x prefix key
MacroStartCtrlX()
{
  ctrlXActive := True
  SetTimer, ClearCtrlX, -750

  If (IsEmacs())
  {
    Send ^x
  }
}

; Clears Ctrl-x prefix state, invoked by the timer above.
ClearCtrlX()
{
  ctrlXActive := False
}

; Macro to kill a line and add it to the clipboard
MacroKillLine()
{
  Send {ShiftDown}{END}{ShiftUp}
  Send ^x
  Send {Del}
}

