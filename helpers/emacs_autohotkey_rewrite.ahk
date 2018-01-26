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
      : {"a": ["{Home}", False, ""]
        ,"b": ["{Left}", False, ""]
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
ProcessKey(A_ThisHotkey)
Return

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

ProcessKey(modAndKey)
{
  mods := ParseMods(modAndKey)
  key := ParseKey(modAndKey)
  namespace := CurrentNamespace(mods, key)

  If (IsEmacs() && namespace != "globalOverride")
  {
    Passthrough(modAndKey)
    Return
  }

  If KeybindingExists(namespace, mods, key)
  {
    LookupKeyAndTranslate(namespace, mods, key)
  }
  Else
  {
    Passthrough(modAndKey)
  }

  Return
}

LookupKeyAndTranslate(namespace, modifiers, key)
{
  config := keys[namespace][modifiers][key]
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
    ; MsgBox Sending: %toModifiers%%toKey%
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

KeybindingExists(namespace, mods, letter)
{
  Return (keys[namespace] && keys[namespace][mods] && keys[namespace][mods][letter])
}

ParseMods(key)
{
  If InStr(key, "!+")
  {
    Return "altShift"
  }
  Else If InStr(key, "^")
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
  Else If InStr(key, "!")
  {
    Return "alt"
  }

  Return key
}

ParseKey(key)
{
  If InStr(key, "Space")
  {
    Return "space"
  }

  StringRight, letter, key, 1
  Return letter
}

AddShift(modifiers, ctrlSpaceSensitive)
{
  holdShift := (ctrlSpaceSensitive && ctrlSpaceActive)

  If (holdShift)
  {
    Return "+" . modifiers
  }

  Return modifiers
}

Passthrough(key)
{
    If InStr(key, "Space")
    {
      Send ^{Space}
    }
    Else
    {
      Send %key%
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
