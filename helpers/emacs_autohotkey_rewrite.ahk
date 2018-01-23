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
        ,"space": ["", True, "MacroCtrlSpace"] }}}


;       ['d'] = {'ctrl', 'd', false, nil},
;       ['e'] = {'ctrl', 'e', true, nil},
;       ['f'] = {nil, 'right', true, nil},
;       ['g'] = {nil, 'escape', false, nil},      
;       ['k'] = {'ctrl', 'k', false, nil},      
;       ['n'] = {nil, 'down', true, nil},
;       ['o'] = {nil, 'return', false, nil},
;       ['p'] = {nil, 'up', true, nil},
;       ['r'] = {'cmd', 'f', false, nil},      
;       ['s'] = {'cmd', 'f', false, nil},
;       ['v'] = {nil, 'pagedown', true, nil},
;       ['w'] = {'cmd', 'x', false, nil},
;       ['x'] = {nil, nil, false, 'macroStartCtrlX'},
;       ['y'] = {'cmd', 'v', false, nil},                        
;       ['space'] = {nil, nil, true, 'macroCtrlSpace'},




;       ['t'] = {nil, nil, false, 'macroAltTab'}, -- sorta working
;       ['j'] = {'cmd', 'space', false, nil}
;     }
;   },
;   ['globalEmacs'] = {
;     ['ctrl'] = {
;       ['a'] = {'ctrl', 'a', true, nil},
;       ['b'] = {nil, 'left', true, nil},
;       ['d'] = {'ctrl', 'd', false, nil},
;       ['e'] = {'ctrl', 'e', true, nil},
;       ['f'] = {nil, 'right', true, nil},
;       ['g'] = {nil, 'escape', false, nil},      
;       ['k'] = {'ctrl', 'k', false, nil},      
;       ['n'] = {nil, 'down', true, nil},
;       ['o'] = {nil, 'return', false, nil},
;       ['p'] = {nil, 'up', true, nil},
;       ['r'] = {'cmd', 'f', false, nil},      
;       ['s'] = {'cmd', 'f', false, nil},
;       ['v'] = {nil, 'pagedown', true, nil},
;       ['w'] = {'cmd', 'x', false, nil},
;       ['x'] = {nil, nil, false, 'macroStartCtrlX'},
;       ['y'] = {'cmd', 'v', false, nil},                        
;       ['space'] = {nil, nil, true, 'macroCtrlSpace'},
;     },
;     ['ctrlXPrefix'] = {
;       ['f'] = {'cmd', 'o', false, nil},
;       ['g'] = {'cmd', 'f', false, nil},      
;       ['h'] = {'cmd', 'a', false, nil},
;       ['k'] = {'cmd', 'w', false, nil},
;       ['r'] = {'cmd', 'r', false, nil},      
;       ['s'] = {'cmd', 's', false, nil},
;       ['u'] = {'cmd', 'z', false, nil},
;       ['w'] = {{'shift', 'cmd'}, 's', false, nil},      
;     },
;     ['alt'] = {
;       ['f'] = {'alt', 'f', true, nil},
;       ['n'] = {'cmd', 'n', false, nil},
;       ['v'] = {nil, 'pageup', true, nil},
;       ['w'] = {'cmd', 'c', false, nil},
;       ['y'] = {'cmd', 'v', false, nil},            
;     },
;     ['altShift'] = {
;       ['.'] = {nil, 'end', false, nil},
;       [','] = {nil, 'home', false, nil},      
;     },
;   },
;   ['Google Chrome'] = {
;     ['ctrlXPrefix'] = {
;       ['b'] = {'cmd', 'b', false, nil},
;       ['d'] = {{'cmd', 'alt'}, 'j', false, nil}, 
;       ['f'] = {'cmd', 'l', false, nil},
;     }
;   }
; }

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
^Space::
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


ProcessKey(key)
{
  modifiers := ParseMods(key)
  letter := ParseLetter(key)

  currentApp := CurrentApp()

  If (ctrlXActive && (modifiers == "ctrl"))
  {
    modidifers := "ctrlXPrefix"
  }

  namespace := "globalEmacs"
    
  If KeybindingExists("globalOverride", modifiers, letter)
  {
    namespace := "globalOverride"   
  }
  Else If (currentApp && KeybindingExists(currentApp, modifiers, letter))
  {
    namespace := currentApp      
  }

  If IsEmacs() && namespace != "globalOverride"
  {
    Passthrough(key)
    Return
  }

  If KeybindingExists(namespace, modifiers, letter)
  {
    LookupKeyAndTranslate(namespace, modifiers, letter)
  }
  Else
  {
    Passthrough(key)
  }

  Return
}

LookupKeyAndTranslate(namespace, modifiers, letter)
{
  config := keys[namespace][modifiers][letter]
  toKeys := config[1]
  ctrlSpaceSensitive := config[2]
  toMacro := config[3]

  If (toMacro && (toMacro != ""))
  {
    ; MsgBox Executing a macro: %toMacro%
    %toMacro%()
  }
  Else
  {
    ;MsgBox Sending: %toModifiers%%toLetter%
    toKeys := AddShift(toKeys, ctrlSpaceSensitive)
    Send %toKeys%
  }

  If !ctrlSpaceSensitive
  {
    ctrlSpaceActive := False
  }

  If toMacro = 'macroStartCtrlX'
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
  If InStr(key, "^") || InStr(key, "$^")
  {
    Return "ctrl"
  }
  Else If InStr(key, "!")
  {
    Return "alt"
  }

  Return key
}

ParseLetter(key)
{
  If InStr(key, "Space")
  {
    Return "space"
  }

  StringRight, letter, key, 1
  Return letter
}

ShortModifiers(longModifiers, ctrlSpaceSensitive)
{
  modifiers := ""

  If (longModifiers = "ctrl")
  {
    modifiers := "^"
  }
  Else If (longModifiers = "alt")
  {
    modifiers := "!"
  }

  Return AddShift(modifiers, ctrlSpaceSensitive)
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

CurrentApp()
{
  WinGet, exeName, ProcessName, A
  StringLower, exeName, exeName

  Return exeName
}
