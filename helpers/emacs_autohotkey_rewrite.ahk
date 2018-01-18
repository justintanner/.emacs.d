#SingleInstance
#Installkeybdhook
#UseHook
SetKeyDelay 0

global keys
:= {"globalOverride"
  : {"ctrl"
    : {"t": ["", "", False, "macroAltTab"]
      ,"j": ["alt", "tab", False, ""] }}
 , "globalEmacs"
    : {"ctrl"
      : {"z": ["", "", False, "MacroTest"] }}}

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

^z::
ProcessKey(A_ThisHotkey)
Return

MacroTest()
{
  MsgBox YoDawg
}

ProcessKey(key)
{
  If IsEmacs()
  {
    Send key
    Return
  }

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

  ;MsgBox %currentApp% + %namespace% + %modifiers% + %letter%

  If KeybindingExists(namespace, modifiers, letter)
  {
    LookupKeyAndTranslate(namespace, modifiers, letter)
  }
  Else
  {
    Send key
  }

  Return
}

LookupKeyAndTranslate(namespace, mod, key)
{
  config := keys[namespace][mod][key]
  toMod := ShortModifiers(config[1])
  toKey := config[2]
  ctrlSpaceSensitive := config[3]
  toMacro := config[4]

  If (toMacro && (toMacro != ""))
  {
    ; MsgBox Executing a macro: %toMacro%
    %toMacro%()

  }
  Else
  {
    ; MsgBox Sending: %toMod%%toKey%
    holdShift := (ctrlSpaceSensitive && ctrlSpaceActive)
    ; TODO: Move create a function that deals with shift
    Send %toMod%%toKey%
  }

  If !ctrlSpaceSensitive
  {
    ctrlSpaceActive := false
  }

  ; Does this work? (need parens?)
  If toMacro = 'macroStartCtrlX'
  {
    ctrlXActive := false
  }
}

KeybindingExists(namespace, mods, letter)
{
  Return (keys[namespace] && keys[namespace][mods] && keys[namespace][mods][letter])
}

ParseMods(key)
{
  If InStr(key, "^")
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
  StringRight, letter, key, 1
  Return letter
}

ShortModifiers(longModifiers)
{
  If (longModifiers = "ctrl")
  {
    Return "^"
  }
  Else If (longModifiers = "alt")
  {
    Return "!"
  }
}

IsEmacs()
{
  For index, appName in appsWithNativeEmacsKeybindings
  {
    StringLower, appNameLower, appName

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
