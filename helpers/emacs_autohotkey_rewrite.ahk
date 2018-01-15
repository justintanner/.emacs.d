#SingleInstance
#Installkeybdhook
#UseHook
SetKeyDelay 0

global keys
:= {"globalOverride"
  : {"ctrl"
    : {"t": ["", "", False, "macroAltTab"]
      ,"j": ["alt", "tab", False, ""]}}}

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
;hey := keys["globalOverride"]["ctrl"]["j"][1]
ProcessKey(A_ThisHotkey)
Return

; function processKey(mod, key)
;   return function()
;     emacsMap:exit()
;     if ctrlXActive and mod == 'ctrl' then
;       mod = 'ctrlXPrefix'
;     end

;     namespace = 'globalEmacs'
    
;     if keybindingExists('globalOverride', mod, key) then
;       namespace = 'globalOverride'   
;     elseif currentApp ~= nil and keybindingExists(currentApp, mod, key) then
;       namespace = currentApp      
;     end

;     if keybindingExists(namespace, mod, key) then
;       lookupKeyAndTranslate(namespace, mod, key)
;     else
;       tapKey(mod, key)
;     end

;     emacsMap:enter()
;   end
; end

ProcessKey(key)
{
  If IsEmacs()
  {
    Send key
    Return
  }

  modifiers := ParseMods(key)
  letter := ParseLetter(key)

  If (ctrlXActive && (modifiers == "ctrl"))
  {
    modidifers := "ctrlXPrefix"
  }

  namespace := "globalEmacs"
    
  If KeybindingExists("globalOverride", modifiers, key)
  {
    namespace := "globalOverride"   
  }
  Else If (currentApp && KeybindingExists(currentApp, modifiers, key))
  {
    namespace := currentApp      
  }

  ; LookupKeyAndTranslate(namespace, modifiers, key)

  ;MsgBox %modifiers% + %letter%
  Return
}

LookupKeyAndTranslate(namespace, mod, key)
{
  config := keys[namespace][mod][key]
  toMod := config[1]
  toKey := config[2]
  ctrlSpaceSensitive := config[3]
  toMacro := config[4]

  If (toMacro && (toMacro != ""))
  {
    %toMacro%()
    OutputDebug "Executing a macro " + toMacro
  }
  Else
  {
    holdShift := (ctrlSpaceSensitive && ctrlSpaceActive)
    ;; Send Key
    ;; Debug statement
  }

  If !ctrlSpaceSensitive
  {
    ctrlSpaceActive := false
  }

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
  letter = ""
  StringRight, letter, key, 1
  Return letter
}

IsEmacs()
{
  For index, appName in appsWithNativeEmacsKeybindings
  {
    appNameLower := ""
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
