#SingleInstance
#Installkeybdhook
#UseHook

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

^a::
^b::
;hey := keys["globalOverride"]["ctrl"]["j"][1]
ProcessKey(A_ThisHotkey)
Return

ProcessKey(key)
{
  MsgBox %key%
  Return
}

ParseMod(key)
{
  ; Parse out modifiers
}

ParseKey(key)
{
  ; Parse out modifiers
}
