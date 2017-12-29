--- Emacs Hammerspoon Script
-- Author: Justin Tanner
-- Email: work@jwtanner.com
-- License: MIT

--- What does this thing do?
-- Allows you to have Emacs *like* keybindings in apps other than Emacs.
-- You can use Ctrl-Space to mark and cut text just like Emacs. Also enables Emacs prefix keys such as Ctrl-xs (save).

--- Installation
-- 1) Download and hammerspoon http://www.hammerspoon.org/
-- 2) Copy emacs_hammerspoon.lua to ~/.hammerspoon/init.lua
-- 3) Reload hammerspoon if already running

--- Keybindings Lookup Table

-- Namespaces:
-- globalOverride overrides both Emacs and non-Emacs apps
-- globalEmacs overrides all apps except those specified in appsWithNativeEmacsKeybindings
-- App Name (eg Google Chrome) specifies app specific exceptions

-- Usage:
-- keys[namespace][modifier-key][key] = {to-modifier, to-key, mark-sensitive, macro}

local keys = {
  ['globalOverride'] = {
    ['ctrl'] = {
      ['t'] = {nil, nil, false, 'macroAltTab'}, -- sorta working
      ['j'] = {'cmd', 'space', false, nil}
    }
  },
  ['globalEmacs'] = {
    ['ctrl'] = {
      ['a'] = {'ctrl', 'a', true, nil},
      ['b'] = {nil, 'left', true, nil},
      ['d'] = {'ctrl', 'd', false, nil},
      ['e'] = {'ctrl', 'e', true, nil},
      ['f'] = {nil, 'right', true, nil},
      ['g'] = {nil, 'escape', false, nil},      
      ['k'] = {'ctrl', 'k', false, nil},      
      ['n'] = {nil, 'down', true, nil},
      ['o'] = {nil, 'return', false, nil},
      ['p'] = {nil, 'up', true, nil},
      ['r'] = {'cmd', 'f', false, nil},      
      ['s'] = {'cmd', 'f', false, nil},
      ['v'] = {nil, 'pagedown', true, nil},
      ['w'] = {'cmd', 'x', false, nil},
      ['x'] = {nil, nil, false, 'macroStartCtrlX'},
      ['y'] = {'cmd', 'v', false, nil},                        
      ['space'] = {nil, nil, true, 'macroCtrlSpace'},
    },
    ['ctrlXPrefix'] = {
      ['f'] = {'cmd', 'o', false, nil},
      ['g'] = {'cmd', 'f', false, nil},      
      ['h'] = {'cmd', 'a', false, nil},
      ['k'] = {'cmd', 'w', false, nil},
      ['r'] = {'cmd', 'r', false, nil},      
      ['s'] = {'cmd', 's', false, nil},
      ['u'] = {'cmd', 'z', false, nil},
      ['w'] = {{'shift', 'cmd'}, 's', false, nil},      
    },
    ['alt'] = {
      ['f'] = {'alt', 'f', true, nil},
      ['n'] = {'cmd', 'n', false, nil},
      ['v'] = {nil, 'pageup', true, nil},
      ['w'] = {'cmd', 'c', false, nil},
      ['y'] = {'cmd', 'v', false, nil},            
    },
    ['altShift'] = {
      ['.'] = {nil, 'end', false, nil},
      [','] = {nil, 'home', false, nil},      
    },
  },
  ['Google Chrome'] = {
    ['ctrlXPrefix'] = {
      ['b'] = {'cmd', 'b', false, nil},
      ['d'] = {{'ctrl', 'shift'}, 'j', false, nil}, 
      ['f'] = {'cmd', 'l', false, nil},
    }
  }
}

-- NOTE: Use lower case app names
local appsWithNativeEmacsKeybindings = {
  'emacs',
  'rubymine',
  'terminal'
}

local ctrlXActive = false
local ctrlSpaceActive = false
local currentApp = nil
local emacsMap = hs.hotkey.modal.new()
local overrideMap = hs.hotkey.modal.new()

--- Processes a keybinding. Translates keys or runs a macro.
-- @param mod String containing a modifier such as: ctrl, alt or ctrlXPrefix
-- @param key String containing a key such as: a, b, c, etc
function processKey(mod, key)
  return function()
    emacsMap:exit()

    if ctrlXActive and mod == 'ctrl' then
      mod = 'ctrlXPrefix'
    end

    namespace = 'globalEmacs'
    
    if keybindingExists('globalOverride', mod, key) then
      namespace = 'globalOverride'   
    elseif currentApp ~= nil and keybindingExists(currentApp, mod, key) then
      namespace = currentApp      
    end

    if keybindingExists(namespace, mod, key) then
      lookupKeyAndTranslate(namespace, mod, key)
    else
      tapKey(mod, key)
    end

    emacsMap:enter()
  end
end

--- Executes a keystroke with hammerspoon.
-- @param mods String or table containing a modifiers
-- @param key String containing a key such a key
function tapKey(mods, key)
  -- Faster than hs.eventtap.keystroke
  hs.eventtap.event.newKeyEvent(mods, key, true):post()
  hs.eventtap.event.newKeyEvent(mods, key, false):post()
end

--- Looks up a keybinding in the global keybindings table and translates that keybinding or runs a macro.
-- @param namespace String containg the namespace to lookup a key (eg Google Chrome or GlobalEmacs)
-- @param mod String containing a modifier key such as ctrl or alt. Also accepts modifiers with states such as ctrlXPrefix
-- @param key String containing a key such as: a, b or c
function lookupKeyAndTranslate(namespace, mod, key)
  config = keys[namespace][mod][key]
  toMod = config[1]
  toKey = config[2]
  ctrlSpaceSensitive = config[3]
  toMacro = config[4]

  if toMacro ~= nil then
    _G[toMacro]()
    print('Executing a macro ' .. toMacro)              
  else
    holdShift = (ctrlSpaceSensitive and ctrlSpaceActive)
    
    tapKey(prepModifier(toMod, holdShift), toKey)

    print(changingMessage(mod, key, toMod, toKey, holdShift))
  end

  if not ctrlSpaceSensitive then
    ctrlSpaceActive = false
  end

  if toMacro ~= 'macroStartCtrlX' then
    ctrlXActive = false
  end
end

function changingMessage(fromMod, fromKey, toMod, toKey, holdingShift)
  message = 'Changing ' .. fromMod .. '+' .. fromKey .. ' to '

  if holdingShift then
    message = message .. 'shift'
   end

  if type(toMod) == 'string' then
    message = message .. toMod
  elseif type(toMod) == 'table' then
    for index, mod in pairs(toMod) do
      message = message .. mod .. '+'
    end
  end
  
  return message .. ' + ' .. (toKey or '')
end

function prepModifier(mod, holdShift)
  if holdShift then
    return addShift(mod)
  end

  if type(mod) == 'string' then
    return {mod}
  end

  return {}
end

function addShift(mod)
  if type(mod) == 'string' then
    return {'shift', mod}
  elseif type(mod) == 'table' then
    table.insert(mod, 1, 'shift')
    return mod
  end
  
  return {'shift'}
end

function keybindingExists(namespace, mod, key)
  return (
    keys[namespace] ~= nil and
    keys[namespace][mod] ~= nil and
    keys[namespace][mod][key] ~= nil)
end

function assignKeys()
  letters = {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
             'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'}
  
  for i, letter in ipairs(letters) do
    emacsMap:bind('ctrl', letter, processKey('ctrl', letter), nil)
    emacsMap:bind('alt', letter, processKey('alt', letter), nil)  
  end
  
  emacsMap:bind('ctrl', 'space', processKey('ctrl', 'space'), nil)
  emacsMap:bind({'alt', 'shift'}, '.', processKey('altShift', '.'), nil)
  emacsMap:bind({'alt', 'shift'}, ',', processKey('altShift', ','), nil)

  overrideMap:bind('ctrl', 't', processKey('ctrl', 't'), nil)
  overrideMap:bind('ctrl', 'j', processKey('ctrl', 'j'), nil)
end

function hasValue (tab, val)
  for index, value in ipairs(tab) do
    if value == val then
      return true
    end
  end

  return false
end

function chooseKeyMap()
  if hasValue(appsWithNativeEmacsKeybindings, currentApp:lower()) then
    print('Turnning OFF keybindings for: ' .. currentApp)
    emacsMap:exit()      

  else
    print('Turning ON keybindings for: ' .. currentApp)
    emacsMap:enter()      
  end
end

function appOnStartup()
  app = hs.application.frontmostApplication()

  if app ~= nil then
    return app:title()
  end
end

function appWatcherFunction(appName, eventType, appObject)
  if (eventType == hs.application.watcher.activated) then
    currentApp = appName
    
    chooseKeyMap()    
  end
end

-- Macro for apps that map ctrl+k to something else
function macroKillLine()
  tapKey({'shift', 'ctrl'}, 'e')
  tapKey({}, 'shift')
  tapKey({'cmd'}, 'x')
  ctrlSpaceActive = false
end

function macroAltTab()
  -- Include minimized/hidden windows (sorta works)
  switcher_space = hs.window.switcher.new(hs.window.filter.new():setCurrentSpace(true):setDefaultFilter{})
  switcher_space.nextWindow()

  window = hs.window.frontmostWindow()
  window:focus()
end

function macroCtrlSpace()
  ctrlSpaceActive = not ctrlSpaceActive
  
  tapKey({}, 'shift')
end

function macroStartCtrlX()
  ctrlXActive = true

  hs.timer.doAfter(0.75,function() ctrlXActive = false end)
end

-- Application start
print('---------------------------------')
print('Starting Emacs hammerspoon Script')
assignKeys()

currentApp = appOnStartup()

chooseKeyMap()

local appWatcher = hs.application.watcher.new(appWatcherFunction)
appWatcher:start()

overrideMap:enter()      
