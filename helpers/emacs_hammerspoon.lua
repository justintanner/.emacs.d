-- Emacs Hammerspoon Script
-- Author: Justin Tanner
-- Email: work@jwtanner.com
-- License: MIT

-- What does this script do?
-- Allows you to have *most* Emacs keybindings in other apps.
-- Ctrl-Space space be used to mark and cut text just like Emacs. Also enables Emacs prefix keys such as Ctrl-xs (save).

-- Installation
-- 1) Download and hammerspoon http://www.hammerspoon.org/
-- 2) Copy emacs_hammerspoon.lua to ~/.hammerspoon/init.lua (or import emacs_hammerspoon.lua into your exisiting inti.lua)
-- 3) Start Hammerspoon

-- Customization
-- To customize the keybindings modfiy the global "keys" below.

-- Namespaces
-- This script uses namespaces to send different commands to different apps.
-- "globalOverride" contains keybindings that override all other apps (including Emacs)
-- "globalEmacs" brings Emacs style keybindings to all other apps
-- "Google Chrome" (and other app names) creates app specific keybindings that take precendence over "globalEmacs"

-- Syntax Example
-- ['globalEmacs'] = { ["ctrl"] = { ['f'] = {nil, 'right', true, nil} } }
-- This keybinding states, for all apps other than Emacs map Ctrl+f to the right arrow key. true maintains the current mark (if set).

-- ['globalEmacs'] = { ["ctrl"] = { ['x'] = {nil, nil, false, 'macroStartCtrlX'} } }
-- This keybinding states, for all apps other than Emacs map Ctrl+x to a macro called "macroStartCtrlX" and unset any previous mark.

local keys = {
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
      ['/'] = {'cmd', 'z', false, nil},
      ['space'] = {nil, nil, true, 'macroCtrlSpace'},
      ['delete'] = {nil, nil, false, 'macroBackwardsKillWord'},      
    },
    ['ctrlXPrefix'] = {
      ['c'] = {'cmd', 'q', false, nil},      
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
      ['delete'] = {'cmd', 'z', false, nil},                  
    },
    ['altShift'] = {
      ['.'] = {'cmd', 'down', true, nil},
      [','] = {'cmd', 'up', true, nil},      
    },
  },
  ['Google Chrome'] = {
    ['ctrlXPrefix'] = {
      ['b'] = {'cmd', 'b', false, nil},
      ['d'] = {{'cmd', 'alt'}, 'j', false, nil}, 
      ['f'] = {'cmd', 'l', false, nil},
      ['k'] = {'cmd', 'w', false, nil},      
    },
    ['alt'] = {
      ['n'] = {'cmd', 't', false, nil},            
    }
  },
  ['globalOverride'] = {
    ['ctrl'] = {
      ['x'] = {nil, nil, false, 'macroStartCtrlX'},
    },
    ['ctrlXPrefix'] = {
      ['t'] = {nil, nil, false, 'macroAltTab'},
      ['j'] = {'cmd', 'space', false, nil},
      [']'] = {{'ctrl', 'cmd'}, '2', false, nil},
      ['['] = {{'ctrl', 'cmd'}, '1', false, nil},            
    },
    ['alt'] = {
      ['s'] = {{'shift', 'ctrl', 'cmd'}, '5', false, nil},
    }
  }  
}

local appsWithNativeEmacsKeybindings = {
  'emacs',
  'rubymine',
  'terminal'
}

local ctrlXActive = false
local ctrlSpaceActive = false
local currentApp = nil
local modal = hs.hotkey.modal.new()

--- Entry point for processing keystrokes and taking the appropriate action.
-- @param mods String modifiers such as: ctrl or alt
-- @param key String keys such as: a, b, c, etc
function processKeystrokes(originalMods, originalKey)
  return function()
    -- My lack of understand of lua and closures probably requires this assignment.
    mods = originalMods
    key = originalKey
    
    if ctrlXActive and mods == 'ctrl' then
      mods = 'ctrlXPrefix'
    end

    namespace = currentNamespace(mods, key)

    if isEmacs() and namespace ~= 'globalOverride' then
      print('Passingthrough to Emacs mods: ' .. mods .. ' keys: ' .. key)
      tapKey(mods, key)    
      return
    end

    if keybindingExists(namespace, mods, key) then
      lookupAndTranslate(namespace, mods, key)      
    else
      print('Passingthrough mods: ' .. mods .. ' keys: ' .. key)
      tapKey(mods, key)
    end
  end
end

--- Looks up a keybinding in the global keybindings table and translates that keybinding or runs a macro.
-- @param namespace String the namespace to lookup a key (eg Google Chrome or GlobalEmacs)
-- @param mods String modifiers key such as ctrl or alt.
-- @param key String keys such as: a, b or c
function lookupAndTranslate(namespace, mods, key)
  config = keys[namespace][mods][key]
  toMods = config[1]
  toKey = config[2]
  ctrlSpaceSensitive = config[3]
  toMacro = config[4]

  if toMacro ~= nil then
    print('Executing macro: ' .. toMacro)              
    _G[toMacro]()
  else
    toMods = addShift(toMods, ctrlSpaceSensitive)
    
    tapKey(toMods, toKey)

    logTranslation(mods, key, toMods, toKey)
  end

  if not ctrlSpaceSensitive then
    ctrlSpaceActive = false
  end

  if toMacro ~= 'macroStartCtrlX' then
    ctrlXActive = false
  end  
end

--- Logs a keybinding translation to the Hammerspoon console
-- @param fromMods String original modifiers
-- @param fromKey String original key
-- @param toMods String or Table destination modifiers
-- @param toKey String destionation key
function logTranslation(fromMods, fromKey, toMods, toKey)
  message = 'Translating: ' .. fromMods .. '+' .. fromKey .. ' to '

  if type(toMods) == 'string' then
    message = message .. toMods
  elseif type(toMods) == 'table' then
    for index, mods in pairs(toMods) do
      message = message .. mods .. '+'
    end
  end
  
  print(message .. '+' .. (toKey or ''))
end

--- Checks the global keys table for a keybinding
-- @param namespace String namespace of the keybinding (eg globalEmacs, Google Chrome, etc)
-- @param mods String modifier keys such as alt, ctrl, ctrlXPrefix, etc
-- @param key String key such as a, b, c, etc
function keybindingExists(namespace, mods, key)
  return (
    keys[namespace] ~= nil and
    keys[namespace][mods] ~= nil and
    keys[namespace][mods][key] ~= nil)
end



--- Checks the global keys table for a keybinding
-- @param namespace String namespace of the keybinding (eg globalEmacs, Google Chrome, etc)
-- @param mods String modifier keys such as alt, ctrl, ctrlXPrefix, etc
-- @param key String key such as a, b, c, etc
function currentNamespace(mods, key)
  if keybindingExists('globalOverride', mods, key) then
    return 'globalOverride'   
  elseif currentApp ~= nil and keybindingExists(currentApp, mods, key) then
    return currentApp      
  end

  return 'globalEmacs'
end

--- Presses and releases a keystroke with modifiers (faster than hs.eventtap.keystroke)
-- @param mods String modifier keys such as alt, ctrl, shift
-- @param key String key such as a, b, c, etc
function tapKey(mods, key)
  modal:exit()
  
  hs.eventtap.event.newKeyEvent(mods, key, true):post()
  hs.eventtap.event.newKeyEvent(mods, key, false):post()

  modal:enter()
end

function holdKey(mods, key)
  modal:exit()
  
  hs.eventtap.event.newKeyEvent(mods, key, true):post()

  modal:enter()
end

function releaseKey(mods, key)
  modal:exit()
  
  hs.eventtap.event.newKeyEvent(mods, key, false):post()

  modal:enter()
end

--- Appends a shift modifier key (if needed) to the existing mods.
-- @param mods String modifier keys such as alt, ctrl, ctrlXPrefix, etc
-- @param ctrlSpaceSensitive Boolean current keybinding is amenable to holding shift
function addShift(mods, ctrlSpaceSensitive)
  if ctrlSpaceSensitive and ctrlSpaceActive then
    if mods == nil then
      return 'shift'
    elseif type(mods) == 'string' then
      return {'shift', mods}
    elseif type(mods) == 'table' then
      table.insert(mods, 1, 'shift')
      return mods
    end
  end
 
  return mods
end

--- Does the current app already have Emacs keybinings
-- @return Boolean true if Emacs
function isEmacs()
  for index, value in ipairs(appsWithNativeEmacsKeybindings) do
    if value:lower() == currentApp:lower() then
      return true
    end
  end

  return false
end

--- Currently running application on Hammerspoon start-up
function appOnStartup()
  app = hs.application.frontmostApplication()

  if app ~= nil then
    return app:title()
  end
end

-- Updates the global currentApp when the user switches applications
function appWatcherFunction(appName, eventType, appObject)
  if (eventType == hs.application.watcher.activated) then
    currentApp = appName
  end
end

--- Assign all keybindings based on the entries in the global keys
function assignKeys()
  for namespace, modTable in pairs(keys) do
    for mod, keyTable in pairs(modTable) do
      for key, keyConfig in pairs(keyTable) do
        assignKey(mod, key)
      end
    end
  end  
end

--- Assigns a single keystroke to the global modal, which allows this script to intercept keys
-- @param mod String modifier key such as alt, ctrl, shift
-- @param key String key such as a, b, c, etc
function assignKey(mod, key)
  if mod == 'altShift' then
    modal:bind({'alt', 'shift'}, key, processKeystrokes('altShift', key), nil, nil)
  elseif mod:match('alt') then
    modal:bind('alt', key, processKeystrokes('alt', key), nil, nil)
  elseif mod:match('ctrl') then
    modal:bind('ctrl', key, processKeystrokes('ctrl', key), nil, nil)
  end  
end

-- Launches the Hammerspoon alt-tab alternative. Include minimized/hidden windows (sorta works).
function macroAltTab()
  switcher_space = hs.window.switcher.new(hs.window.filter.new():setCurrentSpace(true):setDefaultFilter{})
  switcher_space.nextWindow()

  window = hs.window.frontmostWindow()
  window:focus()
end

-- Start a selection mark in a non Emacs app
function macroCtrlSpace()
  ctrlSpaceActive = not ctrlSpaceActive
  
  tapKey({}, 'shift')
end

-- Activate the Ctrl-x prefix key
function macroStartCtrlX()
  ctrlXActive = true

  hs.timer.doAfter(1, function() print 'End CtrlX'; ctrlXActive = false end)

  if isEmacs() then
    print('Passingthrough C-x to Emacs')
    tapKey({'ctrl'}, 'x')
  end
end

-- Kill a word behind the cursor and put it in the clipboard
function macroBackwardsKillWord()
  tapKey({'shift', 'alt'}, 'left')
  tapKey('cmd', 'x')
end

print('---------------------------------')
print('Starting Emacs Hammerspoon Script')

assignKeys()

currentApp = appOnStartup()

local appWatcher = hs.application.watcher.new(appWatcherFunction)
appWatcher:start()

modal:enter()  
