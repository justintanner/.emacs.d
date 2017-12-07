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
      ['k'] = {nil, nil, true, 'macroKillLine'},
      ['n'] = {nil, 'down', true, nil},
      ['o'] = {nil, 'enter', false, nil},
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
      ['s'] = {'cmd', 's', false, nil},
      ['u'] = {'cmd', 'z', false, nil},
    },
    ['alt'] = {
      ['f'] = {'alt', 'f', true, nil},
      ['n'] = {'cmd', 'n', false, nil},
      ['v'] = {nil, 'pageup', true, nil},
    },
    ['altShift'] = {
      ['.'] = {nil, 'end', false, nil},
      [','] = {nil, 'home', false, nil},      
    },
  },
  ['Google Chrome'] = {
    ['ctrl'] = {},
    ['ctrlXPrefix'] = {
      ['b'] = {'cmd', 'b', false, nil},
    }
  }
}

local ctrlXActive = false
local ctrlSpaceActive = false
local currentApp = nil
local emacsMap = hs.hotkey.modal.new()
local overrideMap = hs.hotkey.modal.new()

function processKey(mod, key)
  return function()
    emacsMap:exit()

    alteredMod = mod
    if ctrlXActive and mod == 'ctrl' then
      alteredMod = 'ctrlXPrefix'
    end

    namespace = 'globalEmacs'
    
    if keybindingExists('globalOverride', alteredMod, key) then
      namespace = 'globalOverride'   
    elseif currentApp ~= nil and keybindingExists(currentApp, alteredMod, key) then
      namespace = currentApp      
    end

    if keybindingExists(namespace, alteredMod, key) then
      changeKey(namespace, alteredMod, key)
    else
      tapKey(mod, key)
    end

    emacsMap:enter()
  end
end

function tapKey(mod, key)
  -- Faster than hs.eventtap.keystroke
  hs.eventtap.event.newKeyEvent(mod, key, true):post()
  hs.eventtap.event.newKeyEvent(mod, key, false):post()
end

function changeKey(namespace, mod, key)
  config = keys[namespace][mod][key]
  newMod = config[1]
  newKey = config[2]
  ctrlSpaceSensitive = config[3]
  macro = config[4]

  if macro ~= nil then
    _G[macro]()
    print('Executing a macro ' .. macro .. ' for ' .. (mod or '') .. '+' .. (key or ''))              
  else
    holdShift = (ctrlSpaceSensitive and ctrlSpaceActive)
    if holdShift then print 'Holding shift' end
    
    tapKey(prepModifier(newMod, holdShift), newKey)
    
    message = 'Changing ' .. mod .. '+' .. key .. ' to '
    if newMod ~= nil then
      message = message .. (newMod or '') .. '+'
    end
    message = message .. (newKey or '')

    print(message)
  end

  if not ctrlSpaceSensitive then
    ctrlSpaceActive = false
  end

  if not macro == 'macroStartCtrlX' then
    ctrlXActive = false
  end
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

function isEmacs()
  return (currentApp == 'Emacs')
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

function macroAltTab()
  hs.eventtap.event.newKeyEvent({'cmd'}, 'tab', true):post()
  tapKey({}, 'left')
  hs.eventtap.event.newKeyEvent({'cmd'}, 'tab', false):post()
end

function macroKillLine()
  tapKey({'shift', 'ctrl'}, 'e')
  tapKey({}, 'shift')
  tapKey({'cmd'}, 'x')
  ctrlSpaceActive = false
end

function macroCtrlSpace()
  ctrlSpaceActive = not ctrlSpaceActive
  
  tapKey({}, 'shift')
end

function macroStartCtrlX()
  ctrlXActive = true

  hs.timer.doAfter(0.75,function() ctrlXActive = false end)
end

function chooseKeyMap()
  if currentApp == 'Emacs' then
    print('Turning off keybindings for Emacs')
    emacsMap:exit()      
    overrideMap:enter()      
  else
    print('Turning on keybindings for ' .. currentApp)
    overrideMap:exit()      
    emacsMap:enter()      
  end
end

function appOnStartup()
  app = hs.application.frontmostApplication()

  if app ~= nil then
    return app:title()
  end

  return ''
end

function appWatcherFunction(appName, eventType, appObject)
  if (eventType == hs.application.watcher.activated) then
    currentApp = appName
    
    chooseKeyMap()    
  end
end

-- Application start
print('Starting emacs_hammerspoon')
assignKeys()

currentApp = appOnStartup()

chooseKeyMap()

local appWatcher = hs.application.watcher.new(appWatcherFunction)
appWatcher:start()
