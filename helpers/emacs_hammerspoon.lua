local keys = {
  ['globalOverride'] = {
    ['ctrl'] = {
      ['t'] = {'cmd', 'tab', false, nil}, -- sorta working
      ['j'] = {'cmd', 'space', false, nil}
    }
  },
  ['globalEmacs'] = {
    ['ctrl'] = {
      ['s'] = {'cmd', 'f', false, nil},
      ['n'] = {nil, 'down', true, nil},
      ['x'] = {nil, nil, false, 'macroStartCtrlX'},     
      ['space'] = {nil, nil, false, 'macroStartCtrlSpace'},
    }
  }
  ['xPrefix'] = {
    ['ctrl'] = {
      ['b'] = { } -- Finish me!
    }
  }
}

local ctrlXActive = false
local ctrlSpaceActive = false
local currentApp = nil
local map = hs.hotkey.modal.new()

map:bind('ctrl', 'space', nil, function() processKey('ctrl', 'space') end)
map:bind('ctrl', 'j', nil, function() processKey('ctrl', 'j') end)
map:bind('ctrl', 'n', nil, function() processKey('ctrl', 'n') end)
map:bind('ctrl', 't', nil, function() processKey('ctrl', 't') end)
map:bind('ctrl', 's', nil, function() processKey('ctrl', 's') end)
map:bind('ctrl', 'x', nil, function() processKey('ctrl', 'x') end)

map:enter()

function macroStartMarkSpace()
  hs.eventtap.keyStroke({}, 'shift')  
  ctrlSpaceActive = true
end

function marcroStartCtrlX()
  ctrlXActive = true

  hs.timer.doAfter(0.75, function() ctrlXActive = false end)
end

function processKey(mod, key)
  map:exit()

  if keys['globalOverride'][mod][key] ~= nil then
    config = keys['globalOverride'][mod][key]
    changeKey(mod, key, config[1], config[2], config[3], config[4])    
  elseif (isEmacs()) then
    print('Letting ' .. mod .. '+' .. key .. ' passthrough to Emacs')
    hs.eventtap.keyStroke(mod, key)
  elseif currentApp ~= nil and keys[currentApp] ~= nil and keys[currentApp][mod][key] ~= nil then
    config = keys[currentApp][mod][key]
    changeKey(mod, key, config[1], config[2], config[3], config[4])
  elseif keys['globalEmacs'][mod][key] ~= nil then
    config = keys['globalEmacs'][mod][key]
    changeKey(mod, key, config[1], config[2], config[3], config[4])
  end
  
  map:enter()
end

function changeKey(mod, key, newMod, newKey, ctrlSpaceSensitive, macro)
  if macro ~= nil then
    _G[macro]()
    print('Executing a macro ' .. macro .. ' for ' .. (mod or '') .. '+' .. (key or ''))              
  else
    holdShift = (ctrlSpaceSensitive and ctrlSpaceActive)
    if holdShift then print 'Holding shift' end
    hs.eventtap.keyStroke(prepModifier(newMod, holdShift), newKey)
    
    print('Changing ' .. mod .. '+' .. key .. ' to ' .. (newMod or '') .. '+' .. (newKey or ''))
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

function isEmacs()
  return (currentApp == 'Emacs')
end

function applicationWatcher(appName, eventType, appObject)
  if (eventType == hs.application.watcher.activated) then
    currentApp = appName
    print(currentApp)
  end
end

local appWatcher = hs.application.watcher.new(applicationWatcher)
appWatcher:start()

function dump(o)
  if type(o) == 'table' then
    local s = '{ '
    for k,v in pairs(o) do
      if type(k) ~= 'number' then k = '"'..k..'"' end
      s = s .. '['..k..'] = ' .. dump(v) .. ','
    end
    return s .. '} '
  else
    return tostring(o)
  end
end


