local keys = {
  ["globalOverride"] = {
    ["ctrl"] = {
      ["t"] = {'cmd', 'tab', false, nil}, -- sort working
      ["j"] = {'cmd', 'space', false, nil}
    }
  },
  ["globalEmacs"] = {
    ["ctrl"] = {
      ["s"] = {'cmd', 'f', false, nil}
    }
  }
}

local markActive = false
local currentApp = nil
local map = hs.hotkey.modal.new()

map:bind('ctrl', 'j', nil, function() processKey('ctrl', 'j') end)
map:bind('ctrl', 't', nil, function() processKey('ctrl', 't') end)
map:bind('ctrl', 's', nil, function() processKey('ctrl', 's') end)

map:enter()

function processKey(mod, key)
  map:exit()

  if keys["globalOverride"][mod][key] ~= nil then
    config = keys["globalOverride"][mod][key]
    changeKey(mod, key, config[1], config[2], config[3], config[4])    
  elseif (isEmacs()) then
    print("Letting " .. mod .. "+" .. key .. " passthrough to Emacs")
    hs.eventtap.keyStroke(mod, key)
  elseif currentApp ~= nil and keys[currentApp][mod][key] ~= nil then
    config = keys[currentApp][mod][key]
    changeKey(mod, key, config[1], config[2], config[3], config[4])
  elseif keys["globalEmacs"][mod][key] ~= nil then
    config = keys["globalEmacs"][mod][key]
    changeKey(mod, key, config[1], config[2], config[3], config[4])
  end
  
  map:enter()
end

function changeKey(mod, key, newMod, newKey, clearMark, macro)
  if (macro ~= nil) then
    print("Executing a macro " .. macro .. " for " .. mod .. "+" .. key)            
    _G[macro]()
  else
    print("Changing " .. mod .. "+" .. key .. " to " .. newMod .. "+" .. newKey)    
    hs.eventtap.keyStroke(newMod, newKey)
  end    
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

-- hs.timer.doAfter(0.5, function()
--   print 'timer is up!'                   
-- end)
