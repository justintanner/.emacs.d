local keys = {
  ["global"] = {
    ["ctrl"] = {
      ["s"] = {'cmd', 'f', false, nil}
    }
  }
}

local markActive = false
local currentApp = nil
local map = hs.hotkey.modal.new()

map:bind('ctrl', 's', nil, function() processKey('ctrl', 's') end)

map:enter()

function processKey(mod, key)
  map:exit()
  
  if (isEmacs()) then
    print("Letting " .. mod .. "+" .. key .. " passthrough to Emacs")
    hs.eventtap.keyStroke(mod, key)
  elseif keys["global"][mod][key] ~= nil then
    config = keys["global"][mod][key]
    print(dump(config))

    if (config[4] ~= nil) then
      print("Executing a macro " .. config[4] .. " for " .. mod .. "+" .. key)            
      _G[config[4]]()
    else
      print("Changing " .. mod .. "+" .. key .. " to " .. config[1] .. "+" .. config[2])    
      hs.eventtap.keyStroke(config[1], config[2])      
    end  
  end
  
  map:enter()
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
