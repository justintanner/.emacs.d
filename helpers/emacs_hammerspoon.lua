local currentApp = nil
function applicationWatcher(appName, eventType, appObject)
  if (eventType == hs.application.watcher.activated) then
    currentApp = appName
  end
end


local appWatcher = hs.application.watcher.new(applicationWatcher)
appWatcher:start()

-- altTab = function() hs.eventtap.keyStroke('cmd', 'tab') end
-- hs.hotkey.bind('ctrl', 't', 'ctrl+t', altTab) 

-- local markActive = hs.hotkey.modal.new('ctrl', 'space')

-- function markActive:entered()
--   hs.alert 'Entered mode'
-- end

-- function markActive:exited()
--   hs.alert 'Exited mode'
-- end

-- markActive:bind('', 'escape', function() k:exit() end)
-- markActive:bind('', 'J', 'Pressed J', function() print 'let the record show that J was pressed' end)


function altTab()
  hs.eventtap.keyStroke('cmd', 'tab')
end

function bindHotkey(mod, key, functionName)
  if (currentApp == 'Emacs') then
    print 'skipping ctrl+t'
  else
    print 'using binding for ctrl+t'
    -- hs.hotkey.bind(mod, key, '', _G[functionName])
    _G[functionName]()
  end
end

hs.hotkey.bind('ctrl', 't', '', bindHotkey('ctrl', 't', 'altTab'))




