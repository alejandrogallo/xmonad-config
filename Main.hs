import           XMonad
import           XMonad.Prompt.Workspace
import qualified XMonad.StackSet as W
import           XMonad.Util.Dmenu (dmenu, menuArgs, menuMapArgs)
import           XMonad.Util.Run (spawnPipe)

import qualified XMonad.Actions.GroupNavigation as AGR
import qualified XMonad.Actions.Commands as AC
import qualified XMonad.Actions.CycleWindows as ACW
import qualified XMonad.Actions.ShowText as ST
import qualified XMonad.Actions.WorkspaceNames as AWN
import qualified XMonad.Actions.CycleWS as ACW
import qualified XMonad.Prompt as Prompt
import qualified XMonad.Prompt.Window as APW
import           XMonad.Actions.FindEmptyWorkspace (viewEmptyWorkspace)
import           XMonad.Actions.UpdatePointer (updatePointer)
import           XMonad.Actions.WindowMenu (windowMenu)
import qualified XMonad.Util.EZConfig as EZ
import           Graphics.X11.Xlib.Misc (keysymToString)
import           XMonad.Actions.Minimize ( minimizeWindow
                                         , maximizeWindowAndFocus
                                         , withLastMinimized)


import           XMonad.Hooks.EwmhDesktops (ewmh)
import           XMonad.Hooks.DynamicLog ( dynamicLogWithPP, shorten
                                         , xmobarColor, xmobarPP, PP(..))
import           XMonad.Layout.Maximize (maximizeRestore)
import           XMonad.Layout.ToggleLayouts (ToggleLayout(Toggle))

import           Graphics.X11.ExtraTypes.XF86

import qualified Data.Map as M
import           Control.Monad ((>=>))
import           Data.String
import           System.IO (hPutStrLn)
import           Data.Char (ord, chr)
import           Data.Word (Word64)

import           Utils (notifySend)
import           MusicMode (musicMode)
import           ScreenshotMode (screenshotMode)
import           Layout (myLayout)

type KeyMap = M.Map (KeyMask, KeySym) (X ())

main :: IO ()
main = do
  notifySend "restart" ""
  putStrLn "XMONAD: starting xmobar"
  --xmobarProcess <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad $ ewmh (def
    { terminal = "xterm"
    , focusFollowsMouse = False
    , clickJustFocuses = True
    , workspaces = myWorkspaces
    , keys = myKeys
    , modMask = altkey
    -- STYLING
    , normalBorderColor  = "#AAAAAA"
    , focusedBorderColor = "#AA00FF"
    , borderWidth = 2
    -- HOOKS
    , layoutHook = myLayout
    , logHook = AGR.historyHook
              >> updatePointer (0.25, 0.25) (0.25, 0.25)
              >> AWN.workspaceNamesPP def
              >>= (\x -> pure ())

    }) `EZ.additionalMouseBindings` myMouseBindings

notifyWorkspace :: Word64 -> X ()
notifyWorkspace k = notifySend header ""
  where
    header = kk
    kk = toString k
    toString = (:[]) . chr . (fromIntegral :: Word64 -> Int)

winkey :: KeyMask
winkey = mod4Mask

altkey :: KeyMask
altkey = mod1Mask

charToKeySym :: Char -> KeySym
charToKeySym = fromIntegral . ord

myWorkspaces :: [String]
myWorkspaces = (:[]) <$> "asdfghjkl;"

data DmenuAction = Go | Shift
  deriving (Eq, Show)

dmenuActionWorkspace :: DmenuAction -> X ()
dmenuActionWorkspace d = do
  --XState { windowset = state } <- get
  --let a = W.allWindows state
  --ws <- menuArgs "dmenu" ["-p", "GoTo: "] $ Prelude.map show a
  --focus $ read ws
  let args = ["-l", "0", "-p", if d == Go then "Go To" else "Move To"]
  --ST.flashText ST.def 1 $ "Select to " ++ show d
  w <- menuArgs "dmenu" args myWorkspaces
  case d of
    Go -> windows $ W.greedyView w
    Shift -> windows $ W.shift w

dmenuActionWindow :: APW.WindowPrompt -> X ()
dmenuActionWindow p = do
  ws <- APW.allWindows
  w <- menuMapArgs "dmenu" ["-i"] ws
  case w of
    Just x -> focus x
    Nothing -> return ()

notifyLayout :: X ()
notifyLayout = do
  --ws <- windows id
  withFocused (\x -> notifySend (toString x) "asdf")
  --w <- W.workspace . W.current $ ws
  return ()
  where
    toString = (:[]) . chr . (fromIntegral :: Word64 -> Int)

withMask m = map (\(sym, action) -> ((m, sym), action))

myMouseBindings = let leftclick   = 1
                      middleclick = 2
                      rightclick  = 3
                      wheelup     = 4
                      wheeldown   = 5
                      wheelleft   = 6
                      wheelright  = 7
                      mouse8  = 8
                      mouse9  = 9
                   in
                   withMask winkey
                    -- Set the window to floating mode and move by dragging
                    [ (leftclick, \_ -> withFocused $ sendMessage . maximizeRestore)
                    -- Raise the window to the top of the stack
                    , (middleclick, \w -> focus w >> windows W.swapMaster)
                    -- Set the window to floating mode and resize by dragging
                    , (rightclick, \w -> focus w >> mouseResizeWindow w)
                    , (wheelup   , \_ -> windows W.focusUp)
                    , (wheeldown , \_ -> windows W.focusDown)
                    , (wheelleft , \_ -> sendMessage Shrink)
                    , (wheelright, \_ -> sendMessage Expand)
                    , (mouse8, \_ -> ACW.nextWS)
                    , (mouse9, \_ -> ACW.prevWS)
                    ]
                ++
                  withMask (winkey .|. shiftMask)
                    [ (middleclick, \_ -> withFocused $ windows . W.sink)
                    , (leftclick  , \w -> focus w >> mouseMoveWindow w)
                    , (rightclick, \w -> focus w >> mouseResizeWindow w)
                    , (wheelup , \_ -> sendMessage Shrink)
                    , (wheeldown, \_ -> sendMessage Expand)
                    ]
                ++
                  withMask (winkey .|. controlMask)
                    [ (wheelup   , \_ -> windows W.swapUp)
                    , (wheeldown , \_ -> windows W.swapDown)
                    , (wheelleft , \_ -> sendMessage $ IncMasterN 1)
                    , (wheelright, \_ -> sendMessage $ IncMasterN (-1))
                    ]


myKeys conf@XConfig {XMonad.modMask = meta}
  = M.union (emacsKeymap conf) $ M.fromList $
  [ ((winkey, xK_x), spawn "~/.dotfiles/bin/,dmenu-run")
  , ((winkey, xK_e), spawn "emacsclient -c")
  , ((meta, xK_Return), spawn $ XMonad.terminal conf)
  , ((winkey, xK_Return), spawn $ XMonad.terminal conf)
  -- this is for the very small sub-40 crkbd
  , ((meta, xK_slash), spawn $ XMonad.terminal conf)

  , ((0, xF86XK_AudioLowerVolume)
    , spawn "~/.dotfiles/bin/,music lower_volume")
  , ((0, xF86XK_AudioRaiseVolume)
    , spawn "~/.dotfiles/bin/,music raise_volume")
  , ((shiftMask, xF86XK_AudioLowerVolume)
    , spawn "~/.dotfiles/bin/,music forward")
  , ((shiftMask, xF86XK_AudioRaiseVolume)
    , spawn "~/.dotfiles/bin/,music rewind")

  , ((winkey, xK_o), AGR.nextMatch AGR.History (return True))
  , ((winkey .|. shiftMask, xK_o), viewEmptyWorkspace)

  -- Rotate through the available layout algorithms
  , ((winkey, xK_space ), sendMessage NextLayout >> notifyLayout)
  --  Reset the layouts on the current workspace to default
  , ((winkey .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

  , ((winkey .|. shiftMask, xK_q), withFocused killWindow)

  -- Move focus to the next window
  , ((meta,               xK_j     ), windows W.focusDown)
  , ((winkey .|. controlMask, xK_j     ), windows W.focusDown)
  -- Move focus to the previous window
  , ((meta,               xK_k     ), windows W.focusUp  )
  , ((winkey .|. controlMask, xK_k     ), windows W.focusUp  )
  -- Swap the focused window with the next window
  , ((winkey .|. shiftMask, xK_j     ), windows W.swapDown  )
  -- Swap the focused window with the previous window
  , ((winkey .|. shiftMask, xK_k     ), windows W.swapUp    )

  , ((meta .|. shiftMask, xK_semicolon), AC.defaultCommands >>= AC.runCommand)

  -- %! Move focus to the master window
  , ((winkey              , xK_m), windows W.focusMaster)
  -- Swap the focused window and the master window
  , ((winkey .|. shiftMask, xK_m), windows W.swapMaster)

  , ((winkey .|. controlMask, xK_f), withFocused (sendMessage . maximizeRestore))

  , ((winkey, xK_n), ACW.nextWS)
  , ((winkey, xK_p), ACW.prevWS)

  , ((winkey .|. controlMask, xK_r), AWN.renameWorkspace def)
  , ((meta, xK_z), ACW.rotOpposite)

  ] ++ [

  -- Shrink the master area
    ((meta,               xK_h     ), sendMessage Shrink)

  -- Expand the master area
  , ((meta,               xK_l     ), sendMessage Expand)

  -- Push window back into tiling
  , ((meta,               xK_t     ), withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area
  , ((winkey            , xK_comma ), sendMessage (IncMasterN 1))

  -- Deincrement the number of windows in the master area
  , ((winkey            , xK_period), sendMessage (IncMasterN (-1)))

  , ((meta              , xK_g), dmenuActionWorkspace Go)
  , ((meta .|. shiftMask, xK_g), dmenuActionWorkspace Shift)
  , ((winkey , xK_slash), dmenuActionWindow APW.Goto)
  ]

  ++
  --
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  --
  [((mod .|. winkey, k), windows (fun i) >> notifyWorkspace k)
      | (i, k) <- zip (XMonad.workspaces conf)
                      (charToKeySym <$> concat (XMonad.workspaces conf))
      , (fun, mod) <- [(W.view, 0), (W.shift, altkey)]]

  {--
  ++
  [((m .|. winkey, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_e, xK_w, xK_r] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
  --}



emacsKeymap :: XConfig l -> KeyMap
emacsKeymap conf = EZ.mkKeymap conf $

  [("M4-w M4-" ++ [k], windows $ W.shift i) | (i, k) <- zip wks (concat wks)]

  ++

  [ ( "M4-w " ++ keysymToString key
    , screenWorkspace sc >>= flip whenJust (windows . W.view)
    ) | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
  ]

  ++

  [ ( "M4-w M4-" ++ keysymToString key
    , screenWorkspace sc >>= flip whenJust (windows . W.shift)
    ) | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
  ]

  ++

  [ ( "M4-w M4-S-" ++ keysymToString key
    , (screenWorkspace sc >>= flip whenJust (windows . W.shift))
    >> (screenWorkspace sc >>= flip whenJust (windows . W.view))
    ) | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
  ]

  ++


  [ ("M4-c c", spawn "notify-send -a say EZ hello")
  , ("M4-c M4-s", screenshotMode)
  , ("M4-c M4-m", musicMode)
  , ("M4-c M4-semicolon", AC.defaultCommands >>= AC.runCommand)
  , ("M4-c M4-x M4-v", spawn "notify-send -a say EZ 'G-{c,x,v} yeah!'")
  , ("M4-<Tab>", sendMessage NextLayout >> notifyLayout)
  , ("M4-C-i", sendMessage NextLayout >> notifyLayout)
  , ("M4-S-<Tab>", sendMessage FirstLayout >> notifyLayout)
  , ("M4-S-C-i", sendMessage FirstLayout >> notifyLayout)
  -- window related
  , ("M4-S-f", withFocused $ sendMessage . maximizeRestore)
  , ("M4-w f", withFocused $ sendMessage . maximizeRestore)
  , ("M4-w n", viewEmptyWorkspace)
  , ("M4-w M4-c", withFocused killWindow)
  , ("M4-w t", withFocused $ windows . W.sink)
  , ("M4-w minus", withFocused minimizeWindow)
  , ("M4-w plus", withLastMinimized maximizeWindowAndFocus)

  , ("M4-u M4-x", spawn "env UNIVERSAL_ARGUMENT=1 ~/.dotfiles/bin/,dmenu-run")
  , ("M4-c M4-a a", spawn "~/.dotfiles/bin/,edit-arabic")
  , ("M4-c M4-a h", spawn "~/.dotfiles/bin/,edit-hebrew")
  ]
    where wks = XMonad.workspaces conf
