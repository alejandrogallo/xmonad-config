module ScreenshotMode where

import qualified XMonad.Actions.Submap as AS
import qualified Data.Map as M

import qualified XMonad as XM

import Utils (notifySend)


screenshotMode :: XM.X ()
screenshotMode = do
  notifySend " -- SCREENSHOT -- " ""
  AS.submap . M.fromList $
    [ ((0, XM.xK_r), continue_spawn "~/.dotfiles/bin/screenshot.sh root")
    , ((0, XM.xK_w), continue_spawn "~/.dotfiles/bin/screenshot.sh active_window")
    , ((0, XM.xK_y), continue_spawn "~/.dotfiles/bin/screenshot.sh yank_last")
    , ((0, XM.xK_p), continue_spawn "~/.dotfiles/bin/screenshot.sh yank_last_path")
    , ((0, XM.xK_s), continue_spawn "~/.dotfiles/bin/screenshot.sh select")
    ]
    where continue_spawn cmd = do { XM.spawn cmd ; screenshotMode }
