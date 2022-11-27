module MusicMode where

import qualified XMonad.Actions.Submap as AS
import qualified Data.Map as M

import qualified XMonad as XM

import Utils (notifySend)

musicMode :: XM.X ()
musicMode = do
  notifySend " -- MUSIC -- " ""
  AS.submap . M.fromList $
    [ ((0, XM.xK_l), continue_spawn "~/.dotfiles/bin/,music forward")
    , ((0, XM.xK_h), continue_spawn "~/.dotfiles/bin/,music rewind")
    , ((0, XM.xK_w), continue_spawn "~/.dotfiles/bin/,music next")
    , ((0, XM.xK_f), continue_spawn "~/.dotfiles/bin/,music next")

    , ((0, XM.xK_j), continue_spawn "~/.dotfiles/bin/,music lower_volume")
    , ((0, XM.xK_k), continue_spawn "~/.dotfiles/bin/,music raise_volume")

    , ((0, XM.xK_b), continue_spawn "~/.dotfiles/bin/,music prev")
    , ((0, XM.xK_0), continue_spawn "~/.dotfiles/bin/,music prev")

    , ((0, XM.xK_s), continue_spawn "~/.dotfiles/bin/,music toggle_random")
    , ((0, XM.xK_m), continue_spawn "~/.dotfiles/bin/,music toggle_mute")
    , ((0, XM.xK_r), continue_spawn "~/.dotfiles/bin/,music toggle_repeat")

    , ((0, XM.xK_p), continue_spawn "~/.dotfiles/bin/,music pause")
    , ((0, XM.xK_g), continue_spawn "~/.dotfiles/bin/,music get_current_info")

    , ((0, XM.xK_Escape), notifySend " -- NORMAL -- " "")
    ]
    where continue_spawn cmd = do { XM.spawn cmd ; musicMode }


