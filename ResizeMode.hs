module ResizeMode where

import qualified XMonad.Actions.Submap as AS
import qualified Data.Map as M
import qualified XMonad.StackSet as W

import qualified XMonad as XM

import Utils (notifySend)

resizeMode :: XM.X ()
resizeMode = do
  notifySend " -- RESIZE -- " ""
  AS.submap . M.fromList $
    [ ((0, XM.xK_h), continue $ XM.sendMessage XM.Shrink)
    , ((0, XM.xK_l), continue $ XM.sendMessage XM.Expand)
    , ((0, XM.xK_t), continue $ XM.withFocused $ XM.windows . W.sink)
    , ((0, XM.xK_Escape), notifySend " -- NORMAL -- " "")
    -- TODO: it does not yet work
    , ((0, XM.xK_j), continue $ XM.windows W.swapDown)
    , ((0, XM.xK_k), continue $ XM.windows W.swapDown)
    ]
    where continue cmd = do { cmd ; resizeMode }


