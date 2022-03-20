module Utils where

import XMonad (spawn, MonadIO)


notifySend :: MonadIO m => String -> String -> m ()
notifySend header body = spawn $ notify ++ header ++ "' '" ++ body ++ "'"
  where notify = "notify-send -a xmonad '"
