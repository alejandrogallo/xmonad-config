module Layout where

import           XMonad

import           XMonad.Layout.CenteredMaster
import           XMonad.Layout.Circle
import           XMonad.Layout.Cross
import           XMonad.Layout.Dwindle
import           XMonad.Layout.Grid
import           XMonad.Layout.Maximize
import           XMonad.Layout.TwoPane
import           XMonad.Layout.Mosaic
import           XMonad.Layout.ToggleLayouts

myLayout = maximize $
           Tall 1 (3/100) (1/2) |||
           Mirror (Tall 1 (3/100) (1/2)) |||
           Full |||
           TwoPane (1/2) (1/2) |||
           centerMaster (Tall 1 (3/100) (1/2)) |||
           simpleCross |||
           Circle |||
           Dwindle R CW 1.5 1.1 |||
           Grid
