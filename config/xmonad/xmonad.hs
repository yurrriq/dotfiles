{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main (main) where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import XMonad
import XMonad.Actions.CycleWS (WSType (Not), doTo, emptyWS, moveTo)
import XMonad.Actions.Navigation2D (navigation2DP, windowGo, windowSwap)
import XMonad.Actions.RotSlaves (rotSlavesDown, rotSlavesUp)
import XMonad.Actions.Warp (warpToWindow)
import XMonad.Hooks.DynamicLog (PP (..), shorten, statusBar, wrap, xmobarColor)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks
  ( AvoidStruts,
    Direction1D (..),
    Direction2D,
    avoidStruts,
    docks,
  )
import XMonad.Hooks.StatusBar (defToggleStrutsKey)
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.Decoration (ModifiedLayout)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.TwoPane (TwoPane (TwoPane))
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.NamedScratchpad
  ( NamedScratchpad (NS),
    customFloating,
    namedScratchpadAction,
    scratchpadWorkspaceTag,
  )
import XMonad.Util.Scratchpad (scratchpadManageHook)
import XMonad.Util.WorkspaceCompare (filterOutWs, getSortByIndex)

main :: IO ()
main = xmonad =<< myXmobar myConfig

myXmobar :: LayoutClass l Window => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
myXmobar = statusBar "xmobar" myPP defToggleStrutsKey

myConfig =
  navigation2DP
    def
    ("<U>", "<L>", "<D>", "<R>")
    [("M-", bringMouse windowGo), ("M-S-", windowSwap)]
    False
    . docks
    $ ewmh
      def
        { borderWidth = 0,
          focusFollowsMouse = True,
          keys = myKeys,
          layoutHook = myLayout,
          -- TODO: namedScratchpadManageHook scratchpads,
          manageHook = manageScratchPad <+> myManageHook,
          modMask = mod4Mask,
          terminal = "kitty",
          workspaces = myWorkspaces
        }

bringMouse :: (Direction2D -> Bool -> X ()) -> (Direction2D -> Bool -> X ())
bringMouse windowAction dir rap = windowAction dir rap >> warpToWindow (1 / 2) (1 / 2)

myLayout =
  avoidStruts $
    tall
      ||| twopane
      ||| Mirror tall
      -- TODO: ||| Mirror twopane
      ||| noBorders Full
      ||| emptyBSP
  where
    tall = Tall 1 delta ratio
    twopane = TwoPane delta ratio
    delta = 3 / 100
    ratio = 1 / 2

myPP :: PP
myPP =
  def
    { ppCurrent = xmobarColor "#656565" "" . wrap "(" ")",
      ppLayout = \case
        "Tall" -> "<fn=1>🗼</fn>"
        "TwoPane" -> "<fn=1>👬</fn>"
        "Mirror Tall" -> "<fn=1>💁</fn>"
        -- TODO: "Mirror TwoPane" -> "Mirror TwoPane"
        "Full" -> "<fn=1>🖕</fn>"
        "BSP" -> "<fn=1>🐚</fn>"
        layout -> layout,
      ppSep = " ",
      ppSort = (. filterOutWs [scratchpadWorkspaceTag]) <$> ppSort def,
      ppTitle = xmobarColor "#fdf6e3" "" . shorten 80,
      ppUrgent = xmobarColor "#ff605a" ""
    }

myManageHook :: ManageHook
myManageHook =
  composeAll $
    concatMap
      (\(n, klasses) -> [className =? klass --> doShift (nthWorkspace n) | klass <- klasses])
      [ (1, ["Firefox"]),
        (5, ["Slack"]),
        (6, ["zoom"]),
        (8, ["Signal"]),
        (9, ["Clementine", "Spotify", "spotify"])
      ]

manageScratchPad :: ManageHook
manageScratchPad =
  scratchpadManageHook (W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS
      "emacs"
      "emacsclient -a '' -nc -F '((name . \"emacs-scratch\"))'"
      (title =? "emacs-scratch")
      (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)),
    NS
      "kitty"
      "kitty --name=scratchpad"
      (resource =? "scratchpad")
      (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
  ]

myKeys :: XConfig l -> Map (KeyMask, KeySym) (X ())
myKeys cfg =
  mkKeymap cfg $
    [ ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"),
      ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"),
      ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"),
      ("<XF86AudioPrev>", spawn "playerctl previous"),
      ("<XF86AudioPlay>", spawn "playerctl play-pause"),
      ("<XF86AudioNext>", spawn "playerctl next"),
      -- FIXME: ("<XF86MonBrightnessDown>", spawn "xbacklight -10"),
      -- FIXME: ("<XF86MonBrightnessUp>", spawn "xbacklight +10"),
      ("<Print>", spawn "flameshot gui"),
      ("M-S--", namedScratchpadAction scratchpads "emacs"),
      ("M--", namedScratchpadAction scratchpads "kitty"),
      ("M-<Esc>", asks (cfgDir . directories) >>= spawn . wrap "i3lock --raw 3840x2400:rgb --image " "/skyrim.raw"),
      ("M-<Space>", spawn "dunstctl close"),
      ("M-M1-<Space>", spawn "dunstctl close-all"),
      ("M-M1-b", spawn "rofi-bluetooth"),
      ("C-`", spawn "dunstctl history-pop"),
      ("M-<Return>", spawn (terminal cfg)),
      ("M-<Tab>", spawn "rofi -show window"),
      ("M-S-<Space>", toggleFloat),
      ("M-S-b", sendMessage (JumpToLayout "BSP")),
      ("M-S-c", spawn "rofi -modi calc -show calc"),
      ("M-S-e", spawn "emacsclient -nc -e '(switch-to-buffer nil)'"),
      ("M-S-l", sendMessage NextLayout),
      ("M-S-p", spawn "rofi-pass"),
      ("M-S-q", kill),
      ("M-S-r", spawn "rofi-rbw"),
      ("M-S-s", spawn "rofi-systemd"),
      ("M-S-t", spawn "rofi -modi top -show top"),
      ("M-e", sendMessage (JumpToLayout "Tall")),
      ("M-f", sendMessage (JumpToLayout "Full")),
      ("M-h", sendMessage (JumpToLayout "Mirror Tall")),
      ("M-j", rotSlavesDown),
      ("M-k", rotSlavesUp),
      ("M-m", windows W.focusMaster),
      ("M-w", sendMessage (JumpToLayout "TwoPane")),
      ("M1-<Space>", spawn "rofi -modi combi,window -show combi -combi-modi run,drun"),
      ("C-M-M1-x", spawn "xmonad --restart"),
      ("C-M-M1-c", spawn "systemctl --user restart picom.service"),
      ("C-M-<Left>", moveTo Prev (Not emptyWS)),
      ("C-M-<Right>", moveTo Next (Not emptyWS)),
      ("C-M-M1-<Left>", shiftAndMoveTo Prev emptyWS),
      ("C-M-M1-<Right>", shiftAndMoveTo Next emptyWS),
      ("C-M-M1-f", shiftAndMoveTo Next emptyWS)
    ]
      ++ [ ( intercalate "-" (catMaybes [Just "M", maybeShift, Just key]),
             windows (f workspace)
           )
           | (workspace, key) <-
               zip (workspaces cfg) (show <$> reverse (0 : [9, 8 .. 1 :: Int])),
             (f, maybeShift) <-
               [(W.greedyView, Nothing), (W.shift, Just "S")]
         ]

shiftAndMoveTo :: Direction1D -> WSType -> X ()
shiftAndMoveTo dir t = doTo dir t getSortByIndex shiftAndMove

shiftAndMove :: WorkspaceId -> X ()
shiftAndMove n = windows (W.shift n) *> windows (W.greedyView n)

toggleFloat :: X ()
toggleFloat = withFocused $ \this ->
  do
    isFloating <- gets (M.member this . W.floating . windowset)
    if isFloating
      then withFocused $ windows . W.sink
      else float this

nthWorkspace :: Int -> String
nthWorkspace = (myWorkspaces !!) . subtract 1

myWorkspaces :: [String]
myWorkspaces =
  [ "<fn=1>🌐</fn>",
    "<fn=1>💻</fn>",
    "<fn=1>🤖</fn>",
    "<fn=1>🏌</fn>",
    "<fn=1>📟</fn>",
    "<fn=1>🏎</fn>",
    "<fn=1>☢</fn>",
    "<fn=1>🗨</fn>",
    "<fn=1>🎶</fn>"
  ]
