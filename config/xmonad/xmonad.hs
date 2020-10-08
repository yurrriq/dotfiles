{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main
  ( main,
  )
where

import Data.List (intercalate)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import XMonad hiding ((|||))
import XMonad.Actions.CopyWindow (runOrCopy)
import XMonad.Actions.Navigation2D
import XMonad.Actions.Warp
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.LayoutCombinators (JumpToLayout (..), (|||))
import XMonad.Layout.NoBorders
import XMonad.Layout.TwoPane
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Scratchpad

main :: IO ()
main = xmonad =<< statusBar "xmobar" myPP toggleStrutsKey myConfig

-- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/src/XMonad.Hooks.DynamicLog.html#toggleStrutsKey
toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig {modMask = modm} = (modm, xK_b)

myConfig =
  navigation2DP
    def
    ("<U>", "<L>", "<D>", "<R>")
    [("M-", bringMouse windowGo), ("M-S-", windowSwap)]
    False
    $ ewmh
      def
        { borderWidth = 0,
          focusFollowsMouse = True,
          -- focusedBorderColor = "#7BB6B3",
          handleEventHook = handleEventHook def <+> docksEventHook,
          keys = myKeys,
          layoutHook = myLayout,
          -- TODO: namedScratchpadManageHook scratchpads,
          manageHook = manageScratchPad <+> myManageHook,
          modMask = mod4Mask,
          -- normalBorderColor = "#967bb6",
          terminal = "kitty",
          workspaces = myWorkspaces
        }

bringMouse :: (Direction2D -> Bool -> X ()) -> (Direction2D -> Bool -> X ())
bringMouse windowAction dir wrap = windowAction dir wrap >> warpToWindow (1 / 2) (1 / 2)

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
    { ppCurrent = xmobarColor "#b58900" "" . wrap "(" ")",
      ppLayout = \case
        "Tall" -> "<fn=1>🗼</fn>"
        "TwoPane" -> "<fn=1>👬</fn>"
        "Mirror Tall" -> "<fn=1>💁</fn>"
        -- TODO: "Mirror TwoPane" -> "Mirror TwoPane"
        "Full" -> "<fn=1>🖕</fn>"
        "BSP" -> "<fn=1>🐚</fn>"
        layout -> layout,
      ppSep = " ",
      ppSort = (. namedScratchpadFilterOutWorkspace) <$> ppSort def,
      ppTitle = xmobarColor "#839496" "" . shorten 80,
      ppUrgent = xmobarColor "#fdf6e3" "#dc322f"
    }

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "Slack" --> doShift (myWorkspaces !! 4),
      className =? "zoom" --> doShift (myWorkspaces !! 5),
      className =? "Signal" --> doShift (myWorkspaces !! 7)
      -- FIXME: className =? "Spotify" --> doShift (myWorkspaces !! 8),
      -- FIXME: className =? "Mail" --> doShift (myWorkspaces !! 9)
    ]

manageScratchPad :: ManageHook
manageScratchPad =
  scratchpadManageHook (W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))

-- TODO
-- scratchpads :: [NamedScratchpad]
-- scratchpads =
--   [ NS
--       "kitty"
--       "kitty --title=Scratchpad"
--       (title =? "Scratchpad")
--       (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)),
--   ]

myKeys :: XConfig l -> Map (KeyMask, KeySym) (X ())
myKeys cfg =
  mkKeymap cfg $
    [ ("<Print>", spawn "flameshot gui"),
      ("<XF86AudioLowerVolume>", spawn "amixer sset Master 5%-"),
      ("<XF86AudioMute>", spawn "amixer sset Master toggle"),
      ("<XF86AudioPrev>", spawn "playerctl previous"),
      ("<XF86AudioRaiseVolume>", spawn "amixer sset Master 5%+"),
      ("<XF86MonBrightnessDown>", spawn "xbacklight -10"),
      ("<XF86MonBrightnessUp>", spawn "xbacklight +10"),
      -- TODO: ("M--", namedScratchpadAction scratchpads "kitty"),
      ("M--", scratchpadSpawnActionCustom "kitty --name=scratchpad"),
      ("M-<Esc>", getXMonadDataDir >>= spawn . wrap "i3lock -i " "/matrix.png"),
      ("M-<Return>", spawn (terminal cfg)),
      ("M-<Tab>", spawn "rofi -show window"),
      ("M-S-<Space>", withFocused (windows . W.sink)),
      ("M-S-b", sendMessage (JumpToLayout "BSP")),
      ("M-S-e", runOrCopy "emacsclient -c" (className =? "Emacs")),
      ("M-S-l", sendMessage NextLayout),
      ("M-S-p", spawn "rofi-pass"),
      ("M-S-q", kill),
      ("M-e", sendMessage (JumpToLayout "Tall")),
      ("M-f", sendMessage (JumpToLayout "Full")),
      ("M-h", sendMessage (JumpToLayout "Mirror Tall")),
      ("M-m", windows W.focusMaster),
      ("M-w", sendMessage (JumpToLayout "TwoPane")),
      ("M1-<Space>", spawn "rofi -modi combi,window -show combi -combi-modi run,drun"),
      ("XF86AudioNext>", spawn "playerctl next")
    ]
      ++ [ ( intercalate "-" (catMaybes [Just "M", maybeShift, Just key]),
             windows (f workspace)
           )
           | (workspace, key) <-
               zip (workspaces cfg) (show <$> reverse (0 : [9, 8 .. 1 :: Int])),
             (f, maybeShift) <-
               [(W.greedyView, Nothing), (W.shift, Just "S")]
         ]

myWorkspaces :: [String]
myWorkspaces =
  [ "<fn=1>🌐</fn>",
    "<fn=1>💻</fn>",
    "3",
    "4",
    "<fn=1>📟</fn>",
    "<fn=1>🏎</fn>",
    "7",
    "<fn=1>🗨</fn>",
    "<fn=1>🎶</fn>",
    "<fn=1>📬</fn>"
  ]
