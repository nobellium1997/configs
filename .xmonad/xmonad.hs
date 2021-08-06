{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}

------------------------------------------------------------------------}}}
-- Modules                                                              {{{
---------------------------------------------------------------------------
--import Control.Monad (liftM2)             -- myManageHookShift
import Control.Monad (liftM, liftM2, join)  -- myManageHookShift
import Data.List
import qualified Data.Map as M
import Data.Monoid
import System.Exit
import System.IO                            -- for xmonbar
import System.Posix.Process(executeFile)

import XMonad hiding ( (|||) )              -- ||| from X.L.LayoutCombinators
import qualified XMonad.StackSet as W       -- myManageHookShift

import XMonad.Actions.Commands
import qualified XMonad.Actions.ConstrainedResize as Sqr
import XMonad.Actions.CopyWindow            -- like cylons, except x windows
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FloatSnap
import XMonad.Actions.MessageFeedback       -- pseudo conditional key bindings
import XMonad.Actions.Navigation2D
import XMonad.Actions.Promote               -- promote window to master
import XMonad.Actions.SinkAll
import XMonad.Actions.SpawnOn
--import XMonad.Actions.Volume
import XMonad.Actions.WindowGo
import XMonad.Actions.WithAll               -- action all the things

import XMonad.Hooks.DynamicLog              -- for xmobar
import XMonad.Hooks.DynamicProperty         -- 0.12 broken; works with github version
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks             -- avoid xmobar
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

--import XMonad.Layout hiding ( (|||) )       -- ||| from X.L.LayoutCombinators
import XMonad.Layout.Accordion
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.BorderResize
import XMonad.Layout.Column
import XMonad.Layout.Combo
import XMonad.Layout.ComboP
import XMonad.Layout.DecorationMadness      -- testing alternative accordion styles
import XMonad.Layout.Dishes
import XMonad.Layout.DragPane
import XMonad.Layout.Drawer
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.Hidden
import XMonad.Layout.LayoutBuilder
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutScreens
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.OneBig
import XMonad.Layout.PerScreen              -- Check screen width & adjust layouts
import XMonad.Layout.PerWorkspace           -- Configure layouts on a per-workspace
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile          -- Resizable Horizontal border
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing                -- this makes smart space around windows
import XMonad.Layout.StackTile
import XMonad.Layout.SubLayouts             -- Layouts inside windows. Excellent.
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts          -- Full window at any time
import XMonad.Layout.TrackFloating
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation

import XMonad.Prompt                        -- to get my old key bindings working
import XMonad.Prompt.ConfirmPrompt          -- don't just hard quit

import XMonad.Util.Cursor
import XMonad.Util.EZConfig                 -- removeKeys, additionalKeys
import XMonad.Util.Loggers
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import XMonad.Util.Paste as P               -- testing
import XMonad.Util.Run                      -- for spawnPipe and hPutStrLn
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare         -- custom WS functions filtering NSP
import XMonad.Util.XSelection

-- experimenting with tripane
import XMonad.Layout.Decoration
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.Maximize
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders

------------------------------------------------------------------------}}}
-- Main                                                                 {{{
---------------------------------------------------------------------------

main = do

    xmproc <- spawnPipe myStatusBar

    -- for independent screens
    -- nScreens <- countScreens

    -- for taffybar, add pagerHints below

    xmonad 
        $ withNavigation2DConfig myNav2DConf
        -- $ withUrgencyHook NoUrgencyHook
        $ withUrgencyHook LibNotifyUrgencyHook
        $ ewmh
        $ addDescrKeys' ((myModMask, xK_F1), showKeybindings) myKeys
        $ myConfig xmproc

myConfig p = def
        { borderWidth        = border
        , focusFollowsMouse  = myFocusFollowsMouse
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , manageHook         = myManageHook
        , handleEventHook    = myHandleEventHook
        , layoutHook         = myLayoutHook
        , logHook            = myLogHook p
        , modMask            = myModMask
        , mouseBindings      = myMouseBindings
        , startupHook        = myStartupHook
        , terminal           = myTerminal
        , workspaces         = myWorkspaces
        }


------------------------------------------------------------------------}}}
-- Workspaces                                                           {{{
---------------------------------------------------------------------------

wsAV    = "AV"
wsBSA   = "BSA"
wsCOM   = "COM"
wsDOM   = "DOM"
wsDMO   = "DMO"
wsFLOAT = "FLT"
wsGEN   = "GEN"
wsGCC   = "GCC"
wsMON   = "MON"
wsOSS   = "OSS"
wsRAD   = "RAD"
wsRW    = "RW"
wsSYS   = "SYS"
wsTMP   = "TMP"
wsVIX   = "VIX"
wsWRK   = "WRK"
wsWRK2  = "WRK:2"
wsGGC   = "GGC"

-- myWorkspaces = map show [1..9]
myWorkspaces = [wsGEN, wsWRK, wsCOM, wsSYS, wsMON, wsFLOAT, wsRW, wsTMP]

------------------------------------------------------------------------}}}
-- Applications                                                         {{{
---------------------------------------------------------------------------

-- | Uses supplied function to decide which action to run depending on current workspace name.

myTerminal          = "alacritty"
myStatusBar         = "xmobar -x0 /home/nobel/.xmonad/xmobar.conf"
myLauncher          = "dmenu_run"

------------------------------------------------------------------------}}}
-- Theme                                                                {{{
---------------------------------------------------------------------------

myFocusFollowsMouse  = True

base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green       = "#859900"

-- sizes
gap         = 10
topbar      = 10
border      = 0
prompt      = 20
status      = 20

myNormalBorderColor     = "#000000"
myFocusedBorderColor    = active

active      = blue
activeWarn  = red
inactive    = base02
focusColor  = blue
unfocusColor = base02

myFont      = "-*-terminus-medium-*-*-*-*-160-*-*-*-*-*-*"
myBigFont   = "-*-terminus-medium-*-*-*-*-240-*-*-*-*-*-*"
myWideFont  = "xft:Eurostar Black Extended:"
            ++ "style=Regular:pixelsize=180:hinting=true"

-- this is a "fake title" used as a highlight bar in lieu of full borders
-- (I find this a cleaner and less visually intrusive solution)
topBarTheme = def
    { fontName              = myFont
    , inactiveBorderColor   = base03
    , inactiveColor         = base03
    , inactiveTextColor     = base03
    , activeBorderColor     = active
    , activeColor           = active
    , activeTextColor       = active
    , urgentBorderColor     = red
    , urgentTextColor       = yellow
    , decoHeight            = topbar
    }

myTabTheme = def
    { fontName              = myFont
    , activeColor           = active
    , inactiveColor         = base02
    , activeBorderColor     = active
    , inactiveBorderColor   = base02
    , activeTextColor       = base03
    , inactiveTextColor     = base00
    }

myPromptTheme = def
    { font                  = myFont
    , bgColor               = base03
    , fgColor               = active
    , fgHLight              = base03
    , bgHLight              = active
    , borderColor           = base03
    , promptBorderWidth     = 0
    , height                = prompt
    , position              = Top
    }

warmPromptTheme = myPromptTheme
    { bgColor               = yellow
    , fgColor               = base03
    , position              = Top
    }

hotPromptTheme = myPromptTheme
    { bgColor               = red
    , fgColor               = base3
    , position              = Top
    }

myShowWNameTheme = def
    { swn_font              = myWideFont
    , swn_fade              = 0.5
    , swn_bgcolor           = "#000000"
    , swn_color             = "#FFFFFF"
    }

------------------------------------------------------------------------}}}
-- Layouts                                                              {{{
--
-- WARNING: WORK IN PROGRESS AND A LITTLE MESSY
---------------------------------------------------------------------------

-- Tell X.A.Navigation2D about specific layouts and how to handle them

myNav2DConf = def
    { defaultTiledNavigation    = centerNavigation
    , floatNavigation           = centerNavigation
    , screenNavigation          = lineNavigation
    , layoutNavigation          = [("Full",          centerNavigation)
    -- line/center same results   ,("Simple Tabs", lineNavigation)
    --                            ,("Simple Tabs", centerNavigation)
                                  ]
    , unmappedWindowRect        = [("Full", singleWindowRect)
    -- works but breaks tab deco  ,("Simple Tabs", singleWindowRect)
    -- doesn't work but deco ok   ,("Simple Tabs", fullScreenRect)
                                  ]
    }


data FULLBAR = FULLBAR deriving (Read, Show, Eq, Typeable)
instance Transformer FULLBAR Window where
    transform FULLBAR x k = k barFull (\_ -> x)

-- tabBarFull = avoidStruts $ noFrillsDeco shrinkText topBarTheme $ addTabs shrinkText myTabTheme $ Simplest
barFull = avoidStruts $ Simplest

-- cf http://xmonad.org/xmonad-docs/xmonad-contrib/src/XMonad-Config-Droundy.html

myLayoutHook = showWorkspaceName
             $ onWorkspace wsFLOAT floatWorkSpace
             $ fullscreenFloat -- fixes floating windows going full screen, while retaining "bounded" fullscreen
             $ fullScreenToggle
             $ fullBarToggle
             $ mirrorToggle
             $ reflectToggle
             $ tiled ||| tabs
  where

--    testTall = Tall 1 (1/50) (2/3)
--    myTall = subLayout [] Simplest $ trackFloating (Tall 1 (1/20) (1/2))

    floatWorkSpace      = simplestFloat
    fullBarToggle       = mkToggle (single FULLBAR)
    fullScreenToggle    = mkToggle (single FULL)
    mirrorToggle        = mkToggle (single MIRROR)
    reflectToggle       = mkToggle (single REFLECTX)
    showWorkspaceName   = showWName' myShowWNameTheme

    named n             = renamed [(XMonad.Layout.Renamed.Replace n)]
    trimNamed w n       = renamed [(XMonad.Layout.Renamed.CutWordsLeft w),
                                   (XMonad.Layout.Renamed.PrependWords n)]
    suffixed n          = renamed [(XMonad.Layout.Renamed.AppendWords n)]
    trimSuffixed w n    = renamed [(XMonad.Layout.Renamed.CutWordsRight w),
                                   (XMonad.Layout.Renamed.AppendWords n)]

    addTopBar           = noFrillsDeco shrinkText topBarTheme

    mySpacing           = spacing gap
    sGap                = quot gap 2
    myGaps              = gaps [(U, gap),(D, gap),(L, gap),(R, gap)]
    mySmallGaps         = gaps [(U, sGap),(D, sGap),(L, sGap),(R, sGap)]
    myBigGaps           = gaps [(U, gap*2),(D, gap*2),(L, gap*2),(R, gap*2)]

    -- default tiling algorithm partitions the screen into two panes
    tiled   = named "Tiled"
                $ avoidStruts
                $ addTopBar
                $ myGaps
                $ mySpacing
                $ Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

    threeCol = named "Unflexed"
         $ avoidStruts
         $ addTopBar
         $ myGaps
         $ mySpacing
         $ ThreeColMid 1 (1/10) (1/2)

    tabs = named "Tabs"
         $ avoidStruts
         $ addTopBar
         $ addTabs shrinkText myTabTheme
         $ Simplest

------------------------------------------------------------------------}}}
-- Bindings                                                             {{{
---------------------------------------------------------------------------

myModMask = mod4Mask -- super (and on my system, hyper) keys

-- Display keyboard mappings using zenity
-- from https://github.com/thomasf/dotfiles-thomasf-xmonad/
--              blob/master/.xmonad/lib/XMonad/Config/A00001.hs
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
    h <- spawnPipe "zenity --text-info --font=terminus"
    hPutStr h (unlines $ showKm x)
    hClose h
    return ()

wsKeys = map show $ [1..9] ++ [0]

-- any workspace but scratchpad
notSP = (return $ ("NSP" /=) . W.tag) :: X (WindowSpace -> Bool)
shiftAndView dir = findWorkspace getSortByIndex dir (WSIs notSP) 1
        >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)

-- hidden, non-empty workspaces less scratchpad
shiftAndView' dir = findWorkspace getSortByIndexNoSP dir HiddenNonEmptyWS 1
        >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)
nextNonEmptyWS = findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1
        >>= \t -> (windows . W.view $ t)
prevNonEmptyWS = findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1
        >>= \t -> (windows . W.view $ t)
getSortByIndexNoSP =
        fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex

-- toggle any workspace but scratchpad
myToggle = windows $ W.view =<< W.tag . head . filter 
        ((\x -> x /= "NSP" && x /= "SP") . W.tag) . W.hidden

myKeys conf = let

    subKeys str ks = subtitle str : mkNamedKeymap conf ks
    screenKeys     = ["w","v","z"]
    dirKeys        = ["j","k","h","l"]
    arrowKeys        = ["<D>","<U>","<L>","<R>"]
    dirs           = [ D,  U,  L,  R ]

    --screenAction f        = screenWorkspace >=> flip whenJust (windows . f)

    zipM  m nm ks as f = zipWith (\k d -> (m ++ k, addName nm $ f d)) ks as
    zipM' m nm ks as f b = zipWith (\k d -> (m ++ k, addName nm $ f d b)) ks as

    -- from xmonad.layout.sublayouts
    focusMaster' st = let (f:fs) = W.integrate st
        in W.Stack f [] fs
    swapMaster' (W.Stack f u d) = W.Stack f [] $ reverse u ++ d

    -- try sending one message, fallback if unreceived, then refresh
    tryMsgR x y = sequence_ [(tryMessage_ x y), refresh]

    -- warpCursor = warpToWindow (9/10) (9/10)

    -- cf https://github.com/pjones/xmonadrc
    --switch :: ProjectTable -> ProjectName -> X ()
    --switch ps name = case Map.lookup name ps of
    --  Just p              -> switchProject p
    --  Nothing | null name -> return ()

    -- do something with current X selection
    unsafeWithSelection app = join $ io $ liftM unsafeSpawn $ fmap (\x -> app ++ " " ++ x) getSelection

    toggleFloat w = windows (\s -> if M.member w (W.floating s)
                    then W.sink w s
                    else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))

    in

    -----------------------------------------------------------------------
    -- System / Utilities
    -----------------------------------------------------------------------
    subKeys "System"
    [ ("M-q"                    , addName "Restart XMonad"                  $ spawn "xmonad --restart")
    , ("M-C-c"                  , addName "Rebuild & restart XMonad"        $ spawn "xmonad --recompile && xmonad --restart")
    , ("M-S-c"                  , addName "Quit XMonad"                     $ confirmPrompt hotPromptTheme "Quit XMonad" $ io (exitWith ExitSuccess))
    , ("M-<F4>"                    , addName "Print Screen"                    $ return ())
  --, ("M-F1"                   , addName "Show Keybindings"                $ return ())
    ] ^++^

    -----------------------------------------------------------------------
    -- Actions
    -----------------------------------------------------------------------
    subKeys "Actions"
    [ ("M-o"                    , addName "Speaker"       $ spawn "pacmd set-default-sink alsa_output.pci-0000_00_1f.3.hdmi-stereo-extra1")
    , ("M-S-o"                    , addName "Headphones"       $ spawn "pacmd set-default-sink alsa_output.usb-Logitech_G533_Gaming_Headset-00.analog-stereo")
    , ("M-w"                   , addName "MuteMic"                         $ spawn "/home/nobel/Scripts/mutemic.sh")
    , ("M-v"                   , addName "ClipMenu"                         $ spawn "clipmenu")
    , ("M-p"                   , addName "Media Play Pause"                         $ spawn "playerctl play-pause")
    , ("M-S-s"                   , addName "Flameshot"                         $ spawn "flameshot gui")
    , ("M-x"                   , addName "suspend"                         $ spawn "systemctl suspend")
    , ("M-e"                   , addName "emacs-everywhere"                         $ spawn "emacsclient --eval '(emacs-everywhere)'")
    ] ^++^

    -----------------------------------------------------------------------
    -- Launchers
    -----------------------------------------------------------------------
    subKeys "Launchers"
    [ ("M-i"              , addName "Launcher"                        $ spawn myLauncher)
    , ("M-<Return>"             , addName "Terminal"                        $ spawn myTerminal)
    ] ^++^

    -----------------------------------------------------------------------
    -- Windows
    -----------------------------------------------------------------------

    subKeys "Windows"
    (
    [ ("M-<Backspace>"          , addName "Kill"                            kill1)
    , ("M-S-<Backspace>"        , addName "Kill all"                        $ confirmPrompt hotPromptTheme "kill all" $ killAll)
    , ("M-d"                    , addName "Duplicate w to all ws"           $ toggleCopyToAll)
    , ("M-b"                    , addName "Promote"                         $ promote)
    , ("M-z u"                  , addName "Focus urgent"                    focusUrgent)
    , ("M-z m"                  , addName "Focus master"                    $ windows W.focusMaster)
    , ("M-'"                    , addName "Focus down"                      $ windows W.focusDown)
    , ("M-;"                    , addName "Focus up"                        $ windows W.focusUp)
    , ("C-'"                    , addName "Swap tab D"                      $ windows W.swapDown)
    , ("C-;"                    , addName "Swap tab U"                      $ windows W.swapUp)
    ]

    ++ zipM' "M-"               "Navigate window"                           dirKeys dirs windowGo True
    ++ zipM' "M-S-"               "Move window"                               dirKeys dirs windowSwap True
    ++ zipM  "M-C-"             "Merge w/sublayout"                         dirKeys dirs (sendMessage . pullGroup)
    ++ zipM' "M-"               "Navigate screen"                           arrowKeys dirs screenGo True
    ++ zipM' "M-C-"             "Move window to screen"                     arrowKeys dirs windowToScreen True
    ) ^++^

    -----------------------------------------------------------------------
    -- Workspaces & Projects
    -----------------------------------------------------------------------

    -- original version was for dynamic workspaces
    --    subKeys "{a,o,e,u,i,d,...} focus and move window between workspaces"
    --    (  zipMod "View      ws" wsKeys [0..] "M-"      (withNthWorkspace W.greedyView)

    subKeys "Workspaces & Projects"
    (
    [ ("M-S-w"                  , addName "Shift to Project"            $ shiftToProjectPrompt warmPromptTheme)
    , ("M-<Escape>"             , addName "Next non-empty workspace"    $ nextNonEmptyWS)
    , ("M-S-<Escape>"           , addName "Prev non-empty workspace"    $ prevNonEmptyWS)
    , ("M-a"                    , addName "Toggle last workspace"       $ toggleWS' ["NSP"])
    ]
    ++ zipM "M-"                "View      ws"                          wsKeys [0..] (withNthWorkspace W.greedyView)
    -- ++ zipM "M-S-"              "Move w to ws"                          wsKeys [0..] (withNthWorkspace W.shift)
    ++ zipM "C-"                "Move w to ws"                          wsKeys [0..] (withNthWorkspace W.shift)
    -- TODO: make following a submap
    ++ zipM "M-S-C-"            "Copy w to ws"                          wsKeys [0..] (withNthWorkspace copy)
    ) ^++^

    -- TODO: consider a submap for nav/move to specific workspaces based on first initial

    -----------------------------------------------------------------------
    -- Layouts & Sublayouts
    -----------------------------------------------------------------------

    subKeys "Layout Management"

    [ ("M-t"                , addName "Cycle all layouts"               $ sendMessage NextLayout)
    , ("M-C-<Tab>"              , addName "Cycle sublayout"                 $ toSubl NextLayout)
    , ("M-S-<Tab>"              , addName "Reset layout"                    $ setLayout $ XMonad.layoutHook conf)

    , ("M-y"                    , addName "Float tiled w"                   $ withFocused toggleFloat)
    , ("M-S-y"                  , addName "Tile all floating w"             $ sinkAll)

    -- If following is run on a floating window, the sequence first tiles it.
    -- Not perfect, but works.
    , ("M-f"                , addName "Fullscreen"                      $ sequence_ [ (withFocused $ windows . W.sink)
                                                                        , (sendMessage $ XMonad.Layout.MultiToggle.Toggle FULL) ])

    -- Fake fullscreen fullscreens into the window rect. The expand/shrink
    -- is a hack to make the full screen paint into the rect properly.
    -- The tryMsgR handles the BSP vs standard resizing functions.
    , ("M-S-f"                  , addName "Fake fullscreen"             $ sequence_ [ (P.sendKey P.noModMask xK_F11)
                                                                                    , (tryMsgR (ExpandTowards L) (Shrink))
                                                                                    , (tryMsgR (ExpandTowards R) (Expand)) ])
    , ("C-S-h"                  , addName "Ctrl-h passthrough"          $ P.sendKey controlMask xK_h)
    , ("C-S-j"                  , addName "Ctrl-j passthrough"          $ P.sendKey controlMask xK_j)
    , ("C-S-k"                  , addName "Ctrl-k passthrough"          $ P.sendKey controlMask xK_k)
    , ("C-S-l"                  , addName "Ctrl-l passthrough"          $ P.sendKey controlMask xK_l)
    ] ^++^

    -----------------------------------------------------------------------
    -- Resizing
    -----------------------------------------------------------------------

    subKeys "Resize"

    [
      ("M-["                    , addName "Expand (L on BSP)"           $ tryMsgR (ExpandTowards L) (Shrink))
    , ("M-]"                    , addName "Expand (R on BSP)"           $ tryMsgR (ExpandTowards R) (Expand))
    , ("M-S-["                  , addName "Expand (U on BSP)"           $ tryMsgR (ExpandTowards U) (MirrorShrink))
    , ("M-S-]"                  , addName "Expand (D on BSP)"           $ tryMsgR (ExpandTowards D) (MirrorExpand))

    , ("M-C-["                  , addName "Shrink (L on BSP)"           $ tryMsgR (ShrinkFrom R) (Shrink))
    , ("M-C-]"                  , addName "Shrink (R on BSP)"           $ tryMsgR (ShrinkFrom L) (Expand))
    , ("M-C-S-["                , addName "Shrink (U on BSP)"           $ tryMsgR (ShrinkFrom D) (MirrorShrink))
    , ("M-C-S-]"                , addName "Shrink (D on BSP)"           $ tryMsgR (ShrinkFrom U) (MirrorExpand))

    ]
		where
			toggleCopyToAll = wsContainingCopies >>= \ws -> case ws of
							[] -> windows copyToAll
							_ -> killAllOtherCopies

myMouseBindings (XConfig {XMonad.modMask = myModMask}) = M.fromList $

    [ ((myModMask,               button1) ,(\w -> focus w
      >> mouseMoveWindow w
      >> ifClick (snapMagicMove (Just 50) (Just 50) w)
      >> windows W.shiftMaster))

    , ((myModMask .|. shiftMask, button1), (\w -> focus w
      >> mouseMoveWindow w
      >> ifClick (snapMagicResize [L,R,U,D] (Just 50) (Just 50) w)
      >> windows W.shiftMaster))

    , ((myModMask,               button3), (\w -> focus w
      >> mouseResizeWindow w
      >> ifClick (snapMagicResize [R,D] (Just 50) (Just 50) w)
      >> windows W.shiftMaster))

    , ((myModMask .|. shiftMask, button3), (\w -> focus w
      >> Sqr.mouseResizeWindow w True
      >> ifClick (snapMagicResize [R,D] (Just 50) (Just 50) w)
      >> windows W.shiftMaster ))

    , ((0, 8), (\_ -> spawn "/home/nobel/Scripts/mutemic.sh"))

--    , ((mySecondaryModMask,      button4), (\w -> focus w
--      >> prevNonEmptyWS))
--
--    , ((mySecondaryModMask,      button5), (\w -> focus w
--      >> nextNonEmptyWS))

    ]

------------------------------------------------------------------------}}}
-- Startup                                                              {{{
---------------------------------------------------------------------------

myStartupHook = do

    spawn "clipmenud"
    spawn "feh --bg-fill /home/nobel/Wallpapers/quarry.jpeg"

    setDefaultCursor xC_left_ptr

quitXmonad :: X ()
quitXmonad = io (exitWith ExitSuccess)

rebuildXmonad :: X ()
rebuildXmonad = do
    spawn "xmonad --recompile && xmonad --restart"

restartXmonad :: X ()
restartXmonad = do
    spawn "xmonad --restart"

------------------------------------------------------------------------}}}
-- Log                                                                  {{{
---------------------------------------------------------------------------

myLogHook h = do

    -- following block for copy windows marking
    copies <- wsContainingCopies
    let check ws | ws `elem` copies =
                   pad . xmobarColor yellow red . wrap "*" " "  $ ws
                 | otherwise = pad ws

    fadeWindowsLogHook myFadeHook
    ewmhDesktopsLogHook
    --dynamicLogWithPP $ defaultPP
    dynamicLogWithPP $ def

        { ppCurrent             = xmobarColor active "" . wrap "[" "]"
        , ppTitle               = xmobarColor active "" . shorten 50
        , ppVisible             = xmobarColor base0  "" . wrap "(" ")"
        , ppUrgent              = xmobarColor red    "" . wrap " " " "
        , ppHidden              = check
        , ppHiddenNoWindows     = const ""
        , ppSep                 = xmobarColor red blue "  :  "
        , ppWsSep               = " "
        , ppLayout              = xmobarColor yellow ""
        , ppOrder               = id
        , ppOutput              = hPutStrLn h  
        , ppSort                = fmap 
                                  (namedScratchpadFilterOutWorkspace.)
                                  (ppSort def)
                                  --(ppSort defaultPP)
        , ppExtras              = [] }

myFadeHook = composeAll
    [ opaque -- default to opaque
    , isUnfocused --> opacity 0.85
    , (className =? "Terminator") <&&> (isUnfocused) --> opacity 0.9
    , (className =? "URxvt") <&&> (isUnfocused) --> opacity 0.9
    , fmap ("Google" `isPrefixOf`) className --> opaque
    , isDialog --> opaque 
    --, isUnfocused --> opacity 0.55
    --, isFloating  --> opacity 0.75
    ]

------------------------------------------------------------------------}}}
-- Actions                                                              {{{
---------------------------------------------------------------------------


---------------------------------------------------------------------------
-- Urgency Hook                                                            
---------------------------------------------------------------------------
-- from https://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "notify-send" [show name, "workspace " ++ idx]
-- cf https://github.com/pjones/xmonadrc


---------------------------------------------------------------------------
-- New Window Actions
---------------------------------------------------------------------------

-- https://wiki.haskell.org/Xmonad/General_xmonad.hs_config_tips#ManageHook_examples
-- <+> manageHook defaultConfig

myManageHook :: ManageHook
myManageHook =
        manageSpecific
    <+> manageDocks
    <+> fullscreenManageHook
    <+> manageSpawn
    where
        manageSpecific = composeOne
            [ resource =? "desktop_window" -?> doIgnore
            , resource =? "stalonetray"    -?> doIgnore
            , resource =? "vlc"    -?> doFloat
            , transience
            --, isConsole -?> forceCenterFloat
            , isRole =? gtkFile  -?> forceCenterFloat
            , isDialog -?> doCenterFloat
            , isRole =? "pop-up" -?> doCenterFloat
            , isInProperty "_NET_WM_WINDOW_TYPE"
                           "_NET_WM_WINDOW_TYPE_SPLASH" -?> doCenterFloat
            , resource =? "console" -?> tileBelowNoFocus
            , isFullscreen -?> doFullFloat
            , pure True -?> tileBelow ]
        gtkFile = "GtkFileChooserDialog"
        isRole = stringProperty "WM_WINDOW_ROLE"
        -- insert WHERE and focus WHAT
        tileBelow = insertPosition Below Newer
        tileBelowNoFocus = insertPosition Below Older

---------------------------------------------------------------------------
-- X Event Actions
---------------------------------------------------------------------------

-- for reference, the following line is the same as dynamicTitle myDynHook
-- <+> dynamicPropertyChange "WM_NAME" myDynHook

-- I'm not really into full screens without my say so... I often like to
-- fullscreen a window but keep it constrained to a window rect (e.g.
-- for videos, etc. without the UI chrome cluttering things up). I can
-- always do that and then full screen the subsequent window if I want.
-- THUS, to cut a long comment short, no fullscreenEventHook
-- <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook

myHandleEventHook = docksEventHook
                <+> fadeWindowsEventHook
                <+> dynamicTitle myDynHook
                <+> handleEventHook def
                <+> XMonad.Layout.Fullscreen.fullscreenEventHook
    where
        myDynHook = composeAll
            []

---------------------------------------------------------------------------
-- Custom hook helpers
---------------------------------------------------------------------------

forceCenterFloat :: ManageHook
forceCenterFloat = doFloatDep move
  where
    move :: W.RationalRect -> W.RationalRect
    move _ = W.RationalRect x y w h

    w, h, x, y :: Rational
    w = 1/3
    h = 1/2
    x = (1-w)/2
    y = (1-h)/2
