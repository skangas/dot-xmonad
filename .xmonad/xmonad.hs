--
-- skangas xmonad configuration
--
-- This configuration is an adaption of the configuration by And1 over at the
-- xmonad wiki, so if you like this one you should check his stuff out.  I've
-- adapted it to better fit my way of working, which is pretty Emacs-centric.
--
-- NOTE: Those updating from earlier xmonad versions, who use
-- EwmhDesktops, safeSpawn, WindowGo, or the simple-status-bar
-- setup functions (dzen, xmobar) probably need to change
-- xmonad.hs, please see the notes below, or the following
-- link for more details:
--
-- http://www.haskell.org/haskellwiki/Xmonad/Notable_changes_since_0.8
--

import Control.Monad (liftM2)
import Data.Monoid
import System.Exit
import System.IO
import XMonad
import XMonad.Actions.Warp
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName -- needed to work around buggy java
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Ssh
import XMonad.Util.Run
 
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

------------------------------------------------------------------------

myStatusBar = "dzen2 -x '0' -y '0' -h '16' -w '600' -ta 'l' "
              ++ "-fg '" ++ myNormalFGColor ++ "' "
              ++ "-bg '" ++ myNormalBGColor ++ "' "
              ++ "-fn '" ++ myFont ++ "' "
myTopBar = "conky -c .conkytop | dzen2 -x '600' -y '0' -h '16' -w '616' -ta 'r' "
           ++ "-fg '" ++ myDzenFGColor ++ "' "
           ++ "-bg '" ++ myNormalBGColor ++ "' "
           ++ "-fn '" ++ myFont ++ "' "
myTrayer = "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 5 --transparent true --tint 0x000000 --height 12"
 
-- Urgency hint options:
myUrgencyHook = withUrgencyHook dzenUrgencyHook
    { args = ["-x", "0", "-y", "1184", "-h", "16", "-w", "1920", "-ta", "r", "-expand", "l", "-fg", "" ++ myUrgentFGColor ++ "", "-bg", "" ++ myNormalBGColor ++ "", "-fn", "" ++ myFont ++ ""] }
 
--myMPDBar = "conky -c .conkympd | dzen2 -x '0' -y '1184' -h '16' -w '1600' -ta 'l' -fg '" ++ myDzenFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
--myHDDBar = "conky -c .conkyhdd | dzen2 -x '1600' -y '1184' -h '16' -w '320' -ta 'r' -fg '" ++ myDzenFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"

-- Color, font and iconpath definitions:
myFont = "-xos4-terminus-medium-r-normal-*-12-*-*-*-c-*-iso10646-1"
myIconDir = "/home/and1/.dzen"
myNormalFGColor = "grey"
myNormalBGColor = "#0f0f0f"
myDzenFGColor = myNormalFGColor
myDzenBGColor = myNormalBGColor
myFocusedFGColor = "#f0f0f0"
myFocusedBGColor = "#333333"
myUrgentFGColor = "#0099ff"
myUrgentBGColor = "#0077ff"
myIconFGColor = "#777777"
myIconBGColor = "#0f0f0f"
mySeperatorColor = "#555555"

------------------------------------------------------------------------
-- Window rules:

myManageHook = composeAll . concat $
    [ [className =? c --> doFloat | c <- myCFloats]
    , [ isFullscreen --> doFullFloat ]
--    , [isDialog --> doFloat]
    , [title =? t --> doFloat | t <- myTFloats]
    , [resource =? r --> doFloat | r <- myRFloats]
    , [resource =? i --> doIgnore | i <- myIgnores]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces!!0) | x <- my1Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces!!1) | x <- my2Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces!!2) | x <- my3Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces!!3) | x <- my4Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces!!4) | x <- my5Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces!!5) | x <- my6Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (myWorkspaces!!6) | x <- my7Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (myWorkspaces!!7) | x <- my8Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (myWorkspaces!!8) | x <- my9Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (myWorkspaces!!8) | x <- my9Shifts]
    ]
    where
    doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
    myCFloats = ["Ekiga", "Nvidia-settings", "XCalc", "Xmessage", "java-lang-Thread", "LCSMain", "Nautilus"] --"MPlayer", "Nitrogen", "XFontSel", WM_CLASS(STRING) = "sun-awt-X11-XFramePeer", "java-lang-Thread"
    myTFloats = ["Downloads", "Iceweasel Preferences", "Save As..."]
    myRFloats = []
    myIgnores = ["desktop_window", "kdesktop"]
    my1Shifts = ["Emacs"]
    my2Shifts = ["Conkeror", "Iceweasel", "Firefox"]
    my3Shifts = ["Ekiga","Skype","Pidgin"]
    my4Shifts = []
    my5Shifts = []
    my6Shifts = ["Gimp"]
    my7Shifts = ["Virtual-Box", "Wine"]
    my8Shifts = ["MPlayer"]
    my9Shifts = ["Amarok"]

-- XPConfig options:
myXPConfig = defaultXPConfig
    { font = "" ++ myFont ++ ""
    , bgColor = "" ++ myNormalBGColor ++ ""
    , fgColor = "" ++ myNormalFGColor ++ ""
    , fgHLight = "" ++ myNormalFGColor ++ ""
    , bgHLight = "" ++ myUrgentBGColor ++ ""
    , borderColor = "" ++ myFocusedBorderColor ++ ""
    , promptBorderWidth = 1
    , position = Bottom
    , height = 16
    , historySize = 100
    --, historyFilter = ""
    --, promptKeymap = ""
    --, completionKey = ""
    --, defaultText = ""
    --, autoComplete = "KeySym"
    --, showCompletionOnTab = ""
    }
 
-- Theme options:
myTheme = defaultTheme
    { activeColor = "" ++ myFocusedBGColor ++ ""
    , inactiveColor = "" ++ myDzenBGColor ++ ""
    , urgentColor = "" ++ myUrgentBGColor ++ ""
    , activeBorderColor = "" ++ myFocusedBorderColor ++ ""
    , inactiveBorderColor = "" ++ myNormalBorderColor ++ ""
    , urgentBorderColor = "" ++ myNormalBorderColor ++ ""
    , activeTextColor = "" ++ myFocusedFGColor ++ ""
    , inactiveTextColor = "" ++ myDzenFGColor ++ ""
    , urgentTextColor = "" ++ myUrgentFGColor ++ ""
    , fontName = "" ++ myFont ++ ""
    --, decoWidth = ""
    --, decoHeight = ""
    }

------------------------------------------------------------------------

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "xterm"
 
-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
 
-- Width of the window border in pixels.
--
myBorderWidth   = 1
 
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
myNumlockMask   = 0
 
myWorkspaces    = ["1:emacs","2:browser","3:comm","4","5","6","7","8:multimedia","9:music"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#151515"
myFocusedBorderColor = "#ffff00"
 
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm,               xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_e     ), spawn "emacsclient -c -a emacs")
    , ((modm,               xK_a     ), spawn "amarok")
    , ((modm,               xK_f     ), spawn "conkeror")
    , ((modm .|. shiftMask, xK_f     ), spawn "firefox")
--    , ((modm .|. shiftMask, xK_p     ), spawn "pidgin")
      -- FIXME: when xterm isn't resized, alsamixer stays really small.
      -- add stuff to resize the window after spawned
    , ((modm .|. shiftMask, xK_v     ), do spawn "xterm -e alsamixer"
                                           spawn "xterm -e echo \"FIXME: fulhack\"")
    , ((modm,               xK_j     ), spawn "exe=`dmenu_path | dmenu -b -nb black -nf grey` && eval \"exec $exe\"")
    , ((modm .|. shiftMask, xK_j     ), runOrRaisePrompt defaultXPConfig)
    , ((modm,               xK_s     ), sshPrompt defaultXPConfig)

    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm .|. controlMask, xK_b   ), sendMessage $ ToggleStruts)

    , ((modm,               xK_space ), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modm,               xK_r     ), refresh)
    , ((modm,               xK_b     ), banishScreen UpperRight)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_n     ), windows W.focusDown)
    , ((modm,               xK_p     ), windows W.focusUp)
    , ((modm,               xK_m     ), windows W.focusMaster  )
    , ((modm .|. shiftMask, xK_m     ), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown)
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp)
 
    -- Shrink/Expand the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
 
    -- Increment/Deincrement the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Quit/Restart xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm              , xK_q     ), do spawn "killall trayer"
                                           spawn "killall dzen2"
                                           spawn "killall conky"
                                           restart "xmonad" True)
    ]
    ++
 
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    -- ++
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    -- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
 
 
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
 
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
 
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
 
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
 
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
 
------------------------------------------------------------------------
-- Layouts:
 
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- * NOTE: XMonad.Hooks.EwmhDesktops users must remove the obsolete
-- ewmhDesktopsLayout modifier from layoutHook. It no longer exists.
-- Instead use the 'ewmh' function from that module to modify your
-- defaultConfig as a whole. (See also logHook, handleEventHook, and
-- startupHook ewmh notes.)
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
--Layouts:
myLayout = avoidStruts $ layoutHints $ onWorkspace (myWorkspaces!!1) (TwoPane (1/3) (2/3)) $ onWorkspace (myWorkspaces!!5) gimpLayout $ onWorkspace (myWorkspaces!!7) (noBorders Simplest) $ smartBorders (Full ||| resizableTile ||| Mirror resizableTile)
   where
    resizableTile = ResizableTall nmaster delta ratio []
    tabbedLayout = tabbedBottomAlways shrinkText myTheme
    gimpLayout = tabbedLayout ||| Full
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio = toRational (2/(1+sqrt(5)::Double))
    -- Percent of screen to increment by when resizing panes
    delta = 3/100

------------------------------------------------------------------------
-- Event handling
 
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH event handling to your custom event hooks by
-- combining them with ewmhDesktopsEventHook.
--
--myEventHook = mempty -- 0.9
 
------------------------------------------------------------------------
-- Status bars and logging
 
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH logHook actions to your custom log hook by
-- combining it with ewmhDesktopsLogHook.
--
--myLogHook = 
 
------------------------------------------------------------------------
-- Startup hook
 
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add initialization of EWMH support to your custom startup
-- hook by combining it with ewmhDesktopsStartup.
--
myStartupHook = setWMName "LG3D" -- needed to work around buggy java
 
------------------------------------------------------------------------
-- Run xmonad with the settings you specify. No need to modify this.
main = do
--  xmproc <- spawnPipe "/home/skangas/local/bin/xmobar /home/skangas/.xmobarrc"
-- spawnPipe "xmessage 'hello'"
  dzen <- spawnPipe myStatusBar
  topBar <- spawnPipe myTopBar
  trayer <- spawnPipe myTrayer
  xmonad $ myUrgencyHook $ defaultConfig
           { terminal           = myTerminal
           , focusFollowsMouse  = myFocusFollowsMouse
           , borderWidth        = myBorderWidth
           , modMask            = myModMask
           , numlockMask        = myNumlockMask
           , workspaces         = myWorkspaces
           , normalBorderColor  = myNormalBorderColor
           , focusedBorderColor = myFocusedBorderColor

           , keys               = myKeys
           , mouseBindings      = myMouseBindings

--           , logHook = dynamicLogWithPP $ xmobarPP
--                       { ppOutput = hPutStrLn xmproc
--                       , ppTitle = xmobarColor "green" "" . shorten 50
--                       }
           , logHook    = dynamicLogWithPP $ myDzenPP dzen
           , startupHook        = myStartupHook
           , layoutHook         = myLayout
                                  --layoutHook = smartBorders $ avoidStruts $ layoutHook defaultConfig
           , manageHook         = manageDocks <+> myManageHook
--           , handleEventHook    = myEventHook, -- 0.9
           }

-- dynamicLog pretty printer for dzen:
myDzenPP h = defaultPP
    { ppCurrent = wrap ("^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myFocusedBGColor ++ ")^p()^i(" ++ myIconDir ++ "/corner.xbm)^fg(" ++ myNormalFGColor ++ ")") "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppVisible = wrap ("^fg(" ++ myNormalFGColor ++ ")^bg(" ++ myNormalBGColor ++ ")^p()^i(" ++ myIconDir ++ "/corner.xbm)^fg(" ++ myNormalFGColor ++ ")") "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppHidden = wrap ("^i(" ++ myIconDir ++ "/corner.xbm)") "^fg()^bg()^p()" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId -- don't use ^fg() here!!
    --, ppHiddenNoWindows = wrap ("^fg(" ++ myDzenFGColor ++ ")^bg()^p()^i(" ++ myIconDir ++ "/corner.xbm)") "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppHiddenNoWindows = \wsId -> if wsId `notElem` staticWs then "" else wrap ("^fg(" ++ myDzenFGColor ++ ")^bg()^p()^i(" ++ myIconDir ++ "/corner.xbm)") "^fg()^bg()^p()" . dropIx $ wsId
    , ppUrgent = wrap (("^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myNormalBGColor ++ ")^p()^i(" ++ myIconDir ++ "/corner.xbm)^fg(" ++ myUrgentFGColor ++ ")")) "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppSep = " "
    , ppWsSep = " "
    , ppTitle = dzenColor ("" ++ myNormalFGColor ++ "") "" . wrap "< " " >"
    , ppLayout = dzenColor ("" ++ myNormalFGColor ++ "") "" .
        (\x -> case x of
        "Hinted Full" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-full.xbm)"
        "Hinted ResizableTall" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-tall-right.xbm)"
        "Hinted Mirror ResizableTall" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-mirror-bottom.xbm)"
        "Hinted combining Tabbed Bottom Simplest and Full with DragPane  Vertical 0.1 0.8" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-gimp.xbm)"
        _ -> x
        )
    , ppOutput = hPutStrLn h
    }
    where
    dropIx wsId = if (':' `elem` wsId) then drop 2 wsId else wsId -- remove number in front of name
    staticWs = [] -- nothing WAS take 1 myWorkspaces

