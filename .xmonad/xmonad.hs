--
-- skangas xmonad configuration
--
-- This configuration is an adaption of the configuration by And1 over at the
-- xmonad wiki, so if you like this one you should check his stuff out.  I've
-- adapted it to better fit my way of working, which is pretty Emacs-centric.
--

import Control.Monad (liftM2)
import Data.Monoid
import System.Exit
import System.IO
import System.Posix.Unistd
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Search
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
import XMonad.Prompt.AppLauncher as AL
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Ssh
import XMonad.Util.EZConfig
import XMonad.Util.Run

import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

------------------------------------------------------------------------
-- -- My password extension

-- -- ## cabal is horrid
-- -- sudo aptitude install xclip
-- -- sudo cabal update
-- -- sudo cabal install --global sha base64-bytestring

-- import Data.ByteString.Base64
-- import qualified Data.ByteString.Char8 as S
-- import qualified Data.ByteString.Lazy.Char8 as L
-- import Data.Digest.Pure.SHA
-- import System.Environment
-- -- import XMonad.Util.XSelection -- xmonad-contrib-0.10 needed...

-- sha1_base64 :: [Char] -> [Char]
-- sha1_base64 = S.unpack . encode . S.pack . L.unpack . bytestringDigest . sha1 . L.pack

-- passwd :: [Char] -> [Char]
-- passwd s = (take 8 $ sha1_base64 s) ++ "1a"

-- mkPass :: [Char] -> IO [Char]
-- mkPass site = do
--   home <- getEnv "HOME"
--   pass <- readFile (home ++ "/bin/.webpass")
--   return $ passwd (pass ++ ":" ++ site)

-- data PWPrompt = PWPrompt

-- instance XPrompt PWPrompt where
--   showXPrompt PWPrompt = "Site: "

-- pwPrompt :: XPConfig -> X ()
-- pwPrompt c = do
--   sc <- io sshComplList
--   mkXPrompt PWPrompt c (mkComplFunFromList []) toclip

-- toclip :: String -> X ()
-- toclip s = spawn $ "echo '" ++ s ++ "' | xclip"

------------------------------------------------------------------------

statusWidth "huey"      = 800
statusWidth "kollontaj" = 550
statusWidth _           = 600
topWidth "huey"      = 1024
topWidth "kollontaj" = 748
topWidth _           = 600
                                       

myStatusBarFont = "-artwiz-nu.se-medium-r-normal-*-*-*-*-*-*-*-iso8859-1"
myStatusBar host = "dzen2 -x '0' -y '0' -h '12' -ta 'l' "
                   ++ "-w '"  ++ show (statusWidth host) ++ "' "
                   ++ "-fg '" ++ myDzenFGColor     ++ "' "
                   ++ "-bg '" ++ myNormalBGColor   ++ "' "
                   ++ "-fn '" ++ myStatusBarFont   ++ "' "
                      where

myTopBar host = "conky -c ~/.xmonad/conkyrc | dzen2 -y '0' -h '12' -ta 'r' "
                ++ "-x '"  ++ show (statusWidth host)  ++ "' "
                ++ "-w '"  ++ show (topWidth host) ++ "' "
                ++ "-fg '" ++ myDzenFGColor     ++ "' "
                ++ "-bg '" ++ myNormalBGColor   ++ "' "
                ++ "-fn '" ++ myStatusBarFont   ++ "' "

myTrayer = "trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --width 5 --transparent true --tint 0x000000 --heighttype pixel --height 5"

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
myFocusedFGColor = "#000000" -- XXX: does not work!
myFocusedBGColor = "#666"
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
    myCFloats = ["Ekiga", "Nvidia-settings", "XCalc", "Xmessage", "java-lang-Thread", "LCSMain", "Nautilus", "Eclipse", "Ediff"] --"MPlayer", "Nitrogen", "XFontSel", WM_CLASS(STRING) = "sun-awt-X11-XFramePeer", "java-lang-Thread"
    myTFloats = ["Downloads", "Iceweasel Preferences", "Save As...", "Ediff"]
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
myTerminal      = "rxvt"
 
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
 
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#151515"
myFocusedBorderColor = "#ffff00"
 
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--

tyda = searchEngine "tyda" "http://tyda.se/search?form=1&w_lang=&x=0&y=0&w="

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_F1    ), spawn $ "setxkbmap se_sv_dvorak")
    , ((modm .|. shiftMask, xK_F2    ), spawn $ "setxkbmap se")
    , ((modm,               xK_F11   ), spawn $ "mplayer ~/.xmonad/rimshot.mp3")
    , ((modm,               xK_F12   ), spawn $ "xscreensaver-command --lock")

    , ((modm,               xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_e     ), spawn "emacsclient -c -a emacs")

    , ((modm,               xK_w     ), spawn "conkeror")
    , ((modm .|. shiftMask, xK_w     ), spawn "iceweasel")

    , ((modm .|. shiftMask, xK_v     ), spawn "urxvt -tr -tint yellow -sh 15 -e alsamixer")
    , ((modm .|. shiftMask, xK_z     ), spawn "urxvt -tr -tint yellow -sh 15 -e ncmpcpp")

    , ((modm,               xK_j     ), spawn "exe=`dmenu_path | dmenu -b -nb black -nf grey` && eval \"exec $exe\"")
    , ((modm .|. shiftMask, xK_j     ), runOrRaisePrompt defaultXPConfig)

    , ((modm,               xK_s     ), sshPrompt defaultXPConfig)

    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm .|. controlMask, xK_b   ), sendMessage $ ToggleStruts)

    -- Change screen layout
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modm,               xK_r     ), refresh)

    -- Banish mouse pointer to top right
    , ((modm,               xK_u     ), banishScreen UpperRight)

    -- CycleWS
    , ((modm,               xK_b     ), moveTo Prev NonEmptyWS)
    , ((modm,               xK_f     ), moveTo Next NonEmptyWS)
    , ((modm .|. shiftMask, xK_b     ), shiftToPrev)
    , ((modm .|. shiftMask, xK_f     ), shiftToNext)
    , ((modm,               xK_Right), nextScreen)
    , ((modm,               xK_Left),  prevScreen)
    , ((modm .|. shiftMask, xK_Right), shiftNextScreen)
    , ((modm .|. shiftMask, xK_Left),  shiftPrevScreen)
    , ((modm,               xK_z),     toggleWS)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_n     ), windows W.focusDown)
    , ((modm,               xK_p     ), windows W.focusUp)
    , ((modm,               xK_m     ), windows W.focusMaster  )
    , ((modm .|. shiftMask, xK_m     ), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_n     ), windows W.swapDown)
    , ((modm .|. shiftMask, xK_p     ), windows W.swapUp)
 
    -- Generate password for weblogins
    , ((modm,               xK_g     ), AL.launchApp defaultXPConfig "webpass")

    -- Shrink/Expand the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment/Deincrement the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    , ((modm .|. shiftMask, xK_t),      do promptSearch defaultXPConfig tyda
                                           windows (W.greedyView ((XMonad.workspaces conf) !! 1)))

    -- Quit/Restart xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm              , xK_q     ), do spawn "killall trayer"
                                           spawn "killall dzen2"
                                           spawn "killall conky"
                                           restart "xmonad" True)
    ]

     --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]


    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    -- ++
    -- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
 
-- Additional keybindings, used by additionalKeysP below
myAdditionalKeys =
  [ ("<XF86AudioLowerVolume>", spawn "pavcs.sh down")
  , ("<XF86AudioMute>",        spawn "pavcs.sh toggle")
  , ("<XF86AudioRaiseVolume>", spawn "pavcs.sh up")
  , ("<XF86AudioPlay>",        spawn "mpc toggle")
  , ("<XF86AudioStop>",        spawn "mpc stop")
  , ("<XF86AudioPrev>",        spawn "mpc prev")
  , ("<XF86AudioNext>",        spawn "mpc next")
  , ("<XF86Sleep>",            spawn "xlock") ]
 
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
myLayout = avoidStruts $ layoutHints $ onWorkspace (myWorkspaces!!5) gimpLayout $ onWorkspace (myWorkspaces!!7) (noBorders Simplest) $ smartBorders (Full ||| resizableTile ||| Mirror resizableTile)
   where
    resizableTile = ResizableTall nmaster delta ratio []
    -- tabbedLayout = tabbedBottomAlways shrinkText myTheme
    -- gimpLayout = tabbedLayout ||| Full
    gimpLayout = withIM (0.11) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") Full
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
  host <- fmap nodeName getSystemID
  dzen <- spawnPipe (myStatusBar host)
  topBar <- spawnPipe (myTopBar host)
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
           } `additionalKeysP` myAdditionalKeys

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
    staticWs = [] -- WAS take 1 myWorkspaces

