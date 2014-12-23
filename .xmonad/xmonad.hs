--
-- skangas xmonad configuration
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
import XMonad.Prompt
import XMonad.Prompt.AppLauncher as AL
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Ssh
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.Scratchpad

import XMonad.Layout.IM
import XMonad.Layout.LayoutHints
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

main :: IO()
main = do
--  xmproc <- spawnPipe "/home/skangas/local/bin/xmobar /home/skangas/.xmobarrc"
  host <- fmap nodeName getSystemID
  dzen <- spawnPipe (myStatusBar host)
  topBar <- spawnPipe (myTopBar host)
  trayer <- spawnPipe myTrayer
  xmonad $ myConfig host dzen

myConfig host dzen = myUrgencyHook $
     defaultConfig
        { terminal           = if (elem host workHosts) then "xterm" else "rxvt"
        , focusFollowsMouse  = True
        , borderWidth        = 1
        , modMask            = mod4Mask
--        , numlockMask        = 0
        , workspaces         = myWorkspaces
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor

        , mouseBindings      = myMouseBindings

--        , logHook = dynamicLogWithPP $ xmobarPP
--                    { ppOutput = hPutStrLn xmproc
--                    , ppTitle = xmobarColor "green" "" . shorten 50
--                    }
        , logHook            = dynamicLogWithPP $ myDzenPP dzen
        , startupHook        = do
               setWMName "LG3D"
               return ()
               checkKeymap (myConfig host dzen) (myKeys host dzen) -- needed to work around buggy java
        , layoutHook         = myLayout
        , manageHook         = manageDocks <+> myManageHook
        -- , handleEventHook    = followOnlyIf (queryFocused whenToFollow)
        } `additionalKeysP` myKeys host dzen


workHosts = ["eselnts1280"]

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

myTrayer = "trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --width 5 --transparent true --tint 0x000000 --heighttype pixel --height 11"

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
    , [ namedScratchpadManageHook scratchpads ]
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

myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
myNormalBorderColor  = "#151515"
myFocusedBorderColor = "#ffff00"

myKeys host dzen = myKeymap host (myConfig host dzen)
myKeymap host conf
  | elem host workHosts = limitedKeymap host conf
  | otherwise     = defaultKeymap host conf        

-- Limited keymap for work NX environment
limitedKeymap host conf =
  -- mod-[F1..F12],       Switch to workspace N
  -- mod-shift-[F1..F12], Move client to workspace N
  [ ("M-" ++ m ++ k, windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) ["F1", "F2", "F3", "F4", "F5",
                                                "F6", "F7", "F8", "F9"]
      , (f, m) <- [ (W.greedyView, "")
                  , (W.shift, "S-") ]
  ]
  ++
  [ ("C-c M-<Space>",    sendMessage NextLayout)
  -- , ("M-S-<space>", setLayout $ XMonad.layoutHook conf)
  -- Move focus to the next window
  , ("M-c n",         windows W.focusDown)
  , ("M-c p",         windows W.focusUp)
  , ("M-c m",         windows W.focusMaster)
  , ("M-c M",       windows W.swapMaster)
  , ("M-c N",       windows W.swapDown)
  , ("M-c P",       windows W.swapUp)
  , ("M-c t",         withFocused $ windows . W.sink)

  , ("M-c M-r",         refresh)
  , ("M-c M-u",         banishScreen UpperRight)

  , ("M-c S-<F1>",    spawn $ "setxkbmap se_sv_dvorak")
  , ("M-c S-<F2>",    spawn $ "setxkbmap se")
  , ("M-c <F12>",     spawn $ "xscreensaver-command --lock")

  , ("M-c <Return>",  spawn "rxvt")
  , ("M-c e",         spawn "emacsclient -c -a emacs")

  , ("M-c x c",       spawn "exe=`dmenu_path | dmenu -b -nb black -nf grey` && eval \"exec $exe\"")
  , ("M-c x f",       spawn "firefox")
  , ("M-c x r",       runOrRaisePrompt myXPConfig)
  , ("M-c s",         sshPrompt myXPConfig)

  , ("M-c S-c",       kill)
  , ("M-c C-b",       sendMessage $ ToggleStruts)

  , ("M-c '", namedScratchpadAction scratchpads "term")

  , ("M-c Q", io (exitWith ExitSuccess))
  , ("M-c q", do spawn "killall trayer"
                 spawn "killall dzen2"
                 spawn "killall conky"
                 restart "xmonad" True)

  -- CycleWS
  , ("M-c b", moveTo Prev NonEmptyWS)
  , ("M-c f", moveTo Next NonEmptyWS)
  , ("M-c S-b", shiftToPrev)
  , ("M-c S-f", shiftToNext)
  , ("M-c <Right>", nextScreen)
  , ("M-c <Left>",  prevScreen)
  , ("M-c S-<Right>", shiftNextScreen)
  , ("M-c S-<Left>",  shiftPrevScreen)
  , ("M-c z",     toggleWS)

  -- Shrink/Expand the master area
  , ("M-c h", sendMessage Shrink)
  , ("M-c l", sendMessage Expand)
  ]

defaultKeymap host conf =
  -- mod-[1..],       Switch to workspace N
  -- mod-shift-[1..], Move client to workspace N
  -- mod-ctrl-[1..],  Switch to workspace N on other screen
  [ ("M-" ++ m ++ [k], windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) "1234567890[]"
      , (f, m) <- [ (W.greedyView, "")
                  , (W.shift, "S-") ]
  ]
  ++
  [ ("M-<Space>",    sendMessage NextLayout)
  -- , ("M-S-<space>", setLayout $ XMonad.layoutHook conf)
  -- Move focus to the next window
  , ("M-n",         windows W.focusDown)
  , ("M-p",         windows W.focusUp)
  , ("M-m",         windows W.focusMaster)
  , ("M-S-m",       windows W.swapMaster)
  , ("M-S-n",       windows W.swapDown)
  , ("M-S-p",       windows W.swapUp)
  , ("M-t",         withFocused $ windows . W.sink)

  , ("M-r",         refresh)
  , ("M-u",         banishScreen UpperRight)

  , ("M-S-<F1>",    spawn $ "setxkbmap se_sv_dvorak")
  , ("M-S-<F2>",    spawn $ "setxkbmap se")
  , ("M-<F12>",     spawn $ "xscreensaver-command --lock")

  , ("M-<Return>",  spawn "rxvt")
  , ("M-e",         spawn "emacsclient -c -a emacs")

  , ("M-x c",       spawn "exe=`dmenu_path | dmenu -b -nb black -nf grey` && eval \"exec $exe\"")
  , ("M-x f",       spawn "firefox")
  , ("M-x m",       spawn "thunderbird")
  , ("M-x p",       spawn "pidgin")
  , ("M-x x",       spawn "conkeror")
  , ("M-x r",       runOrRaisePrompt myXPConfig)
  -- , ("M-x t", do promptSearch myXPConfig tyda
  --                windows (W.greedyView ((XMonad.workspaces conf) !! 1)))
  -- , ("M-x w",         spawn "iceweasel")

  , ("M-s",         sshPrompt myXPConfig)

  , ("M-S-c",       kill)
  , ("M-C-b",       sendMessage $ ToggleStruts)

  , ("<XF86AudioLowerVolume>", spawn "pavcs.sh down")
  , ("<XF86AudioMute>",        spawn "pavcs.sh toggle")
  , ("<XF86AudioRaiseVolume>", spawn "pavcs.sh up")
  , ("<XF86AudioPlay>",        spawn "mpc toggle")
  , ("<XF86AudioStop>",        spawn "mpc stop")
  , ("<XF86AudioPrev>",        spawn "mpc prev")
  , ("<XF86AudioNext>",        spawn "mpc next")

  , ("<XF86Sleep>",            spawn "xlock")

  , ("M-'", namedScratchpadAction scratchpads "term")
  , ("M-S-v", namedScratchpadAction scratchpads "mixer")
  , ("M-S-z", namedScratchpadAction scratchpads "music")

  , ("M-S-q", io (exitWith ExitSuccess))
  , ("M-q", do spawn "killall trayer"
               spawn "killall dzen2"
               spawn "killall conky"
               restart "xmonad" True)

  -- CycleWS
  , ("M-b", moveTo Prev NonEmptyWS)
  , ("M-f", moveTo Next NonEmptyWS)
  , ("M-S-b", shiftToPrev)
  , ("M-S-f", shiftToNext)
  , ("M-<Right>", nextScreen)
  , ("M-<Left>",  prevScreen)
  , ("M-S-<Right>", shiftNextScreen)
  , ("M-S-<Left>",  shiftPrevScreen)
  , ("M-z",     toggleWS)

    -- Generate password for weblogins
   , ("M-g", AL.launchApp myXPConfig "webpass")

  -- Shrink/Expand the master area
  , ("M-h", sendMessage Shrink)
  , ("M-l", sendMessage Expand)

  -- Increment/Deincrement the number of windows in the master area
  , ("M-,", sendMessage (IncMasterN 1))
  , ("M-.", sendMessage (IncMasterN (-1)))
  ]
 -- where
 --    tyda = searchEngine "tyda" "http://tyda.se/search?form=1&w_lang=&x=0&y=0&w="

scratchpads = [
     NS "term"  (term ++ "-name scratchpad") (resource =? "scratchpad") termFloat,
     NS "mixer" (term ++ "-e alsamixer") (title =? "alsamixer") mySPFloat,
     NS "music" (term ++ "-e ncmpcpp") (title =? "ncmpcpp") mySPFloat
 ] where
     term = "urxvt -tr -sh 15 "
     mySPFloat = customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)
     termFloat = customFloating $ W.RationalRect 0.04 0.04 0.92 0.92
     role = stringProperty "WM_WINDOW_ROLE"

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

myLayout =
    smartBorders $
    avoidStruts $
    layoutHints $
    onWorkspace "6" gimpLayout $
    onWorkspace "8" (noBorders Simplest) $
    tabbed shrinkText tabTheme ||| TwoPane (3/100) (1/2) ||| ThreeCol 1 (3/100) (1/3) ||| Full
  where
    tabTheme = defaultTheme {
             decoHeight = 15,
             fontName = "lime.se"}
    resizableTile = ResizableTall nmaster delta ratio []
    -- gimpLayout = tabbedLayout ||| Full
    -- tabbedLayout = tabbedBottomAlways shrinkText myTheme
    gimpLayout = withIM (0.11) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") Full
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio = toRational (2/(1+sqrt(5)::Double))
    -- Percent of screen to increment by when resizing panes
    delta = 3/100

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
