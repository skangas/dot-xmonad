--
-- skangas' xmonad configuration
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
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
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
-- import XMonad.Layout.TwoPane
-- import XMonad.Layout.WindowNavigation

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

--- Needed for password extension below
-- import Data.ByteString.Base64
-- import Data.Char (isSpace)
-- import qualified Data.ByteString.Char8 as S
-- import qualified Data.ByteString.Lazy.Char8 as L
-- import Data.Digest.Pure.SHA
-- import System.Environment
-- import System.Process
-- import qualified XMonad.Util.Paste as Paste



main :: IO()
main = do
--  xmproc <- spawnPipe "/home/skangas/local/bin/xmobar /home/skangas/.xmobarrc"
  host <- fmap nodeName getSystemID
  dzen <- spawnPipe (myStatusBar host)
  topBar <- spawnPipe (myTopBar host)
  trayer <- spawnPipe myTrayer
  xmonad $ ewmh $ myConfig host dzen

workHosts = ["eselnts1280"]
myTerm host = if (elem host workHosts) then "xterm" else "rxvt -e zsh"

myConfig host dzen = myUrgencyHook $
     defaultConfig
        { terminal           = myTerm host
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
        , logHook            = (dynamicLogWithPP $ myDzenPP dzen) >> updatePointer (0.5, 0.5) (1, 1)
        , startupHook        = do
               setWMName "LG3D"
               return ()
               checkKeymap (myConfig host dzen) (myKeys host dzen) -- needed to work around buggy java
        , layoutHook         = myLayout
        , manageHook         = manageDocks <+> myManageHook
        -- , handleEventHook    = followOnlyIf (queryFocused whenToFollow)
        } `additionalKeysP` myKeys host dzen


leftStatusWidth "joffe"      = 1200
leftStatusWidth "kollontaj" = 550
leftStatusWidth _           = 600
rightStatusWidth "joffe"      = 1232
rightStatusWidth "kollontaj" = 748
rightStatusWidth _           = 600


-- myStatusBarFont = "-artwiz-nu.se-*-*-*-*-*-*-*-*-*-*-iso8859-1"
myStatusBarFont = "-artwiz-aqui.se-*-*-*-*-16-*-*-*-*-*-*-*"
myStatusBar host = "dzen2 -x '0' -y '0' -h '16' -ta 'l' "
                   ++ "-w '"  ++ show (leftStatusWidth host) ++ "' "
                   ++ "-fg '" ++ myDzenFGColor     ++ "' "
                   ++ "-bg '" ++ myNormalBGColor   ++ "' "
                   ++ "-fn '" ++ myStatusBarFont   ++ "' "

myTopBar host = "conky -c ~/.xmonad/conkyrc | dzen2 -y '0' -h '16' -ta 'r' "
                ++ "-x '"  ++ show (leftStatusWidth host)  ++ "' "
                ++ "-w '"  ++ show (rightStatusWidth host) ++ "' "
                ++ "-fg '" ++ myDzenFGColor     ++ "' "
                ++ "-bg '" ++ myNormalBGColor   ++ "' "
                ++ "-fn '" ++ myStatusBarFont   ++ "' "

myTrayer = "trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --width 5 --transparent true --tint 0x000000 --alpha 0 --heighttype pixel --height 16"

-- Urgency hint options:
myUrgencyHook = withUrgencyHook dzenUrgencyHook
    { args = ["-x", "0", "-y", "1184", "-h", "16", "-w", "1920", "-ta", "r", "-expand", "l", "-fg", "" ++ myUrgentFGColor ++ "", "-bg", "" ++ myNormalBGColor ++ "", "-fn", "" ++ myFont ++ ""] }

--myMPDBar = "conky -c .conkympd | dzen2 -x '0' -y '1184' -h '16' -w '1600' -ta 'l' -fg '" ++ myDzenFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
--myHDDBar = "conky -c .conkyhdd | dzen2 -x '1600' -y '1184' -h '16' -w '320' -ta 'r' -fg '" ++ myDzenFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"

-- Color, font and iconpath definitions:
myFont = "-xos4-terminus-medium-r-normal-*-12-*-*-*-c-*-iso10646-1"
myIconDir = "/home/skangas/.xmonad/dzen2-icons"
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
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (myWorkspaces!!9) | x <- my10Shifts]
    , [ namedScratchpadManageHook scratchpads ]
    ]
    where
    doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
    myCFloats = ["Ekiga", "Nvidia-settings", "XCalc", "Xmessage", "java-lang-Thread", "LCSMain", "Nautilus", "Eclipse"] --"MPlayer", "Nitrogen", "XFontSel", WM_CLASS(STRING) = "sun-awt-X11-XFramePeer", "java-lang-Thread"
    myTFloats = ["Downloads", "Iceweasel Preferences", "Save As...", "SK Fractals"]
    myRFloats = []
    myIgnores = ["desktop_window", "kdesktop"]
    my1Shifts = ["Emacs"]
    my2Shifts = ["Conkeror", "Iceweasel", "Firefox"]
    my3Shifts = []
    my4Shifts = []
    my5Shifts = []
    my6Shifts = ["Gimp"]
    my7Shifts = ["Virtual-Box", "Wine"]
    my8Shifts = ["mplayer2"]
    my9Shifts = ["Amarok"]
    my10Shifts = ["Ekiga","Skype","Pidgin"]

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

myWorkspaces    = ["1:emacs","2:www","3","4","5","6","7","8","9-virt","10-im","11-irc","12-tor"]
myNormalBorderColor  = "#151515"
myFocusedBorderColor = "#ffff00"

myKeys host dzen = myKeymap host (myConfig host dzen)
myKeymap host conf =
  -- mod-[1..],       Switch to workspace N
  -- mod-shift-[1..], Move client to workspace N
  -- mod-ctrl-[1..],  Switch to workspace N on other screen
  [ ("M-" ++ m ++ [k], windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) "1234567890[]"
      , (f, m) <- [ (W.greedyView, "")
                  , (W.shift, "S-")]
  ]
  ++
  -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
  [("M-" ++ m ++ [k], screenWorkspace sc >>= flip whenJust (windows . f))
      | (k, sc) <- zip "we" [0..]
      , (f, m) <- [(W.view, ""), (W.shift, "S-")]]
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

  , ("M-<Return>",  spawn $ myTerm host)
  , ("M-x e",       spawn "emacsclient -c -a emacs")

  , ("M-x c",       spawn "exe=`dmenu_path | dmenu -b -nb black -nf grey` && eval \"exec $exe\"")
  , ("M-x f",       spawn "firefox")
  , ("M-x y",       spawn "conkeror")
  , ("M-x l",       spawn "xscreensaver-command -lock")
  , ("M-x o",       spawn "libreoffice")
  , ("M-x m",       spawn "thunderbird")
  , ("M-x p",       spawn "pidgin")
  , ("M-x s",       spawn "skype")
  , ("M-x g",       spawn "steam")
  , ("M-x r",       runOrRaisePrompt myXPConfig)
  -- , ("M-x q",       pwPaster 13 "@1a" myXPConfig)
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
   , ("M-S-g", AL.launchApp myXPConfig "webpass -old")
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
     NS "term"  (term ++ "-name scratchpad -e zsh") (resource =? "scratchpad") termFloat,
     NS "mixer" (term ++ "-e alsamixer") (title =? "alsamixer") mySPFloat,
     NS "music" (term ++ "-e ncmpcpp") (title =? "ncmpcpp") mySPFloat
 ] where
     term = "rxvt -tr -sh 15 "
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
    -- layoutHints $ -- Why was this enabled?  Could it have something to od
    --               -- with scrolling in emacs?  Re-enable this if problems
    --               -- crop up.   -- skangas @ 2016-04-10
    onWorkspace "6" gimpLayout $
    onWorkspace "8" (noBorders Simplest) $
    tabbed shrinkText tabTheme ||| ThreeCol 1 (3/100) (1/3) ||| Full
--  tabbed shrinkText tabTheme ||| TwoPane (3/100) (1/2) ||| ThreeCol 1 (3/100) (1/3) ||| Full
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
    { ppCurrent = activeCorner . dzenColor myNormalFGColor myFocusedBGColor . dropIx
    , ppVisible = inactiveCorner . dzenColor myNormalFGColor myFocusedBGColor . dropIx
    , ppHidden = inactiveCorner . dropIx
    , ppHiddenNoWindows = \wsId -> ""
    , ppUrgent = dzenColor myUrgentFGColor myNormalBGColor. wrap (icon "corner.xbm") "" . dropIx
    , ppSep = "   "
    , ppWsSep = "  "
    , ppTitle = dzenColor myNormalFGColor "" . wrap ">       " ""
    , ppLayout = dzenColor myNormalFGColor "" .
        (\x -> case x of
        "Full" -> dzenColor myIconFGColor "" $ icon "layout-full.xbm"
        "Tabbed Simplest" -> dzenColor myIconFGColor "" $ icon "layout-full.xbm"
        "ThreeCol" -> dzenColor myIconFGColor "" $ icon "layout-threecol.xbm"
        _ -> x
        )
    , ppOutput = hPutStrLn h
    }
    where
      -- shorthands:
      activeCorner = wrap (dzenColor "#FFFFFF" myFocusedBGColor (icon "corner.xbm")) ""
      inactiveCorner = wrap (dzenColor myNormalFGColor myNormalBGColor (icon "corner.xbm")) ""
      icon i = "^i(" ++ myIconDir ++ "/" ++ i ++ ")"
      -- remove number in front of name:
      dropIx id
        | ':' `elem` id = split_ ':' id
        | id == "NSP"   = "" -- scratchpad
        | otherwise     = id
        where split_ c = drop 1 . dropWhile (/= c)

-- My password extension -------------------------------------------------------

-- ## cabal is horrid
-- sudo apt-get install xclip libghc-base64-bytestring-dev
-- cabal update
-- cabal install sha base64-bytestring

{--
mkPass :: [Char] -> Int -> [Char] -> [Char] -> [Char]
mkPass master num end site = trimWS $ append end $ take num $ enc master
  where
    enc master = sha1_base64 $ (trimWS master) ++ ":" ++ site
    append = flip (++)
    trimWS :: [Char] -> [Char]
    trimWS = f . f
      where
        f = reverse . dropWhile isSpace
    sha1_base64 :: [Char] -> [Char]
    sha1_base64 = S.unpack . encode . S.pack . L.unpack . bytestringDigest . sha1 . L.pack

readMasterPassword :: IO [Char]
readMasterPassword = do
  home <- getEnv "HOME"
  pass <- readFile (home ++ "/bin/.webpass")
  return pass

-- Xmonad stuff below
data PWPrompt = PWPrompt

instance XPrompt PWPrompt where
  showXPrompt PWPrompt = "Site: "

pwPrompt :: Int -> [Char] -> XPConfig -> X ()
pwPrompt num end c = do
  master <- liftIO readMasterPassword
  mkXPrompt PWPrompt c (mkComplFunFromList []) (toclip . mkPass master num end)

toclip :: String -> X ()
-- toclip s = Paste.pasteString s
toclip s = spawn $ "echo '" ++ s ++ "' | xclip"

-- TODO: Fix this below

readCurPage :: IO [Char]
readCurPage = do
  home <- getEnv "HOME"
  url <- readProcess (home ++ "/bin/getffurl.py") [] []
  return url

pwPaster :: Int -> [Char] -> XPConfig -> X ()
pwPaster num end c = do
  master <- liftIO readMasterPassword
  site <- liftIO readCurPage
  -- inget händer efter detta
  toclip $ mkPass master num end site
--}
