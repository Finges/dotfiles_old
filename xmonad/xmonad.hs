-------------------------------------------------------------------------------
-- xmonad.hs for xmonad-darcs
-- Author: Øyvind 'Mr.Elendig' Heggstad <mrelendig AT har-ikkje DOT net>
-------------------------------------------------------------------------------
-- Compiler flags --
{-# LANGUAGE NoMonomorphismRestriction #-}

-- Imports --
-- stuff
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit
import XMonad.Util.Run (safeSpawn)
import Graphics.X11.ExtraTypes.XF86

-- actions
import XMonad.Actions.GridSelect

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.InsertPosition

-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM
import XMonad.Layout.Grid

-------------------------------------------------------------------------------
-- Main --
main :: IO ()
main = xmonad =<< statusBar cmd pp kb conf
  where 
    uhook = withUrgencyHookC NoUrgencyHook urgentConfig
    cmd = "bash -c \"tee >(xmobar -x0) | xmobar -x1\""
    pp = customPP
    kb = toggleStrutsKey
    conf = uhook myConfig

-------------------------------------------------------------------------------
-- Configs --
myConfig = defaultConfig { workspaces = workspaces'
                         , modMask = modMask'
                         , borderWidth = borderWidth'
                         , normalBorderColor = normalBorderColor'
                         , focusedBorderColor = focusedBorderColor'
                         , terminal = terminal'
                         , keys = keys'
                         , layoutHook = layoutHook'
                         , manageHook = manageHook'
                         }

-------------------------------------------------------------------------------
-- Window Management --
manageHook' = composeAll [ isFullscreen             --> doFullFloat
                         , className =? "MPlayer"   --> doFloat
                         , className =? "mplayer2"  --> doFloat
                         , className =? "Gimp"      --> doFloat
                         , className =? "Vlc"       --> doFloat
                         , className =? "Pidgin"    --> doShift "4:PIDGIN"
			 , insertPosition Below Newer
			 , transience'
                         ]


-------------------------------------------------------------------------------
-- Looks --
-- bar
customPP = defaultPP { ppCurrent = xmobarColor "#859900" "" . wrap "[" "]"
                     --, ppHidden = xmobarColor "#C98F0A" ""
                     --, ppHiddenNoWindows = xmobarColor "#C9A34E" ""
                     , ppUrgent = xmobarColor "#E20048" "" . wrap "<" ">" 
                     , ppLayout = xmobarColor "#cb4b16" ""
                     , ppTitle =  xmobarColor "#859900" "" . shorten 80
                     , ppSep = xmobarColor "#268bd2" "" " | "
                     }
-- GridSelect
myGSConfig = defaultGSConfig { gs_cellwidth = 160 }

-- urgent notification
urgentConfig = UrgencyConfig { suppressWhen = Focused, remindWhen = Dont }

-- borders
borderWidth' = 1 
normalBorderColor'  = "#586E75"
focusedBorderColor' = "#93A1A1"

-- tabs 
tabTheme1 = defaultTheme { decoHeight = 16     
         , activeColor = "#0A64A4" 
         , activeBorderColor = "#0A64A4"    
         , activeTextColor = "#000000"
         , inactiveBorderColor = "#000000"   
         }

-- workspaces
workspaces' = ["1:MAIN","2:DEV","3:CHATS","4:PIDGIN","5:MEDIA","6:AUDIO","7","8","9"]

-- layouts
layoutHook' = onWorkspace "4:PIDGIN" imLayout $ standardLayouts
  where
    standardLayouts = tile ||| mtile ||| tab ||| full
        where
            rt = ResizableTall 1 (2/100) (1/2) []
            tile = renamed [Replace "TILE"] $ smartBorders rt
            mtile = renamed [Replace "MTILE"] $ smartBorders $ Mirror rt
            tab = renamed [Replace "TAB"] $ noBorders $ tabbed shrinkText tabTheme1
            full = renamed [Replace "FULL"] $ noBorders Full 

    imLayout = withIM (1/5) pidginRoster $ Grid
    
    pidginRoster = ClassName "Pidgin" `And` Role "buddy_list"

-------------------------------------------------------------------------------
-- Terminal --
terminal' = "termite"

-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
modMask' = mod1Mask

-- keys
toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask,               xK_Return), safeSpawn (XMonad.terminal conf) []) 
    , ((modMask,               xK_p     ), safeSpawn "dmenu_run" []) 
    , ((modMask .|. shiftMask, xK_p     ), safeSpawn "gmrun" [])
    , ((modMask .|. shiftMask, xK_c     ), kill)

    -- grid
    , ((modMask,               xK_g     ), goToSelected myGSConfig)

    -- layouts
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- floating layer stuff
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

    -- refresh
    , ((modMask,               xK_n     ), refresh)

    -- focus
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp)
    , ((modMask,               xK_m     ), windows W.focusMaster)

    -- swapping
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorExpand)

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-[w,e] %! switch to twinview screen 1/2
    -- mod-shift-[w,e] %! move window to screen 1/2
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-------------------------------------------------------------------------------
