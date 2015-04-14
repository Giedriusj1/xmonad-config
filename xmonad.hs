import System.IO
import System.IO.Unsafe (unsafePerformIO) -- needed to get env variable
import System.Exit
import System.Environment
import XMonad
import XMonad.Config.Xfce
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Fullscreen
import XMonad.Layout.Maximize
import XMonad.Layout.NoBorders
import XMonad.Util.Run(spawnPipe, safeSpawn)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.List
import XMonad.Actions.PhysicalScreens

-- import XMonad.Prompt
-- import XMonad.Prompt.Shell
-- import Data.Bits

------------------------------------------------------------------------
-- Window rules
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ resource  =? "desktop_window" --> doIgnore
    , className =? "Galculator"     --> doFloat
    , className =? "Steam"          --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "gpicview"       --> doFloat
    , className =? "MPlayer"        --> doFloat
    , className =? "stalonetray"    --> doIgnore
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)

    ]


myLayout = avoidStruts (
                        tiled ||| noBorders Full)
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
 
    -- The default number of windows in the master pane
    nmaster = 1
 
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
 
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100
              
------------------------------------------------------------------------

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  -- Start a terminal.  Terminal to start is specified by myTerminal variable.
  [ ((modMask .|. shiftMask, xK_Return),
     spawn $ XMonad.terminal conf)

  , ((modMask,               xK_p     ), spawn "dmenu_run -i -l 20")

  -- Launch KeepassC
  , ((modMask .|. controlMask .|. shiftMask, xK_p),
     spawn "gnome-terminal -x sh -c keepassc")

  -- Close focused window.
  , ((modMask, xK_c),
     kill)

  -- Maximize the focused window temporarily
  , ((modMask, xK_m), withFocused $ sendMessage . maximizeRestore)

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space),
     sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space),
     setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size.
  , ((modMask, xK_n),
     refresh)

  -- Move focus to the next window.
  , ((modMask, xK_Tab),
     windows W.focusDown)

  -- Move focus to the next window.
  , ((modMask, xK_j),
     windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_k),
     windows W.focusUp  )

  -- Move focus to the master window.
  , ((modMask, xK_m),
     windows W.focusMaster  )

  -- Swap the focused window and the master window.
  , ((modMask, xK_Return),
     windows W.swapMaster)

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j),
     windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k),
     windows W.swapUp    )

  -- Shrink the master area.
  , ((modMask, xK_h),
     sendMessage Shrink)

  -- Expand the master area.
  , ((modMask, xK_l),
     sendMessage Expand)

  -- Push window back into tiling.
  , ((modMask, xK_t),
     withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  , ((modMask, xK_comma),
     sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((modMask, xK_period),
     sendMessage (IncMasterN (-1)))

  -- Toggle the status bar gap.
  , ((modMask, xK_b),
     sendMessage ToggleStruts)

     -- Restart xmonad
  , ((modMask .|. shiftMask     , xK_q     ), spawn "xmonad --recompile; xmonad --restart")


  , ((modMask, xK_a), onPrevNeighbour W.view)
 , ((modMask, xK_o), onNextNeighbour W.view)
 , ((modMask .|. shiftMask, xK_a), onPrevNeighbour W.shift)
 , ((modMask .|. shiftMask, xK_o), onNextNeighbour W.shift)
 
  ]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++

  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [  xK_q, xK_w, xK_e] [0..2]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
          
------------------------------------------------------------------------
-- Mouse bindings

button6 = 6 :: Button
button7 = 7 :: Button
button8 = 8 :: Button
button9 = 9 :: Button

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))
    
    
  -- mod-button2, Raise the window to the top of the stack
  , ((modMask, button2),
     (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))

  , ((0, button6),
     (windows . W.sink))
    
  , ((0, button7),
     (\w -> focus w >> mouseMoveWindow w))
    
  -- mod-button3, Set the window to floating mode and resize by dragging
    , ((0, button8),
       (\w -> focus w >> mouseResizeWindow w))
  ]



-- Will check env variable for correct mod mask.
-- echo "export XMONADMODKEY=4" > /etc/profile.d/xmonad.sh

findModMask
    | envKey == "1" = mod1Mask
    | envKey == "2" = mod2Mask
    | envKey == "3" = mod3Mask
    | envKey == "4" = mod4Mask
    | envKey == "5" = mod5Mask
    | otherwise = mod4Mask      -- win key as default?
    where envKey = unsafePerformIO $ do
                     e <- getEnv "XMONADMODKEY"
                     return e

main :: IO ()
main = xmonad defaults

defaults = ewmh defaultConfig {
    -- simple stuff
    terminal           = "/usr/bin/gnome-terminal --hide-menubar",
    focusFollowsMouse  = True,
    borderWidth        = 2,
    modMask = findModMask,
    workspaces         = ["web","emacs"] ++ map show [3..9],
    normalBorderColor  = "#000000",
    focusedBorderColor =  "#Ff8c00",

    -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    layoutHook         = smartBorders $ myLayout,
    manageHook         = myManageHook <+> manageDocks,
    startupHook        = ewmhDesktopsStartup
}
