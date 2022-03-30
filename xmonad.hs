{-# OPTIONS_GHC -Wno-deprecations #-}

-- Notes:
-- /usr/include/X11/keysymdef.h has keydefs

import XMonad
import XMonad.Config.Xfce
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Util.CustomKeys

main = xmonad xfceConfig {
  layoutHook =
      -- Has to be wrapped in avoidStruts to work nicely with xfce panel
      avoidStruts(smartBorders (Tall 1 (3/100) (1/2)) ||| noBorders Full),

    startupHook        = ewmhDesktopsStartup,
    borderWidth        = 2,
    normalBorderColor  = "#696969",
    focusedBorderColor = "#9400d3",
    keys = customKeys delkeys inskeys }

  where
      delkeys :: XConfig l -> [(KeyMask, KeySym)]
      delkeys XConfig {modMask = modm} =
        [ (modm .|. m, k) | m <- [0, shiftMask], k <-
            [xK_c, xK_p, xK_n, xK_m, xK_slash, xK_period, xK_comma] ]

      inskeys :: XConfig l -> [((KeyMask, KeySym), X ())]
      inskeys conf@(XConfig {modMask = modm}) =
          [
            ((mod1Mask,             xK_q  ), spawn "xmonad --restart && xfce4-panel --restart")
          , ((mod1Mask,             xK_F4 ), kill) -- %! Close the focused window
          ]
