import XMonad
import XMonad.Config.Xfce
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Util.CustomKeys

-- Notes:
-- /usr/include/X11/keysymdef.h has keydefs

main = xmonad $ ewmhFullscreen xfceConfig {
  layoutHook = avoidStruts (smartBorders (Tall 1 (3/100) (1/2)) ||| noBorders Full),
  borderWidth        = 2,
  normalBorderColor  = "#000000",
  focusedBorderColor = "#9400d3",
  keys = customKeys delkeys inskeys }

  where
      delkeys :: XConfig l -> [(KeyMask, KeySym)]
      delkeys XConfig {modMask = modm} =
        [ (modm .|. m, k) | m <- [0, shiftMask], k <-
            [xK_c, xK_p, xK_n, xK_m, xK_slash, xK_period, xK_comma] ]

      inskeys :: XConfig l -> [((KeyMask, KeySym), X ())]
      inskeys conf@(XConfig {modMask = modm}) =
          [ ((mod1Mask,               xK_q  ), spawn "xmonad --restart")
          , ((mod1Mask .|. shiftMask, xK_e  ), spawn "emacsclient --create-frame")

            -- Close the focused window
          , ((mod1Mask,               xK_F4 ), kill)

            -- Increment the number of windows in the master area
          , ((mod1Mask              , xK_equal ), sendMessage (IncMasterN 1))

            -- Deincrement the number of windows in the master area
          , ((mod1Mask              , xK_minus), sendMessage (IncMasterN (-1)))


          ,((mod1Mask, xK_F10     ), spawn "xfce4-panel --quit")
          ,((mod1Mask, xK_F11     ), spawn "xfce4-panel &")
          ]
