{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -W -Wall -fhelpful-errors #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE UnicodeSyntax       #-}

import Prelude         ( show )
import XMonad.StackSet qualified as W

-- like shadowned names, deprecated functions and unused values.
-- remove it when you can.

import Prelude ()

-- base --------------------------------

import Control.Monad          ( return, unless, (>>=) )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Eq                ( (==) )
import Data.Function          ( flip, ($), (.) )
import Data.List              ( intercalate, zip, (++) )
import Data.Monoid            ( (<>) )
import Data.String            ( String, unwords )
import GHC.Num                ( (+) )
import GHC.Real               ( (/) )
import System.IO              ( FilePath, Handle, IO, hPutStrLn, stderr )

-- base-unicode-symbols ----------------

import Data.Function.Unicode ( (∘) )

-- data-default ------------------------

import Data.Default ( def )

-- directory ---------------------------

import System.Directory ( getHomeDirectory )

-- unix --------------------------------

import System.Posix.Directory ( createDirectory )
import System.Posix.Files     ( createNamedPipe, fileExist, ownerExecuteMode,
                                ownerReadMode, ownerWriteMode )
import System.Posix.User      ( getEffectiveUserName )

-- X11 ---------------------------------

import Graphics.X11.Types ( KeyMask, KeySym )

-- xmonad ------------------------------

import XMonad ( ChangeLayout(NextLayout), Choose, Full(Full),
                IncMasterN(IncMasterN), Mirror(Mirror), Resize(Expand, Shrink),
                Tall(Tall), X, handleEventHook, io, kill, layoutHook, logHook,
                manageHook, mod4Mask, modMask, refresh, screenWorkspace,
                sendMessage, setLayout, shiftMask, spawn, terminal, windows,
                withFocused, workspaces, xK_0, xK_9, xK_Down, xK_F1, xK_F2,
                xK_Return, xK_Tab, xK_Up, xK_c, xK_comma, xK_e, xK_equal, xK_h,
                xK_j, xK_k, xK_l, xK_m, xK_minus, xK_n, xK_p, xK_period,
                xK_plus, xK_q, xK_r, xK_slash, xK_space, xK_t, xK_w, xmonad,
                (.|.), (<+>), (|||) )

import XMonad.StackSet ( greedyView, shift )

-- xmonad-contrib ----------------------

import XMonad.Core                ( Layout, XConfig(XConfig), whenJust )
import XMonad.Hooks.DynamicLog    ( dynamicLogWithPP, ppOutput, ppTitle,
                                    shorten, xmobarColor, xmobarPP )
import XMonad.Hooks.ManageDocks   ( avoidStruts, docks, manageDocks )
import XMonad.Layout.NoBorders    ( smartBorders )
import XMonad.Layout.ThreeColumns ( ThreeCol(ThreeCol) )
import XMonad.Prompt              ()
import XMonad.Prompt.Input        ( inputPrompt, (?+) )
import XMonad.Util.EZConfig       ( additionalKeysP, mkNamedKeymap,
                                    removeKeysP )
import XMonad.Util.NamedActions   ( HasName, NamedAction, addDescrKeys,
                                    addDescrKeys', addName, sendMessage',
                                    separator, subtitle, xMessage, (^++^) )
import XMonad.Util.Run            ( spawnPipe, unsafeSpawn )

--------------------------------------------------------------------------------

----------------------------------------------------------------------
--                        to test this, run                         --
-- /nix/store/q*-ghc-*-packages/bin/ghci -Wall ~/.xmonad/xmonad.hs  --
----------------------------------------------------------------------

type 𝕊 = String

data Context = Context { name_ :: 𝕊
                         {- a comment  -}
                       , home_ :: 𝕊
                       }

defaultLayout ∷ Choose Tall (Choose (Mirror Tall) Full) a

defaultLayout = tiled ||| Mirror tiled ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

--------------------

--   The first argument specifies how many windows initially appear in the main
-- window.
--   The second argument argument specifies the amount to resize while resizing
-- and the third argument specifies the initial size of the columns.
--   A positive size designates the fraction of the screen that the main window
-- should occupy, but if the size is negative the absolute value designates the
-- fraction a slave column should occupy. If both slave columns are visible,
-- they always occupy the same amount of space.

threeColLayout ∷ ThreeCol a
threeColLayout = ThreeCol 1 (3/100) (3/7)

--------------------

myLayout ∷ Choose (Choose Tall (Choose (Mirror Tall) Full)) ThreeCol a
myLayout = defaultLayout ||| threeColLayout

----------------------------------------

-- whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
-- whenJust mg f = maybe (pure ()) f mg

----------------------------------------

tmpdir ∷ FilePath → FilePath
tmpdir name = "/tmp" </> name

i3pipe ∷ FilePath → FilePath
i3pipe name = tmpdir name </> "i3status"

-- | create the given fifo (perms 0600) if the file doesn't already exist
ensureFifo ∷ FilePath → IO ()
ensureFifo fn = fileExist fn >>= flip unless mkFifo
  where mkFifo = createNamedPipe fn (ownerReadMode + ownerWriteMode)

----------------------------------------

(</>) ∷ FilePath → FilePath → FilePath
pfx </> sfx = pfx <> "/" <> sfx

swBin ∷ FilePath
swBin = "/run/current-system/sw/bin"

urxvt ∷ FilePath
urxvt = swBin </> "urxvt"

xmobarrc ∷ FilePath
xmobarrc = "/etc/xmobarrc"

i3status ∷ FilePath
i3status = swBin </> "i3status"

i3statusrc ∷ FilePath
i3statusrc = "/etc/i3status"

----------------------------------------

myLogHook ∷ Handle → X ()
myLogHook process =
  dynamicLogWithPP xmobarPP { ppOutput = hPutStrLn process, ppTitle  = xmobarColor "green" "" . shorten 50 }

----------------------------------------

exec ∷ MonadIO μ ⇒ 𝕊 → [𝕊] → μ ()
exec exe args = spawn $ intercalate " " (exe : args)

pactl      ∷ MonadIO μ ⇒ [𝕊] → μ ()
pactl args = exec "__pactl_exe__" args

pactl_vol ∷ MonadIO μ ⇒ 𝕊 → μ ()
pactl_vol x = pactl ["set-sink-volume", "@DEFAULT_SINK@", x]

pactl_mute ∷ MonadIO μ ⇒ μ ()
pactl_mute  = pactl ["set-sink-mute", "@DEFAULT_SINK@", "toggle" ]

touchpad ∷ MonadIO μ ⇒ [𝕊] → μ ()
touchpad args = let touchpad_exe = "__touchpad_exe__"
                in  exec touchpad_exe args

touchpad_toggle ∷ MonadIO μ ⇒ [𝕊] → μ ()
touchpad_toggle _ = touchpad ("toggle" : [])

xrandr ∷ MonadIO μ ⇒ [𝕊] → μ ()
xrandr args = exec "__xrandr_exe__ " args

xrandr_orientation ∷ MonadIO μ ⇒ 𝕊 → μ ()
xrandr_orientation o = xrandr ["--orientation", o ]


xbacklight ∷ MonadIO μ ⇒ [𝕊] → μ ()
xbacklight args = exec "__xbacklight_exe__" args

xscreensaver_cmd ∷ MonadIO μ ⇒ [𝕊] → μ ()
xscreensaver_cmd args =
  exec "__xscreensaver_command_exe__ " args

xscreensaver_lock ∷ MonadIO μ ⇒ μ ()
xscreensaver_lock = xscreensaver_cmd ["-lock"]

byobu_exe ∷ 𝕊
byobu_exe = "__byobu_exe__"

alacritty ∷ MonadIO μ ⇒ Context → 𝕊 → μ ()
alacritty ctxt x = let args = if "" == x
                              then [ "new" ]
                              else [ "new", "-A", "-t", x ]
                   in do
                     liftIO $ hPutStrLn stderr $ intercalate " "  ("CMD>" : "__alacritty_exe__" : [ "--config-file", (home_ ctxt </> "/rc/alacritty/config.yml" )
                                           , "--command", byobu_exe ] <> args)
  -- byobu particularly relies on the path to access its own utility programs :-(
  --                   liftIO $ setEnv "PATH" "/home/martyn/.nix-profiles/default--/bin/:/run/current-system/sw/bin/" True
                     exec "/usr/bin/env" []
                     exec "__alacritty_exe__" ([ "--config-file", (home_ ctxt </> "/rc/alacritty/config.yml" )
                                                  , "--command", byobu_exe ] <> args)

keys ∷ Context → [(String, X ())]
keys ctxt = [ ("M-0", windows $ greedyView "0")
            , ("M-S-0", windows $ shift "0")
            , ("M-\\", xscreensaver_lock)
            , ("M--", sendMessage Shrink)
            -- M-+ doesn't work, = is the same key but without the shift
            , ("M-=", sendMessage Expand)
            , ("M-S-<Space>", inputPrompt def "session" ?+ (alacritty ctxt))

            {- Keys:

               Yoga260 (dissolve)

               Mute                (F1)
               Vol-                (F2)
               Vol+                (F3)
               Bright-             (F5)
               Bright+             (F6)
               TouchPad Toggle     (F7)
               WIFI                (F8) # Hardware
               Screen Normal       (F9)
               Screen Bottom Right (F10)
               Screen Bottom Left  (F11)
               Screen Invert       (F12)
             -}

            , ("<XF86AudioMute>"        , pactl_mute)
            , ("<XF86AudioRaiseVolume>" , pactl_vol "+1.5%")
            , ("<XF86AudioLowerVolume>" , pactl_vol "-1.5%")
            , ("<XF86MonBrightnessUp>"  , xbacklight ["-inc", "5"])
            , ("<XF86MonBrightnessDown>", xbacklight ["-dec", "5"])
              -- for lenovo yoga S260
            , ("<XF86Tools>"            , xrandr_orientation "normal")
            , ("<XF86Search>"           , xrandr_orientation "right")
            , ("<XF86LaunchA>"          , xrandr_orientation "left")
            , ("<XF86Explorer>"         , xrandr_orientation "inverted")
            -- (F7)/Projector on Lenovo Yoga 260
            , ("<XF86Display>"          , touchpad_toggle [])
            , ("<XF86TouchpadToggle>"   , touchpad_toggle [])
            -- (F4)/Mic on Lenovo Yoga 260; toggle TrackPointer (nipple)
            , ("<XF86AudioMicMute>"     , touchpad_toggle [ "--trackpoint" ])
            -- (F5)/Play on Dell XPS 9315;  toggle TrackPad
            , ("<XF86AudioPlay>"        , touchpad_toggle [])
            ]

-- | A version of the default keys from the default configuration, but with
-- 'NamedAction'  instead of @X ()@
defaultKeysDescr ∷ Context → XConfig ω → [((KeyMask, KeySym), NamedAction)]
defaultKeysDescr ctxt conf@XConfig{XMonad.modMask = modm} =
    [ subtitle "launching and killing programs"
    , ((modm .|. shiftMask, xK_Return), addName "Launch Terminal" $ spawn $ XMonad.terminal conf) -- %! Launch terminal
    , ((modm .|. shiftMask, xK_k     ), addName "Close the focused window" kill) -- %! Close the focused window

    , subtitle "changing layouts"
    , ((modm,               xK_space ), sendMessage' NextLayout) -- %! Rotate through the available layout algorithms
--    , ((modm .|. shiftMask, xK_space ), addName "Reset the layout" $ setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default
--    , ((modm,               xK_n     ), addName "Refresh" refresh) -- %! Resize viewed windows to the correct size

    , separator
    , subtitle "move focus up or down the window stack"
    , ((modm,               xK_Tab   ), addName "Focus down" $ windows W.focusDown) -- %! Move focus to the next window
    , ((modm .|. shiftMask, xK_Tab   ), addName "Focus up"   $ windows W.focusUp  ) -- %! Move focus to the previous window

    , subtitle "modifying the window order"
    , ((modm,               xK_Return), addName "Swap with the master" $ windows W.swapMaster) -- %! Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_Down  ), addName "Swap down" $ windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_Up    ), addName "Swap up"   $ windows W.swapUp    ) -- %! Swap the focused window with the previous window

    , subtitle "resizing the master/slave ratio"
    , ((modm,               xK_minus ), sendMessage' Shrink) -- %! Shrink the master area
    , ((modm,               xK_equal ), sendMessage' Expand) -- %! Expand the master area

    , subtitle "floating layer support"
    , ((modm,               xK_slash  ), addName "Push floating to tiled" $ withFocused $ windows . W.sink) -- %! Push window back into tiling

    , subtitle "change the number of windows in the master area"
    , ((modm              , xK_comma ), sendMessage' (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modm              , xK_period), sendMessage' (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

    , subtitle "quit, or restart"
--    , ((modm .|. shiftMask, xK_q     ), addName "Quit" $ io exitSuccess) -- %! Quit xmonad
    , ((modm              , xK_q     ), addName "Restart" $ spawn "__xmonad_exe__ --recompile && xmonad --restart") -- %! Restart xmonad
    ]
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    ++
    subtitle "switching workspaces":
    [((m .|. modm, k), addName (n ++ i) $ windows $ f i)
        | (f, m, n) <- [(W.greedyView, 0, "Switch to workspace "), (W.shift, shiftMask, "Move client to workspace ")]
        , (i, k) <- zip (XMonad.workspaces conf) [xK_0 .. xK_9]]
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
   ++
   subtitle "switching screens" :
   [((m .|. modm, key), addName (n ++ show sc) $ screenWorkspace sc >>= flip whenJust (windows . f))
        | (f, m, n) <- [(W.view, 0, "Switch to screen number "), (W.shift, shiftMask, "Move client to screen number ")]
        , (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]]

   ++
   mkNamedKeymap conf [("<XF86AudioPlay>", "toggle touchpad" `addName` touchpad_toggle [])
                          , ("M-\\", "lock screen" `addName` xscreensaver_lock)
                          , ("M--", "shrink master area" `addName` sendMessage Shrink)
                          -- M-+ doesn't work, = is the same key but without the shift
                          , ("M-=", "expand master area" `addName` sendMessage Expand)
                          , ("M-S-<Space>", "alacritty session" `addName` (inputPrompt def "session" ?+ (alacritty ctxt)))
                        ]

main ∷ IO ()
main = do
  name <- getEffectiveUserName
  home <- getHomeDirectory

  let rwx = ownerReadMode + ownerWriteMode + ownerExecuteMode
      tmp = tmpdir name
  fileExist (tmp) >>= \ e -> unless e $ createDirectory (tmpdir name) rwx

  let i3p = i3pipe name
  -- initialize i3status
  ensureFifo i3p
  unsafeSpawn $ unwords [ i3status, "-c", i3statusrc, ">", i3p ]
  xmobarArgs <- let rc = xmobarrc
                 in fileExist rc >>= \e -> if e then return [rc] else return []
  xmproc <- spawnPipe $ unwords ("__xmobar_exe__" : xmobarArgs)
  let add_descr_keys ∷ HasName b ⇒ (XConfig Layout → [((KeyMask, KeySym), b)]) → XConfig l → XConfig l
      add_descr_keys = addDescrKeys ((mod4Mask, xK_F2), xMessage)
      add_descr_keys' ∷ XConfig l → XConfig l
      add_descr_keys' = add_descr_keys myKeys
  let base_config = def { modMask = mod4Mask
                        , manageHook = manageDocks <+> manageHook def
                        , layoutHook =
                            avoidStruts $ smartBorders myLayout
                        , handleEventHook = handleEventHook def
                        , logHook    = myLogHook xmproc
                        , terminal   = urxvt
                        , workspaces = [ "0", "1", "2", "3", "4"
                                       , "5", "6", "7", "8", "9" ]
                        }
{-
  xmonad ∘ docks . add_descr_keys' $ (additionalKeysP base_config
                                           (keys $ Context { name_ = name, home_ = home })
                                           `removeKeysP` ["M-l","M4-j"])
-}

  xmonad ∘ docks . addDescrKeys' ((mod4Mask, xK_F1), xMessage) (defaultKeysDescr $ Context name home)
   $ (additionalKeysP base_config (keys $ Context { name_ = name, home_ = home })                                         `removeKeysP` ["M-l","M4-j"])

myKeys ∷ XConfig l → [((KeyMask, KeySym), NamedAction)]
myKeys c = (subtitle "Custom Keys":) $ mkNamedKeymap c $
   [("M-x a", addName "useless message" $ spawn "xmessage foo"),
    ("M-c", sendMessage' Expand)]
    ^++^
   [("<XF86AudioPlay>", spawn "mpc toggle" :: X ()),
    ("<XF86AudioNext>", spawn "mpc next")]
{-
main = xmonad $ addDescrKeys ((mod4Mask, xK_F1), xMessage) myKeys
                   def { modMask = mod4Mask }

myKeys c = (subtitle "Custom Keys":) $ mkNamedKeymap c $
   [("M-x a", addName "useless message" $ spawn "xmessage foo"),
    ("M-c", sendMessage' Expand)]
    ^++^
   [("<XF86AudioPlay>", spawn "mpc toggle" :: X ()),
    ("<XF86AudioNext>", spawn "mpc next")]
-}

-- that's all, folks! ----------------------------------------------------------
