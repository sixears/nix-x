{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -W -Wall -fhelpful-errors #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE UnicodeSyntax       #-}

----------------------------------------------------------------------
--                        to test this, run                         --
-- /nix/store/q*-ghc-*-packages/bin/ghci -Wall ~/.xmonad/xmonad.hs  --
--                                                                  --
--                             or maybe                             --
--                ghci -Wall ~/nix/x/pkgs/xmonad.hs                 --
----------------------------------------------------------------------

import Prelude         ( show )
import XMonad.StackSet qualified as W

-- like shadowned names, deprecated functions and unused values.
-- remove it when you can.

import Prelude ()

-- base --------------------------------

import Control.Applicative    ( pure )
import Control.Monad          ( return, unless, (>>), (>>=) )
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
                Tall(Tall), X, handleEventHook, kill, layoutHook, logHook,
                manageHook, mod4Mask, modMask, screenWorkspace,
                sendMessage, shiftMask, spawn, terminal, windows,
                withFocused, workspaces, xK_0, xK_9, xK_Down, xK_F1,
                xK_Return, xK_Tab, xK_Up, xK_comma, xK_e, xK_equal,
                xK_k, xK_minus, xK_period,
                xK_q, xK_r, xK_slash, xK_space, xK_w, xmonad,
                (.|.), (<+>), (|||) )

-- xmonad-contrib ----------------------

import XMonad.Core                ( XConfig(XConfig), whenJust )
import XMonad.Hooks.DynamicLog    ( dynamicLogWithPP, ppOutput, ppTitle,
                                    shorten, xmobarColor, xmobarPP )
import XMonad.Hooks.ManageDocks   ( avoidStruts, docks, manageDocks )
import XMonad.Layout.NoBorders    ( smartBorders )
import XMonad.Layout.ThreeColumns ( ThreeCol(ThreeCol) )
import XMonad.Prompt              ()
import XMonad.Prompt.Input        ( inputPrompt, (?+) )
import XMonad.Util.EZConfig       ( mkNamedKeymap )
import XMonad.Util.NamedActions   ( NamedAction, addDescrKeys', addName, noName
                                  , sendMessage', separator, subtitle, xMessage )
import XMonad.Util.Run            ( safeSpawn, spawnPipe, unsafeSpawn )

--------------------------------------------------------------------------------

type 𝕊 = String

data Context = Context { _name :: 𝕊
                         {- a comment  -}
                       , _home :: 𝕊
                       }

----------------------- executables ------------------------

alacritty_exe ∷ FilePath
alacritty_exe = "__alacritty_exe__"

byobu_exe ∷ FilePath
byobu_exe = "__byobu_exe__"

pactl_exe ∷ FilePath
pactl_exe = "__pactl_exe__"

touchpad_exe ∷ FilePath
touchpad_exe = "__touchpad_exe__"

xbacklight_exe ∷ FilePath
xbacklight_exe = "__xbacklight_exe__"

xmonad_exe ∷ FilePath
xmonad_exe = "__xmonad_exe__"

xrandr_exe ∷ FilePath
xrandr_exe = "__xrandr_exe__"

xscreensaver_command_exe ∷ FilePath
xscreensaver_command_exe = "__xscreensaver_command_exe__"

------------------------------------------------------------

(↣) ∷ 𝕊 → X () → NamedAction
(↣) = addName

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

warn ∷ MonadIO μ ⇒ 𝕊 → μ ()
warn = liftIO ∘ hPutStrLn stderr

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
exec exe args = do
  warn $ intercalate " " ("CMD>" : exe : args)
  safeSpawn exe args

xmonad_do ∷ MonadIO μ ⇒ 𝕊 → μ ()
xmonad_do = exec xmonad_exe ∘ pure

xmonad_recompile ∷ MonadIO μ ⇒ μ ()
xmonad_recompile = xmonad_do "--recompile"

xmonad_restart ∷ MonadIO μ ⇒ μ ()
xmonad_restart = xmonad_do "--restart"

xmonad_recompile_and_restart ∷ MonadIO μ ⇒ μ ()
xmonad_recompile_and_restart = xmonad_recompile >> xmonad_restart

pactl      ∷ MonadIO μ ⇒ [𝕊] → μ ()
pactl args = exec pactl_exe args

pactl_vol ∷ MonadIO μ ⇒ 𝕊 → μ ()
pactl_vol x = pactl ["set-sink-volume", "@DEFAULT_SINK@", x]

pactl_mute ∷ MonadIO μ ⇒ μ ()
pactl_mute  = pactl ["set-sink-mute", "@DEFAULT_SINK@", "toggle" ]

touchpad ∷ MonadIO μ ⇒ [𝕊] → μ ()
touchpad args = exec touchpad_exe args

touchpad_toggle ∷ MonadIO μ ⇒ μ ()
touchpad_toggle = touchpad ("toggle" : [])

touchpad_toggle_x ∷ MonadIO μ ⇒ 𝕊 → μ ()
touchpad_toggle_x x = touchpad ("toggle" : [x])

trackpoint_toggle ∷ MonadIO μ ⇒ μ ()
trackpoint_toggle = touchpad_toggle_x "--trackpoint"

touchscreen_toggle ∷ MonadIO μ ⇒ μ ()
touchscreen_toggle = touchpad_toggle_x "--touchscreen"

xrandr ∷ MonadIO μ ⇒ [𝕊] → μ ()
xrandr args = exec xrandr_exe args

xrandr_orientation ∷ MonadIO μ ⇒ 𝕊 → μ ()
xrandr_orientation o = xrandr ["--orientation", o ]

xbacklight ∷ MonadIO μ ⇒ [𝕊] → μ ()
xbacklight args = exec xbacklight_exe args

xscreensaver_cmd ∷ MonadIO μ ⇒ [𝕊] → μ ()
xscreensaver_cmd args =
  exec xscreensaver_command_exe args

xscreensaver_lock ∷ MonadIO μ ⇒ μ ()
xscreensaver_lock = xscreensaver_cmd ["-lock"]

alacritty ∷ MonadIO μ ⇒ Context → 𝕊 → μ ()
alacritty ctxt x = do
  let args = [ "--config-file", config_file , "--command", byobu_exe ]
           <> if "" == x then [ "new" ] else [ "new", "-A", "-t", x ]
      config_file = _home ctxt </> "/rc/alacritty/config.yml"
  exec alacritty_exe args

alacritty_session ∷ Context → X ()
alacritty_session ctxt = inputPrompt def "session" ?+ (alacritty ctxt)

-- | A version of the default keys from the default configuration, but with
-- 'NamedAction'  instead of @X ()@
defaultKeysDescr ∷ Context → XConfig ω → [((KeyMask, KeySym), NamedAction)]
defaultKeysDescr ctxt conf@XConfig{XMonad.modMask = modm} =
    [ subtitle "launching and killing programs"
--    , ((modm .|. shiftMask, xK_Return),
--       addName "Launch Terminal" $ spawn $ XMonad.terminal conf) -- %! Launch terminal
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
    , ((modm              , xK_q     ), "Restart" ↣ xmonad_recompile_and_restart) -- %! Restart xmonad
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
   mkNamedKeymap conf
     [ {- Keys:

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

       ("M-S-<Return>", "Launch Terminal" ↣ spawn (XMonad.terminal conf))

     , ("M-\\", "lock screen" `addName` xscreensaver_lock)
     , ("M--", "shrink master area" `addName` sendMessage Shrink)
       -- M-+ doesn't work, = is the same key but without the shift
     , ("M-=", "expand master area" `addName` sendMessage Expand)
     , ("M-S-<Space>", "alacritty session"  ↣ alacritty_session ctxt)

     , ("<XF86AudioMute>"        , "volume mute" ↣ pactl_mute)
     , ("<XF86AudioRaiseVolume>" , "volume up" ↣ pactl_vol "+1.5%")
     , ("<XF86AudioLowerVolume>" , "volume down" ↣ pactl_vol "-1.5%")
     , ("<XF86MonBrightnessUp>"  , "backlight up" ↣ xbacklight ["-inc", "5"])
     , ("<XF86MonBrightnessDown>", "backlight down" ↣ xbacklight ["-dec", "5"])
       -- for lenovo yoga 260
     , ("<XF86Tools>"            , "screen normal" ↣ xrandr_orientation "normal")
     , ("<XF86Search>"           ,
        "screen clockwise" ↣ xrandr_orientation "right")
     , ("<XF86LaunchA>"          ,
        "screen anti-clockwise" ↣ xrandr_orientation "left")
     , ("<XF86Explorer>"         ,
        "screen invert" ↣ xrandr_orientation "inverted")
     , ("<XF86TouchpadToggle>"   , "toggle touchpad" ↣ trackpoint_toggle)
     -- (F4)/Mic on Lenovo Yoga 260
     , ("<XF86AudioMicMute>"     ,
        "toggle track pointer (nipple)" ↣ trackpoint_toggle)
     -- (F5)/Play on Dell XPS 9315
     , ("<XF86AudioPlay>", "toggle touchpad" `addName` touchpad_toggle)
     -- (F7)/Projector on Lenovo Yoga 260
     , ("<XF86Display>"          , "toggle touchpad" ↣ touchpad_toggle)
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
  let base_config = def { modMask         = mod4Mask
                        , manageHook      = manageDocks <+> manageHook def
                        , layoutHook      = avoidStruts $ smartBorders myLayout
                        , handleEventHook = handleEventHook def
                        , logHook         = myLogHook xmproc
                        , terminal        = urxvt
                        , workspaces      = [ "0", "1", "2", "3", "4"
                                            , "5", "6", "7", "8", "9" ]
                        }
  -- M-F1 (Win F1): show keylisting
  xmonad ∘ docks . addDescrKeys' ((mod4Mask, xK_F1), xMessage) (defaultKeysDescr $ Context name home) $ base_config

-- that's all, folks! ----------------------------------------------------------
