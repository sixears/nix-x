{ pkgs, byobu, touchpad }: pkgs.writeTextDir "share/xmonad.hs" ''
{-# OPTIONS_GHC -W -Wall -fhelpful-errors #-}
{-# LANGUAGE NoImplicitPrelude            #-}
{-# LANGUAGE UnicodeSyntax                #-}

-- -Wno-deprecations used to stop it failing on warning things
-- like shadowned names, deprecated functions and unused values.
-- remove it when you can.

import Prelude ()

-- base --------------------------------

import Control.Monad  ( (>>=), return, unless )
import Control.Monad.IO.Class  ( MonadIO, liftIO  )
import Data.Eq        ( (==) )
import Data.Function  ( (.), ($), flip )
import Data.List      ( intercalate )
import Data.Monoid    ( (<>) )
import Data.String    ( String, unwords )
import GHC.Num        ( (+) )
import GHC.Real       ( (/) )
import System.IO      ( FilePath, Handle, IO, hPutStrLn, stderr )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-default ------------------------

import Data.Default  ( def )

-- directory ---------------------------

import System.Directory  ( getHomeDirectory )

-- unix --------------------------------

import System.Posix.Directory  ( createDirectory )
import System.Posix.Files      ( createNamedPipe, fileExist, ownerExecuteMode
                               , ownerReadMode, ownerWriteMode )
import System.Posix.User       ( getEffectiveUserName )

-- xmonad ------------------------------

import XMonad  ( Choose, Full( Full ), Mirror( Mirror )
               , Resize( Expand, Shrink ), Tall( Tall ), X
               , (<+>), (|||)
               , handleEventHook, layoutHook, logHook, manageHook
               , modMask, mod4Mask, sendMessage, spawn, terminal, windows
               , workspaces, xmonad
               )

import XMonad.StackSet  ( greedyView, shift )

-- xmonad-contrib ----------------------

import XMonad.Layout.NoBorders    ( smartBorders )
import XMonad.Layout.ThreeColumns ( ThreeCol( ThreeCol ) )
import XMonad.Hooks.ManageDocks   ( avoidStruts, docks, docksEventHook, manageDocks )
import XMonad.Hooks.DynamicLog    ( dynamicLogWithPP, ppOutput, ppTitle, shorten
                                  , xmobarColor, xmobarPP )
import XMonad.Prompt              ( )
import XMonad.Prompt.Input        ( inputPrompt, (?+) )
import XMonad.Util.EZConfig       ( additionalKeysP )
import XMonad.Util.Run            ( spawnPipe, unsafeSpawn )

--------------------------------------------------------------------------------

----------------------------------------------------------------------
--                        to test this, run                         --
-- /nix/store/q*-ghc-*-packages/bin/ghci -Wall ~/.xmonad/xmonad.hs  --
----------------------------------------------------------------------

type 𝕊 = String

data Context = Context { name_ ∷ 𝕊, home_ ∷ 𝕊 }

defaultLayout :: Choose Tall (Choose (Mirror Tall) Full) a

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

threeColLayout :: ThreeCol a
threeColLayout = ThreeCol 1 (3/100) (3/7)

--------------------

myLayout :: Choose (Choose Tall (Choose (Mirror Tall) Full)) ThreeCol a
myLayout = defaultLayout ||| threeColLayout

----------------------------------------

-- whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
-- whenJust mg f = maybe (pure ()) f mg

----------------------------------------

tmpdir :: FilePath -> FilePath
tmpdir name = "/tmp" </> name

i3pipe :: FilePath -> FilePath
i3pipe name = tmpdir name </> "i3status"

-- | create the given fifo (perms 0600) if the file doesn't already exist
ensureFifo :: FilePath -> IO ()
ensureFifo fn = fileExist fn >>= flip unless mkFifo
  where mkFifo = createNamedPipe fn (ownerReadMode + ownerWriteMode)

----------------------------------------

(</>) :: FilePath -> FilePath -> FilePath
pfx </> sfx = pfx <> "/" <> sfx

swBin ∷ FilePath
swBin = "/run/current-system/sw/bin"

urxvt :: FilePath
urxvt = swBin </> "urxvt"

xmobarrc :: FilePath
xmobarrc = "/etc/xmobarrc"

i3status :: FilePath
i3status = swBin </> "i3status"

i3statusrc :: FilePath
i3statusrc = "/etc/i3status"

----------------------------------------

myLogHook :: Handle -> X ()
myLogHook process =
  dynamicLogWithPP xmobarPP
                   { ppOutput = hPutStrLn process
                   , ppTitle  = xmobarColor "green" "" . shorten 50
                   }

----------------------------------------

exec ∷ MonadIO μ ⇒ 𝕊 → [𝕊] → μ ()
exec exe args = spawn $ intercalate " " (exe : args)

pactl_exe      ∷ MonadIO μ ⇒ [𝕊] → μ ()
pactl_exe args = exec ("${pkgs.pulseaudio}/bin/pactl") args

pactl_vol ∷ MonadIO μ ⇒ 𝕊 → μ ()
pactl_vol x = pactl_exe ["set-sink-volume", "@DEFAULT_SINK@", x]

pactl_mute ∷ MonadIO μ ⇒ μ ()
pactl_mute  = pactl_exe ["set-sink-mute", "@DEFAULT_SINK@", "toggle" ]

touchpad ∷ MonadIO μ ⇒ [𝕊] → μ ()
touchpad args = let touchpad_exe = "${touchpad}/bin/touchpad"
                in  exec touchpad_exe args

touchpad_toggle ∷ MonadIO μ ⇒ [𝕊] → μ ()
touchpad_toggle _ = touchpad ("toggle" : [])

xrandr ∷ MonadIO μ ⇒ [𝕊] → μ ()
xrandr args = let xrandr_exe = "${pkgs.xorg.xrandr}/bin/xrandr"
              in  exec xrandr_exe args

xrandr_orientation ∷ MonadIO μ ⇒ 𝕊 → μ ()
xrandr_orientation o = xrandr ["--orientation", o ]


xbacklight ∷ MonadIO μ ⇒ [𝕊] → μ ()
xbacklight args = exec "${pkgs.acpilight}/bin/xbacklight" args

xscreensaver_cmd ∷ MonadIO μ ⇒ [𝕊] → μ ()
xscreensaver_cmd args =
  let xscreensaver_command = "${pkgs.xscreensaver}/bin/xscreensaver-command"
  in  exec xscreensaver_command args

xscreensaver_lock ∷ MonadIO μ ⇒ μ ()
xscreensaver_lock = xscreensaver_cmd ["-lock"]

byobu_exe ∷ 𝕊
byobu_exe = "${byobu}/bin/byobu"

alacritty ∷ MonadIO μ ⇒ Context → 𝕊 → μ ()
alacritty ctxt x = let alacritty_exe = "${pkgs.alacritty}/bin/alacritty"
                       args          = if "" == x
                                       then [ "new" ]
                                       else [ "new", "-A", "-t", x ]
                   in do
                     liftIO $ hPutStrLn stderr $ intercalate " "  ("CMD>" : alacritty_exe : [ "--config-file", (home_ ctxt </> "/rc/alacritty/config.yml" )
                                           , "--command", byobu_exe ] <> args)
  -- byobu particularly relies on the path to access its own utility programs :-(
  --                   liftIO $ setEnv "PATH" "/home/martyn/.nix-profiles/default--/bin/:/run/current-system/sw/bin/" True
                     exec "/usr/bin/env" []
                     exec alacritty_exe ([ "--config-file", (home_ ctxt </> "/rc/alacritty/config.yml" )
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

main :: IO ()
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
  xmproc <- spawnPipe $ unwords ("${pkgs.xmobar}/bin/xmobar" : xmobarArgs)
  xmonad ∘ docks $ additionalKeysP (def { modMask = mod4Mask
                                , manageHook = manageDocks <+> manageHook def
                                , layoutHook = avoidStruts $ smartBorders myLayout
                                  -- this is essential, and docksEventHook must be
                                  -- last, for the xmobar to position correctly wrt
                                  -- other windows
                                , handleEventHook =
                                    handleEventHook def -- <+> docksEventHook
                                , logHook    = myLogHook xmproc
                                , terminal   = urxvt
                                , workspaces = [ "1", "2", "3", "4", "5"
                                               , "6", "7", "8", "9", "0" ]
                                })
                           (keys $ Context { name_ = name, home_ = home })

-- that's all, folks! ----------------------------------------------------------
''

# Local Variables:
# mode: haskell
# sh-basic-offset: 2
# End:
