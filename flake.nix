{
  description = "nix configuration for X-based things";

  inputs = {
    nixpkgs.url     = github:nixos/nixpkgs/be44bf67; # nixos-22.05 2022-10-15
    flake-utils.url = github:numtide/flake-utils/c0e246b9;
    myPkgs          = {
      url    = github:sixears/nix-pkgs/r0.0.1.3;
      inputs = { nixpkgs.follows = "nixpkgs"; };
    };
  };

  outputs = { self, nixpkgs, flake-utils, myPkgs }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs    = nixpkgs.legacyPackages.${system};
        my-pkgs = myPkgs.packages.${system};
        xmonad  = pkgs.xmonad-with-packages.override {
          packages = p: with p; [ base-unicode-symbols data-default
                                  xmonad-contrib ];
        };
      in
        rec {
          packages = flake-utils.lib.flattenTree (with pkgs; {
            inherit alacritty acpilight arandr i3status xmonad xscreensaver;
            inherit (xorg) xdpyinfo;

            urxvt = rxvt_unicode-with-plugins;

            xmonad-hs =
              let
                src = nixpkgs.lib.strings.fileContents ./pkgs/xmonad.hs;
                replacements = {
                  __alacritty_exe__  = "${pkgs.alacritty}/bin/alacritty";
                  __byobu_exe__      = "${my-pkgs.byobu}/bin/byobu";
                  __pactl_exe__      = "${pkgs.pulseaudio}/bin/pactl";
                  __touchpad_exe__   = "${my-pkgs.touchpad}/bin/touchpad";
                  __xbacklight_exe__ = "${pkgs.acpilight}/bin/xbacklight";
                  __xmobar_exe__     = "${pkgs.xmobar}/bin/xmobar";
                  __xmonad_exe__     = "${xmonad}/bin/xmonad";
                  __xrandr_exe__     = "${pkgs.xorg.xrandr}/bin/xrandr";
                  __xscreensaver_command_exe__ =
                    "${pkgs.xscreensaver}/bin/xscreensaver-command";
                };
                repl_from = builtins.attrNames replacements;
                repl_to   = map (x: replacements.${x}) repl_from;
                repl_src = builtins.replaceStrings repl_from repl_to src;
              in
                pkgs.writeTextDir "share/xmonad-hs" repl_src;
            # import ./pkgs/xmonad.hs.nix {
            #   inherit pkgs;
            #   inherit (my-pkgs) byobu touchpad;
            # };

            # xssv-ep-f
            # xsession = import ../pkgs/xsession { inherit nixpkgs; };
          });
        }
    );
}
