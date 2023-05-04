{
  description = "nix configuration for X-based things";

  inputs = {
    nixpkgs.url     = github:nixos/nixpkgs/be44bf67; # nixos-22.05 2022-10-15
    flake-utils.url = github:numtide/flake-utils/c0e246b9;
    myPkgs          = {
      url    = github:sixears/nix-pkgs/r0.0.1.0;
      inputs = { nixpkgs.follows = "nixpkgs"; };
    };
  };

  outputs = { self, nixpkgs, flake-utils, myPkgs }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        my-pkgs = myPkgs.packages.${system};
      in
        rec {
          packages = flake-utils.lib.flattenTree (with pkgs; {
            inherit alacritty acpilight i3status xscreensaver;

            xmonad = xmonad-with-packages.override {
              packages = p: with p; [ base-unicode-symbols data-default
                                      xmonad-contrib ];
            };

            urxvt = rxvt_unicode-with-plugins;

            xmonad-hs =
              import ./pkgs/xmonad.hs.nix {
                inherit pkgs;
                inherit (my-pkgs) byobu touchpad;
              };

            # xssv-ep-f
            # xsession = import ../pkgs/xsession { inherit nixpkgs; };
          });
        }
    );
}
