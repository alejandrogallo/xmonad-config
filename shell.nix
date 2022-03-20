with import <nixpkgs> {};

let
  haskellDeps = ps: [
    ps.xmonad
    ps.xmonad-contrib
    ps.hoogle
    ps.hlint
  ];
  myghc = haskellPackages.ghcWithPackages haskellDeps;
  nixPackages = [
    myghc
    #haskell-language-server
    ctags
    git
    xmobar
    libnotify
    ];
in
mkShell rec {
  name = "gallo-xmonad";
  buildInputs = nixPackages;
  system = builtins.currentSystem;
}
