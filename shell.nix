let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit = import ./nix/pre-commit.nix { inherit sources; };
in
pkgs.haskell.lib.buildStackProject {
  name = "intray-shell";
  buildInputs = with pkgs; [
    (import sources.niv { }).niv
    feedback
    haskellPackages.autoexporter
    killall
    linkcheck
    seocheck
    stripe-cli
    unzip
    zlib
  ] ++ pre-commit.tools;
  shellHook = pre-commit.run.shellHook + pkgs.feedback.shellHook;
}
