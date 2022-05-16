let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit = import ./nix/pre-commit.nix { inherit sources; };

in
{
  inherit (pkgs) intrayNotification;
  release = pkgs.intrayRelease;
  pre-commit-check = pre-commit.run;
  nixos-module-test = import ./nix/nixos-module-test.nix { inherit pkgs; };
  shell = pkgs.symlinkJoin {
    name = "shell";
    paths = (import ./shell.nix { inherit sources pkgs pre-commit; }).buildInputs;
  };
}
