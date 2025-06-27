{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    # nativeBuildInputs is usually what you want -- tools you need to run
    nativeBuildInputs = with pkgs.buildPackages; [
      xorg.libX11.dev
      xorg.libXcursor.dev
      xorg.libXrandr.dev
      xorg.libXinerama.dev
      xorg.libXi.dev

      libGL

      sbcl
    ];
}
