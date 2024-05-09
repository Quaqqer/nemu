{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  LD_LIBRARY_PATH = builtins.concatStringsSep ":" [
    "${pkgs.xorg.libX11}/lib"
    "${pkgs.xorg.libXi}/lib"
    "${pkgs.libGL}/lib"
  ];
}
