{ pkgs ? import ./pkgs.nix {} }: with pkgs;

stdenv.mkDerivation {
  name = "rfc8259";
  buildInputs = [
    erlang
    pandoc
    rebar3
  ];
}
