{ pkgs ? import ./pkgs.nix {} }: with pkgs;

stdenv.mkDerivation {
  name = "json";
  buildInputs = [
    erlang
    pandoc
    rebar3
  ];
}
