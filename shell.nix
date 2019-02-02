{ pkgs ? import ./pkgs.nix {} }: with pkgs;

stdenv.mkDerivation {
  name = "json";
  buildInputs = [
    cmark
    erlang
    rebar3
  ];
}
