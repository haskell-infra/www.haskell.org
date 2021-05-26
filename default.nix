{ doCheck ? true
, sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
}:

let
  builder = import ./builder { inherit sources pkgs; };
  built = pkgs.stdenv.mkDerivation {
    name = "haskell.org";
    inherit doCheck;

    src = pkgs.nix-gitignore.gitignoreSourcePure [
      ./.gitignore
      ".git"
      "*.cabal"
      "*.hs"
      ".github"
      ] ./.;
    buildInputs = [ builder pkgs.linkchecker ];

    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
    LC_ALL = "C.UTF-8";

    buildPhase = ''
      ${builder}/bin/haskell-org-site build
    '';
    checkPhase = ''
      linkchecker _site
    '';
    installPhase = ''
      cp -r _site $out
    '';
  };
in
  if pkgs.lib.inNixShell then builder
  else { inherit builder built; inherit (pkgs) linkchecker lftp; }
