{ sources ? import ../nix/sources.nix
, pkgs ? import sources.nixpkgs {}
}:
(pkgs.haskellPackages.developPackage {
  name = builtins.baseNameOf ./.;
  root = pkgs.nix-gitignore.gitignoreSourcePure [
    ../.gitignore
    "*.markdown"
    "*.md"
    "*.html"
    "templates/*"
    "css/*"
    "js/*"
    "img/*"
    ".git"
    ".github"
  ] ./.;
  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = with pkgs.haskellPackages; (attrs.buildTools or []) ++ [
      cabal-install
      ghcid
      hakyll
      pkgs.linkchecker
    ];
  });
}).overrideAttrs (old: {
  LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  LC_ALL = "C.UTF-8";
  shellHook = ''
        echo ""
        echo "  Haskell.org Dev Shell"
        echo "    \`linkchecker\`, \`ghcid\` and \`cabal\` are provided in this environment."
        echo ""
      '';
})
