with (import ./default.nix {});

hsPkgs.shellFor {
  packages = _: [
    hsdatalog
  ];

  withHoogle = true;
}
