with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  name = "CLUtil";
  buildInputs = stdenv.lib.optionals stdenv.isDarwin [darwin.apple_sdk.frameworks.OpenCL];
  # shellHook = "prependSearchPath";
  ghc = haskell.compiler.ghc822;
}
