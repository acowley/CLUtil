with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  name = "CLUtil";
  buildInputs = [ darwin.libobjc darwin.apple_sdk.libs.xpc ] ++
    (with darwin.apple_sdk.frameworks; [OpenCL]);
  # shellHook = "prependSearchPath";
  ghc = ghc;
}
