with import <nixpkgs> {}; mkShell {
  buildInputs = [ 
    cargo
    clippy
    darwin.apple_sdk.frameworks.Security
    libiconv
    rustc
    rustfmt
    rust-analyzer
  ];
}
