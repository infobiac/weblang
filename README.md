## Programming Languages and Translators

[Alex](https://www.haskell.org/alex/) for lexing,
[Happy](https://www.haskell.org/happy/) for parsing,
[hs-llvm](https://hackage.haskell.org/package/llvm-hs) and [hs-llvm-pure](https://hackage.haskell.org/package/llvm-hs-pure) for the LLVM interface.

Some links:

Article about building a compiler in Haskell with Alex/Happy and LLVM: https://bjbell.wordpress.com/haskell-compiler-series/

Tutorial and examples on Alex/Happy: https://leanpub.com/alexandhappy/read

### Compiling

I'm not sure how hard this will be to compile on your systems, but here's how I think you should try:

1. Install [stack](https://docs.haskellstack.org/en/stable/README/)
2. Install [LLVM 4](https://llvm.org/)
  * Method 1: Install directly using your package manager
  * Method 2: Install [Nix](https://nixos.org/nix/)
     Install Nix with your package manager or from the site, then when you use a stack command like 'stack build' or 'stack exec', instead use 'stack build --nix' or 'stack exec --nix'
3. In this directory, run:
   stack --install-ghc
   stack build
   stack exec -- plt
