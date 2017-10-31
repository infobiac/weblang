## Programming Languages and Translators

[Alex](https://www.haskell.org/alex/) for lexing,
[Happy](https://www.haskell.org/happy/) for parsing,
[hs-llvm](https://hackage.haskell.org/package/llvm-hs) and [hs-llvm-pure](https://hackage.haskell.org/package/llvm-hs-pure) for the LLVM interface.

Some links:

Article about building a compiler in Haskell with Alex/Happy and LLVM: https://bjbell.wordpress.com/haskell-compiler-series/

Tutorial and examples on Alex/Happy: https://leanpub.com/alexandhappy/read

Nice article with introduction to LLVM, LLVM haskell bindings, and an example of building up LLVM code from an AST: http://www.stephendiehl.com/llvm/#haskell-llvm-bindings

### Compiling

I'm not sure how hard this will be to compile on your systems, but here's how I think you should try:

1. Install [stack](https://docs.haskellstack.org/en/stable/README/)
   * MacOs
   ```
    brew install haskell-stack
   ```
2. Install [LLVM 4](https://llvm.org/)
  * Method 1: Install directly using your package manager
  * Method 2: Install [Nix](https://nixos.org/nix/)
  * MacOs
  ```
  brew install --with-toolchain llvm
  ```

     Install Nix with your package manager or from the site, then when you use a stack command like `stack build` or `stack exec`, instead use `stack build --nix` or `stack exec --nix`
3. In this directory, run:

   ```
   stack --install-ghc
   stack build
   stack exec weblang
   ```


### Spinning up server to call Weblang helloworld endpoint
1. 'stack build --nix'
2. './makeServer.sh' to make server
3. './runWeblangServer 8000'
4. make post request to http://35.194.4.65:8000/helloworld with a payload of key being 'arg' and value being anything. eg {'arg'='testArg'}
5. response is output of running Ryan's hello world example in examples/parsing-hello-world-example.wl
