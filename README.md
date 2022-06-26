# HP0 Front-Facing Repository

This is HP0, a reimplementation of the P0 compiler from McMaster's
CS 4TB3 course. It will compile a modified subset of Pascal code,
and contains features not included in the base P0 compiler.

## Installation

This assumes you have the following programs installed:
- The Haskell Stack Tool `stack`: To compile the HP0 compiler
- `wat2wasm` which can be installed with npm: `npm install -g wat2wasm`
- cargo, for running Wasmwer written in Rust

After cloning the repository, you can run:

```
stack build
stack run test/suite/IfThenElse.p
```


