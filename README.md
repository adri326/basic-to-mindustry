# BASIC to Mindustry logic

This is a small transpiler from a dialect of the [BASIC](https://en.wikipedia.org/wiki/BASIC) language, "MinBasic" (also known as `mbas`), to [Mindustry](https://github.com/Anuken/Mindustry/)'s [logic system](https://www.reddit.com/r/Mindustry/comments/kfea1e/an_overly_indepth_logic_guide/) (also known as `mlog`).
Basic is chosen as the source language as it already contains jumps (which mindustry heavily relies on), while allowing for some higher-order constructs like conditions, loops and functions.

## Installation and running

To use this project, start by cloning this git repository:

```sh
git clone https://git.shadamethyst.xyz/amethyst/basic-to-mindustry/
cd basic-to-mindustry
```

You will then need an installation of the Rust compiler, which you can quickly get from [rustup.rs](https://rustup.rs/).

```sh
# To build the source code:
cargo build

# To run the binary:
./target/debug/basic-to-mindustry examples/prime.mbas

# You can do both of these with the following command (note the --):
cargo run -- examples/prime.mbas
```

<!-- TODO: add options -->

## VSCode syntax highlighting

Any language support extension for QuickBasic (the dialect MinBasic is based on) will work,
but if you would like an extension that was tailored to support MinBasic, you can have a look at [the one bundled with this project](./minbasic-vscode/README.md).

## Language features

The [GUIDE.md](./GUIDE.md) file describes how to write programs in MinBasic.
