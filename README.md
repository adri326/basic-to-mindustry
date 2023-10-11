# BASIC to Mindustry logic

*Write simple code for complex logic in [Mindustry](https://github.com/Anuken/Mindustry/)*

```basic
LET time = 0

WHILE true
    time = time + 1

    reactor1.enabled = time % 3 == 0
    reactor2.enabled = time % 3 == 1
    reactor3.enabled = time % 3 == 2

    WAIT(1)
WEND
```

![Output of the above code block](./assets/blinking-reactors.gif)

`basic-to-mindustry` is a small transpiler from a dialect of the [BASIC](https://en.wikipedia.org/wiki/BASIC) language, "MinBasic" (also known as `mbas`), to [Mindustry](https://github.com/Anuken/Mindustry/)'s [logic system](https://www.reddit.com/r/Mindustry/comments/kfea1e/an_overly_indepth_logic_guide/) (also known as `mlog`).
Basic is chosen as the source language as it already contains jumps (which mindustry heavily relies on), while allowing for some higher-order constructs like conditions, loops and functions.

## Installation and running

You will need an installation of the Rust compiler, which you can quickly get from [rustup.rs](https://rustup.rs/).

Then, simply run the following command to install the MinBasic compiler:

```sh
cargo install --git "https://github.com/adri326/basic-to-mindustry/"
```

Finally, you can run `basic-to-mindustry --help` to get a usage guide:

```
A BASIC to Mindustry Logic compiler and optimizer.

Usage: basic-to-mindustry [OPTIONS] <INPUT>

Arguments:
  <INPUT>
          The path to the input program

Options:
  -c, --copy
          If set, copies the results to the clipboard.

  -o, --opt <OPT>
          Defines the amount of optimizations done on the generated program:
          - A value of 0 means that no optimizations will be done (useful for debugging the compiler itself)
          - A value of 1 means that conservative optimizations will be done (which still allows you to embed the code generated with other pieces of code)
```

### For contributing

If you wish to make changes to the source code, then you will need to clone this git repository:

```sh
git clone https://github.com/adri326/basic-to-mindustry/
cd basic-to-mindustry
```

You will also need an installation of the Rust compiler, which you can quickly get from [rustup.rs](https://rustup.rs/).

```sh
# To build the source code:
cargo build

# To run the binary:
./target/debug/basic-to-mindustry examples/prime.mbas

# You can do both of these with the following command (note the --):
cargo run -- examples/prime.mbas

# To run the unit tests:
cargo test
```

## VSCode syntax highlighting

Any language support extension for QuickBasic (the dialect MinBasic is based on) will work,
but if you would like an extension that was tailored to support MinBasic, you can have a look at [the one bundled with this project](./minbasic-vscode/README.md).

## Language features

The [GUIDE.md](./GUIDE.md) file describes how to write programs in MinBasic.
