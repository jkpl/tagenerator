# tagenerator

A small simple text adventure generator made in Haskell. This project is my
final assignment for university course "Automatons and formal languages", which
was held in University of Jyväskylä during Summer of 2013.

Tagenerator can parse a really simple playable text adventure game from files
that follow a format made specifically for this project. The format is a bit
similar to JSON. An example game can be found from `examplegame/` directory.

## Goal

The point of this project is to demonstrate the process of parsing games and
game commands from predefined languages, which is why this project uses a
homebrew monadic parser combinator instead of a library like [Parsec][].

## Building

Probably the easiest way to build this project is to use [cabal][]. To build
the project, go to the project root directory and write the following commands:

    $ cabal configure
    $ cabal build

This should build an executable named `tagenerator` in directory
`dist/build/tagenerator/`.

## Usage

Call the executable with the directory to your text adventure files as the
first parameter of the command:

    $ dist/build/tagenerator/tagenerator examplegame/

The executable will try to find text adventure files from the current
directory, if no path is provided. Each text adventure file should have `ta` as
file extension (example: `mygame.ta`). A single text adventure game can be (but
doesn't have to be) separated into multiple files.

## License

The MIT License. See `LICENSE` file for further details.

[Parsec]: http://www.haskell.org/haskellwiki/Parsec
[cabal]: http://www.haskell.org/cabal/
