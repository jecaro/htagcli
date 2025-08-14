# htagcli

[![nix][status-nix-png]][status-nix]

`htaglib` is a command line tool to view and edit tags in audio files.

Get started with:

```
$ htagcli --help
```

# Hacking

The project can be built with [nix].

Install with:

```bash
$ nix profile install
```

Build with:

```bash
$ nix build
```

The binary is then created in `./result/bin/htagcli`

Hack with:

```bash
$ nix develop
```

You will be dropped in a shell with all the needed tools in scope: `cabal` to 
build the project and `haskell-language-server` for a better developer 
experience.

[nix]: https://nixos.org/
[status-nix-png]: https://github.com/jecaro/htagcli/workflows/nix/badge.svg
[status-nix]: https://github.com/jecaro/htagcli/actions/workflows/nix.yml
