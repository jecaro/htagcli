# htagcli

[![nix][status-nix-png]][status-nix]

`htaglib` is a command-line tool for viewing and editing tags in audio files.
It also includes utilities to organize your music collection in various ways.
All audio formats supported by [taglib] are supported.

![demo][demo]

## Installation

A static binary is available in the [releases][releases] page. It should work 
on any linux distribution. For nix users, an overlay is available in [the flake 
file](./flake.nix).

## Basic usage

The basic syntax of `htagcli` is:

```
$ htagcli COMMAND [OPTIONS] [files FILE1 FILE2 ...|directory --extension mp3 --extension flac DIR]
```

You can specify the files to operate on in two ways:
- With the `files` subcommand followed by a list of files
- With the `directory` subcommand followed by a directory and optional 
  `--extension` options to filter files by extension. If no extension is given, 
  `htagcli` defaults to all audio file formats supported by [taglib].

`htagcli` supports the `--help` option at any level, command or subcommand. 
When in doubt, use it to get detailed usage information.

## Tag operations

To display the tags of one or more files, use the `get` command:

```
$ htagcli get files ./data/sample.mp3
File: /path/to/htagcli/data/sample.mp3
Title: title
Artist: artist
Album Artist: albumartist
Album: album
Genre: genre
Year: 2055
Track: 7
```

To edit tags, use the `set` command. For example, to change the genre for all 
files in an album:

```
$ htagcli set --genre "Avant-garde" directory ./data
```

It is also possible, to edit the tags in your `$EDITOR` with the `edit` 
command:

```
$ htagcli edit files ./data/sample.mp3
```

This opens the tags of the specified files in your default text editor. Edit
the tags as needed, then save and quit the editor. The changes will be applied
accordingly.

Delete the whole file content before quitting to cancel the operation.

## Configuration

The next commands require a configuration file. You can generate [a default 
one](data/htagcli.toml) with the `create-config` command:

```
$ htagcli create-config
```

This creates a TOML configuration file at `~/.config/htagcli/htagcli.toml`, 
including comments explaining each option.

You can always override configuration settings using command-line options.
However, keeping your preferred settings in the configuration file makes it
easier to reuse the same options.

## Checks

`htagcli` can perform several checks on your audio files to help you keep your
collection clean and well-organized. Available checks include:

- Track level:
    - Missing tags: Detects files with missing tag fields
    - Genre: Verifies that the genre exists in a predefined list
    - File path: Ensures that the file path follows a given pattern
- Album level:
    - Album directory: Checks that all files from an album are stored in the 
      same directory
    - Cover file: Checks the presence of a cover image in the album directory
    - Album tags: Checks that the tags from all files in an album are the same
- Artist level:
    - Genre: Ensures that all tracks from an artist share the same genre

For more information about available checks, run:

```
$ htagcli check --help
```

### Fixing paths

`htagcli` can automatically move files to match a given path pattern, ensuring
the file path check passes. For example, to reorganize files by artist and 
album, use:

```
$ htagcli fix-paths --base-dir ~/Music --pattern "{artist}/{album}/{track}-{title}" directory ./music-to-clean-up
```

This moves your files under `~/Music`, creating subdirectories for each artist
and album.

Use the `--dry-run` option to preview the planned changes without modifying
any files. It’s a good idea to use it first to confirm everything looks right.

# Hacking

The project can be built with [nix].

Install with:

```
$ nix profile install
```

Build with:

```
$ nix build
```

The binary is then created in `./result/bin/htagcli`

Hack with:

```
$ nix develop
```

You will be dropped in a shell with all the needed tools in scope: `cabal` to 
build the project and `haskell-language-server` for a better developer 
experience.

# Acknowledgments

This project uses [htaglib] as the underlying library to manipulate audio file.

[demo]: ./demo.png
[htaglib]: https://github.com/mrkkrp/htaglib
[nix]: https://nixos.org/
[releases]: https://github.com/jecaro/htagcli/releases
[status-nix-png]: https://github.com/jecaro/htagcli/workflows/nix/badge.svg
[status-nix]: https://github.com/jecaro/htagcli/actions/workflows/nix.yml
[taglib]: https://taglib.org/
