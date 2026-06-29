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
$ htagcli COMMAND [OPTIONS] [FILE|DIRECTORY]
```

You can pass as many files or directories as needed. Directories are processed 
recursively. For directories, only audio files with extensions supported by 
[taglib] are considered. You can change this with the `--extension` option.

`htagcli` supports the `--help` option at any level. When in doubt, use it to 
get detailed usage information.

## Tag operations

To display the tags of one or more files, use the `get` command:

```
$ htagcli get ./data/sample.mp3
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
$ htagcli set --genre "Avant-garde" ./data
```

It is also possible, to edit the tags in your `$EDITOR` with the `edit` 
command:

```
$ htagcli edit ./data/sample.mp3
```

This opens the tags of the specified files in your default text editor. Edit
the tags as needed, then save and quit the editor. The changes will be applied
accordingly.

Delete the whole file content before quitting to cancel the operation.

## Setting tags with MusicBrainz

`htagcli` can fetch tags from [MusicBrainz][musicbrainz] to automatically set 
the tags of your files. To do this, one can search for an album/artist using 
the `search` command:

```
$ htagcli search --album repeater --artist fugazi
Searching: "fugazi" - "repeater"

3 releases found

1. ID: 37e6a462-1417-45dc-9d88-4ef9aff4bc19
   Artist: Fugazi
   Album: Repeater
   Year: 2005
   Discs: 1
   Tracks: 14

   Disc 1: Tracks: 14

     1. Turnover
     2. Repeater
     3. Brendan #1
...
```

If the search is run against an existing album, `htagcli` will show similarity 
values to help you choose the best match. In the following case, the first 
result shows a low similarity because the number of tracks is different from 
the actual album. The second result is a perfect match.

```
$ htagcli search ./repeater/
Searching: "Fugazi" - "Repeater"

3 releases found

1. ID: 37e6a462-1417-45dc-9d88-4ef9aff4bc19 (87%)
   Artist: Fugazi (100%)
   Album: Repeater (100%)
   Year: 2005 (1990)
   Discs: 1
   Tracks: 14 (11)

   Disc 1: (79%) - Tracks: 14 (11)

     1. Turnover (100%)
     2. Repeater (100%)
     3. Brendan #1 (100%)
     4. Merchandise (100%)
     5. Blueprint (100%)
     6. Sieve-Fisted Find (100%)
     7. Greed (100%)
     8. Two Beats Off (100%)
     9. Styrofoam (100%)
     10. Reprovisional (100%)
     11. Shut the Door (100%)
     12. Song #1
     13. Joe #1
     14. Break-In

2. ID: 00baa173-29db-33a9-af6d-fe109e53a211 (100%)
   Artist: Fugazi (100%)
   Album: Repeater (100%)
   Year: 1990
   Discs: 1
   Tracks: 11

   Disc 1: (100%) - Tracks: 11

     1. Turnover (100%)
     2. Repeater (100%)
     3. Brendan #1 (100%)
     4. Merchandise (100%)
     5. Blueprint (100%)
     6. Sieve-Fisted Find (100%)
     7. Greed (100%)
     8. Two Beats Off (100%)
     9. Styrofoam (100%)
     10. Reprovisional (100%)
     11. Shut the Door (100%)

...
```

Once you found a matching release, you can set the tags using the ID of the 
release:

```
$ htagcli set --id 00baa173-29db-33a9-af6d-fe109e53a211 ./repeater/
```

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
- Disc level:
    - Disc directory: Checks that all files from a disc are stored in the 
      same directory
    - Cover file: Checks the presence of a cover image in the disc directory. 
      Also verifies that the cover image size is within specified limits.
    - Disc tags: Checks that the tags from all files in a disc are the same
    - Track numbers: Checks that the track numbers are sequential and start 
      from 1
- Album level:
    - Disc numbers: Checks that the disc numbers are sequential and start from 
      1
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
$ htagcli fix-paths --base-dir ~/Music --pattern "{artist}/{album}/{track}-{title}" ./music-to-clean-up
```

This moves your files under `~/Music`, creating subdirectories for each artist
and album.

Use the `--dry-run` option to preview the planned changes without modifying
any files. It’s a good idea to use it first to confirm everything looks right.

Warning: `htagcli` will remove empty directories after moving the files to 
their new location.

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
[musicbrainz]: https://musicbrainz.org/
[nix]: https://nixos.org/
[releases]: https://github.com/jecaro/htagcli/releases
[status-nix-png]: https://github.com/jecaro/htagcli/workflows/nix/badge.svg
[status-nix]: https://github.com/jecaro/htagcli/actions/workflows/nix.yml
[taglib]: https://taglib.org/
