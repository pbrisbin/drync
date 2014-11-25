# drync: Google Drive Sync

![drynk](images/boozetime.gif)

Sync a local directory with Google Drive. Very beta. Beware.

```
Usage: drync [-p|--profile NAME] [-r|--refresh-oauth] [-f|--sync-from DIR]
             [-x|--exclude PATTERN] [-T|--throttle N] [-P|--progress N]
             [-d|--debug]
  Sync a local directory with one on Google Drive

Available options:
  -h,--help                Show this help text
  -p,--profile NAME        Use the named profile
  -r,--refresh-oauth       Ignore cached OAuth2 credentials
  -f,--sync-from DIR       Sync from the given directory
  -x,--exclude PATTERN     Exclude files and folders matching PATTERN
  -T,--throttle N          Throttle HTTP to N KB/s
  -P,--progress N          Output transfer progress every N bytes
  -d,--debug               Output debugging messages
```

## Installation

For now, `drync` is packaged as a binary distribution and only for 64 bit Arch.

```
% curl https://github.com/pbrisbin/drync/blob/master/pkg/PKGBUILD > PKGBUILD
% makepkg -s -c -i
% drync --help
```

On non-Arch systems, feel free to grab the [archive][], extract, and do a `make
install`. If you happen to have shared object libraries in the same locations as
my system, it might work!

[archive]: http://source.pbrisbin.com

## Development Installation and Usage

This should work on any platform with a Haskell installation.

- Use the Google Developers Console to create a project
- Enable OAuth2 for the project
- Enable the Drive API for the project
- Copy `files/Client.hs` to `src/Drync/Client.hs` and add your credentials

```
% cabal sandbox init
% cabal install --dependencies-only --enable-tests --avoid-reinstalls -j
% cabal run -- --help
```

Optionally, `cp` (or `ln`) `dist/build/drync/drync` into `$PATH`.
