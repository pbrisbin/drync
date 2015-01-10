# drync: Google Drive Sync

![drynk](images/boozetime.gif)

Sync a local directory with Google Drive. Very beta. Beware.

```
Usage: drync [DIRECTORY] [-x|--exclude PATTERN] [--delete-local]
             [--delete-remote] [-p|--profile NAME] [-r|--refresh-oauth]
             [-t|--throttle N] [-s|--silent] [-d|--debug]
  Sync a local directory with Google Drive

Available options:
  -h,--help                Show this help text
  -x,--exclude PATTERN     Exclude files and folders matching PATTERN
  --delete-local           Delete files which exist only locally
  --delete-remote          Delete files which exist only on your Drive
  -p,--profile NAME        Use the named profile
  -r,--refresh-oauth       Ignore cached OAuth2 credentials
  -t,--throttle N          Throttle HTTP to N KB/s
  -s,--silent              Output nothing beyond errors
  -d,--debug               Output debugging messages
```

## Installation

For now, `drync` is packaged as a binary distribution and only for 64 bit Arch.

```
% curl -O https://github.com/pbrisbin/drync/blob/master/pkg/PKGBUILD
% makepkg -s -c -i
% drync --help
```

## Development Installation and Usage

This should work on any platform with a Haskell installation.

- Use the [Google Developers Console][console] to create a project
- Enable OAuth2 for the project
- Enable the Drive API for the project
- Copy `.env.sample` to `.env` and add your credentials.

**NOTE**: The format of the `.env` file is very simplistic, keep things exactly
as they are in the sample -- only with your own Client Id and Secret

[console]: https://console.developers.google.com

```
% cabal sandbox init
% cabal install --dependencies-only
% cabal run -- --help
```

Optionally, `cp` (or `ln`) `dist/build/drync/drync` into `$PATH`.
