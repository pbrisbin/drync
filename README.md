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

## Development Installation and Usage

- Use the Google Developers Console to create a project
- Enable OAuth2 for the project
- Enable the Drive API for the project
- Copy `files/Client.hs` to `src/Drync/Client.hs` and add your credentials
- Clone `google-drive` (not yet on Hackage, will be soon)

```
% git clone https://github.com/pbrisbin/google-drive
% git clone https://github.com/pbrisbin/drync
% cd drync
% cabal sandbox init
% cabal sandbox add-source ../google-drive
% cabal install --dependencies-only --enable-tests --avoid-reinstalls -j
% cabal run -- --help
```
