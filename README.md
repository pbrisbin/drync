# drync: Google Drive Sync

![drynk](images/boozetime.gif)

Sync a local directory with Google Drive. Very beta. Beware.

```
Preprocessing library drync-0.0.0...
In-place registering drync-0.0.0...
Preprocessing executable 'drync' for drync-0.0.0...
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

```
% cabal sandbox init
% cabal install --dependencies-only --enable-tests --avoid-reinstalls -j
% cabal run -- --help
```

## Packages

Eventually, this project should split into the following packages:

- `conduit-progress` - show progress as data moves through a conduit
- `conduit-throttle` - throttle data as it moves through a conduit
- `google-api` - service-agnostic Google API client
- `google-drive` - API client specifically for the Drive API
