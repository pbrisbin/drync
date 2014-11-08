# drync: Google Drive Sync

![drynk](images/boozetime.gif)

Sync a local directory with Google Drive. Very alpha. Beware.

```
Usage: drync [-p|--profile NAME] [-r|--refresh-oauth] [-f|--sync-from DIR]
             [-t|--sync-to FOLDER]
  Sync a local directory with one on Google Drive

Available options:
  -h,--help                Show this help text
  -p,--profile NAME        Use the named profile
  -r,--refresh-oauth       Ignore cached OAuth2 credentials
  -f,--sync-from DIR       Sync from the given directory
  -t,--sync-to FOLDER      Sync to the given folder
```

## Features

**MVP**

- [x] OAuth2 negotiation and token auth
- [x] Command-line options
- [x] Recursively finding files to sync
- [x] Creating folders on remote
- [x] Creating files on remote
- [x] Updating files on remote
- [x] Downloading files from remote
- [x] In-line Network.Google.OAuth2
- [x] Up/Download progress reporting
- [x] Resumable uploads
- [x] Resumable upload retries
- [x] Up/Download throttling

**Enhancements**

- [ ] Extract sub-packages
- [ ] Downloadable binaries
- [ ] Allow syncing to a nested folder (`--sync-to /foo/bar`)
- [ ] Allow syncing the entire drive (`--sync-to /`)
- [ ] Logging (`WriterT`, levels, etc)
- [x] Error handling
- [ ] Concurrency

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
- `google-oath2` - token exchange logic
- `google-api` - service agnostic Google API client
- `google-drive` - API client specifically for the Drive API
