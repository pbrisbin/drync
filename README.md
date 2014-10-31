# drync: Google Drive Sync

![drynk](images/boozetime.gif)

Sync a local directory with Google Drive. Very alpha. Beware.

## Usage

```
usage: drync [options]
options:
    -f, --sync-from DIR         Sync from the given directory
    -t, --sync-to FOLDER        Sync to the given folder

    -p, --profile NAME          Use the named profile
    -r, --refresh-oauth         Ignore cached OAuth2 credentials
```

## Features

**MVP**

- [x] OAuth2 negotiation and token auth
- [ ] Command-line options
- [x] Recursively finding files to sync
- [x] Creating folders on remote
- [ ] Creating files on remote
- [ ] Updating files on remote
- [x] Downloading files from remote

**Enhancements**

- [ ] Logging (`WriterT`, levels, etc)
- [ ] Error handling (ignore, retry, etc)
- [ ] Concurrency

## Development / Installation

Use `bin/setup` to get started. Use `cabal run` to try it out.
