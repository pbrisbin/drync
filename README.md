# drync: Google Drive Sync

![drynk](images/boozetime.gif)

Sync a local directory with Google Drive. Very alpha. Beware.

## Eventual Usage

```
usage: drync [options]
options:
    -p, --profile NAME          Use the named profile
    -f, --sync-from DIR         Sync from the given directory
    -t, --sync-to FOLDER        Sync to the given folder
```

## Working So Far

- OAuth2 negotiation and token auth
- Printing out required actions, non-recursively

## Development / Installation

Use `bin/setup` to get started. Use `cabal run` to try it out.
