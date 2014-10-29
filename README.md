# drync: Google Drive Sync

![drynk](images/boozetime.gif)

Sync a local directory with Google Drive. Very alpha. Beware.

## Eventual Usage

```
% drync [FOLDER]
```

Sync the current directory with `FOLDER` on Google Drive. If not given, `FOLDER`
defaults to "root", syncing the current directory with the entire Drive.

## Working So Far

- OAuth2 negotiation and token auth
- Pulling folder contents and file information from the Drive API

## Development / Installation

Use `bin/setup` to get started. Use `cabal run` to try it out.
