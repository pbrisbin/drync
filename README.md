# drync: Google Drive Sync

![drynk](images/boozetime.gif)

Sync a local directory with Google Drive.

## Installation

`drync` is packaged as a static binary suitable for 64 bit Arch Linux.

```
% curl -O https://github.com/pbrisbin/drync/blob/master/pkg/PKGBUILD
% makepkg -s -c -i
% man 1 drync
% drync --help
```

## Development Installation and Usage

- Create a project in the [Google Developers Console][console]
- Enable OAuth2 for the project
- Enable the Drive API for the project
- Create `src/Drync/Client.hs` with the following contents:

```hs
module Drync.Client (client) where

import Network.Google.OAuth2 (OAuth2Client(..))

client :: OAuth2Client
client = OAuth2Client
    { clientId = "..."
    , clientSecret = "..."
    }
```

[console]: https://console.developers.google.com

```
% cabal sandbox init
% cabal install --dependencies-only
% cabal run -- --help
```

Optionally, `cp` (or `ln`) `dist/build/drync/drync` into `$PATH`.
