# drync: Google Drive Sync

![drynk](images/boozetime.gif)

Sync a local directory with Google Drive. Very alpha. Beware.

## Development

- Go to Google Developers Console
- Create an App and get its Client Id and Secret
- Enable the Drive API
- Create `src/Drync/Client.sh` with the following contents:

```haskell
module Drync.Client
    ( OAuth2Client(..)
    , client
    ) where

import Network.Google.OAuth2 (OAuth2Client(..))

client :: OAuth2Client
client = OAuth2Client
    { clientId = "your-client-id"
    , clientSecret = "your-client-secret"
    }
```

- Initialize a sandbox

```
% cabal sandbox init
```

- Clone my fork of handa-gdata and add it as a source

```
% cd .. && git clone https://github.com/pbrisbin/hgdata
% cd drync && cabal sandbox add-source ../hgdata
```

- Install

```
% cabal install --dependencies-only --enable-tests
```

- Run

```
% cabal run
```
