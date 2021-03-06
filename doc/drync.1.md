# drync 1 "March 2015" drync "User Manuals"

## SYNOPSIS

`drync` [OPTION ...] [DIRECTORY]

*DIRECTORY* defaults to current working directory.

https://github.com/pbrisbin/drync

## DESCRIPTION

Sync a local directory with Google Drive

## OPTIONS

`-x`, `--exclude` *PATTERN*
  Exclude files and folders matching the given pattern. This option may be
  passed multiple times.

  The pattern is compared against file basenames and remote file titles as they
  are seen. If a directory or folder matches, it is not synced or traversed. If
  a local or remote file matches, it is not uploaded or downloaded. POSIX-like
  globs are supported.

`--delete-local`
  Delete any files or directories which only exist locally.

`--delete-remote`
  Delete any files or folders which only exist on your Drive.

`-p`, `--profile` *NAME*
  Use the named profile. Defaults to the string *default*.

  This is useful if you have multiple Google accounts. Using *drync* to sync
  multiple accounts requires separate files for cached OAuth tokens. This is
  accomplished by naming the files based on this option.

`-r`, `--refresh-oauth`
  Ignore any cached OAuth2 credentials and re-run the verification process. This
  is required if you revoke or otherwise have issues with the cached access
  tokens.

`-t`, `--throttle` *N*
  Throttle HTTP connections to the given value in KB/s. This effects both
  uploads and downloads. The default is *0*, disabled.

`-s`, `--silent`
  Output nothing beyond errors.

`-d`, `--debug`
  Output debugging messages.

## FILES

*$XDG_CACHE_HOME/drync/$name.token*
  Location of cached OAuth2 tokens for profile named *$name*.

*$XDG_CACHE_HOME/drync/config*
  Location of config file. See *CONFIG FILE FORMAT*.

*$XDG_CACHE_HOME/drync/exclude*
  Location of exclude file. See *EXLUDE FILE FORMAT*.

## CONFIG FILE FORMAT

A minimal complete example:

    sync_from = "$(HOME)/drive"
    profile = "default"
    throttle = 1000

## EXCLUDE FILE FORMAT

One pattern per line, following the same rules as arguments to `--exclude`.

Lines beginning with `#` are ignored. Blank lines result in empty patterns but
the empty pattern only matches the empty filename, so these can be considered
ignored as well.

## AUTHOR

Patrick Brisbin <pbrisbin@gmail.com>
