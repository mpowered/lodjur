# Lodjur

*A Nixops Web Deployment Manager!*

## Building

Lodjur can be built with Cabal, Stack, or Nix.

**TODO: More information on commands.**

## Running

Lodjur requires a configuration file, in TOML format, to start its web server.
It's path is specified using the `-c` command line flag, defaulting to
`lodjur.toml` in the current working directory.

```sh
$ lodjur -c my-config.toml
```

The provided [`lodjur.example.toml`](./lodjur.example.toml) file can be used as
a template.
