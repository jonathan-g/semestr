# Expose contents of an environment in the current environment

Expose the contents of an environment in the current environment.
Similar to [attach](https://rdrr.io/r/base/attach.html), but it exposes
the contents only in the current environment and does not change the
global search.

## Usage

``` r
pull_env(env)
```

## Arguments

- env:

  The environment to attach locally.
