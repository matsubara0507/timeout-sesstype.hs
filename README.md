# timeout-sesstype.hs

This is a command-line interface, an implementation of Multiparty Session Types with Timeout.

## Build

To build the `timeout-sesstype-cli` binary from source

```
stack build --copy-bins
```

## Using the tool

Parsing the global type

```
$ timeout-sesstype-cli parse examples/simple_timeout.mpst
A@[x < 10.0, A -> B : ping . B -> A : pong . end] . (A -> B : ok . end, A -> B : fail . end)
```

Projecting the global type for role A

```
$ timeout-sesstype-cli project --role A examples/simple_timeout.mpst
A@[x < 10.0, B!ping . B?pong . end] . (B!ok . end, B!fail . end)
```

if not select role, Projecting the global type for all participant

```
$ timeout-sesstype-cli project examples/simple_timeout.mpst
A: A@[x < 10.0, B!ping . B?pong . end] . (B!ok . end, B!fail . end)
B: A@[x < 10.0, A?ping . A!pong . end] . (A?ok . end, A?fail . end)
```

For more options, use the `-h` flag

```
$ timeout-sesstype-cli -h
```

## Acknowledgement

This tool is greatly inspired by [nickng's `sesstype-cli.rs`](https://github.com/nickng/sesstype-cli.rs).
