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

## Parsing

double quote keyword `"hoge"` is ignore space on around, but single quote keyword `'a'` is not ignore space.

```
-- Common
ident    = [a-z][A-Za-z0-9]*
role     = [A-Z][A-Za-z0-9]*
message  = ident
var      = ident
space    = ' '*
delta    = clock "<" time
         | clock "<=" time
clock    = ident
time     = [0-9]+'.'[0-9]+

-- Global Type
global      = role "->" role ":" message "." global
            | recur
            | var
            | end
            | timeout
recur       = '*' var "." global
end         = "end"
timeout     = role '@' '[' space delta "," sync_global space ']' "." global'
global'     = "(" global "," global ")"
sync_global = role "->" role ":" message "." sync_global | end

-- Local Type
local      = role "!" message "." local
           | role "!" message "." local
           | lrecur
           | lend
           | ltimeout
lrecur     = '*' var "." local
lend       = "end"
ltimeout   = role '@' '[' space delta "," sync_local space ']' "." local'
local'     = "(" local "," local ")"
           | local
sync_local = role "!" message "." sync_global
           | role "?" message "." sync_global
           | lend
```

## Acknowledgement

This tool is greatly inspired by [nickng's `sesstype-cli.rs`](https://github.com/nickng/sesstype-cli.rs).
