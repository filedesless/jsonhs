# jsonhs

JSON parser

## Build, run, test and bench

```
stack build
stack run
stack test
stack bench
```

## Profiling

```
# create jsonhs-benchmark.prof
stack build --bench --profile 

# check it in the browser
profiteur jsonhs-benchmark.prof index.html
open index.html
```

## TODO

- try Text.ParserCombinators.ReadP instead of Parsec maybe?
- see why handling strings is so slow (Data.Text maybe?)
- `jsonUnEscape :: String -> String` ?