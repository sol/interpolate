# String interpolation for Haskell done right

Interpolates arbitrary Haskell expressions

```
>>> :set -XQuasiQuotes
>>> import Data.String.Interpolate
>>> let name = "foobar"
>>> let age = 23
>>> putStrLn [i|name: #{name}, age: #{age}|]
name: foobar, age: 23
```

and properly handles escape sequences in string literals

```
>>> putStrLn [i|foo-\955-bar|]
foo-λ-bar
```

and in interpolated expressions

```
>>> putStrLn [i|some #{unwords ["foo", "\955", "bar"]} test|]
some foo λ bar test
```
