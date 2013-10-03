# String interpolation done right!

    >>> :set -XQuasiQuotes
    >>> import Data.String.Interpolate

Interpolates strings

    >>> let name = "Marvin"
    >>> putStrLn [i|name: #{name}|]
    name: Marvin

or integers

    >>> let age = 23
    >>> putStrLn [i|age: #{age}|]
    age: 23

or arbitrary Haskell expressions

    >>> let profession = "\955-scientist"
    >>> putStrLn [i|profession: #{unwords [name, "the", profession]}|]
    profession: Marvin the λ-scientist
