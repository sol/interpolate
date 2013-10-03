# String interpolation done right

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

    >>> let profission = "\955-scientist"
    >>> putStrLn [i|profission: #{unwords [name, "the", profission]}|]
    profission: Marvin the λ-scientist
