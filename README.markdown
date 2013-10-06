# String interpolation done right!

There is not much here yet.  But bellow is an example and links to other string
interpolation libraries.

## Example

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

## Other string interpolation libraries for Haskell

There are many other string interpolation libraries for Haskell, each with it's
own set of limitations.  Here is a short list, a more thorough analysis will
follow.

 * [interpolatedstring-qq](http://hackage.haskell.org/package/interpolatedstring-qq)
   (insterpolating `String` and `Text` does not really work)
 * [interpolatedstring-perl6](http://hackage.haskell.org/package/interpolatedstring-perl6)
   (uses `IncoherentInstances`)

There are more at http://www.haskell.org/haskellwiki/Quasiquotation and
http://packdeps.haskellers.com/reverse/haskell-src-meta.
