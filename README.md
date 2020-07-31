
# Haskell Calculator using Parser Combinators

This is an interactive calculator written in pure Haskell that supports a large number of operations despite only being in the order of about 400 lines of code.

The parser combinators and parser definition reside in `ParserCombinator.hs`, a number type wrapper which allows the calculator to return fractions, integers and doubles is in `Number.hs`, while all calculator-specific code, tests, and the interactive interface reside in `Calculator.hs`.

## Features

- Produces simplified fractions or integers where possible, only returns decimals for transcendental functions
- Binary infix operators +, -, *, /, and right associative ** or ^
- Implicit multiplication - `2(3+4) == 2*(3+4)` and `sin pi cos pi == sin(pi)*cos(pi)`
- Factorial, permutations (`nPr`), combinations (`nCr` or `n choose r`)
- Common unary/binary functions (trigonometry, max/min, log/exp, gcd, etc)
- pi and e constants
- Scientific notation (eg 5.2e-3)
- Function application is higher precedence than binary infix functions (eg `ln e^2 -> (ln e)^2`) like in Haskell

## Examples

The calculator supports standard infix math notation used in programming, as well as LaTeX style markup in parallel. This works by defining a function `\frac` and allowing `{}` brackets to be used to denote a new expression for function application, so `frac 1 2` and `\frac{1}{2}` are parsed the same way.

TODO: explain number wrapper, applicatives meaning AST is not needed, etc

```
> 2 / 1 * (3 - 4 + 2*2)
6
> atan 1 + atan 2 + atan 3 - acos -1
0.0
> 3\times \left(\frac{5}{9}\right)^2*\left(\frac{27}{5}\right)^1/2
5/2
> max 5 6 + ln e
7.0
```

## Inner workings

We define a monadic `Parser` type, which contains a function taking a string and returning a tuple containing a parsed token and the unparsed remainder of the string. This is a functor, so we can apply functions to the result of a parser (eg if we parse a sequence of digits we can run `int = fmap read digitsParser`). It is an applicative and monad, so we can write `mul = symb "*" *> return (*)` to get a function embedded in the parser type if the `*` symbol is found. Then we can apply binary functions using applicative syntax like `int >>= (\a -> mul <*> pure a <*> int)` to parse `3 * 4 -> Parser 12`.

This means we can evaluate in-place instead of constructing an abstract syntax tree, as the grammar used is context free. The expression parser then uses a sub-expression parser which handles numbers, constants, function application and bracketed expressions (recursively), and chains the sub-expressions by infix operators with a parser combinator `chainl1` which applies the functions in-place.

## Credits

This calculator is inspired by Hutton & Meijer's 1998 paper [Monadic Parsing in Haskell](https://www.cs.tufts.edu/comp/150FP/archive/graham-hutton/monadic-parsing-jfp.pdf) and a [Youtube tutorial by Tsoding](https://www.youtube.com/watch?v=N9RUqGYuGfw) about writing JSON parsers.
