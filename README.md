
# Haskell Calculator using Parser Combinators

This is an interactive calculator written in pure Haskell that supports a large number of operations, LaTeX formatting, and defining custom functions, despite only being in the order of about 500 lines of code.

## Features

- Define custom functions (eg `f x y = x^2+lny` then `10 / f 2 e == 2.0`). Note variable names are one character only.
- Automatic differentiation of custom functions (eg `fx=2xlnx`, then `f' (exp 2) == 6.0`)
- Automatic partial differentiation (`f x y = 3xy; df/dx 4 2` prints `6.0`)
- Produces simplified fractions or integers where possible, only returns decimals for transcendental functions
- Force decimal form via `R` function, rational via `Q`, int via `Z` (eg `R (1 / 2)`)
- Limit denominator of rational form via `Ql 10 pi == 22/7`
- Binary infix operators +, -, *, /, and right associative ** or ^
- Implicit multiplication - `2(3+4) == 2*(3+4)` and `sin pi cos pi == sin(pi)*cos(pi)`
- Factorial, permutations (`nPr`), combinations (`nCr` or `n choose r`)
- Common functions (sin/cos/tan, max/min, log/exp, gcd, etc)
- Scientific notation (eg 5.2e-3), pi and e constants, define own variables (`k = sqrt 2`)

## Drawbacks

- Order of operations is sometimes counterintuitive - `ln e^2 == (ln e)^2` and `-1^2 == 1`. It is intended to resemble Haskell, with improved unary minus behaviour. The priority is unary minus, brackets, function calls, then exponentiation, multiplication/division, addition/subtraction.
- There are bound to be bugs.
- Not all functions support automatic differentiation (every function except `!`, `nCr`, `nPr`, `gcd`)
- Inconsistent handling of division by zero and other errors (eg factorial of non-integer). This could be solved by adding a `NumERR String` value constructor that propagates through calculations into the REPL output.
- Derivatives of functions of user defined functions are broken. This is particularly difficult to implement as you could have `g x = f (ln x) 0 (x^2)` for `f` in `R^3->R`, for instance. The easiest way to fix this is to simply replace `f` with its definition and differentiate `g` as normal.
- Integration is unsupported.
- Algebra solving is unsupported.

## Examples

The calculator supports standard infix math notation used in programming, as well as LaTeX style markup in parallel. This works by defining a function `\frac` and allowing `{}` brackets to be used to denote a new expression for function application, so `frac 1 2` and `\frac{1}{2}` are parsed the same way. Many other LaTeX functions are supported.

```
> 2 / 1 * (3 - 4 + 2*2)
6
> atan 1 + atan 2 + atan 3 - acos -1
0.0
> 3\times \left(\frac{5}{9}\right)^2*\left(\frac{27}{5}\right)^1/2
5/2
> Z (max 5 6 + ln e)
7
> f x y = x^2+lny
defined f
> g x = f x (exp 2)
defined g
> 3 / Z (g 3)
3/11
> f x = xx; g x = f(lnx); g(exp2)
4.0
> e^2 * g' (exp 2)
4.0
```

## Inner workings

- `ParserCombinator.hs`: Parser combinators and `Parser` type definition.
- `Number.hs`: numeric type wrapper which allows the calculator to return fractions, integers and doubles, and which performs automatic differentiation.
- `Calculator.hs`: uses parser combinators and `Number` to evaluate expressions.
- `Main.hs`: REPL interface and some simple tests.

We define a monadic `Parser` type, which contains a function taking a string and returning a tuple containing a parsed token and the unparsed remainder of the string. This is a functor, so we can apply functions to the result of a parser (eg if we parse a sequence of digits we can run `int = fmap read digitsParser`). It is an applicative and monad, so we can write `mul = symb "*" *> return (*)` to get a function embedded in the parser type if the `*` symbol is found. Then we can apply binary functions using applicative syntax like `int >>= (\a -> mul <*> pure a <*> int)` to parse `3 * 4 -> Parser 12`.

This means we can evaluate in-place instead of constructing an abstract syntax tree, as the grammar used is context free. The expression parser then uses a sub-expression parser which handles numbers, constants, function application and bracketed expressions (recursively), and chains the sub-expressions by infix operators with a parser combinator `chainl1` which applies the functions in-place.

## Installation

Install GHC, clone this repository, then run `ghc Calculator.hs` in the main folder. The only dependency is Haskeline, a CLI library. I have a keybinding set where Meta+C opens the calculator in a terminal: `terminator --geometry=400x300-0+0 -e "bash -c '~/Programming/Haskell/ParserCombinator/Main'"`.

## Credits

This calculator is inspired by Hutton & Meijer's 1998 paper [Monadic Parsing in Haskell](https://www.cs.tufts.edu/comp/150FP/archive/graham-hutton/monadic-parsing-jfp.pdf) and a [Youtube tutorial by Tsoding](https://www.youtube.com/watch?v=N9RUqGYuGfw) about writing JSON parsers.
