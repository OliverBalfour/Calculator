
# Haskell Calculator using Parser Combinators

This is an interactive calculator written in pure Haskell that supports a large number of operations despite only being in the order of about 300 lines of code.

The parser combinators and parser definition reside in `ParserCombinator.hs`, while all calculator-specific code, tests, and the interactive interface reside in `Calculator.hs`.

## Features

- Left associative binary infix operators +, -, *, /
- Right associative binary infix operator ** or ^
- Unary minus
- Implicit multiplication - `2(3+4) == 2*(3+4)` and `sin pi cos pi == sin(pi)*cos(pi)`
- Factorial, permutations (`nPr`), combinations (`nCr` or `n choose r`)
- Binary and unary functions (trigonometry, max/min, log/exp, etc)
- pi and e constants
- Scientific notation (eg 5.2e-3)

## Drawbacks

- The order of operations is not consistent with common sense in some instances - eg `ln e^2 == (ln e) ^ 2` and `sin 2pi == pi sin 2`.

This is because function and operator application have different mechanisms, and so either all functions are evaluated followed by all operators, or vice versa. The solution is to rewrite the order of operations code to unify functions and operators, and use fixity and precedence declarations. This way the order of operations can become brackets -> exponentiation -> implicit multiplication (eg 2e -> 2*e) -> function application -> multiplication, etc.

- All numbers are converted to double precision floating point numbers, resulting in rounding errors and no fractional/exact value answers.

Every number is converted to a double, meaning the factorial function must first round the number or use the gamma function, and the answers are not given as fractions where appropriate. Fixing this will be quite difficult as Haskell is very strict regarding numeric types. To fix this, the type of an expression must default to an integer, then degrade to rational if division is required (or the other operand to a binary function is rational, etc), then to a double if a special function or constant is required.

Note: a fix for this is in progress. Most expressions will be output as the simplest form.

## Examples

The calculator supports standard infix math notation used in programming, as well as LaTeX style markup in parallel. This works by defining a function `\frac` and allowing `{}` brackets to be used to denote a new expression for function application, so `frac 1 2` and `\frac{1}{2}` are parsed the same way.

```
> 2 / 1.0 * (3 - 4 + 2*2)
6.0
> atan 1 + atan 2 + atan 3 - acos -1
0.0
> 3\times \left(\frac{5}{9}\right)^2*\left(\frac{27}{5}\right)^1
5.0
> max 5 6 + ln e
7.0
```
## Credits

This calculator is inspired by Hutton & Meijer's 1998 paper [Monadic Parsing in Haskell](https://www.cs.tufts.edu/comp/150FP/archive/graham-hutton/monadic-parsing-jfp.pdf) and a [Youtube tutorial by Tsoding](https://www.youtube.com/watch?v=N9RUqGYuGfw) about writing JSON parsers.
