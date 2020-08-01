
import Calculator
import Control.Monad.Trans
import System.Console.Haskeline

repl :: CalcState -> InputT IO ()
repl st = do
  minput <- getInputLine "> "
  case minput of
    Nothing -> outputStrLn "Exiting"
    Just "exit" -> outputStrLn "Exiting"
    Just "quit" -> outputStrLn "Exiting"
    Just input ->
      let (output, st') = prettyExpr (input, st)
      in (liftIO $ putStrLn output) >> (repl st')

main :: IO ()
main = do
  -- test
  putStrLn "Calculator"
  runInputT defaultSettings (repl emptyState)

-- test :: IO [()]
-- test = mapM printFailed (filter testFails tests) where
--   printFailed = \(cs, out) -> putStrLn $ "error: " ++ cs ++ " /= " ++ show out
--   testFails (cs, out) = show out /= fst (prettyExpr (cs, emptyState))
--   tests = [
--     ("1+2", NumZ 3), ("1.0 + 2.1", NumQ (31:%10)), ("   0. + 0.1   + 0.2 -  .3", NumZ 0), -- basic arithmetic, spaces
--     ("5 + -4 - (5 - 4)", NumZ 0), ("-pi", NumR (-pi)), ("--pi", NumR pi), -- unary minus
--     ("(3 + 4) / (5 - 4)", NumZ 7), ("[5] - {4} * [3 - 2]", NumZ 1), ("5 * (3 - 2)", NumZ 5), -- brackets
--     ("2 /1.0 * (3*pi - 4 + 2*2)+e", NumR$6*pi+exp 1), -- constants
--     ("3  - 2 * 3", NumZ(-3)), ("(2**3)^4", NumZ 4096), ("2^3^2",NumZ 512), ("2/3*3", NumZ 2), -- associativity
--     ("3\\times \\left(\\frac{5}{9}\\right)^2*\\left(\\frac{4}{9}\\right)^1", NumQ (100:%243)), -- latex
--     ("2pi(3 + 4)", NumR$14.0 * pi), ("5^2\\frac(1) 5", NumZ 5), ("2 / 3(2 + 4)", NumZ 4), -- implicit mult
--     ("sin 0", NumR 0.0), ("5max 7 4", NumZ 35), ("max max max 1 20 3 4", NumZ 20), -- functions
--     ("\\frac{1}{2}", NumQ (1:%2)), ("ln e", NumR 1.0), ("log 100", NumR 2.0), ("\\log_2 8", NumR 3.0),
--     ("10e2", NumZ 1000), ("9e-2", NumQ (9:%100)), ("3.1415e4", NumZ 31415), -- floating point, scientific notation
--     ("5!", NumZ 120), ("10 choose 4", NumZ 210), ("10C6", NumZ 210), -- permutations and combinations
--     ("fx=xx;gx=f(lnx);g(exp2)", NumR 4.0), ("fx=xx;gx=f(lnx);e^2g'(e^2)", NumR 4.0) -- user functions
--     ]
