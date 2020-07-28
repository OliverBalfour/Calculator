
import ParserCombinator

expr :: Parser Int
expr = term `chainl1` addop

term = factor `chainl1` mulop

factor = first
  integer
  (do symb "("
      n <- expr
      symb ")"
      return n)

addop :: Parser (Int -> Int -> Int)
addop = first
  (do symb "+"
      return (+))
  (do symb "-"
      return (-))

mulop :: Parser (Int -> Int -> Int)
mulop = first
  (do symb "*"
      return (*))
  (do symb "/"
      return (div))

main = putStrLn . show $ apply expr " 11 - 20 * (3 - 2) + 4 "
