
import ParserCombinator
import GHC.Float (int2Double)
import Control.Applicative ((<|>))

expr :: Parser Double
expr = term `chainl1` addop
term = factor `chainl1` mulop
factor = number <|> brackets
brackets = symb "(" *> expr <* symb ")"

addop :: Parser (Double -> Double -> Double)
addop = (symb "+" *> return (+)) <|> (symb "-" *> return (-))

mulop :: Parser (Double -> Double -> Double)
mulop = (symb "*" *> return (*)) <|> (symb "/" *> return (/))

main = interact $ unlines . map (show . fst . (!!0) . apply expr) . lines
