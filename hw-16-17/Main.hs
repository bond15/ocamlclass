module Main where
import SimPL 
import Parser 
import Control.Monad ( forever )


main :: IO ()
main = 
    forever $ do
        putStr ">> "
        s <- getLine
        print(runParser e s)




runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res,[])] -> res   
    [(_,rs)] -> error $  "leftovers: " ++ show rs
    _ -> error "Invalid"