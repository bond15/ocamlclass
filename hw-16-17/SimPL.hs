module SimPL where
import Control.Applicative ( Alternative((<|>)), some )
import Data.Char ( isAlpha )
import Ast
import Parser 


e :: Parser Expr
e = 
        x
    <|> b
    <|> parens op
    <|> i
    <|> ifel
    <|> lett


ifel :: Parser Expr
ifel = do
    reserved "if"
    b <- token e
    reserved "then"
    e1 <- token e
    reserved "else"
    e2 <- token e
    return $ If b e1 e2

lett :: Parser Expr
lett = do
    reserved "let"
    x' <- token x
    reserved "="
    e1 <- token e
    reserved "in"
    e2 <- token e
    return $ Let x' e1 e2

op :: Parser Expr
op = do
    b <- token bop
    e1 <- token e
    e2 <- token e
    return $ BinOp b e1 e2


bop :: Parser Bop
bop = 
        reserved "+"   >> result Add
    <|> (reserved "*"  >> result Mult)
    <|> (reserved "<=" >> result Leq)

 
x :: Parser Expr
x = oneOf "xyz" >>= result . Var 

i :: Parser Expr
i = number >>= result . Num


b :: Parser Expr
b =      reserved "true"  >> result (Buul True)
    <|> (reserved "false" >> result (Buul False))