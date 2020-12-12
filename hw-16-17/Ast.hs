module Ast where

data Bop = Add | Mult | Leq deriving Show

data Expr =     Var Char
            |   Num Int
            |   Buul Bool
            |   BinOp Bop Expr Expr
            |   Let Expr Expr Expr
            |   If Expr Expr Expr deriving Show
