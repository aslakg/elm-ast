module Ast
    exposing
        ( parseExpression
        , parseDExpression
        , parseStatement
        , parseDStatement
        , parseOpTable
        , parseModule
        , parse
            , parse2
        )

{-| This module exposes functions for parsing Elm code.


# Parsers

@docs parseExpression, parseStatement, parseDExpression, parseDStatement, parseOpTable, parseModule, parse

-}

import Combine exposing (end, (<*))
import Ast.BinOp exposing (OpTable, operators)
import Ast.Expression exposing (Expression, expression)
import Ast.Statement exposing (Statement, statement, statements, opTable)
import Ast.DoExpression as AD exposing (dexpression)
import Ast.DoStatement as DS

{-| Parse an Elm expression.
-}
parseExpression : OpTable -> String -> Result (Combine.ParseErr ()) (Combine.ParseOk () Expression)
parseExpression ops =
    Combine.parse (expression ops <* end)

{-| Parse an Elm expression extended with do syntax -}        
parseDExpression : OpTable -> String -> Result (Combine.ParseErr ()) (Combine.ParseOk () AD.Expression)
parseDExpression ops =
    Combine.parse (dexpression ops <* end)

{-| Parse an Elm statement.
-}
parseStatement : OpTable -> String -> Result (Combine.ParseErr ()) (Combine.ParseOk () Statement)
parseStatement ops =
    Combine.parse (statement ops <* end)

{-| Parse an Elm statement, extended with do syntax -}        
parseDStatement : OpTable -> String -> Result (Combine.ParseErr ()) (Combine.ParseOk () (DS.Statement AD.Expression))
parseDStatement ops =
    Combine.parse (DS.statement ops AD.dexpression AD.term <* end)

{-| Parse an OpTable from a module.
-}
parseOpTable : OpTable -> String -> Result (Combine.ParseErr ()) (Combine.ParseOk () OpTable)
parseOpTable ops =
    Combine.parse (opTable ops)


{-| Parse an Elm module.
-}
parseModule : OpTable -> String -> Result (Combine.ParseErr ()) (Combine.ParseOk () (List Statement))
parseModule ops =
    Combine.parse (statements ops)


{-| Parse an Elm module, scanning for infix declarations first.
-}
parse : String -> Result (Combine.ParseErr ()) (Combine.ParseOk () (List Statement))
parse input =
    case parseOpTable operators input of
        Ok ( state, stream, ops ) ->
            parseModule ops input

        Err e ->
            (Err e)


{-| Parse an Elm module.
-}
parseModule2 : OpTable -> String -> Result (Combine.ParseErr ()) (Combine.ParseOk () (List (DS.Statement AD.Expression)))
parseModule2 ops =
    Combine.parse (DS.statements ops AD.dexpression AD.term)


{-| Parse an Elm module, scanning for infix declarations first.
-}
parse2 : String -> Result (Combine.ParseErr ()) (Combine.ParseOk () (List (DS.Statement AD.Expression)))
parse2 input =
    case parseOpTable operators input of
        Ok ( state, stream, ops ) ->
            parseModule2 ops input

        Err e ->
            (Err e)
                
