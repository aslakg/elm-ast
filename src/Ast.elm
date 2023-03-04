module Ast exposing (parse2, parseBareExpression, parsePattern, parseExpression, parseStatement, parseDExpression, parseDStatement, parseOpTable, parseModule, parse)

{-| This module exposes functions for parsing Elm code.


# Parsers

@docs parse2, parseBareExpression, parsePattern, parseExpression, parseStatement, parseDExpression, parseDStatement, parseOpTable, parseModule, parse

-}

import Ast.BinOp exposing (OpTable, operators)
import Ast.Common exposing (Pattern)
import Ast.DoExpression as AD exposing (dexpression)
import Ast.DoStatement as DS
import Ast.Expression exposing (Expression, expression)
import Ast.Pattern as P
import Ast.Statement exposing (Statement, opTable, statement, statements)
import Combine exposing ((<*), end)


{-| Parse an Elm expression.
-}
parseExpression : OpTable -> String -> Result (Combine.ParseErr ()) (Combine.ParseOk () Expression)
parseExpression ops =
    Combine.parse (expression ops <* end)


{-| Parse an Elm expression extended with do syntax
-}
parseDExpression : OpTable -> String -> Result (Combine.ParseErr ()) (Combine.ParseOk () AD.Expression)
parseDExpression ops =
    Combine.parse (dexpression ops <* end)


{-| Parse an Elm pattern.
-}
parsePattern : String -> Result (Combine.ParseErr ()) (Combine.ParseOk () Pattern)
parsePattern =
    Combine.parse (P.pattern <* end)


{-| Parse an Elm statement.
-}
parseStatement : OpTable -> String -> Result (Combine.ParseErr ()) (Combine.ParseOk () Statement)
parseStatement ops =
    Combine.parse (statement ops <* end)


{-| Parse an Elm statement, extended with do syntax
-}
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
            Err e


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
            Err e


{-| Parse an Elm module, scanning for infix declarations first.
-}
parseBareExpression : String -> Result (Combine.ParseErr ()) Expression
parseBareExpression input =
    case parseOpTable operators input of
        Ok ( state, stream, ops ) ->
            case parseExpression ops input of
                Ok ( _, _, x ) ->
                    Ok x

                Err e ->
                    Err e

        Err e ->
            Err e
