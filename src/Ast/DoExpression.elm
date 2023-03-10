module Ast.DoExpression exposing
    ( term
    , DoExpression(..), ExpressionD, ExpressionH(..), dexpression, doArrow, doExpression, expressionD
    )

{-| This module exposes parsers for Elm expressions.


# Types

@docs Expression


# Parsers

@docs expression


# Expression

@docs term

-}

import Ast.BinOp exposing (..)
import Ast.Common exposing (..)
import Ast.Helpers exposing (..)
import Ast.Pattern exposing (pattern)
import Char
import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Num
import Dict exposing (Dict)
import Hex
import List exposing (singleton)
import List.Extra exposing (break)
import String


type Collect a
    = Cont a
    | Stop a


type DoExpression e
    = DoLet String e
    | DoArrow String e
    | DoRaw e


{-| Representations for Elm's expressions.
-}
type ExpressionH v b
    = Character Char
    | String String
    | Integer Int
    | Float Float
    | Variable v
    | List (List (ExpressionH v b))
    | Tuple (List (ExpressionH v b))
    | Access (ExpressionH v b) (List Name)
    | AccessFunction Name
    | Record (List ( Name, ExpressionH v b ))
    | RecordUpdate Name (List ( Name, ExpressionH v b ))
    | If (ExpressionH v b) (ExpressionH v b) (ExpressionH v b)
    | Let (List ( PatternH b, ExpressionH v b )) (ExpressionH v b)
    | Case (ExpressionH v b) (List ( PatternH b, ExpressionH v b ))
    | Lambda (PatternH b) (ExpressionH v b)
    | Application (ExpressionH v b) (ExpressionH v b)
    | BinOp (ExpressionH v b) (ExpressionH v b) (ExpressionH v b)
    | Do (List (DoExpression (ExpressionH v b)))


type alias Expr =
    ExpressionH Name Name


type alias ExpressionD =
    Expr


mkVar =
    Variable


character : Parser s Expr
character =
    Character
        <$> between_ (Combine.string "'")
                (((Combine.string "\\" *> regex "(n|t|r|\\\\|x..)")
                    >>= (\a ->
                            case String.uncons a of
                                Just ( 'n', "" ) ->
                                    succeed '\n'

                                Just ( 't', "" ) ->
                                    succeed '\t'

                                Just ( 'r', "" ) ->
                                    succeed '\x0D'

                                Just ( '\\', "" ) ->
                                    succeed '\\'

                                Just ( '0', "" ) ->
                                    succeed '\x00'

                                Just ( 'x', hex ) ->
                                    hex
                                        |> String.toLower
                                        |> Hex.fromString
                                        |> Result.map Char.fromCode
                                        |> Result.map succeed
                                        |> Result.withDefault (fail "Invalid charcode")

                                Just other ->
                                    fail ("No such character as \\" ++ toString other)

                                Nothing ->
                                    fail "No character"
                        )
                 )
                    <|> anyChar
                )


string : Parser s Expr
string =
    let
        singleString =
            String
                <$> (Combine.string "\"" *> regex "(\\\\\\\\|\\\\\"|[^\"\n])*" <* Combine.string "\"")

        multiString =
            (String << String.concat)
                <$> (Combine.string "\"\"\"" *> many (regex "[^\"]*") <* Combine.string "\"\"\"")
    in
    multiString <|> singleString


integer : Parser s Expr
integer =
    Integer <$> Combine.Num.int


float : Parser s Expr
float =
    Float <$> Combine.Num.float


access : Parser s Expr
access =
    Access <$> variable <*> many1 (Combine.string "." *> loName)


accessFunction : Parser s Expr
accessFunction =
    AccessFunction <$> (Combine.string "." *> loName)



-- variableL =
--     Variable
--         <$> choice
--                 [ singleton <$> loName
--                 , sepBy1 (Combine.string ".") upName
--                 , singleton <$> parens operator
--                 , singleton <$> parens (Combine.regex ",+")
--                 , singleton <$> emptyTuple
--                 ]


{-| I don't want a list of names here - if I need a list later I can split the name by "."
-}



-- variable : Parser s Expr


variable =
    Variable
        <$> choice
                [ loName

                -- , sepBy1 (Combine.string ".") upName
                , upName
                , parens operator
                , parens (Combine.regex ",+")
                , emptyTuple
                ]


list : OpTable -> Parser s Expr
list ops =
    lazy <|
        \() ->
            List <$> brackets (commaSeparated_ <| expression ops)


tuple : OpTable -> Parser s Expr
tuple ops =
    lazy <|
        \() ->
            Tuple
                <$> (parens (commaSeparated_ <| expression ops)
                        >>= (\a ->
                                case a of
                                    [ _ ] ->
                                        fail "No single tuples"

                                    anyOther ->
                                        succeed anyOther
                            )
                    )


record : OpTable -> Parser s Expr
record ops =
    lazy <|
        \() ->
            Record <$> braces (commaSeparated ((,) <$> loName <*> (symbol "=" *> expression ops)))


simplifiedRecordL =
    lazy <|
        \() ->
            Record <$> braces (commaSeparated ((\a -> ( a, Variable [ a ] )) <$> loName))


simplifiedRecord : Parser s Expr
simplifiedRecord =
    lazy <|
        \() ->
            Record <$> braces (commaSeparated ((\a -> ( a, Variable a )) <$> loName))


recordUpdate : OpTable -> Parser s Expr
recordUpdate ops =
    lazy <|
        \() ->
            RecordUpdate
                <$> (symbol "{" *> loName)
                <*> (symbol "|"
                        *> commaSeparated ((,) <$> loName <*> (symbol "=" *> expression ops))
                        <* Combine.string "}"
                    )


letExpression : OpTable -> Parser s Expr
letExpression ops =
    lazy <|
        \() ->
            Let
                <$> (symbol_ "let" *> many1 (letBinding ops))
                <*> (symbol "in" *> expression ops)


letBinding : OpTable -> Parser s ( Pattern, Expr )
letBinding ops =
    lazy <|
        \() ->
            (,)
                -- <$> (between_ whitespace <| expression ops)
                <$> (between_ whitespace <| pattern)
                <*> (symbol "=" *> expression ops)


ifExpression : OpTable -> Parser s Expr
ifExpression ops =
    lazy <|
        \() ->
            If
                <$> (symbol "if" *> expression ops)
                <*> (symbol "then" *> expression ops)
                <*> (symbol "else" *> expression ops)


caseExpression : OpTable -> Parser s Expr
caseExpression ops =
    let
        binding indent =
            lazy <|
                \() ->
                    (,)
                        <$> (exactIndentation indent *> pattern)
                        <*> (symbol "->" *> expression ops)
    in
    lazy <|
        \() ->
            Case
                <$> (symbol "case" *> expression ops)
                <*> (whitespace
                        *> Combine.string "of"
                        *> lookAhead countIndent
                        >>= (\indent ->
                                many1 (binding indent)
                            )
                    )


{-| TODO Implement
-}
expressionD : OpTable -> Parser s Expr
expressionD ops =
    let
        binding indent =
            lazy <|
                \() ->
                    doExpression ops indent
    in
    lazy <|
        \() ->
            Do
                <$> (whitespace
                        *> Combine.string "do"
                        *> lookAhead countIndent
                        >>= (\indent ->
                                many1 (binding indent)
                            )
                    )


doArrow ops =
    (DoArrow <$> loName) <*> (symbol "<-" *> expression ops)


doExpression : OpTable -> Int -> Parser s (DoExpression Expr)
doExpression ops indent =
    exactIndentation indent
        *> choice
            [ doArrow ops
            , DoRaw <$> expression ops
            ]


countIndent : Parser s Int
countIndent =
    newline *> spaces >>= (String.filter (\char -> char == ' ') >> String.length >> succeed)


lambda : OpTable -> Parser s Expr
lambda ops =
    lazy <|
        \() ->
            Lambda
                <$> (symbol "\\" *> pattern
                     -- many (between_ spaces <| term ops)
                    )
                <*> (symbol "->" *> expression ops)


application : OpTable -> Parser s Expr
application ops =
    lazy <|
        \() ->
            withColumn (\l -> chainl (Application <$ spacesOrIndentedNewline (l + 1)) (term ops))


spacesOrIndentedNewline : Int -> Parser s ()
spacesOrIndentedNewline indentation =
    lazy <|
        \() ->
            or (spaces_ *> succeed ())
                (countIndent
                    >>= (\column ->
                            if column < indentation then
                                fail "Arguments have to be at least the same indentation as the function"

                            else
                                succeed ()
                        )
                )


operatorOrAsBetween : Parser s String
operatorOrAsBetween =
    lazy <|
        \() ->
            between_ whitespace <| operator <|> symbol_ "as"


successOrEmptyList : Parser s (List a) -> Parser s (List a)
successOrEmptyList p =
    lazy <| \() -> choice [ p, succeed [] ]


binary : OpTable -> Parser s Expr
binary ops =
    lazy <|
        \() ->
            let
                next =
                    operatorOrAsBetween
                        >>= (\op ->
                                lazy <|
                                    \() ->
                                        or (Cont <$> application ops) (Stop <$> expression ops)
                                            >>= (\e ->
                                                    case e of
                                                        Cont t ->
                                                            (::) ( op, t ) <$> successOrEmptyList next

                                                        Stop ex ->
                                                            succeed [ ( op, ex ) ]
                                                )
                            )
            in
            application ops
                >>= (\e -> successOrEmptyList next >>= (\eops -> split ops 0 e eops))


{-| A parser for term
-}
term : OpTable -> Parser s Expr
term ops =
    lazy <|
        \() ->
            choice
                [ access
                , variable
                , accessFunction
                , string
                , float
                , integer
                , character
                , parens (between_ whitespace (expression ops))
                , list ops
                , tuple ops
                , recordUpdate ops
                , record ops
                , simplifiedRecord
                ]


{-| A parser for Elm expressions.
-}
expression : OpTable -> Parser s Expr
expression ops =
    lazy <|
        \() ->
            choice
                [ expressionD ops
                , binary ops
                , letExpression ops
                , caseExpression ops
                , ifExpression ops
                , lambda ops
                ]


dexpression =
    expression


op : OpTable -> String -> ( Assoc, Int )
op ops n =
    Dict.get n ops
        |> Maybe.withDefault ( L, 9 )


assoc : OpTable -> String -> Assoc
assoc ops n =
    Tuple.first <| op ops n


level : OpTable -> String -> Int
level ops n =
    Tuple.second <| op ops n


hasLevel : OpTable -> Int -> ( String, Expr ) -> Bool
hasLevel ops l ( n, _ ) =
    level ops n == l


split : OpTable -> Int -> Expr -> List ( String, Expr ) -> Parser s Expr
split ops l e eops =
    case eops of
        [] ->
            succeed e

        _ ->
            findAssoc ops l eops
                >>= (\assoc ->
                        sequence (splitLevel ops l e eops)
                            >>= (\es ->
                                    let
                                        ops_ =
                                            List.filterMap
                                                (\x ->
                                                    if hasLevel ops l x then
                                                        Just (Tuple.first x)

                                                    else
                                                        Nothing
                                                )
                                                eops
                                    in
                                    case assoc of
                                        R ->
                                            joinR es ops_

                                        _ ->
                                            joinL es ops_
                                )
                    )


splitLevel : OpTable -> Int -> Expr -> List ( String, Expr ) -> List (Parser s Expr)
splitLevel ops l e eops =
    case break (hasLevel ops l) eops of
        ( lops, ( _, e_ ) :: rops ) ->
            split ops (l + 1) e lops :: splitLevel ops l e_ rops

        ( lops, [] ) ->
            [ split ops (l + 1) e lops ]


joinL : List Expr -> List String -> Parser s Expr
joinL es ops =
    case ( es, ops ) of
        ( [ e ], [] ) ->
            succeed e

        ( a :: b :: remE, op :: remO ) ->
            joinL (BinOp (mkVar op) a b :: remE) remO

        _ ->
            fail ""


joinR : List Expr -> List String -> Parser s Expr
joinR es ops =
    case ( es, ops ) of
        ( [ e ], [] ) ->
            succeed e

        ( a :: b :: remE, op :: remO ) ->
            joinR (b :: remE) remO
                |> andThen
                    (\e ->
                        succeed (BinOp (mkVar op) a e)
                    )

        _ ->
            fail ""


findAssoc : OpTable -> Int -> List ( String, Expr ) -> Parser s Assoc
findAssoc ops l eops =
    let
        lops =
            List.filter (hasLevel ops l) eops

        assocs =
            List.map (assoc ops << Tuple.first) lops

        error issue =
            let
                operators =
                    List.map Tuple.first lops |> String.join " and "
            in
            "conflicting " ++ issue ++ " for operators " ++ operators
    in
    if List.all ((==) L) assocs then
        succeed L

    else if List.all ((==) R) assocs then
        succeed R

    else if List.all ((==) N) assocs then
        case assocs of
            [ _ ] ->
                succeed N

            _ ->
                fail <| error "precedence"

    else
        fail <| error "associativity"
