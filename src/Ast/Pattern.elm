module Ast.Pattern exposing (applicationFromList, applicationToList, pattern)

import Ast.Common exposing (..)
import Ast.Helpers exposing (..)
import Ast.Literal exposing (..)
import Combine exposing (..)


{-| Parses a pattern for matching
-}
pattern : Parser s Pattern
pattern =
    lazy <|
        \() ->
            cycle
                [ asParser
                , consParser
                , appParser
                , terminalParser
                ]


{-| Parses an `as` keyword used to assign another name to an expression in pattern
-}
asParser : Parser s Pattern -> Parser s Pattern
asParser next =
    lazy <|
        \() ->
            choice
                [ (PAs <$> next <*> (symbol "as" *> varName)) >>= (asParser << succeed)
                , next
                ]


{-| Parses a cons (::) operator recursively
-}
consParser : Parser s Pattern -> Parser s Pattern
consParser next =
    lazy <|
        \() ->
            choice
                [ PCons
                    <$> next
                    <* symbol "::"
                    <*> consParser next
                , next
                ]


{-| Parses an application in a pattern. Used both for tagged union constructors
and for function definitions in let..in
-}
appParser : Parser s Pattern -> Parser s Pattern
appParser next =
    lazy <|
        \() ->
            withLocation
                (\l ->
                    chainl
                        ((\a b ->
                            PApplication a b
                         )
                            <$ spacesOrIndentedNewline (l.column + 1)
                        )
                        next
                )


terminalParser : Parser s Pattern -> Parser s Pattern
terminalParser next =
    lazy <|
        \() ->
            choice
                [ wildcardParser
                , varParser
                , constructorParser
                , PLiteral <$> literalParser
                , PRecord <$> (braces <| commaSeparated_ <| loName)
                , PTuple <$> tupleParser next
                , PList <$> listParser next
                , parens next
                ]


{-| Parsers a distinguishable wildcard variable specified as an underscore
-}
wildcardParser : Parser s Pattern
wildcardParser =
    always PWildcard <$> wildcard


{-| Parser a variable in a pattern
-}
varParser : Parser s Pattern
varParser =
    PVariable <$> funName


{-| Parses a constructor function for a tagged union without its parameters
-}
constructorParser : Parser s Pattern
constructorParser =
    PConstructor <$> upName


applicationToList : Pattern -> List Pattern
applicationToList application =
    case application of
        PApplication left right ->
            applicationToList left ++ [ right ]

        other ->
            [ other ]


applicationFromList : Pattern -> List Pattern -> Pattern
applicationFromList acc l =
    case l of
        right :: rest ->
            applicationFromList (PApplication acc right) rest

        _ ->
            acc
