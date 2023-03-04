module Ast.Literal exposing (characterParser, floatParser, intParser, literalParser, stringParser)

-- import Ast.Common exposing (Literal(..))

import Ast.Helpers exposing (..)
import Char
import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Num
import Hex
import Optimized exposing (Literal(..))



{-
   type Literal
       = IntNum Int
       | FloatNum Float
       | Chr Char
       | Str String
       | Boolean Bool
-}


literalParser : Parser s Literal
literalParser =
    choice
        [ FloatNum <$> floatParser
        , IntNum <$> intParser
        , Chr <$> characterParser
        , Str <$> stringParser
        ]



-- literalParser : Parser s Literal
-- literalParser =
--     choice
--         [ Float <$> floatParser
--         , Integer <$> intParser
--         , Character <$> characterParser
--         , String <$> stringParser
--         ]


characterParser : Parser s Char
characterParser =
    between_ (Combine.string "'")
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


stringParser : Parser s String
stringParser =
    let
        singleString =
            Combine.string "\"" *> regex "(\\\\\\\\|\\\\\"|[^\"\n])*" <* Combine.string "\""

        multiString =
            String.concat <$> (Combine.string "\"\"\"" *> many (regex "[^\"]*") <* Combine.string "\"\"\"")
    in
    multiString <|> singleString


intParser : Parser s Int
intParser =
    choice
        [ (optional "" (Combine.string "-")
            |> map String.append
          )
            <*> (Combine.string "0x" *> regex "[0-9a-fA-F]+")
            >>= (String.toLower
                    >> Hex.fromString
                    >> Result.map succeed
                    >> Result.withDefault (fail "Not a number")
                )
        , Combine.Num.int
        ]


floatParser : Parser s Float
floatParser =
    Combine.Num.float
