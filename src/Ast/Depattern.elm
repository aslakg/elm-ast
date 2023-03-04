module Ast.Depattern exposing (..)

import Ast.Common exposing (..)
import IDSupply exposing (..)



-- import Ast.DoExpression exposing (Expression(..))
-- type DoExpression e
--     = DoLet String e
--     | DoArrow String e
--     | DoRaw e
-- type Expression
--     = Character Char
--     | String String
--     | Integer Int
--     | Float Float
--     | Variable (List Name)
--     | List (List Expression)
--     | Tuple (List Expression)
--     | Access Expression (List Name)
--     | AccessFunction Name
--     | Record (List ( Name, Expression ))
--     | RecordUpdate Name (List ( Name, Expression ))
--     | If Expression Expression Expression
--     | Let (List ( Expression, Expression )) Expression
--     | Case Expression (List ( Pattern, Expression ))
--     | Lambda (List Expression) Expression
--     | Application Expression Expression
--     | BinOp Expression Expression Expression
--     | Do (List (DoExpression Expression))
-- depattern :: P.Canonical -> Can.Expr -> Env.Optimizer (String, Can.Expr)
{-
   This changes a pattern to an expression.
   So f.ex. with lam (Person foo) body
   you call depattern (Person foo) body

   and get back
    ("x1", case x1 of
              (Person foo) -> body
    )

    where x1 is a fresh variable. Because body refers to "foo", and not x1, this
    works beautifully.
    And this is generic enough to be used for let and lambda. It's not, however,
    doing anything for case itself - that's not its purpose.
-}
-- depattern : IDSupply -> Pattern -> Expression -> ( String, Expression, IDSupply )


depattern : IDSupply -> (String -> a) -> (a -> List ( Pattern, b ) -> b) -> Pattern -> b -> ( String, b, IDSupply )
depattern varCounter var caseExpr pattern expr =
    let
        -- caseExpr e branches =
        --     Case e branches
        -- var name =
        --     varConstructor  name
        caseify =
            let
                name =
                    fresh varCounter
            in
            ( name
            , caseExpr (var name) [ ( pattern, expr ) ]
            , leftSupply varCounter
            )
    in
    case pattern of
        PVariable name ->
            ( name, expr, varCounter )

        PWildcard ->
            let
                name =
                    fresh varCounter
            in
            ( name, expr, leftSupply varCounter )

        _ ->
            caseify
