module NumberParser exposing (fromString)

import Char


fromString : String -> Float
fromString input =
    String.toFloat
        (String.filter isNumber input)
        |> Result.withDefault 0


isNumber : Char -> Bool
isNumber c =
    case c of
        '.' ->
            True

        _ ->
            Char.isDigit c
