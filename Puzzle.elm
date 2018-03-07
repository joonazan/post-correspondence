module Puzzle exposing (..)


type alias Puzzle =
    { set : List Card
    , solution : List Card
    }


type alias Card =
    ( String, String )


concatCards : List Card -> Card
concatCards cards =
    let
        top =
            List.map Tuple.first cards
                |> String.concat

        bottom =
            List.map Tuple.second cards
                |> String.concat
    in
        ( top, bottom )
