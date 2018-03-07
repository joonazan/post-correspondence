module Puzzle exposing (..)

import Array exposing (Array)


type alias Puzzle =
    { set : Array Card
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
