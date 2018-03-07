module Update exposing (update)

import Array
import Maybe.Extra as Maybe
import Puzzle exposing (..)
import View exposing (Internal(..))


update : Internal -> Puzzle -> Puzzle
update msg ({ set, solution } as model) =
    { model
        | solution =
            case msg of
                Palette x ->
                    Array.get x set
                        |> Maybe.map (\x -> solution ++ [ x ])
                        |> Maybe.filter goodSoFar
                        |> Maybe.withDefault solution

                Solution x ->
                    List.take x solution
    }


goodSoFar : List Card -> Bool
goodSoFar cards =
    let
        ( top, bottom ) =
            concatCards cards
    in
        String.startsWith top bottom || String.startsWith bottom top
