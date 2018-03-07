module Update exposing (update)

import Puzzle exposing (..)
import View exposing (Internal(..))


update : Internal -> Puzzle -> Puzzle
update msg ({ set, solution } as model) =
    { model
        | solution =
            case msg of
                AddCard card ->
                    let
                        newSolution =
                            solution ++ [ card ]
                    in
                        if valid newSolution then
                            newSolution
                        else
                            solution

                Solution x ->
                    List.take x solution
    }


valid : List Card -> Bool
valid cards =
    let
        ( top, bottom ) =
            concatCards cards
    in
        String.startsWith top bottom || String.startsWith bottom top
