module Main exposing (..)

import Array
import Html.Styled as Html
import Maybe.Extra as Maybe
import Model exposing (..)
import View exposing (Msg(..))


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = { set = Array.fromList easyPuzzle, solution = [] }
        , update = update
        , view = View.view
        }


easyPuzzle : List Card
easyPuzzle =
    [ ( "b", "ca" )
    , ( "a", "ab" )
    , ( "ca", "a" )
    , ( "abc", "c" )
    ]


update : Msg -> Model -> Model
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
