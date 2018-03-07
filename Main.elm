module Main exposing (..)

import Html.Styled as Html exposing (Html)
import Array
import Update
import Puzzle exposing (..)
import View exposing (Msg(..))


type alias Model =
    { puzzle : Puzzle
    , puzzles : List (List Card)
    }


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model =
            { puzzle = { set = Array.empty, solution = [] }
            , puzzles = puzzles
            }
                |> nextPuzzle
        , update = update
        , view = view
        }


update : Msg -> Model -> Model
update msg ({ puzzle } as model) =
    case msg of
        NextPuzzle ->
            nextPuzzle model

        Internal m ->
            { model | puzzle = Update.update m puzzle }


view : Model -> Html Msg
view { puzzle, puzzles } =
    View.view puzzle


nextPuzzle : Model -> Model
nextPuzzle ({ puzzles } as old) =
    case puzzles of
        head :: tail ->
            { puzzle = { set = Array.fromList head, solution = [] }
            , puzzles = tail
            }

        _ ->
            old


puzzles : List (List Card)
puzzles =
    [ simple, turingMachine ]


simple : List Card
simple =
    [ ( "b", "ca" )
    , ( "a", "ab" )
    , ( "ca", "a" )
    , ( "abc", "c" )
    ]


turingMachine : List Card
turingMachine =
    ( ".#", "." ++ star "#c110#" ++ "." )
        :: ( ".a.#.?", "?" )
        :: List.map (\( a, b ) -> ( "." ++ star a, star b ++ "." ))
            (List.map id [ "0", "1", "#" ]
                ++ [ ( "c1", "0c" )
                   , ( "c0", "1n" )
                   , ( "n0", "0n" )
                   , ( "n1", "1n" )
                   , ( "n#", "a#" )
                   , ( "c#", "1a#" )
                   , ( "a0", "a" )
                   , ( "a1", "a" )
                   , ( "0a", "a" )
                   , ( "1a", "a" )
                   ]
            )


id : String -> Card
id x =
    ( x, x )


star : String -> String
star =
    String.toList
        >> List.intersperse '.'
        >> String.fromList
