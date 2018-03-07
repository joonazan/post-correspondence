module View exposing (Msg(..), Model, Internal(..), view)

import Css exposing (..)
import Css.Colors exposing (gray)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Puzzle exposing (..)


type alias Model =
    { puzzle : Puzzle
    , puzzles : List (List Card)
    }


type Msg
    = Internal Internal
    | NextPuzzle Model


type Internal
    = AddCard Card
    | Solution Int


view : Puzzle -> Maybe Model -> Html Msg
view { set, solution } next =
    let
        problem =
            [ drawSet set
            , hr [] []
            , drawSolution solution (min alen blen)
            ]
                |> List.map (map Internal)

        ( a, b ) =
            concatCards solution

        ( alen, blen ) =
            ( String.length a, String.length b )

        isSolved =
            alen /= 0 && alen == blen

        all =
            if isSolved then
                problem
                    ++ [ p [] [ text "Good Job!" ]
                       , next
                            |> Maybe.map (\p -> button [ onClick (NextPuzzle p) ] [ text "NEXT" ])
                            |> Maybe.withDefault (p [] [ text "All puzzles solved!" ])
                       ]
            else
                problem
    in
        div [ css [ fontSize large ] ] all


drawSolution : List Card -> Int -> Html Internal
drawSolution cards same =
    List.indexedMap (,) cards
        |> List.foldl drawCardInSolution ( [], same, same )
        |> (\( x, _, _ ) -> cardRow <| List.reverse x)


drawCardInSolution :
    ( Int, ( String, String ) )
    -> ( List (Html Internal), Int, Int )
    -> ( List (Html Internal), Int, Int )
drawCardInSolution ( i, ( top, bottom ) ) ( acc, lettersTop, lettersBottom ) =
    ( drawCard (Solution i) (split lettersTop top) (split lettersBottom bottom) :: acc
    , lettersTop - String.length top
    , lettersBottom - String.length bottom
    )


split : Int -> String -> List (Html msg)
split x s =
    [ text (String.left x s)
    , span [ css [ color gray ] ] [ text (String.dropLeft x s) ]
    ]


drawSet : List Card -> Html Internal
drawSet =
    List.map drawCardInSet >> cardRow


cardRow : List (Html msg) -> Html msg
cardRow =
    div
        [ css
            [ displayFlex
            , flexWrap wrap
            , margin (Css.em 1)
            ]
        ]


drawCardInSet : Card -> Html Internal
drawCardInSet (( top, bottom ) as card) =
    drawCard (AddCard card) [ text top ] [ text bottom ]


drawCard : msg -> List (Html msg) -> List (Html msg) -> Html msg
drawCard effect top bottom =
    div
        [ css
            [ display inlineFlex
            , flexDirection column
            , margin (Css.em 0.11)
            , hover
                [ boxShadow3 (Css.em 0.2) (Css.em 0.1) gray
                ]
            ]
        , onClick effect
        ]
        [ drawHalf top, drawHalf bottom ]


drawHalf : List (Html msg) -> Html msg
drawHalf s =
    div
        [ css
            [ borderStyle solid
            , padding (Css.em 0.5)
            ]
        ]
        s
