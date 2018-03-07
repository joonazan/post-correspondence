module View exposing (Msg(..), Internal(..), view)

import Array exposing (Array)
import Css exposing (..)
import Css.Colors exposing (gray)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Puzzle exposing (..)


type Msg
    = Internal Internal
    | NextPuzzle


type Internal
    = Palette Int
    | Solution Int


view : Puzzle -> Html Msg
view { set, solution } =
    let
        problem =
            [ drawSet set
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
                       , button [ onClick NextPuzzle ] [ text "NEXT" ]
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
    ( (drawCard (split lettersTop top) (split lettersBottom bottom)
        |> map (always <| Solution i)
      )
        :: acc
    , lettersTop - String.length top
    , lettersBottom - String.length bottom
    )


split : Int -> String -> List (Html msg)
split x s =
    [ text (String.left x s)
    , span [ css [ color gray ] ] [ text (String.dropLeft x s) ]
    ]


drawSet : Array Card -> Html Internal
drawSet cards =
    Array.toList cards
        |> List.indexedMap drawCardInSet
        |> cardRow
        |> map Palette


cardRow : List (Html msg) -> Html msg
cardRow =
    div
        [ css
            [ displayFlex
            , margin (Css.em 1)
            ]
        ]


drawCardInSet : Int -> Card -> Html Int
drawCardInSet index ( top, bottom ) =
    drawCard [ text top ] [ text bottom ]
        |> map (always index)


drawCard : List (Html ()) -> List (Html ()) -> Html ()
drawCard top bottom =
    div
        [ css
            [ display inlineFlex
            , flexDirection column
            , margin (Css.em 0.11)
            , hover
                [ boxShadow3 (Css.em 0.2) (Css.em 0.1) gray
                ]
            ]
        , onClick ()
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
