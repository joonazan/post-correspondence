module View exposing (Msg(..), view)

import Array exposing (Array)
import Css exposing (..)
import Css.Colors exposing (gray)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Model exposing (..)


type Msg
    = Palette Int
    | Solution Int


view : Model -> Html Msg
view { set, solution } =
    let
        problem =
            [ drawSet set
            , drawSolution solution (min alen blen)
            ]

        ( a, b ) =
            concatCards solution

        ( alen, blen ) =
            ( String.length a, String.length b )

        all =
            if alen /= 0 && alen == blen then
                problem ++ [ text "Good Job!" ]
            else
                problem
    in
    div [ css [ fontSize large ] ] all


drawSolution : List Card -> Int -> Html Msg
drawSolution cards same =
    List.indexedMap (,) cards
        |> List.foldl drawCardInSolution ( [], same, same )
        |> (\( x, _, _ ) -> cardRow <| List.reverse x)


drawCardInSolution :
    ( Int, ( String, String ) )
    -> ( List (Html Msg), Int, Int )
    -> ( List (Html Msg), Int, Int )
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


drawSet : Array Card -> Html Msg
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
            , margin (Css.em 0.2)
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
