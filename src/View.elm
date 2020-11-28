module View exposing (view)

import Element exposing (Attribute, Element, centerX, centerY, column, el, fill, height, padding, paragraph, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Helpers.View exposing (cappedWidth, style)
import Html exposing (Html)
import Material.Icons as Icons
import Material.Icons.Types exposing (Icon)
import OneDark exposing (black, white)
import Types exposing (Model, Msg(..))


view : Model -> Html Msg
view model =
    [ viewLeft model
    , viewRight model
    ]
        |> row
            [ cappedWidth 1500
            , height fill
            , centerX
            , shadow
            , Border.rounded 20
            ]
        |> el [ padding 50, width fill, height fill ]
        |> Element.layoutWith
            { options =
                [ Element.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            [ width fill
            , height fill
            ]


viewLeft : Model -> Element Msg
viewLeft _ =
    [ [ Element.image [ height <| px 50 ]
            { src = "./hb.svg", description = "" }
      , text "NEWSBALE"
            |> el
                [ Font.color <| Element.rgb255 255 255 255
                , Element.centerY
                , Font.family [ Font.typeface "Courier" ]
                , Font.size 35
                ]
      ]
        |> row
            [ spacing 10
            , centerX
            ]
    , [ text "Sort by"
      , [ Input.button [ centerX, centerY, Font.size 50 ]
            { onPress = Just FetchIds
            , label = text "comments"
            }
        , Input.button [ centerX, centerY, Font.size 50 ]
            { onPress = Just FetchIds
            , label = text "date"
            }
        ]
            |> column []
      ]
        |> column []
    , Element.newTabLink
        [ centerX
        , Font.underline
        , Font.size 25
        , Font.color white
        , Element.mouseOver [ Font.color OneDark.magenta ]
        ]
        { url = "https://tarbh.engineering/"
        , label = text "Made by Tarbh"
        }
    ]
        |> column
            [ height fill
            , Element.spaceEvenly
            , padding 20
            , Background.color OneDark.gutterGrey
            , width <| px 350
            , Font.color black
            , Border.roundEach
                { topLeft = 20
                , bottomLeft = 20
                , bottomRight = 0
                , topRight = 0
                }
            ]


viewRight : Model -> Element Msg
viewRight model =
    (if List.isEmpty model.posts then
        [ if model.state then
            icon Icons.refresh 50
                |> el [ centerX, centerY, rotate ]

          else
            Input.button [ centerX, centerY, Font.size 50 ]
                { onPress = Just FetchIds
                , label = text "📰"
                }
        ]

     else
        [ model.posts
            |> List.map
                (\p ->
                    [ [ text p.title ]
                        |> paragraph [ Font.color black ]

                    -- paragraph inside newTabLink is broken
                    , Element.newTabLink
                        [ Element.mouseOver [ fade ]
                        , width <| px 50
                        ]
                        { url =
                            p.url
                                |> Maybe.withDefault ("https://news.ycombinator.com/item?id=" ++ String.fromInt p.id)
                        , label = text "🌍"
                        }
                    , Element.newTabLink
                        [ Element.mouseOver [ fade ]
                        , width <| px 50
                        ]
                        { url = "https://news.ycombinator.com/item?id=" ++ String.fromInt p.id
                        , label = text <| "📜 " ++ String.fromInt p.commentsCount
                        }
                    ]
                        |> row
                            [ width fill
                            ]
                )
            |> column
                [ spacing 20
                , Element.paddingXY 20 0
                , Element.scrollbarY

                --, style "min-height" "auto"
                , height fill
                , width fill
                ]
        , [ Input.button [ Element.mouseOver [ fade ] ]
                { onPress = Just FetchIds, label = text "load more" }
          , Input.button [ Element.mouseOver [ fade ] ]
                { onPress = Just ClearHN, label = text "clear" }
          ]
            |> row [ centerX, spacing 20, Font.underline, Font.size 30 ]
        ]
    )
        |> column
            [ --, style "min-height" "auto"
              height fill
            , spacing 10
            , 4 |> Element.fillPortion |> width
            , padding 10
            , Background.color white
            , model.posts
                |> List.length
                |> String.fromInt
                |> text
                |> el [ padding 15, Element.alignBottom, Font.size 25 ]
                |> Element.onRight

            --, Background.gradient
            --{ angle = 0
            --, steps =
            --[ Element.rgb255 142 158 171
            --, Element.rgb255 238 242 243
            --]
            --}
            , Font.color black
            , Border.roundEach
                { topLeft = 0
                , bottomLeft = 0
                , bottomRight = 20
                , topRight = 20
                }
            ]


shadow : Attribute msg
shadow =
    Border.shadow
        { offset = ( 3, 3 )
        , blur = 3
        , size = 0
        , color = black
        }


rotate : Attribute msg
rotate =
    style
        "animation"
        "rotation 0.7s infinite linear"


fade : Element.Attr a b
fade =
    Element.alpha 0.7


icon : Icon msg -> Int -> Element msg
icon ic n =
    ic n Material.Icons.Types.Inherit
        |> Element.html
        |> el []
