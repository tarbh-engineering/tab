module View exposing (view)

import Calendar
import DateTime exposing (DateTime)
import Dict
import Element exposing (Attribute, Color, Element, centerX, centerY, column, el, fill, height, mouseOver, none, padding, paragraph, px, rgb255, row, spaceEvenly, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import Helpers.View exposing (cappedWidth, style, when, whenAttr, whenJust)
import Html exposing (Html)
import Material.Icons as Icons
import Material.Icons.Types exposing (Icon)
import Maybe.Extra exposing (unwrap)
import OneDark exposing (black, blue, darkRed)
import String.Conversions
import Types exposing (Feed, Model, Msg(..), Post)


view : Model -> Html Msg
view model =
    [ [ [ Element.image [ height <| px 50 ]
            { src = "./hb.svg", description = "" }
        , text "NEWSBALE"
            |> el
                [ Element.centerY
                , Font.size 35
                ]
        ]
            |> row
                [ spacing 10
                ]
      , [ Input.button
            [ Font.italic
            , hover
            , Font.underline |> whenAttr (model.sort == Types.SortComments)
            ]
            { onPress = Just <| SetSort Types.SortComments
            , label = text "sort by comments"
            }
        , Input.button
            [ Font.italic
            , hover
            , Font.underline |> whenAttr (model.sort == Types.SortDate)
            ]
            { onPress = Just <| SetSort Types.SortDate
            , label = text "sort by date"
            }
        , Input.button [ hover ]
            { onPress = Just <| SetSort model.sort
            , label =
                icon
                    (if model.sortReverse then
                        Icons.south

                     else
                        Icons.north
                    )
                    30
            }
        ]
            |> row [ spacing 10, Element.alignRight, Font.size 20 ]
            |> when (Maybe.Extra.isJust model.view)
      ]
        |> row [ width fill, spaceEvenly ]

    --, el [ height fill, width <| px 1, Background.color black ] none
    , model.view
        |> unwrap
            ([ viewCard Types.Top
             , viewCard Types.New
             , viewCard Types.Best
             ]
                |> row [ spacing 20, Font.size 25, centerX, centerY ]
            )
            (\v ->
                [ [ viewToggle Types.Top model.top model.view
                  , viewToggle Types.New model.new model.view
                  , viewToggle Types.Best model.best model.view
                  ]
                    |> column [ spacing 20, Font.size 25, centerX, Element.alignTop ]
                , case v of
                    Types.Top ->
                        viewRight model model.top FetchIds

                    Types.New ->
                        viewRight model model.new FetchNew

                    Types.Best ->
                        viewRight model model.best FetchBest
                ]
                    |> row [ width fill, height fill ]
            )
    , viewFooter model
    ]
        |> column
            [ cappedWidth 1500
            , height fill
            , centerX
            , Border.rounded 20
            , spacing 30
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
            , Background.color bg
            , Font.family [ Font.typeface "Abel" ]
            , Font.size 24
            ]


viewFooter : Model -> Element Msg
viewFooter model =
    [ Element.newTabLink
        [ Font.size 25
        , hover
        , Element.alignBottom
        ]
        { url = "https://tarbh.engineering/"
        , label =
            [ text "Made by "
            , text "Tarbh" |> el [ Font.bold ]
            , text " "
            , text "🐂"
            ]
                |> row []
        }
    , model.view
        |> whenJust
            (\v ->
                let
                    feed =
                        case v of
                            Types.Best ->
                                model.best

                            Types.New ->
                                model.new

                            Types.Top ->
                                model.top

                    msg =
                        case v of
                            Types.Top ->
                                FetchIds

                            Types.New ->
                                FetchNew

                            Types.Best ->
                                FetchBest
                in
                [ feed.posts
                    |> List.length
                    |> String.fromInt
                    |> text
                    |> el [ centerX, centerY ]
                    |> el
                        [ width <| px 60
                        , height <| px 60
                        , Background.color white
                        , Border.rounded 30
                        ]
                    |> when False
                , spinner 50
                    |> when
                        ((feed.ffs
                            |> Dict.isEmpty
                            |> not
                         )
                            || feed.ids
                            == Types.InProg
                        )
                , viewBtn "Refresh" (Just msg)
                , viewBtn "Load more" (Just Next)
                ]
                    |> row [ spacing 20, Font.size 30, height <| px 50 ]
            )
    ]
        |> row [ width fill, spaceEvenly ]


viewBtn : String -> Maybe msg -> Element msg
viewBtn txt msg =
    Input.button
        [ hover
        , padding 15
        , Border.rounded 5
        , Background.color blue
        , Element.mouseDown [ Background.color darkRed ]
        ]
        { onPress = msg, label = text txt }


viewToggle : Types.View -> Feed -> Maybe Types.View -> Element Msg
viewToggle v feed curr =
    let
        fnt =
            22

        icn =
            case v of
                Types.Best ->
                    Icons.star

                Types.New ->
                    Icons.local_fire_department

                Types.Top ->
                    Icons.trending_up

        txt =
            case v of
                Types.Best ->
                    "Best Stories"

                Types.New ->
                    "New Stories"

                Types.Top ->
                    "Top Stories"

        active =
            Just v == curr
    in
    Input.button
        [ Font.italic
        , Border.rounded 20
        , width <| px 210
        , height <| px 100
        , (if active then
            blue

           else
            white
          )
            |> Background.color
        , hover
            |> whenAttr (not active)
        , padding 20
        ]
        { onPress = Just <| SetView v
        , label =
            [ [ icon icn fnt
              , text txt
              ]
                |> row [ spacing 10, Font.size fnt ]
            , feed.since
                |> unwrap
                    (if feed.ids == Types.InProg then
                        spinner fnt

                     else
                        text "-"
                    )
                    (formatDateTime
                        >> text
                        >> el [ Font.size 18 ]
                    )
                |> el [ centerY ]
            ]
                |> column [ spacing 10, Font.italic, height fill ]
        }


viewCard : Types.View -> Element Msg
viewCard v =
    let
        txt =
            case v of
                Types.Best ->
                    "Best Stories"

                Types.New ->
                    "New Stories"

                Types.Top ->
                    "Top Stories"

        icn =
            case v of
                Types.Best ->
                    Icons.star

                Types.New ->
                    Icons.local_fire_department

                Types.Top ->
                    Icons.trending_up
    in
    Input.button
        [ mouseOver [ Border.glow darkRed 4, Font.color darkRed ]
        , Background.color blue
        , Border.shadow
            { offset = ( 1, 1 )
            , blur = 2
            , size = 0
            , color = black
            }
        ]
        { onPress = Just <| SetView v
        , label =
            [ icon icn 100
                |> el [ centerX, centerY ]
                |> el [ height fill, width fill ]
            , text txt
                |> el [ centerX, padding 30, Font.size 40 ]
            ]
                |> column [ width <| px 350, height <| px 500 ]
        }


viewRight : Model -> Feed -> Msg -> Element Msg
viewRight model feed msg =
    (if List.isEmpty feed.posts then
        --[ if inProg then
        --spinner
        --|> el [ centerX, centerY ]
        --|> when False
        --else
        [ Input.button [ centerX, centerY, Font.size 50, hover ]
            { onPress = Just msg
            , label = text "📰"
            }
            |> when False
        ]

     else
        [ feed.posts
            |> (case model.sort of
                    Types.SortComments ->
                        List.sortBy .commentsCount

                    Types.SortDate ->
                        List.sortWith
                            (\a b ->
                                DateTime.compare a.time b.time
                            )
               )
            |> (if model.sortReverse then
                    List.reverse

                else
                    identity
               )
            |> List.map viewPost
            |> column
                --|> List.map (\p -> ( String.fromInt p.id, viewPost p ))
                --|> Element.Keyed.column
                [ spacing 20
                , Element.paddingXY 20 0
                , Element.scrollbarY

                --, style "min-height" "auto"
                , height fill
                , width fill
                ]
        ]
    )
        |> column
            [ --, style "min-height" "auto"
              height fill
            , spacing 10
            , width fill

            --, Background.color white
            --, Background.gradient
            --{ angle = 0
            --, steps =
            --[ Element.rgb255 142 158 171
            --, Element.rgb255 238 242 243
            --]
            --}
            , Font.color black
            ]


viewPost : Post -> Element msg
viewPost p =
    [ [ Element.newTabLink
            [ hover
            , width fill
            ]
            { url =
                p.url
                    |> Maybe.withDefault ("https://news.ycombinator.com/item?id=" ++ String.fromInt p.id)
            , label =
                text p.title
            }
      ]
        |> paragraph [ Font.size 28, Font.bold ]
    , [ [ [ [ DateTime.getDay p.time |> String.fromInt
            , DateTime.getMonth p.time
                |> String.Conversions.fromMonth
            ]
                |> String.join " "
                |> text
          , DateTime.getYear p.time
                |> String.fromInt
                |> text
          , [ DateTime.getHours p.time
                |> String.fromInt
                |> String.padLeft 2 '0'
            , ":"
            , DateTime.getMinutes p.time
                |> String.fromInt
                |> String.padLeft 2 '0'
            ]
                |> String.join ""
                |> text
          ]
            |> row [ Font.italic, spacing 10 ]
        ]
            |> column [ spacing 10, width fill ]
      , Element.newTabLink
            [ hover
            , Font.underline
            ]
            { url = "https://news.ycombinator.com/item?id=" ++ String.fromInt p.id
            , label =
                text <|
                    String.fromInt p.commentsCount
                        ++ " comment"
                        ++ (if p.commentsCount == 1 then
                                ""

                            else
                                "s"
                           )
            }
      ]
        |> row [ width fill, spaceEvenly ]
    ]
        |> column
            [ width fill
            , spacing 10
            , Background.color white
            , padding 20
            , Border.width 2

            --, popIn
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
    style "animation" "rotation 0.7s infinite linear"


spinner : Int -> Element msg
spinner =
    icon Icons.refresh
        >> el [ rotate ]


fade : Element.Attr a b
fade =
    Element.alpha 0.7


hover : Attribute msg
hover =
    mouseOver [ fade ]


icon : Icon msg -> Int -> Element msg
icon ic n =
    ic n Material.Icons.Types.Inherit
        |> Element.html
        |> el []


popIn : Attribute msg
popIn =
    style "animation" "enter 0.3s"


bg : Color
bg =
    rgb255 251 244 233


white : Color
white =
    rgb255 255 255 255


formatDateTime : DateTime -> String
formatDateTime d =
    [ [ DateTime.getDay d
            |> String.fromInt
            |> String.padLeft 2 '0'
      , DateTime.getMonth d
            |> Calendar.monthToInt
            |> String.fromInt
            |> String.padLeft 2 '0'

      --|> String.Conversions.fromMonth
      , DateTime.getYear d
            |> String.fromInt
      ]
        |> String.join "/"

    --|> String.dropLeft 2
    , [ DateTime.getHours d
            |> String.fromInt
            |> String.padLeft 2 '0'
      , DateTime.getMinutes d
            |> String.fromInt
            |> String.padLeft 2 '0'
      , DateTime.getSeconds d
            |> String.fromInt
            |> String.padLeft 2 '0'
      ]
        |> String.join ":"
    ]
        |> String.join " "
