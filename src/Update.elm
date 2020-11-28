module Update exposing (update)

import Http
import Json.Decode as Decode exposing (Decoder)
import Result.Extra exposing (unpack)
import Types exposing (HNPost, Model, Msg(..))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchIds ->
            if List.isEmpty model.postIds then
                ( { model | state = True }, fetchIds )

            else
                ( { model | postIds = List.drop 20 model.postIds }
                , model.postIds
                    |> List.take 20
                    |> List.map getPost
                    |> Cmd.batch
                )

        PostCb res ->
            res
                |> unpack
                    (\_ ->
                        ( model, Cmd.none )
                    )
                    (\data ->
                        ( { model | posts = data :: model.posts }
                        , Cmd.none
                        )
                    )

        IdsCb res ->
            res
                |> unpack
                    (\_ ->
                        ( model, Cmd.none )
                    )
                    (\data ->
                        ( { model | postIds = List.drop 20 data }
                        , data
                            |> List.take 20
                            |> List.map getPost
                            |> Cmd.batch
                        )
                    )

        ClearHN ->
            ( { model
                | posts = []
                , postIds = []
                , state = False
              }
            , Cmd.none
            )


fetchIds : Cmd Msg
fetchIds =
    Http.get
        { url = "https://hacker-news.firebaseio.com/v0/topstories.json"
        , expect = Http.expectJson IdsCb <| Decode.list Decode.int
        }


getPost : Int -> Cmd Msg
getPost id =
    Http.get
        { url = "https://hacker-news.firebaseio.com/v0/item/" ++ String.fromInt id ++ ".json"
        , expect = Http.expectJson PostCb decodePost
        }


decodePost : Decoder HNPost
decodePost =
    Decode.map4 HNPost
        (Decode.field "id" Decode.int)
        (Decode.field "title" Decode.string)
        --(Decode.field "time_ago" Decode.string)
        --(Decode.field "by" Decode.string)
        (Decode.field "descendants" Decode.int
            |> Decode.maybe
            |> Decode.map (Maybe.withDefault 0)
        )
        (Decode.field "url" Decode.string
            |> Decode.maybe
            |> Decode.map
                (Maybe.map
                    (\url ->
                        if String.startsWith "item?id=" url then
                            "https://news.ycombinator.com/" ++ url

                        else
                            url
                    )
                )
        )
