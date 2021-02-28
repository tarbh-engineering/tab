module Update exposing (update)

import DateTime
import Dict
import Http
import Json.Decode as JD exposing (Decoder)
import Maybe.Extra exposing (unwrap)
import Misc
import Result.Extra exposing (unpack)
import Task exposing (Task)
import Time
import Types exposing (Model, Msg(..), Post)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchIds ->
            launchTop model

        FetchBest ->
            launchBest model

        FetchNew ->
            launchNew model

        Next ->
            model.view
                |> unwrap
                    ( model, Cmd.none )
                    (\v ->
                        case v of
                            Types.Top ->
                                fetchTopIds model

                            Types.New ->
                                fetchNewIds model

                            Types.Best ->
                                fetchBestIds model
                    )

        TopPostCb id res ->
            let
                model_ =
                    { model
                        | top =
                            model.top
                                |> (\r ->
                                        { r
                                            | ffs =
                                                r.ffs
                                                    |> Dict.remove id
                                        }
                                   )
                    }
            in
            res
                |> unpack
                    (\_ ->
                        ( model_, Cmd.none )
                    )
                    (\data ->
                        ( if data.type_ == "job" then
                            model_

                          else
                            { model_
                                | top =
                                    model_.top
                                        |> (\r ->
                                                { r | posts = data :: r.posts }
                                           )
                            }
                        , Cmd.none
                        )
                    )

        NewPostCb id res ->
            let
                model_ =
                    { model
                        | new =
                            model.new
                                |> (\r ->
                                        { r
                                            | ffs =
                                                r.ffs
                                                    |> Dict.remove id
                                        }
                                   )
                    }
            in
            res
                |> unpack
                    (\_ ->
                        ( model_, Cmd.none )
                    )
                    (\data ->
                        ( if data.type_ == "job" then
                            model_

                          else
                            { model_
                                | new =
                                    model_.new
                                        |> (\r ->
                                                { r | posts = data :: r.posts }
                                           )
                            }
                        , Cmd.none
                        )
                    )

        BestPostCb id res ->
            let
                model_ =
                    { model
                        | best =
                            model.best
                                |> (\r ->
                                        { r
                                            | ffs =
                                                r.ffs
                                                    |> Dict.remove id
                                        }
                                   )
                    }
            in
            res
                |> unpack
                    (\_ ->
                        ( model_
                        , Cmd.none
                        )
                    )
                    (\data ->
                        ( if data.type_ == "job" then
                            model_

                          else
                            { model_
                                | best =
                                    model_.best
                                        |> (\r ->
                                                { r | posts = data :: r.posts }
                                           )
                            }
                        , Cmd.none
                        )
                    )

        IdsCb res ->
            let
                model_ =
                    { model
                        | top =
                            model.top
                                |> (\r ->
                                        { r
                                            | ids = Types.Waiting
                                        }
                                   )
                    }
            in
            res
                |> unpack
                    (\_ ->
                        ( model_, Cmd.none )
                    )
                    (\data ->
                        { model_
                            | top =
                                model_.top
                                    |> (\r ->
                                            { r
                                                | ids = Types.Success data.ids
                                                , posts = []
                                                , since = Just data.time
                                            }
                                       )
                        }
                            |> fetchTopIds
                    )

        BestCb res ->
            let
                model_ =
                    { model
                        | best =
                            model.best
                                |> (\r ->
                                        { r
                                            | ids = Types.Waiting
                                        }
                                   )
                    }
            in
            res
                |> unpack
                    (\_ ->
                        ( model_, Cmd.none )
                    )
                    (\data ->
                        { model_
                            | best =
                                model_.best
                                    |> (\r ->
                                            { r
                                                | ids = Types.Success data.ids
                                                , posts = []
                                                , since = Just data.time
                                            }
                                       )
                        }
                            |> fetchBestIds
                    )

        NewCb res ->
            let
                model_ =
                    { model
                        | new =
                            model.new
                                |> (\r ->
                                        { r
                                            | ids = Types.Waiting
                                        }
                                   )
                    }
            in
            res
                |> unpack
                    (\_ ->
                        ( model_, Cmd.none )
                    )
                    (\data ->
                        { model_
                            | new =
                                model_.new
                                    |> (\r ->
                                            { r
                                                | ids = Types.Success data.ids
                                                , posts = []
                                                , since = Just data.time
                                            }
                                       )
                        }
                            |> fetchNewIds
                    )

        SetSort s ->
            ( { model
                | sort = s
                , sortReverse =
                    if s == model.sort then
                        not model.sortReverse

                    else
                        True
              }
            , Cmd.none
            )

        SetView v ->
            let
                model_ =
                    { model
                        | view = Just v
                    }
            in
            case v of
                Types.Top ->
                    if shouldLoad model.top then
                        launchTop model_

                    else
                        ( model_, Cmd.none )

                Types.New ->
                    if shouldLoad model.new then
                        launchNew model_

                    else
                        ( model_, Cmd.none )

                Types.Best ->
                    if shouldLoad model.best then
                        launchBest model_

                    else
                        ( model_, Cmd.none )


launchBest : Model -> ( Model, Cmd Msg )
launchBest model =
    ( { model
        | best = Misc.emptyFeed |> (\r -> { r | ids = Types.InProg })
      }
    , request "beststories"
        |> Task.attempt BestCb
    )


launchNew : Model -> ( Model, Cmd Msg )
launchNew model =
    ( { model
        | new = Misc.emptyFeed |> (\r -> { r | ids = Types.InProg })
      }
    , request "newstories"
        |> Task.attempt NewCb
    )


launchTop : Model -> ( Model, Cmd Msg )
launchTop model =
    ( { model
        | top = Misc.emptyFeed |> (\r -> { r | ids = Types.InProg })
      }
    , request "topstories"
        |> Task.attempt IdsCb
    )


shouldLoad : Types.Feed -> Bool
shouldLoad f =
    f.ids == Types.Waiting


fetchTopIds : Model -> ( Model, Cmd Msg )
fetchTopIds model =
    let
        ids =
            case model.top.ids of
                Types.Waiting ->
                    []

                Types.InProg ->
                    []

                Types.Success xs ->
                    xs

        next =
            List.take 20 ids
    in
    ( { model
        | top =
            model.top
                |> (\r ->
                        { r
                            | ids = Types.Success <| List.drop 20 ids
                            , ffs =
                                next
                                    |> List.foldr
                                        (\x -> Dict.insert x 0)
                                        r.ffs
                        }
                   )
      }
    , next
        |> List.map (getPost TopPostCb)
        |> Cmd.batch
    )


fetchNewIds : Model -> ( Model, Cmd Msg )
fetchNewIds model =
    let
        ids =
            case model.new.ids of
                Types.Waiting ->
                    []

                Types.InProg ->
                    []

                Types.Success xs ->
                    xs

        next =
            List.take 20 ids
    in
    ( { model
        | new =
            model.new
                |> (\r ->
                        { r
                            | ids = Types.Success <| List.drop 20 ids
                            , ffs =
                                next
                                    |> List.foldr
                                        (\x -> Dict.insert x 0)
                                        r.ffs
                        }
                   )
      }
    , next
        |> List.map (getPost NewPostCb)
        |> Cmd.batch
    )


fetchBestIds : Model -> ( Model, Cmd Msg )
fetchBestIds model =
    let
        ids =
            case model.best.ids of
                Types.Waiting ->
                    []

                Types.InProg ->
                    []

                Types.Success xs ->
                    xs

        next =
            List.take 20 ids
    in
    ( { model
        | best =
            model.best
                |> (\r ->
                        { r
                            | ids = Types.Success <| List.drop 20 ids
                            , ffs =
                                next
                                    |> List.foldr
                                        (\x -> Dict.insert x 0)
                                        r.ffs
                        }
                   )
      }
    , next
        |> List.map (getPost BestPostCb)
        |> Cmd.batch
    )


getPost : (Int -> Result Http.Error Post -> msg) -> Int -> Cmd msg
getPost msg id =
    Http.get
        { url = "https://hacker-news.firebaseio.com/v0/item/" ++ String.fromInt id ++ ".json"
        , expect = Http.expectJson (msg id) decodePost
        }


decodePost : Decoder Post
decodePost =
    JD.map6 Post
        (JD.field "id" JD.int)
        (JD.field "title" JD.string)
        (JD.field "type" JD.string)
        --(JD.field "time_ago" JD.string)
        --(JD.field "by" JD.string)
        (JD.field "descendants" JD.int
            |> JD.maybe
            |> JD.map (Maybe.withDefault 0)
        )
        (JD.field "url" JD.string
            |> JD.maybe
            |> JD.map
                (Maybe.map
                    (\url ->
                        if String.startsWith "item?id=" url then
                            "https://news.ycombinator.com/" ++ url

                        else
                            url
                    )
                )
        )
        (JD.field "time" JD.int
            |> JD.map
                ((*) 1000
                    >> Time.millisToPosix
                    >> DateTime.fromPosix
                )
        )


request : String -> Task Http.Error Types.IdsResult
request key =
    Task.map2 Types.IdsResult
        (Http.task
            { method = "GET"
            , headers = []
            , url = "https://hacker-news.firebaseio.com/v0/" ++ key ++ ".json"
            , body = Http.emptyBody
            , resolver =
                Http.stringResolver
                    (\response ->
                        case response of
                            Http.BadUrl_ u ->
                                Http.BadUrl u
                                    |> Err

                            Http.Timeout_ ->
                                Http.Timeout
                                    |> Err

                            Http.NetworkError_ ->
                                Http.NetworkError
                                    |> Err

                            Http.BadStatus_ metadata _ ->
                                Http.BadStatus metadata.statusCode
                                    |> Err

                            Http.GoodStatus_ _ body_ ->
                                body_
                                    |> JD.decodeString (JD.list JD.int)
                                    |> Result.mapError (JD.errorToString >> Http.BadBody)
                    )
            , timeout = Nothing
            }
        )
        Misc.now
