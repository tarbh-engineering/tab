module Main exposing (main)

import Browser
import Types exposing (Model, Msg)
import Update exposing (update)
import View exposing (view)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { posts = []
      , postIds = []
      , state = False
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
