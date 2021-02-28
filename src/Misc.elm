module Misc exposing (emptyFeed, emptyModel, now)

import DateTime exposing (DateTime)
import Dict
import Task exposing (Task)
import Time
import Types exposing (Feed, Model)


now : Task e DateTime
now =
    Task.map2
        (\z t ->
            (Time.posixToMillis t + DateTime.getTimezoneOffset z t)
                |> Time.millisToPosix
                |> DateTime.fromPosix
        )
        Time.here
        Time.now


emptyFeed : Feed
emptyFeed =
    { ids = Types.Waiting
    , posts = []
    , since = Nothing
    , ffs = Dict.empty
    }


emptyModel : Model
emptyModel =
    { sort = Types.SortDate
    , sortReverse = True
    , view = Nothing
    , top = emptyFeed
    , new = emptyFeed
    , best = emptyFeed
    }
