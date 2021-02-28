module Types exposing (Feed, IdsResult, Model, Msg(..), Post, Remote(..), Sort(..), View(..))

import DateTime exposing (DateTime)
import Dict exposing (Dict)
import Http


type alias Model =
    { sort : Sort
    , sortReverse : Bool
    , view : Maybe View
    , top : Feed
    , new : Feed
    , best : Feed
    }


type Msg
    = TopPostCb Int (Result Http.Error Post)
    | NewPostCb Int (Result Http.Error Post)
    | BestPostCb Int (Result Http.Error Post)
    | IdsCb (Result Http.Error IdsResult)
    | BestCb (Result Http.Error IdsResult)
    | NewCb (Result Http.Error IdsResult)
    | FetchIds
    | FetchBest
    | FetchNew
    | Next
    | SetSort Sort
    | SetView View


type Remote a
    = Waiting
    | InProg
    | Success a


type View
    = Top
    | Best
    | New


type Sort
    = SortDate
    | SortComments


type alias IdsResult =
    { ids : List Int
    , time : DateTime
    }


type alias Post =
    { id : Int
    , title : String
    , type_ : String
    , commentsCount : Int
    , url : Maybe String
    , time : DateTime
    }


type alias Feed =
    { ids : Remote (List Int)
    , posts : List Post
    , ffs : Dict Int Int
    , since : Maybe DateTime
    }
