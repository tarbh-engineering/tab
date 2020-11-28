module Types exposing (HNPost, Model, Msg(..))

import Http


type alias Model =
    { posts : List HNPost
    , postIds : List Int
    , state : Bool
    }


type Msg
    = PostCb (Result Http.Error HNPost)
    | IdsCb (Result Http.Error (List Int))
    | FetchIds
    | ClearHN


type alias HNPost =
    { id : Int
    , title : String
    , commentsCount : Int
    , url : Maybe String
    }
