module Models.Model exposing (..)

-- MODEL
-- We have a todo


type alias Todo =
    { title : String
    , completed : Bool
    , editing : Bool
    }



-- Filter state for the application


type FilterState
    = All
    | Active
    | Completed



-- Entire Application state's model


type alias Model =
    { todos : List Todo
    , todo : Todo
    , filter : FilterState
    }



-- Types of messages that can occur


type Msg
    = Add Todo
    | Delete Todo
    | Complete Todo
    | Filter FilterState
