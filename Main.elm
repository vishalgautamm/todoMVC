module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, keyCode, onInput, onCheck, onClick)
import Json.Decode as Json


-- MAIN PROGRAM


main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }



-- MODEL


type alias Todo =
    { title : String
    , completed : Bool
    , editing : Bool
    , identifier : Int
    }


type FilterState
    = All
    | Active
    | Completed


type alias Model =
    { todos : List Todo
    , todo : Todo
    , filter : FilterState
    , nextIdentifier : Int
    }


newTodo : Todo
newTodo =
    { title = ""
    , completed = False
    , editing = False
    , identifier = 0
    }


initialModel : Model
initialModel =
    { todos =
        [ { title = "Learn elm programming"
          , completed = False
          , editing = False
          , identifier = 1
          }
        ]
    , todo = { newTodo | identifier = 2 }
    , filter = All
    , nextIdentifier = 3
    }



-- UPDATE


type Msg
    = Add
    | Delete Todo
    | Complete Todo
    | Uncomplete Todo
    | UpdatedField String
    | Filter FilterState
    | Clear


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add ->
            { model
                | todos = model.todo :: model.todos
                , todo = { newTodo | identifier = model.nextIdentifier }
                , nextIdentifier = model.nextIdentifier + 1
            }

        Delete todo ->
            { model | todos = List.filter (\targetTodo -> todo.identifier /= targetTodo.identifier) model.todos }

        Complete todo ->
            let
                updatedTodo thisTodo =
                    if thisTodo.identifier == todo.identifier then
                        { todo | completed = True }
                    else
                        thisTodo
            in
                { model
                    | todos = List.map updatedTodo model.todos
                }

        Uncomplete todo ->
            let
                updatedTodo thisTodo =
                    if thisTodo.identifier == todo.identifier then
                        { todo | completed = False }
                    else
                        thisTodo
            in
                { model
                    | todos = List.map updatedTodo model.todos
                }

        UpdatedField str ->
            let
                todo =
                    model.todo

                updatedTodo =
                    { todo | title = str }
            in
                { model | todo = updatedTodo }

        Filter filterState ->
            { model | filter = filterState }

        Clear ->
            { model
                | todos = List.filter (\todo -> todo.completed == False) model.todos
            }


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "Not the right keycode"
    in
        on "keydown" (keyCode |> Json.andThen isEnter)



-- VIEW


todoView : Todo -> Html Msg
todoView todo =
    let
        handleComplete =
            case todo.completed of
                True ->
                    (\_ -> Uncomplete todo)

                False ->
                    (\_ -> Complete todo)
    in
        li [ classList [ ( "completed", todo.completed ) ] ]
            [ div [ class "view" ]
                [ input
                    [ class "toggle"
                    , type_ "checkbox"
                    , checked todo.completed
                    , onCheck handleComplete
                    ]
                    []
                , label [] [ text todo.title ]
                , button
                    [ class "destroy"
                    , onClick (Delete todo)
                    ]
                    []
                ]
            ]


filterItemView : Model -> FilterState -> Html Msg
filterItemView model filterState =
    li []
        [ a
            [ classList [ ( "selected", (model.filter == filterState) ) ]
            , href "#"
            , onClick (Filter filterState)
            ]
            [ text (toString filterState) ]
        ]


filteredTodos : Model -> List Todo
filteredTodos model =
    let
        matchesFilter =
            case model.filter of
                All ->
                    (\_ -> True)

                Active ->
                    (\todo -> todo.completed == False)

                Completed ->
                    (\todo -> todo.completed == True)
    in
        List.filter matchesFilter model.todos


view : Model -> Html Msg
view model =
    div []
        [ section [ class "todoapp" ]
            [ header [ class "header" ]
                [ h1 [] [ text "Todo Elm" ]
                , input
                    [ class "new-todo"
                    , placeholder "What needs to be done"
                    , value model.todo.title
                    , autofocus True
                    , onEnter Add
                    , onInput UpdatedField
                    ]
                    []
                ]
            , section [ class "main" ]
                [ ul [ class "todo-list" ]
                    (List.map todoView (filteredTodos model))
                ]
            , footer [ class "footer" ]
                [ span [ class "todo-count" ]
                    [ strong [] [ text (toString (List.length (List.filter (\todo -> todo.completed == False) model.todos))) ]
                    , text " items left"
                    ]
                , ul [ class "filters" ]
                    [ filterItemView model All
                    , filterItemView model Active
                    , filterItemView model Completed
                    ]
                , button
                    [ class "clear-completed"
                    , onClick Clear
                    ]
                    [ text "Clear completed" ]
                ]
            ]
        ]
