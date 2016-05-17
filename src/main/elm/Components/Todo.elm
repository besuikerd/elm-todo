port module Components.Todo exposing (Model, Msg, update, view, init)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json exposing (Decoder)

main : Program Never
main = App.program
  {
      init = init ! []
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
  }

port focus : String -> Cmd msg

type Msg =
    NoOp
  | Add
  | UpdateField String
  | EditTask Int Bool
  | UpdateTaskDescription Int String
  | ToggleTask Int
  | DeleteTask Int
  | SetFilter Filter

type alias Model = {
      tasks: List Task
    , filter: Filter
    , field: String
    , uid: Int
  }

type alias Task  = {
      id: Int
    , description: String
    , completed: Bool
    , editing: Bool
  }

type Filter = Completed | All | NotCompleted

filters: List Filter
filters = [Completed, All, NotCompleted]

isVisible : Filter -> Task -> Bool
isVisible filter task = case filter of
  Completed -> task.completed
  All -> True
  NotCompleted -> not task.completed


filterName : Filter -> String
filterName filter = case filter of
  Completed -> "Completed"
  All -> "All"
  NotCompleted -> "Not Completed"

init : Model
init = {
        tasks = []
      , filter = All
      , field = ""
      , uid = 0
  }

createTask: String -> Int -> Task
createTask description uid = {
      id = uid
    , description = description
    , completed = False
    , editing = False
  }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp -> model ! []
    Add -> { model |
          uid = model.uid + 1
        , field = ""
        , tasks = case model.field of
          "" -> model.tasks
          str -> model.tasks ++ [createTask model.field model.uid]
      } ! []
    UpdateField value -> { model | field = value } ! []
    EditTask id editing ->
      let
        updateTask task = if task.id == id then { task | editing = editing } else task
      in
        { model | tasks = List.map updateTask model.tasks }
          ! if editing then
            [ focus <| taskIdCss id ]
          else
            []
    UpdateTaskDescription id description ->
      let
        updateTask task = if task.id == id then { task | description = description } else task
      in
        { model | tasks = List.map updateTask model.tasks } ! []
    ToggleTask id ->
      let
        updateTask task = if task.id == id then { task | completed = not task.completed } else task
      in
        { model | tasks = List.map updateTask model.tasks } ! []
    DeleteTask id ->
      { model | tasks = List.filter (\task -> task.id /= id) model.tasks} ! []
    SetFilter filter ->
      { model | filter = filter } ! []

view : Model -> Html Msg
view model = div [] [
      todoHeader
    , todoForm model.field
    , todoFilters model.filter filters
    , filteredTodoList model.filter model.tasks
  ]

todoHeader : Html Msg
todoHeader = div [] [
    h1 [] [text "Todo"]
  ]

todoForm : String -> Html Msg
todoForm fieldValue = div
  [class ""]
  [
    input [
        class "form-control"
      , onInput UpdateField
      , onEnter NoOp Add
      , value fieldValue
    ] []
  ]

todoFilters : Filter -> List Filter -> Html Msg
todoFilters activeFilter filters =
  let
    filterButton filter =
      div [class "btn-group"] [
        button [
            onClick <| SetFilter filter
          , class ((if filter == activeFilter then "active " else "") ++ "btn btn-primary")
        ] [text <| filterName filter]
      ]
  in
  div
    [class "btn-group btn-group-justified"]
    (List.map filterButton filters)

filteredTodoList: Filter -> List Task -> Html Msg
filteredTodoList filter tasks =
  todoList <| List.filter (isVisible filter) tasks

todoList : List Task -> Html Msg
todoList tasks = table
  [class "tasks table table-striped"]
  [ tbody []
      (List.map  (\item -> todoItem item) tasks)
  ]

todoItem : Task -> Html Msg
todoItem task = tr [] [
      td [class "column-wrapped"] [
        input[
            type' "checkbox"
          , checked task.completed
          , onClick <| ToggleTask task.id
        ] []
      ]
    , td [] [todoField task]
    , td [class "column-wrapped"] [
      span [
          class "glyphicon glyphicon-remove"
        , onClick <| DeleteTask task.id
      ] []
    ]
  ]

todoField : Task -> Html Msg
todoField task = if task.editing then
    input [
        class "form-control"
      , value task.description
      , id (taskIdCss task.id)
      , onEnter NoOp (EditTask task.id False)
      , onBlur <| EditTask task.id False
      , on "input" (Json.map (UpdateTaskDescription task.id) targetValue)
    ]
    []
  else
    div [
      onClick (EditTask task.id True)
    ] [
      text task.description
    ]

taskIdCss : Int -> String
taskIdCss id = "task-" ++ toString id

onEnter : msg -> msg -> Attribute msg
onEnter fail success =
  let
    tagger code =
      if code == 13 then
        success
      else
        fail
  in
     on "keyup" <| Json.map tagger keyCode
