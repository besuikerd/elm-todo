module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App
import Html.Events exposing (..)
import Components.Todo as Todo

main : Program Never
main = Html.App.program {
      view = view
    , update = update
    , init = init ! []
    , subscriptions = \_ -> Sub.none
  }

type alias Id = Int

type alias Model = {
      todos: List (Id, Todo.Model)
    , uid: Int
  }

init : Model
init = {
      todos = []
    , uid = 0
  }

type Msg =
    AddTodo
  | RemoveTodo Id
  | TodoMsg Id Todo.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  AddTodo -> {
      model | todos = model.todos ++ [(model.uid, Todo.init)]
    , uid = model.uid + 1
  } ! []
  RemoveTodo id -> {model | todos = List.filter (\(id', _) -> id /= id') model.todos} ! []
  TodoMsg id msg ->
    let
      updateTodo (id', model) =
        if id == id' then
          let
            (model', cmds) = Todo.update msg model
          in
            ((id', model'), Cmd.map (TodoMsg id) cmds)
        else
          ((id', model), Cmd.none)
      (todos', cmds) = List.unzip (List.map updateTodo model.todos)
    in
      ({ model | todos = todos' }, Cmd.batch(cmds))

view: Model -> Html Msg
view model = div []
  (header :: List.map (uncurry todo) model.todos)


header : Html Msg
header = div [] [
    button [
        class "btn btn-primary"
      , onClick AddTodo
    ] [ text "Add Todo"]
  ]

todo : Id -> Todo.Model -> Html Msg
todo id model = div [] [
      button [
          class "btn"
        , onClick <| RemoveTodo id
      ] [ span [class "glyphicon glyphicon-remove"] [text <| toString id]]
    , Html.App.map (TodoMsg id) (Todo.view model)
  ]
