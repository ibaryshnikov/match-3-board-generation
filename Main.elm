import Html exposing (Html, text, div)
import Html.Attributes exposing (style)
import List
import Task

import Helpers exposing ((=>))
import RandomBoard
import Shared exposing (Model, Msg(..))
import View

main: Program Never Model Msg
main = Html.program {
    init = init,
    view = view,
    update = update,
    subscriptions = subscriptions
 }

send : msg -> Cmd msg
send msg =
  Task.succeed msg
  |> Task.perform identity

init: (Model, Cmd Msg)
init = (Model [] 10 10, send Roll)

view: Model -> Html Msg
view model = div [ style [
  "-moz-user-select" => "none",
  "user-select" => "none"
 ] ] (List.map View.line model.board)

none: Model -> (Model, Cmd Msg)
none model = (model, Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  Roll -> (model, RandomBoard.generate model)
  NewBoard board -> ((Model board model.width model.height), Cmd.none)

subscriptions: Model -> Sub Msg
subscriptions model = Sub.none
