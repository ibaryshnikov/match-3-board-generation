import Html exposing (Html)
import Task
import Dict

import RandomBoard
import Shared exposing (Gems, Board, Model, Msg(..))
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

gems: Gems
gems = ["a", "b", "c", "d", "e"]

init: (Model, Cmd Msg)
init = (Model 10 10 Dict.empty gems, send Roll)

view: Model -> Html Msg
view model = View.render model

replaceBoard: Board -> Model -> Model
replaceBoard board { width, height, gems } =
 Model width height board gems

update: Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  Roll -> (model, RandomBoard.generate model)
  NewBoard board -> ((replaceBoard board model), Cmd.none)

subscriptions: Model -> Sub Msg
subscriptions model = Sub.none
