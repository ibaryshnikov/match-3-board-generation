import Html exposing (Html)
import Task
import Dict

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
init = (Model Dict.empty 10 10, send Roll)

view: Model -> Html Msg
view model = View.render model

update: Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  Roll -> (model, RandomBoard.generate model)
  NewBoard board -> ((Model board model.width model.height), Cmd.none)

subscriptions: Model -> Sub Msg
subscriptions model = Sub.none
