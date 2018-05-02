import Html exposing (Html, text, div)
import Html.Attributes exposing (style)
import List

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

init: (Model, Cmd Msg)
init = (Model [], RandomBoard.generate (Model []))

view: Model -> Html Msg
view model = div [ style [
  "-moz-user-select" => "none",
  "user-select" => "none"
 ] ] (List.map View.line model.list)

none: Model -> (Model, Cmd Msg)
none model = (model, Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  Roll -> (model, RandomBoard.generate model)
  NewList list -> none (Model list)

subscriptions: Model -> Sub Msg
subscriptions model = Sub.none
