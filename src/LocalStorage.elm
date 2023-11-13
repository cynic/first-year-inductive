port module LocalStorage exposing (save, loadFromUser, loadResult)
import DataTypes

-- A port module for accessing the get/set functions of the localStorage API.
-- See https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage

port saveToStorage : String -> Cmd msg
port loadFromUser : (String -> msg) -> Sub msg
port loadResult : String -> Cmd msg

save : DataTypes.Model -> Cmd msg
save model =
  saveToStorage (DataTypes.encodeModelAsString model)