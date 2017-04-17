port module Ports exposing (..)

port sendData : (Int, Int, List Int) -> Cmd msg
port getRect : ((Int, Int, Int) -> msg) -> Sub msg
