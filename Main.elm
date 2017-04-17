module Ftack exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Task exposing (perform)
import Array exposing (Array, fromList, toList, length, filter)
import Window exposing (Size, resizes, size)
import Maybe exposing (withDefault)
import Random exposing (..)
import Ports exposing (..)
import Json.Encode as EJson
import ParseInt exposing (toHex, parseIntHex)
import Debug exposing (log)
import Time exposing (..)


main : Program Never Model Msg
main =
  Html.program
    { init = init
     , view = view
     , update = update
     , subscriptions = subs--\_ -> Sub.none
    }


-- MODEL


type alias CN =
  { r : Float
  , i : Float
  }


type alias C =
  { r : Float
  , g : Float
  , b : Float
  }


type alias Cband =
  { c : C
  , v : Float
  }


type alias Model =
    { cdepth : Int
    , res : Int
    , p : CN
    , u : Float
    , v : Float
    , cosampl : Int
    , grad : Array Cband
    , comp : List (List Int)
    , gamma : { act : Bool, g : Float}
    , renderPar : {comp : Int, paint: Int, proc: Int}
    }


init : (Model, Cmd Msg)
init  =
  Model 50 10 (CN -2.5 2) 4.0 -4.0 1 (Array.fromList [(Cband (C 1 1 1) 0), (Cband (C 0 0 1) 0.25), (Cband (C 1 0 0) 0.5), (Cband (C 1 1 0) 0.75), (Cband (C 0 0 0) 1)]) [[0]] { act = True, g = 2.2 } { comp = -1, paint = -1, proc = -1 }
    ! [ Task.perform Res Window.size ]


-- UPDATE


type Msg
        = Compute
        | Paint
        | Res Window.Size
        | Rect (Int, Int, Int)
        | ChangeDepth String
        | ChangeOsampl String
        | ChangeGrad String Int String
        | SwitchGamma Bool
        | ChangeGamma String
        | Render Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
      case msg of
        Compute ->
          compute model
            ! [ Task.perform identity (Task.succeed Paint) ]

        Paint ->
          model
            ! [ paint model ]

        Render _ ->
          let
            rp = model.renderPar
          in
          if rp.comp >= 0 || rp.paint >= 0 then
            if rp.comp >= 3 then
              { model | renderPar = { comp = -1, paint = -1, proc = 1 } }
                ! [ Task.perform identity (Task.succeed Compute) ]
            else if rp.paint >= 3 then
              { model | renderPar = { rp | paint = -1 } }
                ! [ Task.perform identity (Task.succeed Paint) ]
            else
              { model | renderPar = { comp =  if rp.comp >= 0 then rp.comp + 1 else rp.comp, paint =  if rp.paint >= 0 then rp.paint + 1 else rp.paint, proc = -1 } }
                ! []
          else
              { model | renderPar = { rp | proc = -1 } }
                ! []

        Res s ->
            { model | res = s.height }
              ! [ Task.perform identity (Task.succeed Compute) ]

        Rect (ax,ay, bx) ->
          let
            ores = 1.0 / toFloat model.res
            p_ar = ores * toFloat ax
            p_ai = ores * toFloat ay
            uv_a = ores *  (toFloat bx)
            np = CN (model.p.r + p_ar*model.u) (model.p.i + p_ai*model.v)
            nu = uv_a * model.u
            nv = uv_a * model.v
          in
          { model | p = np, u = nu, v = nv }
            ! [ Task.perform identity (Task.succeed Compute) ]

        ChangeDepth d ->
          let
            nd = Result.withDefault 50 (String.toInt d)
            rp = model.renderPar
          in
          { model | cdepth = nd, renderPar = { rp | comp = 0} }
            ! []

        ChangeOsampl o ->
          let
            no = Result.withDefault 1 (String.toInt o)
            rp = model.renderPar
          in
          { model | cosampl = if no > 0 then no else 1, renderPar = { rp | comp = 0} }
            ! []

        ChangeGrad t i val ->
           case Array.get i model.grad of
              Nothing ->
                model ! []
              Just v ->
                case t of
                  "c" ->
                    { model | grad = Array.set i { v | c  = hexToCol val } model.grad }
                      ! [ Task.perform identity (Task.succeed Paint) ]
                  "v" ->
                    case String.toFloat val of
                      Err _ ->
                        model ! []
                      Ok a ->
                        let
                          g = Array.set i { v | v  = a } model.grad
                          rp = model.renderPar
                        in
                        { model | grad = g, renderPar = { rp | paint = 0 } }
                          ! []
                  "d" ->
                    let
                      rp = model.renderPar
                      a = Array.slice 0 i model.grad
                      b = Array.slice (i + 1) (Array.length model.grad) model.grad
                      c = Array.append a b
                    in
                    { model | grad = c, renderPar = { rp | paint = 0 } }
                      ! []
                  "a" ->
                    let
                      rp = model.renderPar
                      c = Array.push (Cband (C 0 0 0) 1) model.grad
                    in
                    { model | grad = c, renderPar = { rp | paint = 0 } }
                      ! []
                  _ ->
                    model ! []

        SwitchGamma g ->
          let
            og =  model.gamma
            rp = model.renderPar
          in
          { model | gamma = { og | act = g }, renderPar = { rp | paint = 0 } }
            ! []

        ChangeGamma g ->
          let
            no = Result.withDefault 2.2 (String.toFloat g)
            og =  model.gamma
            rp = model.renderPar
          in
          { model | gamma = { og | g = no }, renderPar = { rp | paint = 0} }
            ! []


-- SUBSCRIPTIONS


subs : Model -> Sub Msg
subs model =
  Sub.batch [ Window.resizes Res, getRect Rect, every (250*millisecond) Render ]


-- VIEW


view : Model -> Html Msg
view model =
  section []
    [ p []
      [ label [ for "depth" ] [text "maximum number of iterations"]
      , input
          [ id "depth"
          , type_ "number"
          , property "min" (EJson.int 1)
          , property "step" (EJson.int 10)
          , placeholder <| "default: " ++ toString model.cdepth
          , onInput ChangeDepth
          ] []
      , label [ for "osampl" ] [text "antialiasing"]
      , input
          [ id "osampl"
          , type_ "number"
          , property "min" (EJson.int 1)
          , property "step" (EJson.int 1)
          , placeholder <| "default: " ++ toString model.cosampl
          , onInput ChangeOsampl
          ] []
      ]
    , grad model
    , p []
      [ input
          [ type_ "checkbox"
          , title "turn gamma on/off"
          , checked (if model.gamma.act then True else False)
          , Html.Events.onCheck SwitchGamma
          ] []
      , label [ for "gamma" ] [text "gamma correction"]
      , input
          [ id "gamma"
          , type_ "number"
          , property "min" (EJson.float 0)
          , property "step" (EJson.float 0.1)
          , placeholder <| "default: " ++ toString model.gamma.g
          , onInput ChangeGamma
          ] []
      ]
    ]


grad : Model -> Html Msg
grad model =
  p []
      [ label [] [ text "gradient" ]
      , section []
        (toList <| Array.indexedMap (\i gr ->
          section []
            [ input
              [ type_ "number"
              , property "min" (EJson.float 0)
              , property "max" (EJson.float 1)
              , property "step" (EJson.float 0.01)
              , value <| toString gr.v
              , onInput (ChangeGrad "v" i)
              ] []
            , input
              [ type_ "color"
              , value <| colToHex gr.c
              , onInput (ChangeGrad "c" i)
              ] []
            , button [ title "remove color", Html.Events.onClick (ChangeGrad "d" i "") ] [ text "x"]
            ]
          ) model.grad)
      , button [ title "add color", Html.Events.onClick (ChangeGrad "a" 0 "") ] [ text "+"]
      ]


--


mult : CN -> CN -> CN
mult a b =
  CN (a.r*b.r - a.i*b.i) (a.r*b.i + a.i*b.r)


add : CN -> CN -> CN
add a b =
  CN (a.r  + b.r) (a.i + b.i)


eval : CN ->  CN -> Int -> Int -> (CN, Int)
eval z c d i =
    if z.r < -2.0 || z.r > 2.0 || z.i < -2.0 || z.i > 2.0 then
      (z, i)
    else if i >= d then
      (z, i)
    else
      eval (add (mult z z) c) c d (i + 1)


compute : Model -> Model
compute m =
  let
    t = 1.0 / toFloat m.res
    cc = List.concat <| List.indexedMap (\y a ->
      List.indexedMap (\x b ->
        List.map (\rnd ->
          let
            (ru, r0) = Random.step (Random.float 0 1) (Random.initialSeed (x*y*rnd))
            (rv, _) = Random.step (Random.float 0 1) r0
            du = t * m.u * ru
            dv = t * m.v * rv
            p = CN (m.p.r + t * (toFloat x) * m.u + du) (m.p.i + t * (toFloat y) * m.v + dv)
            (_, i) = eval (CN 0.0 0.0) p m.cdepth 0
          in
          i
        ) b
      ) a
    ) (List.repeat m.res (List.repeat m.res (List.range 1 m.cosampl)))
  in
  { m | comp = cc }


gamma : Model -> Float -> Int
gamma m c =
  let
    nc = if m.gamma.act then
      c ^ (1.0/m.gamma.g)
    else
      c
  in
  clamp 0 255 <| round (255 * nc )


paint : Model -> Cmd Msg
paint m =
  let
    grad = Array.fromList <| List.sortBy .v (Array.toList m.grad)
    data =
      List.map (\b ->
        let
          c = List.foldr (\i sp ->
            let
              sf = 1.0 / toFloat m.cosampl
              col =
                if i >= m.cdepth then
                  (Maybe.withDefault (Cband (C 0 0 0) 0) (Array.get ((Array.length grad)-1) grad)).c
                else
                  let
                    t = logBase 2 ( (toFloat i) / (toFloat m.cdepth) +1)
                    a = Array.filter (\g -> g.v <= t) grad
                    b = Array.filter (\g -> g.v >= t) grad
                    va = Maybe.withDefault (Cband (C 0 0 0) 0) (Array.get ((Array.length a)-1) a)
                    vb = Maybe.withDefault (Cband (C 0 0 0) 0) (Array.get 0  b)
                    ut = (t - va.v) / (vb.v - va.v)
                    u = if isNaN ut then 1 else ut
                    ur = (1 - u) * va.c.r + vb.c.r * u
                    ug = (1 - u) * va.c.g + vb.c.g * u
                    ub = (1 - u) * va.c.b + vb.c.b * u
                  in
                  C  ur ug ub
            in
            C (sp.r + sf * col.r) (sp.g + sf * col.g) (sp.b + sf * col.b)
          ) (C 0 0 0) b
        in
        C c.r c.g c.b
      ) m.comp
    x = List.concat <| List.map (\c ->
      [ gamma m c.r, gamma m c.g, gamma m c.b, 255]
    ) data
  in
  Ports.sendData (m.res, m.res, x)


hexToCol : String -> C
hexToCol s =
  let
    rh = Result.withDefault 0 (parseIntHex <| String.slice 1 3 s)
    gh = Result.withDefault 0 (parseIntHex <| String.slice 3 5 s)
    bh = Result.withDefault 0 (parseIntHex <| String.slice 5 7 s)
  in
  C ((toFloat rh)/255.0) ((toFloat gh)/255.0) ((toFloat bh)/255.0)


colToHex : C -> String
colToHex c =
  "#" ++ (padToHex (round (255*c.r))) ++ (padToHex (round (255*c.g))) ++ (padToHex (round (255*c.b)))


padToHex : Int -> String
padToHex i =
  let
    h = toHex i
  in
  if String.length h > 1 then h else "0" ++ h
