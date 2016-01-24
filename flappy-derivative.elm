import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import List exposing (..)
import Random exposing (..)

type alias Model = { x : Float, a : Float, b : Float, c : Float, barriers : List Barrier, seed : Seed }

type alias Keys = { x : Int, y : Int }

type alias Barrier = { x : Float, y : Float, dir : Direction }
type Direction = Up | Down

s0 : Model
s0 = { x = 0, a = 0, b = 0, c = 0, barriers = [], seed = initialSeed 12345 }

vx : Float
vx = 1

update : ((Float, Float), (Float, Keys)) -> Model -> Model
update ((w,h), (dt, keys)) s = s |> handle keys |> removeBarriers (w,h) |> addBarriers (w,h) |> evolve dt
    
handle : Keys -> Model -> Model
handle keys s =
  case keys.x of
    0    -> { s | b = s.b + toFloat keys.y / 5 }
    1    -> { s | b = 0 }
    _    -> s0

removeBarriers : (Float,Float) -> Model -> Model
removeBarriers (w,h) s = { s | barriers = filter (\b -> b.x >= s.x - w) s.barriers }

addBarriers : (Float,Float) -> Model -> Model
addBarriers (w,h) s =
  let lastX = foldl max s.x <| List.map (.x) s.barriers in
  if lastX > s.x + w then s else
  let (newY, seed')    = generate (float (-200) 200) s.seed
      (newDir, seed'') = generate (int 0 1) seed'
      (newX, seed''')  = generate (float 0 300) seed''
      newBarrier = { x = lastX + 100 + newX, y = newY, dir = if newDir == 0 then Up else Down }
  in  addBarriers (w,h) { s | barriers = newBarrier :: s.barriers, seed = seed''' }

evolve : Float -> Model -> Model
evolve dt s =
  let a' = s.a + dt*s.b
      b' = s.b + dt*s.c
      c' = s.c
      x' = s.x + dt*vx
  in  { s | x = x', a = a', b = b', c = c' }

view : (Float, Float) -> Model -> Element
view (w,h) s =
  let
    src        = "http://elm-lang.org/imgs/mario/jump/right.gif"
    marioImage = image 35 35 src
  in
    collage (round w) (round h) <|
      [ rect w h
          |> filled (rgb 174 238 238)
      , marioImage
          |> toForm
          |> move (0, s.a)
      ] ++ List.map (move (-s.x, 0) << viewBarrier h) s.barriers

viewBarrier h {x,y,dir} = case dir of
    Up   -> rect 20 h |> filled (rgb 0 0 0) |> move (x,  h/2 + y)
    Down -> rect 20 h |> filled (rgb 0 0 0) |> move (x, -h/2 + y)

main : Signal Element
main =
    let dim = Signal.map (\(w,h) -> (toFloat w, toFloat h)) Window.dimensions
    in  Signal.map2 view dim (Signal.foldp update s0 <| Signal.map2 (\a b -> (a,b)) dim input)

input : Signal (Float, Keys)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 30)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)
