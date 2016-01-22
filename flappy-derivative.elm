import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window

type alias Model = { a : Float, b : Float, c : Float }

type alias Keys = { x:Int, y:Int }

s0 : Model
s0 = { a = 0, b = 0, c = 0 }

update : (Float, Keys) -> Model -> Model
update (dt, keys) s = s |> handle keys |> evolve dt
    
handle : Keys -> Model -> Model
handle keys s =
  case keys.x of
    0    -> { a = s.a, b = s.b, c = s.c + toFloat keys.y / 500 }
    1    -> { a = s.a, b = s.b, c = 0 }
    _    -> s0

evolve : Float -> Model -> Model
evolve dt {a,b,c} =
  let a' = a + dt*b
      b' = b + dt*c
      c' = c
  in  { a = a', b = b', c = c' }

view : (Int, Int) -> Model -> Element
view (w',h') s =
  let
    (w,h)      = (toFloat w', toFloat h')
    src        = "http://elm-lang.org/imgs/mario/jump/right.gif"
    marioImage = image 35 35 src
  in
    collage w' h'
      [ rect w h
          |> filled (rgb 174 238 238)
      , rect w 50
          |> filled (rgb 74 167 43)
          |> move (0, 0)
      , rect 10 50
          |> filled (rgb 174 167 43)
          |> move (s.b * 50, 0)
      , rect 10 50
          |> filled (rgb 214 0 43)
          |> move (s.c * 300, 0)
      , marioImage
          |> toForm
          |> move (0, s.a)
      ]

main : Signal Element
main = Signal.map2 view Window.dimensions (Signal.foldp update s0 input)

input : Signal (Float, Keys)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 30)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)
