module HeavyFingers where
-- See this document for more information on making Pong:
-- http://elm-lang.org/blog/games-in-elm/part-0/Making-Pong.html

import Keyboard
import Window
import Color
import Touch
-- Inputs

type Input = { space:Bool, touches: [Touch.Touch], delta:Time }

delta = inSeconds <~ fps 35

input = sampleOn delta (Input <~ Keyboard.space
                               ~ Touch.touches
                               ~ delta)


-- Model

(gameWidth,gameHeight) = (1200,800)
(halfWidth,halfHeight) = (gameWidth/2,gameHeight/2)

data State = Play | Pause

type Coord = {x:Float, y:Float}
type Game = { state: State, objects:[Object], transitory:[Object] }
type Object = { loc:Coord, mass:Float, vel:Coord,
                name:String, colour:Color}

-- player : Float -> Player
-- player x = { x=x, y=0, vx=0, vy=0, score=0 }

defaultGame : Game
defaultGame =
  { state   = Pause,
    transitory = [],
    objects = [ { loc = {x = 0, y = 0},
                  mass = 1000,
                  vel = { x = 0, y = 0 },
                  name = "Sun",
                  colour = rgb 240 240 240 }] ++ map (\i ->
                                                     { loc = {x = 10*i, y = -i*10},
                                                       mass = 200,
                                                       vel = { x = (10-i)*10 - 50 , y = 50 - (i*10)},
                                                       name = "Earth",
                                                       colour = rgb 30 30 30 }) [1..10]
  }
-- Updates
--
--   |\
--   |y \ z            force_y**2 + force_x**2 = force_z**2
--   |____\            force_y = sqrt(force_z**2 - x_force
--      x
--
--  y_component = force_z * (y/z)
sqd : Coord -> Coord -> Float
sqd a b = (a.x - b.x)^2 + (a.y - b.y)^2

signum x = if x >= 0
           then 1
           else -1
gravconst = 100

computeGravity : Object -> [Object] -> (Float,Float)
computeGravity obj = foldl ( \ other (acc_x,acc_y) ->
                                    let
                                        squared_dist = sqd obj.loc other.loc
                                        attraction = (other.mass * gravconst) / (squared_dist)
                                        scaled = attraction / squared_dist
                                        xdiff = other.loc.x - obj.loc.x
                                        ydiff = other.loc.y - obj.loc.y
                                        xdist_squared = xdiff^2
                                        ydist_squared = ydiff^2
                                    in (acc_x+(scaled*xdist_squared) * signum xdiff,
                                        acc_y+(scaled*ydist_squared) * signum ydiff)) (0,0)


stepObj t objects obj = let (acc_x,acc_y) = computeGravity obj (filter (\x -> x.name /= obj.name) objects)
  in { obj | loc <- { x = obj.loc.x + obj.vel.x*t,
                      y = obj.loc.y + obj.vel.y*t },
             vel <- { x = obj.vel.x + acc_x, y = obj.vel.y + acc_y }
     }

near k c n = n >= k-c && n <= k+c
within ball paddle = (ball.x |> near paddle.x 8)
                  && (ball.y |> near paddle.y 20)

stepV v lowerCollision upperCollision =
  if | lowerCollision -> abs v
     | upperCollision -> 0 - abs v
     | otherwise      -> v

stepGame : Input -> Game -> Game
stepGame {space,touches,delta} ({state,objects} as game) =
  let ts = map mkTouch touches
      allobjs = ts ++ objects
  in
  {state = if | space            -> Play
              | otherwise        -> state,
   transitory = [],
   objects = map (stepObj delta allobjs) allobjs
  }

translate obj = { x=obj.x - halfWidth + 120,
                  y= (-1 * obj.y) + halfHeight - 100
                }

mkTouch : Touch.Touch -> Object
mkTouch touch = { loc = translate {x = toFloat touch.x, y= toFloat touch.y},
                  vel = {x=0,y=0},
                  name = "no",
                  colour = red,
                  mass = 2000.0 }

gameState = foldp stepGame defaultGame input


-- Display
red = rgb 255 0 0
pongGreen = rgb 60 100 60
textGreen = rgb 160 200 160
txt f = text . f . monospace . Text.color textGreen . toText
msg = "SPACE to start, WS and &uarr;&darr; to move"

make obj shape = shape |> filled (obj.colour)
                       |> move (obj.loc.x,obj.loc.y)

display : (Int,Int) -> Game -> Element
display (w,h) {state,objects,transitory} =
  container w h middle <| collage gameWidth gameHeight
    ([ rect gameWidth gameHeight |> filled pongGreen
--     , oval 15 15 |> make' (head objects)
     , toForm (if state == Play then spacer 1 1 else txt id msg)
       |> move (0, 40 - gameHeight/2)
     ] ++ map (\x -> (let r = rad x.mass in oval r r |> make x)) (objects ++ transitory))

-- this should be cube root of mass, but i am heeeeellll of lazy
rad mass = sqrt mass

main = lift2 display Window.dimensions gameState
