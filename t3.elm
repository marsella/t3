-- tic tac toe

import Mouse
import Window

clickLocations = foldp (::) [] (sampleOn Mouse.clicks Mouse.position)

clicks num =  text <| Text.height 50 <| toText <| show num

drawBoard (w,h) = 
  let drawVBar = rect 10 (toFloat h) |> filled (hsva 23.0 1 1 1.0)
      path = segment (toFloat w / 6, -1 * toFloat h) (toFloat w / 6, toFloat h)
          |> traced (solid (hsva 55 1 1 1.0))

  in collage w h [path]

display (w,h) locs =
  let tri (x,y) = 
          ngon 3 70 |> filled (hsva (toFloat x) 1 1 0.7)
                    |> move (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
                    |> rotate (toFloat x)
  in  layers [ collage w h (map tri locs),
               drawBoard (w,h),
               container w h middle <| clicks <| length locs ]


main = lift2 display Window.dimensions clickLocations