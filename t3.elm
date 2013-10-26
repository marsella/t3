import Mouse
import Window

clickLocations = foldp (::) [] (sampleOn Mouse.clicks Mouse.position)

-- Data

data Player = X | O
type Space  = Maybe Player
type Board = { tl:Space, tm:Space, tr:Space,
                l:Space,  m:Space,  r:Space,
               bl:Space, bm:Space, br:Space }

bigLetter l = text <| Text.height 150 <| toText l

drawSpace (w,h) space = 
  case space of 
    Nothing -> spacer (round (toFloat w / 3)) (round (toFloat h / 3))
    Just p -> 
      case p of
        X -> bigLetter "x"
        O -> bigLetter "o"

--  Board

clicks num = text <| Text.height 50 <| toText <| show num

drawBoard (w,h) = 
      --style : LineStyle
  let style = { color=(hsva 23 1 2 1.0), width=10, cap=Round, join=Smooth, dashing=[], dashOffset=0 }
  
      path p1 p2 = 
          segment p1 p2
          |> traced style
      vbars = path (toFloat w / 6,      -1 * toFloat h) 
                   (toFloat w / 6,      toFloat h) :: 
              path (-1 * toFloat w / 6, -1 * toFloat h) 
                   (-1 * toFloat w / 6, toFloat h) :: 
              []
      hbars = path (toFloat w,      toFloat h / 6) 
                   (-1 * toFloat w, toFloat h / 6) :: 
              path (toFloat w,      -1 * toFloat h / 6) 
                   (-1 * toFloat w, -1 * toFloat h / 6) :: 
              []

  in collage w h <| vbars ++ hbars

-- 
display (w,h) locs =
  let tri (x,y) = 
          ngon 3 70 |> filled (hsva (toFloat x) 1 1 0.7)
                    |> move (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
                    |> rotate (toFloat x)
  in  layers [ collage w h (map tri locs),
               drawBoard (w,h),
               container w h middle <| clicks <| length locs ]


main = lift2 display Window.dimensions clickLocations
