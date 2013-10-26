import Mouse
import Window

clickLocations = foldp (::) [] (sampleOn Mouse.clicks Mouse.position)

clicks num =  text <| Text.height 50 <| toText <| show num

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

display (w,h) locs =
  let tri (x,y) = 
          ngon 3 70 |> filled (hsva (toFloat x) 1 1 0.7)
                    |> move (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
                    |> rotate (toFloat x)
  in  layers [ collage w h (map tri locs),
               drawBoard (w,h),
               container w h middle <| clicks <| length locs ]


main = lift2 display Window.dimensions clickLocations
