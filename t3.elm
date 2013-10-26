import Mouse
import Window

inputs = sampleOn Mouse.clicks (lift Input Mouse.position)

-- Model

data Input = Input (Int,Int)

defaultInput = Input (0,0)
input = lift Input Mouse.position

data Player = X | O
type Space  = Maybe Player
type Row    = {l:Space, m:Space, r:Space}
type Board  = {top:Row, mid:Row, bot:Row}

initialBoard : Board
initialBoard = 
  let initRow = { l=Nothing, m=Nothing, r=Nothing }
  in {top=initRow, mid=initRow, bot=initRow}
  
toList : Board -> [Space]
toList {top, mid, bot} = 
  -- row to list
  let rtl {l, m, r} = l :: m :: r :: []
  in rtl top ++ rtl mid ++ rtl bot

-- Drawing

bigLetter : String -> Element
bigLetter l = text <| Text.height 150 <| toText l

drawSpace : (Int, Int) -> Space -> Element
drawSpace (w,h) space = 
  case space of 
    Nothing -> spacer (round (toFloat w / 3)) (round (toFloat h / 3))
    Just p -> 
      case p of
        X -> bigLetter "x"
        O -> bigLetter "o"

drawBoard : (Int,Int) -> Board -> Element
drawBoard (w,h) board = 
  let style = { color=(hsva 23 1 2 1.0), width=10, cap=Round, join=Smooth, dashing=[], dashOffset=0 }
  
      path p1 p2 = 
          segment p1 p2
          |> traced style
      -- center of the window is (0,0). I don't want to talk about it.
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

-- Update

stepGame : Input -> Board -> Board
stepGame input board = board

gameState = foldp stepGame initialBoard inputs

display : (Int, Int) -> Board -> Element
display (w,h) board = drawBoard (w,h) board


main = lift2 display Window.dimensions gameState
