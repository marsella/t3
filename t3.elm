import Mouse
import Window

inputs = sampleOn Mouse.clicks (lift Input Mouse.position)

-- Model

data Input = Input (Int,Int)

defaultInput = Input (0,0)
input = lift Input Mouse.position

data Player = X | O
type Space  = Maybe Player
type Row    = { l:Space, m:Space, r:Space }
type Board  = { top:Row, mid:Row, bot:Row }
type Game   = { p:Player, board:Board }

initialBoard : Board
initialBoard = 
  let initRow = { l=Nothing, m=Nothing, r=Nothing }
  in {top=initRow, mid=initRow, bot=initRow}

initialGame : Game
initialGame = {p=X, board=initialBoard }

toList : Board -> [Space]
toList {top, mid, bot} = 
  -- row to list
  let rtl {l, m, r} = l :: m :: r :: []
  in rtl top ++ rtl mid ++ rtl bot

-- Drawing

drawSpace : (Int, Int) -> Space -> Element
drawSpace (w,h) space = 
  let bigLetter l = text <| Text.height 150 <| toText l
      dim x = round (toFloat x / 3)
      cont s = container (dim w) (dim h) middle (bigLetter s)
  in case space of 
    Nothing -> spacer (dim w) (dim h)
    Just X -> cont "x"
    Just O -> cont "o"

drawPieces : (Int,Int) -> Board -> Element
drawPieces dims {top, mid, bot} = 
  let ds = drawSpace dims
      drawRow {l, m, r} = flow right [ds l, ds m, ds r]
  in flow down [drawRow top, drawRow mid, drawRow bot]

drawBoard : (Int,Int) -> Game -> Element
drawBoard (w,h) {p, board} = 
  let style = { color=(hsva 18 1 2 1.0), 
                width=10, 
                cap=Round, 
                join=Smooth, 
                dashing=[], 
                dashOffset=0 }
  
      path p1 p2 = segment p1 p2 |> traced style
          
      -- center of the window is (0,0). I don't want to talk about it.
      vbars = [path (toFloat w / 6,      -1 * toFloat h) 
                   (toFloat w / 6,      toFloat h),
              path (-1 * toFloat w / 6, -1 * toFloat h) 
                   (-1 * toFloat w / 6, toFloat h)]
              
      hbars = [path (toFloat w,      toFloat h / 6) 
                   (-1 * toFloat w, toFloat h / 6), 
              path (toFloat w,      -1 * toFloat h / 6) 
                   (-1 * toFloat w, -1 * toFloat h / 6)]

  in layers [ collage w h <| vbars ++ hbars,
              drawPieces (w,h) board ]

-- Update

updateBoard : Board -> Input -> Player -> Board
updateBoard {top,mid,bot} input p = fullBoard

fullBoard = 
  let row1 = {l=Just X, m=Just O, r=Just X}
      row2 = {l=Just X, m=Just X, r=Just O}
      row3 = {l=Just O, m=Just X, r=Just O}
  in {top=row1, mid=row2, bot=row3}

stepGame : Input -> Game -> Game
stepGame input {p, board} =
  let newP = case p of X -> O
                       O -> X
      newBoard = updateBoard board input p
  in {p=newP, board=newBoard}

gameState = foldp stepGame initialGame inputs

main = lift2 drawBoard Window.dimensions gameState
