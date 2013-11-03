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
type Game   = { p:Player, board:Board}

dims : Float
dims = 620

initialBoard : Board
initialBoard = 
  let initRow = { l=Nothing, m=Nothing, r=Nothing }
  in {top=initRow, mid=initRow, bot=initRow}

initialGame : Game
initialGame = {p=X, board=initialBoard}

toList : Board -> [Space]
toList {top, mid, bot} = 
  -- row to list
  let rtl {l, m, r} = l :: m :: r :: []
  in rtl top ++ rtl mid ++ rtl bot
  
fromList : [Space] -> Board
fromList spaces =
  case spaces of
    a :: b :: c :: d :: e :: f :: g :: h :: i :: [] -> 
         {top={l=a,m=b,r=c},mid={l=d,m=e,r=f},bot={l=g,m=h,r=i}}
    _ -> initialBoard

-- Display

drawSpace : Space -> Element
drawSpace space = 
  let bigLetter l = text <| Text.height 150 <| toText l
      dim x = round (x / 3)
      cont s = container (dim dims) (dim dims) middle (bigLetter s)
  in case space of 
    Nothing -> spacer (dim dims) (dim dims)
    Just X -> cont "x"
    Just O -> cont "o"

drawPieces : Board -> Element
drawPieces {top, mid, bot} = 
  let ds = drawSpace
      drawRow {l, m, r} = flow right [ds l, ds m, ds r]
  in flow down [drawRow top, drawRow mid, drawRow bot]

drawBoard : Game -> Element
drawBoard {p, board} = 
  let style = { color=(hsva 18 3 1 1.0), 
                width=10, 
                cap=Round, 
                join=Smooth, 
                dashing=[], 
                dashOffset=0 }
  
      path p1 p2 = segment p1 p2 |> traced style
      w = dims
      h = dims
      -- This is heinous because paths work relative to the previous bit.
      -- Will improve soon.
      vbars = [path (w / 6, -1 * h)      (w / 6, h),
               path (-1 * w / 6, -1 * h) (-1 * w / 6, h)]
              
      hbars = [path (w, h / 6)      (-1 * w, h / 6), 
               path (w, -1 * h / 6) (-1 * w, -1 * h / 6)]

  in layers [ collage (round w) (round h) <| vbars ++ hbars,
              drawPieces board ]

-- Update
detectBox : Input -> (Int,Int)
detectBox (Input (x,y)) = 
  let cond l g = 
        if | (toFloat l) < (g / 3) -> 1
           | (toFloat l) < (2 * g / 3) -> 2
           | (toFloat l) < g -> 3
           | otherwise -> 4 --break stuff
      row = cond y dims
      col = cond x dims
  in (col, row)


updateBoard : Game -> Input -> Board
updateBoard {p, board} input = 
  let 
    {top, mid, bot} = board
    placeRow : Int -> Row -> Row
    placeRow c {l, m, r} = 
      if | c == 1 -> {l=Just p, m=m, r=r}
         | c == 2 -> {l=l, m=Just p, r=r}
         | c == 3 -> {l=l, m=m, r=Just p}
  in case (detectBox input) of 
    (c, 1) -> {top=(placeRow c top), mid=mid,            bot=bot}
    (c, 2) -> {top=top,              mid=placeRow c mid, bot=bot}
    (c, 3) -> {top=top,              mid=mid,            bot=placeRow c bot}
    _  -> initialBoard

fullBoard = 
  let row1 = {l=Just X, m=Just X, r=Just O}
      row2 = {l=Just X, m=Just O, r=Just X}
      row3 = {l=Just O, m=Just X, r=Just X}
  in {top=row1, mid=row2, bot=row3}
  
third = 
  let row1 = {l=Just X, m=Just X, r=Just O}
      erow = {l=Nothing, m=Nothing, r=Nothing}
  in {top=row1, mid=erow, bot=erow}

twothird = 
  let row1 = {l=Just X, m=Just X, r=Just O}
      erow = {l=Nothing, m=Nothing, r=Nothing}
  in {top=row1, mid=row1, bot=erow}

stepGame : Input -> Game -> Game
stepGame input {p, board} =
  let newP = case p of X -> O
                       O -> X
      newBoard = updateBoard {p=p, board=board} input
  in {p=newP, board=newBoard}

gameState = foldp stepGame initialGame inputs

main = lift drawBoard gameState
