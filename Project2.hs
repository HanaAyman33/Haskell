type Position = (Int,Int)
data Color = W | B deriving (Eq, Show)
data Peg = Peg Position Color deriving (Eq, Show)
data Move = M Position deriving (Eq, Show)
type Board = [Peg]
data State = S Move Board deriving (Eq, Show)


createBoard:: Position -> Board
createBoard (x,y) = if(elem (x,y) [(-3,-1),(-3,0),(-3,1),(-2,-1) , (-2,0) , (-2,1) , (-1,-3) , (-1,-2) , (-1,-1) , (-1,0) , (-1,1) , (-1,2) , (-1,3) , (0,-3) , (0,-2) , (0,-1) , (0,0) , (0,1) , (0,2) , (0,3) , (1,-3) , (1,-2) , (1,-1) , (1,0) , (1,1) , (1,2) , (1,3) , (2,-1) , (2,0) , (2,1) , (3,-1) , (3,0) , (3,1) ]) 
        then createBoardHelper (x,y) [(-3,-1),(-3,0),(-3,1),(-2,-1) , (-2,0) , (-2,1) , (-1,-3) , (-1,-2) , (-1,-1) , (-1,0) , (-1,1) , (-1,2) , (-1,3) , (0,-3) , (0,-2) , (0,-1) , (0,0) , (0,1) , (0,2) , (0,3) , (1,-3) , (1,-2) , (1,-1) , (1,0) , (1,1) , (1,2) , (1,3) , (2,-1) , (2,0) , (2,1) , (3,-1) , (3,0) , (3,1) ]
        else error "The position is not valid."
createBoardHelper:: Position ->[Position] -> Board
createBoardHelper _ [] = []
createBoardHelper (x,y)  ((x1,y1):t) = if(x==x1 && y==y1) 
            then (Peg (x1,y1) W : createBoardHelper (x,y) t)
            else (Peg (x1,y1) B : createBoardHelper (x,y) t)

isValidMove:: Move -> Board -> Bool
isValidMove (M (x,y)) board = if(not(elem (Peg (x,y) B) board)) 
            then False
            else if(elem (Peg (x+1,y) W) board || elem (Peg (x-1,y) W) board || elem (Peg (x,y+1) W) board || elem (Peg (x,y-1) W) board)
                 then True  
                 else False

isGoal:: Board -> Bool
isGoal [] = True
isGoal ((Peg _ c):hs) = if(c==W) then isGoal hs
                        else False

showPossibleNextStates:: Board -> [State]
showPossibleNextStates board = if(isGoal board)
                            then error "No Possible States Exist."
                            else showPossibleNextStatesHelper board board

showPossibleNextStatesHelper:: Board -> Board -> [State]
showPossibleNextStatesHelper [] _ = []
showPossibleNextStatesHelper (Peg (x,y) c:t) board = if(isValidMove (M (x,y)) board) 
                                    then (S (M (x,y)) (possibleBoard (x,y) board):showPossibleNextStatesHelper t board)
                                    else showPossibleNextStatesHelper t board

possibleBoard:: Position -> Board -> Board
possibleBoard _ [] = []
possibleBoard (x,y)  (Peg (x1,y1) c:t) = if(x==x1 && y==y1) 
            then (Peg (x1,y1) W : possibleBoard (x,y) t)
            else (Peg (x1,y1) c : possibleBoard (x,y) t)
