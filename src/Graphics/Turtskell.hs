module Graphics.Turtskell
    ( runCommand,
      runCommands,
      forward,
      backward,
      turn,
      setX,
      getX,
      setY,
      getY,
      setAngle,
      getAngle,
      save,
      restore
    ) where

import Graphics.Gloss 
import Control.Monad.State 

type Angle = Float 
type Coordinate = Float 
type Length = Float 
type Position = (Coordinate,Coordinate) 
type TurtleState = (Angle,Position,Brush) -- Angle,(X,Y),Bursh
type Brush = (Bool,Color,Int) --Brush down, color, thinkness
type TurtleCommand a = State([Picture],Turtle) a

runCommand :: TurtleCommand a -> IO ()
runCommand c = runCommands [c]

runCommands :: [TurtleCommand a] -> IO ()
runCommands tc = display window background (evaluate tc)
    where window = InWindow "Dank Turtle" (600,600) (0,0)
          background = white

evaluate :: [TurtleCommand a] -> Picture
evaluate tc     = Pictures p
    where p = fst $ snd $ runState (chainCommands tc) initState

--Turn all the commands into a single command
chainCommands :: [TurtleCommand a] -> TurtleCommand ()
chainCommands cmds = foldr (>>) (return ()) cmds

data Turtle = Turtle {
              angle :: Angle
             ,pos   :: Position
             ,brush :: Brush
             ,stack :: [TurtleState]
              } deriving (Eq,Show)


initTurtle = Turtle 0 (0,0) (True,black,1) [] 
initState = ([],initTurtle)

pop :: [a] -> (a,[a])
pop (x:xs) = (x,xs)
pop _      = error "pop empty stack"

push :: a -> [a] -> [a]
push a xs = (a:xs)


restore :: TurtleCommand ()
restore =  state $ \(p,t) -> let ((newAngle,newPos,newBrush),newStack) = pop $ stack t
                                 newT = Turtle newAngle newPos newBrush newStack 
            in  ((),(p,newT))

save :: TurtleCommand ()
save = state $ \(p,t) -> let currentState = (angle t, pos t, brush t) --current Turtle state (position)
                             newT = Turtle (angle t) (pos t) (brush t) 
                                     (push currentState (stack t)) --Push current state onto stack
        in ((),(p,newT))


turn :: Angle -> TurtleCommand ()
turn alpha = state $ \(p,t) -> let newAngle = angle t + alpha
                                   newT = Turtle newAngle (pos t) (brush t) (stack t)
             in ((),(p,newT))


forward :: Length -> TurtleCommand ()
forward l = state $ \(p,t) -> let path   = computePath (pos t) l (angle t)
                                  newP   = (line path) : p
                                  newPos = path !! 1 --new turtle is at the end of the line
                                  newT   = Turtle (angle t) newPos (brush t) (stack t)
             in ((),(newP,newT))

backward :: Length -> TurtleCommand ()
backward l = turn 180 >> forward l

setX :: Coordinate -> TurtleCommand () 
setX x = state $ \(p,t) -> let (_,y ) = pos t
                               newPos = (x,y)
                               newT   = Turtle (angle t) newPos (brush t) (stack t)
          in ((),(p,newT))

setY :: Coordinate -> TurtleCommand ()
setY y = state $ \(p,t) -> let (x,_ ) = pos t
                               newPos = (x,y)
                               newT   = Turtle (angle t) newPos (brush t) (stack t)
          in ((),(p,newT))

setAngle :: Angle -> TurtleCommand ()
setAngle alpha = state $ \(p,t) -> let newT   = Turtle alpha (pos t) (brush t) (stack t)
          in ((),(p,newT))

getX :: TurtleCommand Coordinate
getX = state $ \(p,t) -> let (x,_) = pos t in (x,(p,t))

getY :: TurtleCommand Coordinate
getY = state $ \(p,t) -> let (_,y) = pos t in (y,(p,t))

getAngle :: TurtleCommand Angle
getAngle = state $ \(p,t) -> let alpha = angle t
            in (alpha,(p,t))

computePath :: Position -> Length -> Angle -> [Position]
computePath (x,y) length angle = [(x,y),(x+adjacent,y+opposite)]
    where adjacent = length * cos(radians)
          opposite = length * sin(radians)
          radians  = angleToRadian angle
          angleToRadian = (* (pi/180))
