module Turtskell(runCommands,
              TurtleCommand(Forward,
                            Backward,
                            Turn,
                            SetX,
                            SetY,
                            Save,
                            Restore)) where

import Graphics.Gloss 
import Control.Monad.State 

type Angle = Float 
type Coordinate = Float 
type Length = Float 
type Position = (Coordinate,Coordinate) 
type TurtleState = (Angle,Position) -- Angle,(X,Y)

data TurtleCommand = Forward  Length     |
                     Backward Length     |
                     Turn     Angle      |
                     SetX     Coordinate |
                     SetY     Coordinate |
                     Save                |
                     Restore
                     deriving(Read,Eq,Show)
                    
runCommands :: [TurtleCommand] -> IO ()
runCommands tc = display window background (evaluate tc)
    where window = InWindow "Dank Turtle" (600,600) (0,0)
          background = white


evaluate :: [TurtleCommand] -> Picture
evaluate tc     = Pictures p
    where p = fst $ snd $ runState (chainCommands tc) ([],initTurtle)


commandToAction :: TurtleCommand -> State ([Picture],Turtle) ()
commandToAction (Forward  l) = forward l
commandToAction (Backward l) = backward l
commandToAction (Turn     a) = turn a
commandToAction (SetX     x) = setx x
commandToAction (SetY     y) = sety y
commandToAction (Save      ) = save
commandToAction (Restore   ) = restore


--Turn all the commands into a single statefull action
chainCommands :: [TurtleCommand] -> State ([Picture],Turtle) ()
chainCommands cmds = foldr (>>) (return ()) (map commandToAction cmds)


data Turtle = Turtle {
              angle :: Angle
             ,pos   :: Position
             ,stack :: [TurtleState]
              } deriving (Read,Eq,Show)


initTurtle = Turtle 0 (0,0) []

pop :: [a] -> (a,[a])
pop (x:xs) = (x,xs)
pop _      = error "pop empty stack"

push :: a -> [a] -> [a]
push a xs = (a:xs)

restore :: State ([Picture],Turtle) ()
restore =  state $ \(p,t) -> let ((newAngle,newPos), newStack) = pop $ stack t
                                 newT = Turtle newAngle newPos newStack
            in  ((),(p,newT))

save :: State ([Picture],Turtle) ()
save = state $ \(p,t) -> let currentState = (angle t, pos t) --current Turtle state (position)
                             newT = Turtle (angle t) (pos t) 
                                     (push currentState (stack t)) --Push current state onto stack
        in ((),(p,newT))


turn :: Angle -> State ([Picture],Turtle) ()
turn alfa = state $ \(p,t) -> let newAngle = angle t + alfa
                                  newT = Turtle newAngle (pos t) (stack t)
             in ((),(p,newT))


forward :: Length -> State ([Picture],Turtle) ()
forward l = state $ \(p,t) -> let path   = computePath (pos t) l (angle t)
                                  newP   = (line path) : p
                                  newPos = path !! 1 --new turtle is at the end of the line
                                  newT   = Turtle (angle t) newPos (stack t)
             in ((),(newP,newT))

backward :: Length -> State ([Picture],Turtle) ()
backward l = turn 180 >> forward l

setx :: Coordinate -> State ([Picture],Turtle) ()
setx x = state $ \(p,t) -> let (_,y ) = pos t
                               newPos = (x,y)
                               newT   = Turtle (angle t) newPos (stack t)
          in ((),(p,newT))

sety :: Coordinate -> State ([Picture],Turtle) ()
sety y = state $ \(p,t) -> let (x,_ ) = pos t
                               newPos = (x,y)
                               newT   = Turtle (angle t) newPos (stack t)
          in ((),(p,newT))

computePath :: Position -> Length -> Angle -> [Position]
computePath (x,y) length angle = [(x,y),(x+adjacent,y+opposite)]
    where adjacent = length * cos(radians)
          opposite = length * sin(radians)
          radians  = angleToRadian angle
angleToRadian = (* (pi/180))