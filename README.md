# Turtskell
Turtle graphics in haskell with a simple interface!

# Install
git clone https://github.com/Oliv95/Turtskell  
cd Turtskell  
./install.sh  

# How to use
The following Turtle commands are available:  
forward length  --tells the turtle to move forward some given length  
backward length --tells the turtle to mave backwards some given length  
turn angle      --tells the turtle to turn counter clockwise by some number of degrees  
setX Coordinate --sets the turtles x coordinate  
setY Coordinate --sets the turtles y coordinate  
save            --save current position and angle  (push)  
restore         --set the angle and position to the most recenly saved (pop)  
getAngle        --returns the current angle
setAngle angle  --sets the turles current angle
getX            --returns the current x coordinate
getY            --returns the current y coordinate


Make a for example a list of these commands, for example [forward 5,turn 90,forward 15]  
To draw the picture give the list to the runCommands function and have main call it  
You can also create your own command by gluing together other commands and draw it with the runCommand function
Done!  

