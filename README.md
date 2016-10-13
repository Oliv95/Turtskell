# Turtskell
Turtle graphics in haskell with a simple interface!

# Install
git clone https://github.com/Oliv95/Turtskell  
cd Turtskell  
./install.sh  

# How to use
The following Turtle commands are available:  
Forward length  --tells the turtle to move forward some given length  
Backward length --tells the turtle to mave backwards some given length  
Turn angle      --tells the turtle to turn counter clockwise by some number of degrees  
SetX Coordinate --sets the turtles x coordiate  
SetY Coordinate --sets the turtles y coordiate  
Save            --save current position and angle  (push)  
Restore         --set the angle and position to the most recenly saved (pop)  


Make a list of these commands, for example [Forward 5,Turn 90,Forward 15]  
To draw the picture give the list to the runCommands function and have main call it  
Done!  

