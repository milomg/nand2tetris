```java
class Dot {
  field int x, y, lastx, lasty, mass;
  
  constructor Dot new(x,y,mass);

  method void setX(int newX);
  method int getX();
  method void setY(int newX);
  method int getY();

  // add to y
  method void gravity();

  method void update();

  // draw a circle with radius mass
  method void draw();
}

class Edge {
  field Dot start, end; 
  field int stiffness;

  constructor Edge new(start,end,stiffness);

  // I moved this out of the node class because it operates on both nodes at once
  method void constrainEdge(); 

  // Draw a line from start to end
  method void draw();
}

class Game {
  field int mx, my; // mouseX, mouseY
  field boolean holding; // is mouse down (space to toggle)
  field Dot currentNode; // selected node
  field Array dots;
  field Array edges;
  field int dotsLength, edgesLength;
  field boolean paused;
  method void main();
  method void drawEdges();
  method void drawDots();
  method void update();
  method void updateKeys();
  method void drawMouse();
}
```