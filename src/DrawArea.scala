import Interface._
import java.io._
import scala.collection.mutable.Buffer
import java.awt.Graphics2D
import scala.swing._
import java.awt.geom
import java.awt.Color
import javax.imageio.ImageIO
import java.io.File
import java.awt.image.BufferedImage
import java.awt.GraphicsConfiguration
import java.awt.Transparency
import scala.swing.event._
import java.awt.RenderingHints

class DrawArea() extends Panel {
  minimumSize = new Dimension(500,500)
  background = Color.white
  // Application doesn't detect changing of shape from shape menu, edit here to test 
  Interface.shapeButton.text = "Circle"
  var s = "Circle"
  var redo: Option[Shape] = None

  listenTo(mouse.clicks, mouse.moves, keys)
  var path = new geom.GeneralPath
  var shapes  = Buffer[Shape]()

  
  override def paintComponent(g: Graphics2D): Unit = {
        super.paintComponent(g)
        g.setColor(new Color(100, 100, 100))
        val h = size.height
        g.drawString("Press left mouse button and drag to paint.", 10, h - 26)
        if (hasFocus) g.drawString("Press 'c' to clear.", 10, h - 10)
        g.setColor(Interface.colorButton.background)
        g.draw(path)

        if (!shapes.isEmpty) {
          shapes.foreach(x => {
            g.setColor(x.color)
            if      (x.shape == "Circle") g.drawOval(x.pointVector(0), x.pointVector(1), x.pointVector(2), x.pointVector(3))
            else if (x.shape == "Square") g.drawRect(x.pointVector(0), x.pointVector(1), x.pointVector(3), x.pointVector(3))
          })
        }
        

      }
  
     
  s match {
    case "Free" => {
      
      def lineTo(p: Point): Unit = {
        path.lineTo(p.x, p.y); repaint()
      }
  
      def moveTo(p: Point): Unit = {
        path.moveTo(p.x, p.y); repaint()
        
      }
            
      reactions += {
        case e: MousePressed =>
          moveTo(e.point)
          requestFocusInWindow()
        case e: MouseDragged  => lineTo(e.point)
        case e: MouseReleased => lineTo(e.point)
        case KeyTyped(_, 'c', _, _) =>
          path = new geom.GeneralPath
          repaint()
        case _: FocusLost => repaint()
      }
  
      /* records the dragging */

    }
    
    case "Circle" => {
      var p1 = new Point(0,0)
      var p2 = new Point(0,0)
      reactions += {
        case e: MousePressed =>
          p1 = e.point
          requestFocusInWindow()
        case e: MouseReleased => { 
          p2 = e.point
          circleTo(p1,p2)
          repaint()
        }
        case KeyTyped(_, 'c', _, _) =>
          repaint()
        case _: FocusLost => repaint()
      }
      
      def circleTo(p1:Point, p2: Point) = {
        val r = scala.math.sqrt(((p1.x - p2.x)*(p1.x - p2.x) + (p1.y - p2.y)*(p1.y - p2.y)))
        shapes += new Circle(p1,p2, Interface.colorButton.background)
      }
      
    }
    
    case "Square" => {
      var p1 = new Point(0,0)
      var p2 = new Point(0,0)
      reactions += {
        case e: MousePressed =>
          p1 = e.point
          println(p1.x + " ja " + p1.y)
          requestFocusInWindow()
        case e: MouseReleased => { 
          p2 = e.point
          println(p2.x + " ja " + p2.y)
          squareTo(p1,p2)
          repaint()
        }
        case KeyTyped(_, 'c', _, _) =>
          repaint()
        case _: FocusLost => repaint()
      }
      
      def squareTo(p1:Point, p2:Point) {
        shapes += new Square(p1,p2, Interface.colorButton.background)
      } 
    }
    
    case "Ellipse" => ???
    
    case _ => 

  }

   
  override def repaint() {
    super.repaint()
  }
  
  def save(s:Buffer[Shape]) {
    val file = new File("testi")
    val bw = new BufferedWriter(new FileWriter(file))
    for(i <- s) {
      bw.write(i.color.getRed + ":" + i.color.getGreen + ":" + i.color.getBlue + ";" + i.p1.x + ":" + i.p1.y + ":" + i.p2.x + ":" + i.p2.y + ";" + i.shape + "\n")      
    }
    bw.close()
  }
  
  def load(filename: String) {
    val bufferedSource = scala.io.Source.fromFile(filename)
    for (line <- bufferedSource.getLines) {
      val parts = line.split(";")
      parts(2) match {
        case "Circle" => {
          val color  = parts(0).split(":")
          val points = parts(1).split(":")
          shapes += new Circle(new Point(points(0).toInt,points(1).toInt), new Point(points(2).toInt, points(3).toInt), new Color(color(0).toInt, color(1).toInt, color(2).toInt))          
        }
        case "Square" => {
          val color  = parts(0).split(":")
          val points = parts(1).split(":")
          shapes += new Square(new Point(points(0).toInt,points(1).toInt), new Point(points(2).toInt, points(3).toInt), new Color(color(0).toInt, color(1).toInt, color(2).toInt)) 
        }
      }
    }
    bufferedSource.close()
  }
  
  saveButton.reactions += {
    case clickEvent: ButtonClicked => 
      save(shapes)
  }
  
  loadButton.reactions += {
    case clickEvent: ButtonClicked => {
      shapes.clear()
      load("testi")
      repaint()
    }
  }
  
  newButton.reactions += {
    case clickEvent: ButtonClicked =>
      path = new geom.GeneralPath
      shapes.clear()
      repaint()
  }
  
  Interface.undoButton.reactions += {
    case clickEvent: ButtonClicked =>
      redo = Some(shapes.last)
      shapes -= shapes.last
      repaint()
  }
  
  redoButton.reactions += {
    case clickEvent: ButtonClicked => {
      if (redo != None) {
        shapes += redo.get
        redo = None
        repaint()
      }
    }
  }

  
  
}



trait Shape {
  val color: Color
  var pointVector: Vector[Int] = Vector(0,0,0,0)
  val shape: String
  val p1: Point
  val p2: Point
  
}

class Circle(val p1: Point, val p2: Point, val color: Color) extends Shape {
  val shape = "Circle"
  
  val r = scala.math.sqrt(((p1.x - p2.x)*(p1.x - p2.x) + (p1.y - p2.y)*(p1.y - p2.y)))
  pointVector = Vector((p1.x-r).toInt, (p1.y-r).toInt, (r*2).toInt, (r*2).toInt)
  
}
  
class Square(val p1: Point, val p2: Point, val color: Color) extends Shape {
  val shape = "Square"
  if(p1.x < p2.x) {
    if (p1.y < p2.y) {pointVector = Vector(p1.x,p1.y, p2.x-p1.x, p2.y-p1.y)}
    else             {pointVector = Vector(p1.x,p2.y, p2.x-p1.x, p1.y-p2.y)}
  } else {
    if (p1.y < p2.y) {pointVector = Vector(p1.x-(p1.x-p2.x),p1.y, p1.x-p2.x, p2.y-p1.y)}
    else             {pointVector = Vector(p2.x,p2.y, p2.x-p1.x, p1.y-p2.y)}
  }
}



