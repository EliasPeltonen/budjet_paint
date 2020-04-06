import Interface._
import scala.util.control.Breaks._
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
  minimumSize = new Dimension(500,565)
  maximumSize = new Dimension(500,565)
  background = Color.white
  var redo: Option[Buffer[Shape]] = None
  listenTo(mouse.clicks, mouse.moves, keys)
  var shapes  = Buffer[Shape]()

  
  override def paintComponent(g: Graphics2D): Unit = {
        super.paintComponent(g)
        g.setColor(new Color(100, 100, 100))
        val h = size.height
        g.drawString("Press left mouse button and drag to paint.", 10, h - 20)
        g.setColor(Interface.colorButton.background)

        if (!shapes.isEmpty) {
          shapes.foreach(x => {
            g.setColor(x.color)
            if      (x.shape == "Circle") g.drawOval(x.pointVector(0), x.pointVector(1), x.pointVector(2), x.pointVector(3))
            else if (x.shape == "Square") g.drawRect(x.pointVector(0), x.pointVector(1), x.pointVector(2), x.pointVector(3))
            else if (x.shape == "Ellipse")g.drawOval(x.pointVector(0), x.pointVector(1), x.pointVector(2), x.pointVector(3))
            else if (x.shape == "Line")   g.drawLine(x.pointVector(0), x.pointVector(1), x.pointVector(2), x.pointVector(3))
          })
        }
      }
  
  var p1 = new Point(0,0)
  var p2 = new Point(0,0)
        
  reactions += {
    case e: MousePressed =>
      p1 = e.point
      requestFocusInWindow()
    case e: MouseDragged  => {
      if(Interface.shapeButton.text == "Free") {
        p2 = e.point
        shapes += new Line(p1,p2, Interface.colorButton.background)
        repaint()
        p1 = e.point}
    }
    case e: MouseReleased => {
      p2 = e.point
      Interface.shapeButton.text match {
        case "Circle"   => circleTo(p1,p2)
        case "Ellipse"  => ellipseTo(p1,p2)
        case "Square"   => squareTo(p1,p2)
        case "Free"     => shapes += new Line(p1,p2, Interface.colorButton.background)
        case _ =>
      }
      repaint()
    }
  } 
   
  def circleTo(p1:Point, p2: Point) = {
    val r = scala.math.sqrt(((p1.x - p2.x)*(p1.x - p2.x) + (p1.y - p2.y)*(p1.y - p2.y)))
    shapes += new Circle(p1,p2, Interface.colorButton.background)
  }
  def squareTo(p1:Point, p2:Point) {
    shapes += new Square(p1,p2, Interface.colorButton.background)
  }     
  def ellipseTo(p1:Point, p2:Point) {
    shapes += new Ellipse(p1,p2, Interface.colorButton.background)
  }
    
  override def repaint() {
    super.repaint()
  }
  
    
  def save(s:Buffer[Shape]) {
    val fileText = new Frame
    val cont = new FlowPanel
    val output = new TextField("", 20)
    val submitButton = new Button("Submit")
    submitButton.reactions += {
      case clickEvent: ButtonClicked =>
        val file = new File(output.text)
        val bw = new BufferedWriter(new FileWriter(file))
        for(i <- s) {
          bw.write(i.color.getRed + ":" + i.color.getGreen + ":" + i.color.getBlue + ";" + i.p1.x + ":" + i.p1.y + ":" + i.p2.x + ":" + i.p2.y + ";" + i.shape + "\n")      
        }
        bw.close()
        fileText.close()       
    }
    cont.contents += new Label("Enter filename:")
    cont.contents += output
    cont.contents += submitButton
    fileText.contents = cont
    fileText.open()
  }
  
  def load(filename: String) {
    val filen = new FileChooser(new File("."))
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
        case "Line" => {
          val color  = parts(0).split(":")
          val points = parts(1).split(":")
          shapes += new Line(new Point(points(0).toInt,points(1).toInt), new Point(points(2).toInt, points(3).toInt), new Color(color(0).toInt, color(1).toInt, color(2).toInt)) 
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
      shapes.clear()
      repaint()
  }
  
  Interface.undoButton.reactions += {
    case clickEvent: ButtonClicked =>
      if(!shapes.isEmpty) {
        if (shapes.last.shape != "Line") {
          redo = Some(Buffer(shapes.last))
          shapes -= shapes.last
          repaint() 
        } else {
          var lines = Buffer[Shape]()
          breakable {
            for (i <- shapes.size-1 to 0 by -1) {
               if (i != 0 && shapes(i).p1 == shapes(i-1).p2) {
                 lines += shapes(i)
               } else {
                 lines += shapes(i)
                 break
               }
            }
            
          }
          redo = Some(lines.reverse)
          for(i <- lines) shapes -= i
          repaint()
        }
        
      }
  }
  
  redoButton.reactions += {
    case clickEvent: ButtonClicked => {
      if (redo != None) {
        if (redo.get.size == 1) {
          shapes += redo.get(0)
          redo = None
          repaint()
        } else {
          redo.get.foreach(shapes += _)
          redo = None
          repaint()
        }
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

class Line(val p1: Point, val p2: Point, val color: Color) extends Shape {
  val shape = "Line"
  pointVector = Vector(p1.x, p1.y, p2.x, p2.y)
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
    if (p1.y < p2.y) {pointVector = Vector(p2.x,p1.y, p1.x-p2.x, p2.y-p1.y)}
    else             {pointVector = Vector(p2.x,p2.y, p1.x-p2.x, p1.y-p2.y)}
  }
}

class Ellipse(val p1: Point, val p2: Point, val color: Color) extends Shape {
  val shape = "Ellipse"
  if(p1.x < p2.x) {
    if (p1.y < p2.y) {pointVector = Vector(p1.x,p1.y, p2.x-p1.x, p2.y-p1.y)}
    else             {pointVector = Vector(p1.x,p2.y, p2.x-p1.x, p1.y-p2.y)}
  } else {
    if (p1.y < p2.y) {pointVector = Vector(p2.x, p1.y, p1.x-p2.x, p2.y-p1.y)}
    else             {pointVector = Vector(p2.x, p2.y, p1.x-p2.x, p1.y-p2.y)}
  }
}



