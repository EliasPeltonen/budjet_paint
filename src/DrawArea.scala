import Interface._

import scala.util.control.Breaks._
import java.io._
import scala.collection.mutable.Buffer
import java.awt.Graphics2D._
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
  minimumSize = new Dimension(600,700)
  maximumSize = new Dimension(600,700)
  background = Color.white
  var redo: Option[Buffer[Shape]] = None
  listenTo(mouse.clicks, mouse.moves, keys)
  var strings = Buffer[Text]()
  var shapes  = Buffer[Shape]()

  
  override def paintComponent(g: Graphics2D): Unit = {
        super.paintComponent(g)
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
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
        
        
        if (!strings.isEmpty) {
          strings.foreach(x => {
            g.setFont(new Font("Arial", 20,20))
            g.drawString(x.text,x.p1.x, x.p1.y)
            })
        }
      }
  
  var p1 = new Point(0,0)
  var p2 = new Point(0,0)
        
  reactions += {
    case e: MousePressed =>
      p1 = e.point
      requestFocusInWindow()
      circleTo(p1,p1)
    case e: MouseDragged  => {
      Interface.shapeButton.text match {
        case "Free" => {     
          p2 = e.point
          shapes += new Line(p1,p2, Interface.colorButton.background)
          repaint()
          p1 = e.point}
        case "Circle"   => {
          p2 = e.point
          shapes -= shapes.last
          circleTo(p1,p2)
          repaint()}
        case "Ellipse"  => {
          p2 = e.point
          shapes -= shapes.last
          ellipseTo(p1,p2)
          repaint()}
        case "Square"   => {
          p2 = e.point
          shapes -= shapes.last
          squareTo(p1,p2)
          repaint()}
        case _ =>
      }
    }
    case e: MouseReleased => {
      p2 = e.point
      Interface.shapeButton.text match {
        case "Circle"   => {shapes -= shapes.last
          circleTo(p1,p2)}
        case "Ellipse"  => {shapes -= shapes.last
          ellipseTo(p1,p2)}
        case "Square"   => {shapes -= shapes.last
          squareTo(p1,p2)}
        case "Free"     => shapes += new Line(p1,p2, Interface.colorButton.background)
        case "Text"     => textTo(p1,p2)
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
  
  def textTo(p1:Point, p2: Point) {
    val fileText = new Frame
    val cont = new FlowPanel
    val output = new TextField("", 20)
    val submitButton = new Button("Submit")
    submitButton.reactions += {
      case clickEvent: ButtonClicked => {
        strings += new Text(p1,p2, 20, output.text)
        fileText.close()
            repaint()
      }
    }
    cont.contents += output
    cont.contents += submitButton
    fileText.contents = cont
    fileText.open()

  }
    
  override def repaint() {
    super.repaint()
  }
  
    
  def save(s:Buffer[Shape], x:Buffer[Text]) {
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
        for(j <- x) {
          bw.write(j.p1.x + ":" + j.p1.y + ":" + j.p2.x + ":" + j.p2.y + ";" + j.size + ";" + "Text" + ";" + j.text+ "\n") 
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
        case "Text" => {
          val points = parts(0).split(":")
          strings += new Text(new Point(points(0).toInt,points(1).toInt), new Point(points(2).toInt, points(3).toInt), parts(1).toInt, parts(3))
        }
      }
    }
    bufferedSource.close()
  }
  
  saveButton.reactions += {
    case clickEvent: ButtonClicked => 
      save(shapes, strings)
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



class Text(val p1: Point, val p2: Point, val size: Int, val text:String) 

trait Shape {
  val color: Color
  var pointVector: Vector[Int] = Vector(0,0,0,0)
  val shape: String
  val p1: Point
  val p2: Point
  def contains(p: Point): Boolean
  
}

class Line(val p1: Point, val p2: Point, val color: Color) extends Shape {
  val shape = "Line"
  pointVector = Vector(p1.x, p1.y, p2.x, p2.y)
  def contains(p: Point): Boolean = {
    p == p1 || p == p2
  }
}

class Circle(val p1: Point, val p2: Point, val color: Color) extends Shape {
  val shape = "Circle"
  
  val r = scala.math.sqrt(((p1.x - p2.x)*(p1.x - p2.x) + (p1.y - p2.y)*(p1.y - p2.y)))
  pointVector = Vector((p1.x-r).toInt, (p1.y-r).toInt, (r*2).toInt, (r*2).toInt)
  
  def contains(p: Point): Boolean = {
    p.distance(p1) == r
  }
  
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
  def contains(p: Point): Boolean = {
    false
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
  def contains(p: Point): Boolean = {
    false
  }
}



