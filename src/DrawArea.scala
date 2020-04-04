import Interface._
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
  Interface.shapeButton.text = "Square"
  

  listenTo(mouse.clicks, mouse.moves, keys)
  var path = new geom.GeneralPath
  var circles = scala.collection.mutable.Map[scala.swing.Color, Vector[Int]]()
  var squares = scala.collection.mutable.Map[scala.swing.Color, Vector[Int]]()
  
  override def paintComponent(g: Graphics2D): Unit = {
        super.paintComponent(g)
        g.setColor(new Color(100, 100, 100))
        val h = size.height
        g.drawString("Press left mouse button and drag to paint.", 10, h - 26)
        if (hasFocus) g.drawString("Press 'c' to clear.", 10, h - 10)
        g.setColor(Interface.colorButton.background)
        g.draw(path)
        if (!circles.isEmpty) { circles.foreach(x => { 
          g.setColor(x._1) 
          g.drawOval(x._2(0), x._2(1), x._2(2),x._2(3))         
          })
        } 
        if (!squares.isEmpty) {
          squares.foreach(x => { 
          g.setColor(x._1) 
          g.drawRect(x._2(0), x._2(1), x._2(2),x._2(3))         
          })
        }
        

      }
  
     
  Interface.shapeButton.text match {
    case "Free" => {
      
      var points = scala.collection.mutable.Buffer()
      
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
        circles += ((Interface.colorButton.background, Vector((p1.x-r).toInt, (p1.y-r).toInt, (r*2).toInt, (r*2).toInt)))

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
        if(p1.x < p2.x) {
          if (p1.y < p2.y) squares += ((Interface.colorButton.background, Vector(p1.x,p1.y, p2.x-p1.x, p2.y-p1.y))) 
          else             squares += ((Interface.colorButton.background, Vector(p1.x,p1.y, p2.x-p1.x, p2.y-p1.y))) 
        } else if (p1.x > p2.x)
          if (p1.y < p2.y) squares += ((Interface.colorButton.background, Vector(p1.x,p1.y, p2.x-p1.x, p2.y-p1.y))) 
          else             squares += ((Interface.colorButton.background, Vector(p1.x,p1.y, p2.x-p1.x, p2.y-p1.y))) 
      }
    }
    
    case "Ellipse" => ???
    
    case _ => 

  }

   
  override def repaint() {
    super.repaint()
  }
  
  newButton.reactions += {
    case clickEvent: ButtonClicked =>
      path = new geom.GeneralPath
      repaint()
  }
  
  
}