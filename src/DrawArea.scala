import Interface._
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
  preferredSize = new Dimension(500,500)
  background = Color.white

  listenTo(mouse.clicks, mouse.moves, keys)
  var path = new geom.GeneralPath
  
  override def paintComponent(g: Graphics2D): Unit = {
        super.paintComponent(g)
        g.setColor(new Color(100, 100, 100))
        val h = size.height
        g.drawString("Press left mouse button and drag to paint.", 10, h - 26)
        if (hasFocus) g.drawString("Press 'c' to clear.", 10, h - 10)
        g.setColor(Interface.colorButton.background)
        g.draw(path)

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
      
      def paintComponent(g: Graphics2D): Unit = {
        super.paintComponent(g)
        g.setColor(new Color(100, 100, 100))
        val h = size.height
        g.drawString("Press left mouse button and drag to paint.", 10, h - 26)
        if (hasFocus) g.drawString("Press 'c' to clear.", 10, h - 10)
        g.setColor(Interface.colorButton.background)
        g.draw(path)

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
          path = new geom.GeneralPath
        }
        case KeyTyped(_, 'c', _, _) =>
          path = new geom.GeneralPath
          repaint()
        case _: FocusLost => repaint()
      }      

    }
    
    case "Square" => ???          
    
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