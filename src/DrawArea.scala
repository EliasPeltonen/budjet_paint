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

    reactions += {
      case e: MousePressed =>
        moveTo(e.point)
        requestFocusInWindow()
      case e: MouseDragged => lineTo(e.point)
      case e: MouseReleased => lineTo(e.point)
      case KeyTyped(_, 'c', _, _) =>
        path = new geom.GeneralPath
        repaint()
      case _: FocusLost => repaint()
    }

    /* records the dragging */
    var path = new geom.GeneralPath

    def lineTo(p: Point): Unit = {
      path.lineTo(p.x, p.y); repaint()
    }

    def moveTo(p: Point): Unit = {
      path.moveTo(p.x, p.y); repaint()
    }

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      g.setColor(new Color(100, 100, 100))
      val h = size.height
      g.drawString("Press left mouse button and drag to paint.", 10, h - 26)
      if (hasFocus) g.drawString("Press 'c' to clear.", 10, h - 10)
      g.setPaint(Interface.colorButton.background)
      g.draw(path)
    }
  
}