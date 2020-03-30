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

class Circle(p1: Point, p2: Point) extends DrawArea {
  
  
  def circleTo(g: Graphics2D, p1:Point, p2: Point) = {
        val r = scala.math.sqrt(((p1.x - p2.x)^2 + (p1.y - p2.y)^2).toDouble)
        g.drawOval(p1.x, p1.y, r.toInt, r.toInt)
      }
  
   override def paintComponent(g: Graphics2D): Unit = {

        g.setColor(new Color(100, 100, 100))
        g.drawString("Press left mouse button, drag and release to create circle.", 10, 500 - 26)
        g.setColor(Interface.colorButton.background)
        circleTo(g,p1,p2)
      }
   

}