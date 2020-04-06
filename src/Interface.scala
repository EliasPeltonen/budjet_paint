import scala.swing._

import java.awt.Color
import java.awt.{ Point, Rectangle }

import scala.io.Source
import javax.imageio.ImageIO
import java.io.File
import java.awt.image.BufferedImage
import scala.swing.event._



object Interface extends SimpleSwingApplication {
  
  val newButton   = new Button("New") 
  val undoButton  = new Button("Undo")
  val redoButton  = new Button("Redo")
  val colorButton = new Button("Color")
  colorButton.background = new Color(255,255,255)
  val shapeButton = new Button("Shape")
  val saveButton = new Button("Save")
  val loadButton = new Button("Load")

  
  colorButton.reactions += {
    case clickEvent: ButtonClicked => 
      colorPanel.open()
  }
  shapeButton.reactions += {
    case clickEvent: ButtonClicked => 
      shapePanel.open()
  }
  
  
  val topMenu = new MainFrame {
    title = "Draw"
    resizable = false
    
    val width      = 500
    val height     = 600
    val fullHeight = 600
    
    minimumSize   = new Dimension(width,fullHeight)
    preferredSize = new Dimension(width,fullHeight)
    maximumSize   = new Dimension(width,fullHeight)
    
    } 
    
    
    
    val topBar     = new FlowPanel
    topBar.maximumSize = new Dimension(500,35)
    topBar.contents += newButton
    topBar.contents += undoButton
    topBar.contents += redoButton
    topBar.contents += colorButton
    topBar.contents += shapeButton
    topBar.contents += saveButton
    topBar.contents += loadButton
    
    val drawArea = new DrawArea()
      
    val container = new BoxPanel(Orientation.Vertical)
    container.contents += topBar
    container.contents += drawArea
    

    topMenu.contents = container

    
    
  val colorPanel = new Frame {
      val width = 250
      val height = 200
      preferredSize = new Dimension(width,height)
      
      contents = new GridPanel(4,4) {
        val redButton = new Button("") 
        redButton.background = new Color(204,0,0)
        val lRedButton = new Button("")
        lRedButton.background = new Color(255,0,0)
        val blueButton = new Button("")
        blueButton.background = new Color(0,0,204)
        val lBlueButton = new Button("")
        lBlueButton.background = new Color(0,0,255)
        val tealButton = new Button("")
        tealButton.background = new Color(0,204,204)
        val lTealButton = new Button("")
        lTealButton.background = new Color(0,255,255)
        val greenButton = new Button("")
        greenButton.background = new Color(102,204,0)
        val lGreenButton = new Button("")
        lGreenButton.background = new Color(128,255,0)
        val yellowButton = new Button("")
        yellowButton.background = new Color(204,204,0)
        val lYellowButton = new Button("")
        lYellowButton.background = new Color(255,255,0)
        val orangeButton = new Button("")
        orangeButton.background = new Color(204,102,0)
        val lOrangeButton = new Button("")
        lOrangeButton.background = new Color(255,128,0)
        val purpleButton = new Button("")
        purpleButton.background = new Color(204,0,204)
        val lPurpleButton = new Button("")
        lPurpleButton.background = new Color(255,0,255)
        val greyButton = new Button("")
        greyButton.background = new Color(128,128,128)
        val blackButton = new Button("")
        blackButton.background = new Color(255,255,255)
        val whiteButton = new Button("")
        whiteButton.background = new Color(0,0,0)
        
        contents ++= Vector(
            redButton,
            lRedButton,
            orangeButton,
            lOrangeButton,
            yellowButton,
            lYellowButton,
            greenButton,
            lGreenButton,
            tealButton,
            lTealButton,
            blueButton, 
            lBlueButton,
            lPurpleButton,
            greyButton,
            blackButton,
            whiteButton
            )
            
        listenTo(redButton,
            lRedButton,
            orangeButton,
            lOrangeButton,
            yellowButton,
            lYellowButton,
            greenButton,
            lGreenButton,
            tealButton,
            lTealButton,
            blueButton, 
            lBlueButton,
            lPurpleButton,
            greyButton,
            blackButton,
            whiteButton)
        reactions += {
          case ButtonClicked(`redButton`)     => retColor(redButton)
          case ButtonClicked(`lRedButton`)    => retColor(lRedButton)
          case ButtonClicked(`orangeButton`)  => retColor(orangeButton)
          case ButtonClicked(`lOrangeButton`) => retColor(lOrangeButton)
          case ButtonClicked(`yellowButton`)  => retColor(yellowButton)
          case ButtonClicked(`lYellowButton`) => retColor(lYellowButton)
          case ButtonClicked(`greenButton`)   => retColor(greenButton)
          case ButtonClicked(`lGreenButton`)  => retColor(lGreenButton)
          case ButtonClicked(`tealButton`)    => retColor(tealButton)
          case ButtonClicked(`lTealButton`)   => retColor(lTealButton)
          case ButtonClicked(`blueButton`)    => retColor(blueButton)
          case ButtonClicked(`lBlueButton`)   => retColor(lBlueButton)
          case ButtonClicked(`lPurpleButton`) => retColor(lPurpleButton)
          case ButtonClicked(`greyButton`)    => retColor(greyButton)
          case ButtonClicked(`blackButton`)   => retColor(blackButton)
          case ButtonClicked(`whiteButton`)   => retColor(whiteButton)
        }
      }
      
      
      def retColor(button: Button) = {
        Interface.colorButton.background = button.background
        this.close()
      }      
  }
    
  val shapePanel = new Frame {
    val width  = 500
    val height = 100
    minimumSize = new Dimension(width,height)
    
    val freeButton   = new Button("Free")
    val circleButton = new Button("Circle")
    val squareButton = new Button("Square")
    val ellipseButton = new Button("Ellipse")
    
    contents = new GridPanel(1,4) {
      contents ++= Vector(freeButton, circleButton, squareButton, ellipseButton)
      listenTo(freeButton, circleButton, squareButton, ellipseButton)
      reactions += {
        case ButtonClicked(`freeButton`)    => setShape(freeButton)
        case ButtonClicked(`circleButton`)  => setShape(circleButton)
        case ButtonClicked(`squareButton`)  => setShape(squareButton)
        case ButtonClicked(`ellipseButton`) => setShape(ellipseButton)
      }
    }
    
    def setShape(button: Button) = {
      Interface.shapeButton.text = button.text
      this.close()
    }
    
    
  }
    
  

    
  def top = this.topMenu
    
   
}