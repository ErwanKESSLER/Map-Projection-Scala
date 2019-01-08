package com.top12.func
import java.awt.Toolkit
import scala.swing._

class gui extends SimpleSwingApplication {
  def top = new MainFrame {frame =>
    val closeBtn = new Button(Action("Close Me") {
      sf.visible=true
      frame.dispose()
    })
    val exitBtn = new Button(Action("Exit") {
      quit()
    })
    val sf = new Frame {
      secondFrame =>
      title = "Map Projection TOP 12"
      visible = false

      preferredSize = desiredInitialSize
      contents = new FlowPanel {
        contents += new Button(Action("Recover") {
          frame.pack()
          frame.visible=true
        })
        contents += new Button(Action("Exit") {
          quit()
        })
      }
    }
    val searchField = new TextField { columns = 32 }
    val step1to4=new BoxPanel(Orientation.Vertical){
      val step1=new BoxPanel(Orientation.Vertical){
        contents+=closeBtn
        contents += exitBtn
      }
      contents += Swing.VStrut(10)
      val step2=new BoxPanel(Orientation.Vertical){
        contents+=searchField
      }
      contents += Swing.VStrut(10)
      val step3=new BoxPanel(Orientation.Vertical){

      }
      contents += Swing.VStrut(10)
      val step4=new BoxPanel(Orientation.Vertical){

      }
    }
    contents = new FlowPanel {
      contents+=step1to4

    }

  }


  def desiredInitialSize = {
    val screenSize =   Toolkit.getDefaultToolkit.getScreenSize
    val w = (screenSize.getWidth * 0.75).toInt
    val h = (screenSize.getHeight * 0.75).toInt
    new Dimension(w, h)
  }


}