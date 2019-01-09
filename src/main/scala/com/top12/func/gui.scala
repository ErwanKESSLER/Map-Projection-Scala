package com.top12.func

import java.awt.Toolkit
import java.io.File
import java.net.URLDecoder

import scala.swing._

class gui extends SimpleSwingApplication {
  def top:MainFrame = new MainFrame {
    frame =>
    preferredSize = siz(0).left.get
    title = "Map Projection TOP 12"
    location = siz(3).right.get
    val sf: Frame = new Frame {
      secondFrame =>
      title = "Escape"
      visible = false
      location = siz(1).right.get
      preferredSize = siz(2).left.get
      contents = new FlowPanel {
        contents += new Button(Action("Recover") {
          frame.pack()
          frame.visible = true
        })
        contents += new Button(Action("Exit") {
          quit()
        })
      }
    }
    val exitBtn = new Button(Action("Exit") {
      sf.visible = true
      frame.dispose()
    })

    val step1to4: FlowPanel = new FlowPanel{

      preferredSize = siz(5).left.get
      border = Swing.TitledBorder(Swing.EtchedBorder(Swing.Lowered), "Etape 1 à 4")
      val step1: BoxPanel = new BoxPanel(Orientation.Vertical) {
        preferredSize = siz(4).left.get
        border = Swing.TitledBorder(Swing.EtchedBorder(Swing.Lowered), "Etape 1: Charger les données")
      }

      val step2: BoxPanel = new BoxPanel(Orientation.Vertical) {
        preferredSize = siz(4).left.get
        border = Swing.TitledBorder(Swing.EtchedBorder(Swing.Lowered), "Etape 2: Calculer les distances")
      }

      val step3: BoxPanel = new BoxPanel(Orientation.Vertical) {
        preferredSize = siz(4).left.get
        border = Swing.TitledBorder(Swing.EtchedBorder(Swing.Lowered), "Etape 3: Statistiques")
      }

      val step4: BoxPanel = new BoxPanel(Orientation.Vertical) {
        preferredSize = siz(4).left.get
        border = Swing.TitledBorder(Swing.EtchedBorder(Swing.Lowered), "Etape 4: Restrictions")
      }
      contents += step1
      contents += step2
      contents += step3
      contents += step4

    }

    val image:com.top12.utils.ImagePanel=new com.top12.utils.ImagePanel{
      preferredSize=siz(6).left.get
      val filename:String="Sources/Conformal/guyou.jpg"
      val extension:String = filename.split("\\.")(1)
      val path:String = "/Results/" + filename.split("\\.")(0).split("/").dropRight(1).drop(1).mkString("/") + "/"
      val nam:String = filename.split("\\.")(0).split("/").last
      val jarPath:String = URLDecoder.decode(getClass.getProtectionDomain.getCodeSource.getLocation.getPath, "UTF-8")
      val parent:File=new File(jarPath.substring(0, jarPath.lastIndexOf("/")) +path)
      parent.mkdirs()
      val file:File=new File(parent,nam + "_result." + extension)
      width=preferredSize.width
      height=preferredSize.height
      imagePath=file.getPath
    }
    contents = new FlowPanel {
      contents += step1to4
      contents += Swing.Glue
      contents+=image
    }

  }


  def siz(i: Int): Either[swing.Dimension, swing.Point] = {
    val screenSize = Toolkit.getDefaultToolkit.getScreenSize
    val w = screenSize.getWidth
    val h = screenSize.getHeight
    i match {
      case 0 => Left(new Dimension((w * 0.75).toInt, (h * 0.75).toInt))
      case 1 => Right(new Point((w * 0.5 - 100).toInt, (h * 0.5 - 37.5).toInt))
      case 2 => Left(new Dimension(200, 75))
      case 3 => Right(new Point((w * 0.125).toInt, (h * 0.125).toInt))
      case 4 => Left(new Dimension((w * 0.75 * 0.2*0.94).toInt, (h * 0.75 * 0.25*0.95*0.93).toInt))
      case 5 => Left(new Dimension((w * 0.75 * 0.2).toInt, (h * 0.75*0.94).toInt))
      case 6=>Left(new Dimension((w * 0.75*0.6).toInt, (h * 0.75*0.6*0.94).toInt))
    }

  }


}

