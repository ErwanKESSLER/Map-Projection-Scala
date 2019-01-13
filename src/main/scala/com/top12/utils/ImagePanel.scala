package com.top12.utils
import scala.math.max
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO

import scala.swing._

class ImagePanel extends Panel {
  private var _imagePath:File = _
  private var bufferedImage: BufferedImage = _
  private var w = 100
  private var h = 100

  def imagePath:File = _imagePath

  def width:Int = w

  def height:Int = h

  def width_=(wi: Int): Unit = {
    w = wi
  }

  def height_=(he: Int): Unit = {
    h = he
  }

  def imagePath_=(value: File): Unit = {
    _imagePath = value
    bufferedImage = ImageIO.read(_imagePath)
  }


  override def paintComponent(g: Graphics2D):Unit = {
    val m=max(bufferedImage.getWidth(),bufferedImage.getHeight())

    if (null != bufferedImage) g.drawImage(bufferedImage, 0, 0, w*bufferedImage.getWidth()/m, w*bufferedImage.getHeight()/m, null)
  }
}

object ImagePanel {
  def apply() = new ImagePanel()
  def reload()=new ImagePanel()
}
