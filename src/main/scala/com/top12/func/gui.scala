package com.top12.func

import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.{Color, Toolkit}
import java.io.File
import java.net.URLDecoder
import scala.util.matching.Regex
import javax.swing.table.DefaultTableCellRenderer

import scala.swing._
import scala.swing.event.{ButtonClicked, EditDone, SelectionChanged}
import scala.util.matching.Regex

class gui extends SimpleSwingApplication {
  val loadAirport = new com.top12.func.loadAirports
  val distance = new com.top12.func.distanceAirports
  val stats = new com.top12.func.statsAirports
  val util = new com.top12.utils.utils
  val restriction = new com.top12.func.restrictArea
  val density = new com.top12.func.densityAirports
  val equidistant = new com.top12.func.equirectangularProjection
  val conformal = new com.top12.func.conformalProjections
  val equalArea = new com.top12.func.equalAreaProjections
  val airports: Array[(Int, String, String, String, Double, Double)] = loadAirport.loadAirport(filename = "airports.dat")
  val airports2: Array[Array[Any]] = airports.map(el => el.productIterator.map(el => el.toString.asInstanceOf[Any]).toArray)
  lazy val distances: Array[Double] = distance.distancesArray(airports)
  val dmn: String = stats.distanceMin(distances).toString
  val dmx: String = stats.distanceMax(distances).toString
  val dmy: String = stats.distanceMoyenne(distances).toString
  val dmd: String = stats.distanceMediane2(distances).toString
  val ect: String = stats.ecartType(distances).toString
  var currentButton:Button=_
  var currentCheckBox:CheckBox=_
  var saveCondition:String=""
  var saveTypeCondition:String="Par ID"
  def top: MainFrame = new MainFrame {
    frame =>
    title = "Map Projection TOP 12"
    preferredSize = siz(0).left.get
    location = siz(3).right.get
    //-------------------------------------Exiting--------------------------------//
    val sf: Frame = new Frame {
      secondFrame =>
      secondFrame.peer.addWindowListener(new WindowAdapter() {
        override def windowClosing(e: WindowEvent) {
          System.exit(0)
        }
      })
      title = "Escape"
      visible = false
      location = siz(1).right.get
      preferredSize = siz(2).left.get
      contents = new FlowPanel {
        contents += new Button(Action("Recover") {
          frame.pack()
          frame.visible = true
          secondFrame.visible = false
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

    val step1to4: FlowPanel = new FlowPanel {
      preferredSize = siz(5).left.get
      border = Swing.TitledBorder(Swing.EtchedBorder(Swing.Lowered), "Etape 1 à 4")
      val step1: BoxPanel = new BoxPanel(Orientation.Vertical) {
        preferredSize = siz(4).left.get
        border = Swing.TitledBorder(Swing.EtchedBorder(Swing.Lowered), "Etape 1: Charger les données")
        contents += new Button(Action("Afficher les données") {
          airportsList.visible = true
        })
      }
      val step2: BoxPanel = new BoxPanel(Orientation.Vertical) {
        preferredSize = siz(4).left.get
        val distanceA1: TextArea = new TextArea {
          rows = 1
          lineWrap = true
          wordWrap = true
          editable = false
          text = "Aéroport 1: " + airports(0)._2
          caret.position = 0
        }
        val distanceA2: TextArea = new TextArea {
          rows = 1
          lineWrap = true
          wordWrap = true
          editable = false
          text = "Aéroport 2: " + airports(1)._2
          caret.position = 0
        }
        val distanceA1A2: TextArea = new TextArea {
          rows = 1
          lineWrap = true
          wordWrap = true
          editable = false
          text = "Distance (en m) entre aéroport 1 et aéroport 2: " + distance.distanceHaversine(airports(0)._5, airports(1)._5, airports(0)._6, airports(1)._6).toString
          caret.position = 0
        }
        border = Swing.TitledBorder(Swing.EtchedBorder(Swing.Lowered), "Etape 2: Calculer les distances")
        contents += new Button(Action("Afficher les distances") {
          airportsDistance.visible = true
        })
        contents += distanceA1
        contents += distanceA2
        contents += distanceA1A2


      }
      val step3: BoxPanel = new BoxPanel(Orientation.Vertical) {
        preferredSize = siz(4).left.get
        border = Swing.TitledBorder(Swing.EtchedBorder(Swing.Lowered), "Etape 3: Statistiques")
        val distanceMin: Label = new Label {
          text = "Minimum : " + dmn + " m."
        }
        val distanceMax: Label = new Label {
          text = "Maximum : " + dmx + " m."
        }
        val distanceMoyenne: Label = new Label {
          text = "Moyenne : " + dmy + " m."
        }
        val distanceMediane: Label = new Label {
          text = "Médiane : " + dmd + " m."
        }
        val ecartype: Label = new Label {
          text = "Ecart type : " + ect + " m."
        }
        contents += distanceMin
        contents += distanceMax
        contents += distanceMoyenne
        contents += distanceMediane
        contents += ecartype
      }
      val step4: BoxPanel = new BoxPanel(Orientation.Vertical) {
        preferredSize = siz(4).left.get
        border = Swing.TitledBorder(Swing.EtchedBorder(Swing.Lowered), "Etape 4: Selection")
        val FirstPointButton = new Button(airports(0)._2)
        val SecondPointButton = new Button(airports(1)._2)
        val ConditionsButton = new Button(airports(1)._2)
        val FirstCheckBox = new CheckBox()
        val SecondCheckBox = new CheckBox()
        val ThirdCheckBox = new CheckBox()
        val FourthCheckBox = new CheckBox()
        listenTo(FirstPointButton)
        listenTo(SecondPointButton)
        listenTo(ConditionsButton)
        listenTo(FirstCheckBox)
        listenTo(SecondCheckBox)
        listenTo(ThirdCheckBox)
        listenTo(FourthCheckBox)
        reactions += {
          case ButtonClicked(FirstPointButton) => SmartOneSelect(FirstPointButton)
          case ButtonClicked(SecondPointButton) => SmartOneSelect(SecondPointButton)
          case ButtonClicked(ConditionsButton) => SmartConditionsSelect(ConditionsButton)
          case ButtonClicked(FirstCheckBox) => excludeAndNotify(FirstCheckBox)
          case ButtonClicked(SecondCheckBox) => excludeAndNotify(SecondCheckBox)
          case ButtonClicked(ThirdCheckBox) => excludeAndNotify(ThirdCheckBox)
          case ButtonClicked(FourthCheckBox) => excludeAndNotify(FourthCheckBox)


        }
        FirstCheckBox.selected = true
        contents += new BoxPanel(Orientation.Horizontal) {
          contents += new Label {
            text = "Un Point"
          }
          contents += Swing.HStrut(20)
          contents += FirstPointButton
          contents += Swing.HStrut(20)
          contents += FirstCheckBox
        }
        contents += new BoxPanel(Orientation.Horizontal) {
          contents += new Label {
            text = "Deux Points"
          }
          contents += Swing.HStrut(20)
          contents += SecondPointButton
          contents += Swing.HStrut(20)
          contents += SecondCheckBox
        }
        contents += new BoxPanel(Orientation.Horizontal) {
          contents += new Label {
            text = "Multiple Points"
          }
          contents += Swing.HStrut(20)
          contents += ConditionsButton
          contents += Swing.HStrut(20)
          contents += ThirdCheckBox
        }
        contents += new BoxPanel(Orientation.Horizontal) {
          contents += new Label {
            text = "Tout les Points"
          }
          contents += Swing.HStrut(20)
          contents += FourthCheckBox
        }
      }
      contents += step1
      contents += step2
      contents += step3
      contents += step4
    }

    //-------------------------------------------Etape 1----------------------------------------//
    lazy val airportsList: Frame = new Frame {
      airportFrame =>
      title = "Liste des aéroports"
      preferredSize = siz(0).left.get
      location = siz(3).right.get
      visible = false
      val exited = new Button(Action("Exit") {
        airportFrame.visible = false
      })

      val table: Table = new Table(airports2, Seq("ID", "Name", "City", "Country", "Latitude", "Longitude")) {
        background = new Color(204, 204, 204)
        autoResizeMode = Table.AutoResizeMode.SubsequentColumns
      }

      val centerRenderer = new DefaultTableCellRenderer()
      centerRenderer.setHorizontalAlignment(0)
      table.peer.setDefaultRenderer(classOf[String], centerRenderer)
      contents = new BoxPanel(Orientation.Vertical) {
        contents += new BorderPanel {
          add(exited, BorderPanel.Position.Center)
        }
        contents += Swing.VStrut(10)
        contents += new ScrollPane(table) {
          preferredSize = siz(7).left.get
        }
        border = Swing.EmptyBorder(10, 10, 10, 10)
      }

    }
    //--------------------------------Etape 2---------------------------
    lazy val airportsDistance: Frame = new Frame {
      distanceFrame =>
      title = "Distance entre les aéroports"
      preferredSize = siz(0).left.get
      location = siz(3).right.get
      visible = false
      val exited = new Button(Action("Exit") {
        distanceFrame.visible = false
      })
      //ici on triche mais disons que ce n'est pas raisonable d'occuper autant de ressource et de memoir pour quelque chose
      //que l'on n'utilisera pas...
      val distancesArray: Array[Array[Any]] = Array.ofDim[Any](airports.length / 6 * (airports.length / 6 - 1) / 2, 3)
      var index: Int = 0
      for (i <- Range(0, airports.length / 6)) {
        for (j <- 0 until i) {
          distancesArray(index)(0) = airports(i)._2
          distancesArray(index)(1) = airports(j)._2
          distancesArray(index)(2) = distance.distanceHaversine(airports(i)._5, airports(j)._5, airports(i)._6, airports(j)._6).toString.asInstanceOf[Any]
          index += 1
        }
      }
      val table: Table = new Table(distancesArray, Seq("Aéroport 1", "Aéroport 2", "Distance en mètres")) {
        background = new Color(204, 204, 204)
        autoResizeMode = Table.AutoResizeMode.SubsequentColumns
      }

      val centerRenderer = new DefaultTableCellRenderer()
      centerRenderer.setHorizontalAlignment(0)
      table.peer.setDefaultRenderer(classOf[String], centerRenderer)
      contents = new BoxPanel(Orientation.Vertical) {
        contents += new BorderPanel {
          add(exited, BorderPanel.Position.Center)
        }
        contents += Swing.VStrut(10)
        contents += new ScrollPane(table) {
          preferredSize = siz(7).left.get
        }
        border = Swing.EmptyBorder(10, 10, 10, 10)
      }

    }
    //----------------------------------------------Etape4----------------------------------------------------------
    lazy val selectOnePoint: Frame = new Frame {
      selectOneFrame =>

      title = "Selection de points"
      preferredSize = siz(0).left.get
      location = siz(3).right.get
      visible = false
      val exited = new Button(Action("Done") {
        selectOnePoint.visible = false
      })

      var table: Table = new Table(airports2, Seq("ID", "Name", "City", "Country", "Latitude", "Longitude")) {
        background = new Color(204, 204, 204)
        autoResizeMode = Table.AutoResizeMode.SubsequentColumns
      }
      val searchField:TextField = new TextField { columns = 200 }
      val criteriaSearch:ComboBox[String]=new ComboBox[String](List("Par ID","Par Nom","Par Ville","Par Pays","Par Latitude et Longitude"))
      val centerRenderer = new DefaultTableCellRenderer()
      listenTo(criteriaSearch)
      listenTo(searchField)
      reactions+={
        case EditDone(`searchField`)=>
          saveCondition=searchField.text
          updateText()
        case SelectionChanged(`criteriaSearch`)=>
          saveTypeCondition=criteriaSearch.selection.item
          updateText()
      }
      def updateText(): Unit = {
        println(saveTypeCondition)
        saveTypeCondition match {
          case "Par ID"=>updateTable(partOfDB((a:Array[Any])=>a(0).toString,saveCondition.r))
        }
      }
      def updateTable(array: Array[Array[Any]]): Unit ={
        var table: Table = new Table(array, Seq("ID", "Name", "City", "Country", "Latitude", "Longitude")) {
          background = new Color(204, 204, 204)
          autoResizeMode = Table.AutoResizeMode.SubsequentColumns
        }
        step4.revalidate()
        step4.repaint()
      }
      def partOfDB(f:Array[Any]=>String,rx:Regex): Array[Array[Any] ]={

        val temp=airports2.filter(x=>rx.findFirstIn(f(x)).isDefined)
        println(temp.length,airports2.length)
        temp
      }
      centerRenderer.setHorizontalAlignment(0)
      table.peer.setDefaultRenderer(classOf[String], centerRenderer)
      val step4:BoxPanel = new BoxPanel(Orientation.Vertical) {
        contents += new BorderPanel {
          add(exited, BorderPanel.Position.Center)
        }
        contents += Swing.VStrut(10)
        contents += new BoxPanel(Orientation.Horizontal){
          contents+=searchField
          contents+=Swing.HStrut(20)
          contents+=criteriaSearch
        }
        contents += new ScrollPane(table) {
          preferredSize = siz(7).left.get
        }
        border = Swing.EmptyBorder(10, 10, 10, 10)
      }
      contents=step4
    }
    def SmartOneSelect(button:Button)={
      selectOnePoint.visible=true
    }
    def SmartConditionsSelect(button:Button)={

    }
    def excludeAndNotify(b:CheckBox)={

    }

    val image: com.top12.utils.ImagePanel = new com.top12.utils.ImagePanel {
      preferredSize = siz(6).left.get
      val filename: String = "Sources/Conformal/guyou.jpg"
      val extension: String = filename.split("\\.")(1)
      val path: String = "/Results/" + filename.split("\\.")(0).split("/").dropRight(1).drop(1).mkString("/") + "/"
      val nam: String = filename.split("\\.")(0).split("/").last
      val jarPath: String = URLDecoder.decode(getClass.getProtectionDomain.getCodeSource.getLocation.getPath, "UTF-8")
      val parent: File = new File(jarPath.substring(0, jarPath.lastIndexOf("/")) + path)
      parent.mkdirs()
      val file: File = new File(parent, nam + "_result." + extension)
      width = preferredSize.width
      height = preferredSize.height
      imagePath = file.getPath
    }
    contents = new BorderPanel {
      add(step1to4, BorderPanel.Position.West)
      add(image, BorderPanel.Position.Center)
      add(exitBtn, BorderPanel.Position.South)
      add(Button("Authors") {
        pressMe()
      }, BorderPanel.Position.North)
    }

    def pressMe() {
      Dialog.showMessage(contents.head, "UI by Erwan KESSLER, code by Victor COUR, Camille Coue and Erwan KESSLER", title = "Authors")
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
      case 4 => Left(new Dimension((w * 0.75 * 0.2 * 0.94).toInt, (h * 0.75 * 0.25 * 0.95 * 0.87).toInt))
      case 5 => Left(new Dimension((w * 0.75 * 0.2).toInt, (h * 0.75 * 0.94).toInt))
      case 6 => Left(new Dimension((w * 0.75 * 0.6).toInt, (h * 0.75 * 0.6 * 0.94).toInt))
      case 7 => Left(new Dimension((w * 0.75 * 0.95).toInt, (h * 0.75 * 80).toInt))
    }

  }


}
