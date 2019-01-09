package com.top12.func

import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.{Color, Toolkit}
import java.io.File
import java.net.URLDecoder
import java.text.DecimalFormat

import com.top12.utils.ImagePanel
import javax.swing.table.DefaultTableCellRenderer

import scala.swing._
import scala.swing.event._
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
  var airportsTemp: Array[(Int, String, String, String, Double, Double)] = airports
  var airports2: Array[Array[Any]] = airportsTemp.map(el => el.productIterator.map(el => el.toString.asInstanceOf[Any]).toArray)
  var distances: Array[Double] = distance.distancesArray(airports)
  val dmn: String = stats.distanceMin(distances).toString
  val dmx: String = stats.distanceMax(distances).toString
  val dmy: String = stats.distanceMoyenne(distances).toString
  val dmd: String = stats.distanceMediane2(distances).toString
  val ect: String = stats.ecartType(distances).toString
  var (dmnTemp, dmxTemp, dmyTemp, dmdTemp, ectTemp) = (dmn, dmx, dmy, dmd, ect)
  val countries: Array[String] = util.showAllCountries(airports)
  var currentButton: (Button, Int) = _
  var currentButton1: Int = 1
  var currentButton2: Int = 2
  var currentButton3: String = "Par Pays"
  var currentCheckBox: CheckBox = _
  var saveCondition: String = ""
  var saveTypeCondition: String = "Par ID"
  var tempArray: Array[Array[Any]] = airports2
  var selectedCountries: Set[String] = Set()
  var rowSelection: Int = _
  var fileName: String = "Sources/Conformal/guyou.jpg"
  var image: ImagePanel = setImage(fileName)
  var densiteArray: Array[Array[Any]] = _
  var Lontemp: String = _
  var Lattemp: String = _
  var pointr: String = _
  var rtemp: String = _

  var division: Int = 6

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
        preferredSize = siz(9).left.get
        border = Swing.TitledBorder(Swing.EtchedBorder(Swing.Lowered), "Etape 1: Charger les données")
        contents += new Button(Action("Afficher les données") {
          airportsList.visible = true
        })
      }
      var step2: BoxPanel = new BoxPanel(Orientation.Vertical) {
        preferredSize = siz(4).left.get
        val distanceA1: TextArea = new TextArea {
          rows = 1
          lineWrap = true
          wordWrap = true
          editable = false
          text = "Aéroport 1: " + airportsTemp(0)._2
          caret.position = 0
        }
        val distanceA2: TextArea = new TextArea {
          rows = 1
          lineWrap = true
          wordWrap = true
          editable = false
          text = "Aéroport 2: " + (if (airportsTemp.length > 1) airportsTemp(1)._2 else airportsTemp(0)._2)
          caret.position = 0
        }
        val distanceA1A2: TextArea = new TextArea {
          rows = 1
          lineWrap = true
          wordWrap = true
          editable = false
          text = "Distance (en km) entre aéroport 1 et aéroport 2: " +
            distance.distanceHaversine(airportsTemp(0)._5, if (airportsTemp.length > 1) airportsTemp(1)._5 else airportsTemp(0)._5, airportsTemp(0)._6, if (airportsTemp.length > 1) airportsTemp(1)._6 else airportsTemp(0)._6).toString
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
          text = "Minimum : " + dmnTemp + " km."
        }
        val distanceMax: Label = new Label {
          text = "Maximum : " + dmxTemp + " km."
        }
        val distanceMoyenne: Label = new Label {
          text = "Moyenne : " + dmyTemp + " km."
        }
        val distanceMediane: Label = new Label {
          text = "Médiane : " + dmdTemp + " km."
        }
        val ecartype: Label = new Label {
          text = "Ecart type : " + ectTemp + " km."
        }
        contents += distanceMin
        contents += distanceMax
        contents += distanceMoyenne
        contents += distanceMediane
        contents += ecartype
      }
      val step4: BoxPanel = new BoxPanel(Orientation.Vertical) {
        preferredSize = siz(10).left.get
        border = Swing.TitledBorder(Swing.EtchedBorder(Swing.Lowered), "Etape 4: Selection")
        val FirstPointButton = new Button(airports(0)._2)
        val SecondPointButton = new Button(airports(1)._2)
        val ConditionsButton = new Button("Par Pays")
        val FirstCheckBox = new CheckBox()
        val SecondCheckBox = new CheckBox()
        val ThirdCheckBox = new CheckBox()
        val FourthCheckBox = new CheckBox()
        val FifthCheckBox = new CheckBox()
        val SixthCheckBox = new CheckBox()
        val latRest: TextField = new TextField {
          columns = 20
        }
        val lonRest: TextField = new TextField {
          columns = 20
        }
        val pointRest: TextField = new TextField {
          columns = 20
        }
        val rayonRest: TextField = new TextField {
          columns = 20
        }

        listenTo(FirstPointButton)
        listenTo(SecondPointButton)
        listenTo(ConditionsButton)
        listenTo(FirstCheckBox)
        listenTo(SecondCheckBox)
        listenTo(ThirdCheckBox)
        listenTo(FourthCheckBox)
        listenTo(FifthCheckBox)
        listenTo(SixthCheckBox)
        listenTo(latRest)
        listenTo(lonRest)
        listenTo(pointRest)
        listenTo(rayonRest)
        reactions += {
          case ButtonClicked(FirstPointButton) => SmartOneSelect(FirstPointButton, 0)
          case ButtonClicked(SecondPointButton) => SmartOneSelect(SecondPointButton, 1)
          case ButtonClicked(ConditionsButton) => SmartConditionsSelect(ConditionsButton)
          case ButtonClicked(FirstCheckBox) => excludeAndNotify(FirstCheckBox, 0)
          case ButtonClicked(SecondCheckBox) => excludeAndNotify(SecondCheckBox, 1)
          case ButtonClicked(ThirdCheckBox) => excludeAndNotify(ThirdCheckBox, 2)
          case ButtonClicked(FourthCheckBox) => excludeAndNotify(FourthCheckBox, 3)
          case ButtonClicked(FifthCheckBox) => excludeAndNotify(FifthCheckBox, 4)
          case ButtonClicked(SixthCheckBox) => excludeAndNotify(SixthCheckBox, 5)
          case EditDone(`lonRest`) => Lontemp = lonRest.text
          case EditDone(`latRest`) => Lattemp = latRest.text
          case EditDone(`pointRest`) => pointr = pointRest.text
          case EditDone(`rayonRest`) => rtemp = rayonRest.text

        }
        FourthCheckBox.selected = true
        currentCheckBox = FourthCheckBox
        contents += new BoxPanel(Orientation.Horizontal) {
          contents += new Label {
            text = "Un Point"
          }
          contents += Swing.HStrut(10)
          contents += FirstPointButton
          contents += Swing.HStrut(10)
          contents += FirstCheckBox
        }
        contents += new BoxPanel(Orientation.Horizontal) {
          contents += new Label {
            text = "Deux Points"
          }
          contents += Swing.HStrut(10)
          contents += SecondPointButton
          contents += Swing.HStrut(10)
          contents += SecondCheckBox
        }
        contents += new BoxPanel(Orientation.Horizontal) {
          contents += new Label {
            text = "Multiple Points"
          }
          contents += Swing.HStrut(10)
          contents += ConditionsButton
          contents += Swing.HStrut(10)
          contents += ThirdCheckBox
        }
        contents += new BoxPanel(Orientation.Horizontal) {
          contents += new Label {
            text = "Tout les Points"
          }
          contents += Swing.HStrut(20)
          contents += FourthCheckBox
        }
        contents += new BoxPanel(Orientation.Horizontal) {
          contents += new BoxPanel(Orientation.Vertical) {
            contents += new Label {
              text = "Par Point1 (lat,lon)"
            }
            contents += latRest
          }
          contents += Swing.HStrut(5)
          contents += new BoxPanel(Orientation.Vertical) {
            contents += new Label {
              text = "Par Point2 (lat,lon)"
            }
            contents += lonRest
          }
          contents += Swing.HStrut(5)
          contents += FifthCheckBox
        }
        contents += new BoxPanel(Orientation.Horizontal) {
          contents += new BoxPanel(Orientation.Vertical) {
            contents += new Label {
              text = "Par Point"
            }
            contents += pointRest
          }
          contents += Swing.HStrut(5)
          contents += new BoxPanel(Orientation.Vertical) {
            contents += new Label {
              text = "+ rayon"
            }
            contents += rayonRest
          }
          contents += Swing.HStrut(5)
          contents += SixthCheckBox
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
      val distancesArray: Array[Array[Any]] = Array.ofDim[Any](airportsTemp.length / division * (airportsTemp.length / division - 1) / 2, 3)
      var index: Int = 0
      for (i <- Range(0, airportsTemp.length / division)) {
        for (j <- 0 until i) {
          distancesArray(index)(0) = airportsTemp(i)._2
          distancesArray(index)(1) = airportsTemp(j)._2
          distancesArray(index)(2) = distance.distanceHaversine(airportsTemp(i)._5, airportsTemp(j)._5, airportsTemp(i)._6, airportsTemp(j)._6).toString.asInstanceOf[Any]
          index += 1
        }
      }
      val table: Table = new Table(distancesArray, Seq("Aéroport 1", "Aéroport 2", "Distance en kilomètres")) {
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
        currentButton._2 match {
          case 0 => currentButton._1.text = tempArray(rowSelection)(1).toString
            currentButton1 = tempArray(rowSelection)(0).toString.toInt
          case 1 => currentButton._1.text = tempArray(rowSelection)(1).toString
            currentButton2 = tempArray(rowSelection)(0).toString.toInt
        }
      })
      val searchField: TextField = new TextField {
        columns = 200
      }
      val criteriaSearch: ComboBox[String] = new ComboBox[String](List("Par ID", "Par Nom", "Par Ville", "Par Pays", "Par Latitude et Longitude"))

      var table: Table = new Table(airports2, Seq("ID", "Name", "City", "Country", "Latitude", "Longitude")) {
        background = new Color(204, 204, 204)
        autoResizeMode = Table.AutoResizeMode.SubsequentColumns
      }
      listenTo(table.selection)
      listenTo(criteriaSearch.selection)
      listenTo(searchField)
      reactions += {
        case EditDone(`searchField`) =>
          saveCondition = searchField.text
          updateText()
          step4.requestFocus()
        case SelectionChanged(`criteriaSearch`) =>
          saveTypeCondition = criteriaSearch.selection.item
          updateText()
        case TableRowsSelected(_, r, _) =>
          rowSelection = table.selection.rows.leadIndex

      }

      def updateText(): Unit = {
        saveTypeCondition match {
          case "Par ID" => updateTable(partOfDB((a: Array[Any]) => a(0).toString, saveCondition.r))
          case "Par Nom" => updateTable(partOfDB((a: Array[Any]) => a(1).toString, saveCondition.r))
          case "Par Ville" => updateTable(partOfDB((a: Array[Any]) => a(2).toString, saveCondition.r))
          case "Par Pays" => updateTable(partOfDB((a: Array[Any]) => a(3).toString, saveCondition.r))
          case "Par Latitude et Longitude" => updateTable(partOfDB2((a: Array[Any]) => (a(4).toString.toDouble, a(5).toString.toDouble), saveCondition))

        }
      }

      def updateTable(array: Array[Array[Any]]): Unit = {
        table = new Table(array, Seq("ID", "Name", "City", "Country", "Latitude", "Longitude")) {
          background = new Color(204, 204, 204)
          autoResizeMode = Table.AutoResizeMode.SubsequentColumns
        }
        tempArray = array
        listenTo(table.selection)
        table.peer.setDefaultRenderer(classOf[String], centerRenderer)
        step4.contents -= tableScrollable

        tableScrollable = new ScrollPane(table) {
          preferredSize = siz(7).left.get
        }
        step4.contents += tableScrollable
        step4.revalidate()
        step4.repaint()
      }

      def partOfDB(f: Array[Any] => String, rx: Regex): Array[Array[Any]] = {
        airports2.filter(x => rx.findFirstIn(f(x)).isDefined)
      }

      def partOfDB2(f: Array[Any] => (Double, Double), rx: String): Array[Array[Any]] = {
        try {
          val r = rx.split(",")
          val (lmn, lmx, lamn, lamx) = (r(0).toDouble, r(1).toDouble, r(2).toDouble, r(3).toDouble)
          airports2.filter(x => f(x)._1 >= lamn && f(x)._1 <= lamx && f(x)._2 >= lmn && f(x)._2 <= lmx)
        }
        catch {
          case _: Throwable => println("error the format is latmin,latmax,lonmin,lonmax")
            airports2
        }

      }

      val centerRenderer = new DefaultTableCellRenderer()
      centerRenderer.setHorizontalAlignment(0)
      table.peer.setDefaultRenderer(classOf[String], centerRenderer)
      var tableScrollable: ScrollPane = new ScrollPane(table) {
        preferredSize = siz(7).left.get
      }
      val step4: BoxPanel = new BoxPanel(Orientation.Vertical) {
        border = Swing.EmptyBorder(10, 10, 10, 10)
        contents += new BorderPanel {
          add(exited, BorderPanel.Position.Center)
        }
        contents += Swing.VStrut(10)
        contents += new BoxPanel(Orientation.Horizontal) {
          contents += searchField
          contents += Swing.HStrut(20)
          contents += criteriaSearch
        }
        contents += tableScrollable
      }
      contents = step4
    }

    def SmartOneSelect(button: Button, i: Int): Unit = {
      selectOnePoint.visible = true
      currentButton = (button, i)
    }

    lazy val selectMultiplePoint: Frame = new Frame {
      selectMultipleFrame =>

      title = "Selection de points"
      preferredSize = siz(0).left.get
      location = siz(3).right.get
      visible = false


      var table: TextArea = new TextArea {
        background = new Color(204, 204, 204)
        rows = 10
        lineWrap = true
        wordWrap = true
        editable = false
        text = countries.mkString("\n")
      }

      var tableScrollable: ScrollPane = new ScrollPane(table) {
        preferredSize = siz(8).left.get
      }
      var tablebis: TextArea = new TextArea {
        background = new Color(204, 204, 204)
        rows = 10
        lineWrap = true
        wordWrap = true
        editable = true
        text = selectedCountries.mkString("\n")
      }
      var tableScrollable2: ScrollPane = new ScrollPane(tablebis) {
        preferredSize = siz(8).left.get
      }
      val exited = new Button(Action("Done") {
        selectMultiplePoint.visible = false
        selectedCountries = tablebis.text.split("\n").toSet
      })
      val countriesPlus: Button = new Button("+")
      val countriesMoins: Button = new Button("-")
      val step4bis: BoxPanel = new BoxPanel(Orientation.Vertical) {
        contents += new BorderPanel {
          add(exited, BorderPanel.Position.Center)
        }
        contents += new BoxPanel(Orientation.Horizontal) {

          border = Swing.EmptyBorder(10, 10, 10, 10)
          contents += tableScrollable
          contents += tableScrollable2
          contents += new BoxPanel(Orientation.Vertical) {
            contents += countriesPlus
            contents += countriesMoins
          }
        }
      }
      table.requestFocus()
      contents = step4bis
    }

    def SmartConditionsSelect(button: Button): Unit = {
      selectMultiplePoint.visible = true
    }

    def excludeAndNotify(b: CheckBox, i: Int): Unit = {
      currentCheckBox.selected = false
      currentCheckBox = b
      i match {
        case 0 =>
          var line = util.airportsIdToNumbers(airports)(currentButton1)
          airportsTemp = Array(airports(line))
          airports2 = airportsTemp.map(el => el.productIterator.map(el => el.toString.asInstanceOf[Any]).toArray)
          division = 1
          distances= distance.distancesArray(airportsTemp)
          dmnTemp = stats.distanceMin(distances).toString
          dmxTemp = stats.distanceMax(distances).toString
          dmyTemp = stats.distanceMoyenne(distances).toString
          dmdTemp = stats.distanceMediane2(distances).toString
          ectTemp = stats.ecartType(distances).toString
        case 1 =>
          var line = util.airportsIdToNumbers(airports)(currentButton1)
          var line2 = util.airportsIdToNumbers(airports)(currentButton2)
          airportsTemp = Array(airports(line), airports(line2))
          airports2 = airportsTemp.map(el => el.productIterator.map(el => el.toString.asInstanceOf[Any]).toArray)
          distances= distance.distancesArray(airportsTemp)
          division = 1
          dmnTemp = stats.distanceMin(distances).toString
          dmxTemp = stats.distanceMax(distances).toString
          dmyTemp = stats.distanceMoyenne(distances).toString
          dmdTemp = stats.distanceMediane2(distances).toString
          ectTemp = stats.ecartType(distances).toString
        case 3 =>
          airportsTemp = airports.clone()
          airports2 = airportsTemp.map(el => el.productIterator.map(el => el.toString.asInstanceOf[Any]).toArray)
          distances= distance.distancesArray(airportsTemp)
          dmnTemp = dmn
          dmxTemp = dmx
          dmyTemp = dmy
          dmdTemp = dmd
          ectTemp = ect
          division = 6
        case 2 =>
          airportsTemp = restriction.byCountry(airports, selectedCountries)
          airports2 = airportsTemp.map(el => el.productIterator.map(el => el.toString.asInstanceOf[Any]).toArray)
          division = 1
           distances = distance.distancesArray(airports)
          dmnTemp = stats.distanceMin(distances).toString
          dmxTemp = stats.distanceMax(distances).toString
          dmyTemp = stats.distanceMoyenne(distances).toString
          dmdTemp = stats.distanceMediane2(distances).toString
          ectTemp = stats.ecartType(distances).toString
        case 4 =>
          try {
            val lon = Lontemp.split(",")
            val lat = Lattemp.split(",")
            airportsTemp = restriction.byArea(airports, (lon(0).toDouble, lon(1).toDouble), (lat(0).toDouble, lat(1).toDouble))
            airports2 = airportsTemp.map(el => el.productIterator.map(el => el.toString.asInstanceOf[Any]).toArray)
            division = 1
            distances= distance.distancesArray(airports)
            dmnTemp = stats.distanceMin(distances).toString
            dmxTemp = stats.distanceMax(distances).toString
            dmyTemp = stats.distanceMoyenne(distances).toString
            dmdTemp = stats.distanceMediane2(distances).toString
            ectTemp = stats.ecartType(distances).toString
          }
          catch {
            case _: Throwable => println("error the format is lat1,lon1 et lat2,lon2")
          }
        case 5 =>
          try {
            val point = pointr.split(",")
            airportsTemp = restriction.byRadius(airports, (point(0).toDouble, point(1).toDouble), rtemp.toDouble)
            airports2 = airportsTemp.map(el => el.productIterator.map(el => el.toString.asInstanceOf[Any]).toArray)
            division = 1
            distances= distance.distancesArray(airports)
            dmnTemp = stats.distanceMin(distances).toString
            dmxTemp = stats.distanceMax(distances).toString
            dmyTemp = stats.distanceMoyenne(distances).toString
            dmdTemp = stats.distanceMediane2(distances).toString
            ectTemp = stats.ecartType(distances).toString
          }
          catch {
            case _: Throwable => println("error the format is lat1,lon1 et R")
          }

      }
      globalWindows.revalidate()
      globalWindows.repaint()
    }

    //-----------------------------------------------------------------------------------------------------------
    val step5to8: FlowPanel = new FlowPanel {
      preferredSize = siz(5).left.get
      border = Swing.TitledBorder(Swing.EtchedBorder(Swing.Lowered), "Etape 5 à 8")
      lazy val densite: Frame = new Frame {
        densiteFrame =>
        title = "Densite par rapport a une metrique"
        preferredSize = siz(0).left.get
        location = siz(3).right.get
        visible = false
        val exited = new Button(Action("Exit") {
          densiteFrame.visible = false
        })

        val table: Table = new Table(densiteArray, Seq("Pays", "Densite")) {
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
      val step5: BoxPanel = new BoxPanel(Orientation.Vertical) {
        preferredSize = siz(4).left.get
        border = Swing.TitledBorder(Swing.EtchedBorder(Swing.Lowered), "Etape 5: Densité")
        val typeDensite = new ComboBox(List("surfaces", "populations"))
        contents += new Button(Action("Afficher les données") {
          val formatter = new DecimalFormat("#.###################")
          densiteArray = density.Densite(airportsTemp, typeDensite.selection.item + ".csv").map(el => Array(el._1.asInstanceOf[Any], formatter.format(el._2.toDouble))).toArray
          densite.visible = true
        })
        contents += typeDensite
      }
      val step6: BoxPanel = new BoxPanel(Orientation.Vertical) {
        preferredSize = siz(9).left.get

        border = Swing.TitledBorder(Swing.EtchedBorder(Swing.Lowered), "Etape 6: projection equidistante")
        val button6 = new Button(Action("Afficher") {

          fileName = "Sources/Equidistant/equirectangular.png"
          equidistant.modifyImage(fileName, airportsTemp)
          imagePart.contents -= image
          image = setImage(fileName)
          imagePart.contents += image
          imagePart.revalidate()
          imagePart.repaint()
          globalWindows.repaint()
        })
        contents += button6

      }
      val step7: BoxPanel = new BoxPanel(Orientation.Vertical) {
        preferredSize = siz(4).left.get
        border = Swing.TitledBorder(Swing.EtchedBorder(Swing.Lowered), "Etape 7: Projection Conforme ou équivalente")
        contents += new BoxPanel(Orientation.Horizontal) {
          val equivalentList = new ComboBox(List("lambertCylindric", "behrmann", "eckert1",
            "eckert2", "eckert3", "eckert4", "eckert5", "eckert6", "gallPeters",
            "hoboDyer", "mollweide", "sinusoidal", "balthasart",
            "toblersWIS", "equalEarth"))
          contents += equivalentList
          contents += new Button(Action("Afficher \n Equivalente") {
            fileName = "Sources/EqualArea/" + equivalentList.selection.item + ".jpg"
            val exceptions = Set("eckert1", "eckert3", "eckert5", "balthasart", "toblersWIS")

            equalArea.whichProjection("all", equivalentList.selection.item + ".jpg", if (exceptions(equivalentList.selection.item)) "dot" else "circle", util.RGBtoHexa(255, 0, 0), Left(airportsTemp))
            imagePart.contents -= image
            image = setImage(fileName)
            imagePart.contents += image
            imagePart.revalidate()
            imagePart.repaint()
            globalWindows.repaint()
          })
        }
        contents += new BoxPanel(Orientation.Horizontal) {
          val conformList = new ComboBox(List("mercator", "lambertConic", "mercatorTransverse",
            "stereographic", "peirceQuincuncial", "guyou", "adamshemisphere1", "adamshemisphere2",
            "adamsWIS1", "adamsWIS2"))
          contents += conformList
          contents += new Button(Action("Afficher \n Conforme") {
            fileName = "Sources/Conformal/" + conformList.selection.item + ".jpg"
            val exception = Set("adamshemisphere2", "adamsWIS1", "adamsWIS2")
            conformal.whichProjection("all", conformList.selection.item + ".jpg", if (exception(conformList.selection.item)) "dot" else "circle", util.RGBtoHexa(255, 0, 0), Left(airportsTemp))
            imagePart.contents -= image
            image = setImage(fileName)
            imagePart.contents += image
            imagePart.revalidate()
            imagePart.repaint()
            globalWindows.repaint()
          })
        }
      }
      contents += step5
      contents += step6
      contents += step7
    }
    //----------------------------------------------Etape5----------------------------------------------------------


    val imagePart: BoxPanel = new BoxPanel(Orientation.Horizontal) {
      contents += image

    }
    var globalWindows: BorderPanel = new BorderPanel {
      add(step1to4, BorderPanel.Position.West)
      add(imagePart, BorderPanel.Position.Center)
      add(exitBtn, BorderPanel.Position.South)
      add(Button("Authors") {
        pressMe()
      }, BorderPanel.Position.North)
      add(step5to8, BorderPanel.Position.East)
    }
    contents = globalWindows

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
      case 6 => Left(new Dimension((w * 0.75 * 0.6 * 0.80).toInt, (h * 0.75 * 0.6 * 0.80).toInt))
      case 7 => Left(new Dimension((w * 0.75 * 0.95).toInt, (h * 0.75 * 80).toInt))
      case 8 => Left(new Dimension((w * 0.75 * 0.95 * 0.2).toInt, (h * 0.75 * 30).toInt))
      case 9 => Left(new Dimension((w * 0.75 * 0.2 * 0.94).toInt, (h * 0.75 * 0.10 * 0.95 * 0.87).toInt))
      case 10 => Left(new Dimension((w * 0.75 * 0.2 * 0.94).toInt, (h * 0.75 * 0.40 * 0.95 * 0.87).toInt))
    }

  }

  def setImage(pat: String): ImagePanel = {
    var image: com.top12.utils.ImagePanel = new com.top12.utils.ImagePanel {

      preferredSize = siz(6).left.get
      var filename: String = pat
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
    image
  }


}

