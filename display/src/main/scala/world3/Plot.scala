package world3

import com.definitelyscala.plotlyjs._
import com.definitelyscala.plotlyjs.all._
import com.definitelyscala.plotlyjs.PlotlyImplicits._
import org.scalajs.dom.raw.Element

import scala.scalajs.js.JSConverters._
import scalatags.JsDom.all._

import scala.scalajs._

object Plot {

  def apply = {

    val series = Data.tenPercent.map {
      _.stripMargin.split("\t").map {
        _.toDouble
      }
    }.transpose

    val averageSeries = series.map {
      _.sum / 100
    }.toJSArray
    val populations = (0 to 200).toJSArray.map {
      _.toDouble
    }

    val plotDiv = div.render

    val config = Config.displayModeBar(false)

    val layout = Layout
      .title("Population evolution")
      .xaxis(plotlyaxis.title("Time"))
      .yaxis(plotlyaxis.title("Population"))
      .width(1600)

    val data = PlotData
      .set(plotlymode.markers.lines)
      .set(plotlymarker.set(plotlysymbol.square))

    val data1 = data
      .x(populations)
      .y(averageSeries)
      .set(plotlymarker.size(12.0).set(plotlycolor.rgb(180, 0, 0)))
      .name("Reds")

    def dataBoxes = series.zipWithIndex.map { case (s, index) =>
      PlotData.set(plotlytype.box)
        .y(s.toJSArray)
        .set(plotlymarker.set(plotlycolor.rgba(180, 180, 0, 0.5)).set(plotlysizemode.area))
        .name(index.toString)
    }.toJSArray

    val fullData = (dataBoxes :+ data1).map {
        _._result
      }

    Plotly.newPlot(plotDiv,
      fullData,
      layout,
      config)

    plotDiv.render
  }
}