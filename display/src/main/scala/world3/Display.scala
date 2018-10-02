package world3
import scaladget.api.{JSDependency, BootstrapTags => bs}
import scaladget.stylesheet.{all => sheet}
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom


object Display {

  @JSExportTopLevel("plot")
  def plot(): Unit = {
    org.scalajs.dom.document.body.appendChild(Plot.apply)
  }

}
