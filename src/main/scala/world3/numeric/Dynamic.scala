package world3.numeric

import org.apache.commons.math3.ode._
import org.apache.commons.math3.ode.nonstiff._

object Dynamic {
  def apply(equations: (Array[Double], Double) => Double*): Dynamic = new Dynamic(equations: _*)
}

class Dynamic(equations: (Array[Double], Double) => Double*) extends FirstOrderDifferentialEquations {

  def integrate(y0: Array[Double], integrationStep: Double, samples: Vector[Double]) = {
    val integrator = new ClassicalRungeKuttaIntegrator(integrationStep)

    samples.tail.foldLeft((samples.head, y0) :: Nil) {
      case (ys, s) => {
        val (curT, curY) = ys.head
        val y = Array.ofDim[Double](equations.size)
        integrator.integrate(this, curT, curY, s, y)
        if (y.exists(_.isNaN)) throw new RuntimeException(s"""Dynamic from ${curY.toVector} at step ${s} using integration step ${integrationStep} produces NaN: ${y.toVector}"""")
        (s, y) :: ys
      }
    }.reverse.map(_._2).toVector

  }

  val equationsArray = equations.toArray

  override def computeDerivatives(t: Double, y: Array[Double], yDot: Array[Double]): Unit = {
    var i = 0
    while(i < equationsArray.size) {
      yDot(i) = equationsArray(i)(y, t)
      i += 1
    }
  }

  override def getDimension: Int = equationsArray.size

}
