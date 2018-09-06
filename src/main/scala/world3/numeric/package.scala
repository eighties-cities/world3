package world3

import org.apache.commons.math3.analysis.interpolation.LinearInterpolator

package object numeric {

  /*
  Intermediate values are calculated with linear interpolation between the intermediate points.
  Out-of-range values are the same as the closest endpoint (i.e, no extrapolation is performed).
   */
  def lookup(x:Double, xs:Vector[Double], ys:Vector[Double]) = interp(x, xs, ys)

  /*
   Intermediate values are calculated with linear interpolation between the intermediate points.
   Out-of-range values are calculated with linear extrapolation from the last two values at either end.
  */

  def lookup_extrapolation(x:Double, xs:Vector[Double], ys:Vector[Double]): Double = {
    val length = xs.length
    if (x < xs(0)) {
      val dx = xs(1) - xs(0)
      val dy = ys(1) - ys(0)
      val k = dy / dx
      ys(0) + (x - xs(0)) * k
    }
    if (x > xs(length - 1)) {
      val dx = xs(length - 1) - xs(length - 2)
      val dy = ys(length - 1) - ys(length - 2)
      val k = dy / dx
      ys(length - 1) + (x - xs(length - 1)) * k
    }
    return interp(x, xs, ys)
  }
  /*
  One-dimensional linear interpolation.

  Returns the one-dimensional piecewise linear interpolant to a function
  with given values at discrete data-points.

    Parameters
  ----------
  x : array_like
  The x-coordinates of the interpolated values.

    xp : 1-D sequence of floats
    The x-coordinates of the data points, must be increasing if argument
  `period` is not specified. Otherwise, `xp` is internally sorted after
  normalizing the periodic boundaries with ``xp = xp % period``.

  fp : 1-D sequence of float or complex
    The y-coordinates of the data points, same length as `xp`.
   */

  def interp(x:Double, xp:Vector[Double], fp:Vector[Double]): Double = {
    val psf = new LinearInterpolator().interpolate(xp.toArray, fp.toArray)
    psf.value(x)
  }

}
