package world3

import scala.collection.mutable.ListBuffer

object Box {

  object Equation {
    implicit def intToEquation(major: Int) = Equation(major, None)
    implicit def tupleToEqation(t: (Int, Int)) = Equation(t._1, Some(t._2))
  }
  case class Equation(major: Int, minor: Option[Int])

  sealed trait All {
    def j: Option[Double]
    def k: Option[Double]
    def k_=(v: Option[Double])
    def qName: String
    def reset()
    def warmup(dt: Double)
    def tick()
    def update(dt: Double)
    val dependencies: Vector[String]
  }

  object Level {
    def apply(qName: String, qNumber: Equation, initVal: Double, updateFn: () => Option[Double], units: String = "dimensionless", dependencies: Vector[String] = Vector()) =
      new Level(qName, qNumber, initVal, updateFn, units, dependencies)
  }

  class Level(
    val qName: String,
    val qNumber: Equation,
    val initVal: Double,
    val updateFn: () => Option[Double],
    val units: String,
    val dependencies: Vector[String]) extends All {

    val qType = "Level"
    var j: Option[Double] = Some(initVal)
    var k: Option[Double] = Some(initVal)

    def reset() = {
      k = Some(initVal)
      j = Some(initVal)
    }

    def warmup(dt: Double) = { k = updateFn() }

    def update(dt: Double) = {
      k = updateFn()
      k.get
    }

    def tick() = { j = k }
  }

  object Rate {

    def apply(
               qName: String,
               qNumber: Int,
               updateFn: () => Option[Double],
               units: String = "dimensionless",
               dependencies: Vector[String] = Vector()) =
      new Rate(qName, qNumber, units, updateFn, dependencies)

  }

  class Rate(
              val qName: String,
              val qNumber: Int,
              val units: String,
              val updateFn: () => Option[Double],
              val dependencies: Vector[String]) extends All {

    val qType = "Rate"
    var j: Option[Double] = None
    var k: Option[Double] = None

    def reset() = {
      j = None
      k = None
    }

    def warmup(dt: Double) = { k = updateFn() }

    def update(dt: Double) = {
      k = updateFn()
      k.get
    }

    def tick() = { j = k }
  }

  object Aux {
    def apply(qName: String, qNumber: Equation, updateFn: () => Option[Double], units: String = "dimensionless", dependencies: Vector[String] = Vector()) =
      new Aux(qName, qNumber, updateFn, units, dependencies)
  }

  class Aux(
             val qName: String,
             val qNumber: Equation,
             val updateFn: () => Option[Double],
             val units: String,
             val dependencies: Vector[String]) extends All {
    val qType = "Aux"
    var j: Option[Double] = None
    var k: Option[Double] = None

    def reset() = {
      j = None
      k = None
    }

    def warmup(dt: Double) = { k = updateFn() }

    def update(dt: Double) = {
      k = updateFn()
      k.get
    }

    def tick() = { j = k }
  }

  object Smooth {
    def apply(
               qName: String,
               qNumber: Equation,
               delay: Double,
               initFn: () => All,
               initVal: Option[Double] = None,
               units: String = "dimensionless",
               dependencies: Vector[String] = Vector()) =
      new Smooth(qName, qNumber, initFn, initVal, units, delay, dependencies)

  }

  class Smooth(
                val qName: String,
                val qNumber: Equation,
                val initFn: () => All,
                val initVal: Option[Double],
                val units: String,
                val delay: Double,
                val dependencies: Vector[String]) extends All {
    val qType = "Smooth"
    var j: Option[Double] = None
    var k: Option[Double] = None
    var firstCall = true

    def init()  = {
      j = initFn().k orElse initVal
      k = j
    }

    def reset() = {
      firstCall = true
      j = None
      k = None
    }

    def update(dt: Double) = {
      val theInput = initFn()
      if (firstCall) {
        j = Some(theInput.k.orElse(initVal).get)
        k = Some(theInput.k.orElse(initVal).get)
        firstCall = false
        k.get
      } else {
        k = Some(j.get + dt * (theInput.j.get - j.get) / delay)
        k.get
      }
    }

    def warmup(dt: Double): Unit = init()
    def tick() = { j = k }
  }

  //  // constructor for Delay3 objects
  //  // third-order exponential delay for Rate variables
  object Delay3 {
    case class JK(j: Option[Double], k: Option[Double])

    def apply(qName: String, qNumber: Equation, initFn: () => All, delay: Double, units: String = "dimensionless", dependencies: Vector[String] = Vector()) =
      new Delay3(qName, qNumber, initFn, units, delay, dependencies)
  }

  class Delay3(
                val qName: String,
                val qNumber: Equation,
                val initFn: () => All,
                val units: String,
                val delay: Double,
                val dependencies: Vector[String]) extends All {
    val qType = "Delay3"
    var j: Option[Double] = None
    var k: Option[Double] = None
    var firstCall = true

    val delayPerStage = delay / 3

    var alpha: Option[Delay3.JK] = None
    var beta: Option[Delay3.JK] = None
    var gama: Option[Delay3.JK] = None

    def init() = {
      val theInput = initFn()
      j = theInput.k
      k = theInput.k
      alpha = Some(Delay3.JK(theInput.j, theInput.j))
      beta = Some(Delay3.JK(theInput.j, theInput.j))
      gama = Some(Delay3.JK(theInput.j, theInput.j))
    }

    def reset() = {
      firstCall = true
      j = None
      k = None
      alpha = None
      beta = None
      gama = None
    }

    def update(dt: Double) = {
      val theInput = initFn()
      if(firstCall) {
        j = theInput.k
        k = theInput.k
        alpha = Some(Delay3.JK(j = theInput.k, k = theInput.k))
        beta = Some(Delay3.JK(j = theInput.k, k = theInput.k))
        gama = Some(Delay3.JK(j = theInput.k, k = theInput.k))
        firstCall = false
        k.get
      } else {
        val alphaK = alpha.get.j.get + dt * (theInput.j.get - alpha.get.j.get) / delayPerStage
        val betaK = beta.get.j.get + dt * (alpha.get.j.get - beta.get.j.get) / delayPerStage
        val gamaK = gama.get.j.get + dt * (beta.get.j.get - gama.get.j.get) / delayPerStage

        alpha = Some(Delay3.JK(j = Some(alphaK), k = Some(alphaK)))
        beta = Some(Delay3.JK(j = Some(betaK), k = Some(betaK)))
        gama = Some(Delay3.JK(j = Some(gamaK), k = Some(gamaK)))

        k = Some(gamaK)
        k.get
      }
    }

    def warmup(dt: Double) = init()
    def tick() = { j = k }
  }

  object Table {

    def apply(
               qName: String,
               qNumber: Equation,
               data: Vector[Double],
               iMin: Double,
               iMax: Double,
               iDelta: Double,
               updateFn: () => Option[Double],
               units: String = "dimensionless",
               dependencies: Vector[String] = Vector()) =
      new Table(qName, qNumber, data, iMin, iMax, iDelta, updateFn, units, dependencies)


  }

  class Table(
               val qName: String,
               val qNumber: Equation,
               val data: Vector[Double],
               val iMin: Double,
               val iMax: Double,
               val iDelta: Double,
               val updateFn: () => Option[Double],
               val units: String,
               val dependencies: Vector[String]) extends All {

    var j: Option[Double] = None
    var k: Option[Double] = None

    def interpolate(lower: Int, upper: Int, fraction: Double) = {
      val lowerVal = data(lower)
      val upperVal = data(upper)
      lowerVal + (fraction * (upperVal - lowerVal))
    }

    def lookup(v: Double): Double = {
      if(v <= iMin) data(0)
      else if(v >= iMax) data(data.length - 1)
      else {
        for ((i, j) <- (BigDecimal(iMin) to iMax by iDelta).zipWithIndex) {
          if(i >= v) {
            //            if (qName == "mortality0To14") println(s"$qName lookup ($v) > $j = ${interpolate(j - 1, j, (v - (i - iDelta)) / iDelta)}")
            return interpolate(j - 1, j, (v - (i.toDouble - iDelta)) / iDelta)
          }
        }
        ???
      }
    }

    def reset() = {}

    def update(dt: Double) = {
      k = updateFn().map(lookup)
    }

    def warmup(dt: Double) = update(dt)

    def tick() = { j = k }
  }




}
