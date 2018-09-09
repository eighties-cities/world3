package world3.numeric

import scala.collection.mutable.ListBuffer
import io.monadless.stdlib.MonadlessOption._
import better.files._

object World3 extends App {

  def scale(x: Double, xMin: Double = 0.0, xMax: Double): Double = (x - xMin) / (xMax - xMin)

  def csv(result: Vector[StepValues]):String = {
    def header = "step,population,nonrenewableResourceFractionRemaining,serviceOutput," +
      "foodPerCapita,industrialOutputPerCapita,indexOfPersistentPollution,lifeExpectancy," +
      "crudeBirthRate,crudeDeathRate,arableLand,landYield"
    Vector(header).++(result.map {
      r => s"${r.step}," +
        s"${scale(r.population,xMax=1.6e10)}," +
        s"${scale(r.nonrenewableResourceFractionRemaining,xMax=1.0)}," +
        s"${scale(r.foodPerCapita,xMax=1000)}," +
        s"${scale(r.industrialOutputPerCapita,xMax=500)}," +
        s"${scale(r.serviceOutput,xMax=1.0e13)}," +
        s"${scale(r.indexOfPersistentPollution,xMax=32)}," +
        s"${scale(r.lifeExpectancy,xMax=80)}," +
        s"${scale(r.crudeBirthRate,xMax=50)}," +
        s"${scale(r.crudeDeathRate,xMax=50)}," +
        s"${scale(r.arableLand,xMax=3.0e9)}," +
        s"${scale(r.landYield,xMax=3000)}"
    }).mkString("\n")
  }

  def check(result: Vector[StepValues]): Unit = {
    assert(File("/tmp/results.csv").contentAsString == csv(result))
  }

  val w3 = new World3()
  val result = w3.fastRun()

  val writer = File("/tmp/results.csv").newFileWriter()
  writer.write(csv(result))
  writer.close()
//  check(result)

}

case class StepValues(
  step: Double,
  population: Double,
  nonrenewableResourceFractionRemaining: Double,
  foodPerCapita: Double,
  industrialOutputPerCapita: Double,
  serviceOutput: Double,
  indexOfPersistentPollution: Double,
  lifeExpectancy: Double,
  crudeBirthRate: Double,
  crudeDeathRate: Double,
  arableLand: Double,
  landYield: Double)

class World3 {




  /*  Limits to Growth: This is a re-implementation in JavaScript
    of World3, the social-economic-environmental model created by
    Dennis and Donella Meadows and others circa 1970. The results
    of the modeling exercise were published in The Limits to Growth
    in 1972, and the model itself was more fully documented in
    Dynamics of Growth in a Finite World in 1974.

*/


  object Constants {
    val lifeExpectancyNormal = 32.0 // used in eqn 19
    val subsistenceFoodPerCapitaK = 230.0 // kilograms per person-year, used in eqns 20, 127
    var effectiveHealthServicesPerCapitaImpactDelay = 20.0 // years, used in eqn 22
    val potentiallyArableLandTotal = 3.2e9   // hectares, used here and in eqn 97
    val industrialOutputValueIn1970 = 7.9e11 // for eqns 106 and 107
    val averageLifetimeOfAgriculturalInputsK = 2.0 // years, eqn 99 (in lieu of 100)
    val socialDiscount = 0.07 // eqn 109
    val averageLifeOfLandNormal = 6000.0 // years, used in eqn 112
    val inherentLandFertilityK = 600.0 // kilograms per hectare-year, used in eqns 114, 115 and 124
    val developmentTime = 10.0 // years, used in eqn 119
    val foodShortagePerceptionDelayK = 2.0 // years, used in eqn 128
    val nonrenewableResourcesInitialK = 1.0e12 // resource units, used in eqns 129 and 133
    val fractionOfResourcesAsPersistentMaterial = 0.02 // dimensionless, used in eqn 139
    val industrialMaterialsEmissionFactor = 0.1 // dimensionless, used in eqn 139
    val industrialMaterialsToxicityIndex = 10.0 // pollution units per resource unit, used in eqn 139
    val persistentPollutionTransmissionDelayK = 20.0 // years, used in eqn 141
    val fractionOfInputsAsPersistentMaterial = 0.001 // dimensionless, used in eqn 141
    val agriculturalMaterialsToxicityIndex = 1.0 // pollution units per dollar, used in eqn 141
    val pollutionValueIn1970 = 1.36e8 // pollution units, used in eqn 143
    val lifetimeMultiplierFromHealthServicesPolicyYear = 1940.0
    val birthsPerYearReproductiveLifetime = 30.0 // years
    val birthsPerYearPopulationEquilibriumTime = 4000.0 // year
    val maxTotalFertilityNormal = 12.0 // dimensionless
    val lifetimePerceptionDelayK = 20.0 // years, used in eqn 37
    val desiredCompletedFamilySizeNormal = 4.0
    val zeroPopulationGrowthTargetYear = 4000.0
    val assimilationHalfLifeValueIn1970 = 1.5 // years, used in eqn 146
    val socialAdjustmentDelayK = 20.0 // years, used in eqn 40
    val incomeExpectationAveragingTimeK = 3.0 // years, used in eqn 43
    val healthServicesImpactDelayK = 20.0 // years, for eqn 46
    val industrialCapitalOutputRatioBefore = 3.0
    val industrialCapitalOutputRatioAfter = 3.0
    val averageLifetimeOfIndustrialCapitalBefore = 14.0
    val averageLifetimeOfIndustrialCapitalAfter = 14.0

    val fractionOfIndustrialOutputAllocatedToConsumptionIndustrialEquilibriumTime = 4000.0  // year
    val fractionOfIndustrialOutputAllocatedToConsumptionConstantBefore = 0.43
    val fractionOfIndustrialOutputAllocatedToConsumptionConstantAfter = 0.43
    val fractionOfIndustrialOutputAllocatedToConsumptionVariableIndustrialOutputPerCapitaDesired = 400.0

    val averageLifetimeOfServiceCapitalBefore = 20.0 // years
    val averageLifetimeOfServiceCapitalAfter = 20.0 // years

    val serviceCapitalOutputRatioBefore = 1.0
    val serviceCapitalOutputRatioAfter = 1.0

    val laborForceParticipationFraction = 0.75  // dimensionless
    val laborUtilizationFractionDelayedDelayTime = 2.0 // years, eqn 82

    val landFractionCultivatedPotentiallyArableLandTotal = 3.2e9   // hectares, used here and in eqn 97

    val foodLandFractionHarvestedK = 0.7   // dimensionless
    val foodProcessingLossK = 0.1          // dimensionless
  }

  // The DYNAMO clip function, a poor-man's conditional expression.
  //  function clip(a, b, x, y) {
  //    if (x >= y)
  //      return a;
  //    else
  //      return b
  //  }

  def clip(a: Option[Double], b: Option[Double], x: Option[Double], y: Option[Double]): Option[Double] =
    lift{ if (unlift(x) >= unlift(y)) unlift(a) else unlift(b) }

  sealed trait All {
    def j: Option[Double]
    def k: Option[Double]
    def qName: String
    def reset()
    def warmup()
    def tick()
    def update()
  }

  object Level {
    def apply(qName: String, qNumber: Int, initVal: Double, updateFn: () => Option[Double], units: String = "dimensionless", dependencies: Vector[String] = Vector()) =
      new Level(qName, qNumber, initVal, updateFn, units, dependencies)
  }

  class Level(
    val qName: String,
    val qNumber: Int,
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

    def warmup() = { k = updateFn() }

    def update() = {
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

    def warmup() = { k = updateFn() }

    def update() = {
      k = updateFn()
      k.get
    }

    def tick() = { j = k }
  }

  object Aux {
    def apply(qName: String, qNumber: Int, updateFn: () => Option[Double], units: String = "dimensionless", dependencies: Vector[String] = Vector()) =
      new Aux(qName, qNumber, updateFn, units, dependencies)
  }

  class Aux(
    val qName: String,
    val qNumber: Int,
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

    def warmup() = { k = updateFn() }

    def update() = {
      k = updateFn()
      k.get
    }

    def tick() = { j = k }
  }

  object Smooth {
    def apply(
      qName: String,
      qNumber: Int,
      delay: Double,
      initFn: () => All,
      initVal: Option[Double] = None,
      units: String = "dimensionless",
      dependencies: Vector[String] = Vector()) =
        new Smooth(qName, qNumber, initFn, initVal, units, delay, dependencies)

  }

  class Smooth(
    val qName: String,
    val qNumber: Int,
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

    def update() = {
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

    def warmup(): Unit = init()
    def tick() = { j = k }
  }

  //  // constructor for Delay3 objects
  //  // third-order exponential delay for Rate variables
  object Delay3 {
    case class JK(j: Option[Double], k: Option[Double])

    def apply(qName: String, qNumber: Int, initFn: () => All, delay: Double, units: String = "dimensionless", dependencies: Vector[String] = Vector()) =
      new Delay3(qName, qNumber, initFn, units, delay, dependencies)
  }

  class Delay3(
    val qName: String,
    val qNumber: Int,
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

    def update() = {
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

    def warmup() = init()
    def tick() = { j = k }
  }

  object Table {

    def apply(
      qName: String,
      qNumber: Int,
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
    val qNumber: Int,
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

    def update() = {
      k = updateFn().map(lookup)
    }

    def warmup() = update()

    def tick() = { j = k }
  }



  // PARAMETERS THAT GOVERN THE RUNNING OF THE MODEL
  val startTime: Double = 1900.0
  val stopTime: Double = 2100.0

  var t: Double = 1900.0
  val dt: Double = 1.0
  val policyYear: Double = 1975.0                 // eqn 150.1

  def resetModel() =  {
    t = startTime
    all.foreach(_.reset())
  }

  def initSmoothsAndDelay3s() =
    all.foreach {
      case s: Smooth => s.init()
      case d: Delay3 => d.init()
      case _ =>
    }

  def updateAuxen() = auxSequence.foreach(_.update())
  def updateRates() = rates.foreach(_.update())
  def updateLevels() =  levels.foreach(_.update())

  def warmupAuxen() = auxSequence.foreach(_.warmup())
  def warmupRates() = rates.foreach(_.warmup())
  def tock() = all.foreach(_.tick())

  def initModel() =  {
    initSmoothsAndDelay3s()
    t = startTime
  }

   def timeStep() =  {
      updateLevels()
      updateAuxen()
      updateRates()
      tock()
      t += dt
    }

  def fastRun() =  {
    resetModel()
    initModel()

    for (_ <- 1 to 100) {
      warmupAuxen()
      warmupRates()
      tock()
    }

    assert(all.forall(_.k.isDefined))

    val result = ListBuffer[StepValues]()

    while (t <= stopTime) {
      result += StepValues(t, population.k.get, nonrenewableResourceFractionRemaining.k.get, foodPerCapita.k.get, industrialOutputPerCapita.k.get, serviceOutput.k.get, indexOfPersistentPollution.k.get, lifeExpectancy.k.get, crudeBirthRate.k.get, crudeDeathRate.k.get, arableLand.k.get, landYield.k.get)
      timeStep()
    }

    result.toVector
  }



 // THE POPULATION SECTOR

  val population =
    Aux(
      qName = "population",
      qNumber = 1,
      updateFn = () => lift { unlift(population0To14.k) + unlift(population15To44.k) + unlift(population45To64.k) + unlift(population65AndOver.k) },
      units = "persons"
    )

  val population0To14: Level =
    Level(
      qName = "population0To14",
      qNumber = 2,
      initVal = 6.5e8,
      units = "persons",
      updateFn = () => lift {
        unlift(population0To14.j) + dt *
          (unlift(birthsPerYear.j) - unlift(deathsPerYear0To14.j) - unlift(maturationsPerYear14to15.j))
      }
    )

  val deathsPerYear0To14 =
    Rate(
      qName = "deathsPerYear0To14",
      qNumber = 3,
      units = "persons per year",
      updateFn = () => lift { unlift(population0To14.k) * unlift(mortality0To14.k) }
    )

  val mortality0To14 =
    Table(
      qName = "mortality0To14",
      qNumber = 4,
      data = Vector(0.0567, 0.0366, 0.0243, 0.0155, 0.0082, 0.0023, 0.0010),
      iMin = 20,
      iMax = 80,
      iDelta = 10,
      units =  "deaths per person-year",
      dependencies = Vector("lifeExpectancy"),
      updateFn = () => lifeExpectancy.k
    )

  val maturationsPerYear14to15 =
    Rate(
      qName = "maturationsPerYear14to15",
      qNumber = 5,
      units = "persons per year",
      updateFn = () => lift { unlift(population0To14.k) * (1 - unlift(mortality0To14.k)) / 15 }
    )

  val population15To44: Level =
    Level(
      "population15To44",
      6,
      7.0e8,
      units = "persons",
      updateFn = () => lift {
        unlift(population15To44.j) + dt * (unlift(maturationsPerYear14to15.j) - unlift(deathsPerYear15To44.j) - unlift(maturationsPerYear44to45.j))
      }
    )

  val deathsPerYear15To44: Rate =
    Rate(
      "deathsPerYear15To44",
      7,
      units = "persons per year",
      updateFn = () => lift { unlift(population15To44.k) * unlift(mortality15To44.k) }
    )

  val mortality15To44 =
    Table(
      "mortality15To44",
      8,
      Vector(0.0266, 0.0171, 0.0110, 0.0065, 0.0040, 0.0016, 0.0008),
      20,
      80,
      10,
      units = "deaths per person-year",
      dependencies = Vector("lifeExpectancy"),
      updateFn = () => lift { unlift(lifeExpectancy.k) }
    )

  val maturationsPerYear44to45 =
    Rate(
      "maturationsPerYear44to45",
      9,
      units = "persons per year",
      updateFn = () => lift { unlift(population15To44.k) * (1 - unlift(mortality15To44.k)) / 30 }
    )

  val population45To64: Level =
    Level(
      "population45To64",
      10,
      1.9e8,
      units = "persons",
      updateFn = () => lift {
        unlift(population45To64.j) + dt * (unlift(maturationsPerYear44to45.j) - unlift(deathsPerYear45To64.j) - unlift(maturationsPerYear64to65.j))
      }
    )

  val deathsPerYear45To64: Rate =
    Rate(
      "deathsPerYear45To64",
      11,
      units = "persons per year",
      updateFn = () => lift { unlift(population45To64.k) * unlift(mortality45To64.k) }
    )

  val mortality45To64 =
    Table(
      "mortality45To64",
      12,
      Vector(0.0562, 0.0373, 0.0252, 0.0171, 0.0118, 0.0083, 0.0060),
      20,
      80,
      10,
      units = "deaths per person-year",
      dependencies = Vector("lifeExpectancy"),
      updateFn = () => lift { unlift(lifeExpectancy.k) }
    )

  val maturationsPerYear64to65 =
    Rate(
      "maturationsPerYear64to65",
      13,
      units = "persons per year",
      updateFn = () => lift { unlift(population45To64.k) * (1 - unlift(mortality45To64.k)) / 20 }
    )

  val population65AndOver: Level =
    Level(
      "population65AndOver",
      14,
      6.0e7,
      units = "persons",
      updateFn = () => lift {  unlift(population65AndOver.j) + dt * (unlift(maturationsPerYear64to65.j) - unlift(deathsPerYear65AndOver.j)) }
    )

  val deathsPerYear65AndOver =
    Rate(
      "deathsPerYear65AndOver",
      15,
      units = "persons per year",
      updateFn = () => lift { unlift(population65AndOver.k) * unlift(mortality65AndOver.k) }
    )

  val mortality65AndOver =
    Table(
      "mortality65AndOver",
      16,
      Vector(0.13, 0.11, 0.09, 0.07, 0.06, 0.05, 0.04),
      20,
      80,
      10,
      units = "deaths per person-year",
      dependencies = Vector("lifeExpectancy"),
      updateFn = () => lifeExpectancy.k
    )


  // The Death-Rate Subsector
  val deathsPerYear =
    Aux(
      "deathsPerYear",
      17,
      units = "persons per year",
      updateFn =
        () => lift {
          unlift(deathsPerYear0To14.j) +
            unlift(deathsPerYear15To44.j) +
            unlift(deathsPerYear45To64.j) +
            unlift(deathsPerYear65AndOver.j)
        }
    )

  val crudeDeathRate =
    Aux(
      "crudeDeathRate",
      18,
      units = "deaths per 1000 person-years",
      dependencies = Vector("deathsPerYear", "population"),
      updateFn = () => lift { 1000 * unlift(deathsPerYear.k) / unlift(population.k) }
    )

  val lifeExpectancy =
    Aux(
      "lifeExpectancy",
      19,
      units = "years",
      dependencies = Vector("lifetimeMultiplierFromFood", "lifetimeMultiplierFromHealthServices", "lifetimeMultiplierFromPollution", "lifetimeMultiplierFromCrowding"),
      updateFn = () => lift {
       Constants.lifeExpectancyNormal *
        unlift(lifetimeMultiplierFromFood.k) *
        unlift(lifetimeMultiplierFromHealthServices.k) *
        unlift(lifetimeMultiplierFromPollution.k) *
        unlift(lifetimeMultiplierFromCrowding.k) }
    )

  val lifetimeMultiplierFromFood =
    Table(
      "lifetimeMultiplierFromFood",
      20,
      Vector(0, 1, 1.2, 1.3, 1.35, 1.4),
      0,
      5,
      1,
      dependencies = Vector("foodPerCapita"),
      updateFn = () => lift { unlift(foodPerCapita.k) / Constants.subsistenceFoodPerCapitaK }
    )

  val healthServicesAllocationsPerCapita =
    Table(
      "healthServicesAllocationsPerCapita",
      21,
      Vector(0, 20, 50, 95, 140, 175, 200, 220, 230),
      0,
      2000,
      250,
      units = "dollars per person-year",
      dependencies = Vector("serviceOutputPerCapita"),
      updateFn = () => serviceOutputPerCapita.k
    )

  val effectiveHealthServicesPerCapita =
    Smooth(
      "effectiveHealthServicesPerCapita",
      22,
      Constants.effectiveHealthServicesPerCapitaImpactDelay,
      units = "dollars per person-year",
      dependencies = Vector("healthServicesAllocationsPerCapita"),
      initFn = () => healthServicesAllocationsPerCapita
    )

  val lifetimeMultiplierFromHealthServices =
    Aux(
      "lifetimeMultiplierFromHealthServices",
      23,
      dependencies = Vector("lifetimeMultiplierFromHealthServicesBefore", "lifetimeMultiplierFromHealthServicesAfter"),
      updateFn = () => clip(
        lifetimeMultiplierFromHealthServicesAfter.k,
        lifetimeMultiplierFromHealthServicesBefore.k,
        Some(t),
        Some(Constants.lifetimeMultiplierFromHealthServicesPolicyYear)
      )
    )

  val lifetimeMultiplierFromHealthServicesBefore =
    Table(
      "lifetimeMultiplierFromHealthServicesBefore",
      24,
      Vector(1, 1.1, 1.4, 1.6, 1.7, 1.8),
      0,
      100,
      20,
      dependencies = Vector("effectiveHealthServicesPerCapita"),
      updateFn = () => effectiveHealthServicesPerCapita.k
    )

  val lifetimeMultiplierFromHealthServicesAfter =
    Table(
      "lifetimeMultiplierFromHealthServicesAfter",
      25,
      Vector(1, 1.4, 1.6, 1.8, 1.95, 2.0),
      0,
      100,
      20,
      dependencies = Vector("effectiveHealthServicesPerCapita"),
      updateFn = () => effectiveHealthServicesPerCapita.k
    )

  val fractionOfPopulationUrban =
    Table(
      "fractionOfPopulationUrban",
      26,
      Vector(0, 0.2, 0.4, 0.5, 0.58, 0.65, 0.72, 0.78, 0.80),
      0,
      1.6e10,
      2.0e9,
      dependencies = Vector("population"),
      updateFn = () => population.k
    )

  val crowdingMultiplierFromIndustrialization =
    Table(
      "crowdingMultiplierFromIndustrialization",
      27,
      Vector(0.5, 0.05, -0.1, -0.08, -0.02, 0.05, 0.1, 0.15, 0.2),
      0,
      1600,
      200,
      dependencies = Vector("industrialOutputPerCapita"),
      updateFn = () => industrialOutputPerCapita.k
    )

  val lifetimeMultiplierFromCrowding =
    Aux(
      "lifetimeMultiplierFromCrowding",
      28,
      updateFn = () => lift { 1 - (unlift(crowdingMultiplierFromIndustrialization.k) * unlift(fractionOfPopulationUrban.k)) }
    )

  val lifetimeMultiplierFromPollution =
    Table(
      "lifetimeMultiplierFromPollution",
      29,
      Vector(1.0, 0.99, 0.97, 0.95, 0.90, 0.85, 0.75, 0.65, 0.55, 0.40, 0.20),
      0,
      100,
      10,
      dependencies = Vector("indexOfPersistentPollution"),
      updateFn = () => indexOfPersistentPollution.k
    )

    val birthsPerYear =
      Rate(
        "birthsPerYear",
        30,
        units = "persons per year",
        updateFn = () => {
          clip(
            deathsPerYear.k,
            lift { unlift(totalFertility.k) * unlift(population15To44.k) * 0.5 / Constants.birthsPerYearReproductiveLifetime },
            Some(t),
            Some(Constants.birthsPerYearPopulationEquilibriumTime)
          )
        }
      )

  val crudeBirthRate =
    Aux(
      "crudeBirthRate",
      31,
      units = "births per 1000 person-years",
      dependencies = Vector("population"),
      updateFn = () => lift { 1000 * unlift(birthsPerYear.j) / unlift(population.k) }
    )

  val totalFertility =
    Aux(
      "totalFertility",
      32,
      dependencies = Vector("maxTotalFertility", "fertilityControlEffectiveness", "desiredTotalFertility"),
      updateFn = () => lift {
        math.min(
          unlift(maxTotalFertility.k),
          unlift(maxTotalFertility.k) * (1 - unlift(fertilityControlEffectiveness.k)) + unlift(desiredTotalFertility.k) * unlift(fertilityControlEffectiveness.k)
        )
      }
    )

  val maxTotalFertility =
    Aux(
      "maxTotalFertility",
      33,
      dependencies = Vector("fecundityMultiplier"),
      updateFn = () => lift { Constants.maxTotalFertilityNormal * unlift(fecundityMultiplier.k) }
    )

  val fecundityMultiplier =
    Table(
      "fecundityMultiplier",
      34,
      Vector(0.0, 0.2, 0.4, 0.6, 0.8, 0.9, 1.0, 1.05, 1.1),
      0,
      80,
      10,
      dependencies = Vector("lifeExpectancy"),
      updateFn = () => lifeExpectancy.k
    )

  val desiredTotalFertility =
    Aux(
      "desiredTotalFertility",
      35,
      dependencies = Vector("desiredCompletedFamilySize", "compensatoryMultiplierFromPerceivedLifeExpectancy"),
      updateFn = () => lift { unlift(desiredCompletedFamilySize.k) * unlift(compensatoryMultiplierFromPerceivedLifeExpectancy.k) }
    )

  val compensatoryMultiplierFromPerceivedLifeExpectancy =
    Table(
      "compensatoryMultiplierFromPerceivedLifeExpectancy",
      36,
      Vector(3.0, 2.1, 1.6, 1.4, 1.3, 1.2, 1.1, 1.05, 1.0),
      0,
      80,
      10,
      dependencies = Vector("perceivedLifeExpectancy"),
      updateFn = () => perceivedLifeExpectancy.k
    )

  val perceivedLifeExpectancy =
    Delay3(
      "perceivedLifeExpectancy",
      37,
      delay=Constants.lifetimePerceptionDelayK,
      units = "years",
      dependencies = Vector("lifeExpectancy"),
      initFn = () => lifeExpectancy
    )

  val desiredCompletedFamilySize =
    Aux(
      "desiredCompletedFamilySize",
      38,
//      units = "dimensionless",            // not persons?
      dependencies = Vector("familyResponseToSocialNorm", "socialFamilySizeNorm"),
      updateFn = () => clip(
        Some(2.0),
        lift { Constants.desiredCompletedFamilySizeNormal * unlift(familyResponseToSocialNorm.k) * unlift(socialFamilySizeNorm.k) },
        Some(t),
        Some(Constants.zeroPopulationGrowthTargetYear)
      )
    )

  val socialFamilySizeNorm =
    Table(
      "socialFamilySizeNorm",
      39,
      Vector(1.25, 1, 0.9, 0.8, 0.75),
      0,
      800,
      200,
      dependencies = Vector("delayedIndustrialOutputPerCapita"),
      updateFn = () => delayedIndustrialOutputPerCapita.k
    )

  val delayedIndustrialOutputPerCapita =
    Delay3(
      "delayedIndustrialOutputPerCapita",
      40,
      delay=Constants.socialAdjustmentDelayK,
      units = "dollars per person-year",
      dependencies = Vector("industrialOutputPerCapita"),
      initFn = () => industrialOutputPerCapita
    )

  val familyResponseToSocialNorm =
    Table(
      "familyResponseToSocialNorm",
      41,
      Vector(0.5, 0.6, 0.7, 0.85, 1.0),
      -0.2,
      0.2,
      0.1,
      dependencies = Vector("familyIncomeExpectation"),
      updateFn = () => familyIncomeExpectation.k
    )

  val familyIncomeExpectation =
    Aux(
      "familyIncomeExpectation",
      42,
      dependencies = Vector("industrialOutputPerCapita", "averageIndustrialOutputPerCapita"),
      updateFn = () => lift { (unlift(industrialOutputPerCapita.k) - unlift(averageIndustrialOutputPerCapita.k)) / unlift(averageIndustrialOutputPerCapita.k) }
    )

  val averageIndustrialOutputPerCapita =
    Smooth(
      "averageIndustrialOutputPerCapita",
      43,
      Constants.incomeExpectationAveragingTimeK,
      units = "dollars per person-year",
      dependencies = Vector("industrialOutputPerCapita"),
      initFn = () => industrialOutputPerCapita
    )

  val needForFertilityControl =
    Aux(
      "needForFertilityControl",
      44,
      dependencies = Vector("maxTotalFertility", "desiredTotalFertility"),
      updateFn = () => lift { (unlift(maxTotalFertility.k) / unlift(desiredTotalFertility.k)) - 1 }
    )

  val fertilityControlEffectiveness =
    Table(
      "fertilityControlEffectiveness",
      45,
      Vector(0.75, 0.85, 0.90, 0.95, 0.98, 0.99, 1.0),
      0,
      3,
      0.5,
      dependencies = Vector("fertilityControlFacilitiesPerCapita"),
      updateFn = () => fertilityControlFacilitiesPerCapita.k
    )

  val fertilityControlFacilitiesPerCapita =
    Delay3(
      "fertilityControlFacilitiesPerCapita",
      46,
      delay=Constants.healthServicesImpactDelayK,
      units = "dollars per person-year",
      dependencies = Vector("fertilityControlAllocationPerCapita"),
      initFn = () => fertilityControlAllocationPerCapita
    )

  val fertilityControlAllocationPerCapita =
    Aux(
      "fertilityControlAllocationPerCapita",
      47,
      units = "dollars per person-year",
      dependencies = Vector("serviceOutputPerCapita", "fractionOfServicesAllocatedToFertilityControl"),
      updateFn = () => lift { unlift(fractionOfServicesAllocatedToFertilityControl.k) * unlift(serviceOutputPerCapita.k) }
    )

  val fractionOfServicesAllocatedToFertilityControl =
    Table(
      "fractionOfServicesAllocatedToFertilityControl",
      48,
      Vector(0.0, 0.005, 0.015, 0.025, 0.030, 0.035),
      0,
      10,
      2,
      dependencies = Vector("needForFertilityControl"),
      updateFn = () => needForFertilityControl.k
    )


//  // THE CAPITAL SECTOR
//
//  // The Industrial Subsector
  val industrialOutputPerCapita =
    Aux(
      "industrialOutputPerCapita",
      49,
      units = "dollars per person-year",
      dependencies = Vector("industrialOutput", "population"),
      updateFn = () => lift { unlift(industrialOutput.k) / unlift(population.k) }
    )

  val industrialOutput =
    Aux(
      "industrialOutput",
      50,
      units = "dollars per year",
      dependencies = Vector("fractionOfCapitalAllocatedToObtainingResources", "capitalUtilizationFraction", "industrialCapitalOutputRatio"),
      updateFn = () => lift {
        unlift(industrialCapital.k) * (1 - unlift(fractionOfCapitalAllocatedToObtainingResources.k)) * unlift(capitalUtilizationFraction.k) / unlift(industrialCapitalOutputRatio.k) }
    )

  val industrialCapitalOutputRatio =
    Aux(
      "industrialCapitalOutputRatio",
      51,
      units = "years",
      updateFn = () => clip(
        Some(Constants.industrialCapitalOutputRatioAfter),
        Some(Constants.industrialCapitalOutputRatioBefore),
        Some(t),
        Some(policyYear)
      )
    )

  val industrialCapital: Level =
    Level(
      "industrialCapital",
      52,
      2.1e11,
      units = "dollars",
      updateFn = () => lift { unlift(industrialCapital.j) + dt * (unlift(industrialCapitalInvestmentRate.j) - unlift(industrialCapitalDepreciationRate.j)) }
    )

  val industrialCapitalDepreciationRate =
    Rate(
      "industrialCapitalDepreciationRate",
      53,
      units = "dollars per year",
      updateFn = () => lift { unlift(industrialCapital.k) / unlift(averageLifetimeOfIndustrialCapital.k) }
    )

  val averageLifetimeOfIndustrialCapital =
    Aux(
      "averageLifetimeOfIndustrialCapital",
      54,
      units = "years",
      updateFn = () => clip(
        Some(Constants.averageLifetimeOfIndustrialCapitalAfter),
        Some(Constants.averageLifetimeOfIndustrialCapitalBefore),
        Some(t),
        Some(policyYear)
      )
    )

  val industrialCapitalInvestmentRate =
    Rate(
      "industrialCapitalInvestmentRate",
      55,
      units = "dollars per year",
      updateFn = () => lift { unlift(industrialOutput.k) * unlift(fractionOfIndustrialOutputAllocatedToIndustry.k) }
    )

  val fractionOfIndustrialOutputAllocatedToIndustry =
    Aux(
      "fractionOfIndustrialOutputAllocatedToIndustry",
      56,
      dependencies = Vector("fractionOfIndustrialOutputAllocatedToAgriculture", "fractionOfIndustrialOutputAllocatedToServices", "fractionOfIndustrialOutputAllocatedToConsumption"),
      updateFn = () => lift { 1 - unlift(fractionOfIndustrialOutputAllocatedToAgriculture.k) - unlift(fractionOfIndustrialOutputAllocatedToServices.k) - unlift(fractionOfIndustrialOutputAllocatedToConsumption.k) }
    )

  val fractionOfIndustrialOutputAllocatedToConsumption =
    Aux(
      "fractionOfIndustrialOutputAllocatedToConsumption",
      57,
      dependencies = Vector("fractionOfIndustrialOutputAllocatedToConsumptionVariable"),
      updateFn = () => clip(
        fractionOfIndustrialOutputAllocatedToConsumptionVariable.k,
        fractionOfIndustrialOutputAllocatedToConsumptionConstant.k,
        Some(t),
        Some(Constants.fractionOfIndustrialOutputAllocatedToConsumptionIndustrialEquilibriumTime))
    )

  val fractionOfIndustrialOutputAllocatedToConsumptionConstant =
    Aux(
      "fractionOfIndustrialOutputAllocatedToConsumptionConstant",
      58,
      updateFn = () => clip(
        lift(Constants.fractionOfIndustrialOutputAllocatedToConsumptionConstantAfter),
        lift(Constants.fractionOfIndustrialOutputAllocatedToConsumptionConstantBefore),
        Some(t),
        Some(policyYear))
    )

  val fractionOfIndustrialOutputAllocatedToConsumptionVariable =
    Table(
      "fractionOfIndustrialOutputAllocatedToConsumptionVariable",
      59,
      Vector(0.3, 0.32, 0.34, 0.36, 0.38, 0.43, 0.73, 0.77, 0.81, 0.82, 0.83),
      0, 2,
      0.2,
      dependencies = Vector("industrialOutputPerCapita"),
      updateFn = () => lift { unlift(industrialOutputPerCapita.k) / Constants.fractionOfIndustrialOutputAllocatedToConsumptionVariableIndustrialOutputPerCapitaDesired }
    )


  // The Service Subsector

  val indicatedServiceOutputPerCapita =
    Aux(
      "indicatedServiceOutputPerCapita",
      60,
      units = "dollars per person-year",
      dependencies = Vector("indicatedServiceOutputPerCapitaAfter", "indicatedServiceOutputPerCapitaBefore"),
      updateFn = () => clip(
        indicatedServiceOutputPerCapitaAfter.k,
        indicatedServiceOutputPerCapitaBefore.k,
        Some(t),
        Some(policyYear))
    )

  val indicatedServiceOutputPerCapitaBefore =
    Table(
      qName = "indicatedServiceOutputPerCapitaBefore",
      qNumber = 61,
      data = Vector(40, 300, 640, 1000, 1220, 1450, 1650, 1800, 2000),
      iMin = 0,
      iMax = 1600,
      iDelta = 200,
      units = "dollars per person-year",
      dependencies = Vector("industrialOutputPerCapita"),
      updateFn = () => industrialOutputPerCapita.k
    )

  val indicatedServiceOutputPerCapitaAfter =
    Table(
      qName = "indicatedServiceOutputPerCapitaAfter",
      qNumber = 62,
      data = Vector(40, 300, 640, 1000, 1220, 1450, 1650, 1800, 2000),
      iMin = 0,
      iMax = 1600,
      iDelta = 200,
      units = "dollars per person-year",
      dependencies = Vector("industrialOutputPerCapita"),
      updateFn = () => industrialOutputPerCapita.k
    )

  val fractionOfIndustrialOutputAllocatedToServices =
    Aux(
      qName = "fractionOfIndustrialOutputAllocatedToServices",
      qNumber = 63,
      dependencies = Vector("fractionOfIndustrialOutputAllocatedToServicesBefore", "fractionOfIndustrialOutputAllocatedToServicesAfter"),
      updateFn = () => clip(
        fractionOfIndustrialOutputAllocatedToServicesAfter.k,
        fractionOfIndustrialOutputAllocatedToServicesBefore.k,
        Some(t),
        Some(policyYear)
      )
    )

  val fractionOfIndustrialOutputAllocatedToServicesBefore =
    Table(
      qName = "fractionOfIndustrialOutputAllocatedToServicesBefore",
      qNumber = 64,
      data = Vector(0.3, 0.2, 0.1, 0.05, 0),
      iMin = 0, iMax = 2, iDelta = 0.5,
      dependencies = Vector("serviceOutputPerCapita", "indicatedServiceOutputPerCapita"),
      updateFn = () => lift { unlift(serviceOutputPerCapita.k) / unlift(indicatedServiceOutputPerCapita.k) }
    )

  val fractionOfIndustrialOutputAllocatedToServicesAfter =
    Table(
      qName = "fractionOfIndustrialOutputAllocatedToServicesAfter",
      qNumber = 65,
      data = Vector(0.3, 0.2, 0.1, 0.05, 0),
      iMin = 0, iMax = 2, iDelta = 0.5,
      dependencies = Vector("serviceOutputPerCapita", "indicatedServiceOutputPerCapita"),
      updateFn = () => lift { unlift(serviceOutputPerCapita.k) / unlift(indicatedServiceOutputPerCapita.k) }
    )

  val serviceCapitalInvestmentRate =
    Rate(
      "serviceCapitalInvestmentRate",
      66,
      units = "dollars per year",
      updateFn = () => lift { unlift(industrialOutput.k) * unlift(fractionOfIndustrialOutputAllocatedToServices.k) }
    )

  val serviceCapital:Level =
    Level(
      "serviceCapital",
      67,
      1.44e11,
      units = "dollars",
      updateFn = () => lift { unlift(serviceCapital.j) + dt * (unlift(serviceCapitalInvestmentRate.j) - unlift(serviceCapitalDepreciationRate.j)) }
    )

  val serviceCapitalDepreciationRate =
    Rate(
      "serviceCapitalDepreciationRate",
      68,
      units = "dollars per year",
      updateFn = () => lift { unlift(serviceCapital.k) / unlift(averageLifetimeOfServiceCapital.k) }
    )

  val averageLifetimeOfServiceCapital =
    Aux(
      "averageLifetimeOfServiceCapital",
      69,
      units = "years",
      updateFn = () => clip(
        Some(Constants.averageLifetimeOfServiceCapitalAfter),
        Some(Constants.averageLifetimeOfServiceCapitalBefore),
        Some(t),
        Some(policyYear)
      )
    )

  val serviceOutput =
    Aux(
      "serviceOutput",
      70,
      units = "dollars per year",
      dependencies = Vector("capitalUtilizationFraction", "serviceCapitalOutputRatio"),
      updateFn = () => lift { (unlift(serviceCapital.k) * unlift(capitalUtilizationFraction.k)) / unlift(serviceCapitalOutputRatio.k) }
    )

  val serviceOutputPerCapita =
    Aux(
      "serviceOutputPerCapita",
      71,
      units = "dollars per person-year",
      dependencies = Vector("serviceOutput", "population"),
      updateFn = () => lift { unlift(serviceOutput.k) / unlift(population.k) }
    )

  val serviceCapitalOutputRatio =
    Aux(
      "serviceCapitalOutputRatio",
      72,
      units = "years",
      updateFn = () => clip(
        Some(Constants.serviceCapitalOutputRatioAfter),
        Some(Constants.serviceCapitalOutputRatioBefore),
        Some(t),
        Some(policyYear)
      )
    )

 // The Jobs Subsector
  val jobs =
    Aux(
      "jobs",
      73,
      units = "persons",
      dependencies = Vector("potentialJobsInIndustrialSector", "potentialJobsInAgriculturalSector", "potentialJobsInServiceSector"),
      updateFn = () => lift { unlift(potentialJobsInIndustrialSector.k) + unlift(potentialJobsInAgriculturalSector.k) + unlift(potentialJobsInServiceSector.k) }
    )

  val potentialJobsInIndustrialSector =
    Aux(
      "potentialJobsInIndustrialSector",
      74,
      units = "persons",
      dependencies = Vector("jobsPerIndustrialCapitalUnit"),
      updateFn = () => lift { unlift(industrialCapital.k) * unlift(jobsPerIndustrialCapitalUnit.k) }
    )

  val jobsPerIndustrialCapitalUnit =
    Table(
      "jobsPerIndustrialCapitalUnit",
      75,
      Vector(0.00037, 0.00018, 0.00012, 0.00009, 0.00007, 0.00006),
      50,
      800,
      150,
      units = "persons per dollar",
      dependencies = Vector("industrialOutputPerCapita"),
      updateFn = () => { industrialOutputPerCapita.k }
    )

  val potentialJobsInServiceSector =
    Aux(
      "potentialJobsInServiceSector",
      76,
      units = "persons",
      dependencies = Vector("jobsPerServiceCapitalUnit"),
      updateFn = () => lift { unlift(serviceCapital.k) * unlift(jobsPerServiceCapitalUnit.k) }
    )

  val jobsPerServiceCapitalUnit =
    Table(
      "jobsPerServiceCapitalUnit",
      77,
      Vector(0.0011, 0.0006, 0.00035, 0.0002, 0.00015, 0.00015),
      50,
      800,
      150,
      units = "persons per dollar",
      dependencies = Vector("serviceOutputPerCapita"),
      updateFn = () => { serviceOutputPerCapita.k }
    )

  val potentialJobsInAgriculturalSector =
    Aux(
      "potentialJobsInAgriculturalSector",
      78,
      units = "persons",
      dependencies = Vector("jobsPerHectare"),
      updateFn = () => lift { unlift(arableLand.k) * unlift(jobsPerHectare.k) }
    )

  val jobsPerHectare =
    Table(
      "jobsPerHectare",
      79,
      Vector(2, 0.5, 0.4, 0.3, 0.27, 0.24, 0.2, 0.2),
      2,
      30,
      4,
      units = "persons per hectare",
      dependencies = Vector("agriculturalInputsPerHectare"),
      updateFn = () => { agriculturalInputsPerHectare.k }
    )

  val laborForce =
    Aux(
      "laborForce",
      80,
      units = "persons",
      updateFn = () => lift { (unlift(population15To44.k) + unlift(population45To64.k)) * Constants.laborForceParticipationFraction }
    )

  val laborUtilizationFraction =
    Aux(
      "laborUtilizationFraction",
      81,
      dependencies = Vector("jobs", "laborForce"),
      updateFn = () => lift { unlift(jobs.k) / unlift(laborForce.k) }
    )

  val laborUtilizationFractionDelayed =
    Smooth(
      "laborUtilizationFractionDelayed",
      82,
      Constants.laborUtilizationFractionDelayedDelayTime,
      dependencies = Vector("laborUtilizationFraction"),
      initFn = () => laborUtilizationFraction
    )

  val capitalUtilizationFraction: Table =
    Table(
      "capitalUtilizationFraction",
      83,
      Vector(1.0, 0.9, 0.7, 0.3, 0.1, 0.1),
      1,
      11,
      2,
      dependencies = Vector(),   // "laborUtilizationFractionDelayed" removed to break cycle
      updateFn = () => lift { laborUtilizationFractionDelayed.k.getOrElse(1.0) } // to break circularity
    )


//  // THE AGRICULTURAL SECTOR
//
//  // Loop 1: Food from Investment in Land Development

  val landFractionCultivated =
    Aux(
      "landFractionCultivated",
      84,
      updateFn = () => lift {unlift(arableLand.k) / Constants.landFractionCultivatedPotentiallyArableLandTotal }
    )

  val arableLand: Level =
    Level(
      "arableLand",
      85,
      0.9e9,
      units = "hectares",
      updateFn = () => lift {
        unlift(arableLand.j) +
          dt * (unlift(landDevelopmentRate.j) - unlift(landErosionRate.j) - unlift(landRemovalForUrbanIndustrialUse.j))
      }
    )

  val potentiallyArableLand: Level =
    Level(
      "potentiallyArableLand",
      86,
      2.3e9,
      units = "hectares",
      updateFn = () => lift { unlift(potentiallyArableLand.j) + dt * (-unlift(landDevelopmentRate.j)) }
    )

  val food =
    Aux(
      "food",
      87,
      units = "kilograms per year",
      dependencies = Vector("landYield"),
      updateFn = () => lift { unlift(landYield.k) * unlift(arableLand.k) * Constants.foodLandFractionHarvestedK * (1 - Constants.foodProcessingLossK) }
    )

  val foodPerCapita:Aux = Aux(
    qName="foodPerCapita",
    qNumber = 88,
    units = "kilograms per person-year",
    dependencies = Vector("food", "population"),
    updateFn = ()=> lift {unlift(food.k) / unlift(population.k)}
  )

  val indicatedFoodPerCapita =
    Aux(
      "indicatedFoodPerCapita",
      89,
      units = "kilograms per person-year",
      dependencies = Vector("indicatedFoodPerCapitaBefore", "indicatedFoodPerCapitaAfter"),
      updateFn = () => clip(indicatedFoodPerCapitaAfter.k, indicatedFoodPerCapitaBefore.k, Some(t), Some(policyYear))
    )

  val indicatedFoodPerCapitaBefore =
    Table(
      qName = "indicatedFoodPerCapitaBefore",
      qNumber = 90,
      data = Vector(230, 480, 690, 850, 970, 1070, 1150, 1210, 1250),
      iMin = 0, iMax = 1600, iDelta = 200,
      units = "kilograms per person-year",
      dependencies = Vector("industrialOutputPerCapita"),
      updateFn = () => industrialOutputPerCapita.k
    )

  val indicatedFoodPerCapitaAfter =
    Table(
      qName = "indicatedFoodPerCapitaAfter",
      qNumber = 91,
      data = Vector(230, 480, 690, 850, 970, 1070, 1150, 1210, 1250),
      iMin = 0, iMax = 1600, iDelta = 200,
      units = "kilograms per person-year",
      dependencies = Vector("industrialOutputPerCapita"),
      updateFn = () => industrialOutputPerCapita.k
    )

  val totalAgriculturalInvestment =
    Aux(
      "totalAgriculturalInvestment",
      92,
      units = "dollars per year",
      dependencies = Vector("industrialOutput", "fractionOfIndustrialOutputAllocatedToAgriculture"),
      updateFn = () => lift { unlift(industrialOutput.k) * unlift(fractionOfIndustrialOutputAllocatedToAgriculture.k)}
    )

  val fractionOfIndustrialOutputAllocatedToAgriculture =
    Aux(
      "fractionOfIndustrialOutputAllocatedToAgriculture",
      93,
      dependencies = Vector("fractionOfIndustrialOutputAllocatedToAgricultureBefore", "fractionOfIndustrialOutputAllocatedToAgricultureAfter"),
      updateFn = () => clip(
        fractionOfIndustrialOutputAllocatedToAgricultureAfter.k,
        fractionOfIndustrialOutputAllocatedToAgricultureBefore.k,
        Some(t),
        Some(policyYear))
    )

  val fractionOfIndustrialOutputAllocatedToAgricultureBefore =
    Table(
      qName = "fractionOfIndustrialOutputAllocatedToAgricultureBefore",
      qNumber = 94,
      data = Vector(0.4, 0.2, 0.1, 0.025, 0.0, 0.0),
      iMin = 0,
      iMax = 2.5,
      iDelta = 0.5,
      updateFn = () => lift { unlift(foodPerCapita.k) / unlift(indicatedFoodPerCapita.k) },
      dependencies = Vector("foodPerCapita", "indicatedFoodPerCapita")
    )

  val fractionOfIndustrialOutputAllocatedToAgricultureAfter =
    Table(
      qName = "fractionOfIndustrialOutputAllocatedToAgricultureAfter",
      qNumber = 95,
      data = Vector(0.4, 0.2, 0.1, 0.025, 0.0, 0.0),
      iMin = 0,
      iMax = 2.5,
      iDelta = 0.5,
      updateFn = () => lift { unlift(foodPerCapita.k) / unlift(indicatedFoodPerCapita.k)},
      dependencies = Vector("foodPerCapita", "indicatedFoodPerCapita")
    )

  val landDevelopmentRate =
    Rate(
      "landDevelopmentRate",
      96,
      units = "hectares per year",
      updateFn = () => lift {unlift(totalAgriculturalInvestment.k) * unlift(fractionOfInputsAllocatedToLandDevelopment.k) / unlift(developmentCostPerHectare.k)}
  )

  val developmentCostPerHectare = Table(
    qName = "developmentCostPerHectare",
    qNumber = 97,
    data = Vector(100000, 7400, 5200, 3500, 2400, 1500, 750, 300, 150, 75, 50), iMin = 0, iMax = 1.0, iDelta = 0.1, units = "dollars per hectare",
    updateFn = () => lift {unlift(potentiallyArableLand.k) / Constants.potentiallyArableLandTotal}
  )


  // Loop 2: Food from Investment in Agricultural Inputs

  val currentAgriculturalInputs =
    Aux(
      "currentAgriculturalInputs",
      98,
      units = "dollars per year",
      dependencies = Vector("totalAgriculturalInvestment", "fractionOfInputsAllocatedToLandDevelopment"),
      updateFn = () => lift {unlift(totalAgriculturalInvestment.k) * (1 - unlift(fractionOfInputsAllocatedToLandDevelopment.k))}
    )

  val agriculturalInputs =
    Smooth("agriculturalInputs", 99, Constants.averageLifetimeOfAgriculturalInputsK,
      units = "dollars per year",
      dependencies = Vector(),   // "currentAgriculturalInputs" removed to break cycle
      initFn = () => { currentAgriculturalInputs },
      initVal = Some(5.0e9)
    )

  val averageLifetimeOfAgriculturalInputs =
    Aux(
      "averageLifetimeOfAgriculturalInputs",
      100,
      units = "years",
      updateFn = () => clip(Some(2.0), Some(2.0) , Some(t), Some(policyYear))
  )

  val agriculturalInputsPerHectare: Aux =
    Aux("agriculturalInputsPerHectare", 101,
      units = "dollars per hectare-year",
      dependencies = Vector("agriculturalInputs", "fractionOfInputsAllocatedToLandMaintenance"),
      updateFn = () => lift {unlift(agriculturalInputs.k) * (1 - unlift(fractionOfInputsAllocatedToLandMaintenance.k)) / unlift(arableLand.k)}
    )

  val landYieldMultiplierFromCapital =
    Table(
      "landYieldMultiplierFromCapital", 102,
      data=Vector(1, 3, 3.8, 4.4, 4.9, 5.4, 5.7, 6, 6.3, 6.6, 6.9, 7.2, 7.4, 7.6, 7.8, 8, 8.2, 8.4, 8.6, 8.8, 9, 9.2, 9.4, 9.6, 9.8, 10),
      iMin = 0, iMax = 1000, iDelta = 40,
      dependencies = Vector("agriculturalInputsPerHectare"),
      updateFn = () => agriculturalInputsPerHectare.k
    )

  val landYield:Aux =
    Aux(
      "landYield",
      103,
      units = "kilograms per hectare-year", dependencies = Vector("landYieldFactor", "landYieldMultiplierFromCapital", "landYieldMultiplierFromAirPollution"),
      updateFn = () => lift {unlift(landYieldFactor.k) * unlift(landFertility.k) * unlift(landYieldMultiplierFromCapital.k) * unlift(landYieldMultiplierFromAirPollution.k)}
    )

  val landYieldFactor =
    Aux(
      "landYieldFactor",
      104,
      updateFn = () => clip(Some(1.0), Some(1.0), Some(t), Some(policyYear))
    )

  val landYieldMultiplierFromAirPollution = Aux(
    "landYieldMultiplierFromAirPollution", 105,
    dependencies = Vector("landYieldMultiplierFromAirPollutionBefore", "landYieldMultiplierFromAirPollutionAfter"),
    updateFn = () => clip(landYieldMultiplierFromAirPollutionAfter.k, landYieldMultiplierFromAirPollutionBefore.k, Some(t), Some(policyYear))
  )

  val landYieldMultiplierFromAirPollutionBefore = Table("landYieldMultiplierFromAirPollutionBefore", qNumber = 106,
    data = Vector(1.0, 1.0, 0.7, 0.4), iMin = 0, iMax = 30, iDelta = 10,
    dependencies = Vector("industrialOutput"),
    updateFn = () => lift {unlift(industrialOutput.k) / Constants.industrialOutputValueIn1970}
  )

  val landYieldMultiplierFromAirPollutionAfter = Table("landYieldMultiplierFromAirPollutionAfter", qNumber = 107,
    data = Vector(1.0, 1.0, 0.7, 0.4), iMin = 0, iMax = 30, iDelta = 10,
    dependencies = Vector("industrialOutput"),
    updateFn = () => lift {unlift(industrialOutput.k) / Constants.industrialOutputValueIn1970}
  )


  // Loops 1 and 2: The Investment Allocation Decision
  val fractionOfInputsAllocatedToLandDevelopment = Table(
    "fractionOfInputsAllocatedToLandDevelopment", 108,
    data = Vector(0, 0.05, 0.15, 0.30, 0.50, 0.70, 0.85, 0.95, 1), iMin = 0, iMax = 2, iDelta = 0.25,
    dependencies = Vector("marginalProductivityOfLandDevelopment", "marginalProductivityOfAgriculturalInputs"),
    updateFn = () => lift {unlift(marginalProductivityOfLandDevelopment.k) / unlift(marginalProductivityOfAgriculturalInputs.k)}
  )

  val marginalProductivityOfLandDevelopment = Aux("marginalProductivityOfLandDevelopment", 109,
    units = "kilograms per dollar",
    dependencies = Vector("landYield", "developmentCostPerHectare"),
    updateFn = () => lift { unlift(landYield.k) / (unlift(developmentCostPerHectare.k) * Constants.socialDiscount)}
  )

  val marginalProductivityOfAgriculturalInputs = Aux("marginalProductivityOfAgriculturalInputs", 110,
    units = "kilograms per dollar",
    dependencies = Vector("averageLifetimeOfAgriculturalInputs", "landYield", "marginalLandYieldMultiplierFromCapital", "landYieldMultiplierFromCapital"),
    updateFn = () => lift {
      Constants.averageLifetimeOfAgriculturalInputsK * unlift(landYield.k) *
        (unlift(marginalLandYieldMultiplierFromCapital.k) / unlift(landYieldMultiplierFromCapital.k))}
  )

  val marginalLandYieldMultiplierFromCapital = Table(
    "marginalLandYieldMultiplierFromCapital", 111,
    data = Vector(0.075, 0.03, 0.015, 0.011, 0.009, 0.008, 0.007, 0.006, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005),
    iMin = 0, iMax = 600, iDelta = 40, units = "hectares per dollar",
    dependencies = Vector("agriculturalInputsPerHectare"),
    updateFn = () => agriculturalInputsPerHectare.k
  )

  // Loop 3: Land Erosion and Urban-Industrial Use
  val averageLifeOfLand = Aux("averageLifeOfLand", 112, units = "years", dependencies = Vector("landLifeMultiplierFromYield"),
    updateFn = () => lift {Constants.averageLifeOfLandNormal * unlift(landLifeMultiplierFromYield.k)}
  )

  val landLifeMultiplierFromYield = Aux("landLifeMultiplierFromYield", 113,
    dependencies = Vector("landLifeMultiplierFromYieldBefore", "landLifeMultiplierFromYieldAfter"),
    updateFn = () => clip(landLifeMultiplierFromYieldAfter.k, landLifeMultiplierFromYieldBefore.k, Some(t), Some(policyYear))
  )

  val landLifeMultiplierFromYieldBefore = Table("landLifeMultiplierFromYieldBefore", qNumber = 114,
    data = Vector(1.2, 1.0, 0.63, 0.36, 0.16, 0.055, 0.04, 0.025, 0.015, 0.01),
    iMin = 0, iMax = 9, iDelta = 1,
    dependencies = Vector("landYield"),
    updateFn = () => lift {unlift(landYield.k) / Constants.inherentLandFertilityK}
  )

  val landLifeMultiplierFromYieldAfter = Table("landLifeMultiplierFromYieldAfter", qNumber = 115,
    data = Vector(1.2, 1.0, 0.63, 0.36, 0.16, 0.055, 0.04, 0.025, 0.015, 0.01),
    iMin = 0, iMax = 9, iDelta = 1,
    dependencies = Vector("landYield"),
    updateFn = () => lift {unlift(landYield.k) / Constants.inherentLandFertilityK}
  )

  val landErosionRate =
    Rate("landErosionRate", 116,
      units = "hectares per year",
      updateFn = () => lift { unlift(arableLand.k) / unlift(averageLifeOfLand.k) }
    )

  // 2016-08-09: Neil S. Grant reported an error in the table of values
  // for urbanIndustrialLandPerCapita. The third element of the array
  // should be 0.015, not 0.15. Corrected.
  val urbanIndustrialLandPerCapita = Table(
    "urbanIndustrialLandPerCapita", 117,
    data = Vector(0.005, 0.008, 0.015, 0.025, 0.04, 0.055, 0.07, 0.08, 0.09),
    iMin = 0, iMax = 1600, iDelta = 200,
    units = "hectares per person",
    dependencies = Vector("industrialOutputPerCapita"),
    updateFn = () => industrialOutputPerCapita.k
  )

  val urbanIndustrialLandRequired = Aux("urbanIndustrialLandRequired", 118,
    units = "hectares",
    dependencies = Vector("urbanIndustrialLandPerCapita", "population"),
    updateFn = () => lift {unlift(urbanIndustrialLandPerCapita.k) * unlift(population.k)}
  )

  val landRemovalForUrbanIndustrialUse = Rate("landRemovalForUrbanIndustrialUse", 119, units = "hectares per year",
    updateFn = () => lift {math.max(0, (unlift(urbanIndustrialLandRequired.k) - unlift(urbanIndustrialLand.k)) / Constants.developmentTime)}
  )

  val urbanIndustrialLand: Level = Level(
    "urbanIndustrialLand", 120,
    initVal = 8.2e6,
    units = "hectares",
    updateFn = () => lift {unlift(urbanIndustrialLand.j) + dt * unlift(landRemovalForUrbanIndustrialUse.j)}
  )


  // Loop 4: Land fertility degradation
  val landFertility:Level = Level(
    qName="landFertility",
    qNumber = 121,
    initVal = 600,
    units = "kilograms per hectare-year",
    updateFn = ()=> lift {unlift(landFertility.j) + dt * (unlift(landFertilityRegeneration.j) - unlift(landFertilityDegradation.j))}
  )

  val landFertilityDegradationRate = Table(
    qName="landFertilityDegradationRate", qNumber=122, data=Vector(0, 0.1, 0.3, 0.5), iMin=0, iMax = 30, iDelta = 10,
    units = "inverse years",
    dependencies = Vector("indexOfPersistentPollution"),
    updateFn = () => indexOfPersistentPollution.k
    )

  val landFertilityDegradation = Rate(
    qName="landFertilityDegradation",
    qNumber=123,
    units = "kilograms per hectare-year-year",
    updateFn = () => lift {unlift(landFertility.k) * unlift(landFertilityDegradationRate.k)}
  )

  // Loop 5: Land fertility regeneration
  val landFertilityRegeneration = Rate(
    qName="landFertilityRegeneration",
    qNumber=124,
    units = "kilograms per hectare-year-year",
    updateFn = () => lift {(Constants.inherentLandFertilityK - unlift(landFertility.k)) / unlift(landFertilityRegenerationTime.k)}
  )

  val landFertilityRegenerationTime = Table(
    qName= "landFertilityRegenerationTime", qNumber=125,
    data = Vector(20, 13, 8, 4, 2, 2), iMin=0, iMax=0.1, iDelta=0.02,
    units = "years",
    dependencies = Vector("fractionOfInputsAllocatedToLandMaintenance"),
    updateFn = () => fractionOfInputsAllocatedToLandMaintenance.k
  )


  // Loop 6: Discontinuing land maintenance
  val fractionOfInputsAllocatedToLandMaintenance = Table(
    qName = "fractionOfInputsAllocatedToLandMaintenance",
    qNumber = 126,
    data = Vector(0, 0.04, 0.07, 0.09, 0.10),
    iMin=0, iMax=4, iDelta=1,
    dependencies = Vector("perceivedFoodRatio"),
    updateFn = () => perceivedFoodRatio.k
  )

  val foodRatio = Aux(
    qName = "foodRatio",
    qNumber= 127,
    dependencies = Vector("foodPerCapita"),
    updateFn = () => lift {unlift(foodPerCapita.k)/Constants.subsistenceFoodPerCapitaK}
  )

  val perceivedFoodRatio = Smooth(
    qName = "perceivedFoodRatio",
    qNumber = 128,
    delay= Constants.foodShortagePerceptionDelayK,
    initVal = Some(1.0),
    initFn = () => foodRatio // ??? CHECK THAT!!!
  )

  // NONRENEWABLE RESOURCE SECTOR
  val nonrenewableResources: Level = Level(
    qName = "nonrenewableResources",
    qNumber = 129,
    initVal = Constants.nonrenewableResourcesInitialK,
    units = "resource units",
    updateFn = ()=> lift {unlift(nonrenewableResources.j) + dt * (-unlift(nonrenewableResourceUsageRate.j))}
  )

  val nonrenewableResourceUsageRate = Rate(
    qName = "nonrenewableResourceUsageRate",
    qNumber = 130,
    units = "resource units per year",
    updateFn = () => lift {unlift(population.k) * unlift(perCapitaResourceUsageMultiplier.k) * unlift(nonrenewableResourceUsageFactor.k)}
  )

  val nonrenewableResourceUsageFactor = Aux(
    qName = "nonrenewableResourceUsageFactor",
    qNumber = 131,
    updateFn = () => clip(Some(1.0), Some(1.0), Some(t), Some(policyYear)) // ???
  )

  val perCapitaResourceUsageMultiplier = Table(
    qName = "perCapitaResourceUsageMultiplier",qNumber = 132,
    data = Vector(0, 0.85, 2.6, 4.4, 5.4, 6.2, 6.8, 7, 7), iMin = 0, iMax = 1600, iDelta = 200,
    units = "resource units per person-year",
    dependencies = Vector("industrialOutputPerCapita"),
    updateFn = () => industrialOutputPerCapita.k
  )

  val nonrenewableResourceFractionRemaining = Aux(
    qName = "nonrenewableResourceFractionRemaining",
    qNumber = 133,
    updateFn = () => lift {unlift(nonrenewableResources.k) / Constants.nonrenewableResourcesInitialK}
  )

  val fractionOfCapitalAllocatedToObtainingResources = Aux(
    qName = "fractionOfCapitalAllocatedToObtainingResources",
    qNumber = 134,
    dependencies = Vector("fractionOfCapitalAllocatedToObtainingResourcesBefore", "fractionOfCapitalAllocatedToObtainingResourcesAfter"),
    updateFn = () => clip(fractionOfCapitalAllocatedToObtainingResourcesAfter.k, fractionOfCapitalAllocatedToObtainingResourcesBefore.k, Some(t), Some(policyYear))
  )

  val fractionOfCapitalAllocatedToObtainingResourcesBefore = Table(
    qName = "fractionOfCapitalAllocatedToObtainingResourcesBefore",
    qNumber = 135,
    data = Vector(1, 0.9, 0.7, 0.5, 0.2, 0.1, 0.05, 0.05, 0.05, 0.05, 0.05),
    iMin = 0, iMax = 1, iDelta = 0.1,
    dependencies = Vector("nonrenewableResourceFractionRemaining"),
    updateFn = () => nonrenewableResourceFractionRemaining.k
  )

  val fractionOfCapitalAllocatedToObtainingResourcesAfter = Table(
    qName = "fractionOfCapitalAllocatedToObtainingResourcesAfter",
    qNumber = 136,
    data = Vector(1, 0.9, 0.7, 0.5, 0.2, 0.1, 0.05, 0.05, 0.05, 0.05, 0.05),
    iMin = 0, iMax = 1, iDelta = 0.1,
    dependencies = Vector("nonrenewableResourceFractionRemaining"),
    updateFn = () => nonrenewableResourceFractionRemaining.k
  )


  // PERSISTENT POLLUTION SECTOR

  val persistentPollutionGenerationRate = Rate(
    qName = "persistentPollutionGenerationRate",
    qNumber = 137,
    units = "pollution units per year",
    updateFn = () =>
      lift { (unlift(persistentPollutionGeneratedByIndustrialOutput.k) + unlift(persistentPollutionGeneratedByAgriculturalOutput.k)) * unlift(persistentPollutionGenerationFactor.k)}
  )

  val persistentPollutionGenerationFactor = Aux(
    qName = "persistentPollutionGenerationFactor",
    qNumber = 138,
    updateFn = () => clip(Some(1.0),Some(1.0),Some(t),Some(policyYear))
  )

  val persistentPollutionGeneratedByIndustrialOutput = Aux(
    qName = "persistentPollutionGeneratedByIndustrialOutput",
    qNumber = 139,
    units = "pollution units per year",
    dependencies = Vector("perCapitaResourceUsageMultiplier", "population"),
    updateFn = () => lift {unlift(perCapitaResourceUsageMultiplier.k) * unlift(population.k) * Constants.fractionOfResourcesAsPersistentMaterial * Constants.industrialMaterialsEmissionFactor * Constants.industrialMaterialsToxicityIndex}
  )

  val persistentPollutionGeneratedByAgriculturalOutput = Aux(
    qName = "persistentPollutionGeneratedByAgriculturalOutput",
    qNumber = 140,
    units = "pollution units per year",
    dependencies = Vector("agriculturalInputsPerHectare"),
    updateFn = () => lift {unlift(agriculturalInputsPerHectare.k) * unlift(arableLand.k) * Constants.fractionOfInputsAsPersistentMaterial * Constants.agriculturalMaterialsToxicityIndex}
  )

  val persistentPollutionAppearanceRate =
    Delay3(
      qName = "persistentPollutionAppearanceRate",
      qNumber = 141,
      delay = Constants.persistentPollutionTransmissionDelayK,
      units = "pollution units per year",
      initFn = () => { persistentPollutionGenerationRate }
    )

  val persistentPollution: Level =
    Level(
      qName = "persistentPollution",
      qNumber = 142,
      initVal = 2.5e7,
      units = "pollution units",
      updateFn = () => lift {unlift(persistentPollution.j) + dt * (unlift(persistentPollutionAppearanceRate.j) - unlift(persistenPollutionAssimilationRate.j))}
    )

  val indexOfPersistentPollution = Aux(
    qName = "indexOfPersistentPollution",
    qNumber = 143,
    updateFn = () => lift {unlift(persistentPollution.k) / Constants.pollutionValueIn1970}
  )

  val persistenPollutionAssimilationRate = Rate(
    qName = "persistenPollutionAssimilationRate",
    qNumber = 144,
    units = "pollution units per year",
    updateFn = () => lift {unlift(persistentPollution.k) / (unlift(assimilationHalfLife.k) * 1.4)}
  )

  val assimilationHalfLifeMultiplier = Table(
    qName = "assimilationHalfLifeMultiplier",
    qNumber = 145,
    units = "years",
    dependencies = Vector("indexOfPersistentPollution"),
    updateFn = () => indexOfPersistentPollution.k,
    data = Vector(1, 11, 21, 31, 41),
    iMin = 1,
    iMax = 1001,
    iDelta = 250
  )

  val assimilationHalfLife = Aux(
    qName = "assimilationHalfLife",
    qNumber = 146,
    units = "years",
    dependencies = Vector("assimilationHalfLifeMultiplier"),
    updateFn = () => lift {unlift(assimilationHalfLifeMultiplier.k) * Constants.assimilationHalfLifeValueIn1970}
  )

  // SUPPLEMENTARY EQUATIONS
  val fractionOfOutputInAgriculture = Aux(
      qName = "fractionOfOutputInAgriculture",
      qNumber = 147,
      dependencies = Vector("food", "serviceOutput", "industrialOutput"),
      updateFn = () => lift {0.22 * unlift(food.k) / ((0.22 * unlift(food.k)) + unlift(serviceOutput.k) + unlift(industrialOutput.k))}
    )

  val fractionOfOutputInIndustry = Aux(
    qName = "fractionOfOutputInIndustry",
    qNumber = 148,
    dependencies = Vector("food", "serviceOutput", "industrialOutput"),
    updateFn = () => lift {unlift(industrialOutput.k) / (0.22 * unlift(food.k) + unlift(serviceOutput.k) + unlift(industrialOutput.k))}
  )

  val fractionOfOutputInServices = Aux(
    qName = "fractionOfOutputInServices",
    qNumber = 149,
    dependencies = Vector("food", "serviceOutput", "industrialOutput"),
    updateFn = () => lift {unlift(serviceOutput.k) / (0.22 * unlift(food.k) + unlift(serviceOutput.k) + unlift(industrialOutput.k))}
  )

  val auxSequence = Vector[All](
    population,
    deathsPerYear,
    lifetimeMultiplierFromCrowding,
    industrialCapitalOutputRatio,
    averageLifetimeOfIndustrialCapital,
    averageLifetimeOfServiceCapital,
    serviceCapitalOutputRatio,
    laborForce,
    landFractionCultivated,
    developmentCostPerHectare,
    landYieldFactor,
    nonrenewableResourceUsageFactor,
    nonrenewableResourceFractionRemaining,
    persistentPollutionGenerationFactor,
    indexOfPersistentPollution,
    fractionOfIndustrialOutputAllocatedToConsumptionConstant,
    averageLifetimeOfAgriculturalInputs,
    laborUtilizationFractionDelayed,
    agriculturalInputs,
    perceivedFoodRatio,
    fractionOfPopulationUrban,
    crudeDeathRate,
    crudeBirthRate,
    fractionOfCapitalAllocatedToObtainingResourcesBefore,
    fractionOfCapitalAllocatedToObtainingResourcesAfter,
    fractionOfCapitalAllocatedToObtainingResources,
    lifetimeMultiplierFromPollution,
    landFertilityDegradationRate,
    capitalUtilizationFraction,
    industrialOutput,
    industrialOutputPerCapita,
    delayedIndustrialOutputPerCapita,
    socialFamilySizeNorm,
    averageIndustrialOutputPerCapita,
    familyIncomeExpectation,
    familyResponseToSocialNorm,
    desiredCompletedFamilySize,
    crowdingMultiplierFromIndustrialization,
    indicatedServiceOutputPerCapitaBefore,
    indicatedServiceOutputPerCapitaAfter,
    indicatedServiceOutputPerCapita,
    fractionOfIndustrialOutputAllocatedToConsumptionVariable,
    fractionOfIndustrialOutputAllocatedToConsumption,
    jobsPerIndustrialCapitalUnit,
    potentialJobsInIndustrialSector,
    serviceOutput,
    serviceOutputPerCapita,
    fractionOfIndustrialOutputAllocatedToServicesBefore,
    fractionOfIndustrialOutputAllocatedToServicesAfter,
    fractionOfIndustrialOutputAllocatedToServices,
    jobsPerServiceCapitalUnit,
    potentialJobsInServiceSector,
    healthServicesAllocationsPerCapita,
    effectiveHealthServicesPerCapita,
    lifetimeMultiplierFromHealthServicesBefore,
    lifetimeMultiplierFromHealthServicesAfter,
    lifetimeMultiplierFromHealthServices,
    fractionOfInputsAllocatedToLandMaintenance,
    agriculturalInputsPerHectare,
    jobsPerHectare,
    potentialJobsInAgriculturalSector,
    jobs,
    laborUtilizationFraction,
    landYieldMultiplierFromCapital,
    landYieldMultiplierFromAirPollutionBefore,
    landYieldMultiplierFromAirPollutionAfter,
    landYieldMultiplierFromAirPollution,
    landYield,
    marginalProductivityOfLandDevelopment,
    marginalLandYieldMultiplierFromCapital,
    marginalProductivityOfAgriculturalInputs,
    fractionOfInputsAllocatedToLandDevelopment,
    food,
    foodPerCapita,
    indicatedFoodPerCapitaBefore,
    indicatedFoodPerCapitaAfter,
    indicatedFoodPerCapita,
    fractionOfIndustrialOutputAllocatedToAgricultureBefore,
    fractionOfIndustrialOutputAllocatedToAgricultureAfter,
    fractionOfIndustrialOutputAllocatedToAgriculture,
    totalAgriculturalInvestment,
    currentAgriculturalInputs,
    foodRatio,
    landFertilityRegenerationTime,
    lifetimeMultiplierFromFood,
    lifeExpectancy,
    mortality0To14,
    mortality15To44,
    mortality45To64,
    mortality65AndOver,
    fecundityMultiplier,
    perceivedLifeExpectancy,
    compensatoryMultiplierFromPerceivedLifeExpectancy,
    maxTotalFertility,
    desiredTotalFertility,
    needForFertilityControl,
    fractionOfServicesAllocatedToFertilityControl,
    fertilityControlAllocationPerCapita,
    fertilityControlFacilitiesPerCapita,
    fertilityControlEffectiveness,
    totalFertility,
    landLifeMultiplierFromYieldBefore,
    landLifeMultiplierFromYieldAfter,
    landLifeMultiplierFromYield,
    averageLifeOfLand,
    urbanIndustrialLandPerCapita,
    urbanIndustrialLandRequired,
    perCapitaResourceUsageMultiplier,
    persistentPollutionGeneratedByIndustrialOutput,
    persistentPollutionGeneratedByAgriculturalOutput,
    assimilationHalfLifeMultiplier,
    assimilationHalfLife,
    fractionOfIndustrialOutputAllocatedToIndustry,
    fractionOfOutputInAgriculture,
    fractionOfOutputInIndustry,
    fractionOfOutputInServices)


  val levels = Vector(
    population0To14,
    population15To44,
    population45To64,
    population65AndOver,
    industrialCapital,
    serviceCapital,
    arableLand,
    potentiallyArableLand,
    urbanIndustrialLand,
    landFertility,
    nonrenewableResources,
    persistentPollution
  )


  val rates = Vector(
    deathsPerYear0To14,
    maturationsPerYear14to15,
    deathsPerYear15To44,
    maturationsPerYear44to45,
    deathsPerYear45To64,
    maturationsPerYear64to65,
    deathsPerYear65AndOver,
    birthsPerYear,
    industrialCapitalDepreciationRate,
    industrialCapitalInvestmentRate,
    serviceCapitalInvestmentRate,
    serviceCapitalDepreciationRate,
    landDevelopmentRate,
    landErosionRate,
    landRemovalForUrbanIndustrialUse,
    landFertilityDegradation,
    landFertilityRegeneration,
    nonrenewableResourceUsageRate,
    persistentPollutionGenerationRate,
    persistentPollutionAppearanceRate,
    persistenPollutionAssimilationRate
  )


  val all = Vector(
    population,
    population0To14,
    deathsPerYear0To14,
    mortality0To14,
    maturationsPerYear14to15,
    population15To44,
    deathsPerYear15To44,
    mortality15To44,
    maturationsPerYear44to45,
    population45To64,
    deathsPerYear45To64,
    mortality45To64,
    maturationsPerYear64to65,
    population65AndOver,
    deathsPerYear65AndOver,
    mortality65AndOver,
    deathsPerYear,
    crudeDeathRate,
    lifeExpectancy,
    lifetimeMultiplierFromFood,
    healthServicesAllocationsPerCapita,
    effectiveHealthServicesPerCapita,
    lifetimeMultiplierFromHealthServices,
    lifetimeMultiplierFromHealthServicesBefore,
    lifetimeMultiplierFromHealthServicesAfter,
    fractionOfPopulationUrban,
    crowdingMultiplierFromIndustrialization,
    lifetimeMultiplierFromCrowding,
    lifetimeMultiplierFromPollution,
    birthsPerYear,
    crudeBirthRate,
    totalFertility,
    maxTotalFertility,
    fecundityMultiplier,
    desiredTotalFertility,
    compensatoryMultiplierFromPerceivedLifeExpectancy,
    perceivedLifeExpectancy,
    desiredCompletedFamilySize,
    socialFamilySizeNorm,
    delayedIndustrialOutputPerCapita,
    familyResponseToSocialNorm,
    familyIncomeExpectation,
    averageIndustrialOutputPerCapita,
    needForFertilityControl,
    fertilityControlEffectiveness,
    fertilityControlFacilitiesPerCapita,
    fertilityControlAllocationPerCapita,
    fractionOfServicesAllocatedToFertilityControl,
    industrialOutputPerCapita,
    industrialOutput,
    industrialCapitalOutputRatio,
    industrialCapital,
    industrialCapitalDepreciationRate,
    averageLifetimeOfIndustrialCapital,
    industrialCapitalInvestmentRate,
    fractionOfIndustrialOutputAllocatedToIndustry,
    fractionOfIndustrialOutputAllocatedToConsumption,
    fractionOfIndustrialOutputAllocatedToConsumptionConstant,
    fractionOfIndustrialOutputAllocatedToConsumptionVariable,
    indicatedServiceOutputPerCapita,
    indicatedServiceOutputPerCapitaBefore,
    indicatedServiceOutputPerCapitaAfter,
    fractionOfIndustrialOutputAllocatedToServices,
    fractionOfIndustrialOutputAllocatedToServicesBefore,
    fractionOfIndustrialOutputAllocatedToServicesAfter,
    serviceCapitalInvestmentRate,
    serviceCapital,
    serviceCapitalDepreciationRate,
    averageLifetimeOfServiceCapital,
    serviceOutput,
    serviceOutputPerCapita,
    serviceCapitalOutputRatio,
    jobs,
    potentialJobsInIndustrialSector,
    jobsPerIndustrialCapitalUnit,
    potentialJobsInServiceSector,
    jobsPerServiceCapitalUnit,
    potentialJobsInAgriculturalSector,
    jobsPerHectare,
    laborForce,
    laborUtilizationFraction,
    laborUtilizationFractionDelayed,
    capitalUtilizationFraction,
    landFractionCultivated,
    arableLand,
    potentiallyArableLand,
    food,
    foodPerCapita,
    indicatedFoodPerCapita,
    indicatedFoodPerCapitaBefore,
    indicatedFoodPerCapitaAfter,
    totalAgriculturalInvestment,
    fractionOfIndustrialOutputAllocatedToAgriculture,
    fractionOfIndustrialOutputAllocatedToAgricultureBefore,
    fractionOfIndustrialOutputAllocatedToAgricultureAfter,
    landDevelopmentRate,
    developmentCostPerHectare,
    currentAgriculturalInputs,
    agriculturalInputs,
    averageLifetimeOfAgriculturalInputs,
    agriculturalInputsPerHectare,
    landYieldMultiplierFromCapital,
    landYield,
    landYieldFactor,
    landYieldMultiplierFromAirPollution,
    landYieldMultiplierFromAirPollutionBefore,
    landYieldMultiplierFromAirPollutionAfter,
    fractionOfInputsAllocatedToLandDevelopment,
    marginalProductivityOfLandDevelopment,
    marginalProductivityOfAgriculturalInputs,
    marginalLandYieldMultiplierFromCapital,
    averageLifeOfLand,
    landLifeMultiplierFromYield,
    landLifeMultiplierFromYieldBefore,
    landLifeMultiplierFromYieldAfter,
    landErosionRate,
    urbanIndustrialLandPerCapita,
    urbanIndustrialLandRequired,
    landRemovalForUrbanIndustrialUse,
    urbanIndustrialLand,
    landFertility,
    landFertilityDegradationRate,
    landFertilityDegradation,
    landFertilityRegeneration,
    landFertilityRegenerationTime,
    fractionOfInputsAllocatedToLandMaintenance,
    foodRatio,
    perceivedFoodRatio,
    nonrenewableResources,
    nonrenewableResourceUsageRate,
    nonrenewableResourceUsageFactor,
    perCapitaResourceUsageMultiplier,
    nonrenewableResourceFractionRemaining,
    fractionOfCapitalAllocatedToObtainingResources,
    fractionOfCapitalAllocatedToObtainingResourcesBefore,
    fractionOfCapitalAllocatedToObtainingResourcesAfter,
    persistentPollutionGenerationRate,
    persistentPollutionGenerationFactor,
    persistentPollutionGeneratedByIndustrialOutput,
    persistentPollutionGeneratedByAgriculturalOutput,
    persistentPollutionAppearanceRate,
    persistentPollution,
    indexOfPersistentPollution,
    persistenPollutionAssimilationRate,
    assimilationHalfLifeMultiplier,
    assimilationHalfLife,
    fractionOfOutputInAgriculture,
    fractionOfOutputInIndustry,
    fractionOfOutputInServices
  )

}
