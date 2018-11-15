package world3

import better.files._
import io.monadless.stdlib.MonadlessOption._
import Box._
import monocle.macros._
import scala.collection.mutable.ListBuffer
import monocle._

object Run extends App {

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

  val s1 = Constants()
//  val s2 = Constants(nonrenewableResourcesInitialK = 2.0e12)
  //val s3 = Constants(nonrenewableResourcesInitialK = 2.0e12, persistentpollutiontech)

  val w3 = new World3(s1)
  val result = w3.run()

  val writer = File("/tmp/results.csv").newFileWriter()
  writer.write(csv(result))
  writer.close()
  //  check(result)
  /*
  val graphWriter = File("graph.dot").newFileWriter()
  graphWriter.write("digraph G {\n")
  graphWriter.write(w3.graph())
  graphWriter.write("}")
  graphWriter.close()
  */
}

object World3 {

  import Constants._

  def parameters = Vector(
    lifeExpectancyNormal,
    subsistenceFoodPerCapitaK,
    effectiveHealthServicesPerCapitaImpactDelay,
    potentiallyArableLandTotal,
    industrialOutputValueIn1970,
    averageLifetimeOfAgriculturalInputsK,
    socialDiscount,
    averageLifeOfLandNormal,
    inherentLandFertilityK,
    developmentTime,
    foodShortagePerceptionDelayK,
    nonrenewableResourcesInitialK,
    persistentPollutionGenerationFactorBefore,
    technologyDevelopmentDelay,
    fractionOfResourcesAsPersistentMaterial,
    industrialMaterialsEmissionFactor,
    industrialMaterialsToxicityIndex,
    persistentPollutionTransmissionDelayK,
    fractionOfInputsAsPersistentMaterial,
    agriculturalMaterialsToxicityIndex,
    pollutionValueIn1970,
    lifetimeMultiplierFromHealthServicesPolicyYear,
    birthsPerYearReproductiveLifetime,
    birthsPerYearPopulationEquilibriumTime,
    maxTotalFertilityNormal,
    lifetimePerceptionDelayK,
    desiredCompletedFamilySizeNormal,
    zeroPopulationGrowthTargetYear,
    assimilationHalfLifeValueIn1970,
    socialAdjustmentDelayK,
    incomeExpectationAveragingTimeK,
    healthServicesImpactDelayK,
    industrialCapitalOutputRatioBefore,
    industrialCapitalOutputRatioAfter,
    averageLifetimeOfIndustrialCapitalBefore,
    averageLifetimeOfIndustrialCapitalAfter,

    fractionOfIndustrialOutputAllocatedToConsumptionIndustrialEquilibriumTime,
    fractionOfIndustrialOutputAllocatedToConsumptionConstantBefore,
    fractionOfIndustrialOutputAllocatedToConsumptionConstantAfter,
    fractionOfIndustrialOutputAllocatedToConsumptionVariableIndustrialOutputPerCapitaDesired,

    averageLifetimeOfServiceCapitalBefore,
    averageLifetimeOfServiceCapitalAfter,

    serviceCapitalOutputRatioBefore,
    serviceCapitalOutputRatioAfter,

    laborForceParticipationFraction,
    laborUtilizationFractionDelayedDelayTime,

    landFractionCultivatedPotentiallyArableLandTotal,

    foodLandFractionHarvestedK,
    foodProcessingLossK
  ).map { l => (f: Double => Double, c: Constants) => l.modify(f)(c) }

}


@Lenses case class Constants(
  lifeExpectancyNormal: Double = 32.0, // used in eqn 19,
  subsistenceFoodPerCapitaK: Double = 230.0, // kilograms per person-year, used in eqns 20, 127
  effectiveHealthServicesPerCapitaImpactDelay: Double = 20.0, // years, used in eqn 22
  potentiallyArableLandTotal: Double = 3.2e9,   // hectares, used here and in eqn 97
  industrialOutputValueIn1970: Double = 7.9e11, // for eqns 106 and 107
  averageLifetimeOfAgriculturalInputsK: Double = 2.0, // years, eqn 99 (in lieu of 100)
  socialDiscount: Double = 0.07, // eqn 109
  averageLifeOfLandNormal: Double = 6000.0, // years, used in eqn 112
  inherentLandFertilityK: Double = 600.0, // kilograms per hectare-year, used in eqns 114, 115 and 124
  developmentTime: Double = 10.0, // years, used in eqn 119
  foodShortagePerceptionDelayK: Double = 2.0, // years, used in eqn 128
  nonrenewableResourcesInitialK: Double = 1.0e12, // resource units, used in eqns 129 and 133
  persistentPollutionGenerationFactorBefore: Double = 1.0, // Persistent pollution generation factor before policy, used in eqn 138.1 (PPGF1#138.1)
  technologyDevelopmentDelay: Double = 20.0, //technology development delay, used in eqn 138.1
  desiredFoodRatio: Double = 2.0, //desired food ratio (DFR#--)
  fractionOfResourcesAsPersistentMaterial: Double = 0.02, // dimensionless, used in eqn 139
  industrialMaterialsEmissionFactor: Double = 0.1, // dimensionless, used in eqn 139
  industrialMaterialsToxicityIndex: Double = 10.0, // pollution units per resource unit, used in eqn 139
  persistentPollutionTransmissionDelayK: Double = 20.0, // years, used in eqn 141
  fractionOfInputsAsPersistentMaterial: Double = 0.001, // dimensionless, used in eqn 141
  agriculturalMaterialsToxicityIndex: Double = 1.0, // pollution units per dollar, used in eqn 141
  pollutionValueIn1970: Double = 1.36e8, // pollution units, used in eqn 143
  lifetimeMultiplierFromHealthServicesPolicyYear: Double = 1940.0,
  birthsPerYearReproductiveLifetime: Double = 30.0, // years
  birthsPerYearPopulationEquilibriumTime: Double = 4000.0, // year
  maxTotalFertilityNormal: Double = 12.0, // dimensionless
  lifetimePerceptionDelayK: Double = 20.0, // years, used in eqn 37
  desiredCompletedFamilySizeNormal: Double = 4.0,
  zeroPopulationGrowthTargetYear: Double = 4000.0,
  assimilationHalfLifeValueIn1970: Double = 1.5, // years, used in eqn 146
  socialAdjustmentDelayK: Double = 20.0, // years, used in eqn 40
  incomeExpectationAveragingTimeK: Double = 3.0, // years, used in eqn 43
  healthServicesImpactDelayK: Double = 20.0, // years, for eqn 46
  industrialCapitalOutputRatioBefore: Double = 3.0,
  industrialCapitalOutputRatioAfter: Double = 3.0,
  averageLifetimeOfIndustrialCapitalBefore: Double = 14.0,
  averageLifetimeOfIndustrialCapitalAfter: Double = 14.0,

  fractionOfIndustrialOutputAllocatedToConsumptionIndustrialEquilibriumTime: Double = 4000.0,  // year
  fractionOfIndustrialOutputAllocatedToConsumptionConstantBefore: Double = 0.43,
  fractionOfIndustrialOutputAllocatedToConsumptionConstantAfter: Double = 0.43,
  fractionOfIndustrialOutputAllocatedToConsumptionVariableIndustrialOutputPerCapitaDesired: Double = 400.0,

  averageLifetimeOfServiceCapitalBefore: Double = 20.0, // years
  averageLifetimeOfServiceCapitalAfter: Double = 20.0, // years

  serviceCapitalOutputRatioBefore: Double = 1.0,
  serviceCapitalOutputRatioAfter: Double = 1.0,

  laborForceParticipationFraction: Double = 0.75,  // dimensionless
  laborUtilizationFractionDelayedDelayTime: Double = 2.0, // years, eqn 82

  initialArableLand: Double = 0.9e9, // hectare, The initial amount of land that is arable. (ALI#85.2)
  initialPotentiallyArableLand: Double = 2.3e9, //The initial amount of potentially arable land (PALI#86.2).
  landFractionCultivatedPotentiallyArableLandTotal: Double = 3.2e9,  // hectares, used here and in eqn 97

  foodLandFractionHarvestedK: Double = 0.7,   // dimensionless
  foodProcessingLossK: Double = 0.1,          // dimensionless

  desiredPersistentPollutionIndex : Double = 1.2, //Desires persistent pollution index (DPOLX#--). dimensionless
  initialPersistentPollutionTechnology: Double = 1.0, // initial value for Pollution control technology initiated (PTD#--)
  initialServiceCapital: Double = 1.44e11, //The initial level of service capital (SCI#67.2)

  initialPopulation0To14: Double = 6.5e8, //The initial number of people aged 0 to 14 (P1I#2.2).
  initialPopulation15To44:Double = 7e8, //The initial number of people aged 15 to 44 (P2I#6.2).
  initialPopulation45To64:Double = 1.9e8, //The initial number of people aged 45 to 64 (P3I#10.2).
  initialPopulation65Plus:Double = 6e7, //The initial number of people aged 65 and over (P4I#14.2)

  initialIndustrialCapital:Double = 2.1e11, //INDUSTRIAL CAPITAL INITIAL (ICI#52.1).

  initialAgriculturalInputs:Double = 5.0e9,//initial value for CURRENT AGRICULTURAL INPUTS (CAI#98).
  averageLifetimeOfAgriculturalInputsBeforePolicy: Double = 2.0, //The average life of agricultural inputs before policy time (ALAI1#100.1)
  averageLifetimeOfAgriculturalInputsAfterPolicy: Double = 2.0, //The average life of agricultural inputs after policy time (ALAI2#100.2)
  landYieldFactorBeforePolicy: Double = 1.0, //Land yield factor before policy year (LYF1#104.1).

  initialUrbanIndustrialLand: Double = 8.2e6, //URBAN-INDUSTRIAL LAND INITIAL (UILI#120.1).
  initialLandFertility: Double = 600, //LAND FERTILITY INITIAL (LFERTI#121.2)
)

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

class World3(constants: Constants) {

  def graph(nodes: Vector[All] = all): String = nodes.map(n=>if (n.dependencies.isEmpty) "" else n.qName + "-> {" + n.dependencies.mkString(" ") + "}").mkString("\n")

  /*  Limits to Growth: This is a re-implementation in JavaScript
    of World3, the social-economic-environmental model created by
    Dennis and Donella Meadows and others circa 1970. The results
    of the modeling exercise were published in The Limits to Growth
    in 1972, and the model itself was more fully documented in
    Dynamics of Growth in a Finite World in 1974. */

  def clip(a: Option[Double], b: Option[Double], x: Option[Double], y: Option[Double]): Option[Double] =
    lift{ if (unlift(x) >= unlift(y)) unlift(a) else unlift(b) }



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

  def updateAuxen() = auxSequence.foreach(_.update(dt))
  def updateRates() = rates.foreach(_.update(dt))
  def updateLevels() =  levels.foreach(_.update(dt))

  def warmupAuxen() = auxSequence.foreach(_.warmup(dt))
  def warmupRates() = rates.foreach(_.warmup(dt))
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

  def init() = {
    resetModel()
    initModel()

    for (_ <- 1 to 100) {
      warmupAuxen()
      warmupRates()
      tock()
    }

    assert(all.forall(_.k.isDefined))
  }

  def exec() = {
    val result = ListBuffer[StepValues]()

    while (t <= stopTime) {
      result += StepValues(t, population.k.get, nonrenewableResourceFractionRemaining.k.get, foodPerCapita.k.get, industrialOutputPerCapita.k.get, serviceOutput.k.get, indexOfPersistentPollution.k.get, lifeExpectancy.k.get, crudeBirthRate.k.get, crudeDeathRate.k.get, arableLand.k.get, landYield.k.get)
      timeStep()
    }

    result.toVector
  }

  def run() = {
    init()
    exec()
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
      initVal = constants.initialPopulation0To14,
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
      data = Vector((20,0.0567),(30,0.0366),(40,0.0243),(50,0.0155),(60,0.0082),(70,0.0023),(80,0.001)),
//      data = Vector(0.0567, 0.0366, 0.0243, 0.0155, 0.0082, 0.0023, 0.0010),
//      iMin = 20,
//      iMax = 80,
//      iDelta = 10,
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
      initVal = constants.initialPopulation15To44,
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
      data = Vector((20,0.0266),(30,0.0171),(40,0.011),(50,0.0065),(60,0.004),(70,0.0016),(80,0.0008)),
//      Vector(0.0266, 0.0171, 0.0110, 0.0065, 0.0040, 0.0016, 0.0008),
//      20,
//      80,
//      10,
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
      initVal = constants.initialPopulation45To64,
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
      Vector((20,0.0562),(30,0.0373),(40,0.0252),(50,0.0171),(60,0.0118),(70,0.0083),(80,0.006)),
//      Vector(0.0562, 0.0373, 0.0252, 0.0171, 0.0118, 0.0083, 0.0060),
//      20,
//      80,
//      10,
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
      initVal = constants.initialPopulation65Plus,
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
      Vector((20,0.13),(30,0.11),(40,0.09),(50,0.07),(60,0.06),(70,0.05),(80,0.04)),
//      Vector(0.13, 0.11, 0.09, 0.07, 0.06, 0.05, 0.04),
//      20,
//      80,
//      10,
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
       constants.lifeExpectancyNormal *
        unlift(lifetimeMultiplierFromFood.k) *
        unlift(lifetimeMultiplierFromHealthServices.k) *
        unlift(lifetimeMultiplierFromPollution.k) *
        unlift(lifetimeMultiplierFromCrowding.k) }
    )

  val lifetimeMultiplierFromFood =
    Table(
      "lifetimeMultiplierFromFood",
      20,
//      data = Vector((0,0),(1,1),(2,1.43),(3,1.5),(4,1.5),(5,1.5)),//TODO : check that : tables very different
      data = Vector((0,0),(1,1),(2,1.2),(3,1.3),(4,1.35),(5,1.4)),//TODO : check that : tables very different
//      Vector(0, 1, 1.2, 1.3, 1.35, 1.4),
//      0,
//      5,
//      1,
      dependencies = Vector("foodPerCapita"),
      updateFn = () => lift { unlift(foodPerCapita.k) / constants.subsistenceFoodPerCapitaK }
    )

  val healthServicesAllocationsPerCapita =
    Table(
      "healthServicesAllocationsPerCapita",
      21,
      data = Vector((0,0),(250,20),(500,50),(750,95),(1000,140),(1250,175),(1500,200),(1750,220),(2000,230)),
//      Vector(0, 20, 50, 95, 140, 175, 200, 220, 230),
//      0,
//      2000,
//      250,
      units = "dollars per person-year",
      dependencies = Vector("serviceOutputPerCapita"),
      updateFn = () => serviceOutputPerCapita.k
    )

  val effectiveHealthServicesPerCapita =
    Smooth(
      "effectiveHealthServicesPerCapita",
      22,
      constants.effectiveHealthServicesPerCapitaImpactDelay,
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
        Some(constants.lifetimeMultiplierFromHealthServicesPolicyYear)
      )
    )

  val lifetimeMultiplierFromHealthServicesBefore =
    Table(
      "lifetimeMultiplierFromHealthServicesBefore",
      24,
      data = Vector((0,1),(20,1.1),(40,1.4),(60,1.6),(80,1.7),(100,1.8)),
//      Vector(1, 1.1, 1.4, 1.6, 1.7, 1.8),
//      0,
//      100,
//      20,
      dependencies = Vector("effectiveHealthServicesPerCapita"),
      updateFn = () => effectiveHealthServicesPerCapita.k
    )

  val lifetimeMultiplierFromHealthServicesAfter =
    Table(
      "lifetimeMultiplierFromHealthServicesAfter",
      25,
//      data = Vector((0,1),(20,1.5),(40,1.9),(60,2),(80,2),(100,2)),//todo: check different tables
      data = Vector((0,1),(20,1.4),(40,1.6),(60,1.8),(80,1.95),(100,2)),//todo: check different tables
//      Vector(1, 1.4, 1.6, 1.8, 1.95, 2.0),
//      0,
//      100,
//      20,
      dependencies = Vector("effectiveHealthServicesPerCapita"),
      updateFn = () => effectiveHealthServicesPerCapita.k
    )

  val fractionOfPopulationUrban =
    Table(
      "fractionOfPopulationUrban",
      26,
      data = Vector((0,0),(2e+09,0.2),(4e+09,0.4),(6e+09,0.5),(8e+09,0.58),(1e+10,0.65),(1.2e+10,0.72),(1.4e+10,0.78),(1.6e+10,0.8)),
//      Vector(0, 0.2, 0.4, 0.5, 0.58, 0.65, 0.72, 0.78, 0.80),
//      0,
//      1.6e10,
//      2.0e9,
      dependencies = Vector("population"),
      updateFn = () => population.k
    )

  val crowdingMultiplierFromIndustrialization =
    Table(
      "crowdingMultiplierFromIndustrialization",
      27,
      data = Vector((0,0.5),(200,0.05),(400,-0.1),(600,-0.08),(800,-0.02),(1000,0.05),(1200,0.1),(1400,0.15),(1600,0.2)),
//      Vector(0.5, 0.05, -0.1, -0.08, -0.02, 0.05, 0.1, 0.15, 0.2),
//      0,
//      1600,
//      200,
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
      data = Vector((0,1),(10,0.99),(20,0.97),(30,0.95),(40,0.9),(50,0.85),(60,0.75),(70,0.65),(80,0.55),(90,0.4),(100,0.2)),
//      Vector(1.0, 0.99, 0.97, 0.95, 0.90, 0.85, 0.75, 0.65, 0.55, 0.40, 0.20),
//      0,
//      100,
//      10,
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
            lift { unlift(totalFertility.k) * unlift(population15To44.k) * 0.5 / constants.birthsPerYearReproductiveLifetime },
            Some(t),
            Some(constants.birthsPerYearPopulationEquilibriumTime)
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
      updateFn = () => lift { constants.maxTotalFertilityNormal * unlift(fecundityMultiplier.k) }
    )

  val fecundityMultiplier =
    Table(
      "fecundityMultiplier",
      34,
//      data = Vector((0,0),(10,0.2),(20,0.4),(30,0.6),(40,0.7),(50,0.75),(60,0.79),(70,0.84),(80,0.87)),//todo: check different tables!!!
      data = Vector((0,0),(10,0.2),(20,0.4),(30,0.6),(40,0.8),(50,0.9),(60,1.0),(70,1.05),(80,1.1)),//todo: check different tables!!!
      //      Vector(0.0, 0.2, 0.4, 0.6, 0.8, 0.9, 1.0, 1.05, 1.1),
//      0,
//      80,
//      10,
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
      data = Vector((0,3),(10,2.1),(20,1.6),(30,1.4),(40,1.3),(50,1.2),(60,1.1),(70,1.05),(80,1)),
//      Vector(3.0, 2.1, 1.6, 1.4, 1.3, 1.2, 1.1, 1.05, 1.0),
//      0,
//      80,
//      10,
      dependencies = Vector("perceivedLifeExpectancy"),
      updateFn = () => perceivedLifeExpectancy.k
    )

  val perceivedLifeExpectancy =
    Delay3(
      "perceivedLifeExpectancy",
      37,
      delay=constants.lifetimePerceptionDelayK,
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
        lift { constants.desiredCompletedFamilySizeNormal * unlift(familyResponseToSocialNorm.k) * unlift(socialFamilySizeNorm.k) },
        Some(t),
        Some(constants.zeroPopulationGrowthTargetYear)
      )
    )

  val socialFamilySizeNorm =
    Table(
      "socialFamilySizeNorm",
      39,
//      data = Vector((0,1.25),(200,0.94),(400,0.715),(600,0.59),(800,0.5)),//TODO:check different tables
      data = Vector((0,1.25),(200,1),(400,0.9),(600,0.8),(800,0.75)),//TODO:check different tables
      //      Vector(1.25, 1, 0.9, 0.8, 0.75),
//      0,
//      800,
//      200,
      dependencies = Vector("delayedIndustrialOutputPerCapita"),
      updateFn = () => delayedIndustrialOutputPerCapita.k
    )

  //(DIOPC#40)
  val delayedIndustrialOutputPerCapita =
    Delay3(
      qName = "delayedIndustrialOutputPerCapita",
      qNumber = 40,
      delay=constants.socialAdjustmentDelayK,
      units = "dollars per person-year",
      dependencies = Vector("industrialOutputPerCapita"),
      initFn = () => industrialOutputPerCapita
    )

  val familyResponseToSocialNorm =
    Table(
      "familyResponseToSocialNorm",
      41,
      data = Vector((-0.2,0.5),(-0.1,0.6),(0,0.7),(0.1,0.85),(0.2,1)),
//      Vector(0.5, 0.6, 0.7, 0.85, 1.0),
//      -0.2,
//      0.2,
//      0.1,
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
      constants.incomeExpectationAveragingTimeK,
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
      data = Vector((0,0.75),(0.5,0.85),(1,0.9),(1.5,0.95),(2,0.98),(2.5,0.99),(3,1)),
//      Vector(0.75, 0.85, 0.90, 0.95, 0.98, 0.99, 1.0),
//      0,
//      3,
//      0.5,
      dependencies = Vector("fertilityControlFacilitiesPerCapita"),
      updateFn = () => fertilityControlFacilitiesPerCapita.k
    )

  val fertilityControlFacilitiesPerCapita =
    Delay3(
      "fertilityControlFacilitiesPerCapita",
      46,
      delay=constants.healthServicesImpactDelayK,
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
      data = Vector((0,0),(2,0.005),(4,0.015),(6,0.025),(8,0.03),(10,0.035)),
//      Vector(0.0, 0.005, 0.015, 0.025, 0.030, 0.035),
//      0,
//      10,
//      2,
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
        Some(constants.industrialCapitalOutputRatioAfter),
        Some(constants.industrialCapitalOutputRatioBefore),
        Some(t),
        Some(policyYear)
      )
    )

  val industrialCapital: Level =
    Level(
      "industrialCapital",
      52,
      initVal = constants.initialIndustrialCapital,
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
        Some(constants.averageLifetimeOfIndustrialCapitalAfter),
        Some(constants.averageLifetimeOfIndustrialCapitalBefore),
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
        Some(constants.fractionOfIndustrialOutputAllocatedToConsumptionIndustrialEquilibriumTime))
    )

  val fractionOfIndustrialOutputAllocatedToConsumptionConstant =
    Aux(
      "fractionOfIndustrialOutputAllocatedToConsumptionConstant",
      58,
      updateFn = () => clip(
        lift(constants.fractionOfIndustrialOutputAllocatedToConsumptionConstantAfter),
        lift(constants.fractionOfIndustrialOutputAllocatedToConsumptionConstantBefore),
        Some(t),
        Some(policyYear))
    )

  val fractionOfIndustrialOutputAllocatedToConsumptionVariable =
    Table(
      "fractionOfIndustrialOutputAllocatedToConsumptionVariable",
      59,
      data = Vector((0,0.3),(0.2,0.32),(0.4,0.34),(0.6,0.36),(0.8,0.38),(1,0.43),(1.2,0.73),(1.4,0.77),(1.6,0.81),(1.8,0.82),(2,0.83)),
//      Vector(0.3, 0.32, 0.34, 0.36, 0.38, 0.43, 0.73, 0.77, 0.81, 0.82, 0.83),
//      0, 2,
//      0.2,
      dependencies = Vector("industrialOutputPerCapita"),
      updateFn = () => lift { unlift(industrialOutputPerCapita.k) / constants.fractionOfIndustrialOutputAllocatedToConsumptionVariableIndustrialOutputPerCapitaDesired }
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
      data = Vector((0,40),(200,300),(400,640),(600,1000),(800,1220),(1000,1450),(1200,1650),(1400,1800),(1600,2000)),
//      data = Vector(40, 300, 640, 1000, 1220, 1450, 1650, 1800, 2000),
//      iMin = 0,
//      iMax = 1600,
//      iDelta = 200,
      units = "dollars per person-year",
      dependencies = Vector("industrialOutputPerCapita"),
      updateFn = () => industrialOutputPerCapita.k
    )

  val indicatedServiceOutputPerCapitaAfter =
    Table(
      qName = "indicatedServiceOutputPerCapitaAfter",
      qNumber = 62,
      data = Vector((0,40),(200,300),(400,640),(600,1000),(800,1220),(1000,1450),(1200,1650),(1400,1800),(1600,2000)),
//      data = Vector(40, 300, 640, 1000, 1220, 1450, 1650, 1800, 2000),
//      iMin = 0,
//      iMax = 1600,
//      iDelta = 200,
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
      data = Vector((0,0.3),(0.5,0.2),(1,0.1),(1.5,0.05),(2,0)),
//      data = Vector(0.3, 0.2, 0.1, 0.05, 0),
//      iMin = 0, iMax = 2, iDelta = 0.5,
      dependencies = Vector("serviceOutputPerCapita", "indicatedServiceOutputPerCapita"),
      updateFn = () => lift { unlift(serviceOutputPerCapita.k) / unlift(indicatedServiceOutputPerCapita.k) }
    )

  val fractionOfIndustrialOutputAllocatedToServicesAfter =
    Table(
      qName = "fractionOfIndustrialOutputAllocatedToServicesAfter",
      qNumber = 65,
      data = Vector((0,0.3),(0.5,0.2),(1,0.1),(1.5,0.05),(2,0)),
//      data = Vector(0.3, 0.2, 0.1, 0.05, 0),
//      iMin = 0, iMax = 2, iDelta = 0.5,
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
      initVal = constants.initialServiceCapital,
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
        Some(constants.averageLifetimeOfServiceCapitalAfter),
        Some(constants.averageLifetimeOfServiceCapitalBefore),
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
        Some(constants.serviceCapitalOutputRatioAfter),
        Some(constants.serviceCapitalOutputRatioBefore),
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
      data = Vector((50,0.00037),(200,0.00018),(350,0.00012),(500,0.00009),(650,0.00007),(800,0.00006)),
//      Vector(0.00037, 0.00018, 0.00012, 0.00009, 0.00007, 0.00006),
//      50,
//      800,
//      150,
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
      data = Vector((50,0.0011),(200,0.0006),(350,0.00035),(500,0.0002),(650,0.00015),(800,0.00015)),
//      Vector(0.0011, 0.0006, 0.00035, 0.0002, 0.00015, 0.00015),
//      50,
//      800,
//      150,
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
      data = Vector((2,2),(6,0.5),(10,0.4),(14,0.3),(18,0.27),(22,0.24),(26,0.2),(30,0.2)),
//      Vector(2, 0.5, 0.4, 0.3, 0.27, 0.24, 0.2, 0.2),
//      2,
//      30,
//      4,
      units = "persons per hectare",
      dependencies = Vector("agriculturalInputsPerHectare"),
      updateFn = () => { agriculturalInputsPerHectare.k }
    )

  val laborForce =
    Aux(
      "laborForce",
      80,
      units = "persons",
      updateFn = () => lift { (unlift(population15To44.k) + unlift(population45To64.k)) * constants.laborForceParticipationFraction }
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
      constants.laborUtilizationFractionDelayedDelayTime,
      dependencies = Vector("laborUtilizationFraction"),
      initFn = () => laborUtilizationFraction
    )

  val capitalUtilizationFraction: Table =
    Table(
      "capitalUtilizationFraction",
      83,
      data = Vector((1,1),(3,0.9),(5,0.7),(7,0.3),(9,0.1),(11,0.1)),
//      Vector(1.0, 0.9, 0.7, 0.3, 0.1, 0.1),
//      1,
//      11,
//      2,
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
      updateFn = () => lift {unlift(arableLand.k) / constants.landFractionCultivatedPotentiallyArableLandTotal }
    )

  val arableLand: Level =
    Level(
      "arableLand",
      85,
      constants.initialArableLand,
      units = "hectares",
      updateFn = () => lift {
        unlift(arableLand.j) + dt * (unlift(landDevelopmentRate.j) - unlift(landErosionRate.j) - unlift(landRemovalForUrbanIndustrialUse.j))
      }
    )

  val potentiallyArableLand: Level =
    Level(
      "potentiallyArableLand",
      86,
      constants.initialPotentiallyArableLand,
      units = "hectares",
      updateFn = () => lift { unlift(potentiallyArableLand.j) + dt * (-unlift(landDevelopmentRate.j)) }
    )

  val food =
    Aux(
      "food",
      87,
      units = "kilograms per year",
      dependencies = Vector("landYield"),
      updateFn = () => lift { unlift(landYield.k) * unlift(arableLand.k) * constants.foodLandFractionHarvestedK * (1 - constants.foodProcessingLossK) }
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
      data = Vector((0,230),(200,480),(400,690),(600,850),(800,970),(1000,1070),(1200,1150),(1400,1210),(1600,1250)),
//      data = Vector(230, 480, 690, 850, 970, 1070, 1150, 1210, 1250),
//      iMin = 0, iMax = 1600, iDelta = 200,
      units = "kilograms per person-year",
      dependencies = Vector("industrialOutputPerCapita"),
      updateFn = () => industrialOutputPerCapita.k
    )

  val indicatedFoodPerCapitaAfter =
    Table(
      qName = "indicatedFoodPerCapitaAfter",
      qNumber = 91,
      data = Vector((0,230),(200,480),(400,690),(600,850),(800,970),(1000,1070),(1200,1150),(1400,1210),(1600,1250)),
//      data = Vector(230, 480, 690, 850, 970, 1070, 1150, 1210, 1250),
//      iMin = 0, iMax = 1600, iDelta = 200,
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
      data = Vector((0,0.4),(0.5,0.2),(1,0.1),(1.5,0.025),(2,0),(2.5,0)),
//      data = Vector(0.4, 0.2, 0.1, 0.025, 0.0, 0.0),
//      iMin = 0,
//      iMax = 2.5,
//      iDelta = 0.5,
      updateFn = () => lift { unlift(foodPerCapita.k) / unlift(indicatedFoodPerCapita.k) },
      dependencies = Vector("foodPerCapita", "indicatedFoodPerCapita")
    )

  val fractionOfIndustrialOutputAllocatedToAgricultureAfter =
    Table(
      qName = "fractionOfIndustrialOutputAllocatedToAgricultureAfter",
      qNumber = 95,
      data = Vector((0,0.4),(0.5,0.2),(1,0.1),(1.5,0.025),(2,0),(2.5,0)),
//      data = Vector(0.4, 0.2, 0.1, 0.025, 0.0, 0.0),
//      iMin = 0,
//      iMax = 2.5,
//      iDelta = 0.5,
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
    data = Vector((0,100000),(0.1,7400),(0.2,5200),(0.3,3500),(0.4,2400),(0.5,1500),(0.6,750),(0.7,300),(0.8,150),(0.9,75),(1,50)),
//    data = Vector(100000, 7400, 5200, 3500, 2400, 1500, 750, 300, 150, 75, 50), iMin = 0, iMax = 1.0, iDelta = 0.1,
    units = "dollars per hectare",
    updateFn = () => lift {unlift(potentiallyArableLand.k) / constants.potentiallyArableLandTotal}
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

  //AGRICULTURAL INPUTS (AI#99)
  val agriculturalInputs =
    Smooth("agriculturalInputs", 99,
      delay = constants.averageLifetimeOfAgriculturalInputsK,
      units = "dollars per year",
      dependencies = Vector(),   // "currentAgriculturalInputs" removed to break cycle
      initFn = () => { currentAgriculturalInputs },
      initVal = Some(constants.initialAgriculturalInputs)// simplification from complete model?
    )

  val averageLifetimeOfAgriculturalInputs =
    Aux(
      "averageLifetimeOfAgriculturalInputs",
      100,
      units = "years",
      updateFn = () => clip(
        Some(constants.averageLifetimeOfAgriculturalInputsAfterPolicy),
        Some(constants.averageLifetimeOfAgriculturalInputsBeforePolicy),
        Some(t),
        Some(policyYear))
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
//      data = Vector(
//        (0,1),(40,3),(80,4.5),(120,5),(160,5.3),(200,5.6),(240,5.9)
//        ,(280,6.1),(320,6.35),(360,6.6),(400,6.9),(440,7.2),(480,7.4)
//        ,(520,7.6),(560,7.8),(600,8),(640,8.2),(680,8.4),(720,8.6)
//        ,(760,8.8),(800,9),(840,9.2),(880,9.4),(920,9.6),(960,9.8)
//        ,(1000,10)),//TODO : check different tables!
      data = Vector(
        (0,1),(40,3),(80,3.8),(120,4.4),(160,4.9),(200,5.4),(240,5.7)
        ,(280,6),(320,6.3),(360,6.6),(400,6.9),(440,7.2),(480,7.4)
        ,(520,7.6),(560,7.8),(600,8),(640,8.2),(680,8.4),(720,8.6)
        ,(760,8.8),(800,9),(840,9.2),(880,9.4),(920,9.6),(960,9.8)
        ,(1000,10)),//TODO : check different tables!
      //      data=Vector(1, 3, 3.8, 4.4, 4.9, 5.4, 5.7, 6, 6.3, 6.6, 6.9, 7.2, 7.4, 7.6, 7.8, 8, 8.2, 8.4, 8.6, 8.8, 9, 9.2, 9.4, 9.6, 9.8, 10),
//      iMin = 0, iMax = 1000, iDelta = 40,
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
      updateFn = () => clip(
        Some(constants.landYieldFactorBeforePolicy),//TODO: Land yield factor after policy year (LYF2#104.2).
        Some(constants.landYieldFactorBeforePolicy),
        Some(t),
        Some(policyYear))
    )

  val landYieldMultiplierFromAirPollution = Aux(
    "landYieldMultiplierFromAirPollution", 105,
    dependencies = Vector("landYieldMultiplierFromAirPollutionBefore", "landYieldMultiplierFromAirPollutionAfter"),
    updateFn = () => clip(landYieldMultiplierFromAirPollutionAfter.k, landYieldMultiplierFromAirPollutionBefore.k, Some(t), Some(policyYear))
  )

  val landYieldMultiplierFromAirPollutionBefore = Table("landYieldMultiplierFromAirPollutionBefore", qNumber = 106,
    data = Vector((0,1),(10,1),(20,0.7),(30,0.4)),
//    data = Vector(1.0, 1.0, 0.7, 0.4), iMin = 0, iMax = 30, iDelta = 10,
    dependencies = Vector("industrialOutput"),
    updateFn = () => lift {unlift(industrialOutput.k) / constants.industrialOutputValueIn1970}
  )

  val landYieldMultiplierFromAirPollutionAfter = Table("landYieldMultiplierFromAirPollutionAfter", qNumber = 107,
    data = Vector((0,1),(10,1),(20,0.98),(30,0.95)),
//    data = Vector(1.0, 1.0, 0.7, 0.4), iMin = 0, iMax = 30, iDelta = 10,
    dependencies = Vector("industrialOutput"),
    updateFn = () => lift {unlift(industrialOutput.k) / constants.industrialOutputValueIn1970}
  )


  // Loops 1 and 2: The Investment Allocation Decision
  val fractionOfInputsAllocatedToLandDevelopment = Table(
    "fractionOfInputsAllocatedToLandDevelopment", 108,
    data = Vector((0,0),(0.25,0.05),(0.5,0.15),(0.75,0.3),(1,0.5),(1.25,0.7),(1.5,0.85),(1.75,0.95),(2,1)),
//    data = Vector(0, 0.05, 0.15, 0.30, 0.50, 0.70, 0.85, 0.95, 1), iMin = 0, iMax = 2, iDelta = 0.25,
    dependencies = Vector("marginalProductivityOfLandDevelopment", "marginalProductivityOfAgriculturalInputs"),
    updateFn = () => lift {unlift(marginalProductivityOfLandDevelopment.k) / unlift(marginalProductivityOfAgriculturalInputs.k)}
  )

  val marginalProductivityOfLandDevelopment = Aux("marginalProductivityOfLandDevelopment", 109,
    units = "kilograms per dollar",
    dependencies = Vector("landYield", "developmentCostPerHectare"),
    updateFn = () => lift { unlift(landYield.k) / (unlift(developmentCostPerHectare.k) * constants.socialDiscount)}
  )

  val marginalProductivityOfAgriculturalInputs = Aux("marginalProductivityOfAgriculturalInputs", 110,
    units = "kilograms per dollar",
    dependencies = Vector("averageLifetimeOfAgriculturalInputs", "landYield", "marginalLandYieldMultiplierFromCapital", "landYieldMultiplierFromCapital"),
    updateFn = () => lift {
      constants.averageLifetimeOfAgriculturalInputsK * unlift(landYield.k) *
        (unlift(marginalLandYieldMultiplierFromCapital.k) / unlift(landYieldMultiplierFromCapital.k))}
  )

  val marginalLandYieldMultiplierFromCapital = Table(
    "marginalLandYieldMultiplierFromCapital", 111,
    data = Vector(
      (0,0.075),(40,0.03),(80,0.015),(120,0.011),(160,0.009),(200,0.008)
      ,(240,0.007),(280,0.006),(320,0.005),(360,0.005),(400,0.005)
      ,(440,0.005),(480,0.005),(520,0.005),(560,0.005),(600,0.005)),
//    data = Vector(0.075, 0.03, 0.015, 0.011, 0.009, 0.008, 0.007, 0.006, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005),
//    iMin = 0, iMax = 600, iDelta = 40,
    units = "hectares per dollar",
    dependencies = Vector("agriculturalInputsPerHectare"),
    updateFn = () => agriculturalInputsPerHectare.k
  )

  // Loop 3: Land Erosion and Urban-Industrial Use
  val averageLifeOfLand = Aux("averageLifeOfLand", 112,
    units = "years",
    dependencies = Vector("landLifeMultiplierFromYield"),
    updateFn = () => lift {constants.averageLifeOfLandNormal * unlift(landLifeMultiplierFromYield.k)}
  )

  val landLifeMultiplierFromYield = Aux("landLifeMultiplierFromYield", 113,
    dependencies = Vector("landLifeMultiplierFromYieldBefore", "landLifeMultiplierFromYieldAfter"),
    updateFn = () => clip(landLifeMultiplierFromYieldAfter.k, landLifeMultiplierFromYieldBefore.k, Some(t), Some(policyYear))
  )

  val landLifeMultiplierFromYieldBefore = Table("landLifeMultiplierFromYieldBefore", qNumber = 114,
    data = Vector((0,1.2),(1,1),(2,0.63),(3,0.36),(4,0.16),(5,0.055),(6,0.04),(7,0.025),(8,0.015),(9,0.01)),
//    data = Vector(1.2, 1.0, 0.63, 0.36, 0.16, 0.055, 0.04, 0.025, 0.015, 0.01),
//    iMin = 0, iMax = 9, iDelta = 1,
    dependencies = Vector("landYield"),
    updateFn = () => lift {unlift(landYield.k) / constants.inherentLandFertilityK}
  )

  val landLifeMultiplierFromYieldAfter = Table("landLifeMultiplierFromYieldAfter", qNumber = 115,
//    data = Vector((0,1.2),(1,1),(2,0.63),(3,0.36),(4,0.29),(5,0.26),(6,0.24),(7,0.22),(8,0.21),(9,0.2)),//TODO: check different tables!
    data = Vector((0,1.2),(1,1),(2,0.63),(3,0.36),(4,0.16),(5,0.055),(6,0.04),(7,0.025),(8,0.015),(9,0.01)),//TODO: check different tables!
//    data = Vector(1.2, 1.0, 0.63, 0.36, 0.16, 0.055, 0.04, 0.025, 0.015, 0.01),
//    iMin = 0, iMax = 9, iDelta = 1,
    dependencies = Vector("landYield"),
    updateFn = () => lift {unlift(landYield.k) / constants.inherentLandFertilityK}
  )

  val landErosionRate = Rate("landErosionRate", 116,
      units = "hectares per year",
      updateFn = () => lift { unlift(arableLand.k) / unlift(averageLifeOfLand.k) }
    )

  // 2016-08-09: Neil S. Grant reported an error in the table of values for urbanIndustrialLandPerCapita. The third element of the array should be 0.015, not 0.15. Corrected.
  val urbanIndustrialLandPerCapita = Table("urbanIndustrialLandPerCapita", 117,
    data = Vector((0,0.005),(200,0.008),(400,0.015),(600,0.025),(800,0.04),(1000,0.055),(1200,0.07),(1400,0.08),(1600,0.09)),
//    data = Vector(0.005, 0.008, 0.015, 0.025, 0.04, 0.055, 0.07, 0.08, 0.09),
//    iMin = 0, iMax = 1600, iDelta = 200,
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
    updateFn = () => lift {math.max(0, (unlift(urbanIndustrialLandRequired.k) - unlift(urbanIndustrialLand.k)) / constants.developmentTime)}
  )

  val urbanIndustrialLand: Level = Level(
    "urbanIndustrialLand", 120,
    initVal = constants.initialUrbanIndustrialLand,
    units = "hectares",
    updateFn = () => lift {unlift(urbanIndustrialLand.j) + dt * unlift(landRemovalForUrbanIndustrialUse.j)}
  )

  // Loop 4: Land fertility degradation
  val landFertility:Level = Level(
    qName="landFertility",
    qNumber = 121,
    initVal = constants.initialLandFertility,
    units = "kilograms per hectare-year",
    updateFn = ()=> lift {unlift(landFertility.j) + dt * (unlift(landFertilityRegeneration.j) - unlift(landFertilityDegradation.j))}
  )

  val landFertilityDegradationRate = Table(
    qName="landFertilityDegradationRate", qNumber=122,
    data = Vector((0,0),(10,0.1),(20,0.3),(30,0.5)),
//    data=Vector(0, 0.1, 0.3, 0.5), iMin=0, iMax = 30, iDelta = 10,
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
    updateFn = () => lift {(constants.inherentLandFertilityK - unlift(landFertility.k)) / unlift(landFertilityRegenerationTime.k)}
  )

  val landFertilityRegenerationTime = Table(
    qName= "landFertilityRegenerationTime", qNumber=125,
    data = Vector((0,20),(0.02,13),(0.04,8),(0.06,4),(0.08,2),(0.1,2)),
//    data = Vector(20, 13, 8, 4, 2, 2), iMin=0, iMax=0.1, iDelta=0.02,
    units = "years",
    dependencies = Vector("fractionOfInputsAllocatedToLandMaintenance"),
    updateFn = () => fractionOfInputsAllocatedToLandMaintenance.k
  )


  // Loop 6: Discontinuing land maintenance
  val fractionOfInputsAllocatedToLandMaintenance = Table(
    qName = "fractionOfInputsAllocatedToLandMaintenance", qNumber = 126,
    data = Vector((0,0),(1,0.04),(2,0.07),(3,0.09),(4,0.1)),
//    data = Vector(0, 0.04, 0.07, 0.09, 0.10),
//    iMin=0, iMax=4, iDelta=1,
    dependencies = Vector("perceivedFoodRatio"),
    updateFn = () => perceivedFoodRatio.k
  )

  val foodRatio = Aux(
    qName = "foodRatio",
    qNumber= 127,
    dependencies = Vector("foodPerCapita"),
    updateFn = () => lift {unlift(foodPerCapita.k)/constants.subsistenceFoodPerCapitaK}
  )

  val perceivedFoodRatio = Smooth(
    qName = "perceivedFoodRatio",
    qNumber = 128,
    delay= constants.foodShortagePerceptionDelayK,
    initVal = Some(1.0),//TODO: add constant
    initFn = () => foodRatio // ??? CHECK THAT!!!
  )

  // NONRENEWABLE RESOURCE SECTOR
  val nonrenewableResources: Level = Level(
    qName = "nonrenewableResources",
    qNumber = 129,
    initVal = constants.nonrenewableResourcesInitialK,
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
//    data = Vector((0,0),(200,0.85),(400,2.6),(600,3.4),(800,3.8),(1000,4.1),(1200,4.4),(1400,4.7),(1600,5)),//TODO:check different tables!
    data = Vector((0,0),(200,0.85),(400,2.6),(600,4.4),(800,5.4),(1000,6.2),(1200,6.8),(1400,7),(1600,7)),//TODO:check different tables!
//    data = Vector(0, 0.85, 2.6, 4.4, 5.4, 6.2, 6.8, 7, 7), iMin = 0, iMax = 1600, iDelta = 200,
    units = "resource units per person-year",
    dependencies = Vector("industrialOutputPerCapita"),
    updateFn = () => industrialOutputPerCapita.k
  )

  val nonrenewableResourceFractionRemaining = Aux(
    qName = "nonrenewableResourceFractionRemaining",
    qNumber = 133,
    updateFn = () => lift {unlift(nonrenewableResources.k) / constants.nonrenewableResourcesInitialK}
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
    data = Vector((0,1),(0.1,0.9),(0.2,0.7),(0.3,0.5),(0.4,0.2),(0.5,0.1),(0.6,0.05),(0.7,0.05),(0.8,0.05),(0.9,0.05),(1,0.05)),
//    data = Vector(1, 0.9, 0.7, 0.5, 0.2, 0.1, 0.05, 0.05, 0.05, 0.05, 0.05),
//    iMin = 0, iMax = 1, iDelta = 0.1,
    dependencies = Vector("nonrenewableResourceFractionRemaining"),
    updateFn = () => nonrenewableResourceFractionRemaining.k
  )

  val fractionOfCapitalAllocatedToObtainingResourcesAfter = Table(
    qName = "fractionOfCapitalAllocatedToObtainingResourcesAfter",
    qNumber = 136,
    data = Vector((0,1),(0.1,0.2),(0.2,0.1),(0.3,0.05),(0.4,0.05),(0.5,0.05),(0.6,0.05),(0.7,0.05),(0.8,0.05),(0.9,0.05),(1,0.05)),
//    data = Vector(1, 0.9, 0.7, 0.5, 0.2, 0.1, 0.05, 0.05, 0.05, 0.05, 0.05),
//    iMin = 0, iMax = 1, iDelta = 0.1,
    dependencies = Vector("nonrenewableResourceFractionRemaining"),
    updateFn = () => nonrenewableResourceFractionRemaining.k
  )


  // PERSISTENT POLLUTION SECTOR

  val persistentPollutionGenerationRate = Rate(qName = "persistentPollutionGenerationRate", qNumber = 137,
    units = "pollution units per year",
    updateFn = () =>
      lift { (unlift(persistentPollutionGeneratedByIndustrialOutput.k) + unlift(persistentPollutionGeneratedByAgriculturalOutput.k)) * unlift(persistentPollutionGenerationFactor.k)}
  )

//  //Table relating the food ratio gap to the change in agricultural technology (LYCMT#--).
//  val landYieldTechnologyChangeRateMultiplier = Table(qName = "landYieldTechnologyChangeRateMultiplier", qNumber = 138,//NEW
//    units = "1/year",
//    dependencies = Vector("foodRatio"),
//    updateFn = () => lift { constants.desiredFoodRatio - unlift(foodRatio.k) },
////    data = Vector((0,0),(1,0))//TODO:check different tables
//    data = Vector((0,0.1),(1,0.0))//TODO:check different tables
////    data = Vector(0.1, 0.0),
////    iMin = 0,
////    iMax = 1,
////    iDelta = 1
//  )
//
//  val landYieldTechnologyChangeRate = Aux(qName = "landYieldTechnologyChangeRate", qNumber = 138,
//    units = "1/year",
//    updateFn = () => clip(
//      lift { unlift(landYieldTechnology.k) * unlift(landYieldTechnologyChangeRateMultiplier.k)},
//      Some(0),
//      Some(t),
//      Some(policyYear))
//  )
//
//  // LAND YIELD TECHNOLOGY INITIATED (LYTD#--)
//  val landYieldTechnology: Level = Level(//NEW
//    qName = "landYieldTechnology",
//    qNumber = 138,
//    initVal = 1.0,//TODO: add constant
//    units = "hectares",
//    updateFn = () => lift { unlift(landYieldTechnology.j) + dt * unlift(landYieldTechnologyChangeRate.j) }
//  )
//
//  // POLLUTION CONTROL TECHNOLOGY CHANGE MULTIPLIER (POLGFM#--)
//  val persistentPollutionTechnologyChangeRateMultiplier = Table(
//    qName = "persistentPollutionTechnologyChangeRateMultiplier",
//    qNumber = 138,
//    units = "1/year",
//    dependencies = Vector(""),//TODO
//    updateFn = () => lift{1 - unlift(indexOfPersistentPollution.k) / constants.desiredPersistentPollutionIndex},
//    data = Vector((-1,0),(0,0))
////      data = Vector(0, 0),
////      iMin = -1,
////      iMax = 0,
////      iDelta = 1
//  )
//  //pollution control technology change rate (PTDR#--)
//  val persistentPollutionTechnologyChangeRate = Aux(
//    qName = "persistentPollutionTechnologyChangeRate",
//    qNumber = 138,
//    units = "1/year",
//    updateFn = () => clip(
//      lift {unlift(persistentPollutionTechnology.k) * unlift(persistentPollutionTechnologyChangeRateMultiplier.k)},
//      Some(0.0),
//      Some(t),
//      Some(policyYear)
//    )
//  )
//
//  //Pollution control technology initiated (PTD#--)
//  val persistentPollutionTechnology: Level = Level(
//      qName = "persistentPollutionTechnology",
//      qNumber = 138,
//      initVal = constants.initialPersistentPollutionTechnology,
//      updateFn = () => lift { unlift(persistentPollutionTechnology.j) + dt * unlift(persistentPollutionTechnologyChangeRate.j)}
//    )
//    //        INTEG( persistent pollution technology change rate , 1)
//
//  //(PPGF2#138.2)
//  val persistentPollutionTechnologyDevelopment = Delay3(
//    qName = "persistentPollutionTechnologyDevelopment",
//    qNumber = 138,
//    delay = constants.technologyDevelopmentDelay,
//    dependencies = Vector("persistentPollutionTechnology"),
//    initFn = () => persistentPollutionTechnology
//  )

  val persistentPollutionGenerationFactor = Aux(
    qName = "persistentPollutionGenerationFactor",
    qNumber = 138,
    updateFn = () => clip(
//      persistentPollutionTechnologyDevelopment.k,
      Some(constants.persistentPollutionGenerationFactorBefore),
      Some(constants.persistentPollutionGenerationFactorBefore),
      Some(t),
      Some(policyYear))
  )

  val persistentPollutionGeneratedByIndustrialOutput = Aux(
    qName = "persistentPollutionGeneratedByIndustrialOutput",
    qNumber = 139,
    units = "pollution units per year",
    dependencies = Vector("perCapitaResourceUsageMultiplier", "population"),
    updateFn = () => lift {unlift(perCapitaResourceUsageMultiplier.k) * unlift(population.k) * constants.fractionOfResourcesAsPersistentMaterial * constants.industrialMaterialsEmissionFactor * constants.industrialMaterialsToxicityIndex}
  )

  val persistentPollutionGeneratedByAgriculturalOutput = Aux(
    qName = "persistentPollutionGeneratedByAgriculturalOutput",
    qNumber = 140,
    units = "pollution units per year",
    dependencies = Vector("agriculturalInputsPerHectare"),
    updateFn = () => lift {unlift(agriculturalInputsPerHectare.k) * unlift(arableLand.k) * constants.fractionOfInputsAsPersistentMaterial * constants.agriculturalMaterialsToxicityIndex}
  )

  val persistentPollutionAppearanceRate =
    Delay3(
      qName = "persistentPollutionAppearanceRate",
      qNumber = 141,
      delay = constants.persistentPollutionTransmissionDelayK,
      units = "pollution units per year",
      initFn = () => { persistentPollutionGenerationRate }
    )

  val persistentPollution: Level =
    Level(
      qName = "persistentPollution",
      qNumber = 142,
      initVal = 2.5e7,//TODO: add constant
      units = "pollution units",
      updateFn = () => lift {unlift(persistentPollution.j) + dt * (unlift(persistentPollutionAppearanceRate.j) - unlift(persistentPollutionAssimilationRate.j))}
    )

  val indexOfPersistentPollution = Aux(
    qName = "indexOfPersistentPollution",
    qNumber = 143,
    updateFn = () => lift {unlift(persistentPollution.k) / constants.pollutionValueIn1970}
  )

  val persistentPollutionAssimilationRate = Rate(
    qName = "persistentPollutionAssimilationRate",
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
    data = Vector((1,1),(251,11),(501,21),(751,31),(1001,41))
//    data = Vector(1, 11, 21, 31, 41),
//    iMin = 1,
//    iMax = 1001,
//    iDelta = 250
  )

  val assimilationHalfLife = Aux(
    qName = "assimilationHalfLife",
    qNumber = 146,
    units = "years",
    dependencies = Vector("assimilationHalfLifeMultiplier"),
    updateFn = () => lift {unlift(assimilationHalfLifeMultiplier.k) * constants.assimilationHalfLifeValueIn1970}
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

  val auxSequence = Vector(
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
//    landYieldTechnologyChangeRateMultiplier,//new
//    landYieldTechnologyChangeRate,//new
//    persistentPollutionTechnologyChangeRateMultiplier,//new
//    persistentPollutionTechnologyChangeRate,//new
//    persistentPollutionTechnologyDevelopment,//new
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
//    persistentPollutionTechnology,//new
//    landYieldTechnology//new
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
    persistentPollutionAssimilationRate
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
//    landYieldTechnologyChangeRateMultiplier,//new
//    landYieldTechnologyChangeRate,//new
//    persistentPollutionTechnologyChangeRateMultiplier,//new
//    persistentPollutionTechnologyChangeRate,//new
//    persistentPollutionTechnologyDevelopment,//new
//    persistentPollutionTechnology,//new
//    landYieldTechnology,//new
    persistentPollutionGenerationRate,
    persistentPollutionGenerationFactor,
    persistentPollutionGeneratedByIndustrialOutput,
    persistentPollutionGeneratedByAgriculturalOutput,
    persistentPollutionAppearanceRate,
    persistentPollution,
    indexOfPersistentPollution,
    persistentPollutionAssimilationRate,
    assimilationHalfLifeMultiplier,
    assimilationHalfLife,
    fractionOfOutputInAgriculture,
    fractionOfOutputInIndustry,
    fractionOfOutputInServices
  )

}
