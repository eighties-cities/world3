package world3

import numeric._

object Agriculture1 {

  /**
    initial arable land
    hectare
    constant
    The initial amount of land that is arable.                 (ALI#85.2).
  */
  val initial_arable_land = 9e+08

  /*
   @cache('step')
   def arable_land():
   """
     Arable Land

     hectare

     component

     Arable land (AL#85).
     """
   return integ_arable_land()

   integ_arable_land = functions.Integ(lambda: land_development_rate()-land_erosion_rate()-land_removal_for_urban_and_industrial_use(), lambda: initial_arable_land())

    cache step

     Arable Land
     hectare
     component
     Arable land (AL#85).
   */
   def arable_land_dot() =
     land_development_rate() - land_erosion_rate() - land_removal_for_urban_and_industrial_use()

   /*
     land development rate
     hectare/year
     component
     The land developmen rate (LDR#96). */
   def land_development_rate() =
     total_agricultural_investment() * fraction_of_agricultural_inputs_allocated_to_land_development() / development_cost_per_hectare()

   /* cache step

     total agricultural investment
     $/year
     component
     TOTAL AGRICULTURAL INVESTMENT (TAI#92)
    */
   def total_agricultural_investment(time: Double, policy_year: Double) =
     industrial_output() * fraction_of_industrial_output_allocated_to_agriculture(time, policy_year)



   /* cache step

     fraction of industrial output allocated to agriculture 1
     Dmnl
     component
     Fraction of industrial output allocated to agriculture before policy time (FIOAA1#94).
   */
   def fraction_of_industrial_output_allocated_to_agriculture_1() =
     fraction_industrial_output_allocated_to_agriculture_table_1(food_per_capita() / indicated_food_per_capita())

   /*
     fraction industrial output allocated to agriculture table 1
     Dmnl
     lookup

     Table relating food per capita to the fraction of industrial output allocated to agriculture (FIOAA1T#94.1).*/
   def fraction_industrial_output_allocated_to_agriculture_table_1(x: Double) =
     lookup(x, Vector(0, 0.5, 1, 1.5, 2, 2.5), Vector(0.4, 0.2, 0.1, 0.025, 0, 0))



   /* cache step

     development cost per hectare
     $/hectare
     component
     Development cost per hectare (DCPH#97).
    */
   def development_cost_per_hectare() =
     development_cost_per_hectare_table(potentially_arable_land() / potentially_arable_land_total())


  /*
   development cost per hectare table
   $/hectare
   lookup
   Table relating undeveloped land to the cost of land development (DCPHT#97.1).
   */
  def development_cost_per_hectare_table(x: Double) =
    lookup(x, Vector(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), Vector(100000, 7400, 5200, 3500, 2400, 1500, 750, 300, 150, 75, 50))

  /* cache step

     food
     Veg eq kg/year
     component
     The total amount of usable food (F#87).
   */
  def food(arable_land: Double) =
    land_yield() * arable_land * land_fraction_harvested * (1 - processing_loss)


  /* cache step

    food per capita
    Veg eq kg/(Person*year)
    component
    Food per capita (FPC#88)
   */
  def food_per_capita(arable_land: Double) = food(arable_land) / population()


  /* cache step

    land fr cult
    Dmnl
    component
    Land fraction under cultivarion (LFC#84).
   */
  def land_fr_cult(arable_land: Double) = arable_land / potentially_arable_land_total



  /* cache run

    land fraction harvested
    Dmnl
    constant
   Land fraction harvested (LFH#87.1). */
  val land_fraction_harvested = 0.7





  /*
    cache('step')

    fraction of industrial output allocated to agriculture 2
    Dmnl
    component
    Fraction of industrial output allocated to agriculture after policy time (FIOAA2#95).  */
  def fraction_of_industrial_output_allocated_to_agriculture_2() =
    fraction_industrial_output_allocated_to_agriculture_table_2(food_per_capita() / indicated_food_per_capita())


  /*
    fraction industrial output allocated to agriculture table 2
    Dmnl
    lookup
    Table relating food per capita to the fraction of industrial output allocated to agriculture (FIOAA2T#95.1). */
  def fraction_industrial_output_allocated_to_agriculture_table_2(x: Double) =
    lookup(x, Vector(0, 0.5, 1, 1.5, 2, 2.5), Vector(0.4, 0.2, 0.1, 0.025, 0, 0))


  /* cache('step')

    indicated food per capita 1
    Veg eq kg/(Person*year)
    component
    Indicated foord per capita befor policy time (IFPC1#90).
   */
  def indicated_food_per_capita_1() =
    indicated_food_per_capita_table_1(industrial_output_per_capita() / gdp_pc_unit())

  /*
    indicated food per capita table 1
    Veg eq kg/(Person*year)
    lookup
    Table relating industrial output to indicated food requirements 1 (IFPC1T#90.1). */
  def indicated_food_per_capita_table_1(x: Double) =
    lookup(x, Vector(0, 200, 400, 600, 800, 1000, 1200, 1400, 1600), Vector(230, 480, 690, 850, 970, 1070, 1150, 1210, 1250))

  /*
    @cache('step')

    indicated food per capita 2
    Veg eq kg/(Person*year)
    component
    Indicated foord per capita after policy time (IFPC1#90) */
  def indicated_food_per_capita_2() =
    indicated_food_per_capita_table_2(industrial_output_per_capita() / gdp_pc_unit())


  /*

      indicated food per capita table 2
      Veg eq kg/(Person*year)
      lookup
      Table relating industrial output to indicated food requirements 2 (IFPC1T#90.1). */
  def indicated_food_per_capita_table_2(x: Double) =
    lookup(x, Vector(0, 200, 400, 600, 800, 1000, 1200, 1400, 1600), Vector(230, 480, 690, 850, 970, 1070, 1150, 1210, 1250))



  /*
  @cache('step')

  Potentially Arable Land
  hectare
  component

  POTENTIALLY ARABLE LAND (PAL#86).

  integ_potentially_arable_land = functions.Integ(lambda: (-land_development_rate()),
      lambda: initial_potentially_arable_land())
   */
 // def potentially_arable_land() = integ_potentially_arable_land()
  def potentially_arable_land_dot() = -land_development_rate()


  /*
  cache('run')

      initial potentially arable land

      hectare

      constant

      The initial amount of potentially arable land (PALI#86.2).

   */
  val initial_potentially_arable_land = 2.3e+09

  /*
    cache('run')

    potentially arable land total
    hectare
    constant
    POTENTIALLY ARABLE LAND TOTAL (PALT#84.1).*/
  val potentially_arable_land_total = 3.2e+09


  /*
  @cache('run')

      processing loss
      Dmnl
      constant
      PROCESSING LOSS (PL#87.2)
   */
  val processing_loss = 0.1


  /*
  @cache('step')

      fraction of industrial output allocated to agriculture

      Dmnl

      component

      FRACTION OF INDUSTRIAL OUTPUT ALLOCATED TO AGRICULTURE (FIOAA#93).

   */
  def fraction_of_industrial_output_allocated_to_agriculture(time: Double, policy_year: Double) =
    if(time >= policy_year) fraction_of_industrial_output_allocated_to_agriculture_2()
    else fraction_of_industrial_output_allocated_to_agriculture_1()



  /* cache('step')

  indicated food per capita
  Veg eq kg/(Person*year)
  component

  Indicated food per capita (IFPC#89).*/
  def indicated_food_per_capita(time: Double, policy_year: Double) =
    if(time >= policy_year) indicated_food_per_capita_2()
    else indicated_food_per_capita_1()

}

object Agriculture2 {

  /* @cache('step')

  Agricultural Inputs

    $/year

    component

    AGRICULTURAL INPUTS (AI#99)
   */
//  def agricultural_inputs() = smooth_current_agricultural_inputs_average_life_agricultural_inputs_current_agricultural_inputs_1()
//
//  #Circular init!
//    smooth_current_agricultural_inputs_average_life_agricultural_inputs_current_agricultural_inputs_1 = functions.Smooth(
//    lambda: current_agricultural_inputs(), lambda: average_life_agricultural_inputs(),
//  lambda: 5e+09, lambda: 1)
  def agricultural_inputs() = smooth(current_agricultural_inputs(), average_life_agricultural_inputs(),  5e+09, 1)



  /*@cache('step')

    average life agricultural inputs
    year
    component
    AVERAGE LIFETIME OF AGRICULTURAL INPUTS (ALAI#100)
   */
  def average_life_agricultural_inputs(time: Double, policy_year: Double) =
    if(time >= policy_year) average_life_of_agricultural_inputs_2() else average_life_of_agricultural_inputs_1()

  /*  cache('step')

      agricultural input per hectare
    $/(year*hectare)
    component

    AGRICULTURAL INPUTS PER HECTARE (AIPH#101)
   */
  def agricultural_input_per_hectare() =
    agricultural_inputs() * (1 - fraction_of_agricultural_inputs_for_land_maintenance()) / arable_land()


  /*
  @cache('step')

      current agricultural inputs
    $/year
    component
    CURRENT AGRICULTURAL INPUTS (CAI#98).
   */
  def current_agricultural_inputs() =
    //functions.active_initial(
    total_agricultural_investment() * (1 - fraction_of_agricultural_inputs_allocated_to_land_development())//, 5e+09)

  val initial_agricultural_inputs = 5e+09

  /* cache('run')

    desired food ratio
    Dmnl
    constant
    desired food ratio (DFR#--)    */
  val desired_food_ratio = 2


  /*@cache('run')
    """
    IND OUT IN 1970

    $/year

    constant

    INDUSTRIAL OUTPUT IN 1970 (IO70#107.2)
    """
   */
  val ind_out_in_1970 = 7.9e+11


  /*@cache('step')

  land yield

    Veg eq kg/(year*hectare)

    component

    LAND YIELD (LY#103)
   */
  def land_yield(time: Double, policy_year: Double) =
    land_yield_multiplier_from_technology(time, policy_year) * land_fertility() * land_yield_multiplier_from_capital() * land_yield_multiplier_from_air_pollution()

  /*
  @cache('step')

  """
    land yield multiplier from capital

    Dmnl

    component

    LAND YIELD MULTIPLIER FROM CAPITAL (LYMC#102)
    """
   */
  def land_yield_multiplier_from_capital() =
    land_yield_multiplier_from_capital_table(agricultural_input_per_hectare() / unit_agricultural_input())


  /*
    land yield multiplier from capital table
    Dmnl
    lookup
    Table relating agricultural inputs to land yeild (LYMCT#102.1). */
  def land_yield_multiplier_from_capital_table(x: Double) =
    lookup(
      x,
      Vector(0, 40, 80, 120, 160, 200, 240, 280, 320, 360, 400, 440, 480, 520, 560, 600, 640, 680, 720, 760, 800, 840, 880, 920, 960, 1000),
      Vector(1, 3, 4.5, 5, 5.3, 5.6, 5.9, 6.1, 6.35, 6.6, 6.9, 7.2, 7.4, 7.6, 7.8, 8, 8.2, 8.4, 8.6, 8.8, 9, 9.2, 9.4, 9.6, 9.8, 10))


  /*
  @cache('run')
    average life of agricultural inputs 1

    year

    constant

    The average life of agricultural inputs before policy time (ALAI1#100.1)

   */
  val average_life_of_agricultural_inputs_1 = 2


  /*
    @cache('run')

    average life of agricultural inputs 2
    year
    constant

    The average life of agricultural inputs after policy time (ALAI2#100.2) */
  val average_life_of_agricultural_inputs_2 = 2


  /*
  cache('run')

    land yield factor 1
    Dmnl
    constant
    Land yield factor before policy year (LYF1#104.1).
    */
  val land_yield_factor_1 =  1


  /*@cache('step')

    land yield factor 2
    Dmnl
    component
    Land yield factor after policy year (LYF1#104.2). */
  def land_yield_factor_2() =
    smooth(land_yield_technology(), technology_development_delay(), land_yield_technology(), 3)

    //smooth_land_yield_technology_technology_development_delay_land_yield_technology_3()

//  smooth_land_yield_technology_technology_development_delay_land_yield_technology_3 = functions.Smooth(
//    lambda: land_yield_technology(), lambda: technology_development_delay(),
//  lambda: land_yield_technology(), lambda: 3)


  /*
  @cache('step')

    land yield multipler from air pollution 1

    Dmnl
    component
    Land yield multiplier from air pollution before air poll time (LYMAP1#106). */
  def land_yield_multipler_from_air_pollution_1(industrial_output: Double) =
    land_yield_multipler_from_air_pollution_table_1(industrial_output / ind_out_in_1970)



/*
  """
    land yield multipler from air pollution table 1

    Dmnl

    lookup

    Table relating non-persistent pollution from industry to agricultural output (LYMAPT#106.1).
    """
 */
  def land_yield_multipler_from_air_pollution_table_1(x: Double) =
    lookup(x, Vector(0, 10, 20, 30), Vector(1, 1, 0.7, 0.4))



  /*
  @cache('step')

    land yield multiplier from air pollution 2
    Dmnl
    component

    Land yield multiplier from air pollution after air poll time (LYMAP2#107).
   */
  def land_yield_multiplier_from_air_pollution_2(industrial_output: Double) =
    land_yield_multipler_from_air_pollution_table_2(industrial_output / ind_out_in_1970)


  /*
      land yield multipler from air pollution table 2

    Dmnl

    lookup

    Table relating non-persistent pollution from industry to agricultural output (LYMAPT#107.1).

   */
  def land_yield_multipler_from_air_pollution_table_2(x: Double) =
    lookup(x, Vector(0, 10, 20, 30), Vector(1, 1, 0.98, 0.95))




  /*
  @cache('step')

    land yield technology change rate multiplier
    1/year
    component
    Land yield from technology change multiplier (LYCM#--) */
  def land_yield_technology_change_rate_multiplier() =
    land_yield_technology_change_rate_multiplier_table(desired_food_ratio() - food_ratio())

  /*

    land yield technology change rate multiplier table
    1/year
    lookup

    Table relating the food ratio gap to the change in agricultural technology (LYCMT#--).   */
  def land_yield_technology_change_rate_multiplier_table(x: Double) = lookup(x, Vector(0, 1), Vector(0, 0))



  /* @cache('step')

    land yield multiplier from technology
    Dmnl
    component
    Land Yield factor (LYF#104) */
  def land_yield_multiplier_from_technology(time: Double, policy_year: Double) =
    if(time >= policy_year) land_yield_factor_2() else land_yield_factor_1()

  /*
  @cache('step')


    land yield multiplier from air pollution
    Dmnl
    component
    Land yield multiplier from air pollution (LYMAP#105). */

  def land_yield_multiplier_from_air_pollution(time: Double, air_pollution_policy_implementation_time: Double) =
    if(time >= air_pollution_policy_implementation_time) land_yield_multiplier_from_air_pollution_2()
    else land_yield_multipler_from_air_pollution_1()

  /* @cache('run')

    air pollution policy implementation time
    year
    constant
    Air Pollution switch time (ARPTM#--) */
  //val air_pollution_policy_implementation_time = 4000



  /*
  @cache('step')

     Land Yield Technology

    Dmnl

    component

    LAND YIELD TECHNOLOGY INITIATED (LYTD#--)

      integ_land_yield_technology = functions.Integ(lambda: land_yield_technology_change_rate(),
  lambda: 1)

   */
  def land_yield_technology_dot() = land_yield_technology_change_rate() //integ_land_yield_technology()

  val land_yield_technology_init = 1

  /* @cache('step')

    land yield technology change rate
    1/year
    component
    Land yield from technology change rate (LYTDR#--) */
  def land_yield_technology_change_rate(time: Double, policy_year: Double, land_yield_technology: Double) =
    if(time >= policy_year) land_yield_technology * land_yield_technology_change_rate_multiplier() else 0.0

}


object Agriculture3 {


  /*
  @cache('step')

    average life of land
    year
    component

    Average life of land (ALL#112). */
  def average_life_of_land() =
    average_life_of_land_normal * land_life_multiplier_from_land_yield()


  /*@cache('run')

    average life of land normal
    year
    constant

    AVERAGE LIFE OF LAND NORMAL (ALLN#112.1). */
  val average_life_of_land_normal = 1000

  /* @cache('step')


      land erosion rate

      hectare/year

      component

      Land erosion rate (LER#
  */
  def land_erosion_rate(arable_land: Double) = arable_land / average_life_of_land()


  /*@cache('step')


      land removal for urban and industrial use

      hectare/year

      component

      LAND REMOVAL FOR URBAN-INDUSTRIAL USE (LRUI#119).   */
  def land_removal_for_urban_and_industrial_use() =
    math.max(0.0, urban_and_industrial_land_required() - urban_and_industrial_land()) / urban_and_industrial_land_development_time()



  /*  @cache('step')

    land life multiplier from land yield 1
    Dmnl
    component
    Land life multiplier from yield before switch time (LLMY1#114). */
  def land_life_multiplier_from_land_yield_1() =
    land_life_multiplier_from_land_yield_table_1(land_yield() / inherent_land_fertility())


  /*
      land life multiplier from land yield table 1
      Dmnl
      lookup
      Table relating yield to the effect on land life (LLMY1T#114.1). */
  def land_life_multiplier_from_land_yield_table_1(x: Double) =
    lookup(x, Vector(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), Vector(1.2, 1, 0.63, 0.36, 0.16, 0.055, 0.04, 0.025, 0.015, 0.01))


  /* @cache('step')

    land life multiplier from land yield 2
    Dmnl
    component
    Land life multiplier from yield after switch time (LLMY2#115). */
  def land_life_multiplier_from_land_yield_2() =
    land_life_multiplier_from_land_yield_table_2(land_yield() / inherent_land_fertility())


  /*
      land life multiplier from land yield table 2

      Dmnl

      lookup

      Table relating yield to the effect on land life (LLMY2T#115.1).
   */
  def land_life_multiplier_from_land_yield_table_2(x: Double) =
    lookup(x, Vector(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), Vector(1.2, 1, 0.63, 0.36, 0.29, 0.26, 0.24, 0.22, 0.21, 0.2))


  /* @cache('step')

     land life multiplier from land yield
    Dmnl
    component
    LAND LIFE MULTIPLIER FROM YIELD (LLMY#113). */
  def land_life_multiplier_from_land_yield(time: Double, land_life_policy_implementation_time: Double) =
    if(time >= land_life_policy_implementation_time())
    math.pow(0.95, ((time - land_life_policy_implementation_time()) / one_year())) *
      land_life_multiplier_from_land_yield_1() + (1 - 0.95**(
      (time - land_life_policy_implementation_time()) / one_year())) *
      land_life_multiplier_from_land_yield_2(), land_life_multiplier_from_land_yield_1())


  /*
  @cache('run')
   land life policy implementation time

      year

      constant

      Land life multiplier from yield switch time (LLMYTM#--)
   */
  def land_life_policy_implementation_time() = 4000



  /*
  @cache('run')

        urban and industrial land development time

      year

      constant

      Urban industrial land development time (UILDT#119.1).
   */
  def urban_and_industrial_land_development_time() = 10


  /* @cache('step')

     urban and industrial land required per capita
      hectare/Person
      component
      Urban industrial land per capita (UILPC#117). */
  def urban_and_industrial_land_required_per_capita() =
    urban_and_industrial_land_required_per_capita_table(industrial_output_per_capita() / gdp_pc_unit())

  /*
   urban and industrial land required per capita table

  hectare/Person

  lookup

  Table relating industrial output to urban industrial land (UILPCT#117.1) */
  def urban_and_industrial_land_required_per_capita_table(x: Double) =
    lookup(x, Vector(0, 200, 400, 600, 800, 1000, 1200, 1400, 1600), Vector(0.005, 0.008, 0.015, 0.025, 0.04, 0.055, 0.07, 0.08, 0.09))


  /*
  @cache('step')
   """
      urban and industrial land required

      hectare

      component

      Urban industrial land required (UILR#118).
      """
   */
  def urban_and_industrial_land_required(population: Double) =
    urban_and_industrial_land_required_per_capita() * population



  /*
  @cache('step')

  """
      Urban and Industrial Land

      hectare

      component

      URBAN-INDUSTRIAL LAND (UIL#120).


    integ_urban_and_industrial_land = functions.Integ(
      lambda: (land_removal_for_urban_and_industrial_use()),
    lambda: initial_urban_and_industrial_land())

      """
   */
  def urban_and_industrial_land_dot() = land_removal_for_urban_and_industrial_use()


  /* @cache('run')

   initial urban and industrial land
  hectare
  constant
  URBAN-INDUSTRIAL LAND INITIAL (UILI#120.1). */
  def initial_urban_and_industrial_land = 8.2e+06
    //integ_urban_and_industrial_land()

}


object Agriculture4 {


  /*
  @cache('step')

    land fertility degredation

    Veg eq kg/(year*year*hectare)

    component

    LAND FERTILITY DEGRADATION (LFD#123).

   */
  def land_fertility_degredation() =
    land_fertility() * land_fertility_degredation_rate()


  /*@cache('step')

      land fertility degredation rate

      1/year

      component

      Land fertility degradation rate (LFDR#122).
      """
   */
  def land_fertility_degredation_rate() =
    land_fertility_degredation_rate_table(persistent_pollution_index())



  /*

     land fertility degredation rate table

      1/year

      lookup

      Table relating persistent pollution to land fertility degradation (LFDRT#122.1).
      """
 */
  def land_fertility_degredation_rate_table(x: Double) =
    lookup(x, Vector(0, 10, 20, 30), Vector(0, 0.1, 0.3, 0.5))



  /* @cache('step')

      Land Fertility
      Veg eq kg/(year*hectare)
      component
      Land fertility (LFERT#121).

  integ_land_fertility = functions.Integ(
      lambda: (land_fertility_regeneration() - land_fertility_degredation()),
    lambda: initial_land_fertility())
   */
  def land_fertility_dot() = land_fertility_regeneration() - land_fertility_degredation()

  //return integ_land_fertility()


  /* @cache('run')

    initial land fertility
    Veg eq kg/(year*hectare)
    constant
    LAND FERTILITY INITIAL (LFERTI#121.2) */
  val initial_land_fertility = 600


}

object Agriculture5 {

  /*@cache('run')
   """
    inherent land fertility

    Veg eq kg/(year*hectare)

    constant

    INHERENT LAND FERTILITY (ILF#124.1).
    """
   */
  def inherent_land_fertility = 600


  /*
  @cache('step')

    """
    land fertility regeneration

    Veg eq kg/(year*year*hectare)

    component

    Land fertility regeneration (LFR#124).
    """
   */
  def land_fertility_regeneration() = (inherent_land_fertility - land_fertility()) / land_fertility_regeneration_time()


   /* @cache('step')

     """
      land fertility regeneration time

      year

      component

      LAND FERTILITY REGENERATION TIME (LFRT#125)
      """
    */
    def land_fertility_regeneration_time() =
      land_fertility_regeneration_time_table(fraction_of_agricultural_inputs_for_land_maintenance())


  /*
     land fertility regeneration time table
     year
     lookup

     Table relating inputs to land maintenance to land fertility regeneration (LFRTT#125.1) */
   def land_fertility_regeneration_time_table(x: Double) =
    lookup(x, Vector(0, 0.02, 0.04, 0.06, 0.08, 0.1), Vector(20, 13, 8, 4, 2, 2))


}


object Agriculture6 {




  /*@cache('step')

    """
    Perceived Food Ratio

    Dmnl

    component

    PERCEIVED FOOD RATIO (PFR#128).
    """
   */
  def perceived_food_ratio() = smooth_food_ratio_food_shortage_perception_delay_food_ratio_1()

/*
  #Circular init!
    smooth_food_ratio_food_shortage_perception_delay_food_ratio_1 = functions.Smooth(
    lambda: food_ratio(), lambda: food_shortage_perception_delay(), lambda: 1.0,
  lambda: 1)*/

  /*
  @cache('step')
    """
    food ratio

    Dmnl

    component

    FOOD RATIO (FR#127)
    """

  */
  def food_ratio() = food_per_capita() / subsistence_food_per_capita() //, 1)
  val food_ratio_initial = 1


  /* @cache('run')
    """
    food shortage perception delay

    year

    constant

    FOOD SHORTAGE PERCEPTION DELAY (FSPD#128.2)
    """
   */
  def food_shortage_perception_delay() = 2



  /*@cache('step')

    fraction of agricultural inputs for land maintenance
    Dmnl
    component
    FRACTION OF INPUTS ALLOCATED TO LAND MAINTENANCE (FALM#126). */
  def fraction_of_agricultural_inputs_for_land_maintenance() =
    fraction_of_agricultural_inputs_for_land_maintenance_table(perceived_food_ratio())


  /*
  """
      fraction of agricultural inputs for land maintenance table

      Dmnl

      lookup

      Table relating the perceived food ratio to the fraction of input used for land maintenance (FALMT#126.1).
      """
   */
  def fraction_of_agricultural_inputs_for_land_maintenance_table(x: Double) =
    lookup(x, Vector(0, 1, 2, 3, 4), Vector(0, 0.04, 0.07, 0.09, 0.1))


  /*@cache('run')
    """
      subsistence food per capita

      Veg eq kg/(Person*year)

      constant

      Subsistence food per capita (SFPC#127.1).
      """

   */
  val subsistence_food_per_capita = 230


}