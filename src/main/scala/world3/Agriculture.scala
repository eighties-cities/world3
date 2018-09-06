//package world3
//
//object Agriculture {
//
////
////  def integ_arable_land(t) =
////    functions.Integ(lambda: land_development_rate()-land_erosion_rate()-land_removal_for_urban_and_industrial_use(), lambda: initial_arable_land())
//
//  /**
//    initial arable land
//    hectare
//    constant
//    The initial amount of land that is arable.                 (ALI#85.2).
//  */
//  val initial_arable_land = 9e+08
//
//  /*
//    Arable Land
//    hectare
//    component
//    Arable land (AL#85).
//  */
//  def arable_land_dot() =
//    land_development_rate() - land_erosion_rate() - land_removal_for_urban_and_industrial_use()
//
//  /*
//    land development rate
//    hectare/year
//    component
//    The land developmen rate (LDR#96). */
//  def land_development_rate() =
//    total_agricultural_investment() * fraction_of_agricultural_inputs_allocated_to_land_development() / development_cost_per_hectare()
//
//  /* cache step
//
//    total agricultural investment
//    $/year
//    component
//    TOTAL AGRICULTURAL INVESTMENT (TAI#92)
//   */
//  def total_agricultural_investment() =
//    industrial_output() * fraction_of_industrial_output_allocated_to_agriculture()
//
//
//
//  /* cache step
//
//    fraction of industrial output allocated to agriculture 1
//    Dmnl
//    component
//    Fraction of industrial output allocated to agriculture before policy time (FIOAA1#94).
//  */
//  def fraction_of_industrial_output_allocated_to_agriculture_1() =
//    fraction_industrial_output_allocated_to_agriculture_table_1(food_per_capita() / indicated_food_per_capita())
//
//  /*
//  """
//    fraction industrial output allocated to agriculture table 1
//
//    Dmnl
//
//    lookup
//
//    Table relating food per capita to the fraction of industrial output allocated to agriculture (FIOAA1T#94.1).
//    """
//
//  def fraction_industrial_output_allocated_to_agriculture_table_1(x: Double):
//
//  return functions.lookup(x, [0, 0.5, 1, 1.5, 2, 2.5], [0.4, 0.2, 0.1, 0.025, 0, 0])
//*/
//
//
//  /*
//  @cache('step')
//  def arable_land():
//  """
//    Arable Land
//
//    hectare
//
//    component
//
//    Arable land (AL#85).
//    """
//  return integ_arable_land()
//
//  integ_arable_land = functions.Integ(lambda: land_development_rate()-land_erosion_rate()-land_removal_for_urban_and_industrial_use(), lambda: initial_arable_land())
//
//  @cache('run')
//  def initial_arable_land():
//  """
//    initial arable land
//
//    hectare
//
//    constant
//
//    The initial amount of land that is arable.                 (ALI#85.2).
//    """
//  return 9e+08
//
//
//  @cache('step')
//  def development_cost_per_hectare():
//  """
//    development cost per hectare
//
//    $/hectare
//
//    component
//
//    Development cost per hectare (DCPH#97).
//    """
//  return development_cost_per_hectare_table(
//    potentially_arable_land() / potentially_arable_land_total())
//
//
//  def development_cost_per_hectare_table(x):
//  """
//    development cost per hectare table
//
//    $/hectare
//
//    lookup
//
//    Table relating undeveloped land to the cost of land development (DCPHT#97.1).
//    """
//  return functions.lookup(x, [0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1],
//  [100000, 7400, 5200, 3500, 2400, 1500, 750, 300, 150, 75, 50])
//
//
//  @cache('step')
//  def food():
//  """
//    food
//
//    Veg eq kg/year
//
//    component
//
//    The total amount of usable food (F#87).
//    """
//  return land_yield() * arable_land() * land_fraction_harvested() * (1 - processing_loss())
//
//
//  @cache('step')
//  def food_per_capita():
//  """
//    food per capita
//
//    Veg eq kg/(Person*year)
//
//    component
//
//    Food per capita (FPC#88)
//    """
//  return food() / population()
//
//
//
//
//
//  @cache('step')
//  def land_fr_cult():
//  """
//    land fr cult
//
//    Dmnl
//
//    component
//
//    Land fraction under cultivarion (LFC#84).
//    """
//  return arable_land() / potentially_arable_land_total()
//
//
//  @cache('run')
//  def land_fraction_harvested():
//  """
//    land fraction harvested
//
//    Dmnl
//
//    constant
//
//    Land fraction harvested (LFH#87.1).
//    """
//  return 0.7
//
//
//
//
//  @cache('step')
//  def fraction_of_industrial_output_allocated_to_agriculture_2():
//  """
//    fraction of industrial output allocated to agriculture 2
//
//    Dmnl
//
//    component
//
//    Fraction of industrial output allocated to agriculture after policy time (FIOAA2#95).
//    """
//  return fraction_industrial_output_allocated_to_agriculture_table_2(
//    food_per_capita() / indicated_food_per_capita())
//
//
//  def fraction_industrial_output_allocated_to_agriculture_table_2(x):
//  """
//    fraction industrial output allocated to agriculture table 2
//
//    Dmnl
//
//    lookup
//
//    Table relating food per capita to the fraction of industrial output allocated to agriculture (FIOAA2T#95.1).
//    """
//  return functions.lookup(x, [0, 0.5, 1, 1.5, 2, 2.5], [0.4, 0.2, 0.1, 0.025, 0, 0])
//
//
//  @cache('step')
//  def indicated_food_per_capita_1():
//  """
//    indicated food per capita 1
//
//    Veg eq kg/(Person*year)
//
//    component
//
//    Indicated foord per capita befor policy time (IFPC1#90).
//    """
//  return indicated_food_per_capita_table_1(industrial_output_per_capita() / gdp_pc_unit())
//
//
//  def indicated_food_per_capita_table_1(x):
//  """
//    indicated food per capita table 1
//
//    Veg eq kg/(Person*year)
//
//    lookup
//
//    Table relating industrial output to indicated food requirements 1 (IFPC1T#90.1).
//    """
//  return functions.lookup(x, [0, 200, 400, 600, 800, 1000, 1200, 1400, 1600],
//  [230, 480, 690, 850, 970, 1070, 1150, 1210, 1250])
//
//
//  @cache('step')
//  def indicated_food_per_capita_2():
//  """
//    indicated food per capita 2
//
//    Veg eq kg/(Person*year)
//
//    component
//
//    Indicated foord per capita after policy time (IFPC1#90).
//    """
//  return indicated_food_per_capita_table_2(industrial_output_per_capita() / gdp_pc_unit())
//
//
//  def indicated_food_per_capita_table_2(x):
//  """
//    indicated food per capita table 2
//
//    Veg eq kg/(Person*year)
//
//    lookup
//
//    Table relating industrial output to indicated food requirements 2 (IFPC1T#90.1).
//    """
//  return functions.lookup(x, [0, 200, 400, 600, 800, 1000, 1200, 1400, 1600],
//  [230, 480, 690, 850, 970, 1070, 1150, 1210, 1250])
//
//
//  @cache('step')
//  def potentially_arable_land():
//  """
//    Potentially Arable Land
//
//    hectare
//
//    component
//
//    POTENTIALLY ARABLE LAND (PAL#86).
//    """
//  return integ_potentially_arable_land()
//
//  integ_potentially_arable_land = functions.Integ(lambda: (-land_development_rate()),
//    lambda: initial_potentially_arable_land())
//
//  @cache('run')
//  def initial_potentially_arable_land():
//  """
//    initial potentially arable land
//
//    hectare
//
//    constant
//
//    The initial amount of potentially arable land (PALI#86.2).
//    """
//  return 2.3e+09
//
//
//  @cache('run')
//  def potentially_arable_land_total():
//  """
//    potentially arable land total
//
//    hectare
//
//    constant
//
//    POTENTIALLY ARABLE LAND TOTAL (PALT#84.1).
//    """
//  return 3.2e+09
//
//
//  @cache('run')
//  def processing_loss():
//  """
//    processing loss
//
//    Dmnl
//
//    constant
//
//    PROCESSING LOSS (PL#87.2)
//    """
//  return 0.1
//
//
//  @cache('step')
//  def fraction_of_industrial_output_allocated_to_agriculture():
//  """
//    fraction of industrial output allocated to agriculture
//
//    Dmnl
//
//    component
//
//    FRACTION OF INDUSTRIAL OUTPUT ALLOCATED TO AGRICULTURE (FIOAA#93).
//    """
//  return functions.if_then_else(time() >= policy_year(),
//    fraction_of_industrial_output_allocated_to_agriculture_2(),
//    fraction_of_industrial_output_allocated_to_agriculture_1())
//
//
//  @cache('step')
//  def indicated_food_per_capita():
//  """
//    indicated food per capita
//
//    Veg eq kg/(Person*year)
//
//    component
//
//    Indicated food per capita (IFPC#89).
//    """
//  return functions.if_then_else(time() >= policy_year(), indicated_food_per_capita_2(),
//    indicated_food_per_capita_1())
//
//
// */
//
//
//
//}
