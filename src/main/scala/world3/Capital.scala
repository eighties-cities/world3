package world3

object CapitalIndustry {

  /*@cache('step')

    industrial capital output ratio multiplier from resource conservation technology
    year
    component
    Technology driven industrial capital output ratio (ICOR2T#--) */
  def industrial_capital_output_ratio_multiplier_from_resource_conservation_technology() =
    industrial_capital_output_ratio_multiplier_from_resource_table(resource_use_factor())


  /*@cache('step')

   """
    industrial capital output ratio multiplier from pollution technology

    Dmnl

    component

    Pollution control technology multiplier for capital output ratio (COPM#--).
    """
   */
  def industrial_capital_output_ratio_multiplier_from_pollution_technology() =
    industrial_capital_output_ratio_multiplier_from_pollution_table(persistent_pollution_generation_factor())


  /*@cache('step')
    """
    industrial capital output ratio multiplier from land yield technology

    Dmnl

    component

    CAPITAL OUTPUT YIELD MULTIPLIER (COYM#--)
    """
   */
  def industrial_capital_output_ratio_multiplier_from_land_yield_technology() =
    industrial_capital_output_ratio_multiplier_table(land_yield_multiplier_from_technology())


  /*@cache('step')
    """
    fraction of industrial output allocated to investment

    Dmnl

    component

    Fraction of industrial output allocated to industry (FIAOI#56).
    """
   */
  def fraction_of_industrial_output_allocated_to_investment() =
    (1 - fraction_of_industrial_output_allocated_to_agriculture() - fraction_of_industrial_output_allocated_to_services() - fraction_of_industrial_output_allocated_to_consumption())



  /*
  @cache('step')

    """
    industrial capital depreciation

    $/year

    component

    Industrial capital depreciation rate (ICDR#53).
    """
   */
  def industrial_capital_depreciation() = industrial_capital() / average_life_of_industrial_capital()


  /*@cache('step')
    """
    industrial capital investment

    $/year

    component

    Industrial capital investment rate (ICIR#55).
    """

   */
  def industrial_capital_investment() = ((industrial_output())) * (fraction_of_industrial_output_allocated_to_investment())


  /*









  def industrial_capital_output_ratio_multiplier_from_resource_table(x):
  """
    industrial capital output ratio multiplier from resource table

    year

    lookup

    CAPITAL OUTPUT FROM RESOURCES technology multiplier TABLE (ICOR2TT#--)
    """
  return functions.lookup(x, [0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1],
  [3.75, 3.6, 3.47, 3.36, 3.25, 3.16, 3.1, 3.06, 3.02, 3.01, 3])


  @cache('step')
  def industrial_output_per_capita():
  """
    industrial output per capita

    $/(Person*year)

    component

    INDUSTRIAL OUTPUT PER CAPITA (IOPC#49)
    """
  return industrial_output() / population()


  @cache('run')
  def industrial_output_per_capita_desired():
  """
    industrial output per capita desired

    $/(Person*year)

    constant

    Industrial output per capita desired (IOPCD#59.2).
    """
  return 400


  @cache('step')
  def industrial_capital():
  """
    Industrial Capital

    $

    component

    INDUSTRIAL CAPITAL (IC#52).
    """
  return integ_industrial_capital()

  integ_industrial_capital = functions.Integ(
    lambda: (industrial_capital_investment() - industrial_capital_depreciation()),
  lambda: initial_industrial_capital())

  @cache('run')
  def initial_industrial_capital():
  """
    initial industrial capital

    $

    constant

    INDUSTRIAL CAPITAL INITIAL (ICI#52.1).
    """
  return 2.1e+11


  @cache('step')
  def industrial_output():
  """
    industrial output

    $/year

    component

    Industrial output (IO#50)
    """
  return (((industrial_capital())) *
    (1 - fraction_of_industrial_capital_allocated_to_obtaining_resources())) * (
    capacity_utilization_fraction()) / industrial_capital_output_ratio()


  @cache('run')
  def average_life_of_industrial_capital_1():
  """
    average life of industrial capital 1

    year

    constant

    Average life of industrial capital before policy year (ALIC1#54.1).
    """
  return 14


  @cache('run')
  def average_life_of_industrial_capital_2():
  """
    average life of industrial capital 2

    year

    constant

    Average life of industrial capital after policy year (ALIC2#54.2)
    """
  return 14


  @cache('step')
  def fraction_of_industrial_output_allocated_to_consumption_constant():
  """
    fraction of industrial output allocated to consumption constant

    Dmnl

    component

    Fraction of output allocated to consumption CONSTANT (FIAOCC#58).
    """
  return functions.if_then_else(
    time() >= policy_year(),
    fraction_of_industrial_output_allocated_to_consumption_constant_2(),
    fraction_of_industrial_output_allocated_to_consumption_constant_1())


  @cache('run')
  def fraction_of_industrial_output_allocated_to_consumption_constant_1():
  """
    fraction of industrial output allocated to consumption constant 1

    Dmnl

    constant

    Fraction of output allocated to consuption constant 1 (FIAOC1#58.1).
    """
  return 0.43


  @cache('run')
  def fraction_of_industrial_output_allocated_to_consumption_constant_2():
  """
    fraction of industrial output allocated to consumption constant 2

    Dmnl

    constant

    Fraction of output allocated to consuption constant 2 (FIAOC1#58.2).
    """
  return 0.43


  @cache('step')
  def fraction_of_industrial_output_allocated_to_consumption_variable():
  """
    fraction of industrial output allocated to consumption variable

    Dmnl

    component

    Fraction industrial output allocated to consumption variable (FIAOCV#59)
    """
  return fraction_of_industrial_output_allocated_to_consumption_variable_table(
    industrial_output_per_capita() / industrial_output_per_capita_desired())


  def fraction_of_industrial_output_allocated_to_consumption_variable_table(x):
  """
    fraction of industrial output allocated to consumption variable table

    Dmnl

    lookup

    Fraction of industrial output allocated to consumption variable TABLE (FIAOCVT#59.1)
    """
  return functions.lookup(x, [0, 0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2],
  [0.3, 0.32, 0.34, 0.36, 0.38, 0.43, 0.73, 0.77, 0.81, 0.82, 0.83])


  @cache('run')
  def industrial_capital_output_ratio_1():
  """
    industrial capital output ratio 1

    year

    constant

    Industrial capital output ratio prior to the policy year (ICOR1#51.1)
    """
  return 3


  @cache('step')
  def industrial_capital_output_ratio_2():
  """
    industrial capital output ratio 2

    year

    component

    Industrial capital output ratio after the policy year (ICOR2#51.2)
    """
  return industrial_capital_output_ratio_multiplier_from_resource_conservation_technology(
  ) * industrial_capital_output_ratio_multiplier_from_land_yield_technology(
  ) * industrial_capital_output_ratio_multiplier_from_pollution_technology()


  def industrial_capital_output_ratio_multiplier_from_pollution_table(x):
  """
    industrial capital output ratio multiplier from pollution table

    Dmnl

    lookup

    Table relating pollution correction technology to the capital output ratio (COPMT#--)
    """
  return functions.lookup(x, [0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1],
  [1.25, 1.2, 1.15, 1.11, 1.08, 1.05, 1.03, 1.02, 1.01, 1, 1])


  @cache('step')
  def average_life_of_industrial_capital():
  """
    average life of industrial capital

    year

    component

    AVERAGE LIFETIME OF INDUSTRIAL CAPITAL (ALIC#54).
    """
  return functions.if_then_else(time() >= policy_year(), average_life_of_industrial_capital_2(),
    average_life_of_industrial_capital_1())


  @cache('step')
  def fraction_of_industrial_output_allocated_to_consumption():
  """
    fraction of industrial output allocated to consumption

    Dmnl

    component

    Fraction of industrial output allocated to consumption (FIAOC#58)
    """
  return functions.if_then_else(
    time() >= industrial_equilibrium_time(),
    fraction_of_industrial_output_allocated_to_consumption_variable(),
    fraction_of_industrial_output_allocated_to_consumption_constant())


  @cache('step')
  def industrial_capital_output_ratio():
  """
    industrial capital output ratio

    year

    component

    INDUSTRIAL CAPITAL-OUTPUT RATIO (ICOR#51)
    """
  return functions.if_then_else(time() >= policy_year(), industrial_capital_output_ratio_2(),
    industrial_capital_output_ratio_1())


  @cache('run')
  def industrial_equilibrium_time():
  """
    industrial equilibrium time

    year

    constant

    INDUSTRIAL EQUILIBRIUM TIME (IET#57.1).
    """
  return 4000


  def industrial_capital_output_ratio_multiplier_table(x):
  """
    industrial capital output ratio multiplier table

    Dmnl

    lookup

    Table relating the yield of technology to the effect on the capital output ratio (COYMT#--)
!
!
    """
  return functions.lookup(x, [1, 1.2, 1.4, 1.6, 1.8, 2], [1, 1.05, 1.12, 1.25, 1.35, 1.5])
*/

}
