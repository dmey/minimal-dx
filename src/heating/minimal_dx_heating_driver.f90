! MinimalDX version 0.2.0 (https://www.github.com/dmey/minimal-dx).
! Copyright 2018-2020 D. Meyer and R. Raustad. Licensed under MIT.

module MinimalDXHeatingDriver
  !+ Contains a simplified EnergyPlus subroutine for simulating the performance of a DX heating coil.

  implicit none

  private
  public SimMinimalDXHeating

  contains
  subroutine SimMinimalDXHeating(OutdoorDryBulb, OutdoorHumRatio, OutdoorPressure,                    & ! I
                                 InletAirDryBulbTemp, InletAirHumRat,                                 & ! I
                                 RatedCOP, RatedTotCap, SensibleHeatingLoad, RatedAirMassFlowRate,    & ! I
                                 COP, TotalHeatingCapacity,                                           & ! O
                                 OutletAirTemp, OutletAirHumRat,                                      & ! O
                                 ElecHeatingPower, TotalHeatingEnergyRate, TotalSensibleHeatOut,      & ! O
                                 FanMode, PrintWarnings)                                                ! *OPTIONAL

    !+ Simplified EnergyPlus subroutine for calculating the performance of a DX heating coil.
    !+ This is the subroutine to call from your program.


    use MinimalDXHeating, only: CalcMinimalDXHeating

    ! Subroutine arguments
    real, intent(in)    :: OutdoorDryBulb
      !+ Outdoor dry bulb air temperature                             [°C]
    real, intent(in)    :: OutdoorHumRatio
      !+ Outdoor air humidity ratio                                   [kgH₂O kgAIR⁻¹]
    real, intent(in)    :: OutdoorPressure
      !+ Outdoor barometric pressure                                  [Pa]
    real, intent(in)    :: InletAirDryBulbTemp
      !+ Indoor (inlet) dry bulb air temperature                      [°C]
    real, intent(in)    :: InletAirHumRat
      !+ Indoor (inlet) air humidity ratio                            [kgH₂O kgAIR⁻¹]
    real, intent(in)    :: RatedCOP
      !+ Rated Coefficient Of Performance (COP)                       [1]
    real, intent(in)    :: RatedTotCap
      !+ Rated (total) system capacity                                [W]
    real, intent(in)    :: SensibleHeatingLoad
      !+ Building sensible load to be met                             [W]
    real, intent(in)    :: RatedAirMassFlowRate
      !+ Rated air mass flow rate                                     [kg s⁻¹]
    real, intent(out)   :: COP
      !+ Actual (calculated) Coefficient Of Performance (COP)         [1]
    real, intent(out)   :: TotalHeatingCapacity
      !+ Actual (calculated) total system capacity                    [W]
    real, intent(out)   :: OutletAirTemp
      !+ Actual (calculated) outlet air dry bulb temperature
      !+ existing the cooling coil                                    [°C]
    real, intent(out)   :: OutletAirHumRat
      !+ Actual (calculated) outlet air humidity ratio
      !+ existing the cooling coil                                    [kgH₂O kgAIR⁻¹]
    real, intent(out)   :: ElecHeatingPower
      !+ Calculated electrical power consumed by the DX unit          [W]
    real, intent(out)   :: TotalHeatingEnergyRate
      !+ Total cooling power of the DX unit (energy rate
      !+ extracted by DX unit from the indoor environment)            [W]
    real, intent(out)   :: TotalSensibleHeatOut
      !+ Total energy rate rejected by the evaporator
      !+ into the outdoor environment
      !+ i.e. TotalHeatingEnergyRate + ElecHeatingPower               [W]

    ! Optional arguments with default value
    integer, optional :: FanMode
      !+ Fan mode of operation: 1 for on, 0 for off                   [-]
    logical, optional :: PrintWarnings
      !+ Whether to print warnings to standard output.                [-]

    ! Local variables
    real :: PartLoadRatio
      !+ Part load ratio (PLR). This is the actual heating produced
      !+ by the AC unit divided by the maximum heatng  available
      !+ i.e. `PLR = (SensibleHeatingLoad / TotalHeatingEnergyRate)`  [1]

    ! Get SensibleHeatingLoad and SensCoolingEnergyRates to calculate actual PartLoadRatio
    PartLoadRatio = 1.0

    call CalcMinimalDXHeating(OutdoorDryBulb, OutdoorHumRatio, OutdoorPressure,               & ! I
                              InletAirDryBulbTemp, InletAirHumRat,                            & ! I
                              RatedCOP, RatedTotCap, PartLoadRatio, RatedAirMassFlowRate,     & ! I
                              OutletAirTemp, OutletAirHumRat,                                 & ! O
                              ElecHeatingPower, TotalHeatingEnergyRate, TotalSensibleHeatOut, & ! O
                              FanMode, PrintWarnings)                                           ! *OPTIONAL

    ! Calculate PartLoadRatio from previous call
    PartLoadRatio = SensibleHeatingLoad / TotalHeatingEnergyRate

    ! Keep PartLoadRatio  bounded between 0 and 1
    if ( PartLoadRatio < 0.0 ) PartLoadRatio = 0.0
    if ( PartLoadRatio > 1.0 ) PartLoadRatio = 1.0

    call CalcMinimalDXHeating(OutdoorDryBulb, OutdoorHumRatio, OutdoorPressure,               & ! I
                              InletAirDryBulbTemp, InletAirHumRat,                            & ! I
                              RatedCOP, RatedTotCap, PartLoadRatio, RatedAirMassFlowRate,     & ! I
                              OutletAirTemp, OutletAirHumRat,                                 & ! O
                              ElecHeatingPower, TotalHeatingEnergyRate, TotalSensibleHeatOut, & ! O
                              FanMode, PrintWarnings)                                           ! *OPTIONAL

    ! Calculate the actual COP for the DX unit under specified conditions
    COP = TotalHeatingEnergyRate / ElecHeatingPower
    TotalHeatingCapacity = TotalHeatingEnergyRate / PartLoadRatio

  end subroutine SimMinimalDXHeating
end module MinimalDXHeatingDriver