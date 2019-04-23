! MinimalDX version 0.1.0 (https://www.github.com/dmey/minimal-dx).
! Copyright 2018 D. Meyer and R. Raustad. Licensed under MIT.

module MinimalDXCoolingDriver
  !+ Contains a simplified EnergyPlus subroutine for simulating the performance of a DX cooling coil.


  ! Import Fortran 2008 standard to represent double-precision floating-point format
  use, intrinsic :: iso_fortran_env

  implicit none

  private
  public SimMinimalDXCooling

  contains
  subroutine SimMinimalDXCooling(OutdoorTDryBulb, OutdoorHumRatio, OutdoorPressure,                & ! I
                                 InletTDryBulb, InletHumRatio,                                     & ! I
                                 RatedCOP, RatedTotCap, SensibleCoolingLoad, RatedAirMassFlowRate, & ! I
                                 COP, TotalCoolingCapacity,                                        & ! O
                                 OutletTemperature, OutletHumRatio,                                & ! O
                                 ElecCoolingPower, LatCoolingEnergyRate,                           & ! O
                                 TotalCoolingEnergyRate, TotalSensibleHeatOut)                       ! O

    !+ Simplified EnergyPlus subroutine for calculating the performance of a DX cooling coil.
    !+ This is the subroutine to call from your program.

    use MinimalDXCooling, only: CalcMinimalDXCooling

    ! Using fortran 2008 standard to represent double-precision floating-point format
    integer, parameter :: dp = REAL64

    ! Subroutine arguments
    real(dp), intent(in)    :: OutdoorTDryBulb
        !+ Outdoor dry bulb air temperature `[°C]`
    real(dp), intent(in)    :: OutdoorHumRatio
        !+ Outdoor air humidity ratio `[kgH₂O kgAIR⁻¹]`
    real(dp), intent(in)    :: OutdoorPressure
        !+ Outdoor barometric pressure `[Pa]`
    real(dp), intent(in)    :: InletTDryBulb
        !+ Indoor (inlet) dry bulb air temperature `[°C]`
    real(dp), intent(in)    :: InletHumRatio
        !+ Indoor (inlet) air humidity ratio `[kgH₂O kgAIR⁻¹]`
    real(dp), intent(in)    :: RatedCOP
        !+ Rated Coefficient Of Performance (COP) `[1]`
    real(dp), intent(in)    :: RatedTotCap
        !+ Rated (total) system capacity `[W]`
    real(dp), intent(in)    :: SensibleCoolingLoad
        !+ Building sensible load to be met `[W]`
    real(dp), intent(in)    :: RatedAirMassFlowRate
        !+ rated air mass flow rate `[kg s⁻¹]`
    real(dp), intent(out)    :: COP
    !+ Actual (calculated) Coefficient Of Performance (COP) `[1]`
    real(dp), intent(out)   :: TotalCoolingCapacity
        !+ Actual (calculated) total system capacity `[W]`
    real(dp), intent(out)   :: OutletTemperature
        !+ Actual (calculated) outlet air dry bulb temperature existing the cooling coil `[°C]`
    real(dp), intent(out)   :: OutletHumRatio
        !+ Actual (calculated) outlet air humidity ratio existing the cooling coil `[kgH₂O kgAIR⁻¹]`
    real(dp), intent(out)   :: ElecCoolingPower
        !+ Calculated electrical power consumed by the DX unit `[W]`
    real(dp), intent(out)   :: LatCoolingEnergyRate
        ! Total latent cooling energy rate extracted by the coil from the indoor environment `[J kg⁻¹]`
    real(dp), intent(out)   :: TotalCoolingEnergyRate
        !+ Total cooling power of the DX unit (energy rate extracted by DX unit from the indoor environment) `[W]`
    real(dp), intent(out)   :: TotalSensibleHeatOut
        !+ Total power rejected by the evaporator into the outdoor environment
        !+ i.e. TotalCoolingEnergyRate + ElecCoolingPower `[W]`

    ! Local variables
    real(dp) :: PartLoadRatio
    real(dp) :: SensCoolingEnergyRate
        !+ Sensible cooling power used to calculate the PLR. This is the maximum amount of sensible heat rate that the coil
        !+ is capable of extracting from the indoor environment for the specified conditions. `[W]`

    ! Get SensibleCoolingLoad and SensCoolingEnergyRates to calculate actual PartLoadRatio
    PartLoadRatio = 1.0_dp

    call CalcMinimalDXCooling(OutdoorTDryBulb, OutdoorHumRatio, OutdoorPressure,                & ! I
                              InletTDryBulb, InletHumRatio,                                     & ! I
                              RatedCOP, RatedTotCap, PartLoadRatio, RatedAirMassFlowRate,       & ! I
                              OutletTemperature, OutletHumRatio,                                & ! O
                              ElecCoolingPower, SensCoolingEnergyRate, LatCoolingEnergyRate,    & ! O
                              TotalCoolingEnergyRate, TotalSensibleHeatOut)

    ! Calculate PartLoadRatio from previous call
    PartLoadRatio = SensibleCoolingLoad / SensCoolingEnergyRate

    ! Keep PartLoadRatio  bounded between 0 and 1
    if ( PartLoadRatio < 0.0_dp ) PartLoadRatio = 0.0_dp
    if ( PartLoadRatio > 1.0_dp ) PartLoadRatio = 1.0_dp

    ! Call CalcDoe2DXCoil again with correct part load ratio
    call CalcMinimalDXCooling(OutdoorTDryBulb, OutdoorHumRatio, OutdoorPressure,                & ! I
                              InletTDryBulb, InletHumRatio,                                     & ! I
                              RatedCOP, RatedTotCap, PartLoadRatio, RatedAirMassFlowRate,       & ! I
                              OutletTemperature, OutletHumRatio,                                & ! O
                              ElecCoolingPower, SensCoolingEnergyRate, LatCoolingEnergyRate,    & ! O
                              TotalCoolingEnergyRate, TotalSensibleHeatOut)

    COP = TotalCoolingEnergyRate / ElecCoolingPower
    TotalCoolingCapacity = TotalCoolingEnergyRate / PartLoadRatio


  end subroutine SimMinimalDXCooling
end module MinimalDXCoolingDriver