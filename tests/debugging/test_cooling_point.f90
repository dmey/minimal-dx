! MinimalDX version 0.1.0 (https://www.github.com/dmey/minimal-dx).
! Copyright 2018 D. Meyer and R. Raustad. Licensed under MIT.

program  test_cooling_point
  !+ Program to compare MinimalDX against original EnergyPlus implementation.

  ! Import fortran 2008 standard to represent double-precision floating-point format
  use, intrinsic :: iso_fortran_env
  ! Add InitializePsychRoutines here to fix the allocatable array is already allocated error
  use Psychrometrics, only: InitializePsychRoutines, PsyRhoAirFnPbTdbW, PsyWFnTdbRhPb
  ! EnergyPlus DX Coil model (original implementation)
  use EPlusWrapperCooling, only : SimCalcDoe2DXCoil
  ! DX Coil model (simplified) using EnergyPlus psychrometric routines
  use MinimalDXCoolingDriver, only: SimMinimalDXCooling

  implicit none

  ! Using fortran 2008 standard to represent double-precision floating-point format
  integer, parameter :: dp = REAL64

  ! The number one ('1') next to the variable description denotes that the variable is dimensionless - e.g.   COP [1]
  real(dp) :: OutdoorTDryBulb
      !+ Outdoor dry bulb air temperature `[°C]`
  real(dp) :: OutdoorHumRatio
      !+ Outdoor air humidity ratio `[kgH₂O kgAIR⁻¹]`
  real(dp) :: OutdoorPressure
      !+ Outdoor barometric pressure `[Pa]`
  real(dp) :: InletTDryBulb
      !+ Indoor (inlet) dry bulb air temperature `[°C]`
  real(dp) :: InletHumRatio
      !+ Indoor (inlet) air humidity ratio `[kgH₂O kgAIR⁻¹]`
  real(dp) :: RatedCOP
      !+ Rated Coefficient Of Performance (COP) `[1]`
  real(dp) :: RatedTotCap
      !+ Rated (total) system capacity `[W]`
  real(dp) :: SensibleCoolingLoad
  !+ Building sensible load to be met `[W]`
  real(dp) :: RatedAirMassFlowRate
      !+ Rated air mass flow rate `[kg s⁻¹]`
  real(dp) :: COP
      !+ Actual (calculated) Coefficient Of Performance (COP) `[1]`
  real(dp) :: TotalCoolingCapacity
      !+ Actual (calculated) total system capacity `[W]`
  real(dp) :: OutletTemperature
      !+ Actual (calculated) outlet air dry bulb temperature existing the cooling coil `[°C]`
  real(dp) :: OutletHumRatio
      !+ Actual (calculated) outlet air humidity ratio existing the cooling coil `[kgH₂O kgAIR⁻¹]`
  real(dp) :: ElecCoolingPower
      !+ Calculated electrical power consumed by the DX unit `[W]`
  real(dp) :: LatCoolingEnergyRate
      ! Total latent cooling energy rate extracted by the coil from the indoor environment `[J kg⁻¹]`
  real(dp) :: TotalCoolingEnergyRate
      !+ Total cooling power of the DX unit (energy rate extracted by DX unit from the indoor environment) `[W]`
  real(dp) :: TotalSensibleHeatOut
      !+ Total power rejected by the evaporator into the outdoor environment
      !+ i.e. TotalCoolingEnergyRate + ElecCoolingPower `[W]`

  ! call InitializePsychRoutines here to fix the allocatable array is already allocated error
  call InitializePsychRoutines

  OutdoorTDryBulb = 20.
  OutdoorPressure = 90000.
  OutdoorHumRatio = PsyWFnTdbRhPb(OutdoorTDryBulb, 0.5, OutdoorPressure)
  InletTDryBulb = 23.
  InletHumRatio = PsyWFnTdbRhPb(InletTDryBulb, 0.5, OutdoorPressure)
  RatedCOP = 3.
  RatedTotCap = 4000.
  SensibleCoolingLoad = 100.
  RatedAirMassFlowRate = 0.00005 * RatedTotCap * PsyRhoAirFnPbTdbW(OutdoorPressure, InletTDryBulb, InletHumRatio)

  call SimCalcDoe2DXCoil(OutdoorTDryBulb, OutdoorHumRatio, OutdoorPressure,                 &
                          InletTDryBulb, InletHumRatio,                                     &
                          RatedCOP, RatedTotCap, SensibleCoolingLoad, RatedAirMassFlowRate, &
                          COP, TotalCoolingCapacity,                                        &
                          OutletTemperature, OutletHumRatio,                                &
                          ElecCoolingPower, LatCoolingEnergyRate,    &
                          TotalCoolingEnergyRate, TotalSensibleHeatOut)

  call SimMinimalDXCooling(OutdoorTDryBulb, OutdoorHumRatio, OutdoorPressure,                 &
                            InletTDryBulb, InletHumRatio,                                     &
                            RatedCOP, RatedTotCap, SensibleCoolingLoad, RatedAirMassFlowRate, &
                            COP, TotalCoolingCapacity,                                        &
                            OutletTemperature, OutletHumRatio,                                &
                            ElecCoolingPower, LatCoolingEnergyRate,    &
                            TotalCoolingEnergyRate, TotalSensibleHeatOut)

end program test_cooling_point
