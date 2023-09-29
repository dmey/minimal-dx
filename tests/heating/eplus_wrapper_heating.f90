! MinimalDX version 0.2.0 (https://www.github.com/dmey/minimal-dx).
! Copyright 2018-2020 D. Meyer and R. Raustad. Licensed under MIT.

module EPlusWrapperHeating

  implicit none

  private
  public :: SimDXHeatingCoil

  contains

  subroutine  SimDXHeatingCoil(OutdoorDryBulb, OutdoorHumRatio, OutdoorPressure,                  & !I
                               InletAirDryBulbTemp, InletAirHumRat,                               & !I
                               RatedCOP, RatedTotCap, SensibleHeatingLoad, RatedAirMassFlowRate,  & !I
                               COP, TotalHeatingCapacity,                                         & !O
                               OutletAirTemp, OutletAirHumRat,                                    & !O
                               ElecHeatingPower, TotalHeatingEnergyRate, TotalSensibleHeatOut)      !O

    use DXCoils, only: CalcDXHeatingCoil, DXCoil, DXCoilFullLoadOutAirTemp, &
                       DXCoilFullLoadOutAirHumRat, DXCoilOutletTemp, &
                       DXCoilOutletHumRat, DXCoilPartLoadRatio, DXCoilFanOpMode, &
                       DXCoilTotalHeating, DXCoilHeatInletAirDBTemp, &
                       DXCoilHeatInletAirWBTemp

    use DataEnvironment, only:  OutBaroPress, OutDryBulbTemp, OutHumRat, OutWetBulbTemp
    use Psychrometrics, only:  PsyTwbFnTdbWPb,  PsyHFnTdbW
    use DataHeatBalance, only:  HeatReclaimDXCoil
    use CurveManager, only:  NumCurves, PerfCurve
    use ScheduleManager, only:  Schedule

    integer, parameter :: r64=kind(1.0d0)

    ! Subroutine arguments
    ! The number one ('1') next to the variable description denotes that the variable is dimensionless - e.g.   COP [1]
    real(r64), intent(in)    :: OutdoorDryBulb
        !+ Outdoor dry bulb air temperature `[°C]`
    real(r64), intent(in)    :: OutdoorHumRatio
        !+ Outdoor air humidity ratio `[kgH₂O kgAIR⁻¹]`
    real(r64), intent(in)    :: OutdoorPressure
        !+ Outdoor barometric pressure `[Pa]`
    real(r64), intent(in)    :: InletAirDryBulbTemp
        !+ Indoor (inlet) dry bulb air temperature `[°C]`
    real(r64), intent(in)    :: InletAirHumRat
        !+ Indoor (inlet) air humidity ratio `[kgH₂O kgAIR⁻¹]`
    real(r64), intent(in)    :: RatedCOP
        !+ Rated Coefficient Of Performance (COP) `[1]`
    real(r64), intent(in)    :: RatedTotCap
        !+ Rated (total) system capacity `[W]`
    real(r64), intent(in)    :: SensibleHeatingLoad
    !+ Building sensible load to be met `[W]`
    real(r64), intent(in)    :: RatedAirMassFlowRate
        !+ Rated air mass flow rate `[kg s⁻¹]`
    real(r64), intent(out)   :: COP
        !+ Actual (calculated) Coefficient Of Performance (COP) `[1]`
    real(r64), intent(out)   :: TotalHeatingCapacity
        !+ Actual (calculated) total system capacity `[W]`
    real(r64), intent(out)   :: OutletAirTemp
        !+ Actual (calculated) outlet air dry bulb temperature existing the cooling coil `[°C]`
    real(r64), intent(out)   :: OutletAirHumRat
        !+ Actual (calculated) outlet air humidity ratio existing the cooling coil `[kgH₂O kgAIR⁻¹]`
    real(r64), intent(out)   :: ElecHeatingPower
        !+ Calculated electrical power consumed by the DX unit `[W]`
    real(r64), intent(out)   :: TotalHeatingEnergyRate
        !+ Total cooling power of the DX unit (energy rate extracted by DX unit from the indoor environment) `[W]`
    real(r64), intent(out)   :: TotalSensibleHeatOut
        !+ Total power rejected by the evaporator into the outdoor environment
        !+ i.e. TotalHeatingEnergyRate + ElecHeatingPower `[W]`

    ! Local variables
    real(r64)              :: PartLoadRatio                     ! Part load ratio                                   [1]
    integer,   parameter   :: DXCoilNum           = 1           ! The number of the DX coil to be simulated         [1]
    integer,   parameter   :: CompOp              = 1           ! Compressor operation; 1=on, 0=off                 [1]
    logical,   parameter   :: FirstHVACIteration  = .TRUE.      ! True if this is the first iteration of HVAC       [1]
    integer,   parameter   :: FanOpMode           = 2           ! Allows parent obj to control fan operation        [1]
    integer,   parameter   :: Resistive           = 2           ! uses electric resistance heater for defrost
    ! Allocate other arrays used within CalcDoe2DXCoil
    ! from DataHeatBalance
    allocate(HeatReclaimDXCoil(1))
    allocate(Schedule(-1:1))
    Schedule(1)%CurrentValue = 1.0

    ! Initialize EnergyPlus global variables
    OutDryBulbTemp  = OutdoorDryBulb
    OutHumRat       = OutdoorHumRatio
    OutBaroPress    = OutdoorPressure
    OutWetBulbTemp  = PsyTwbFnTdbWPb(OutDryBulbTemp, OutHumRat, OutBaroPress)


    ! Set up the DX coil and initialize data
    allocate(DXCoil(1))
    allocate(DXCoilFullLoadOutAirTemp(1))
    allocate(DXCoilFullLoadOutAirHumRat(1))
    allocate(DXCoilOutletTemp(1))
    allocate(DXCoilOutletHumRat(1))
    allocate(DXCoilPartLoadRatio(1))
    allocate(DXCoilFanOpMode(1))
    allocate(DXCoilTotalHeating(1))
    allocate(DXCoilHeatInletAirDBTemp(1))
    allocate(DXCoilHeatInletAirWBTemp(1))

    DXCoil(1)%SchedPtr                      = 1
    DXCoil(1)%RatedTotCap(1)                = RatedTotCap
    DXCoil(1)%RatedSHR(1)                   = 0.8d0                 ! FIXME: find correct value for RatedSHR
    DXCoil(1)%RatedCBF(1)                   = 0.1d0                 ! FIXME: find correct value for RatedCBF
    DXCoil(1)%RatedEIR(1)                   = 1.0d0 / RatedCOP
    DXCoil(1)%BypassedFlowFrac              = 0.0d0
    DXCoil(1)%RatedAirMassFlowRate          = RatedAirMassFlowRate
    DXCoil(1)%InletAirMassFlowRate          = RatedAirMassFlowRate
    DXCoil(1)%InletAirTemp                  = InletAirDryBulbTemp
    DXCoil(1)%InletAirHumRat                = InletAirHumRat
    DXCoil(1)%MinOATCompressor              = -10.0d0
    DXCoil(1)%MaxOATDefrost                 = 0.0d0
    DXCoil(1)%DefrostStrategy               = Resistive

    DXCoil(1)%InletAirEnthalpy = PsyHFnTdbW(InletAirDryBulbTemp, InletAirHumRat)

    ! Performance Curves
    NumCurves = 5
    allocate(PerfCurve(NumCurves))
    ! Capacity as a function of temperature
    DXCoil(1)%CCapFTemp(1)                  = 1
    DXCoil(1)%TotCapTempModFacCurveType(1)  = 4                 ! BiQuadratic curve
    PerfCurve(1)%InterpolationType          = 3                 ! EvaluateCurveToLimits
    PerfCurve(1)%CurveType                  = 4                 ! BiQuadratic
    PerfCurve(1)%Var1Min                    = -10.0d0           ! min inlet air WB temp limit
    PerfCurve(1)%Var2Min                    = -10.0d0           ! min outdoor air DB temp limit
    PerfCurve(1)%Var1Max                    = 50.0d0            ! max inlet air WB temp limit
    PerfCurve(1)%Var2Max                    = 50.0d0            ! max outdoor air DB temp limit
    PerfCurve(1)%CurveMin                   = 0.0d0             ! min value of curve output
    PerfCurve(1)%CurveMax                   = 5.0d0             ! max value of curve output
    PerfCurve(1)%Coeff1                     = 0.876825d0        !- Coefficient1 Constant
    PerfCurve(1)%Coeff2                     = -0.002955d0      !- Coefficient2 x
    PerfCurve(1)%Coeff3                     = -0.000058d0         !- Coefficient3 x2
    PerfCurve(1)%Coeff4                     = 0.025335d0          !- Coefficient4 y
    PerfCurve(1)%Coeff5                     = 0.000196d0       !- Coefficient5 y2
    PerfCurve(1)%Coeff6                     = -0.000043d0        !- Coefficient6 x*y
    PerfCurve(1)%CurveMaxPresent = .TRUE.
    PerfCurve(1)%CurveMinPresent = .TRUE.

    ! Capacity as a function of flow fraction
    DXCoil(1)%CCapFFlow(1)                  = 2
    PerfCurve(2)%InterpolationType          = 3                 ! EvaluateCurveToLimits
    PerfCurve(2)%CurveType                  = 3                 ! Quadratic
    PerfCurve(2)%Var1Min                    = 0.0d0             ! min flow fraction limit
    PerfCurve(2)%Var1Max                    = 1.5d0             ! max flow fraction limit
    PerfCurve(2)%CurveMin                   = 0.0d0             ! min value of curve output
    PerfCurve(2)%CurveMax                   = 2.0d0             ! max value of curve output
    PerfCurve(2)%Coeff1                     = 0.694045465d0    !- Coefficient1 Constant
    PerfCurve(2)%Coeff2                     = 0.474207981d0     !- Coefficient2 x
    PerfCurve(2)%Coeff3                     = -0.168253446d0   !- Coefficient3 x2

    ! EIR as a function of temperature
    DXCoil(1)%EIRFTemp(1)                   = 3
    DXCoil(1)%EIRTempModFacCurveType(1)     = 4                 ! BiQuadratic curve
    PerfCurve(3)%InterpolationType          = 3                 ! EvaluateCurveToLimits
    PerfCurve(3)%CurveType                  = 4                 ! BiQuadratic
    PerfCurve(3)%Var1Min                    = -10.0d0           ! min inlet air WB temp limit
    PerfCurve(3)%Var2Min                    = -10.0d0           ! min outdoor air DB temp limit
    PerfCurve(3)%Var1Max                    = 50.0d0            ! max inlet air WB temp limit
    PerfCurve(3)%Var2Max                    = 50.0d0            ! max outdoor air DB temp limit
    PerfCurve(3)%CurveMin                   = 0.0d0             ! min value of curve output
    PerfCurve(3)%CurveMax                   = 5.0d0             ! max value of curve output
    PerfCurve(3)%Coeff1                     = 0.704658d0       !- Coefficient1 Constant
    PerfCurve(3)%Coeff2                     = 0.008767d0         !- Coefficient2 x
    PerfCurve(3)%Coeff3                     = 0.000625d0       !- Coefficient3 x2
    PerfCurve(3)%Coeff4                     = -0.009037d0      !- Coefficient4 y
    PerfCurve(3)%Coeff5                     = 0.000738d0          !- Coefficient5 y2
    PerfCurve(3)%Coeff6                     = -0.001025d0       !- Coefficient6 x*y
    PerfCurve(3)%CurveMaxPresent = .TRUE.
    PerfCurve(3)%CurveMinPresent = .TRUE.

    ! EIR as a function of flow fraction
    DXCoil(1)%EIRFFlow(1)                   = 4
    PerfCurve(4)%InterpolationType          = 3                 ! EvaluateCurveToLimits
    PerfCurve(4)%CurveType                  = 3                 ! Quadratic
    PerfCurve(4)%Var1Min                    = 0.0d0             ! min flow fraction limit
    PerfCurve(4)%Var1Max                    = 1.5d0             ! max flow fraction limit
    PerfCurve(4)%CurveMin                   = 0.0d0             ! min value of curve output
    PerfCurve(4)%CurveMax                   = 2.0d0             ! max value of curve output
    PerfCurve(4)%Coeff1                     = 2.185418751d0      !- Coefficient1 Constant
    PerfCurve(4)%Coeff2                     = -1.942827919d0    !- Coefficient2 x
    PerfCurve(4)%Coeff3                     = 0.757409168d0     !- Coefficient3 x2

    ! Part load fraction as a function of part load ratio
    DXCoil(1)%PLFFPLR(1)                    = 5
    PerfCurve(5)%InterpolationType          = 3                 ! EvaluateCurveToLimits
    PerfCurve(5)%CurveType                  = 1                 ! Linear
    PerfCurve(5)%Var1Min                    = 0.0d0             ! min inlet air WB temp limit
    PerfCurve(5)%Var1Max                    = 1.0d0             ! max inlet air WB temp limit
    PerfCurve(5)%Coeff1                     = 0.90d0            !- Coefficient1 Constant
    PerfCurve(5)%Coeff2                     = 0.10d0            !- Coefficient2 x

    ! Run initial case with PartLoadRatio = 1 to get SensibleHeatingLoad and
    ! SensCoolingEnergyRates to calculate actual PartLoadRatio
    PartLoadRatio = 1.0d0

    call CalcDXHeatingCoil(DXCoilNum, PartLoadRatio, FanOpMode)

    ! FIXME: for CalcDoe2DXCoil in SimDoe2DXCoil we use the SensCoolingEnergyRate to calculate PLR
    ! However this is not available in Heating as it seems that the TotCap variable is used. Not sure if this makes sense.
    ! Calculate PartLoadRatio from previous call
    PartLoadRatio = SensibleHeatingLoad / DXCoil(1)%TotalHeatingEnergyRate

    ! Keep PartLoadRatio  bounded between 0 and 1
    if ( PartLoadRatio < 0.0d0 ) PartLoadRatio = 0.0d0
    if ( PartLoadRatio > 1.0d0 ) PartLoadRatio = 1.0d0

    ! Call CalcDoe2DXCoil again with correct part load ratio
    call CalcDXHeatingCoil(DXCoilNum, PartLoadRatio, FanOpMode)

    ! Assign return variables of interest
    COP = DXCoil(1)%TotalHeatingEnergyRate / DXCoil(1)%ElecHeatingPower
    TotalHeatingCapacity = DXCoil(1)%TotalHeatingEnergyRate / DXCoilPartLoadRatio(1)
    OutletAirTemp = DXCoil(1)%OutletAirTemp
    OutletAirHumRat = DXCoil(1)%OutletAirHumRat
    ElecHeatingPower = DXCoil(1)%ElecHeatingPower
    TotalHeatingEnergyRate = DXCoil(1)%TotalHeatingEnergyRate
    TotalSensibleHeatOut = DXCoil(1)%ElecHeatingPower - DXCoil(1)%TotalHeatingEnergyRate

    deallocate(HeatReclaimDXCoil)
    deallocate(Schedule)
    deallocate(DXCoil)
    deallocate(DXCoilFullLoadOutAirTemp)
    deallocate(DXCoilFullLoadOutAirHumRat)
    deallocate(DXCoilOutletTemp)
    deallocate(DXCoilOutletHumRat)
    deallocate(DXCoilPartLoadRatio)
    deallocate(DXCoilFanOpMode)
    deallocate(PerfCurve)
    deallocate(DXCoilTotalHeating)
    deallocate(DXCoilHeatInletAirDBTemp)
    deallocate(DXCoilHeatInletAirWBTemp)

  end subroutine SimDXHeatingCoil
end module EPlusWrapperHeating