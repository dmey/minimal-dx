! MinimalDX version 0.2.0 (https://www.github.com/dmey/minimal-dx).
! Copyright 2018-2020 D. Meyer and R. Raustad. Licensed under MIT.
!
! Description
! This module contains a simplified EnergyPlus subroutine for calculating the performance
! of a DX heating coil `CalcDXHeatingCoil`. Modified by D. Meyer and R. Raustad (2018).
!
! Copyright notice
! EnergyPlus, Copyright (c) 1996-2019, The Board of Trustees of the University of Illinois,
! The Regents of the University of California, through Lawrence Berkeley National Laboratory
! (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
! National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
! contributors. All rights reserved.
!
! NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
! U.S. Government consequently retains certain rights. As such, the U.S. Government has been
! granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
! worldwide license in the Software to reproduce, distribute copies to the public, prepare
! derivative works, and perform publicly and display publicly, and to permit others to do so.
!
! Redistribution and use in source and binary forms, with or without modification, are permitted
! provided that the following conditions are met:
!
! (1) Redistributions of source code must retain the above copyright notice, this list of
!     conditions and the following disclaimer.
!
! (2) Redistributions in binary form must reproduce the above copyright notice, this list of
!     conditions and the following disclaimer in the documentation and/or other materials
!     provided with the distribution.
!
! (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
!     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
!     used to endorse or promote products derived from this software without specific prior
!     written permission.
!
! (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
!     without changes from the version obtained under this License, or (ii) Licensee makes a
!     reference solely to the software portion of its product, Licensee must refer to the
!     software as "EnergyPlus version X" software, where "X" is the version number Licensee
!     obtained under this License and may not use a different name for the software. Except as
!     specifically required in this Section (4), Licensee shall not use in a company name, a
!     product name, in advertising, publicity, or other promotional activities any name, trade
!     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
!     similar designation, without the U.S. Department of Energy's prior written consent.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
! IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
! AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
! CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
! CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
! THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
! OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.

module MinimalDXHeating
  !+ Contains a simplified EnergyPlus subroutine for calculating the performance of a DX heating coil.

  implicit none

  private
  public CalcMinimalDXHeating

  contains
  subroutine CalcMinimalDXHeating(OutdoorDryBulb, OutdoorHumRatio, OutdoorPressure,               & ! I
                                  InletDryBulbTemp, InletAirHumRat,                               & ! I
                                  RatedCOP, RatedTotCap, PartLoadRatio, RatedAirMassFlowRate,     & ! I
                                  OutletAirTemp, OutletAirHumRat,                                 & ! O
                                  ElecHeatingPower, TotalHeatingEnergyRate, TotalSensibleHeatOut, & ! O
                                  FanMode, PrintWarnings)                                           ! *OPTIONAL

    !+ Simplified EnergyPlus subroutine for calculating the performance of a DX heating coil.
    !+ Adapted from EnergyPlus `CalcDXHeatingCoil` by D.Meyer and R. Raustad (2018).
    !+
    !+####ORIGINAL ENERGY PLUS SUBROUTINE INFORMATION:
    !+       AUTHOR         Richard Raustad
    !+       DATE WRITTEN   October 2001
    !+       MODIFIED       Raustad/Shirey Mar 2004
    !+                      Kenneth Tang 2004 (Sensitivity of TotCapTempModFac & EIRTempModFac  to indoor dry bulb temp)
    !+                      Feb 2005 M. J. Witte, GARD Analytics, Inc.
    !+                        Add new coil type COIL:DX:MultiMode:CoolingEmpirical:
    !+       RE-ENGINEERED  na
    !+
    !+####PURPOSE OF THIS SUBROUTINE:
    !+ Calculates the air-side heating performance and electrical heating energy
    !+ use of a direct-expansion, air-cooled heat pump unit.
    !+
    !+####METHODOLOGY EMPLOYED:
    !+ This routine simulates the performance of air-cooled DX heating equipment.
    !+ The routine requires the user to enter the total heating capacity
    !+ and COP for the unit at ARI 210/240 rating conditions (21.11C [70F] dry-bulb,
    !+ 15.55C [60F] wet-bulb air entering the heating coil, 8.33C [47F] dry-bulb,
    !+ 6.11C [43F] wet-bulb air entering the outdoor condenser. Since different
    !+ manufacturer's rate their equipment at different air flow rates, the supply
    !+ air flow rate corresponding to the rated capacities and rated COP must also
    !+ be entered (should be between 300 cfm/ton and 450 cfm/ton). The rated information
    !+ entered by the user should NOT include the thermal or electrical impacts of the
    !+ supply air fan, as this is addressed by another module.
    !+
    !+ With the rated performance data entered by the user, the model employs some of the
    !+ DOE-2.1E curve fits to adjust the capacity and efficiency of the unit as a function
    !+ of outdoor air temperatures and supply air flow rate (actual vs rated flow). The
    !+ model does NOT employ the exact same methodology to calculate performance as DOE-2,
    !+ although some of the DOE-2 curve fits are employed by this model.
    !+
    !+####REFERENCES:
    !+
    !+ Winkelmann, F.C., Birdsall, B.E., Buhl W.F., Ellington, K.L., Erdem, A.E. 1993.
    !+ DOE-2 Supplement Version 2.1E.  Energy and Environment Division, Larwence Berkely
    !+ Laboratory.
    !+
    !+ Henderson, H.I. Jr., Y.J. Huang and Danny Parker. 1999. Residential Equipment Part
    !+ Load Curves for Use in DOE-2.  Environmental Energy Technologies Division, Ernest
    !+ Orlando Lawrence Berkeley National Laboratory.
    !+
    !+####LINKS:
    !+
    !+ 1.
    !+ <https://bigladdersoftware.com/epx/docs/8-7/engineering-reference/coils.html#single-speed-dx-heating-coil-standard-ratings>
    !+
    !+ 2.
    !+ <https://github.com/NREL/EnergyPlusRelease/blob/1ba8474958dbac5a371362731b23310d40e0635d/SourceCode/DXCoil.f90#L10082-L10509>

    use PsychroWrapper, only: InitPsychrometrics, GetMoistAirDensity,           &
                              GetTWetBulbFromHumRatio, GetMoistAirEnthalpy,     &
                              GetHumRatioFromEnthalpyAndTDryBulb,               &
                              GetTDryBulbFromEnthalpyAndHumRatio,               &
                              GetRelHumFromHumRatio, GetHumRatioFromTDewPoint,  &
                              PsyTsatFnHPb

    use MinimalDXFan, only: GetOnOffFan

    ! Subroutine Arguments
    real, intent(in)    :: OutdoorDryBulb
      !+ Outdoor (environmental) air temperature                            [°C]
    real, intent(in)    :: OutdoorHumRatio
      !+ Outdoor (environmental) air humidity ratio                         [kg kg⁻¹]
    real, intent(in)    :: OutdoorPressure
      !+ Outdoor (environmental) pressure                                   [Pa]
    real, intent(in)    :: InletDryBulbTemp
      !+ Actual inlet air temperature                                       [°C]
    real, intent(in)    :: InletAirHumRat
      !+ Actual inlet air humidity ratio                                    [kg kg⁻¹]
    real, intent(in)    :: RatedCOP
      !+ Rated COP                                                          [1]
    real, intent(in)    :: RatedTotCap
      !+ Rated capacity                                                     [W]
    real, intent(in)    :: PartLoadRatio
      !+ Part load ratio (PLR). This is the actual heating produced
      !+ by the AC unit divided by the maximum heatng  available
      !+ i.e. `PLR = (SensibleHeatingLoad / TotalHeatingEnergyRate)`        [1]
    real, intent(in)    :: RatedAirMassFlowRate
      !+ HVAC air mass flow rate                                            [kg s⁻¹]
    real, intent(out)   :: OutletAirTemp
      !+ Actual outlet temperature                                          [°C]
    real, intent(out)   :: OutletAirHumRat
      !+ Actual outlet humidity ratio                                       [kg kg⁻¹]
    real, intent(out)   :: ElecHeatingPower
      !+ Electrical power consumed by the DX unit                           [W]
    real, intent(out)   :: TotalHeatingEnergyRate
      !+ Total energy supplied by the DX unit                               [W]
    real, intent(out)   :: TotalSensibleHeatOut
      !+ Total sensible heat removed by the evaporator                      [W]

    ! Optional arguments with default value
    integer, optional :: FanMode
      !+ Fan mode of operation: 1 for on, 0 for off                         [-]
    logical, optional :: PrintWarnings
      !+ Whether to print warnings to standard output.                      [-]

    ! Local variables
    integer :: FanModeLocal
      ! Fan mode of operation: 1 for on, 0 for off, default to 0.
      ! See below.                                                          [-]
    logical :: PrintWarningsLocal
      ! If PrintWarnings is not set, default to true. See below.            [-]
    real :: IndoorAirDensity
      ! Air density of moist air                                            [kg m⁻³]
    real :: InletAirDryBulbTemp
      ! Indoor (inlet) air dry bulb temperature                             [°C]
    real :: InletAirEnthalpy
      ! Enthalpy at inlet dry-bulb and outlet humidity ratio                [J kg⁻¹]
    real :: FullLoadOutAirEnth
      ! Outlet air enthalpy at full load conditions                         [J kg⁻¹]
    real :: FullLoadOutAirHumRat
      ! Outlet air humidity ratio at full load conditions                   [kgH₂O kgAIR⁻¹]
    real :: FullLoadOutAirTemp
      ! Outlet dry bulb air temperature at full load conditions             [°C]
    real :: OutletAirEnthalpy
      ! Supply air enthalpy (average value for constant fan)                [J kg⁻¹]
    real :: TotCapFlowModFac
      ! Total heating capacity modifier curve function of flow fraction     [1]
    real :: TotCapTempModFac
      ! Total heating capacity modifier curve function temperature          [1]
    real :: EIRTempModFac
      ! Energy input ratio modifier curve function of temperature           [1]
    real :: EIRFlowModFac
      ! Energy input ratio modifier curve function of flow fraction         [1]
    real :: TotCap
      ! Actual capacity                                                     [W]
    real :: EIR
      ! EIR at part load and off rated conditions                           [1]
    real :: PLF
      ! Part load factor, accounts for thermal lag at compressor
      ! startup, used in power calculation                                  [1]
    real :: OutdoorCoilT
      ! Outdoor coil temperature                                            [°C]
    real :: OutdoorCoildw
      ! Outdoor coil delta w assuming coil temperature of OutdoorCoilT      [kg kg⁻¹]
    real :: LoadDueToDefrost
      ! Additonal load due to defrost                                       [1]
    real :: HeatingCapacityMultiplier
      ! Multiplier for heating capacity when system is in defrost           [1]
    real :: FractionalDefrostTime
      ! Fraction of time step when system is in defrost                     [1]
    real :: InputPowerMultiplier
      ! Multiplier for poer when system is in defrost                       [1]
    real :: PLRHeating
      ! Part load ratio in heating                                          [1]
    real :: DefrostPower
      ! Defrost power                                                       [W]
    real :: FullLoadOutAirRH
      ! Outler relative humidity at full load                               [1]
    real :: HeatingCoilRuntimeFraction
      ! Run time fraction of the DX heating unit                            [1]
    real :: DefrostEIRTempModFac
      ! EIR modifier for defrost
      ! (function of entering wetbulb, outside drybulb)                     [1]
    real :: InletAirWetbulbC
      ! Wetbulb temperature of inlet air                                    [°C]
    real :: AirMassFlowRate
      ! Air mass flow rate use in the subroutine for calculations           [kg s⁻¹]
    real :: FanPower
      ! Power of the fan to be simulated                                    [W]

    ! Local parameters
    character (len=12) :: DefrostStrategy = 'Resistive'
      ! TODO: ReverseCycle cannot be currently used due to missing DefrostEIRFT coefficients

    real, parameter     :: MaxOATDefrost    = 0.0
      ! Maximum OAT for defrost operation                                   [°C]
    real, parameter     :: MinOATCompressor = -10.0
      ! Minimum OAT for heat pump compressor operation                      [°C]
    real, parameter     :: DefrostCapacity  = 1000.0 ! FIXME: For now set the heater to 1000 W
      ! Resistive defrost to nominal capacity (at 21.11C/8.33C) ratio       [W]
    real, parameter     :: AirMassFlowRatio = 1.0
      ! Ratio of compressor on airflow to average timestep airflow          [1]
      ! Set to 1. Used only by DX coils with different air flow during heating
      ! and when no heating is required (constant fan, fan speed changes)
    real, parameter     :: MotEff = 0.75
      ! Fan motor efficiency                                                [1]
    real, parameter     :: MotInAirFrac = 1.0
      ! Fraction of motor heat entering air stream                          [1]

    ! Performance curves coefficients taken from:
    ! Cutler, D., Winkler, J., Kruis, N., Christensen, C., & Brendemuehl, M. (2013).
    ! Improved Modeling of Residential Air Conditioners and Heat Pumps for Energy Calculations.
    ! Office of Scientific and Technical Information (OSTI). https://doi.org/10.2172/1067909

    ! Coefficients for HPHeatingCAPFTemp -- Total heating capacity function of temperature curve (bi-quadratic).
    ! Minimum and maximum values of x and y are 0 and 50 respectively with curve output in rage 0 to 5.
    ! Source: Table 18. HP Total Capacity Coefficients as a Function of Operating Temperatures (°C) in Cutler et al. (2013).
    real, parameter :: A1 = 0.876825
      ! Coefficient1 Constant
    real, parameter :: B1 = -0.002955
      ! Coefficient2 x
    real, parameter :: C1 = -0.000058
      ! Coefficient3 x**2
    real, parameter :: D1 = 0.025335
      ! Coefficient4 y
    real, parameter :: E1 = 0.000196
      ! Coefficient5 y**2
    real, parameter :: F1 = -0.000043
      ! Coefficient6 x*y
    real, parameter :: HPHeatingCAPFTempMin = 0.0 !TODO: find curves Min Max
      ! Minimum curve output value
    real, parameter :: HPHeatingCAPFTempMax = 5.0
      ! Maximum curve output value

    ! Coefficients for HPHeatingCAPFFF -- total heating capacity function of flow fraction curve (quadratic).
    ! Minimum and maximum values of x are 0 and 1.5 respectively with curve output in range 0 to 2.
    ! Source: Table 14. HP Total Capacity Coefficients as a Function of Flow Fraction in Cutler et al. (2013).
    real, parameter :: A2 = 0.694045465
      ! Coefficient1 Constant
    real, parameter :: B2 = 0.474207981
      ! Coefficient2 x
    real, parameter :: C2 = -0.168253446
      ! Coefficient3 x**2
    real, parameter :: HPHeatingCAPFFFMin = 0.0
      ! Minimum curve output value
    real, parameter :: HPHeatingCAPFFFMax = 2.0
      ! Maximum curve output value

    ! Coefficients for HPHeatingEIRFTemp -- Energy input ratio function of temperature curve (bi-quadratic).
    ! Minimum and maximum values of x and y are 0 and 50 respectively with curve output in rage 0 to 5.
    ! Source: Table 19. HP EIR Coefficients as a Function of Operating Temperatures (°C) in Cutler et al. (2013).
    real, parameter :: A3 = 0.704658
      ! Coefficient1 Constant
    real, parameter :: B3 = 0.008767
      ! Coefficient2 x
    real, parameter :: C3 = 0.000625
      ! Coefficient3 x**2
    real, parameter :: D3 = -0.009037
      ! Coefficient4 y
    real, parameter :: E3 = 0.000738
      ! Coefficient5 y**2
    real, parameter :: F3 = -0.001025
      ! Coefficient6 x*y
    real, parameter :: HPHeatingEIRFTempMin = 0.0
      ! Minimum curve output value
    real, parameter :: HPHeatingEIRFTempMax = 5.0
      ! Maximum curve output value

    ! Coefficients for EIRFlowModFac -- Energy input ratio function of flow fraction curve (quadratic).
    ! Minimum and maximum values of x are 0 and 1.5 respectively with curve output in range 0 to 2.
    ! Source: Table 15. HP EIR Performance Curve Coefficients as a Function of Flow Fraction in Cutler et al. (2013).
    real, parameter :: A4 = 2.185418751
      ! Coefficient1 Constant
    real, parameter :: B4 = -1.942827919
      ! Coefficient2 x
    real, parameter :: C4 = 0.757409168
      ! Coefficient3 x**2
    real, parameter :: HPHeatingEIRFFFMin = 0.0
      ! Minimum curve output value
    real, parameter :: HPHeatingEIRFFFMax = 2.0
      ! Maximum curve output value

    ! Part Load Fraction curve (quadratic) as a function of Part Load Ratio is default from
    ! Table 6. BEopt AC Rated Value Inputs of NREL report NREL/TP-5500-56354
    ! Minimum and maximum values of x are 0 and 1.5 respectively
    real, parameter :: A5 = 0.90            !- Coefficient1 Constant
    real, parameter :: B5 = 0.10            !- Coefficient2 x
    real, parameter :: C5 = 0.0             !- Coefficient3 x**2

    ! FIXME: the coefficients are set to zero as no data is available.
    ! the DefrostEIRTempModFac curve is only being used for testing purposes.
    ! TODO: find coefficients for DefrostEIRTempModFac
    real, parameter     :: A6 = 1.0          !- Coefficient1 Constant
    real, parameter     :: B6 = 0.0          !- Coefficient2 x
    real, parameter     :: C6 = 0.0          !- Coefficient3 x**2
    real, parameter     :: D6 = 0.0          !- Coefficient4 y
    real, parameter     :: E6 = 0.0          !- Coefficient5 y**2
    real, parameter     :: F6 = 0.0          !- Coefficient6 x*y

    if (present(FanMode)) then
      FanModeLocal = FanMode
    else
      FanModeLocal = 0
    end if
    if (present(PrintWarnings)) then
      PrintWarningsLocal = PrintWarnings
    else
      PrintWarningsLocal = .true.
    end if
    call InitPsychrometrics()

    ! Calculate air density of indoor air using outdoor pressure. Assume indoor pressure = outdoor pressure
    IndoorAirDensity = GetMoistAirDensity(InletDryBulbTemp, InletAirHumRat, OutdoorPressure)

    ! Check that the part load ratio is greater than 0 (i.e. DX unit is off) else just pass through conditions.
    if ((PartLoadRatio > 0) .AND. (OutdoorDryBulb > MinOATCompressor)) then
      ! Set the rated mass flow rate equal the mass flow rate used in the subroutine then check
      ! that the air mass flow rate is within bounds else set air mass flow rate accordingly
      AirMassFlowRate = RatedAirMassFlowRate
      if (AirMassFlowRate / IndoorAirDensity / RatedTotCap < 0.00004027) then
        AirMassFlowRate = 0.00004027 * RatedTotCap * IndoorAirDensity
        if (PrintWarningsLocal) then
          print *, 'Warning: air mass flow rate must be greater than 0.00004027m3/s/W'
          print *, 'Resetting the air mass flow rate to: ', AirMassFlowRate, ' kg/s'
        end if
      else if (AirMassFlowRate / IndoorAirDensity / RatedTotCap > 0.00006041) then
        AirMassFlowRate = 0.00006041 * RatedTotCap * IndoorAirDensity
        if (PrintWarningsLocal) then
          print *, 'Warning: air mass flow rate must be lower than 0.00006041m3/s/W'
          print *, 'Resetting the air mass flow rate to: ', AirMassFlowRate, ' kg/s'
        end if
      end if

      ! Modify the inlet air temperature to account for heat added by the fan motor
      ! The fan power is assumed to be 0.04151 W/W of the rated capacity
      FanPower = 0.04151 * RatedTotCap
      ! GetOnOffFan returns enthaply therefore we calculate an updated value of InletAirDryBulbTemp that
      ! accounts for the added heat released by the fan.
      InletAirEnthalpy = GetMoistAirEnthalpy(InletDryBulbTemp, InletAirHumRat)
      InletAirEnthalpy = GetOnOffFan(FanModeLocal, MotEff, FanPower, MotInAirFrac, InletAirEnthalpy, AirMassFlowRate)
      InletAirDryBulbTemp = GetTDryBulbFromEnthalpyAndHumRatio(InletAirEnthalpy, InletAirHumRat)
      InletAirWetbulbC = GetTWetBulbFromHumRatio(InletAirDryBulbTemp,InletAirHumRat,OutdoorPressure)

      ! Assuming no condensation -> no moisture being extracted from either the
      ! indoor or the outdoor environment -> sensible heating/heating only.
      ! InletAirDryBulbTemp is the dry bulb temperature of the air entering the indoor coil
      ! OutdoorDryBulb is the dry bulb temperature of the air entering the outdoor coil
      ! Total heating capacity modifier curve function of temperature for off-rated conditions
      TotCapTempModFac = A1 + B1 * InletAirDryBulbTemp + C1 * InletAirDryBulbTemp**2        &
                        + D1 * OutdoorDryBulb  + E1 * OutdoorDryBulb**2   &
                        + F1 * InletAirDryBulbTemp * OutdoorDryBulb

      ! Limit the heating capacity modifier curve function of temperature to the its set bounds
      if (TotCapTempModFac < HPHeatingCAPFTempMin) then
        TotCapTempModFac = HPHeatingCAPFTempMin
        if (PrintWarningsLocal) then
          print *, 'Warning: the total heating capacity modifier curve function of temperature exceeds its set bounds'
          print *, 'The curve has been reset to: ', HPHeatingCAPFTempMin
        end if
      else if (TotCapTempModFac > HPHeatingCAPFTempMax) then
        TotCapTempModFac = HPHeatingCAPFTempMax
        if (PrintWarningsLocal) then
          print *, 'Warning: the total heating capacity modifier curve function of temperature exceeds its set bounds'
          print *, 'The curve has been reset to: ', HPHeatingCAPFTempMax
        end if
      end if

      ! Get total capacity modifying factor (function of mass flow) for off-rated conditions
      ! AirMassFlowRatio = AirMassFlow / RatedAirMassFlowRate
      ! Total heating capacity modifier curve function of flow fraction
      TotCapFlowModFac = A2 + B2 * AirMassFlowRatio + C2 * AirMassFlowRatio**2

      ! Limit the heating capacity modifier curve to the its set bounds
      if (TotCapFlowModFac < HPHeatingCAPFFFMin) then
        TotCapFlowModFac = HPHeatingCAPFFFMin
        if (PrintWarningsLocal) then
          print *, 'Warning: the total heating capacity modifier curve function of flow fraction exceeds its set bounds'
          print *, 'The curve has been reset to: ', HPHeatingCAPFFFMin
        end if
      else if (TotCapFlowModFac > HPHeatingCAPFFFMax) then
        TotCapFlowModFac = HPHeatingCAPFFFMax
        if (PrintWarningsLocal) then
          print *, 'Warning: the total heating capacity modifier curve function of flow fraction exceeds its set bounds'
          print *, 'The curve has been reset to: ', HPHeatingCAPFFFMax
        end if
      end if

      ! Calculate total heating capacity for off-rated conditions
      TotCap = RatedTotCap * TotCapFlowModFac * TotCapTempModFac

      ! Calculating adjustment factors for defrost
      ! Calculate delta w through outdoor coil by assuming a coil temp of 0.82*DBT-9.7(F) per DOE2.1E
      OutdoorCoilT = 0.82 * OutdoorDryBulb - 8.589
      OutdoorCoildw = max( 1.0d-6, &
                          (OutdoorHumRatio - GetHumRatioFromTDewPoint(OutdoorCoilT , OutdoorPressure)) )

      ! Initializing defrost adjustment factors
      LoadDueToDefrost            = 0.0
      HeatingCapacityMultiplier   = 1.00
      FractionalDefrostTime       = 0.00
      InputPowerMultiplier        = 1.00
      PLRHeating                  = 0.00
      DefrostPower                = 0.00

      ! Check outdoor temperature to determine of defrost is active
      ! If the outdoor dry bulb temperature, defrost adjustment should be active (MaxOATDefrost = 0).
      if (OutdoorDryBulb <= MaxOATDefrost) then
        ! Calculate defrost adjustment factors assuming defrost control is on-demand
        FractionalDefrostTime = 1.00 / (1.00 + 0.014460 / OutdoorCoildw)
        HeatingCapacityMultiplier = 0.8750 * ( 1.00 - FractionalDefrostTime)
        InputPowerMultiplier = 0.9540 * ( 1.00 - FractionalDefrostTime)

        ! TODO: ReverseCycle is currently deactivated - will need to find DefrostEIRFT coefficients
        !       before using this option. This means that we will always imply resistive defrost below MaxOATDefrost
        if (FractionalDefrostTime > 0.00) then
          if ( trim(adjustl(DefrostStrategy)) == 'ReverseCycle') then
              LoadDueToDefrost = ( 0.010 * FractionalDefrostTime) * (7.2220 - OutdoorDryBulb) * &
                                    (RatedTotCap/1.016670 )
              DefrostEIRTempModFac = A6 + max(15.5550,InletAirWetbulbC) * (B6 + C6 * max(15.5550,InletAirWetbulbC))   &
                                        + max(15.5550,OutdoorDryBulb) * (D6 + E6 * max(15.5550,OutdoorDryBulb))     &
                                        + max(15.5550,InletAirWetbulbC) * max(15.5550,OutdoorDryBulb) * F6
              DefrostPower =  DefrostEIRTempModFac * (RatedTotCap / 1.016670) * FractionalDefrostTime

          else if ( trim(adjustl(DefrostStrategy)) == 'Resistive') then
            ! Calculate defrost adjustment factors - assume resistive defrost only
            DefrostPower = DefrostCapacity * FractionalDefrostTime

          else
            error stop 'Error: you must select a valid defrost strategy'
          end if

        else
          ! Defrost is not active because (FractionalDefrostTime == 0)
          DefrostPower =  0.0
        end if
      end if

      ! Modify total heating capacity based on defrost heating capacity multiplier
      TotCap = TotCap * HeatingCapacityMultiplier

      ! TODO: the crankcase heater option has not been implemented as there is no current need.
      !       maybe done in the future if needed.

      ! Calculate coil condition
      FullLoadOutAirEnth = InletAirEnthalpy + TotCap / AirMassFlowRate

      ! Amount of moisture in/out unchanged as this is sensible heat process only
      FullLoadOutAirHumRat = InletAirHumRat
      FullLoadOutAirTemp = GetTDryBulbFromEnthalpyAndHumRatio(FullLoadOutAirEnth, FullLoadOutAirHumRat)
      FullLoadOutAirRH = GetRelHumFromHumRatio(FullLoadOutAirTemp,FullLoadOutAirHumRat,OutdoorPressure)
      ! Limit to saturated conditions at FullLoadOutAirEnth
      if (FullLoadOutAirRH > 1.00) then
        FullLoadOutAirTemp = PsyTsatFnHPb(FullLoadOutAirEnth, OutdoorPressure)
        FullLoadOutAirHumRat  = GetHumRatioFromEnthalpyAndTDryBulb(FullLoadOutAirEnth, FullLoadOutAirTemp)
      end if

      ! Calculate actual outlet conditions for the input part load ratio
      ! Actual outlet conditions are "average" for time step
      ! Assume continuous fan, cycling compressor
      OutletAirEnthalpy = ( (PartLoadRatio * AirMassFlowRatio) * FullLoadOutAirEnth +  &
                              (1.00 - (PartLoadRatio * AirMassFlowRatio)) * InletAirEnthalpy)

      OutletAirHumRat   = ( PartLoadRatio * FullLoadOutAirHumRat + &
                          (1.00 - PartLoadRatio) * InletAirHumRat )

      OutletAirTemp     = GetTDryBulbFromEnthalpyAndHumRatio(OutletAirEnthalpy,OutletAirHumRat)

      ! Calculate electricity consumed. First, get EIR modifying factors for off-rated conditions
      ! Use biquadratic curve. This allows sensitivity of the EIR to the entering dry-bulb temperature
      ! as well as the outside dry-bulb temperature.
      EIRTempModFac = A3 + InletAirDryBulbTemp * (B3 + C3 * InletAirDryBulbTemp)    &
                      + OutdoorDryBulb * (D3 + E3 * OutdoorDryBulb)  &
                      + InletAirDryBulbTemp * OutdoorDryBulb * F3

      ! Limit the energy input ratio modifier curve function of temperature to its set bounds
      if (EIRTempModFac < HPHeatingEIRFTempMin) then
        EIRTempModFac = HPHeatingEIRFTempMin
        if (PrintWarningsLocal) then
          print *, 'Warning: the energy input ratio modifier curve function of temperature exceeds its set bounds'
          print *, 'The curve has been reset to: ', HPHeatingEIRFTempMin
        end if
      else if (EIRTempModFac > HPHeatingEIRFTempMax) then
        EIRTempModFac = HPHeatingEIRFTempMax
        if (PrintWarningsLocal) then
          print *, 'Warning: the energy input ratio modifier curve function of temperature exceeds its set bounds'
          print *, 'The curve has been reset to: ', HPHeatingEIRFTempMax
        end if
      end if

      ! Energy input ratio modifier curve function of flow fraction
      EIRFlowModFac = A4 + B4 * AirMassFlowRatio + C4 * AirMassFlowRatio**2

      ! Limit the energy input ratio modifier curve function of flow fraction to its set bounds
      if (EIRFlowModFac < HPHeatingEIRFFFMin) then
        EIRFlowModFac = HPHeatingEIRFFFMin
        if (PrintWarningsLocal) then
          print *, 'Warning: the energy input ratio modifier curve function of flow fraction exceeds its set bounds'
          print *, 'The curve has been reset to: ', HPHeatingEIRFFFMin
        end if
      else if (EIRFlowModFac > HPHeatingEIRFFFMax) then
        EIRFlowModFac = HPHeatingEIRFFFMax
        if (PrintWarningsLocal) then
          print *, 'Warning: the energy input ratio modifier curve function of flow fraction exceeds its set bounds'
          print *, 'The curve has been reset to: ', HPHeatingEIRFFFMax
        end if
      end if

      ! Calculate actual EIR
      EIR = (1./RatedCOP) * EIRTempModFac * EIRFlowModFac

      ! Calculate modified PartLoadRatio due to defrost (reverse-cycle defrost only)
      PLRHeating = min( 1.00, (PartLoadRatio + LoadDueToDefrost / TotCap) )

      ! Calculate PLF (0.85, 0.15)
      PLF = A5 + B5 * PartLoadRatio + C5 * PartLoadRatio**2
      if (PLF < 0.70) then
        PLF = 0.70
      end if

      HeatingCoilRuntimeFraction = (PLRHeating / PLF)

      ! Adjust defrost power to correct for DOE-2 bug where defrost power is constant regardless of compressor runtime fraction
      ! Defrosts happen based on compressor run time (frost buildup on outdoor coil), not total elapsed time.
      DefrostPower = DefrostPower * HeatingCoilRuntimeFraction

      ElecHeatingPower = TotCap / HeatingCapacityMultiplier * EIR * HeatingCoilRuntimeFraction * InputPowerMultiplier + DefrostPower

      ! Total heating power of the DX unit (energy rate moved from outdoor to indoor)
      TotalHeatingEnergyRate = AirMassFlowRate * (OutletAirEnthalpy - InletAirEnthalpy)

      ! If/when the fan is on, we add the power consumed by the fan to the electrical power consumed by the DX unit
      if (FanModeLocal == 1) ElecHeatingPower = ElecHeatingPower + FanPower

      ! This is the actual power 'removed' from the outdoor environment.
      ! We assume that all the electric power is dissipated as heat directly in the outdoor environment.
      TotalSensibleHeatOut = ElecHeatingPower - TotalHeatingEnergyRate


    else
      ! The DX coil is off. Pass through conditions
      OutletAirTemp           = InletDryBulbTemp
      OutletAirHumRat         = InletAirHumRat
      ElecHeatingPower        = 0.0
      TotalHeatingEnergyRate  = 0.0
      TotalSensibleHeatOut    = 0.0
    end if
  end subroutine CalcMinimalDXHeating
end module MinimalDXHeating
