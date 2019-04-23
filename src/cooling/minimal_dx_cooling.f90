! MinimalDX version 0.1.0 (https://www.github.com/dmey/minimal-dx).
! Copyright 2018 D. Meyer and R. Raustad. Licensed under MIT.
!
! Description
! This module contains a simplified EnergyPlus subroutine for calculating the performance
! of a DX cooling coil `CalcDoe2DXCoil`. Modified by D. Meyer and R. Raustad (2018).
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

module MinimalDXCooling
  !+ Contains a simplified EnergyPlus subroutine for calculating the performance of a DX cooling coil.

  ! Import Fortran 2008 standard to represent double-precision floating-point format
  use, intrinsic :: iso_fortran_env

  implicit none

  private
  public CalcMinimalDXCooling

  contains
  subroutine CalcMinimalDXCooling(OutdoorTDryBulb, OutdoorHumRatio, OutdoorPressure,                & ! I
                                  InletTDryBulb, InletHumRatio,                                     & ! I
                                  RatedCOP, RatedTotCap, PartLoadRatio, RatedAirMassFlowRate,       & ! I
                                  OutletTemperature, OutletHumRatio,                                & ! O
                                  ElecCoolingPower, SensCoolingEnergyRate, LatCoolingEnergyRate,    & ! O
                                  TotalCoolingEnergyRate, TotalSensibleHeatOut)                       ! O

    !+ Simplified EnergyPlus subroutine for calculating the performance of a DX cooling coil.
    !+ Adapted from EnergyPlus `CalcDoe2DXCoil` by D.Meyer and R. Raustad (2018).
    !+
    !+####ORIGINAL ENERGY PLUS SUBROUTINE INFORMATION:
    !+       AUTHOR         Fred Buhl
    !+       DATE WRITTEN   May 2000
    !+       MODIFIED       Shirey, Feb/October 2001, Feb/Mar 2004
    !+                      Feb 2005 M. J. Witte, GARD Analytics, Inc.
    !+                      Add new coil type COIL:DX:MultiMode:CoolingEmpirical:
    !+                      April 2010 Chandan Sharma, FSEC, Added basin heater
    !+       RE-ENGINEERED  Don Shirey, Aug/Sept 2000
    !+
    !+
    !+####PURPOSE OF THIS SUBROUTINE:
    !+ Calculates the air-side performance and electrical energy use of a direct-
    !+ expansion, air-cooled cooling unit.
    !+
    !+####METHODOLOGY EMPLOYED:
    !+ This routine simulates the performance of air-cooled DX cooling equipment.
    !+ The routine requires the user to enter the total cooling capacity, sensible heat ratio,
    !+ and COP for the unit at ARI 210/240 rating conditions (26.67C [80F] dry-bulb, 19.44C [67F]
    !+ wet-bulb air entering the cooling coil, 35C [95F] dry-bulb air entering the outdoor
    !+ condenser. Since different manufacturer's rate their equipment at different air flow rates,
    !+ the supply air flow rate corresponding to the rated capacities and rated COP must also be
    !+ entered (should be between 300 cfm/ton and 450 cfm/ton). The rated information entered by
    !+ the user should NOT include the thermal or electrical impacts of the supply air fan, as
    !+ this is addressed by another module.
    !+
    !+ With the rated performance data entered by the user, the model employs some of the
    !+ DOE-2.1E curve fits to adjust the capacity and efficiency of the unit as a function
    !+ of entering air temperatures and supply air flow rate (actual vs rated flow). The model
    !+ does NOT employ the exact same methodology to calculate performance as DOE-2, although
    !+ some of the DOE-2 curve fits are employed by this model.
    !+
    !+ The model checks for coil dryout conditions, and adjusts the calculated performance
    !+ appropriately.
    !+
    !+####REFERENCES:
    !+ ASHRAE HVAC 2 Toolkit page 4-81.
    !+
    !+ Henderson, H.I. Jr., K. Rengarajan and D.B. Shirey, III. 1992.The impact of comfort
    !+ control on air conditioner energy use in humid climates. ASHRAE Transactions 98(2):
    !+ 104-113.
    !+
    !+ Henderson, H.I. Jr., Danny Parker and Y.J. Huang. 2000.Improving DOE-2's RESYS routine:
    !+ User Defined Functions to Provide More Accurate Part Load Energy Use and Humidity
    !+ Predictions. Proceedings of ACEEE Conference.
    !+
    !+####LINKS:
    !+
    !+ 1.
    !+ <http://bigladdersoftware.com/epx/docs/8-7/engineering-reference/coils.html#single-speed-electric-dx-air-cooling-coil>
    !+
    !+ 2.
    !+ <https://github.com/NREL/EnergyPlusRelease/blob/1ba8474958dbac5a371362731b23310d40e0635d/SourceCode/DXCoil.f90#L8434>

    use PsychroWrapper, only: InitPsychrometrics, GetMoistAirDensity, GetTWetBulbFromHumRatio, &
                              GetMoistAirEnthalpy, GetHumRatioFromEnthalpyAndTDryBulb, &
                              GetTDryBulbFromEnthalpyAndHumRatio, PsyTsatFnHPb
    use MinimalDXFan, only: GetOnOffFan

    ! Use Fortran 2008 standard to represent double-precision floating-point format
    integer, parameter :: dp = REAL64

    ! Subroutine arguments
    real(dp), intent(in)    :: OutdoorTDryBulb
        !+ Outdoor dry bulb air temperature `[°C]`
    real(dp), intent(in)    :: OutdoorHumRatio ! Currently not used but maybe used in the future for air-to-water types.
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
    real(dp), intent(in)    :: PartLoadRatio
        !+ Part load ratio (PLR). This is the actual cooling effect produced by the AC unit divided by the maximum
        !+ cooling effect available - i.e. `PLR = (SensibleCoolingLoad / SensCoolingEnergyRate)` `[1]`
    real(dp), intent(in)    :: RatedAirMassFlowRate
        !+ Rated air mass flow rate `[kg s⁻¹]`
    real(dp), intent(out)   :: OutletTemperature
        !+ Actual (calculated) outlet air dry bulb temperature existing the cooling coil `[°C]`
    real(dp), intent(out)   :: OutletHumRatio
        !+ Actual (calculated) outlet air humidity ratio existing the cooling coil `[kgH₂O kgAIR⁻¹]`
    real(dp), intent(out)   :: ElecCoolingPower
        !+ Calculated electrical power consumed by the DX unit `[W]`
    real(dp), intent(out)   :: SensCoolingEnergyRate
        !+ Sensible cooling power used to calculate the PLR. This is the maximum amount of sensible heat rate that the coil
        !+ is capable of extracting from the indoor environment for the specified conditions. `[W]`
    real(dp), intent(out)   :: LatCoolingEnergyRate
        ! Total latent cooling energy rate extracted by the coil from the indoor environment `[J kg⁻¹]`
    real(dp), intent(out)   :: TotalCoolingEnergyRate
        !+ Total cooling power of the DX unit (energy rate extracted by DX unit from the indoor environment) `[W]`
    real(dp), intent(out)   :: TotalSensibleHeatOut
        !+ Total power rejected by the evaporator into the outdoor environment
        !+ i.e. TotalCoolingEnergyRate + ElecCoolingPower `[W]`

    ! Local variables
    integer     :: Counter
        ! Counter for dry evaporator iterations                             [1]
    real(dp)    :: IndoorAirDensity
        ! Air density of moist air                                          [kg m⁻3]
    real(dp)    :: InletTWetBulb
        ! Indoor (inlet) air wet bulb temperature                           [°C]
    real(dp)    :: hDelta
        ! Change in air enthalpy across the cooling coil                    [J kg⁻¹]
    real(dp)    :: hADP
        ! Apparatus dew point (ADP) enthalpy                                [J kg⁻¹]
    real(dp)    :: wADP
        ! Apparatus dew point (ADP) humidity ratio                          [kgH₂O kgAIR⁻¹]
    real(dp)    :: tADP
        ! Temperature of air at ADP conditions                              [°C]
    real(dp)    :: hTinwADP
        ! Enthalpy at inlet dry-bulb and wADP                               [J kg⁻¹]
    real(dp)    :: InletAirHumRatTemp
        ! Inlet air humidity ratio used in ADP/BF loop                      [kgH₂O kgAIR⁻¹]
    real(dp)    :: hTinwout
        ! Enthalpy at inlet dry-bulb and outlet humidity ratio              [J kg⁻¹]
    real(dp)    :: InletAirEnthalpy
        ! Enthalpy at inlet dry-bulb and outlet humidity ratio              [J kg⁻¹]
    real(dp)    :: werror
        ! Deviation of humidity ratio in dry evaporator iteration loop      [1]
    real(dp)    :: CBF
        ! Calculated coil bypass factor using relation CBF = exp(-NTU)      [1]
    real(dp)    :: ACCoolingCAPFTemp
        ! Total cooling capacity modifier curve function of temperature     [1]
    real(dp)    :: ACCoolingCAPFFF
        ! Total cooling capacity modifier curve function of flow fraction   [1]
    real(dp)    :: ACCoolingEIRFTemp
        ! Energy input ratio modifier curve function of temperature         [1]
    real(dp)    :: ACCoolingEIRFFF
        ! Energy input ratio modifier curve function of flow fraction       [1]
    real(dp)    :: ACCoolingPLFFPLR
        ! Part load factor, accounts for thermal lag at compressor
        ! startup, used in power calculation                                [1]
    real(dp)    :: SHR
        ! Actual coil sensible heat rate                                    [W]
    real(dp)    :: A0
        ! NTU * air mass flow rate, used in CBF calculation                 [1]
    real(dp)    :: ADiff
        ! Used for exponential (-A0/AirMassFlowRate)                        [1]
    real(dp)    :: FullLoadOutAirEnth
        ! Outlet air enthalpy at full load conditions                       [J kg⁻¹]
    real(dp)    :: FullLoadOutAirHumRat
        ! Outlet air humidity ratio at full load conditions                 [kgH₂O kgAIR⁻¹]
    real(dp)    :: FullLoadOutAirTemp
        ! Outlet dry bulb air temperature at full load conditions           [°C]
    real(dp)    :: OutletAirEnthalpy
        ! Supply air enthalpy (average value for constant fan)              [J kg⁻¹]
    real(dp)    :: MinAirHumRat
        ! Minimum value between the inlet air humidity ratio and
        ! the outlet air humidity ratio                                     [kgH₂O kgAIR⁻¹]
    real(dp)    :: AirMassFlowRate
        ! Air mass flow rate use in the subroutine for calculations         [kg s⁻¹]
    real(dp)    :: EIR
        ! EIR at part load and off rated conditions                         [1]
    real(dp)    :: TotalCoolingCapacity
        ! Gross total cooling capacity at off-rated conditions              [W]
    real(dp) :: FanPower
        ! Power of the fan to be simulated                                  [W]
    real(dp) :: CoolingCoilRuntimeFraction
        ! Run time fraction of the DX cooling unit                          [1]

    ! Local parameters
    integer,  parameter     :: MaxIter       = 30
        ! Maximum number of iterations for dry evaporator calculations      [1]
    real(dp), parameter     :: RF            = 0.4_dp
        ! Relaxation factor for dry evaporator iterations                   [1]
    real(dp), parameter     :: TOLERANCE     = 0.01_dp
        ! Error tolerance for dry evaporator iterations                     [1]
    real(dp), parameter     :: RatedCBF      = 0.1_dp
        ! Coil bypass factor at off rated conditions                        [1] FIXME: temp value for now.
    real(dp), parameter     :: ExpLowerLimit = -20.0_dp
        ! Exponent lower limit                                              [1]
    real(dp), parameter     :: AirFlowRatio  = 1.0_dp
        ! Ratio of compressor on airflow to average time-step airflow
        ! Set to 1. Used only by DX coils with different air flow during
        ! cooling and when no cooling is
        ! required (constant fan, fan speed changes)                        [1]
    integer,  parameter     :: FanMode = 0
        ! Mode of operation: 1 for on, 0 for off                            [1]
    real(dp), parameter     :: MotEff = 0.75_dp
        ! Fan motor efficiency                                              [1]
    real(dp), parameter     :: MotInAirFrac = 1.0_dp
        ! Fraction of motor heat entering air stream                        [1]

    ! Performance curves coefficients
    ! Reference:
    !https://github.com/NREL/EnergyPlus/blob/develop/datasets/ResidentialACsAndHPsPerfCurves.idf#L33-L123

    ! Coefficients for ACCoolingCAPFTemp -- Total cooling capacity function of temperature curve (bi-quadratic).
    ! Minimum and maximum values of x and y are 0 and 50 respectively with curve output in rage 0 to 5
    real(dp), parameter :: A1 =  1.5509_dp
        ! Coefficient1 Constant
    real(dp), parameter :: B1 =  -0.07505_dp
        ! Coefficient2 x
    real(dp), parameter :: C1 =  0.0031_dp
        ! Coefficient3 x**2
    real(dp), parameter :: D1 =  0.0024_dp
        ! Coefficient4 y
    real(dp), parameter :: E1 =  -0.00005_dp
        ! Coefficient5 y**2
    real(dp), parameter :: F1 =  -0.00043_dp
        ! Coefficient6 x*y
    real(dp), parameter :: ACCoolingCAPFTempMin = 0.63_dp
        ! Minimum curve output value
    real(dp), parameter :: ACCoolingCAPFTempMax = 1.57_dp
        ! Maximum curve output value

    ! Coefficients for ACCoolingCAPFFF -- total cooling capacity function of flow fraction curve (quadratic).
    ! Minimum and maximum values of x are 0 and 1.5 respectively with curve output in range 0 to 2
    real(dp), parameter :: A2 =  0.71861_dp
        ! Coefficient1 Constant
    real(dp), parameter :: B2 =  0.41010_dp
        ! Coefficient2 x
    real(dp), parameter :: C2 =  -0.12871_dp
        ! Coefficient3 x**2
    real(dp), parameter :: ACCoolingCAPFFFMin = 0.0_dp
        ! Minimum curve output value
    real(dp), parameter :: ACCoolingCAPFFFMax = 2.0_dp
        ! Maximum curve output value

    ! Coefficients for ACCoolingEIRFTemp -- Energy input ratio function of temperature curve (bi-quadratic).
    ! Minimum and maximum values of x and y are 0 and 50 respectively with curve output in rage 0 to 5
    real(dp), parameter :: A3 =  -0.30428_dp
        ! Coefficient1 Constant
    real(dp), parameter :: B3 =  0.11805_dp
        ! Coefficient2 x
    real(dp), parameter :: C3 =  -0.00342_dp
        ! Coefficient3 x**2
    real(dp), parameter :: D3 =  -0.00626_dp
        ! Coefficient4 y
    real(dp), parameter :: E3 =  0.0007_dp
        ! Coefficient5 y**2
    real(dp), parameter :: F3 =  -0.00047_dp
        ! Coefficient6 x*y
    real(dp), parameter :: ACCoolingEIRFTempMin = 0.83_dp
        ! Minimum curve output value
    real(dp), parameter :: ACCoolingEIRFTempMax = 1.21_dp
        ! Maximum curve output value

    ! Coefficients for ACCoolingEIRFFF -- Energy input ratio function of flow fraction curve (quadratic).
    ! Minimum and maximum values of x are 0 and 1.5 respectively with curve output in range 0 to 2
    real(dp), parameter :: A4 =  1.32299905_dp
        ! Coefficient1 Constant
    real(dp), parameter :: B4 =  -0.477711207_dp
        ! Coefficient2 x
    real(dp), parameter :: C4 =  0.154712157_dp
        ! Coefficient3 x**2
    real(dp), parameter :: ACCoolingEIRFFFMin = 0.0_dp
        ! Minimum curve output value
    real(dp), parameter :: ACCoolingEIRFFFMax = 2.0_dp
        ! Maximum curve output value

    ! Part Load Fraction curve (quadratic) as a function of Part Load Ratio is default from
    ! Table 6. BEopt AC Rated Value Inputs of NREL report NREL/TP-5500-56354
    ! Minimum and maximum values of x are 0 and 1.5 respectively
    real(dp), parameter :: A5 = 0.90_dp            !- Coefficient1 Constant
    real(dp), parameter :: B5 = 0.10_dp            !- Coefficient2 x
    real(dp), parameter :: C5 = 0.0_dp             !- Coefficient3 x**2

    call InitPsychrometrics()

    Counter = 0

    ! Calculate wet bulb temperature entering the cooling coil
    InletTWetBulb = GetTWetBulbFromHumRatio(InletTDryBulb, InletHumRatio, OutdoorPressure)

    ! Inlet Moist air enthalpy
    InletAirEnthalpy  = GetMoistAirEnthalpy(InletTDryBulb, InletHumRatio)
    ! Calculate air density of indoor air using outdoor pressure. Assume indoor pressure = outdoor pressure
    IndoorAirDensity = GetMoistAirDensity(InletTDryBulb, InletHumRatio, OutdoorPressure)
    ! Check that the part load ratio is greater than 0 (i.e. DX unit is off) else just pass through conditions.
    if (PartLoadRatio > 0) then
      ! Set the rated mass flow rate equal the mass flow rate used in the subroutine then check
      ! that the air mass flow rate is within bounds else set air mass flow rate accordingly
      AirMassFlowRate = RatedAirMassFlowRate
      if (AirMassFlowRate / IndoorAirDensity / RatedTotCap < 0.00004027_dp) then
        AirMassFlowRate = 0.00004027_dp * RatedTotCap * IndoorAirDensity
        print *, 'Warning: air mass flow rate must be greater than 0.00004027m3/s/W'
        print *, 'Resetting the air mass flow rate to: ', AirMassFlowRate, ' kg/s'

      else if (AirMassFlowRate / IndoorAirDensity / RatedTotCap > 0.00006041_dp) then
        AirMassFlowRate = 0.00006041_dp * RatedTotCap * IndoorAirDensity
        print *, 'Warning: air mass flow rate must be lower than 0.00006041m3/s/W'
        print *, 'Resetting the air mass flow rate to: ', AirMassFlowRate, ' kg/s'
      end if

      ! Modify the inlet air temperature to account for heat added by the fan motor
      ! The fan power is assumed to be 0.04151 W/W of the rated capacity
      FanPower = 0.04151 * RatedTotCap
      InletAirEnthalpy = GetOnOffFan(FanMode, MotEff, FanPower, MotInAirFrac, InletAirEnthalpy, AirMassFlowRate)

      ! Adjust coil bypass factor for actual air flow rate. Use relation CBF = exp(-NTU) where
      ! NTU = A0/(m*cp). Relationship models the cooling coil as a heat exchanger with Cmin/Cmax = 0.
      if (RatedCBF > 0.0_dp) then
        A0 = -log(RatedCBF) * RatedAirMassFlowRate
      else
        A0 = 0.0_dp
      end if

      ADiff = -A0 / AirMassFlowRate

      if (ADiff >= ExpLowerLimit) then
        CBF = exp(ADiff)
      else
        CBF = 0.0_dp
      end if

      ! Get total capacity modifying factor (function of temperature) for off-rated conditions
      ! InletAirHumRat may be modified in this ADP/BF loop, use temporary variable for calculations
      InletAirHumRatTemp = InletHumRatio

      ! Assuming an air-cooled condenser only.
      do
        ! Total cooling capacity modifier curve function of temperature
        ACCoolingCAPFTemp = A1 + B1 * InletTWetBulb + C1 * InletTWetBulb**2        &
                               + D1 * OutdoorTDryBulb  + E1 * OutdoorTDryBulb**2   &
                               + F1 * InletTWetBulb * OutdoorTDryBulb

        ! Limit the cooling capacity modifier curve function of temperature to the its set bounds
        if (ACCoolingCAPFTemp < ACCoolingCAPFTempMin) then
          ACCoolingCAPFTemp = ACCoolingCAPFTempMin
          print *, 'Warning: the total cooling capacity modifier curve function of temperature exceeds its set bounds'
          print *, 'The curve has been reset to: ', ACCoolingCAPFTempMin

        else if (ACCoolingCAPFTemp > ACCoolingCAPFTempMax) then
          ACCoolingCAPFTemp = ACCoolingCAPFTempMax
          print *, 'Warning: the total cooling capacity modifier curve function of temperature exceeds its set bounds'
          print *, 'The curve has been reset to: ', ACCoolingCAPFTempMax
        end if

        ! Total cooling capacity modifier curve function of flow fraction
        ACCoolingCAPFFF = A2 + B2 * AirFlowRatio + C2 * AirFlowRatio**2

        ! Limit the cooling capacity modifier curve to the its set bounds
        if (ACCoolingCAPFFF < ACCoolingCAPFFFMin) then
          ACCoolingCAPFFF = ACCoolingCAPFFFMin
          print *, 'Warning: the total cooling capacity modifier curve function of flow fraction exceeds its set bounds'
          print *, 'The curve has been reset to: ', ACCoolingCAPFFFMin

        else if (ACCoolingCAPFFF > ACCoolingCAPFFFMax) then
          ACCoolingCAPFFF = ACCoolingCAPFFFMax
          print *, 'Warning: the total cooling capacity modifier curve function of flow fraction exceeds its set bounds'
          print *, 'The curve has been reset to: ', ACCoolingCAPFFFMax
        end if

        ! Calculate the total cooling capacity
        TotalCoolingCapacity = RatedTotCap * ACCoolingCAPFTemp * ACCoolingCAPFFF

        ! Calculate apparatus dew point conditions using TotalCoolingCapacity and CBF
        hDelta = TotalCoolingCapacity / AirMassFlowRate

        ! Apparatus dew point enthalpy
        hADP = InletAirEnthalpy - hDelta / (1.0_dp - CBF)

        ! Apparatus dew point saturated temperature
        tADP = PsyTsatFnHPb(hADP, OutdoorPressure)

        ! Apparatus dew point humidity ratio
        wADP = GetHumRatioFromEnthalpyAndTDryBulb(hADP, tADP)

        ! Apparatus dew point enthalpy
        hTinwADP = GetMoistAirEnthalpy(InletTDryBulb, wADP)

        if ( (InletAirEnthalpy - hADP ) > 1.d-10) then
          SHR = min( (hTinwADP - hADP) / (InletAirEnthalpy - hADP), 1.0_dp )
        else
          SHR = 1.0_dp
        end if

        ! Check for dry evaporator conditions (win < wadp)
        if ( wADP > InletAirHumRatTemp .or. (Counter >= 1 .and. Counter < MaxIter) ) then
          if (InletAirHumRatTemp == 0.0_dp) InletAirHumRatTemp = 0.00001_dp
            werror = (InletAirHumRatTemp - wADP) / InletAirHumRatTemp
            ! Increase InletAirHumRatTemp at constant InletAirTemp to find coil dry-out point. Then use the
            ! capacity at the dry-out point to determine exiting conditions from coil. This is required
            ! since the ACCoolingCAPFTemp doesn't work properly with dry-coil conditions.
            InletAirHumRatTemp = RF*wADP + (1.0_dp - RF) * InletAirHumRatTemp

            InletTWetBulb = GetTWetBulbFromHumRatio(InletTDryBulb, InletAirHumRatTemp, OutdoorPressure)

            Counter = Counter + 1

          if ( abs(werror) > TOLERANCE ) then
            cycle
          else
            exit
          end if
        else
          exit
        end if
      end do

      ACCoolingPLFFPLR = A5 + B5 * PartLoadRatio + C5 * PartLoadRatio**2
      if (ACCoolingPLFFPLR < 0.7_dp) then
        ACCoolingPLFFPLR = 0.7_dp
      end if
      CoolingCoilRuntimeFraction = PartLoadRatio / ACCoolingPLFFPLR

      ! Calculate full load output conditions
      if (SHR > 1.0_dp .or. Counter > 0) SHR = 1.0_dp

      FullLoadOutAirEnth = InletAirEnthalpy  - TotalCoolingCapacity / AirMassFlowRate
      hTinwout = InletAirEnthalpy - (1.0_dp - SHR) * hDelta

      if (SHR < 1.0_dp) then
        FullLoadOutAirHumRat = GetHumRatioFromEnthalpyAndTDryBulb(hTinwout, InletTDryBulb )
      else
        FullLoadOutAirHumRat = InletHumRatio
      end if

      FullLoadOutAirTemp = GetTDryBulbFromEnthalpyAndHumRatio(FullLoadOutAirEnth, FullLoadOutAirHumRat)

      ! Check for saturation error and modify temperature at constant enthalpy
      if ( FullLoadOutAirTemp < PsyTsatFnHPb(FullLoadOutAirEnth, OutdoorPressure) ) then
        FullLoadOutAirTemp = PsyTsatFnHPb(FullLoadOutAirEnth, OutdoorPressure)
        FullLoadOutAirHumRat  = GetHumRatioFromEnthalpyAndTDryBulb(FullLoadOutAirEnth, FullLoadOutAirTemp)
      end if

      ! Continuos fan, cycling compressor
      OutletAirEnthalpy = ( (PartLoadRatio * AirFlowRatio ) * FullLoadOutAirEnth + &
                            ( 1.0_dp - (PartLoadRatio * AirFlowRatio ) ) * InletAirEnthalpy)
      OutletHumRatio = ( (PartLoadRatio * AirFlowRatio ) * FullLoadOutAirHumRat + &
                          ( 1.0_dp - (PartLoadRatio * AirFlowRatio ) ) * InletHumRatio )

      OutletTemperature = GetTDryBulbFromEnthalpyAndHumRatio(OutletAirEnthalpy, OutletHumRatio)

      ! Check for saturation error and modify temperature at constant enthalpy
      if ( OutletTemperature < PsyTsatFnHPb(OutletAirEnthalpy, OutdoorPressure) ) then
        OutletTemperature = PsyTsatFnHPb(OutletAirEnthalpy, OutdoorPressure)
        OutletHumRatio  = GetHumRatioFromEnthalpyAndTDryBulb(OutletAirEnthalpy, OutletTemperature)
      end if

      ! Energy input ratio modifier curve function of temperature
      ACCoolingEIRFTemp = A3 + B3 * InletTWetBulb + C3 * InletTWetBulb**2     &
                          + D3 * OutdoorTDryBulb + E3 * OutdoorTDryBulb**2    &
                          + F3 * InletTWetBulb * OutdoorTDryBulb

      ! Limit the energy input ratio modifier curve function of temperature to its set bounds
      if (ACCoolingEIRFTemp < ACCoolingEIRFTempMin) then
        ACCoolingEIRFTemp = ACCoolingEIRFTempMin
        print *, 'Warning: the energy input ratio modifier curve function of temperature exceeds its set bounds'
        print *, 'The curve has been reset to: ', ACCoolingEIRFTempMin

      else if (ACCoolingEIRFTemp > ACCoolingEIRFTempMax) then
        ACCoolingEIRFTemp = ACCoolingEIRFTempMax
        print *, 'Warning: the energy input ratio modifier curve function of temperature exceeds its set bounds'
        print *, 'The curve has been reset to: ', ACCoolingEIRFTempMax
      end if

      ! Energy input ratio modifier curve function of flow fraction
      ACCoolingEIRFFF = A4 + B4 * AirFlowRatio + C4 * AirFlowRatio**2

      ! Limit the energy input ratio modifier curve function of flow fraction to its set bounds
      if (ACCoolingEIRFFF < ACCoolingEIRFFFMin) then
        ACCoolingEIRFFF = ACCoolingEIRFFFMin
        print *, 'Warning: the energy input ratio modifier curve function of flow fraction exceeds its set bounds'
        print *, 'The curve has been reset to: ', ACCoolingEIRFFFMin
      else if (ACCoolingEIRFFF > ACCoolingEIRFFFMax) then
        ACCoolingEIRFFF = ACCoolingEIRFFFMax
        print *, 'Warning: the energy input ratio modifier curve function of flow fraction exceeds its set bounds'
        print *, 'The curve has been reset to: ', ACCoolingEIRFFFMax
      end if

      ! Calculate the actual EIR for the DX unit under specified conditions
      EIR = (1./RatedCOP) * ACCoolingEIRFFF * ACCoolingEIRFTemp

      ! Calculate the electrical power consumed by the DX unit (compressors plus outdoor condenser fans)
      ElecCoolingPower = TotalCoolingCapacity * EIR * CoolingCoilRuntimeFraction

      TotalCoolingEnergyRate = AirMassFlowRate * (InletAirEnthalpy - OutletAirEnthalpy)

      ! Calculate the total sensible cooling energy extracted by the DX coils
      MinAirHumRat = min(InletHumRatio, OutletHumRatio)
      SensCoolingEnergyRate = AirMassFlowRate * &
                              ( GetMoistAirEnthalpy(InletTDryBulb, MinAirHumRat) - &
                                GetMoistAirEnthalpy(OutletTemperature, MinAirHumRat) )

      ! Don't let sensible capacity be greater than total capacity
      if (SensCoolingEnergyRate > TotalCoolingEnergyRate) then
        SensCoolingEnergyRate = TotalCoolingEnergyRate
        print *, 'Warning: the sensible capacity is greater than the total capacity'
        print *, 'The sensible capacity has been set to equal the total capacity'
      end if

      ! Calculation of the latent cooling power
      LatCoolingEnergyRate = TotalCoolingEnergyRate - SensCoolingEnergyRate

      ! Calculation of total available capacity - this is also the total amount
      ! of thermal energy rejected by the evaporator (external unit)
      TotalSensibleHeatOut  = TotalCoolingEnergyRate + ElecCoolingPower

      ! If/when the fan is on, we add the power consumed by the fan to the electrical power consumed by the DX unit
      if (FanMode == 1) ElecCoolingPower = ElecCoolingPower + FanPower

    else
      ! The DX coil is off. Pass through conditions
      OutletTemperature       = InletTDryBulb
      OutletHumRatio          = InletHumRatio
      ElecCoolingPower        = 0.0_dp
      SensCoolingEnergyRate   = 0.0_dp
      LatCoolingEnergyRate    = 0.0_dp
      TotalCoolingEnergyRate  = 0.0_dp
      TotalSensibleHeatOut    = 0.0_dp
    end if
  end subroutine CalcMinimalDXCooling
end module MinimalDXCooling
