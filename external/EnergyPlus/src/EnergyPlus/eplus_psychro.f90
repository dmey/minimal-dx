! MinimalDX version 0.1.0 (https://www.github.com/dmey/minimal-dx).
! Copyright 2018 D. Meyer and R. Raustad. Licensed under MIT.

! Description
! This module is meant as a wrapper for the `PsyTsatFnHPb` function as it is missing from PsychroLib.
! The `PsyTsatFnHPb` function was copied and pasted from the following link:
! https://github.com/NREL/EnergyPlusRelease/blob/1ba84749/SourceCode/PsychRoutines.f90#L2331-L2542
!
! Changes
! The following changes were made to the`§PsyTsatFnHPb` function:
! - D. Meyer (20190112): replace EnergyPlus function `PsyHFnTdbW` and
!   `PsyWFnTdbTwbPb` with `GetSatAirEnthalpy` from PsychroLib.
! - D. Meyer (20190112): remove unused preprocessing directives.
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

MODULE EPlusPsychro

  IMPLICIT NONE         ! Enforce explicit typing of all variables

  PRIVATE ! Everything private unless explicitly made public

  PUBLIC  PsyTsatFnHPb

  INTEGER, PARAMETER :: r64=KIND(1.0D0)

  CONTAINS

  FUNCTION PsyTsatFnHPb(H,PB,calledfrom) RESULT(T)

            ! FUNCTION INFORMATION:
            !       AUTHOR         George Shih
            !       DATE WRITTEN   May 1976
            !       MODIFIED       July 2003; LKL -- peg min/max values (outside range of functions)
            !       RE-ENGINEERED  na

            ! PURPOSE OF THIS FUNCTION:
            ! This function provides the saturation temperature from the enthalpy
            ! and barometric pressure.

            ! METHODOLOGY EMPLOYED:
            ! na

            ! REFERENCES:
            ! ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P99, EQN 22

            ! USE STATEMENTS:
            USE psychrolib, ONLY: GetSatAirEnthalpy

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

            ! FUNCTION ARGUMENT DEFINITIONS:
        REAL(r64), intent(in) :: H      ! enthalpy {J/kg}
        REAL(r64), intent(in) :: PB     ! barometric pressure {Pascals}
        character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages)
        REAL(r64)        :: T      ! result=> saturation temperature {C}

            ! FUNCTION PARAMETER DEFINITIONS:
            ! na

            ! INTERFACE BLOCK SPECIFICATIONS
            ! na

            ! DERIVED TYPE DEFINITIONS
            ! na

            ! FUNCTION LOCAL VARIABLE DECLARATIONS:
        REAL(r64) T1  ! APPROXIMATE SATURATION TEMPERATURE (C)
        REAL(r64) T2  ! APPROXIMATE SATURATION TEMPERATURE (C)
        REAL(r64) TN  ! NEW ASSUMED SATURATION TEMPERATURE (C)
        REAL(r64) H1  ! APPROXIMATE ENTHALPY (J/KG)
        REAL(r64) H2  ! APPROXIMATE ENTHALPY (J/KG)
        REAL(r64) Y1  ! ERROR IN ENTHALPY
        REAL(r64) Y2  ! ERROR IN ENTHALPY
        INTEGER  IterCount
        REAL(r64) HH  ! temporary enthalpy (calculation) value
        LOGICAL FlagError  ! Set when errors should be flagged
        REAL(r64)      :: Hloc ! local value of H


  !                                      CHECK H IN RANGE.
        HH = H + 1.78637d4

        IF (H >= 0.0d0) THEN
          Hloc = MAX(0.00001d0,H)
        ELSE IF (H < 0.0d0) THEN
          Hloc = MIN(-.00001d0,H)
        END IF

        FlagError=.false.

  !
        IF (HH > 7.5222d4) GO TO 20
        IF (HH > 2.7297d4) GO TO 60
        IF (HH > -6.7012d2) GO TO 50
        IF (HH > -2.2138d4) GO TO 40
        IF (HH < -4.24d4) HH=-4.24d4   ! Peg to minimum
        GO TO 30
    20 CONTINUE
        IF (HH < 1.8379d5) GO TO 70
        IF (HH < 4.7577d5) GO TO 80
        IF (HH < 1.5445d6) GO TO 90
        IF (HH < 3.8353d6) GO TO 100
        IF (HH > 4.5866d7) HH=4.5866d7   ! Peg to maximum
        GO TO 110
  !
  !                                      TEMP. IS FROM -60 C  TO  -40 C
    30 CONTINUE
        T=F6(HH,-19.44d0,8.53675d-4,-5.12637d-9,-9.85546d-14,-1.00102d-18,-4.2705d-24)
        GO TO 120
  !                                      TEMP. IS FROM -40 C  TO  -20 C
    40 CONTINUE
        T=F6(HH,-1.94224d1,8.5892d-4,-4.50709d-9,-6.19492d-14,8.71734d-20,8.73051d-24)
        GO TO 120
  !                                      TEMP. IS FROM -20 C  TO    0 C
    50 CONTINUE
        T=F6(HH,-1.94224d1,8.59061d-4,-4.4875d-9,-5.76696d-14,7.72217d-19,3.97894d-24)
        GO TO 120
  !                                      TEMP. IS FROM   0 C  TO   20 C
    60 CONTINUE
        T=F6(HH,-2.01147d1,9.04936d-4,-6.83305d-9,2.3261d-14,7.27237d-20,-6.31939d-25)
        GO TO 120
  !                                      TEMP. IS FROM  20 C  TO   40 C
    70 CONTINUE
        T=F6(HH,-1.82124d1,8.31683d-4,-6.16461d-9,3.06411d-14,-8.60964d-20,1.03003d-25)
        GO TO 120
  !                                      TEMP. IS FROM  40 C  TO   60 C
    80 CONTINUE
        T=F6(HH,-1.29419d0,3.88538d-4,-1.30237d-9,2.78254d-15,-3.27225d-21,1.60969d-27)
        GO TO 120
  !                                      TEMP. IS FROM   60 C TO   80 C
    90 CONTINUE
        T=F6(HH,2.39214d1,1.27519d-4,-1.52089d-10,1.1043d-16,-4.33919d-23,7.05296d-30)
        GO TO 120
  !                                      TEMP. IS FROM   80 C TO   90 C
    100 CONTINUE
        T=F6(HH,4.88446d1,3.85534d-5,-1.78805d-11,4.87224d-18,-7.15283d-25,4.36246d-32)
        GO TO 120
  !                                      TEMP. IS FROM   90 C TO  100C
    110 CONTINUE
        T=F7(HH,7.60565d11,5.80534d4,-7.36433d-3,5.11531d-10,-1.93619d-17,3.70511d-25, -2.77313d-33)
  !                                      IF THE BAROMETRIC PRESSURE IS
  !                                      EQUAL TO 1.0133E5 , SATURATION
  !                                      TEMP. IS CALCULATED BY ABOVE EQUA
  !                                      OTHERWISE TEMP. IS COMPUTED BY
  !                                      FOLLOWING ITERATION METHOD
    120 CONTINUE
        IF (ABS(PB-1.0133d5)/1.0133d5 <= 0.01d0) GO TO 170
        IterCount=0
        T1=T
        H1=GetSatAirEnthalpy(T1,PB)
        Y1=H1-Hloc
        IF (ABS(Y1/Hloc) <= 0.1d-4) GO TO 140
        T2=T1*0.9d0
    130 IterCount=IterCount+1
        H2=GetSatAirEnthalpy(T2,PB)
        Y2=H2-Hloc
        IF (ABS(Y2/Hloc) <= 0.1d-4) GO TO 150
        IF (Y2 == Y1) GO TO 150
        TN=T2-Y2/(Y2-Y1)*(T2-T1)
        IF (IterCount > 30) GO TO 160
        T1=T2
        T2=TN
        Y1=Y2
        GO TO 130
    140 CONTINUE
        T=T1
        GO TO 170
    150 CONTINUE
        T=T2
        GO TO 170
    160 CONTINUE
    170 CONTINUE

  !   result is T

    RETURN

    CONTAINS

        REAL(r64) FUNCTION F6(X,A0,A1,A2,A3,A4,A5)
        IMPLICIT NONE
        REAL(r64) X
        REAL(r64) A0,A1,A2,A3,A4,A5

        F6=A0+X*(A1+X*(A2+X*(A3+X*(A4+X*A5))))

        RETURN
        END FUNCTION F6

        REAL(r64) FUNCTION F7(X,A0,A1,A2,A3,A4,A5,A6)
        IMPLICIT NONE
        REAL(r64) X,A6
        REAL(r64) A0,A1,A2,A3,A4,A5

        F7=(A0+X*(A1+X*(A2+X*(A3+X*(A4+X*(A5+X*A6))))))/1.0D10

        RETURN
        END FUNCTION F7


  END FUNCTION PsyTsatFnHPb

END MODULE EPlusPsychro
