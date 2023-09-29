<div align="center">

<!-- omit in toc -->
# _Minimal_**DX**

[![Build Status](https://travis-ci.com/dmey/minimal-dx.svg?token=3tqUbxqJuLtozjxqDymC&branch=master)](https://travis-ci.com/dmey/minimal-dx)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3562310.svg)](https://doi.org/10.5281/zenodo.3562310)

[Overview](#overview) | [Prerequisites](#prerequisites) | [Build instructions](#build-instructions) | [Documentation](#documentation) | [Testing](#testing) | [How to cite](#how-to-cite) | [Copyright and License](#copyright-and-license)
</div>

## Overview
 _Minimal_**DX** is a standalone library for simulating the performance of single air-cooled cooling and heating direct expansion (DX) coils -- i.e. a model for a very simple air-to-air air conditioner (AC) and heat pump (HP).  _Minimal_**DX** is based on [EnergyPlus](https://energyplus.net/)'s [Single-Speed Electric DX Air Cooling Coil](http://bigladdersoftware.com/epx/docs/8-7/engineering-reference/coils.html#single-speed-electric-dx-air-cooling-coil) and [Single-Speed DX Heating Coil Standard Ratings](https://bigladdersoftware.com/epx/docs/8-7/engineering-reference/coils.html#single-speed-dx-heating-coil-standard-ratings) models. It is meant to be a more stripped-down version of EnergyPlus's models. _Minimal_**DX**'s main purpose is to be used as a standalone Fortran library in your own program for simulating basic AC and HP equipment. All the functions used in this library were derived and adapted from [EnergyPlus (Fortran)](https://github.com/NREL/EnergyPlusRelease). Performance curves coefficients are taken from [Cutler et al. (2013)](https://doi.org/10.2172/1067909).

## Prerequisites

- [Git](https://git-scm.com/) >= 2
- [CMake](https://cmake.org/) >= 3.1
- A Recent version of GNU/Intel/Cray Fortran compiler
- [Python](https://www.python.org/) >= 3.6 [*Optional for testing*]

## Build instructions

These instructions will work on Windows (Visual Studio with Intel Fortran compilers), Linux (Intel or GNU compilers), and MacOS (Intel or GNU compilers).
To build the library without tests, clone this repository and run the following commands from your command prompt:

```
mkdir build && cd build
cmake -DCMAKE_BUILD_TYPE=Release -DBUILD_TESTING=OFF ..
cmake --build .
```

If you want to specify the installation directory and install, use the `CMAKE_INSTALL_PREFIX` flag and run `cmake --build . --target install`. The `BUILD_TESTING` flag is used to allow users to carry out regression tests -- see [Testing](#Testing) below. By default, we set the real type to an 8 byte wide. This behaviour is controlled by the optional `USE_REAL8` flag (default ON) -- the use of real 8 is required when building with `BUILD_TESTING`.


<details>
<summary>Note for Windows Users</summary>

Make sure you have installed the Intel® Visual Studio Integration plugins or CMake will not be able to identify your compiler (No CMAKE_Fortran_COMPILER could be found error). Make sure that you use Intel® Command-Line Window when launching CMake - The Intel® compiler provides a command-line window with the appropriate environment variables already set (see: [Using the Intel® Command-Line Window](https://software.intel.com/en-us/fortran-compiler-developer-guide-and-reference-using-the-command-line-on-windows)). You may also need to specify the generator flag -G in CMake; for example, if you are using Intel® Command-Line Window for Visual Studio 2010, then the CMake command should now be `cmake -G "Visual Studio 10 2010" ..`. For more information on how to specify generators in CMake see [cmake-generators](https://cmake.org/cmake/help/latest/manual/cmake-generators.7.html#visual-studio-generators).
</details>

## Documentation

The main subroutines used to calculate the performance of a single coil DX cooling and heating equipment are `MinimalDXCoolingDriver` and `MinimalDXHeatingDriver`, respectively:

- `MinimalDXCoolingDriver` was adapted from EnergyPlus `CalcDoe2DXCoil`. The wrapper to call is in `src/cooling/minimal_dx_cooling_driver.f90`, the main calculations are carried out in `src/cooling/minimal_dx_cooling.f90`.

- `MinimalDXHeatingDriver` was adapted from EnergyPlus `CalcDXHeatingCoil`. The wrapper to call is in `src/heating/minimal_dx_heating_driver.f90`, the main calculations are carried out in `src/heating/minimal_dx_heating.f90`.

The API documentation can be found at [https://dmey.github.io/EnergyPlus-TEB-DXCoils/Y5Hsy4kugMbusylbEVPC0f6Z8xmH73oL4FbNMaF1xkujO7QIOOmxKfSF104oneEACI6HWZIzkJ3zYZ4IcFhD5m4eKU1Ve/](https://dmey.github.io/EnergyPlus-TEB-DXCoils/Y5Hsy4kugMbusylbEVPC0f6Z8xmH73oL4FbNMaF1xkujO7QIOOmxKfSF104oneEACI6HWZIzkJ3zYZ4IcFhD5m4eKU1Ve/).

The original EnergyPlus documentation can be found in the [EnergyPlus Engineering Reference](https://bigladdersoftware.com/epx/docs/8-7/engineering-reference/):
- [Single-Speed Electric DX Air Cooling Coil](https://bigladdersoftware.com/epx/docs/8-7/engineering-reference/coils.html#single-speed-electric-dx-air-cooling-coil)
- [Single-Speed DX Heating Coil Standard Ratings](https://bigladdersoftware.com/epx/docs/8-7/engineering-reference/coils.html#single-speed-dx-heating-coil-standard-ratings)


## Testing

Regression tests are useful to compare the two implementations over a large number of different conditions. In order to run these tests you will need to have Python 3.5 or above installed on your system and had enabled testing with the `BUILD_TESTING` flag.

All files used to generate the tests can be found under the `tests` folder. `make_test_data_cooling.f90` and `test_cooling.f90` are responsible for generating the data and invoking the functions respectively. The program `run_tests.py` is responsible for calling the `make_test_data_cooling` and `test_cooling` programs and for generating plots.

To run the regression tests, first ensure that you have built _Minimal_**DX** with `BUILD_TESTING=ON` and in debug mode (`CMAKE_BUILD_TYPE=Debug`), then go to the `tests` folder and run the following commands:

```
pip3 install -r requirements.txt
python3 run_tests.py
```

All output data and plots will be written to `build/outputs`.

### Offline build

When enabling build tests `BUILD_TESTING=ON`,  _Minimal_**DX** will require a working internet connection to download up-to-date EnergyPlus dependencies. If these dependencies have already been downloaded and you do not have a working internet connection, you can still build _Minimal_**DX** with tests enabled by passing the the offline flag (i.e. `-DOFFLINE=ON`) at configure time.

## How to cite

If you are using _Minimal_**DX**, please make sure to cite the specific version you are using. For a list of available versions and Digital Object Identifiers (DOI), please refer to the [_Minimal_**DX** archive on Zenodo](https://doi.org/10.5281/zenodo.3562310).

## Copyright and License

MinimalDX copyright and license is shown at the top of each source file as follows:

```
2018-2020 D. Meyer and R. Raustad. Licensed under MIT.
```

For simplified EnergyPlus subroutines (`minimal_dx_cooling.f90`, `minimal_dx_fan.f90`, and `eplus_psychro.f90`) general EnergyPlus copyright and license applies and is shown at the top of each source file as follows:

```
EnergyPlus, Copyright (c) 1996-2019, The Board of Trustees of the University of Illinois, The Regents of the University of California, through Lawrence Berkeley National Laboratory (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other contributors. All rights reserved.

NOTICE: This Software was developed under funding from the U.S. Department of Energy and the U.S. Government consequently retains certain rights. As such, the U.S. Government has been granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable, worldwide license in the Software to reproduce, distribute copies to the public, prepare derivative works, and perform publicly and display publicly, and to permit others to do so.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

(1) Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

(2) Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

(3) Neither the name of the University of California, Lawrence Berkeley National Laboratory, the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

(4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form without changes from the version obtained under this License, or (ii) Licensee makes a reference solely to the software portion of its product, Licensee must refer to the software as "EnergyPlus version X" software, where "X" is the version number Licensee obtained under this License and may not use a different name for the software. Except as specifically required in this Section (4), Licensee shall not use in a company name, a product name, in advertising, publicity, or other promotional activities any name, trade name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly similar designation, without the U.S. Department of Energy's prior written consent.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```
