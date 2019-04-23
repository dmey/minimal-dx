# _Minimal_**DX**

 _Minimal_**DX** is a standalone library for simulating the performance of single air-cooled cooling direct expansion (DX) coil -- i.e. a model for a very simple air-to-air air conditioner (AC).  _Minimal_**DX** is based on the [EnergyPlus](https://energyplus.net/) [Single-Speed Electric DX Air Cooling Coil](http://bigladdersoftware.com/epx/docs/8-7/engineering-reference/coils.html#single-speed-electric-dx-air-cooling-coil) model. It is meant to be a more stripped-down version of the EnergyPlus model and its main purpose is to be used as a standalone Fortran library in your own program for simulating basic AC equipment. All the functions used in this library were derived and adapted from [EnergyPlus (Fortran)](https://github.com/NREL/EnergyPlusRelease).


## Documentation

The main subroutine used to calculate the performance of a single coil air-cooled DX cooling equipment is `MinimalDXCoolingDriver`.

- `MinimalDXCoolingDriver` was adapted from EnergyPlus `CalcDoe2DXCoil`. The wrapper to call is in `src/cooling/minimal_dx_cooling_driver.f90`, the main calculations are carried out in `src/cooling/minimal_dx_cooling.f90`.


The API documentation can be found [here](https://dmey.github.io/minimal-dx).

The original EnergyPlus documentation can be found in the [EnergyPlus Engineering Reference](https://bigladdersoftware.com/epx/docs/8-7/engineering-reference/):
- [Single-Speed Electric DX Air Cooling Coil](http://bigladdersoftware.com/epx/docs/8-7/engineering-reference/coils.html#single-speed-electric-dx-air-cooling-coil)


## Build instructions

These instructions will work on Windows (Visual Studio with Intel Fortran Compiler), Linux (Intel or gcc), and MacOS (Intel or gcc).
To build the library and tests, run the following commands from your command prompt:

```sh
$ mkdir build && cd build
$ cmake -DBUILD_TESTING=ON ..
$ cmake --build .
```

If you want to specify the installation directory and install, use the `CMAKE_INSTALL_PREFIX` flag and run `cmake --build . --target install`.


### Note for Windows Users
Make sure you have installed the Intel® Visual Studio Integration plugins or CMake will not be able to identify your compiler (No CMAKE_Fortran_COMPILER could be found error). Make sure that you use Intel® Command-Line Window when launching CMake - The Intel® compiler provides a command-line window with the appropriate environment variables already set (see: [Using the Intel® Command-Line Window](https://software.intel.com/en-us/node/522358)). You may also need to specify the generator flag -G in CMake; for example, if you are using Intel® Command-Line Window for Visual Studio 2010, then the CMake command should now be cmake -G "Visual Studio 10 2010" ... For more information on how to specify generators in CMake see [cmake-generators](https://cmake.org/cmake/help/latest/manual/cmake-generators.7.html#visual-studio-generators).


### Offline build

By default,  _Minimal_**DX** will require a working internet connection to download up-to-date dependencies. You can disable this option by enabling the offline flag (i.e. `-DOFFLINE=ON`) at configure time.

## Testing

Regression tests are useful to compare the two implementations over a large number of different conditions. In order to run these tests you will need to have Python 3.5 or above installed on your system and had enabled testing with the `BUILD_TESTING` flag.

All files used to generate the tests can be found under the `tests` folder. `make_test_data_cooling.f90` and `test_cooling.f90` are responsible for generating the data and invoking the functions respectively. The program `compare.py` is responsible for calling the `make_test_data_cooling` and `test_cooling` programs and for generating plots.

To run the regression tests go to the `tests` folder and run `python compare.py`. All output data and plots will be written to `build/outputs`.

## Copyright and License

Copyright 2018 D. Meyer and R. Raustad. Licensed under [MIT](LICENSE.txt).