# MinimalDX version 0.2.0 (https://www.github.com/dmey/minimal-dx).
# Copyright 2018 D. Meyer and R. Raustad. Licensed under MIT.

# Common files
set(SRC_TEST_HEATING
    ../../src/heating/minimal_dx_heating_driver.f90
    ../../src/heating/minimal_dx_heating.f90
    ../../src/shared/minimal_dx_fan.f90
    eplus_wrapper_heating.f90
    test_heating.f90
)

# Program to test the new implementation using the PsychroLib library
# against the original EnergyPlus implementation
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod/test_heating)
add_executable(test_heating
    ${SRC_TEST_HEATING}
    ../../src/shared/psychro_wrapper.f90
    ../../external/EnergyPlus/src/EnergyPlus/eplus_psychro.f90
    ../../external/psychrolib/src/fortran/psychrolib.f90
)
target_link_libraries(test_heating ${ENERGYPLUS_FORTRAN_LIBRARY})
add_dependencies(test_heating EnergyPlus-Fortran-install)
install(TARGETS test_heating
    RUNTIME DESTINATION bin
)

# Program to test the new implementation using the EnergyPlus psychrometric functions
# against the original EnergyPlus implementation
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod/test_heating_eplus)
add_executable(test_heating_eplus
    ${SRC_TEST_HEATING}
    ../shared/psychro_wrapper_eplus.f90
)
target_link_libraries(test_heating_eplus ${ENERGYPLUS_FORTRAN_LIBRARY})
add_dependencies(test_heating_eplus EnergyPlus-Fortran-install)
install(TARGETS test_heating_eplus
    RUNTIME DESTINATION bin
)

# Program to create test data
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod/make_test_data_heating)
add_executable(make_test_data_heating make_test_data_heating.f90)
target_link_libraries(make_test_data_heating ${ENERGYPLUS_FORTRAN_LIBRARY})
add_dependencies(make_test_data_heating EnergyPlus-Fortran-install)
install(TARGETS make_test_data_heating
    RUNTIME DESTINATION bin
)