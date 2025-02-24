cmake_minimum_required(VERSION 3.10)
project(MPI_Fortran_Project LANGUAGES Fortran)

# Enable MPI
find_package(MPI REQUIRED)

# Set source directory
set(SRC_DIR ${CMAKE_SOURCE_DIR}/src)

# List all Fortran source files
set(SOURCES
    ${SRC_DIR}/par_play.f90    
    ${SRC_DIR}/MOD_mpi.f90
    ${SRC_DIR}/test_driver.f90
)

# Add executable
add_executable(test ${SOURCES})

# Link with MPI
target_link_libraries(test MPI::MPI_Fortran)

# Set Fortran compiler flags
set_target_properties(test PROPERTIES
    Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules
)

# Install target
install(TARGETS test DESTINATION bin)
