cmake_minimum_required(VERSION 3.10)

# Define the project name and specify C++17 standard
project(isle VERSION 1.0 LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Include directories
include_directories(${PROJECT_SOURCE_DIR}/include)

# Add tests
enable_testing()
add_subdirectory(tests)