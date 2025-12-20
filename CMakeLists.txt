cmake_minimum_required(VERSION 3.16)
project(fortty LANGUAGES Fortran C)

# C standard for GLAD
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Compiler warnings
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    add_compile_options(-Wall -Wextra -pedantic)
endif()

# Find dependencies
find_package(glfw3 REQUIRED)
find_package(OpenGL REQUIRED)
find_package(Freetype REQUIRED)

# Optional: fontconfig for portable font discovery
find_package(PkgConfig)
if(PkgConfig_FOUND)
    pkg_check_modules(FONTCONFIG fontconfig)
endif()

# C helper libraries (GLAD + FreeType + PTY wrappers)
add_library(helpers STATIC
    src/gl/glad.c
    c_src/gl_loader.c
    c_src/freetype_helpers.c
    c_src/pty_helpers.c
)
target_include_directories(helpers PUBLIC ${CMAKE_SOURCE_DIR}/include)
target_include_directories(helpers PRIVATE ${FREETYPE_INCLUDE_DIRS})
target_link_libraries(helpers PRIVATE glfw ${FREETYPE_LIBRARIES} util)

# Link fontconfig if available
if(FONTCONFIG_FOUND)
    target_compile_definitions(helpers PRIVATE HAVE_FONTCONFIG)
    target_include_directories(helpers PRIVATE ${FONTCONFIG_INCLUDE_DIRS})
    target_link_libraries(helpers PRIVATE ${FONTCONFIG_LIBRARIES})
    message(STATUS "Fontconfig found - enabling portable font discovery")
else()
    message(STATUS "Fontconfig not found - using hardcoded font paths")
endif()

# Fortran module output directory
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)

# Fortran sources (order matters for module dependencies)
set(FORTRAN_SOURCES
    src/core/types.f90
    src/gl/gl_bindings.f90
    src/text/glyph.f90
    src/gl/shader.f90
    src/text/font.f90
    src/text/atlas.f90
    src/text/renderer.f90
    src/terminal/cell.f90
    src/config/toml_parser.f90
    src/config/config.f90
    src/terminal/screen.f90
    src/terminal/cursor.f90
    src/terminal/scrollback.f90
    src/terminal/terminal.f90
    src/terminal/parser.f90
    src/pty/pty_bindings.f90
    src/pty/pty.f90
    src/window/glfw_bindings.f90
    src/window/window.f90
    src/fortty.f90
)

# Main executable
add_executable(fortty ${FORTRAN_SOURCES})
target_include_directories(fortty PRIVATE ${CMAKE_BINARY_DIR}/modules)
target_link_libraries(fortty PRIVATE helpers glfw OpenGL::GL ${FREETYPE_LIBRARIES})

# For Linux, we need dl for GLAD's dlopen
if(UNIX AND NOT APPLE)
    target_link_libraries(fortty PRIVATE dl)
endif()
