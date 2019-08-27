cmake_minimum_required(VERSION 2.8)

project(NEmulator)
add_executable(${PROJECT_NAME} "main.cpp" "cpu.cpp" "bus.cpp")
