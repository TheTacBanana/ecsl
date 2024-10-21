# Minimum Version
cmake_minimum_required(VERSION 3.15)

# This is your project statement. You should always list languages;
# Listing the version is nice here since it sets lots of useful variables
project(ecslvm VERSION 1.0 LANGUAGES C)

# DIRECTORY = "ecsl_vm/"

# Adding something we can run - Output name matches target name
add_executable(ecslvm ecsl_vm/main.c)