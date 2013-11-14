cmake_minimum_required(VERSION 2.6)
project(Compiler)

if(CMAKE_COMPILER_IS_GNUCXX)
	set(CMAKE_CXX_FLAGS ${CMAKE_C_FLAGS} "-std=gnu++0x") # 4.7+ also supports -std=c++11
endif(CMAKE_COMPILER_IS_GNUCXX)

add_executable(compiler
	main.cpp
	Regex.hpp
	)
