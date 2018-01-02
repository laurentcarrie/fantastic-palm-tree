project("fantastic palm tree")
cmake_minimum_required(VERSION 3.5)

#CMAKE_MAKE_PROGRAM(make)


add_compile_options(-std=c++11 -fext-numeric-literals -lpthread)

add_executable(f4242 main.cpp song.cpp json11.cpp 
		     read_util.cpp grille.cpp write_pdf_song.cpp 
			 write_pdf_book.cpp
		     write_mp_grille.cpp 
		     write_mp_tab.cpp
		     tablature.cpp
			 chord.h
			 datamodel.h
			 grille.h
			 json11.hpp
			 read_util.h
			 read_util.hc
			 song.h
			 tablature.h
		     )


add_custom_target(
	run
	DEPENDS f4242
	COMMAND Debug/f4242.exe info.json
)