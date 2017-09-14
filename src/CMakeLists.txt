project("fantastic palm tree")
cmake_minimum_required(VERSION 3.5)


add_compile_options(-std=c++11 -fext-numeric-literals -lpthread)

add_executable(f4242 main.cpp song.cpp json11.cpp 
		     read_util.cpp grille.cpp write_pdf_song.cpp 
		     write_mp_grille.cpp 
		     )
