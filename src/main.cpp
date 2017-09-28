#include <iostream>
#include <vector>
#include <assert.h>
#include <sstream>
#include <fstream>
#include <dirent.h>
#include <sys/stat.h>

#include "json11.hpp"

#include "song.h"
#include "datamodel.h"
#include "read_util.h"
#include "read_util.hc"



Datamodel::Conf read_conf(const std::string& in) {
  try {
    std::string errmsg ;
    json11::Json j ( json11::Json::parse (in,errmsg) ) ;
    assert(j.is_object()) ;
    json11::Json::object o = j.object_items() ;
    Datamodel::Conf la_conf ;
    {
      la_conf.song_ = o["filename"].string_value() ;
      la_conf.srcdir_ = o["srcdir"].string_value() ;
      la_conf.builddir_ = o["builddir"].string_value() ;
    }
    if (! path_is_absolute(la_conf.srcdir_)) {
      std::ostringstream oss ;
      oss << "srcdir is not absolute : '" << la_conf.srcdir_ << "'" << std::endl ;
      throw std::runtime_error(oss.str()) ;
    }
    if (! path_is_absolute(la_conf.builddir_)) {
      std::ostringstream oss ;
      oss << "builddir is not absolute : '" << la_conf.builddir_ << "'" << std::endl ;
      throw std::runtime_error(oss.str()) ;
    }
    return la_conf ;
  }
  catch (std::runtime_error& e) {
    std::cout << e.what () << std::endl ; exit(1) ;
  }
  catch (...) {
    std::cout << "caught unknown" << std::endl ; exit(1) ;
  }
}






int main(int argc,char** argv) {
  try {
    std::cout << __FILE__ << ":" << __LINE__ << std::endl ;
    //  rude::Socket socket ;

    assert(argc>0) ;
    std::ifstream file(argv[1],std::ios::binary) ;
    if (file.fail()) {
      perror(argv[1]) ;
      return 1 ;
    }
    std::basic_string<char> buffer((std::istreambuf_iterator<char>(file)),(std::istreambuf_iterator<char>())) ;
    std::cout << "buffer : " << buffer << std::endl ;
    Datamodel::Conf la_conf = read_conf(buffer) ;


    std::ofstream fout_cmake(la_conf.builddir_ + "/CMakeLists.txt") ;
    fout_cmake << "\
project(\"songs\")\n\
cmake_minimum_required(VERSION 3.5)\n\
set(CMAKE_VERION 3.5)\n\
include(UseLATEX.cmake)\n\
" ;

    /*
      for ( auto i : v) {
      fout << "\n\
      add_custom_command(\n\
      OUTPUT muse/starlight-grille-" << i << ".mps\n\
      DEPENDS muse/starlight-grille-" << i << ".mp\n\
      WORKING_DIRECTORY " << la_conf.builddir_ << "/muse\n\
      COMMAND mpost starlight-grille-" << i << "\n\
      COMMAND mv starlight-grille-" << i << ".1  starlight-grille-" << i << ".mps \n\
      )\n\
      " ;
      }
      }
    */



	
    std::function<void(bool& acc,const std::string& filename,bool)> walk = 
      [&la_conf,&fout_cmake] (bool& acc,const std::string& filename,bool is_dir) {
      if (is_dir) {
	std::string builddir(replace_path(filename,la_conf.srcdir_.c_str(),la_conf.builddir_.c_str())) ;
	std::cout << "mkdir '" << builddir << "'" << std::endl; 
	mkdir(builddir.c_str(),077) ;
      } else if ( extension(filename) == "song") {
	Song song ;
	song.read(la_conf,filename) ;
	song.write(la_conf) ;
	std::string name(replace_path(filename,(la_conf.srcdir_+"/").c_str(),"")) ;
	name = replace_extension(name,"") ;
	fout_cmake << "\n\
\n\
set(LATEX_OUTPUT_PATH xxx-pdf/" << basename(dirname(filename)) << ")\n\
latex_get_output_path(output_dir)\n\
\n\
add_custom_target(\n\
install_" << basename(name) << " ALL\n\
DEPENDS ${output_dir}/" << basename(name) << ".pdf\n\
COMMAND mkdir -p install/" << dirname(name) << "\n\
COMMAND cp ${output_dir}/" << basename(name) << ".pdf install/" << dirname(name) << "/.\n\
)\n\
\n\
add_latex_document(\n\
" << name << ".tex \n\
FORCE_PDF \n\
DEPENDS \n\
" ;

	for ( int count=0;count<song.grilles_.size();++count ) {
	  fout_cmake << "${output_dir}/" << basename(name) << "-grille-" << count << ".mps\n" ;
	}
	for ( int count=0;count<song.tablatures_.size();++count ) {
	  fout_cmake << "${output_dir}/" << basename(name) << "-tab-" << count << ".mps\n" ;
	}
	fout_cmake << ")\n" ;

	for (int count=0;count<song.grilles_.size();++count) {
	  fout_cmake << "\n\
add_custom_command(\n\
OUTPUT ${output_dir}/" << basename(name) << "-grille-" << count << ".mps\n\
DEPENDS " << name << "-grille-" << count << ".mp\n\
WORKING_DIRECTORY " << basename(dirname(filename)) << "\n\
COMMAND mpost " << basename(name) << "-grille-" << count << "\n\
COMMAND cp " << basename(name) << "-grille-" << count << ".1  ${output_dir}/" << basename(name) << "-grille-" << count << ".mps \n \
)\n " ;
	}
	for (int count=0;count<song.tablatures_.size();++count) {
	  fout_cmake << "\n\
add_custom_command(\n\
OUTPUT ${output_dir}/" << basename(name) << "-tab-" << count << ".mps\n\
DEPENDS " << name << "-tab-" << count << ".mp\n\
WORKING_DIRECTORY " << basename(dirname(filename)) << "\n\
COMMAND mpost " << basename(name) << "-tab-" << count << "\n\
COMMAND cp " << basename(name) << "-tab-" << count << ".1  ${output_dir}/" << basename(name) << "-tab-" << count << ".mps \n \
)\n " ;
	}

      }
      else {
	std::cout << "unknown file : '" << filename << "'" << std::endl ;
      }
      return ;
    } ;

    bool ret ;
    walk_tree(la_conf.srcdir_,ret,walk) ;

    std::cout << "DONE !" << std::endl ;
    return 0 ;
  }
  catch (std::exception& e) {
    std::cout << "caught : " << e.what() << std::endl;
    return 1 ;
  }
  catch (...) {
    std::cout << "caught unkonwn" <<  std::endl;
    return 1 ;
  }

}

