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


	
	std::function<void(bool& acc,const std::string& filename,bool)> walk = 
	  [&la_conf] (bool& acc,const std::string& filename,bool is_dir) {
	  if (is_dir) {
	    std::string builddir(replace_path(filename,la_conf.srcdir_.c_str(),la_conf.builddir_.c_str())) ;
	    std::cout << "mkdir '" << builddir << "'" << std::endl; 
	    mkdir(builddir.c_str(),077) ;
	  } else if ( extension(filename) == "song") {
	    Song song ;
	    song.read(la_conf,filename) ;
	    song.write(la_conf) ;
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

