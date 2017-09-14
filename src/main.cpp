#include <iostream>
#include <vector>
#include <assert.h>
#include <sstream>
#include <fstream>

#include "json11.hpp"

#include "song.h"
#include "datamodel.h"
#include "read_util.h"

static Datamodel::Conf la_conf ;

class Data {
public:

    Data(const std::string& in) {
      try {
        std::string errmsg ;
        json11::Json j ( json11::Json::parse (in,errmsg) ) ;
        assert(j.is_object()) ;
        json11::Json::object o = j.object_items() ;
        {
	  la_conf.song_ = o["filename"].string_value() ;
	  la_conf.srcdir_ = o["srcdir"].string_value() ;
	  la_conf.builddir_ = o["builddir"].string_value() ;
        }
	std::cout << "BBBBB " << la_conf.builddir_ << std::endl; 
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
      }
      catch (std::runtime_error& e) {
	std::cout << e.what () << std::endl ; exit(1) ;
      }
      catch (...) {
	std::cout << "caughut unknown" << std::endl ; exit(1) ;
      }
    }

} ;






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
        Data data(buffer) ;


        std::ostringstream oss ;

        Song song ;
        std::cout << "filename : " << la_conf.song_ << std::endl ;
        song.read(la_conf,la_conf.song_) ;

	std::cout << "song.titre : " << song.titre_ << std::endl ;

	song.write(la_conf);

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

