#include <iostream>
#include <vector>
#include <assert.h>
#include <sstream>
#include <fstream>

#include "json11.hpp"

#include "song.h"

class Data {
public:
    std::string song_filename_ ;

    Data(const std::string& in) {
        std::string errmsg ;
        json11::Json j ( json11::Json::parse (in,errmsg) ) ;
        assert(j.is_object()) ;
        json11::Json::object o = j.object_items() ;
        {
            song_filename_ = o["filename"].string_value() ;
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
        std::cout << "filename : " << data.song_filename_ << std::endl ;
        song.read(data.song_filename_) ;

	std::cout << "song.titre : " << song.data.titre << std::endl ;

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

