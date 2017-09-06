#include <iostream>
#include <vector>
#include <assert.h>
#include <sstream>
#include <fstream>
#include <functional>

#include "song.h"

namespace Datamodel {
std::string concat_filenames(const std::string& n1,const std::string& n2) {
    std::string ret ;
    ret = n1 + "/" + n2 ;
    return ret ;
}
}



std::tuple<std::string,std::string> my_split(const std::string& s) {
    auto pos = s.find(' ') ;
    if (pos==std::string::npos) {
        return std::make_tuple(s,std::string("")) ;
    } else {
        std::string s1(s.substr(0,pos)) ;
        std::string s2(s.substr(pos,s.size()-pos-1)) ;
	std::cout << "my_split " << s << " -> " << s1 << " ; " << s2 << std::endl; 
        return std::make_tuple(s1,s2) ;
    }
}

void Song::read(const std::string& filename) {
  std::function<void(std::ifstream&)> r =
    [&r,this](std::ifstream& fin) {
        char line[1001] ;
        try {
            if (fin.eof()) {
                std::cout << "EOF" << std::endl ;
                return ;
            }
            if (fin.bad()) return ;
            if (fin.fail()) return ;
            fin.getline(line,1000) ;
            auto t=my_split(line) ;
            std::string word = std::get<0>(t) ;
            std::string arg = std::get<1>(t) ;
            if ( word == "\\titre" ) {
	      std::cout << "FFFFFFFFFFffound titure : " << arg << std::endl ;
	      this->data.titre = arg ;
            }
            r (fin) ;
        }
        catch (std::exception& e) {
            std::cout << __FILE__ << ":" << __LINE__ << " ; caught " << e.what()  << std::endl ;
        }
        catch (...) {
            std::cout << __FILE__ << ":" << __LINE__ << " ; caught unknown" << std::endl ;
        }
    } ;


  this->data.filename = filename ;

    std::ifstream fin(filename,std::ios::binary) ;
    r(fin) ;
    return ;
}
