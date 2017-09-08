#include <iostream>
#include <vector>
#include <assert.h>
#include <sstream>
#include <fstream>
#include <functional>

#include "grille.h"
#include "read_util.h"


Grille::Grille(std::ifstream& fin,const std::string& titre) {
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

            r (fin) ;
        }
        catch (std::exception& e) {
            std::cout << __FILE__ << ":" << __LINE__ << " ; caught " << e.what()  << std::endl ;
        }
        catch (...) {
            std::cout << __FILE__ << ":" << __LINE__ << " ; caught unknown" << std::endl ;
        }
    } ;
  r(fin) ;
  t_.titre_ = titre ;
}
