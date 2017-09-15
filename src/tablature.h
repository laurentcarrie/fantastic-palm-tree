#ifndef Tablature_defined__
#define Tablature_defined__ 1

#include "datamodel.h"
#include <fstream>



struct Tablature {
    struct note {
        int frette_ ;
        int corde_ ;
    note() : frette_(0),corde_(0) {}
    } ;
    struct paquet  {
      std::vector<note> notes_ ;
      Datamodel::Accord::t chord_ ;
    } ;
    struct bar {
        std::vector<paquet> paquets_ ;
    } ;
    struct line {
        std::vector<bar> bars_ ;
    } ;
    struct t {
        std::string titre_ ;
        std::vector<line> lines_ ;
    };
  
  t t_ ;
  Tablature(std::ifstream&,const std::string&) ;
} ;


#endif
