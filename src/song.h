#ifndef song_h_
#define song_h_ 1

#include "datamodel.h"

class Song {
public:
  Datamodel::Song::t data ;
  void read (const std::string&) ;
} ;


#endif
