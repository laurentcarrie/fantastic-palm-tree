#ifndef song_h_
#define song_h_ 1

#include "datamodel.h"
#include "grille.h"

class Song {
public:
  std::string titre_ ;
  std::string auteur_ ;
  std::string filename_ ;
  std::vector<Grille> grille_ ;
  void read (const std::string&) ;
  const void write () ;
} ;


#endif
