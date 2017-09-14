#ifndef song_h_
#define song_h_ 1

#include "datamodel.h"
#include "grille.h"



struct Lyrics {
  int nb_cols_ ;
  std::string title_ ;
  std::vector<std::string> data_ ;
} ;


class Song {
public:
  std::string titre_ ;
  std::string auteur_ ;
  std::string filename_ ;
  std::vector<Grille> grilles_ ;
  std::vector<Lyrics> lyrics_ ;
  void read (const Datamodel::Conf&,const std::string&) ;
  const void write (const Datamodel::Conf&) ;
} ;


#endif
