#ifndef song_h_
#define song_h_ 1

#include "datamodel.h"
#include "grille.h"
#include "tablature.h"



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
  std::vector<Tablature> tablatures_ ;
  std::vector<Lyrics> lyrics_ ;
  int nb_croches_ ;
  bool read (const Datamodel::Conf&,const std::string&) ;
  const void write_body(const Datamodel::Conf&,std::ofstream&);
  const void write(const Datamodel::Conf&);
} ;


class Book {
public:
	struct song_info {
		Song* song_;
		bool found_;
		std::string filename_;
	};

	std::string filename_;
	std::string title_;
	std::string author_;
	std::vector<song_info> songs_;
	bool print_index_ ;

	void read(const Datamodel::Conf&, const std::string&);
	const void write(const Datamodel::Conf&);
	void print_deps();
};

#endif
