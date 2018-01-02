#include <iostream>
#include <vector>
#include <assert.h>
#include <sstream>
#include <fstream>
#include <functional>
#include <ctime>
#include <iomanip>

#include "song.h"
#include "datamodel.h"
#include "read_util.h"

namespace Datamodel {
std::string concat_filenames(const std::string& n1,const std::string& n2) {
    std::string ret ;
    ret = n1 + "/" + n2 ;
    return ret ;
}
}


bool Song::read(const Datamodel::Conf& la_conf,const std::string& filename) {
  std::function<void(std::ifstream&)> r =
    [&r,this](std::ifstream& fin) {
        char line[1001] ;
        try {
            if (fin.eof()) {
                // std::cout << "EOF" << std::endl ;
                return ;
            }
            if (fin.bad()) return ;
            if (fin.fail()) return ;
            fin.getline(line,1000) ;
            auto t=my_split(line) ;
			std::string word = std::get<0>(t);
	    // std:: cout << "word : '" << word << "'" << std::endl ;
            std::string arg = std::get<1>(t) ;
            if ( word == "\\titre" ) {
	      titre_ = read_string_until_empty_line(fin) ;
            }
            else if ( word == "\\auteur" ) {
	      auteur_ = read_string_until_empty_line(fin) ;
            }
            else if ( word == "\\grille" ) {
	      grilles_.push_back(Grille(fin,arg)) ;
	      // std::cout << "GGGGGGGGGGGGGGGGGGrilles : " << grilles_.size() << std::endl ;
            }
            else if ( word == "\\tab" ) {
	      tablatures_.push_back(Tablature(fin,arg)) ;
            }
            else if ( (word == "\\lyrics") || (word=="\\lyrics2")) {
	      Lyrics l ;
	      if (word=="\\lyrics2") {
		l.nb_cols_ = 2 ;
	      } else {
		l.nb_cols_ = 1 ;
	      }
	      l.title_ = arg ;
	      l.data_ = read_array_until_empty_line(fin) ;
	      lyrics_.push_back(l) ;
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

  filename_ = filename ;
  nb_croches_ = 8 ;
  // std::ifstream fin(la_conf.srcdir_ + "/" + filename,std::ios::binary) ;
  std::ifstream fin(filename_) ;
  if (!fin.good()) {
    std::cout << "BAD file : '" << filename_ << "'" << std::endl ;
    return false ;
  }
  r(fin) ;
  // std::cout << "IIIIIIInfos " << grilles_.size() << " grilles ; " << tablatures_.size() << " tablatures ; " << lyrics_.size() << " lyrics " << std::endl ;
  return true ;
}

void Book::read(const Datamodel::Conf& la_conf,const std::string& filename) {
	print_index_ = false;
	filename_ = filename;

	std::cout << "read book '" << filename << "'" << std::endl;

	std::function<void(std::ifstream&)> r = [&r, this, &la_conf
	](std::ifstream& fin) {
		char line[1001];
		try {
			if (fin.eof()) {
				// std::cout << "EOF" << std::endl ;
				return;
			}
			if (fin.bad()) return;
			if (fin.fail()) return;
			fin.getline(line, 1000);
			std::string word = strip_string(line) ;

			if (word == "\\print_index") {
				this->print_index_ = true;
			}
			else if (word == " " || word == "" ) {

			}
			else {
				song_info info;
				info.filename_ = word;
				info.song_ = new Song;
				info.found_ = info.song_->read(la_conf, la_conf.srcdir_ + "/" + word);
				this->songs_.push_back(info);
			}
			r(fin);
		}
		catch (std::runtime_error& err) {
			std::cout << err.what() << std::endl;
		}
	};

	std::ifstream fin(filename_);
	if (!fin.good()) {
		std::cout << "BAD file : '" << filename_ << "'" << std::endl;
		return;
	}
	r(fin);
	// std::cout << "IIIIIIInfos " << grilles_.size() << " grilles ; " << tablatures_.size() << " tablatures ; " << lyrics_.size() << " lyrics " << std::endl ;
	return;
}

