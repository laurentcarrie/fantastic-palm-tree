#include <iostream>
#include <vector>
#include <assert.h>
#include <sstream>
#include <fstream>
#include <functional>
#include <algorithm>
#include <string>

#include "tablature.h"
#include "read_util.h"

extern Datamodel::Accord chord_of_string(const std::string& s) ;

Tablature::paquet paquet_of_string(const std::string& s) {
  // std::cout << "paquet_of_string '" << s << "'" << std::endl ; 
  std::vector<std::string> v = stringvector_of_string(s,",") ;
  Tablature::paquet ret ;
  // default
  ret.chord_.has_position_ = false ;
  ret.chord_.has_chord_ = false ;

  if (v.empty()) return ret ;

  {
    std::string remain(v.front()) ;
    if (remain.size()>2) {
      if ( (remain[0]=='[') && remain[remain.size()-1]==']') {
	ret.chord_.has_chord_ = true ;
	Datamodel::Accord a = chord_of_string(remain.substr(1,remain.size()-2)) ;
	ret.chord_ = a.t_ ;
	v.erase(v.begin()) ;
      }
    }
  }

  if (v.empty()) {
    std::ostringstream oss ;
    oss << "for string '" << s << "' ; error empty paquet" ;
    throw std::runtime_error(oss.str()) ;
  }

  ret.chord_.has_position_ = true ;
  try {
    ret.chord_.position_ = stoi(v.front()) ;
  } 
  catch (...) {
    std::cout << "failed to convert '" <<  v.front() << "' to integer" << std::endl ;
    assert(false) ;
  }
  v.erase(v.begin()) ;

  std::transform(v.begin(),v.end(),
		 std::back_inserter(ret.notes_),
		 [](const std::string& s) -> Tablature::note { 
		   std::vector<std::string> vv ( stringvector_of_string(s," ")) ;
		   if ( vv.size() != 2 ) {
		     std::ostringstream oss ; 
		     oss << "bad string for paquet : '" << s << "'" ;
		     throw (oss.str()) ;
		   }
		   Tablature::note note ;
		   try {
		     note.corde_  = stoi(vv[0]) ;
		     note.frette_ = stoi(vv[1]) ;
		   } 
		   catch (...) {
		     std::cout << "failed to convert '" <<  vv[0] << "' or << '" << vv[1] << "' to integer" << std::endl ;
		     assert(false) ;
		   }
		   return note ;

		 }) ;

  return ret ;
}

Tablature::bar tablature_bar_of_string(const std::string&s ) {
  std::cout << "tablature_bar_of_string '" << s << "'" << std::endl ; 
  std::vector<std::string> v = stringvector_of_string(s,";") ;
  Tablature::bar b ;
  std::transform (v.begin(),v.end(),std::back_inserter(b.paquets_),[](const std::string& s) -> Tablature::paquet { return (paquet_of_string(s));}) ;
  return b ;
}

std::vector<Tablature::bar> tablature_bars_of_line(const std::string& s) {
  std::cout << "tablature_bars_of_line '" << s << "'" << std::endl ; 
  std::vector<std::string> v (stringvector_of_string(s,"|")) ;
  for (auto ss : v) {
    std:: cout << "SSSSSSSSSSSSSssplit : " << ss << std::endl ;
  }

  std::vector<Tablature::bar> ret ;
  std::transform (v.begin(),v.end(),std::back_inserter(ret),[](const std::string& ss) -> Tablature::bar { return (tablature_bar_of_string(ss));}) ;
  
  return ret ;
}

Tablature::Tablature(std::ifstream& fin,const std::string& titre) {
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
	    if (std::string(line)=="") return ;
	    Tablature::line l ;
	    l.bars_ = tablature_bars_of_line(line) ;
	    t_.lines_.push_back(l) ;
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
  
  for ( auto l : t_.lines_ ) {
    for ( auto b : l.bars_ ) {
      for ( auto p : b.paquets_ ) {
	for ( auto n : p.notes_ ) {
	  std::cout << "NNNNNNNNNNNnote : " << n.corde_ << " ; " << n.frette_ << std::endl ;
	}
      }
    }
  }
  
}
