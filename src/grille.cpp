#include <iostream>
#include <vector>
#include <assert.h>
#include <sstream>
#include <fstream>
#include <functional>
#include <algorithm>

#include "grille.h"
#include "read_util.h"
/*
let chord_of_string s : Accord.c = (
  let a = String.explode s in 
  let (note,a) = match a with
    | [] -> failwith "empty bar"
	| note::a -> note,a  in
  let () = check_note note in
  let (alteration,a) = match a with
    | [] -> Accord.None,[]
    | 'b'::a -> Accord.Flat,a
    | '#'::a -> Accord.Sharp,a
    | _ -> Accord.None,a
  in
  let (sus4,a) = match a with
    | [] -> false,[]
    | 's'::'u'::'s'::'4'::a -> true,a
    | _ -> false,a
  in
  let (diminue,a) = match a with
    | [] -> false,[]
    | 'd'::'i'::'m'::a -> true,a
    | _ -> false,a
  in
  let (minor,a) = match a with
    | [] -> false,[]
    | 'm'::a -> true,a
    | _ -> false,a
  in
  let (minor7,major7,a) = match a with
    | [] -> false,false,[]
    | '7'::'M'::a -> false,true,a
    | '7'::a -> true,false,a
    | _ -> false,false,a
  in
    { Accord.note = note ; minor=minor ; alteration=alteration ; minor7=minor7 ; major7=major7 ; diminue=diminue ; sus4=sus4 }
)
*/

Datamodel::Accord accord_of_string(const std::string& s) {
  Datamodel::Accord a ;
  a.t_.has_position_ = false ;
  a.t_.has_chord_ = true ;
  a.t_.chord_.note_ = 'C' ;
  a.t_.chord_.alteration_ = Datamodel::Accord::None ;
  return a ;
}
  
  


Grille::bar bar_of_string(const std::string&s ) {
  Grille::bar b ;
  std::vector<std::string> v (stringvector_of_string(s," ")) ;
  std::transform (v.begin(),v.end(),std::back_inserter(b.chords_),[](const std::string& s) -> Datamodel::Accord { return (accord_of_string(s));}) ;
  return b ;
}

std::vector<Grille::bar> bars_of_line(const std::string& s) {
  std::vector<std::string> v (stringvector_of_string(s,":")) ;
  std::vector<Grille::bar> ret ;
  std::transform (v.begin(),v.end(),std::back_inserter(ret),[](const std::string& ss) -> Grille::bar { return (bar_of_string(ss));}) ;
  
  return ret ;
}

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
	    if (std::string(line)=="") return ;
	    ligne l ;
	    l.bars_ = bars_of_line(line) ;
	    t_.lignes_.push_back(l) ;
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
