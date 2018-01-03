#include <iostream>
#include <vector>
#include <assert.h>
#include <sstream>
#include <fstream>
#include <functional>
#include <algorithm>

#include "grille.h"
#include "read_util.h"

Datamodel::Accord chord_of_string(const std::string& s) {
	if (s == "") {
		throw std::runtime_error("empty bar");
	}
	std::string remain(s);
	Datamodel::Accord ret;
	ret.t_.chord_.note_ = s[0];

	remain = remain.substr(1, remain.size() - 1);


	// alteration
	if (remain == "") {
		ret.t_.chord_.alteration_ = Datamodel::Accord::None;
		ret.t_.chord_.sus4_ = false;
		ret.t_.chord_.minor_ = false;
		ret.t_.chord_.minor7_ = false;
		ret.t_.chord_.major7_ = false;
		ret.t_.chord_.diminue_ = false;
		return ret;
	}
	else if (remain[0] == '#') {
		ret.t_.chord_.alteration_ = Datamodel::Accord::Sharp;
		remain = remain.substr(1, remain.size() - 1);
	}
	else if (remain[0] == 'b') {
		ret.t_.chord_.alteration_ = Datamodel::Accord::Flat;
		remain = remain.substr(1, remain.size() - 1);
	}
	else {
		ret.t_.chord_.alteration_ = Datamodel::Accord::None;
	}

	// 4
	if (remain.substr(0, 4) == "sus4") {
		ret.t_.chord_.sus4_ = true;
		remain = remain.substr(4, remain.size() - 4);
	}
	else {
		ret.t_.chord_.sus4_ = false;
	}

	// dim
	if (remain.substr(0, 3) == "dim") {
		ret.t_.chord_.diminue_ = true;
		remain = remain.substr(3, remain.size() - 3);
	}
	else {
		ret.t_.chord_.diminue_ = false;
	}

	// minor
	if (remain.substr(0, 1) == "m") {
		ret.t_.chord_.minor_ = true;
		remain = remain.substr(1, remain.size() - 1);
	}
	else {
		ret.t_.chord_.minor_ = false;
	}

	// 7
	if (remain.substr(0, 2) == "7M") {
		ret.t_.chord_.minor7_ = false;
		ret.t_.chord_.major7_ = true;
		remain = remain.substr(2, remain.size() - 2);
	}
	else if (remain.substr(0, 1) == "7") {
		ret.t_.chord_.minor7_ = true;
		ret.t_.chord_.major7_ = false;
		remain = remain.substr(1, remain.size() - 1);
	}
	else {
		ret.t_.chord_.minor7_ = false;
		ret.t_.chord_.major7_ = false;
	}


	return ret;
}

/*

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


Grille::bar bar_of_string(const std::string&s) {
	Grille::bar b;
	std::vector<std::string> v(stringvector_of_string(s, " "));
	std::transform(v.begin(), v.end(), std::back_inserter(b.chords_), [](const std::string& s) -> Datamodel::Accord { return (chord_of_string(s)); });
	return b;
}

std::vector<Grille::bar> bars_of_line(const std::string& s) {
	std::vector<std::string> v(stringvector_of_string(s, ":"));
	std::vector<Grille::bar> ret;
	std::transform(v.begin(), v.end(), std::back_inserter(ret), [](const std::string& ss) -> Grille::bar { return (bar_of_string(ss)); });

	return ret;
}

Grille::Grille(std::ifstream& fin, const std::string& titre) {
	std::function<void(std::ifstream&)> r =
		[&r, this](std::ifstream& fin) {
		char buffer[1001];
		try {
			if (fin.eof()) {
				// std::cout << "EOF" << std::endl ;
				return;
			}
			if (fin.bad()) return;
			if (fin.fail()) return;
			fin.getline(buffer, 1000);
			std::string line(strip_string(buffer));
			if (std::string(line) == "") return;

			std::vector<std::string> v(stringvector_of_string(line, "@"));

			ligne l;
			l.repeat_ = 1;
			l.bars_ = bars_of_line(v.front());
			if (v.size() > 1) {
				std::string arg = strip_string(v.at(1)) ;
				if (arg == "\\r1") {
					l.repeat_ = 1;
				}
				else if (arg == "\\r2") {
					l.repeat_ = 2;
				}
				else if (arg == "\\r3") {
					l.repeat_ = 3;
				}
				else if (arg == "\\r4") {
					l.repeat_ = 4 ;
				}
			}
			t_.lignes_.push_back(l);
			r(fin);
		}
		catch (std::exception& e) {
			std::cout << __FILE__ << ":" << __LINE__ << " ; caught " << e.what() << std::endl;
		}
		catch (...) {
			std::cout << __FILE__ << ":" << __LINE__ << " ; caught unknown" << std::endl;
		}
	};
	r(fin);
	t_.titre_ = titre;
}
