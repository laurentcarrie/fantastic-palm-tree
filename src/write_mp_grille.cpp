#include <iostream>
#include <vector>
#include <assert.h>
#include <sstream>
#include <fstream>
#include <functional>
#include <ctime>
#include <iomanip>
#include <numeric>
#include <string>

#include "song.h"
#include "datamodel.h"
#include "read_util.h"


void write_bar(std::ofstream& fout, int  index, const Grille::bar& bar) {
	fout << std::endl
		<< "%% write bar" << std::endl
		<< "x1 := x0 + width ;" << std::endl
		<< "draw(x0,base_line) -- (x0,base_line+height) ;"
		<< std::endl;

	std::function<bool(const Grille::bar& bar)>  has_position_lambda =
		[](const Grille::bar& bar) {
		if (bar.chords_.size() == 0) { return false; }

		bool first = bar.chords_[0].t_.has_position_;

		bool ret = std::accumulate(bar.chords_.begin(), bar.chords_.end(),
			first,
			[](bool has_position, const Datamodel::Accord& accord) {
			if (!has_position) { return false; }
			else if (accord.t_.has_position_) { return true; }
			else {
				std::ostringstream oss;
				oss << "mix of chords with position and without position";
				throw std::runtime_error(oss.str());
			}
			return true; // pour compiler
		});

		return ret;
	};

	bool has_position = has_position_lambda(bar);


	std::function<void(const std::string& pid, const Datamodel::Accord& a, int nb, int i)> e_of_chord_without_position =
		[&fout](const std::string& pid, const Datamodel::Accord& a, int nb, int i) {
		if (a.t_.has_chord_) {
			fout << "\n% " << __FILE__ << ":" << __LINE__ << "\nboxit." << pid << "_n(btex \\notefont{" << a.t_.chord_.note_ << "} etex) ;\n" << pid << "_n.c = " << pid << ".c ;\ndrawunboxed(" << pid << "_n) ;\n";

			//	    (* minor major *)
			fout << "\nboxit." << pid << "_m(btex \\subscriptfont{";
			if (a.t_.chord_.minor_) { fout << "m"; }
			fout << "} etex) ;\n" << pid << "_m.c = " << pid << "_n.se ;\ndrawunboxed(" << pid << "_m) ;\n";

			// (* alteration *)
			fout << "\nboxit." << pid << "_fs(btex \\subscriptfont{";
			switch (a.t_.chord_.alteration_) {
			case Datamodel::Accord::None: break;
			case Datamodel::Accord::Flat: fout << "\\flatsharpfont{$\\flat$}"; break;
			case Datamodel::Accord::Sharp: fout << "\\flatsharpfont{$\\sharp$}"; break;
			default: {
				std::ostringstream oss;
				oss << __FILE__ << ":" << __LINE__ << ", missing case for alteration_";
				throw std::runtime_error(oss.str());
			}
			}
			fout << "} etex) ;\n" << pid << "_fs.c = " << pid << "_n.ne ;\ndrawunboxed(" << pid << "_fs) ;\n";

			// (* accord *)
			std::string aa;
			if (a.t_.chord_.minor7_) { aa += "\\subscriptfont{7}"; }
			if (a.t_.chord_.major7_) { aa += "\\trianglefont{$\\bigtriangleup$}"; }
			if (a.t_.chord_.sus4_)   { aa += "\\subscriptfont{4}"; }
			if (aa != "") {
				fout << "\nboxit." << pid << "_7(btex " << aa << " etex) ;\n" << pid << "_7.c =  " << pid << "_n.e ; \ndrawunboxed(" << pid << "_7) ;\n";
			}
		}
		else {}
	};   // lambda e_of_chord_without_position 



	std::function<void(const std::string& pid, int i, const Datamodel::Accord& a, int nb)> e_of_chord_with_position = [&fout, index, bar, &e_of_chord_with_position, &e_of_chord_without_position, &has_position]
		(const std::string& pid, int i, const Datamodel::Accord& a, int nb) {
		std::string bid;
		{ std::ostringstream oss; oss << pid << "_" << i; bid = oss.str(); }
		// assert(false) ;
		if (a.t_.has_chord_) {
			fout << "\n\
																																									%% " __FILE__ << ":" << __LINE__ << "\n\
																																																																																																																																																																																													%% print.note : " << a.t_.chord_.note_ << "\n\
																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																					boxit." << bid << "_a(btex \\notefont{" << a.t_.chord_.note_ << "} etex) ;\n \
																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																							boxit." << bid << "_m(btex \\subscriptfont{";
			if (a.t_.chord_.minor_) { fout << "m"; }
			fout << "} etex) ; \n\
																																									";
			fout << "\n\
																																									" << bid << "_a.sw = " << pid << ".sw ;\n	\
																																																																																																																																																																																													" << bid << "_a.ne = " << pid << ".ne ;\n\
																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																					%fill bpath." << bid << "_a withcolor (.8,.2,.1) ;\n\
																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																							";
			fout << "\n\
																																									drawboxed(" << bid << "_a) ;\n\
																																																																																																																																																																																													";

			// (* majeur mineur *)
			fout << "\n\
																																									" << bid << "_m.se = " << bid << "_a.se ;\n\
																																																																																																																																																																																													" << bid << "_m.nw = 0.2 * " << bid << "_a.nw + 0.8 * " << bid << "_a.se ; \n\
																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																					drawunboxed(" << bid << "_m) ; \n\
																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																							";
			//	    (* flat sharp *)
			fout << "\n\
																																									boxit." << bid << "_fs(btex ";
			switch (a.t_.chord_.alteration_) {
			case Datamodel::Accord::None: break;
			case Datamodel::Accord::Flat: fout << "\\flatsharpfont{$\\flat$}"; break;
			case Datamodel::Accord::Sharp: fout << "\\flatsharpfont{$\\sharp$}"; break;
			default: throw std::runtime_error("bad case");
			}
			fout << " etex) ; \n\
																																									" << bid << "_fs.ne = " << bid << "_a.ne ; \n\
																																																																																																																																																																																													" << bid << "_fs.sw = 0.2 * " << bid << "_a.nw + 0.8 * " << bid << "_a.se ; \n\
																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																					drawunboxed(" << bid << "_fs) ; \n\
																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																							";
		}
		else { // no chord 
			fout << "\n\
																																									(" << Datamodel::tex_silence << " etex,(((x1-x0)*" << a.t_.position_ << "/" << nb << ",height*0.6))) ; \n";
		}
	};

	fout << "\n%   draw (x0,base_line) -- (x1,base_line) ;\n\n%draw (x0,base_line+height) -- (x1,base_line+height) ;\n% " << __FILE__ << ":" << __LINE__ << "\nboxit.b" << index << "() ;\nb" << index << ".sw=(x0,base_line) ;\n";
	fout << "b" << index << ".ne = (x1, base_line + height); \n%fill bpath.b" << index << " withcolor (.8,.2,.8) ;\ndrawboxed(b" << index << ") ;\n";

	// if (a.t_.has_position_) {
	if (false) {
		double nb = 4.5;
		int count = 0;
		for (auto c : bar.chords_) {
			//assert(false) ;
			int pos = c.t_.position_;
			pos = 0;
			std::string bid; {
				std::ostringstream oss; oss << "b" << index << "_" << count;
				bid = oss.str();
			}
			fout << "\n% has position\n% " << __FILE__ << ":" << __LINE__ << "\nboxit." << bid << "() ;\n";

			if (count % 2 == 0) {
				fout << "\n" << bid << ".sw=(x0+(x1-x0)*(" << pos << "/2-0.5)/" << nb << ",base_line+height/2) ;\n" << bid << ".ne=(x0+(x1-x0)*(" << pos << "/2+0.5)/" << nb << ",base_line+height) ;\n";
			}
			else {
				fout << "\n" << bid << ".sw=(x0+(x1-x0)*(" << pos << "/2-0.5)/" << nb << ",base_line) ;\n" << bid << ".ne=(x0+(x1-x0)*(" << pos << "/2+0.5)/" << nb << ",base_line+height/2) ;\n";
			}
			fout << "\ndrawunboxed(" << bid << ") ;\n";
			count++;

			e_of_chord_with_position(bid, count, c, nb);
		}
	}
	else {
		int nb = bar.chords_.size();
		int i = 0;
		for (auto c : bar.chords_) {
			std::string bid;
			{ std::ostringstream oss; oss << "b" << index << "_" << i; bid = oss.str(); }
			fout << "\n% " << __FILE__ << ":" << __LINE__ << "\n% has no position\nboxit." << bid << "() ;\nx00 := x0 + 3 ;\nx11 := x1 - 3 ;\n" << bid << ".sw=(x00+(x11-x00)*" << i << "/" << nb << ",base_line) ;\n";
			fout << bid << ".ne=(x00+(x11-x00)*(" << i << "+1)/" << nb << ",base_line+height) ;\ndrawunboxed(" << bid << ") ;\n";
			e_of_chord_without_position(bid, c, nb, i);
			i++;
		}
	}
	fout << "\n\nx0:=x1 ;\n";



}


void write_line(std::ofstream& fout, int index, const Grille::ligne& ligne) {
	fout << "\n%% write line\nx0:=0 ;\n";
	std::accumulate(ligne.bars_.begin(), ligne.bars_.end(), 0,
		[&fout, index](int i, const Grille::bar& bar){
		write_bar(fout, (index * 100 + i), bar);
		return (i + 1);
	});

	fout << "\ndraw(x1,base_line) -- (x1,base_line+height) ;" << std::endl;
	if (ligne.repeat_ > 1) {
		fout << "pickup pencircle scaled 1 ; " << std::endl
			<< "draw halfcircle scaled (height/2) rotated -90 shifted (x1+0.1cm,base_line+height/2) withcolor black ; " << std::endl
			<< "boxit.repeat_" << index << "(btex \\notefont{" << ligne.repeat_ << "} etex) ;" << std::endl
			<< "repeat_" << index << ".w = (x1+0.2cm,base_line+height/2) ;" << std::endl
			<< "drawunboxed(repeat_" << index << ");" << std::endl;
	}
	fout << "\nbase_line := base_line - gap_base_line - height ;\n";

}


void grille_write_mp(const std::string& filename, const Grille& grille) {
	std::cout << "Write mp grille '" << filename << "'" << std::endl;
	std::ofstream fout(filename);
	if (!fout.good()) {
		std::cout << "could not open for writing : '" << filename << "'" << std::endl;
		assert(false);
	}
	fout << "verbatimtex" << std::endl;
	fout << "%&latex" << std::endl;
	fout << "\\documentclass[12pt]{article}" << std::endl;
	fout << "\\usepackage{fixltx2e}" << std::endl;
	fout << "\\usepackage{amssymb}" << std::endl;
	fout << "\\usepackage{wasysym}" << std::endl;
	fout << "\\usepackage{latexsym}" << std::endl;
	fout << "\\usepackage{nicefrac}" << std::endl;
	fout << "\\usepackage{textcomp}" << std::endl;
	fout << "\\newcommand*{\\notefont}{\\fontfamily{ptm}\\fontsize{12}{12}\\selectfont}" << std::endl;
	fout << "\\newcommand*{\\subscriptfont}{\\fontfamily{ptm}\\fontsize{8}{8}\\fontshape{it}\\selectfont}" << std::endl;
	fout << "\\newcommand*{\\flatsharpfont}{\\fontfamily{ptm}\\fontsize{8}{8}\\fontshape{it}\\selectfont}" << std::endl;
	fout << "\\newcommand*{\\trianglefont}{\\fontfamily{ptm}\\fontsize{4}{4}\\selectfont}" << std::endl;
	fout << "\\begin{document}" << std::endl;
	fout << "etex" << std::endl;
	fout << "input boxes ;" << std::endl;
	fout << "beginfig(1) ;" << std::endl;
	fout << "uy=0.2cm ;" << std::endl;
	fout << "%%  ux=0.15cm ;" << std::endl;
	fout << "unote=0.1cm ;" << std::endl;
	fout << "%%  b=10*uy ;" << std::endl;
	fout << "base_line     = 0*uy ;" << std::endl;
	fout << "gap_base_line = 0*uy ;" << std::endl;
	fout << "height        = 4*uy ;" << std::endl;
	fout << "width         = 2cm ;" << std::endl;
	fout << "%% scale pour le normal chord" << std::endl;
	fout << "schord=1.3" << std::endl;
	fout << "%% scale pour le susbcript chord" << std::endl;
	fout << "sschord=1 ;" << std::endl;
	fout << "pickup pencircle scaled 0.15bp ;" << std::endl;

	std::accumulate(grille.t_.lignes_.begin(), grille.t_.lignes_.end(),
		0,
		[&fout](int index, const Grille::ligne& l) {
		write_line(fout, index, l);
		return (index + 1);
	});

	fout << "endfig ;" << std::endl;
	fout << "\\end{document}" << std::endl;
	fout << "bye" << std::endl;
	/*
	let command = sprintf "mpost %s-grille-%d.mp > /dev/null " (Filename.basename name) count in
	let ret = Unix.system command in
	let () = match ret with
	| Unix.WEXITED 0 -> ()
	| _ -> let msg = sprintf "%s\nfailed" command in failwith msg
	in
	let target = sprintf "%s-grille-%d.1" (Filename.basename name) count in
	let () = if Sys.file_exists target then (
	let target2 =  sprintf "%s-grille-%d.mps" (Filename.basename name) count  in
	Sys.rename target target2
	)
	else
	failwith "mpost failed, cannot find target file"
	in
	()
	*/
}
