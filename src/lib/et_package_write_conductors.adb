------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                     PACKAGE WRITE / CONDUCTORS                           --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--
-- To Do:
-- - clean up, use renames



with ada.text_io;				use ada.text_io;
-- with ada.characters.handling;	use ada.characters.handling;
-- with ada.strings; 				use ada.strings;

with et_keywords;						use et_keywords;
with et_package_sections;				use et_package_sections;

with et_board_geometry;					use et_board_geometry;

with et_conductor_segment;				use et_conductor_segment;
with et_conductor_text.packages;		use et_conductor_text.packages;
with et_text_content;					use et_text_content;

with et_file_write;						use et_file_write;


package body et_package_write_conductors is

	use pac_geometry_2;
	use pac_file_rw;
	
	use et_conductor_segment;
	
	use pac_conductor_lines;
	use pac_conductor_arcs;
	use pac_conductor_circles;
	use pac_conductor_texts;
	
	

	procedure write_conductors (
		packge			: in type_package_model;
		log_threshold	: in type_log_level) 
	is
	
		procedure write_line (cursor : in pac_conductor_lines.cursor) is 
			line : type_conductor_line renames element (cursor);
		begin
			section_mark (section_line, HEADER);
			write_line (element (cursor));
			
			write (keyword => keyword_width, 
				parameters => to_string (line.width));
			
			section_mark (section_line, FOOTER);
		end write_line;

		
		procedure write_arc (cursor : in pac_conductor_arcs.cursor) is 
			arc : type_conductor_arc renames element (cursor);
		begin
			section_mark (section_arc , HEADER);
			write_arc (element (cursor));

			write (keyword => keyword_width, 
				parameters => to_string (arc.width));
			
			section_mark (section_arc , FOOTER);
		end write_arc;

		
		procedure write_circle (cursor : in pac_conductor_circles.cursor) is 
			circle : type_conductor_circle renames element (cursor);
		begin
			section_mark (section_circle, HEADER);
			write_circle (circle);
			
			write (keyword => keyword_width, 
				parameters => to_string (circle.width));
			
			section_mark (section_circle, FOOTER);		
		end write_circle;


		procedure write_text (cursor : in pac_conductor_texts.cursor) is begin
			section_mark (section_text, HEADER);
			write (keyword => keyword_content, wrap => true,
				parameters => to_string (element (cursor).content));
			-- CS write_text_properties (element (cursor));
			section_mark (section_text, FOOTER);
		end write_text;


		
	begin
		log (text => "write conductors", level => log_threshold);

		section_mark (section_conductor, HEADER);

		-- top
		section_mark (section_top, HEADER);			
		iterate (packge.conductors.top.lines, write_line'access);
		iterate (packge.conductors.top.arcs, write_arc'access);
		iterate (packge.conductors.top.circles, write_circle'access);
		iterate (packge.conductors.top.texts, write_text'access);
		-- CS zones
		section_mark (section_top, FOOTER);

		-- bottom
		section_mark (section_bottom, HEADER);			
		iterate (packge.conductors.bottom.lines, write_line'access);
		iterate (packge.conductors.bottom.arcs, write_arc'access);
		iterate (packge.conductors.bottom.circles, write_circle'access);
		iterate (packge.conductors.bottom.texts, write_text'access);			
		-- CS zones
		section_mark (section_bottom, FOOTER);

		section_mark (section_conductor, FOOTER);
	
	end write_conductors;

	
end et_package_write_conductors;
