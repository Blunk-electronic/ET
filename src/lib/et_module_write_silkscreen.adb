------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE WRITE / SILKSCREEN                             --
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
--                                                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
-- ToDo:
-- - clean up
--
--
--

with ada.text_io;					use ada.text_io;
with ada.characters;				use ada.characters;
with ada.strings;					use ada.strings;

with et_module;						use et_module;
with et_module_names;				use et_module_names;
with et_keywords;					use et_keywords;
with et_module_board;				use et_module_board;
with et_board_geometry;				use et_board_geometry;
with et_directions;					use et_directions;
with et_silkscreen;					use et_silkscreen;
with et_silkscreen.board;			use et_silkscreen.board;

with et_file_write;					use et_file_write;
with et_file_sections;				use et_file_sections;


package body et_module_write_silkscreen is

	use pac_generic_modules;
	use pac_geometry_2;
	use pac_file_rw;

	

	procedure write_silkscreen (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is
		use pac_silk_lines;
		use pac_silk_arcs;
		use pac_silk_circles;

		
		procedure write_line (cursor : in pac_silk_lines.cursor) is 
			use pac_silk_lines;
		begin
			section_mark (section_line, HEADER);
			write_line (element (cursor));		
			write (keyword => keyword_width, parameters => to_string (element (cursor).width));
			section_mark (section_line, FOOTER);
		end write_line;

		
		procedure write_arc (cursor : in pac_silk_arcs.cursor) is 
			use pac_silk_arcs;
		begin
			section_mark (section_arc , HEADER);
			write_arc (element (cursor));
			write (keyword => keyword_width, parameters => to_string (element (cursor).width));
			section_mark (section_arc , FOOTER);
		end write_arc;

		
		procedure write_circle (cursor : in pac_silk_circles.cursor) is 
			use pac_silk_circles;
		begin
			section_mark (section_circle, HEADER);
			write_circle (element (cursor));
			write (keyword => keyword_width, parameters => to_string (element (cursor).width));
			section_mark (section_circle, FOOTER);
		end write_circle;

		

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is 
			silkscreen : type_silkscreen_both_sides 
				renames module.board.silkscreen;
		begin
			case face is
				when TOP =>
					iterate (silkscreen.top.lines, write_line'access);
					iterate (silkscreen.top.arcs, write_arc'access);
					iterate (silkscreen.top.circles, write_circle'access);

				when BOTTOM =>
					iterate (silkscreen.bottom.lines, write_line'access);
					iterate (silkscreen.bottom.arcs, write_arc'access);
					iterate (silkscreen.bottom.circles, write_circle'access);
			end case;					
		end query_module;
		
			
	begin
		log (text => "module " & to_string (module_cursor)
			 & " write silkscreen lines, arcs and circles",
			 level => log_threshold);

		log_indentation_up;
		query_element (module_cursor, query_module'access);
		log_indentation_down;
	end write_silkscreen;

	
	
end et_module_write_silkscreen;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
