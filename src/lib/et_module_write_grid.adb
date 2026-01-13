------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE WRITE / GRID SETTING                           --
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

with et_schematic_geometry;
with et_board_geometry;

with et_coordinates_formatting;		use et_coordinates_formatting;

with et_file_write;					use et_file_write;
with et_file_sections;				use et_file_sections;



package body et_module_write_grid is

	use pac_generic_modules;


	
	procedure write_drawing_grid (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is 

			
			procedure schematic is
				use et_schematic_geometry;
				use pac_geometry_2;
				use pac_grid;
				g : type_grid;
			begin
				g := get_grid_schematic (module);

				write (keyword => keyword_on_off, parameters => to_string (g.on_off));
				write (keyword => keyword_spacing, parameters => to_string (g.spacing, FORMAT_2));
				write (keyword => keyword_style, parameters => to_string (g.style));
			end schematic;


			procedure board is
				use et_board_geometry;
				use pac_geometry_2;
				use pac_grid;
				g : type_grid;
			begin
				g := get_grid_board (module);

				write (keyword => keyword_on_off, parameters => to_string (g.on_off));
				write (keyword => keyword_spacing, parameters => to_string (g.spacing, FORMAT_2));
				write (keyword => keyword_style, parameters => to_string (g.style));
			end board;



		begin
			section_mark (section_drawing_grid, HEADER);

			section_mark (section_schematic, HEADER);
			schematic;
			section_mark (section_schematic, FOOTER);

			section_mark (section_board, HEADER);
			board;
			section_mark (section_board, FOOTER);
			
			section_mark (section_drawing_grid, FOOTER);
		end query_module;


		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " write drawing grid",
			 level => log_threshold);

		log_indentation_up;
		query_element (module_cursor, query_module'access);
		log_indentation_down;
		
	end write_drawing_grid;


	
	
end et_module_write_grid;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
