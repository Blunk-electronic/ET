------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE WRITE / BOARD / OUTLINE                        --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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

with et_module_names;				use et_module_names;
with et_module;						use et_module;
with et_board_geometry;				use et_board_geometry;
with et_keywords;					use et_keywords;

with et_module_read_board_contour;	use et_module_read_board_contour;
with et_board_holes;				use et_board_holes;
with et_board_outline;				use et_board_outline;

with et_section_headers;			use et_section_headers;
with et_general_rw;					use et_general_rw;
with et_board_write;				use et_board_write;


package body et_module_write_board_outline is
	
	use pac_generic_modules;
	use pac_file_rw;
	-- use pac_geometry_2;
	-- use pac_contours;
		
			

	procedure write_board_outline (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is 
		use et_board_holes;
		use et_board_outline;
		use pac_holes;


		
		procedure query_hole (
			hole : in type_hole)
		is begin
			log (text => "hole", level => log_threshold + 2);
			-- CS log lower left corner
			section_mark (section_hole, HEADER);		
			write_polygon_segments (hole);		
			section_mark (section_hole, FOOTER);		
		end;


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is 
			hole_cursor : pac_holes.cursor := module.board.board_contour.holes.first;

		begin
			-- Write outer contour:
			log (text => "outer contour", level => log_threshold + 1);
			
			section_mark (section_outline, HEADER);
			write_polygon_segments (module.board.board_contour.outline);
			section_mark (section_outline, FOOTER);

			-- Write holes:
			log (text => "holes", level => log_threshold + 1);
			log_indentation_up;
			while has_element (hole_cursor) loop
				query_element (hole_cursor, query_hole'access);
				next (hole_cursor);
			end loop;
			log_indentation_down;
			
		end query_module;

								   
	begin
		log (text => "module " & to_string (module_cursor)
			 & " write board outline",
			 level => log_threshold);

		log_indentation_up;
		
		section_mark (section_pcb_contours, HEADER);
		query_element (module_cursor, query_module'access);		
		section_mark (section_pcb_contours, FOOTER);

		log_indentation_down;
	end write_board_outline;

	
	
end et_module_write_board_outline;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
