------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         SYMBOL WRITE / BODY                              --
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

with ada.text_io;				use ada.text_io;
with ada.strings; 				use ada.strings;

with ada.exceptions;

with et_directions;					use et_directions;

with et_schematic_geometry;			use et_schematic_geometry;

with et_coordinates_formatting;		use et_coordinates_formatting;
with et_file_sections;				use et_file_sections;
with et_file_write;					use et_file_write;

with et_symbol_shapes;				use et_symbol_shapes;
with et_keywords;					use et_keywords;


package body et_symbol_write_body is
	
	use pac_geometry_2;


	
	
	procedure write_body_lines ( 
		symbol			: in type_symbol_model;
		log_threshold	: in type_log_level)
	is
		use pac_symbol_lines;
		
		
		procedure write_line (
			cursor : in pac_symbol_lines.cursor) 
		is begin
			section_mark (section_line, HEADER);
			write (keyword => keyword_start, parameters => to_string (get_A (cursor), FORMAT_2));
			write (keyword => keyword_end  , parameters => to_string (get_B (cursor), FORMAT_2));
			write (keyword => keyword_width, parameters => to_string (element (cursor).width));
			section_mark (section_line, FOOTER);
		end write_line;

		
	begin
		log (text => "write body lines", level => log_threshold);
		log_indentation_up;

		iterate (symbol.shapes.lines, write_line'access);

		log_indentation_down;
	end write_body_lines;


	
	
	
	
	
	procedure write_body_arcs ( 
		symbol			: in type_symbol_model;
		log_threshold	: in type_log_level)
	is
		use pac_symbol_arcs;
		
		
		procedure write_arc (
			cursor : in pac_symbol_arcs.cursor) 
		is begin
			section_mark (section_arc, HEADER);
			write (keyword => keyword_center, parameters => to_string (get_center (cursor), FORMAT_2));
			write (keyword => keyword_start , parameters => to_string (get_A (cursor), FORMAT_2));
			write (keyword => keyword_end   , parameters => to_string (get_B (cursor), FORMAT_2));
			write (keyword => keyword_direction, parameters => to_string (get_direction (cursor)));
			write (keyword => keyword_width , parameters => to_string (element (cursor).width));
			section_mark (section_arc, FOOTER);
		end write_arc;

		
	begin
		log (text => "write body arcs", level => log_threshold);
		log_indentation_up;

		iterate (symbol.shapes.arcs, write_arc'access);

		log_indentation_down;
	end write_body_arcs;

	

	
	
	

	
	
	procedure write_body_circles ( 
		symbol			: in type_symbol_model;
		log_threshold	: in type_log_level)
	is
		use pac_symbol_circles;
		
		
		procedure write_circle (
			cursor : in pac_symbol_circles.cursor) 
		is begin
			section_mark (section_circle, HEADER);
			write (keyword => keyword_center, parameters => to_string (get_center (element (cursor)), FORMAT_2));
			write (keyword => keyword_radius, parameters => to_string (get_radius (element (cursor))));
			write (keyword => keyword_width , parameters => to_string (element (cursor).width));
			write (keyword => keyword_filled, parameters => to_string (element (cursor).filled));
			section_mark (section_circle, FOOTER);
		end write_circle;

		
	begin
		log (text => "write body circles", level => log_threshold);
		log_indentation_up;

		iterate (symbol.shapes.circles, write_circle'access);

		log_indentation_down;
	end write_body_circles;
	
	
end et_symbol_write_body;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
