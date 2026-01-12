------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE WRITE / BOARD TRACKS ROUTE                     --
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

with et_module_names;				use et_module_names;
with et_keywords;					use et_keywords;
with et_module;						use et_module;
with et_module_board;				use et_module_board;
with et_route;						use et_route;
with et_pcb_signal_layers;			use et_pcb_signal_layers;
with et_design_rules_board;			use et_design_rules_board;
with et_board_geometry;				use et_board_geometry;
with et_directions;					use et_directions;
with et_board_text;
with et_conductor_segment.boards;	use et_conductor_segment.boards;
with et_conductors_floating_board;	use et_conductors_floating_board;
with et_general_rw;					use et_general_rw;
with et_board_write;				use et_board_write;


package body et_module_write_freetracks is

	use pac_generic_modules;
	use pac_geometry_2;
	-- use pac_signal_layers;
	
	
	
	procedure write_freetracks (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		use pac_conductor_lines;
		use pac_conductor_arcs;
		use pac_conductor_circles;
			
			
		procedure write_line (
			c : in pac_conductor_lines.cursor) 
		is 
			line : type_conductor_line renames element (c);
		begin
			line_begin;
			write_line (line);
			
			write (keyword => keyword_width, 
				parameters => to_string (line.width));
			
			write (keyword => keyword_layer, parameters => to_string (line.layer));
			line_end;
		end;

		
		procedure write_arc (
			c : in pac_conductor_arcs.cursor) 
		is 
			arc : type_conductor_arc renames element (c);
		begin
			arc_begin;
			write_arc (arc);

			write (keyword => keyword_width, 
				parameters => to_string (arc.width));
				
			write (keyword => keyword_layer, parameters => to_string (arc.layer));
			arc_end;
		end;

		
		procedure write_circle (
			c : in pac_conductor_circles.cursor) 
		is 
			circle : type_conductor_circle renames element (c);
		begin
			circle_begin;
			write_circle (circle);

			write (keyword => keyword_width, 
				parameters => to_string (circle.width));
			
			write (keyword => keyword_layer, parameters => to_string (circle.layer));
			circle_end;			
		end;

	
	
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is 
			tracks : type_conductors_floating renames module.board.conductors_floating;
		begin
			iterate (tracks.lines, write_line'access);
			iterate (tracks.arcs, write_arc'access);
			iterate (tracks.circles, write_circle'access);
		end query_module;
		
		
		
	begin
		log (text => "module " & to_string (module_cursor),
			 level => log_threshold);

		log_indentation_up;
		query_element (module_cursor, query_module'access);
		log_indentation_down;
	end write_freetracks;
			 
			 


	

end et_module_write_freetracks;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
