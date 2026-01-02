------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE READ / BOARD FREETRACKS                        --
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
with ada.characters;				use ada.characters;
with ada.strings;					use ada.strings;

with et_module;						use et_module;
with et_module_names;				use et_module_names;
with et_keywords;					use et_keywords;
with et_route;						use et_route;
with et_pcb_signal_layers;			use et_pcb_signal_layers;
with et_design_rules_board;			use et_design_rules_board;
with et_board_geometry;				use et_board_geometry;
with et_directions;					use et_directions;
with et_board_text;
with et_conductor_segment.boards;	use et_conductor_segment.boards;

with et_general_rw;					use et_general_rw;



package body et_module_read_freetracks is

	use pac_generic_modules;
	use pac_geometry_2;
	use pac_signal_layers;

	track_line : type_conductor_line;
	track_arc : type_conductor_arc;
	track_circle : type_conductor_circle;
	

	
	
	procedure read_freetrack_line (
		line : in type_fields_of_line)
	is
		kw : string := f (line, 1);
		p : type_vector_model;
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_start then -- start x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the start position starting at field 2 of line
			p := to_vector_model (line, 2);
			set_A (track_line, p);
			
		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			p := to_vector_model (line, 2);
			set_B (track_line, p);

		
		elsif kw = keyword_layer then -- layer 2
			expect_field_count (line, 2);
			track_line.layer := to_signal_layer (f (line, 2));


		elsif kw = keyword_width then -- width 0.5
			expect_field_count (line, 2);
			track_line.width := to_distance (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;
	end read_freetrack_line;



	
	
	procedure read_freetrack_arc (
		line : in type_fields_of_line)
	is
		kw : string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_start then -- start x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the start position starting at field 2 of line
			set_A (track_arc, to_vector_model (line, 2));

		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			set_B (track_arc, to_vector_model (line, 2));
			
		elsif kw = keyword_center then -- center x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			set_center (track_arc, to_vector_model (line, 2));

		elsif kw = keyword_direction then -- direction ccw
			expect_field_count (line, 2);

			set_direction (track_arc, to_direction (f (line, 2)));

			
		elsif kw = keyword_layer then -- layer 2
			expect_field_count (line, 2);
			track_arc.layer := to_signal_layer (f (line, 2));


		elsif kw = keyword_width then -- width 0.5
			expect_field_count (line, 2);
			track_arc.width := to_distance (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;
	end read_freetrack_arc;

	

	
	
	
	procedure read_freetrack_circle (
		line : in type_fields_of_line)
	is
		kw : string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_center then -- center x 150 y 45
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			set_center (track_circle, to_vector_model (line, 2));
			
		elsif kw = keyword_radius then -- radius 22
			expect_field_count (line, 2);
			
			set_radius (track_circle, to_radius (f (line, 2)));
			
		elsif kw = keyword_layer then -- layer 2
			expect_field_count (line, 2);
			track_circle.layer := to_signal_layer (f (line, 2));


		elsif kw = keyword_width then -- width 0.5
			expect_field_count (line, 2);
			track_circle.width := to_distance (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;
	end read_freetrack_circle;

	
	
	
	
	
	
	procedure insert_freetrack_line (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		use pac_conductor_lines;
		

		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			pac_conductor_lines.append (
				container	=> module.board.conductors_floating.lines,
				new_item	=> track_line);
				
			-- CS use procedure add_line to add a line
			-- to the floating conductors
		end;
		
		
	begin
		-- CS log messages
		-- CS check signal layer (use get_deepest_conductor_layer (module_cursor))
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);
			
		-- Reset line for next line:
		reset_line (track_line);
	
	end insert_freetrack_line;

	
	
	
		
		
	procedure insert_freetrack_arc (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		use pac_conductor_arcs;
		

		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			pac_conductor_arcs.append (
				container	=> module.board.conductors_floating.arcs,
				new_item	=> track_arc);
				
			-- CS use procedure add_arc to add an arc
			-- to the floating conductors
		end;
		
		
	begin
		-- CS log messages
		-- CS check signal layer (use get_deepest_conductor_layer (module_cursor))
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);
			
		-- Reset arc for next arc:
		reset_arc (track_arc);
	
	end insert_freetrack_arc;


	
	
	
	
	procedure insert_freetrack_circle (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		use pac_conductor_circles;
		

		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			pac_conductor_circles.append (
				container	=> module.board.conductors_floating.circles,
				new_item	=> track_circle);
				
			-- CS use procedure add_circle to add a circle
			-- to the floating conductors of the module
		end;
		
		
	begin
		-- CS log messages
		-- CS check signal layer (use get_deepest_conductor_layer (module_cursor))
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);
			
		reset_circle (track_circle);
	
	end insert_freetrack_circle;

	
	

end et_module_read_freetracks;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
