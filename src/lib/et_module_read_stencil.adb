------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                       MODULE READ / STENCIL                              --
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
with et_board_geometry;				use et_board_geometry;
with et_directions;					use et_directions;
with et_stencil;					use et_stencil;
with et_stencil.board;				use et_stencil.board;
with et_general_rw;					use et_general_rw;



package body et_module_read_stencil is

	use pac_generic_modules;
	use pac_geometry_2;

	
	stencil_line : type_stencil_line;
	stencil_arc : type_stencil_arc;
	stencil_circle : type_stencil_circle;
	





	procedure read_stencil_line (
		line	: in type_fields_of_line)
	is
		kw : string := f (line, 1);
		p : type_vector_model;
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_start then -- start x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the start position starting at field 2 of line
			p := to_vector_model (line, 2);
			set_A (stencil_line, p);

			
		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			p := to_vector_model (line, 2);
			set_B (stencil_line, p);

		
		elsif kw = keyword_width then -- width 0.5
			expect_field_count (line, 2);
			stencil_line.width := to_distance (f (line, 2));
												
			
		else
			invalid_keyword (kw);
		end if;
	end read_stencil_line;


	

	
	
	
	
	procedure read_stencil_arc (
		line	: in type_fields_of_line)
	is
		kw : string := f (line, 1);
		p : type_vector_model;
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_start then -- start x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the start position starting at field 2 of line
			p := to_vector_model (line, 2);
			set_A (stencil_arc, p);

			
		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			p := to_vector_model (line, 2);
			set_B (stencil_arc, p);
		
			
		elsif kw = keyword_center then -- center x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			set_center (stencil_arc, to_vector_model (line, 2));

			
		elsif kw = keyword_direction then -- direction ccw
			expect_field_count (line, 2);

			set_direction (stencil_arc, to_direction (f (line, 2)));


		elsif kw = keyword_width then -- width 0.5
			expect_field_count (line, 2);
			stencil_arc.width := to_distance (f (line, 2));

			
		else
			invalid_keyword (kw);
		end if;
	end read_stencil_arc;

	

	

	
	
	
	procedure read_stencil_circle (
		line	: in type_fields_of_line)
	is
		kw : string := f (line, 1);
	begin
		if kw = keyword_center then -- center x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			set_center (stencil_circle, to_vector_model (line, 2));

			
		elsif kw = keyword_radius then
			expect_field_count (line, 2);

			set_radius (stencil_circle, to_radius (f (line, 2)));


		elsif kw = keyword_width then -- width 0.5
			expect_field_count (line, 2);
			stencil_circle.width := to_distance (f (line, 2));

			
		else
			invalid_keyword (kw);
		end if;
	end read_stencil_circle;
	
	

	
	
	
	
	procedure insert_stencil_line (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is
		use pac_stencil_lines;
		
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			-- CS check line
			add_line (module.board.stencil, stencil_line, face);
		end do_it;

					
	begin
		-- CS log messages
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

		reset_line (stencil_line);
	end insert_stencil_line;


	
	
	
	
	
	procedure insert_stencil_arc (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is
		use pac_stencil_arcs;
		
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			-- CS check arc
			add_arc (module.board.stencil, stencil_arc, face);
		end do_it;

					
	begin
		-- CS log messages
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

		reset_arc (stencil_arc);
	end insert_stencil_arc;

	
	
	
	
	

		
	procedure insert_stencil_circle (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is
		use pac_stencil_circles;
		
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			-- CS check circle
			add_circle (module.board.stencil, stencil_circle, face);
		end do_it;

					
	begin
		-- CS log messages
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);
			
		reset_circle (stencil_circle);		
	end insert_stencil_circle;
	
	
	
end et_module_read_stencil;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
