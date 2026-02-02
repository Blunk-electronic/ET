------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         SYMBOL READ / BODY                               --
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
-- To Do:
-- - clean up
--
--


with ada.text_io;					use ada.text_io;
with ada.exceptions;

with et_schematic_geometry;			use et_schematic_geometry;
with et_directions;					use et_directions;
with et_symbol_shapes;				use et_symbol_shapes;

with et_keywords;					use et_keywords;


package body et_symbol_read_body is

	use pac_geometry_2;



	symbol_line 	: type_symbol_line;
	symbol_arc		: type_symbol_arc;
	symbol_circle	: type_symbol_circle;
	


	procedure read_body_line (
		line : in type_fields_of_line)
	is
		kw : string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_start then -- start x 1 y 2
			expect_field_count (line, 5);

			-- extract the start position starting at field 2
			set_A (symbol_line, to_vector_model (line, 2));
			
		elsif kw = keyword_end then -- end x 0.00 y 0.00
			expect_field_count (line, 5);

			-- extract the end position starting at field 2
			set_B (symbol_line, to_vector_model (line, 2));

		elsif kw = keyword_width then
			expect_field_count (line, 2);
			symbol_line.width := to_distance (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;
	end read_body_line;
	
				
		
		
		
	procedure insert_body_line (
		symbol			: in type_symbol_model_access;
		log_threshold	: in type_log_level)
	is		
		inserted : boolean;
	begin
		log (text => "insert body line", level => log_threshold);
		log_indentation_up;

		-- append symbol_line to unit_symbol
		pac_symbol_lines.append (
			container	=> symbol.shapes.lines,
			new_item	=> symbol_line);

		-- clean up for next line
		reset_line (symbol_line);
		
		log_indentation_down;
	end insert_body_line;
	





	


	procedure read_body_arc (
		line : in type_fields_of_line)
	is
		kw : string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_center then -- center x 1 y 2
			expect_field_count (line, 5);

			-- extract the start position starting at field 2
			set_center (symbol_arc, to_vector_model (line, 2));

		elsif kw = keyword_start then -- start x 1 y 2
			expect_field_count (line, 5);

			-- extract the start position starting at field 2
			set_A (symbol_arc, to_vector_model (line, 2));
			
		elsif kw = keyword_end then -- end x 0.00 y 0.00
			expect_field_count (line, 5);

			-- extract the end position starting at field 2
			set_B (symbol_arc, to_vector_model (line, 2));

		elsif kw = keyword_direction then -- direction ccw
			expect_field_count (line, 2);

			set_direction (symbol_arc, to_direction (f (line, 2)));
			
		elsif kw = keyword_width then
			expect_field_count (line, 2);
			symbol_arc.width := to_distance (f (line, 2));

		else
			invalid_keyword (kw);
		end if;
	end read_body_arc;
	
				
		
		
		
	procedure insert_body_arc (
		symbol			: in type_symbol_model_access;
		log_threshold	: in type_log_level)
	is		
		inserted	: boolean;
	begin
		log (text => "insert body arc", level => log_threshold);
		log_indentation_up;

		-- append symbol_arc to unit_symbol
		pac_symbol_arcs.append (
			container	=> symbol.shapes.arcs,
			new_item	=> symbol_arc);

		-- clean up for next arc
		reset_arc (symbol_arc);
		
		log_indentation_down;
	end insert_body_arc;

	






	


	procedure read_body_circle (
		line : in type_fields_of_line)
	is
		kw : string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_center then -- center x 1 y 2
			expect_field_count (line, 5);

			-- extract the start position starting at field 2
			set_center (symbol_circle, to_vector_model (line,2));

		elsif kw = keyword_width then -- widht 0.2
			expect_field_count (line, 2);
			set_width (symbol_circle, to_distance (f (line, 2)));

		elsif kw = keyword_radius then -- radius 5
			expect_field_count (line, 2);
			set_radius (symbol_circle, to_radius (f (line, 2)));

		elsif kw = keyword_filled then -- filled yes/no
			expect_field_count (line, 2);
			symbol_circle.filled := to_circle_filled (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;
	end read_body_circle;
	
				
		
		
		
	procedure insert_body_circle (
		symbol			: in type_symbol_model_access;
		log_threshold	: in type_log_level)
	is		
		inserted : boolean;
	begin
		log (text => "insert body circle", level => log_threshold);
		log_indentation_up;

		-- append symbol_circle to unit_symbol
		pac_symbol_circles.append (
			container	=> symbol.shapes.circles,
			new_item	=> symbol_circle);

		-- clean up for next circle
		reset_circle (symbol_circle);
		
		log_indentation_down;
	end insert_body_circle;

	
end et_symbol_read_body;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
