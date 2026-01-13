------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         PACKAGE READ / CONTOUR                           --
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

with ada.text_io;						use ada.text_io;
with ada.strings; 						use ada.strings;

with et_primitive_objects;				use et_primitive_objects;
with et_coordinates_formatting;			use et_coordinates_formatting;
with et_keywords;						use et_keywords;
with et_directions;						use et_directions;



package body et_package_read_contour is

	

	procedure read_contour_line (
		line : type_fields_of_line)
	is
		kw : constant string := f (line, 1);
		vm : type_vector_model;
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_start then -- start x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the start position starting at field 2 of line
			vm := to_vector_model (line, 2);
			set_A (contour_line, vm);
			
		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			vm := to_vector_model (line, 2);
			set_B (contour_line, vm);
			
		else
			invalid_keyword (kw);
		end if;
	end;




	
	procedure read_contour_arc (
		line : type_fields_of_line) 
	is
		kw : constant string := f (line, 1);
	begin
		if kw = keyword_start then -- start x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the start position starting at field 2 of line
			set_A (contour_arc, to_vector_model (line, 2));

		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			set_B (contour_arc, to_vector_model (line, 2));
			
		elsif kw = keyword_center then -- center x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			set_center (contour_arc, to_vector_model (line, 2));

		elsif kw = keyword_direction then -- direction ccw
			expect_field_count (line, 2);

			set_direction (contour_arc, to_direction (f (line, 2)));
			
		else
			invalid_keyword (kw);
		end if;
	end read_contour_arc;




	
	
	procedure read_contour_circle (
		line : type_fields_of_line)
	is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_center then -- center x 150 y 45
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			set_center (contour_circle, to_vector_model (line, 2));
			
		elsif kw = keyword_radius then -- radius 22
			expect_field_count (line, 2);
			
			set_radius (contour_circle, to_radius (f (line, 2)));
		else
			invalid_keyword (kw);
		end if;
	end;

	
	
	
	
	

	procedure insert_contour_line is begin
		append_segment (contour, (LINE, contour_line));
		reset_line (contour_line);
	end;

	


	procedure insert_contour_arc is begin
		-- CS board_check_arc (log_threshold + 1);
		
		append_segment (contour, (ARC, contour_arc));
		reset_arc (contour_arc);
	end;



	
	procedure insert_contour_circle is begin
		-- The global contour variable "mutates" so that the contours
		-- consist of a single circle:
		contour := (
			contour	=> (circular => true, others => <>),
			others	=> <>);

		-- From now on the contour consists of just a single circle.
		-- Any attempt to append a line or an arc causes a discriminant error.
		
		-- Assign the circle to the contour:
		set_circle (contour, contour_circle);
		reset_circle (contour_circle);
	end;


	
	
end et_package_read_contour;
