------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                       PACKAGE READ / SILKSCREEN                          --
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

with et_design_rules_board;				use et_design_rules_board;
with et_board_geometry;					use et_board_geometry;
with et_board_text;

with et_primitive_objects;				use et_primitive_objects;
with et_coordinates_formatting;			use et_coordinates_formatting;
with et_keywords;						use et_keywords;
with et_package_model;					use et_package_model;
with et_directions;						use et_directions;

with et_silkscreen;						use et_silkscreen;
with et_silkscreen.packages;			use et_silkscreen.packages;

with et_device_placeholders.packages;

with et_general_rw;						use et_general_rw;
with et_package_read_contour;			use et_package_read_contour;
with et_package_read_text;				use et_package_read_text;


package body et_package_read_silkscreen is

	use pac_geometry_2;	
	
	
	silk_line : type_silk_line;
	silk_arc : type_silk_arc;
	silk_circle : type_silk_circle;
	





	procedure read_silk_line (
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
			set_A (silk_line, p);

			
		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			p := to_vector_model (line, 2);
			set_B (silk_line, p);

		
		elsif kw = keyword_width then -- width 0.5
			expect_field_count (line, 2);
			silk_line.width := to_distance (f (line, 2));
												
			
		else
			invalid_keyword (kw);
		end if;
	end read_silk_line;


	

	
	
	
	
	procedure read_silk_arc (
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
			set_A (silk_arc, p);

			
		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			p := to_vector_model (line, 2);
			set_B (silk_arc, p);
		
			
		elsif kw = keyword_center then -- center x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			set_center (silk_arc, to_vector_model (line, 2));

			
		elsif kw = keyword_direction then -- direction ccw
			expect_field_count (line, 2);

			set_direction (silk_arc, to_direction (f (line, 2)));


		elsif kw = keyword_width then -- width 0.5
			expect_field_count (line, 2);
			silk_arc.width := to_distance (f (line, 2));

			
		else
			invalid_keyword (kw);
		end if;
	end read_silk_arc;

	

	

	
	
	
	procedure read_silk_circle (
		line	: in type_fields_of_line)
	is
		kw : string := f (line, 1);
	begin
		if kw = keyword_center then -- center x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			set_center (silk_circle, to_vector_model (line, 2));

			
		elsif kw = keyword_radius then
			expect_field_count (line, 2);

			set_radius (silk_circle, to_radius (f (line, 2)));


		elsif kw = keyword_width then -- width 0.5
			expect_field_count (line, 2);
			silk_circle.width := to_distance (f (line, 2));

			
		else
			invalid_keyword (kw);
		end if;
	end read_silk_circle;






	procedure insert_silk_line (
		packge			: in type_package_model_access;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is 
		use pac_silk_lines;
	begin
		case face is
			when TOP => 
				append (packge.silkscreen.top.lines, silk_line);

			when BOTTOM => 
				append (packge.silkscreen.bottom.lines, silk_line);
		end case;
		-- CS use procedure add_line
			
		-- clean up for next line
		reset_line (silk_line);		
	end insert_silk_line;
	

	
	
	
	procedure insert_silk_arc (
		packge			: in type_package_model_access;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is 
		use pac_silk_arcs;
	begin
		-- CS check arc
		
		case face is
			when TOP => 
				append (packge.silkscreen.top.arcs, silk_arc);

			when BOTTOM => 
				append (packge.silkscreen.bottom.arcs, silk_arc);
		end case;
		-- CS use procedure add_arc
		
		-- clean up for next arc
		reset_arc (silk_arc);		
	end insert_silk_arc;

	
	

	
	
	procedure insert_silk_circle (
		packge			: in type_package_model_access;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is 
		use pac_silk_circles;
	begin
		case face is
			when TOP => 
				append (packge.silkscreen.top.circles, silk_circle);

			when BOTTOM => 
				append (packge.silkscreen.bottom.circles, silk_circle);
		end case;
		-- CS use procedure add_circle
		
		-- clean up for next circle
		reset_circle (silk_circle);		
	end insert_silk_circle;




	
	
	
	procedure insert_silk_zone (
		packge			: in type_package_model_access;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is 
		use pac_contours;
	begin
		add_zone (packge.silkscreen, (contour with null record), face);

		-- clean up for next zone
		reset_contour (contour);
	end insert_silk_zone;

	
	


	procedure insert_silk_text (
		packge			: in type_package_model_access;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is 
		use et_board_text;
		use pac_text_board_vectorized;
	begin
		add_text (packge.silkscreen, (pac_text with null record), face);

		-- clean up for next text
		reset_text (pac_text);
	end insert_silk_text;


	
	
	
	procedure insert_silk_placeholder (
		packge			: in type_package_model_access;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is 
		use et_device_placeholders.packages;
	begin
		add_placeholder (packge.silkscreen, pac_text_placeholder, face);

		-- clean up for next placeholder
		reset_placeholder (pac_text_placeholder);
	end insert_silk_placeholder;
	
	
	
	
end et_package_read_silkscreen;
