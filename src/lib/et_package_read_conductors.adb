------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         PACKAGE READ / CONDUCTORS                        --
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

with et_mirroring;
with et_primitive_objects;				use et_primitive_objects;
with et_coordinates_formatting;			use et_coordinates_formatting;
with et_keywords;						use et_keywords;
with et_package_model;					use et_package_model;
with et_directions;						use et_directions;
with et_conductor_segment;				use et_conductor_segment;
with et_conductors_floating_package;	use et_conductors_floating_package;

with et_board_text;
with et_conductor_text;
with et_package_read_text;
with et_general_rw;						use et_general_rw;


package body et_package_read_conductors is

	use pac_geometry_2;
	
	conductor_line : type_conductor_line;
	conductor_arc : type_conductor_arc;
	conductor_circle : type_conductor_circle;
	





	procedure read_conductor_line (
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
			set_A (conductor_line, p);

			
		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			p := to_vector_model (line, 2);
			set_B (conductor_line, p);

		
		elsif kw = keyword_width then -- width 0.5
			expect_field_count (line, 2);
			conductor_line.width := to_distance (f (line, 2));
												
			
		else
			invalid_keyword (kw);
		end if;
	end read_conductor_line;


	

	
	
	
	
	procedure read_conductor_arc (
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
			set_A (conductor_arc, p);

			
		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			p := to_vector_model (line, 2);
			set_B (conductor_arc, p);
		
			
		elsif kw = keyword_center then -- center x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			set_center (conductor_arc, to_vector_model (line, 2));

			
		elsif kw = keyword_direction then -- direction ccw
			expect_field_count (line, 2);

			set_direction (conductor_arc, to_direction (f (line, 2)));


		elsif kw = keyword_width then -- width 0.5
			expect_field_count (line, 2);
			conductor_arc.width := to_distance (f (line, 2));

			
		else
			invalid_keyword (kw);
		end if;
	end read_conductor_arc;

	

	

	
	
	
	procedure read_conductor_circle (
		line	: in type_fields_of_line)
	is
		kw : string := f (line, 1);
	begin
		if kw = keyword_center then -- center x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			set_center (conductor_circle, to_vector_model (line, 2));

			
		elsif kw = keyword_radius then
			expect_field_count (line, 2);

			set_radius (conductor_circle, to_radius (f (line, 2)));


		elsif kw = keyword_width then -- width 0.5
			expect_field_count (line, 2);
			conductor_circle.width := to_distance (f (line, 2));

			
		else
			invalid_keyword (kw);
		end if;
	end read_conductor_circle;






	procedure insert_conductor_line (
		packge			: in type_package_model_access;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is 
		use pac_conductor_lines;
	begin
		case face is
			when TOP => 
				append (packge.conductors.top.lines, conductor_line);

			when BOTTOM => 
				append (packge.conductors.bottom.lines, conductor_line);
		end case;
		-- CS use procedure add_line
		
		-- clean up for next line
		reset_line (conductor_line);		
	end insert_conductor_line;
	

	
	
	
	procedure insert_conductor_arc (
		packge			: in type_package_model_access;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is 
		use pac_conductor_arcs;
	begin
		-- CS check arc
		
		case face is
			when TOP => 
				append (packge.conductors.top.arcs, conductor_arc);

			when BOTTOM => 
				append (packge.conductors.bottom.arcs, conductor_arc);
		end case;
		-- CS use procedure add_arc
		
		-- clean up for next arc
		reset_arc (conductor_arc);		
	end insert_conductor_arc;

	
	

	
	
	procedure insert_conductor_circle (
		packge			: in type_package_model_access;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is 
		use pac_conductor_circles;
	begin
		case face is
			when TOP => 
				append (packge.conductors.top.circles, conductor_circle);

			when BOTTOM => 
				append (packge.conductors.bottom.circles, conductor_circle);
		end case;
		-- CS use procedure add_circle
		
		-- clean up for next circle
		reset_circle (conductor_circle);		
	end insert_conductor_circle;





	procedure insert_conductor_text (
		packge			: in type_package_model_access;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is 
		use et_mirroring;
		use et_board_text;
		use pac_text_board_vectorized;
		use et_package_read_text;

		vectors : type_vector_text;
	begin
		vectors := vectorize_text (
			content			=> pac_text.content,
			size			=> pac_text.size,
			rotation		=> pac_text.position.rotation,
			position		=> pac_text.position.place,
			mirror			=> MIRROR_ALONG_Y_AXIS,
			line_width		=> pac_text.line_width,
			alignment		=> pac_text.alignment,
			make_border		=> true,
			log_threshold	=> log_threshold + 1);
			
		add_text (packge.conductors, (pac_text with vectors), face);
		
		-- clean up for next text
		reset_text (pac_text);		
	end;

	
	
	
end et_package_read_conductors;
