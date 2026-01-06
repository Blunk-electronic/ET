------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         PACKAGE READ / ROUTE RESTRICT                    --
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

with et_primitive_objects;				use et_primitive_objects;
with et_coordinates_formatting;			use et_coordinates_formatting;
with et_keywords;						use et_keywords;
with et_package_model;					use et_package_model;
with et_directions;						use et_directions;

with et_route_restrict;					use et_route_restrict;
with et_route_restrict.packages;		use et_route_restrict.packages;

with et_general_rw;						use et_general_rw;
with et_package_read_contour;			use et_package_read_contour;


package body et_package_read_route_restrict is

	use pac_geometry_2;
	
	route_restrict_line : type_route_restrict_line;
	route_restrict_arc : type_route_restrict_arc;
	route_restrict_circle : type_route_restrict_circle;
	





	procedure read_route_restrict_line (
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
			set_A (route_restrict_line, p);

			
		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			p := to_vector_model (line, 2);
			set_B (route_restrict_line, p);
					
			
		else
			invalid_keyword (kw);
		end if;
	end read_route_restrict_line;


	

	
	
	
	
	procedure read_route_restrict_arc (
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
			set_A (route_restrict_arc, p);

			
		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			p := to_vector_model (line, 2);
			set_B (route_restrict_arc, p);
		
			
		elsif kw = keyword_center then -- center x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			set_center (route_restrict_arc, to_vector_model (line, 2));

			
		elsif kw = keyword_direction then -- direction ccw
			expect_field_count (line, 2);

			set_direction (route_restrict_arc, to_direction (f (line, 2)));
			
		else
			invalid_keyword (kw);
		end if;
	end read_route_restrict_arc;

	

	

	
	
	
	procedure read_route_restrict_circle (
		line	: in type_fields_of_line)
	is
		kw : string := f (line, 1);
	begin
		if kw = keyword_center then -- center x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			set_center (route_restrict_circle, to_vector_model (line, 2));

			
		elsif kw = keyword_radius then
			expect_field_count (line, 2);

			set_radius (route_restrict_circle, to_radius (f (line, 2)));

			
		else
			invalid_keyword (kw);
		end if;
	end read_route_restrict_circle;






	procedure insert_route_restrict_line (
		packge			: in type_package_model_access;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is 
		use pac_route_restrict_lines;
	begin
		case face is
			when TOP => 
				append (packge.route_restrict.top.lines, route_restrict_line);

			when BOTTOM => 
				append (packge.route_restrict.bottom.lines, route_restrict_line);
		end case;
		-- CS use procedure add_line
		
		
		-- clean up for next line
		reset_line (route_restrict_line);		
	end insert_route_restrict_line;
	

	
	
	
	procedure insert_route_restrict_arc (
		packge			: in type_package_model_access;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is 
		use pac_route_restrict_arcs;
	begin
		-- CS check arc
		
		case face is
			when TOP => 
				append (packge.route_restrict.top.arcs, route_restrict_arc);

			when BOTTOM => 
				append (packge.route_restrict.bottom.arcs, route_restrict_arc);
		end case;
		-- CS use procedure add_arc
		
		-- clean up for next arc
		reset_arc (route_restrict_arc);		
	end insert_route_restrict_arc;

	
	

	
	
	procedure insert_route_restrict_circle (
		packge			: in type_package_model_access;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is 
		use pac_route_restrict_circles;
	begin
		case face is
			when TOP => 
				append (packge.route_restrict.top.circles, route_restrict_circle);

			when BOTTOM => 
				append (packge.route_restrict.bottom.circles, route_restrict_circle);
		end case;
		-- CS use procedure add_circle

		-- clean up for next circle
		reset_circle (route_restrict_circle);		
	end insert_route_restrict_circle;





	procedure insert_route_restrict_zone (
		packge			: in type_package_model_access;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is 
		use pac_contours;
	begin
		add_zone (packge.route_restrict, (contour with null record), face);
		
		-- clean up for next contour
		reset_contour (contour);
	end;




	procedure insert_route_restrict_zone_cutout (
		packge			: in type_package_model_access;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is 
		use pac_contours;
	begin
		add_cutout (packge.route_restrict, (contour with null record), face);
		
		-- clean up for next contour
		reset_contour (contour);
	end;

	
	
end et_package_read_route_restrict;
