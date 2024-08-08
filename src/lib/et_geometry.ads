------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              GEOMETRY                                    --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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

with ada.text_io;				use ada.text_io;

with glib;

with et_general;				use et_general;
with et_string_processing;		use et_string_processing;

package et_geometry is
	
	keyword_position	: constant string := "position";
	keyword_x 			: constant string := "x";
	keyword_y 			: constant string := "y";		
	keyword_rotation 	: constant string := "rotation";
	keyword_direction	: constant string := "direction";
	keyword_start		: constant string := "start";
	keyword_end			: constant string := "end";
	keyword_center		: constant string := "center";		
	keyword_radius		: constant string := "radius";		
	keyword_diameter	: constant string := "diameter";
	keyword_filled 		: constant string := "filled";

	
	type type_axis is (X, Y, Z);
	subtype type_axis_2d is type_axis range X .. Y;

	function to_string (axis : in type_axis) return string;
	function to_axis (axis : in string) return type_axis;


	axis_separator : constant string := "/";
	point_preamble : constant string := " (x" & axis_separator & "y) ";
	--point_preamble_with_rotation : constant string := 
		--" (x"
		--& axis_separator
		--& "y"
		--& axis_separator
		--& "rotation)";

	

	type type_direction_of_rotation is (
		CW,		-- clockwise
		CCW);	-- counterclockwise

	function to_string (direction : in type_direction_of_rotation) return string;
	function to_direction (direction : in string) return type_direction_of_rotation;

	-- Changes CCW to CW and vice versa.
	function reverse_direction (direction : in type_direction_of_rotation)
		return type_direction_of_rotation;

	


	
	type type_grid_notches is new positive;

	function to_notches (notches : in string) return type_grid_notches;
	function to_string (notches : in type_grid_notches) return string;



	type type_shape is (LINE, ARC, CIRCLE);

	function to_shape (shape : in string) return type_shape;
	function to_string (shape : in type_shape) return string;
	

	
	type type_coordinates is (RELATIVE, ABSOLUTE);

	function to_string (coordinates : in type_coordinates) return string;
	function to_coordinates (coordinates : in string) return type_coordinates;

	
	type type_filled is (NO, YES);
	function to_string (filled : in type_filled) return string;
	function to_filled (filled : in string) return type_filled;
	filled_default : constant type_filled := NO;


	-- FILL STYLE OF OBJECTS WITH A CLOSED CIRCUMFENCE		
	keyword_fill_style : constant string := "fill_style";	
	type type_fill_style is (SOLID, HATCHED);
	fill_style_default : constant type_fill_style := SOLID;
	
	function to_string (fill_style : in type_fill_style) return string;
	function to_fill_style (fill_style : in string) return type_fill_style;

	

	-- scale (relevant for GUI only):
	scale_min : constant glib.gdouble := 0.2;
	scale_max : constant glib.gdouble := 2000.0;
	subtype type_scale is glib.gdouble range scale_min .. scale_max;
	scale_default : constant type_scale := 1.0;
	scale_factor_on_zoom : constant type_scale := 1.05;



-- DISTANCE POINT TO LINE
	
	type type_line_range is (
		BETWEEN_END_POINTS,	-- start and end point excluded
		WITH_END_POINTS,	-- start and end point included
		BEYOND_END_POINTS	-- indefinite long line assumed. extends beyond both start and end point into infinity
		);


	type type_mirror_style is (
		NO_MIRROR,
		MIRROR_X,  -- mirror along X-axis
		MIRROR_Y); -- mirror along Y-axis

	mirror_style_default : constant type_mirror_style := NO_MIRROR;



	
	
	-- Whether a line, arc, circle or contour is drawn dashed or not:
	type type_line_style is (CONTINUOUS, DASHED);
	-- CS other pattersn like jotted, dash-point, ... ?

	
		
end et_geometry;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
