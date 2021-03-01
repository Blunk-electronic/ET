------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              GEOMETRY                                    --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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
with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_sets;
with ada.numerics;

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


	
	-- While drawing and editing we need information about the tool being used.
	-- This is relevant for GUI operations only:
	type type_tool is (MOUSE, KEYBOARD);

	function to_string (tool : in type_tool) return string;

	function to_tool (tool : in string) return type_tool;



	
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

	

	-- scale (relevant for GUI only):
	scale_min : constant glib.gdouble := 0.2;
	scale_max : constant glib.gdouble := 1000.0;
	subtype type_scale is glib.gdouble range scale_min .. scale_max;
	scale_default : constant type_scale := 1.0;
	scale_factor_on_zoom : constant type_scale := 1.05;



	
	-- LINE BENDING -------

	type type_bend_style is (
		STRAIGTH_THEN_ANGLED,
		DIRECT,
		ANGLED_THEN_STRAIGHT,
		VERTICAL_THEN_HORIZONTAL,
		HORIZONTAL_THEN_VERTICAL
		);

	type type_bended is (NO, YES);


	
	-------------------------
	
	generic
		type type_distance is delta <>;
		type type_rotation is delta <>;
		
	package generic_pac_geometry is
		
		zero 		: constant type_distance := 0.0;
		far_right	: constant type_distance := type_distance'last;
		far_left	: constant type_distance := type_distance'first;
		
		subtype type_distance_positive is type_distance range zero .. type_distance'last;
		subtype type_catch_zone is type_distance_positive range zero .. type_distance_positive'last/1000;

		grid_max : constant type_distance_positive := type_distance_positive'last/1000;
		subtype type_distance_grid is type_distance_positive range zero .. grid_max;
		grid_default : constant type_distance_grid := 2.5;
		
		type type_grid is record
			x,y	: type_distance_grid := grid_default;
		end record;

		procedure scale_grid (
			grid	: in out type_grid;
			scale	: in type_distance_positive);			
		
		function to_string (grid : in type_grid) return string;
		
		type type_point is tagged private;

		origin			: constant type_point;		
		far_upper_right	: constant type_point;
		far_lower_left	: constant type_point;
		
		function to_string (point : in type_point) return string;
		
		function to_distance (distance : in string) return type_distance;		
		function to_string (distance : in type_distance) return string;
		
		function x (point : in type_point'class) return type_distance; -- CS class attr. not required ?
		function y (point : in type_point'class) return type_distance;		

		-- Returns the rotation of the given point around the origin.
		-- If for example point is (1/1) then the return is 45 degree.
		-- if point is (-1/-1) then the return is -135 degree.
		function rotation (point : in type_point) return type_rotation;

		
		-- The GUI frequently requires the area (a rectanglular box around an object)
		-- occupied by the object. For preparation we need the type_boundaries.
		-- Boundaries are to be set in the drawing plane where the y-axis increases upwards.
		-- The boundaries are always relative to a certain origin that
		-- sits somewhere inside the rectangle. 
		type type_boundaries is record
			smallest_x, smallest_y : type_distance := type_distance'last;
			greatest_x, greatest_y : type_distance := type_distance'first;
			distance_of_topleft_to_default : type_point := origin;
		end record;

		boundaries_default : constant type_boundaries;
		
		function to_string (boundaries : in type_boundaries) return string;
		
		-- Adds two boundaries.
		procedure add (
			boundaries_one : in out type_boundaries;
			boundaries_two : in type_boundaries);
		
		-- Calculates the boundaries of the given points:
		function boundaries (point_one, point_two : in type_point) return type_boundaries;

		-- Moves the boundaries by the given offset:
		procedure move_by (
			boundaries	: in out type_boundaries;
			offset		: in type_point);

		-- Rotates the given boundaries by given rotation.
		procedure rotate (
			boundaries	: in out type_boundaries;
			rotation	: in type_rotation);
		
		-- In the GUI, in connection with boundaries and bounding boxes a type for
		-- a rectangular area of the drawing is required.
		-- NOTE: The bounding box is something required in the model plane only.
		type type_rectangle is record
			x, y			: type_distance; -- position, upper left corner
			width, height	: type_distance_positive; -- size
		end record;

		no_rectangle : constant type_rectangle := (others => zero);

		function to_string (rectangle : in type_rectangle) return string;

		-- Moves the rectangle by the given offset:
		procedure move_by (
			rectangle	: in out type_rectangle;
			offset		: in type_point);
		
		-- Returns true if the given two rectangles intersect each other in some way:
		function intersects (rect1, rect2 : type_rectangle) return boolean;


		
		
		
		function mil_to_distance (mil : in string) return type_distance;
		-- Converts a mil number (given as a string) to millimeters.	

		function distance_to_mil (distance : in type_distance) return string;

		
		axis_separator : constant string := "/";
		point_preamble : constant string := " (x" & axis_separator & "y) ";
		point_preamble_with_rotation : constant string := 
			" (x"
			& axis_separator
			& "y"
			& axis_separator
			& "rotation)";

		function "<" (left, right : in type_point) return boolean;
		
		function set (x, y : in type_distance) return type_point'class;

		procedure set (
			axis 	: in type_axis_2d;
			value	: in type_distance;					 
			point	: in out type_point'class);

		procedure set (
			point	: in out type_point'class;
			position: in type_point);

		-- The quadrants of the coordinate system are numbered counter clockwise.
		-- Quadrant ONE is top right.
		type type_quadrant is (ONE, TWO, THREE, FOUR);

		function quadrant (point : in type_point) return type_quadrant;
		-- Returns the quadrant the point is located in.
		-- ONE  : point is right of the y-axis or on top of it AND above the x-axis or on top of it
		-- TWO  : point is left of the y-axis AND above the x-axis or on top of it
		-- THREE: point is left of the y-axis AND below the x-axis
		-- FOUR : point is right of the y-axis or on top of it AND below the x-axis
		
		function invert (point : in type_point'class) return type_point'class;
		-- Inverts the given point by multiplying x by -1 and y by -1.

		-- Inverts the point on the given axis.
		function invert (
			point	: in type_point;
			axis	: in type_axis_2d)
			return type_point'class;
		
		procedure reset (point : in out type_point'class);
		-- Moves the given point to the origin (0/0).

		procedure move_by (
		-- Moves a point by the given offset.
			point	: in out type_point'class;
			offset	: in type_point);

		procedure move_to (
		-- Moves a point to the given position.
			point		: in out type_point'class;
			position	: in type_point);
		
		function move (
		-- Moves a point into direction at distance.
			point		: in type_point;
			direction	: in type_rotation;
			distance	: in type_distance_positive)
			return type_point'class;
		
		procedure mirror (
		-- If axis is Y then it swaps right x with left x.
		-- If axis is X then it swaps upper y with lower y.
			point	: in out type_point;
			axis	: in type_axis_2d);	

		-- Returns the distance along the given axis between the given points.
		function distance (
			point_1	: in type_point;
			point_2	: in type_point;
			axis	: in type_axis_2d) 
			return type_distance;
		
		-- Returns the absolute distance along the given axis between the given points.
		-- NOTE: The result in both x and y is always greater or equal zero.
		function distance_abs (
			point_1	: in type_point;
			point_2	: in type_point;
			axis	: in type_axis_2d) 
			return type_distance_positive;

		function "+" (point_one, point_two : in type_point) return type_point'class;
		function "-" (point_one, point_two : in type_point) return type_point'class;
		
		function distance_relative (point_one, point_two : in type_point) return type_point'class;
		-- Returns the relative distance of point_two to point_one.	
		-- Subtracts point_one.x from point_two.y and point_one.y from point_two.y
		-- returns	d.x := point_two.x - point_one.x
		--			d.y := point_two.y - point_one.y;

		
		-- Computes the total distance between point_one and point_two.
		function distance_total (
			point_one, point_two : in type_point)
			return type_distance_positive;

		-- Returns true if point_2 is within the catch zone around point_1:
		function in_catch_zone (
			point_1		: in type_point; -- the reference point
			catch_zone	: in type_catch_zone; -- zone around reference point
			point_2 	: in type_point) -- the point being tested
			return boolean;
		
		function add (left, right : in type_rotation) return type_rotation;
		-- Adds two angles.
		-- If result greater 360 degree then 360 degree is subtracted from result.
		-- If result less than 360 degree then 360 degree is added to the result.


		
		type type_distance_polar is private;

		function distance_polar (point_one, point_two : in type_point) 
			return type_distance_polar;
		-- Returns the distance of point_two to point_one.	
		-- Subtracts point_one.x from point_two.x and point_one.y from point_two.y
		-- returns	total := sqrt ((point_two.x - point_one.x)**2 + (point_two.y - point_one.y)**2)
		--			angle := arctan ((point_two.y - point_one.y) / (point_two.x - point_one.x)
		-- NOTE: If the total distance between the points is zero then
		-- the returned angle is zero. So it is wise to test the two points
		-- for equality befor calling this function.
		
		function angle (distance : in type_distance_polar) 
			return type_rotation;
		
		function absolute (distance : in type_distance_polar) 
			return type_distance_positive;


		
		type type_position is new type_point with private;

-- 		function create (
-- 			point		: in type_point'class;
-- 			rotation	: in type_rotation) 
-- 			return type_position;
		
		function to_rotation (rotation : in string) return type_rotation;
		function to_string (rotation : in type_rotation) return string;

		
		type type_direction_of_rotation is (
			CW,		-- clockwise
			CCW);	-- counterclockwise

		function to_string (direction : in type_direction_of_rotation) return string;
		function to_direction (direction : in string) return type_direction_of_rotation;

		function direction_of_rotation (rotation : in type_rotation) return type_direction_of_rotation;
		-- Returns CW if rotation is negative. Returns CCW if rotation is positive or zero.

		-- Changes CCW to CW and vice versa.
		function reverse_direction (direction : in type_direction_of_rotation)
			return type_direction_of_rotation;
		
		origin_zero_rotation : constant type_position;

		-- A position at the greatest distance in
		-- x and y from the origin:
		far_upper_right_zero_rotation : constant type_position;
		
		units_per_cycle : constant float := 360.0;

		zero_rotation : constant type_rotation := 0.0;

		radians_max : constant float := - 2.0 * ada.numerics.pi;
		
		subtype type_radians is float range (- radians_max) .. radians_max;
		
		function to_radians (degrees : in type_rotation) return float;
		-- Converts degrees to radians.

		function to_degrees (radians : in float) return type_rotation;
		-- Converts radians to degrees.


		function to_position (
			point		: in type_point;
			rotation	: in type_rotation)
			return type_position'class;
		
		procedure set (
		-- Sets the rotation of a position. (position.rotation)
			position	: in out type_position;
			rotation	: in type_rotation);
		
		function rot (position : in type_position'class) return type_rotation;
		-- Returns the rotation of the given position.

		procedure rotate (
		-- Changes the rotation of the given position by the given offset.
		-- Preserves x/y. Changes position.rotation only.
			position	: in out type_position'class;
			offset		: in type_rotation);
		
		procedure rotate_by (
		-- Rotates the given point BY the given angle about the origin.
		-- Changes point.x and point.y only.
			point		: in out type_point'class;
			rotation	: in type_rotation);

		procedure rotate_to (
		-- Rotates the given point TO the given angle about the origin.
		-- Changes point.x and point.y only.
			point		: in out type_point'class;
			rotation	: in type_rotation);

-- 		procedure rotate_by (
-- 		-- Rotates the given point BY the given angle around the given center point.
-- 		-- Changes point.x and point.y only.
-- 			point		: in out type_point'class;
-- 			center		: in type_point;
-- 			rotation	: in type_rotation);
		
		-- Returns a distance rounded according to given grid.		
		function round (
			distance	: in type_distance;
			grid		: in type_distance_grid)
			return type_distance;
		
		-- Returns x/y of point rounded according to given grid.
		function round_to_string (
			point	: in type_point;
			grid	: in type_grid)
			return string;

		-- Returns point rounded according to given grid.		
		-- Use this function for operations like "snap to grid" etc...
		function round (
			point	: in type_point;
			grid	: in type_grid)
			return type_point'class;

		overriding function to_string (point : in type_position) return string;
			
	private
		-- CS need an abstract type_point_abstract ?
		
		type type_point is tagged record
			x, y : type_distance := zero;
		end record;

		type type_distance_polar is record
			absolute: type_distance_positive;
			angle	: type_rotation;
		end record;
		
		origin : constant type_point := (others => zero);
		
		far_upper_right : constant type_point :=
			(x => type_distance'last,
			 y => type_distance'last);

		far_lower_left : constant type_point :=
			(x => type_distance'first,
			 y => type_distance'first);

		
		type type_position is new type_point with record
			rotation : type_rotation := zero_rotation;
		end record;

		origin_zero_rotation : constant type_position := (others => <>);

		far_upper_right_zero_rotation : constant type_position :=
			((far_upper_right with zero_rotation));
		
		boundaries_default : constant type_boundaries := (others => <>);
		
	end generic_pac_geometry;




----- SHAPES ---------------------------------------------------
	

	generic
		with package pac_geometry is new generic_pac_geometry (<>);
		
	package generic_pac_shapes is
		use pac_geometry;

		-- Unites the point with the boundaries. boundaries is updated.
		procedure union (
			boundaries	: in out type_boundaries;
			point		: in type_point);
		
		-- Unites the boundaries "right" with boundaries "left". "left" is updated.
		procedure union (
			left	: in out type_boundaries;
			right	: in type_boundaries);

		
		
		-- A ray has a fixex starting point, a direction and
		-- no end point:
		type type_ray is record
			start_point	: type_point;
			direction	: type_rotation;
		end record;


		
	-- VECTOR OPERATIONS
		
		type type_vector is private;

		null_vector : constant type_vector;

		unity_vector : constant type_vector;
		
		function to_vector (
			point	: in type_point)
			return type_vector;

		function to_point (
			v	: in type_vector)
			return type_point;
		
		function absolute (
			vector	: in type_vector)
			return type_distance_positive;

		function scale (
			v	: in type_vector;
			s	: in float)
			return type_vector;
		
		function add (
			a, b	: in type_vector)
			return type_vector;

		-- Subtracts the components of b from those of a.
		-- return.x = a.x - b.x, ...
		function subtract (
			a, b	: in type_vector)
			return type_vector;
		
		function cross_product ( -- german: Vektorprodukt
			a, b	: in type_vector)
			return type_vector;
		
		function dot_product ( -- german: Skalarprodukt
			a, b	: in type_vector)
			return type_distance;

		function mixed_product ( -- german: Spatprodukt
			a, b, c	: in type_vector)
			return type_distance;
		-- NOTE: Also called scalar triple product or box product.

								   
		-- Divides the components x,y,z of vector a
		-- by one of the components of vector b.
		-- At least one of the components of b 
		-- must not be zero.
		function divide (
			a, b	: in type_vector)
			return type_distance;



		
		-- Returns the location vector of the start point of a ray:
		function start_vector (ray : in type_ray) 
			return type_vector;

		-- Returns the direction vector of a ray:
		function direction_vector (ray : in type_ray) 
			return type_vector;


		type type_line_vector is record
			v_start		: type_vector; -- start vector of line
			v_direction	: type_vector; -- direction vector of line
		end record;


		-- Converts a ray (consisting of start point and a direction)
		-- to a line vector consisting of start vector and
		-- direction vector:
		function to_line_vector (
			ray : in type_ray)
			return type_line_vector;

		
		type type_intersection_status_of_two_lines is (
			NOT_EXISTENT,
			EXISTS,
			OVERLAP);


		-- When finding intersections of two lines this type is required:
		type type_intersection_of_two_lines (status : type_intersection_status_of_two_lines) is record
			case status is
				when EXISTS	=> intersection : type_vector; -- location vector
				when NOT_EXISTENT | OVERLAP => null;
			end case;
		end record;


		-- Tests whether the given two lines intersect.
		-- If there is an intersection, returns the location vector.
		function get_intersection (
			line_1, line_2	: in type_line_vector)
			return type_intersection_of_two_lines;

		function get_angle_of_itersection (
			line_1, line_2	: in type_line_vector)
			return type_rotation;
		
		
		
	-- LINE
		type type_line is abstract tagged record
			start_point 	: type_point;
			end_point   	: type_point;
			-- CS locked : type_locked;
		end record;

		-- Returns the point on the given line
		-- that is between its start and end point:
		function get_center (
			line	: in type_line)
			return type_point;


		-- Tests whether the given probe_line intersects the given 
		-- candidate line.
		-- If there is an intersection between start and end point
		-- of the candidate line (start and end point included),
		-- then returns the location vector of
		-- the intersection.
		-- If the intersection is before start point or
		-- beyond end point of the given line, return NOT_EXISTENT.
		function get_intersection (
			probe_line		: in type_line_vector;
			candidate_line	: in type_line)
			return type_intersection_of_two_lines;


		
		-- Returns the location vector of the start point of a line:
		function start_vector (
			line	: in type_line)
			return type_vector;

		-- Returns the location vector of the end point of a line:
		function end_vector (
			line	: in type_line)
			return type_vector;

		-- Returns the direction vector of a line:
		function direction_vector (
			line	: in type_line)
			return type_vector;

		-- Converts a line (consisting of start and end point)
		-- to a line vector consisting of start vector and
		-- direction vector:
		function to_line_vector (
			line	: in type_line)
			return type_line_vector;
			
		-- Computes the distance of the point from the line.
		-- This computation does not care about end or start point of the line.
		-- It assumes an indefinite long line without start or end point.
		function distance (
			line	: in type_line;
			point	: in type_point)
			return type_distance_positive;
		
		-- Returns the direction in degrees of a line.
		-- Example: If a line runs from 0/0 to 1/1 then the result is 45 degree.
		-- Example: If a line runs from -1/-1 to -4/-4 then the result is 225 degree.
		function direction (
			line	: in type_line)
			return type_rotation;

		-- Moves a line in the given direction by the given distance:
		procedure move_by (
			line		: in out type_line;
			direction	: in type_rotation;
			distance	: in type_distance_positive);
							  
		procedure move_by (
		-- Moves a line by the given offset. 
			line	: in out type_line;
			offset	: in type_point);

		-- Mirrors a line along the given axis.
		procedure mirror (
			line		: in out type_line;
			axis		: in type_axis_2d);
		
		-- Rotates a line about the origin by the given rotation.
		procedure rotate_by (
			line		: in out type_line;
			rotation	: in type_rotation);


		-- When creating a route from one point to another use this type.
		-- NOTE: This is general stuff. This does apply to all kinds of lines
		-- from one point to another (nets, documentation, tracks, ...) !
		-- If no bend, then we have just a start and an end point which 
		--  will result in a direct line between the two points.
		-- If bended, then we get an extra point where the bending takes place
		--  which will result in two lines that connect the two points:
		type type_route (bended : type_bended) is record
			start_point, end_point : type_point;
			case bended is
				when NO		=> null; -- no bend
				when YES	=> bend_point : type_point;
			end case;
		end record;

		-- Computes a route between two points according to the given bend style:
		function to_route (
			start_point, end_point	: in type_point;
			style					: in type_bend_style)
			return type_route;

		-- When a route is being drawn from one point to another
		-- then we speak about a route from start point to end point
		-- and optionally a bending point where the route changes
		-- direction.
		-- This type is required for all kinds of lines (nets, documentation, tracks, ...)
		-- when being drawn via the GUI.
		-- The route being drawn must provide information about the tool it is
		-- being drawn with (mouse, touchpad, keyboard).
		type type_route_live is record
			being_drawn	: boolean := false;

			start_point	: type_point;
			end_point	: type_point;

			bended		: type_bended := NO;
			bend_point	: type_point;
			bend_style	: type_bend_style := HORIZONTAL_THEN_VERTICAL;
			
			tool		: type_tool := MOUSE;
		end record;

		-- Switches to the next bend style of the given live route:
		procedure next_bend_style (route : in out type_route_live);
		
		function boundaries (line : in type_line) return type_boundaries;
		-- Returns the boundaries of the given line.
		
		-- A line is divided into three zones. Their width is the ratio
		-- of line length and the zone_division_factor.
		-- 
		--    S---|---center---|---E
		--
		-- The position of the bar (|) in this drawing depends on the zone_division_factor.
		-- The center length is twice the length of start/end point.
		type type_line_zone is (START_POINT, END_POINT, CENTER);
		line_zone_division_factor : constant positive := 4;
		
		-- Calculates the zone of the line where point is nearest.
		-- Point is not required to sit exactly on the line.
		function which_zone (
			point	: in type_point'class;
			line	: in type_line'class) 
			return type_line_zone;

		type type_distance_point_line is private;

		-- These function return the status of the components of the given type_distance_point_line:
		function out_of_range (d : in type_distance_point_line) return boolean;
		function distance (d : in type_distance_point_line) return type_distance;
		function on_start_point (d : in type_distance_point_line) return boolean;
		function on_end_point (d : in type_distance_point_line) return boolean;
		
		type type_line_range is (
			BETWEEN_END_POINTS,	-- start and end point excluded
			WITH_END_POINTS,	-- start and end point included
			BEYOND_END_POINTS	-- unlimited line assumed. extends beyond both start and end point into infinity
			);
		
		-- Computes the shortest distance (perpendicular) of a given point from the given line. 
		-- The optional parameter catch_zone specifies the range at which the point is regarded
		-- as stitting on the line.
		-- In the result the flag out_of_range will be cleared if the point sits on the line
		-- or within the catch_zone.
		function distance_point_line (
			point		: in type_point; 
			line		: in type_line;
			line_range	: in type_line_range;
			catch_zone	: in type_catch_zone := zero)
			return type_distance_point_line;

		-- Returns true if the given point sits on the given line.
		-- The optional parameter accuracy may be used to specifiy the range at
		-- which the point is regarded as sitting on the line.
		function on_line (
			point		: in type_point;
			line		: in type_line;
			catch_zone	: in type_catch_zone := zero)
			return boolean; 
		
	-- ARC
		type type_arc is abstract tagged record
			center			: type_point;
			start_point		: type_point;
			end_point		: type_point;
			direction		: type_direction_of_rotation := CW;
			-- CS locked : type_locked;		
		end record;

		-- Returns the distance between the start point and the center of the arc.
		function radius_start (arc : in type_arc) return type_distance_positive;

		-- Returns the distance between the end point and the center of the arc.
		function radius_end (arc : in type_arc) return type_distance_positive;

		-- Returns true if start and end point of given arc have same distance to center.
		function is_valid (arc : in type_arc) return boolean;
		
		-- Sometimes (for example with cairo) an arc must be
		-- expressed in terms of start and end angle:
		type type_arc_angles is record
			center		: type_point;
			radius		: type_distance_positive;
			angle_start	: type_rotation;
			angle_end	: type_rotation;
			direction	: type_direction_of_rotation := CW;
		end record;

		function to_arc_angles (arc : in type_arc) return type_arc_angles;
		-- Returns the start and end angles of an arc.
		
		function boundaries (arc : in type_arc) return type_boundaries;
		-- Returns the boundaries of the given arc.
		
		-- Returns true if the given point sits on the given arc.
		-- The optional parameter accuracy may be used to specifiy the range at
		-- which the point is regarded as sitting on the arc.
		function on_arc (
			point		: in type_point;
			arc			: in type_arc;
			accuracy	: in type_catch_zone := zero) -- CS currently ignored
			return boolean; 


		type type_intersection_status_of_line_and_circle is (
			NONE_EXIST, -- no intersection at all
			ONE_EXISTS, -- tangent
			TWO_EXIST); -- two intersections

		-- When finding intersections of a line with a circle we use this type:

		type type_tangent_status is (TANGENT, SECANT);
		
		type type_intersection_of_line_and_circle (status : type_intersection_status_of_line_and_circle) is record
			case status is
				when NONE_EXIST => null;
				
				when ONE_EXISTS	=> 
					intersection	: type_vector; -- location vector
					tangent_status	: type_tangent_status;
				
				when TWO_EXIST	=> 
					intersection_1	: type_vector; -- location vector
					intersection_2	: type_vector; -- location vector
					
			end case;
		end record;


		-- Computes the intersections of a line with an arc:
		function get_intersection (
			line	: in type_line_vector;
			arc		: in type_arc)
			return type_intersection_of_line_and_circle;

		
		
		function arc_end_point (
		-- Computes the end point of an arc.
			center		: in type_point;
			start_point	: in type_point;	
			angle 		: in type_rotation) -- unit is degrees
			return type_point'class;

		procedure move_by (
		-- Moves an arc by the given offset. 
			arc		: in out type_arc;
			offset	: in type_point);

		procedure move_to (
		-- Moves an arc to the given position. 
			arc			: in out type_arc;
			position	: in type_point);

		procedure mirror (
		-- Mirrors an arc along the given axis.
			arc			: in out type_arc;
			axis		: in type_axis_2d);

		-- Rotates an arc about the origin by the given rotation.
		procedure rotate_by (
			arc			: in out type_arc;
			rotation	: in type_rotation);

		
	-- CIRCLE
		type type_circle is abstract tagged record -- CS rename to type_circle_base
			center			: type_point;
			radius  		: type_distance_positive := zero;
			-- CS locked : type_locked;
		end record;

		procedure move_by (
		-- Moves a circle by the given offset. 
			circle	: in out type_circle;
			offset	: in type_point);
		
		procedure mirror (
		-- Mirrors the center of a circle along the given axis.
			circle		: in out type_circle;
			axis		: in type_axis_2d);
		
		-- Rotates the center of a circle about the origin by the given rotation.
		procedure rotate_by (
			circle		: in out type_circle;
			rotation	: in type_rotation);
		
		function boundaries (circle : in type_circle) return type_boundaries;
		-- Returns the boundaries of the given circle.
		
		function on_circle (
		-- Returns true if the given point sits on the given circle circumfence.
		-- The optional parameter accuracy may be used to specifiy the range at
		-- which the point is regarded as sitting on the circle.
			point		: in type_point;
			circle		: in type_circle;
			accuracy	: in type_catch_zone := zero)
			return boolean;
		


		
		-- Computes the intersections of a line with a circle:
		function get_intersection (
			line	: in type_line_vector;
			circle	: in type_circle)
			return type_intersection_of_line_and_circle;

		

		
		function to_string (line : in type_line) return string;
		-- Returns the start and end point of the given line as string.

		function to_string (arc : in type_arc) return string;
		-- Returns the start, end point and angle of the given arc as string.
		
		function to_string (circle : in type_circle) return string;
		-- Returns the center and radius of the given circle as string.



		

	-- POLYGON
		type type_polygon_base is abstract tagged private;

		-- In contrast to the common definition of a polygon, a polygon
		-- in this world is described as a finite number of elements 
		-- like lines, arcs and even circles that form a polygonal circuit.
		
		-- Every segment has an id.
		-- CS: For the moment we limit the number of segments (that form the outline)
		-- to a reasonable value. Increase if necessary:
		type type_polygon_segment_count is range 0 .. 100;
		
		subtype type_polygon_segment_id is type_polygon_segment_count 
			range 1 .. type_polygon_segment_count'last;
		
		type type_polygon_line is new type_line with record
			id : type_polygon_segment_id := type_polygon_segment_id'first;
		end record;

		procedure append_segment_line (
			polygon	: in out type_polygon_base'class;
			segment	: in type_polygon_line);
		
		package pac_polygon_lines is new doubly_linked_lists (type_polygon_line);

		
		type type_polygon_arc is new type_arc with record
			id : type_polygon_segment_id := type_polygon_segment_id'first;
		end record;
		
		procedure append_segment_arc (
			polygon	: in out type_polygon_base'class;
			segment	: in type_polygon_arc);

		package pac_polygon_arcs is new doubly_linked_lists (type_polygon_arc);

		
		type type_polygon_circle is new type_circle with record
			id : type_polygon_segment_id := type_polygon_segment_id'first;
		end record;

		procedure append_segment_circle (
			polygon	: in out type_polygon_base'class;
			segment	: in type_polygon_circle);
		
		package pac_polygon_circles is new doubly_linked_lists (type_polygon_circle);

		type type_polygon_segments is record
			lines	: pac_polygon_lines.list;
			arcs	: pac_polygon_arcs.list;
			circles	: pac_polygon_circles.list;
		end record;

		-- Loads the given lines into given polygon.
		-- NOTE: Overwrites already existing segments in the polygon.
		procedure load_lines (
			polygon		: in out type_polygon_base'class;
			lines		: in pac_polygon_lines.list);

		-- Loads the given circles into given polygon.
		-- NOTE: Overwrites already existing segments in the polygon.
		procedure load_circles (
			polygon		: in out type_polygon_base'class;
			circles		: in pac_polygon_circles.list);

		
		-- Loads the given segments into given polygon.
		-- NOTE: Overwrites already existing segments in the polygon.
		procedure load_segments (
			polygon		: in out type_polygon_base'class;
			segments	: in type_polygon_segments);
		
		procedure delete_segments (polygon : in out type_polygon_base);

		function get_segments (polygon : in type_polygon_base) 
			return type_polygon_segments;

		function get_segments_total (polygon : in type_polygon_base)
			return type_polygon_segment_count;

		-- Transposes a polygon in Y direction.
		-- Each point of each segment gets shifted by
		-- the formula new_y = offset - old_y:
		procedure transpose_polygon (
			polygon	: in out type_polygon_base'class;
			offset	: in type_distance);


		-- Reads the segments provided in a row of
		-- arguments in a form like 
		-- "line 0 0 100 0 /
		--  line 100 0 100 100 / 
		--  arc 50 100 100 100 0 100 ccw / 
		--  line 0 100 0 0"
		-- and builds a polygon:
		function to_polygon (
			arguments : in type_fields_of_line)
			return type_polygon_base'class;
		
		function boundaries (polygon : in type_polygon_base) return type_boundaries;
		-- Returns the boundaries of the given polygon.

		-- A polygon must have a properly closed outline.
		-- The outline check requires a list of points (where the gaps are):
		package pac_polygon_gaps is new doubly_linked_lists (type_point); 

		-- Returns the points where gaps of a polygon outline begin:
		function to_string (gaps : in pac_polygon_gaps.list) return string;
		

		-- The result of an outline check is a parameterized type:
		type type_polygon_status (closed : boolean) is record
			case closed is
				when TRUE	=> null;
				when FALSE	=> gaps : pac_polygon_gaps.list;
			end case;
		end record;
					
		-- Returns true if the given polygon is properly closed.
		-- If there are gaps, a list of points is returned where the gaps are.
		-- The test iterates the segments of the polygon and tests whether
		-- the end point of a segment matches the start point of the next segment.
		-- CS: Special threatment for circle segments: Since a circle does not have 
		-- start and end point, only the center point of the circle must be in 
		-- the chain of segments. 
		-- CS: Improvement required: It is sufficient if the circle
		-- touches one of the other segments (lines and arcs) to regard it as connected
		-- with the polygon.
		function is_closed (
			polygon	: in type_polygon_base)
			return type_polygon_status;

		procedure move_by (
		-- Moves a polygon by the given offset. 
			polygon	: in out type_polygon_base;
			offset	: in type_point);

		procedure mirror (
		-- Mirrors a polygon along the given axis.
			polygon	: in out type_polygon_base;
			axis	: in type_axis_2d);

		procedure rotate_by (
		-- Rotates a polygon about the origin by the given rotation.
			polygon		: in out type_polygon_base;
			rotation	: in type_rotation);


		-- POLYGON OFFSETTING:
		-- See:
		-- <https://gis.stackexchange.com/questions/61786/how-to-scale-reduce-my-polygon-without-changing-the-central-lat-long>
		-- <https://stackoverflow.com/questions/54033808/how-to-offset-polygon-edges>

		type type_polygon_scale is delta 0.1 range 0.1 .. 10.0; -- less than 1.0 -> downscaling, greater 1.0 -> upscaling
		for type_polygon_scale'small use 0.1;

		polygon_scale_default : constant type_polygon_scale := 1.0;

		function to_string (scale : in type_polygon_scale) return string;
		function to_scale (scale : in string) return type_polygon_scale;
		
		type type_offset_style is (
			BY_DISTANCE,
			BY_SCALE);

		type type_offset (style : type_offset_style) is record
			case style is
				when BY_DISTANCE	=> distance	: type_distance;
				when BY_SCALE		=> scale 	: type_polygon_scale;
			end case;
		end record;
		
		-- The procedure shrinks or expands the given polygon.
		procedure offset_polygon (
			polygon		: in out type_polygon_base;
			offset		: in type_offset);

		

		--type type_polygon is new type_polygon_base with private;
		-- not publicly visible. for internal use only.

		-- In order to get the status of a point relative to
		-- a polygon we need this stuff:
		-- The general approach is:
		-- A ray that starts at point and travels in zero degees 
		-- may intersect the polygon edges.
		-- The result of such a query is the type_inside_polygon_query_result
		-- that contains a status flag (inside/outside) and a list
		-- of x values where the ray intersects the polygon.
		-- This list provides the x values ordered according to their
		-- distance to the start point of the ray. Lowest value first.
		type type_probe_line is new type_line with null record;

		-- For collecting the x values of the intersections of a 
		-- probe line with the polygon edges:
		package pac_inside_polygon_query_x_values is new doubly_linked_lists (type_distance);
		
		type type_polygon_point_status is (
			OUTSIDE,	-- point is outside polygon area
			INSIDE);	-- point is in polygon area
		
		type type_inside_polygon_query_result is record
			status		: type_polygon_point_status := OUTSIDE;		
			x_values	: pac_inside_polygon_query_x_values.list;
		end record;

		-- Detects the position of a point relative to the polygon.
		function get_point_position (
			polygon		: in type_polygon_base;	
			point		: in type_point;
			catch_zone	: in type_catch_zone := zero)
			return type_inside_polygon_query_result;

		-- For finding the lower left corner of a polygon this type
		-- is required. The lower left corner can be a point somewhere
		-- on the edge of the polygon. In that case the point is REAL.
		-- If the point is outside the polygon then the point is VIRTUAL.
		type type_lower_left_corner_status is (
			REAL,	-- the corner point is somewhere on the outline
			VIRTUAL	-- the corner point is outside the polygon (where
					-- the lowest x and lowest y are)
			);

		-- When the lower left corner is to be found, then
		-- the result of such a search operation is formed by
		-- this type:
		type type_lower_left_corner is record
			point	: type_point := origin;
			status	: type_lower_left_corner_status := REAL;
		end record;

		-- Searches the lower left corner of a polygon:
		function get_lower_left_corner (polygon	: in type_polygon_base)
			return type_lower_left_corner;


		
		type type_distance_to_polygon (polygon_found : boolean) is record
			case polygon_found is
				when TRUE	=> distance : type_distance_positive;
				when FALSE	=> null;
			end case;
		end record;


		-- Returns the shortest distance of point to the given
		-- polygon in direction zero degrees. 
		-- The polygon is assumed to be on the
		-- right of the given point.
		function get_distance_to_polygon (
			polygon	: in type_polygon_base;
			point	: in type_point)
			return type_distance_to_polygon;
		
		
	private
		type type_vector is	record
			x, y, z : type_distance := zero;
		end record;

		null_vector		: constant type_vector := (others => zero);
		unity_vector	: constant type_vector := (others => 1.0);
		
		type type_distance_point_line is record
			distance		: type_distance_positive := zero;
			sits_on_start	: boolean := false;
			sits_on_end		: boolean := false;
			out_of_range	: boolean := true;
		end record;

		type type_polygon_base is abstract tagged record
			segments		: type_polygon_segments;
			segments_total	: type_polygon_segment_count := type_polygon_segment_count'first;
		end record;

		type type_polygon is new type_polygon_base with null record;
		
	end generic_pac_shapes;
	
	
end et_geometry;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
