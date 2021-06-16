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
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_sets;
with ada.numerics;

with ada.numerics.generic_elementary_functions;

with glib;

with et_general;				use et_general;
with et_string_processing;		use et_string_processing;

package et_geometry is

	package functions_float is new ada.numerics.generic_elementary_functions (float);
	use functions_float;

	-- Returns 1.0 if given x is greater or equal zero.
	-- Returns -1.0 if x less than zero.
	function sgn (x : float) return float;

	
	
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
	scale_max : constant glib.gdouble := 2000.0;
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


	
	-- CURVATURE ------------------
	type type_curvature is (STRAIGHT, CONVEX, CONCAVE);

	function to_string (curvature : in type_curvature) return string;
	
	subtype type_curvature_of_arc is type_curvature range CONVEX .. CONCAVE;

	
	
	-------------------------
	
	generic
		type type_distance is delta <>;
		axis_min, axis_max : type_distance;
		type type_rotation is delta <>;
		
	package generic_pac_geometry is
		
		zero 		: constant type_distance := 0.0;
		far_left	: constant type_distance := axis_min;
		far_right	: constant type_distance := axis_max;

		-- The position along an axis:
		subtype type_position_axis is type_distance 
			range axis_min .. axis_max;

		-- The distance between two objects:
		subtype type_distance_positive is type_distance 
			range zero .. type_distance'last;
		
		subtype type_catch_zone is type_distance_positive
			range zero .. type_distance_positive'last/1000;

		
		subtype type_rotation_positive is type_rotation
			range 0.0 .. type_rotation'last;

		
		
		
		-- Returns the greatest of the given distances. 
		-- If both are equal then "right" will be returned.
		function get_greatest (
			left, right : in type_distance)
			return type_distance;

		-- Returns the smallest of the given distances. 
		-- If both are equal then "right" will be returned.
		function get_smallest (
			left, right : in type_distance)
			return type_distance;


		
		-- Converts an angle like -90.0 degrees to 270 degrees.
		-- Converts an angle like -1.0 degrees to 359 degrees.
		-- Leaves a positive angle like 135 degree as it is and returns it.
		function to_positive_rotation (
			rotation	: in type_rotation)
			return type_rotation_positive;

		subtype type_tangent_angle is type_rotation range 0.0 .. 180.0 - type_rotation'small;
		
		subtype type_rotation_0_90 is type_rotation range 0.0 .. 90.0;
		
		
		-- For collecting distances:
		package pac_distances_positive is new doubly_linked_lists (type_distance_positive);
		package pac_distances is new doubly_linked_lists (type_distance);
		
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
		far_upper_left	: constant type_point;
		far_upper_right	: constant type_point;
		far_lower_left	: constant type_point;
		far_lower_right	: constant type_point;
		
		function to_string (point : in type_point) return string;

		
		type type_distance_relative is record
			x, y : type_distance := zero;
		end record;

		function to_point (d : in type_distance_relative)
			return type_point'class;

		function to_distance_relative (p : in type_point)
			return type_distance_relative;

		-- Inverts the given relative distance by 
		-- multiplying x by -1 and y by -1.
		function invert (d : in type_distance_relative) 
			return type_distance_relative;

		
		
		
		function to_distance (distance : in string) return type_distance;		
		function to_string (distance : in type_distance) return string;
		
		function get_x (point : in type_point'class) return type_position_axis;
		function get_y (point : in type_point'class) return type_position_axis;		

		
		-- Returns the rotation of the given point about the origin.
		-- If for example point is (1/1) then the return is 45 degree.
		-- if point is (-1/-1) then the return is -135 degree.
		function rotation (point : in type_point) return type_rotation; -- CS rename to get_rotation

		
		-- The area (a rectangular box around an object)
		-- occupied by the object.
		-- The boundaries are always relative to a certain origin that
		-- sits somewhere inside the rectangular box. 
		type type_boundaries is record
			smallest_x, smallest_y : type_position_axis := type_position_axis'last;
			greatest_x, greatest_y : type_position_axis := type_position_axis'first;
			distance_of_topleft_to_default : type_point := origin;
		end record;

		boundaries_default : constant type_boundaries;

		-- Returns the height of the given boundaries by
		-- calculating boundaries.greatest_y - boundaries.smallest_y:
		function get_height (boundaries : in type_boundaries)
			return type_distance_positive;

		-- Returns the width of the given boundaries by
		-- calculating boundaries.greatest_x - boundaries.smallest_x:
		function get_width (boundaries : in type_boundaries)
			return type_distance_positive;

		
		function to_string (boundaries : in type_boundaries) return string;

		-- Returns true if the given boundaries intersect each other:
		function intersect (
			boundaries_one : in type_boundaries;
			boundaries_two : in type_boundaries)
			return boolean;

		type type_boundaries_intersection (exists : boolean := true) is record
			case exists is
				when TRUE => intersection : type_boundaries;
				when FALSE => null;
			end case;
		end record;
		
		-- Returns the intersection area (german: Schnittmenge) of two
		-- boundaries. If the boundaries do not overlap each other
		-- then a constraint error is raised:
		function get_intersection (
			boundaries_one : in type_boundaries;
			boundaries_two : in type_boundaries)
			return type_boundaries_intersection;
		
		-- Adds two boundaries.
		procedure add (
			boundaries_one : in out type_boundaries;
			boundaries_two : in type_boundaries);
		
		-- Calculates the boundaries of the given points
		-- connected with a line that has the
		-- given width. The boundaries are extended
		-- by half the given width.
		function get_boundaries (
			point_one	: in type_point;
			point_two	: in type_point;
			width		: in type_distance_positive) 
			return type_boundaries;

		-- Moves the boundaries by the given offset:
		procedure move_by (
			boundaries	: in out type_boundaries;
			offset		: in type_distance_relative);

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
			offset		: in type_distance_relative);
		
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

		
		function set (
			x, y : in type_position_axis) 
			return type_point'class;

		procedure set (
			axis 	: in type_axis_2d;
			value	: in type_position_axis;					 
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

		-- Moves a point by the given offset.
		procedure move_by (
			point	: in out type_point'class;
			offset	: in type_distance_relative);

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


		
		
		-- Returns the relative distance of point_two to point_one.	
		-- Subtracts point_one.x from point_two.y and point_one.y from point_two.y
		-- returns	d.x := point_two.x - point_one.x
		--			d.y := point_two.y - point_one.y;
		function distance_relative ( -- CS rename to get_distance_relative
			point_one, point_two : in type_point) 
			return type_distance_relative;

		
		
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

		function to_polar (
			absolute	: in type_distance_positive;
			angle		: in type_rotation)
			return type_distance_polar;

		procedure set_absolute (
			distance : in out type_distance_polar;
			absolute : in type_distance_positive);

		procedure set_angle (
			distance : in out type_distance_polar;
			angle    : in type_rotation);

		
		-- Returns the distance of point_two to point_one.	
		-- Subtracts point_one.x from point_two.x and point_one.y from point_two.y
		-- returns	total := sqrt ((point_two.x - point_one.x)**2 + (point_two.y - point_one.y)**2)
		--			angle := arctan ((point_two.y - point_one.y) / (point_two.x - point_one.x)
		-- NOTE 1: The angle ranges from -180 to 180 degrees.
		-- NOTE 2: If the total distance between the points is zero then
		--         the returned angle is zero. So it is wise to test the two points
		--         for equality befor calling this function.
		function get_distance (
			point_one, point_two : in type_point)
			return type_distance_polar;

		-- Returns the angle of the given polar distance:
		function get_angle (
			distance : in type_distance_polar)
			return type_rotation;

		-- Returns the absolute of the given polar distance:
		function get_absolute (
			distance : in type_distance_polar)
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
			x, y : type_position_axis := zero;
		end record;

		type type_distance_polar is record
			absolute: type_distance_positive := zero;
			angle	: type_rotation := zero_rotation; -- ranges from -180 to 180 degrees
		end record;
		
		origin : constant type_point := (others => zero);

		far_upper_left : constant type_point :=
			(x => type_position_axis'first,
			 y => type_position_axis'last);
		
		far_upper_right : constant type_point :=
			(x => type_position_axis'last,
			 y => type_position_axis'last);

		far_lower_left : constant type_point :=
			(x => type_position_axis'first,
			 y => type_position_axis'first);
		
		far_lower_right : constant type_point :=
			(x => type_position_axis'last,
			 y => type_position_axis'first);

		
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


		type type_point_status is (
			OUTSIDE,	-- point is outside a certain area
			INSIDE);	-- point is inside a certain area

		function to_string (status : in type_point_status) return string;
		
		procedure toggle_status (status : in out type_point_status);

		
		-- Unites the point with the boundaries. boundaries is updated.
		procedure union (
			boundaries	: in out type_boundaries;
			point		: in type_point);
		
		-- Unites the boundaries "right" with boundaries "left". "left" is updated.
		procedure union (
			left	: in out type_boundaries;
			right	: in type_boundaries);

		
		
		-- A ray has a fixed starting point, a direction and
		-- no end point:
		type type_ray is record
			start_point	: type_point;
			direction	: type_rotation;
		end record;


		
	-- VECTOR OPERATIONS
		
		type type_vector is private;

		null_vector : constant type_vector;

		unity_vector : constant type_vector;

		function to_string (
			v	: in type_vector)
			return string;
		
		function get_x (
			v	: in type_vector)
			return type_distance;

		function get_y (
			v	: in type_vector)
			return type_distance;

		function get_z (
			v	: in type_vector)
			return type_distance;

		
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

		function to_string (
			lv : in type_line_vector)
			return string;
			
		-- Returns the direction of travel of the given line
		-- in degrees:
		function get_angle (
			line	: in type_line_vector)
			return type_rotation;
			
		-- Converts a ray (consisting of start point and a direction)
		-- to a line vector consisting of start vector and
		-- direction vector:
		function to_line_vector (
			ray : in type_ray)
			return type_line_vector;

		-- Returns a line vector perpendicular (german: normalvektor)
		-- to a line that crosses the given point at the given angle.
		-- The start vector of the result starts at the given point.
		-- CS not tested yet !
		function to_perpendicular_line_vector (
			point	: in type_vector;
			angle	: in type_rotation)
			return type_line_vector;

		
		
		type type_intersection_status_of_two_lines is (
			NOT_EXISTENT,
			EXISTS,
			OVERLAP);

		-- In general an intersection is composed of a point, where 
		-- the two objects meet, and and the angle at which they intersect:
		type type_intersection is record
			point	: type_vector; -- location vector -- CS should be a type_point
			angle	: type_rotation := zero_rotation;
		end record;
		
		function to_string (intersection : in type_intersection)
			return string;
		
		-- When finding intersections of two lines this type is required:
		type type_intersection_of_two_lines (
			status : type_intersection_status_of_two_lines) 
		is record
			case status is
				when EXISTS => intersection : type_intersection;
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
		type type_line_base is abstract tagged record
			start_point 	: type_point;
			end_point   	: type_point;
			-- CS locked : type_locked;
		end record;

		type type_line is new type_line_base with null record;
		-- CS use this type wherever a type_line is declared unnessecarily.
		
		-- Swaps start and end point of a line:
		function reverse_line (line : in type_line) return type_line'class;
		procedure reverse_line (line : in out type_line);
		
		-- Indicates whether a line is increasing in y-direction
		-- as it travels from left to right:
		type type_line_direction is (
			HORIZONTAL,					 
			RISING,
			VERTICAL,
			FALLING);

		function to_string (direction : in type_line_direction)
			return string;
		
		-- Returns the direction of a line:
		function get_direction (line : in type_line)
			return type_line_direction;

		-- Converts the angle of a tangent to a
		-- line direction.
		function get_tangent_direction (angle : in type_tangent_angle)
			return type_line_direction;

		
		
		-- If the start/end point of the candidate line is ABOVE-OR-ON the 
		-- threshold AND if the end/start point of the candidate line is BELOW the
		-- threshold then we consider the line to be threshold-crossing.
		function crosses_threshold (
			line		: in type_line;	
			y_threshold	: in type_distance)
			return boolean;

		
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
		-- NOTE: The angle of intersection is measured between the 
		-- start points of the two lines. It is always positive.
		function get_intersection (
			probe_line		: in type_line_vector;
			candidate_line	: in type_line)
			return type_intersection_of_two_lines;


		
		-- Returns the location vector of the start point of a line:
		function start_vector ( -- CS rename to get_start_vector
			line	: in type_line)
			return type_vector;

		-- Returns the location vector of the end point of a line:
		function end_vector ( -- CS rename to get_end_vector
			line	: in type_line)
			return type_vector;

		-- Returns the direction vector of a line:
		function direction_vector ( -- CS rename to get_direction_vector
			line	: in type_line)
			return type_vector;

		-- Converts a line (consisting of start and end point)
		-- to a line vector consisting of start vector and
		-- direction vector:
		function to_line_vector (
			line	: in type_line)
			return type_line_vector;
			
		-- Computes the distance between a point and a line.
		-- This computation does not care about end or start point of the line.
		-- It assumes an indefinite long line without start or end point.
		function get_distance (
			line	: in type_line;
			point	: in type_point)
			return type_distance_positive;
		
		-- Returns the direction in degrees of a line.
		-- Example: If a line runs from 0/0 to 1/1 then the result is 45 degree.
		-- Example: If a line runs from -1/-1 to -4/-4 then the result is 225 degree.
		function direction ( -- CS rename to get_direction
			line	: in type_line)
			return type_rotation;

		-- Moves a line in the given direction by the given distance:
		procedure move_by (
			line		: in out type_line;
			direction	: in type_rotation;
			distance	: in type_distance_positive);
							  
		-- Moves a line by the given offset. 
		procedure move_by (
			line	: in out type_line;
			offset	: in type_distance_relative);

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
		
		-- Returns the boundaries of the given line.
		-- The line has the given width. 
		-- The boundaries are extended by half the given width.
		function get_boundaries (
			line	: in type_line;	
			width	: in type_distance_positive)
			return type_boundaries;
		
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

		-- These functions return the components of the given type_distance_point_line:
		function out_of_range (d : in type_distance_point_line) return boolean;
		function get_distance (d : in type_distance_point_line) return type_distance_positive;
		function get_intersection (d : in type_distance_point_line) return type_point;
		function get_direction (d : in type_distance_point_line) return type_rotation;
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
		function get_distance (
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

		-- Returns the shortest distance from the given point to the
		-- given line:
		function get_shortest_distance (
			point	: in type_point;
			line	: in type_line)
			return type_distance_polar;
			
		
	-- ARC
		type type_arc_base is abstract tagged record
			center			: type_point;
			start_point		: type_point;
			end_point		: type_point;
			direction		: type_direction_of_rotation := CW;
			-- CS locked : type_locked;		
		end record;

		type type_arc is new type_arc_base with null record;
		-- CS use this type wherever a type_arc is declared unnessecarily.

		
		-- Swaps start and end point of an arc. Reverses the direction of the arc:
		function reverse_arc (arc : in type_arc) return type_arc'class;
		procedure reverse_arc (arc : in out type_arc);

		
		-- Returns the shortest distance from the given point to the
		-- given arc:
		function get_shortest_distance (
			point	: in type_point;
			arc		: in type_arc)
			return type_distance_polar;

		
		-- If start/end point of the candidate arc is ABOVE-OR-ON the 
		-- threshold AND if the end/start point of the candidate arc is BELOW the
		-- threshold then we consider the arc to be threshold-crossing.
		function crosses_threshold (
			arc			: in type_arc;
			y_threshold	: in type_distance)
			return boolean;

		
		-- Deduces the curvature of an arc that crosses a certain y_threshold 
		-- line as it intersects the arc from the left to the right.
		function get_curvature (
			arc		: in type_arc)
			return type_curvature_of_arc;
		
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
		
		-- Returns the boundaries of the given arc.
		-- The arc has the given width. 
		-- The boundaries are extended by half the given width.
		function get_boundaries (
			arc			: in type_arc;
			line_width	: in type_distance_positive) 
			return type_boundaries;
		
		-- Returns true if the given point sits on the given arc.
		-- The optional parameter catch_zone may be used to specifiy the range at
		-- which the point is regarded as sitting on the arc.
		-- If no catch_zone provided, then the minimal possible range
		-- will be used:
		function on_arc (
			point		: in type_point;
			arc			: in type_arc;
			catch_zone	: in type_catch_zone := type_distance'small)
			return boolean; 


		type type_intersection_status_of_line_and_circle is (
			NONE_EXIST, -- no intersection at all
			ONE_EXISTS, -- tangent
			TWO_EXIST); -- two intersections

		-- When finding intersections of a line with a circle (or arc)
		-- we use this type:

		type type_tangent_status is (TANGENT, SECANT);
		
		type type_intersection_of_line_and_circle (
			status : type_intersection_status_of_line_and_circle)
		is record
			case status is
				when NONE_EXIST => null;
				
				when ONE_EXISTS	=> 
					intersection	: type_intersection;
					tangent_status	: type_tangent_status;
				
				when TWO_EXIST	=> 
					intersection_1	: type_intersection;
					intersection_2	: type_intersection;
					
			end case;
		end record;


		-- Computes the intersections of a line with an arc:

		-- - If there is no intersection then it returns NONE_EXIST.
		-- - If there is only one intersection then the given line is a tangent.
		--   The return status will then be ONE_EXISTS and the 
		--   actual intersection (with point and angle).
		--   The tangent status will be TANGENT.
		-- - If there are two intersections then the given line is a secant.
		--   The return status will be TWO_EXIST along with the two intersections
		--   (with their point and angle).
		--   NOTE: There is no information about the order of the two intersections
		--   as the line travels through the arc/circle. Use function order_intersections
		--   to get the intersections ordered.
		--
		-- See details of type type_intersection_of_line_and_circle.
		--
		-- IMPORTANT: CONVENTION ON INTERSECTION ANGLE OF A SECANT:
		-- The angle of intersection is defined as follows:
		-- The given line enters and leaves the arc/circle at some point and angle.
		-- As the given line is a line vector, it has a direction. Imagine
		-- sitting on this line as it enters/leveas the circle. 
		-- The angle BETWEEN the line and the circle circumfence visible
		-- on your LEFT is the angle of intersection.
		-- The angle of intersection is always greater zero and less than 180 degrees.
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

		-- Moves an arc by the given offset. 
		procedure move_by (
			arc		: in out type_arc;
			offset	: in type_distance_relative);

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
		type type_circle_base is abstract tagged record
			center			: type_point;
			radius  		: type_distance_positive := zero;
			-- CS locked : type_locked;
		end record;

		type type_circle is new type_circle_base with null record;
		-- CS use this type wherever a type_arc is declared unnessecarily.

		-- Returns the shortest distance from the given point to the
		-- given circle:
		function get_shortest_distance (
			point	: in type_point;
			circle	: in type_circle)
			return type_distance_polar;
		
		-- Moves a circle by the given offset. 
		procedure move_by (
			circle	: in out type_circle;
			offset	: in type_distance_relative);
		
		procedure mirror (
		-- Mirrors the center of a circle along the given axis.
			circle		: in out type_circle;
			axis		: in type_axis_2d);
		
		-- Rotates the center of a circle about the origin by the given rotation.
		procedure rotate_by (
			circle		: in out type_circle;
			rotation	: in type_rotation);
		
		-- Returns the boundaries of the given circle.
		-- The circle has the given width. 
		-- The boundaries are extended by half the given width.
		function get_boundaries (
			circle		: in type_circle;
			line_width	: in type_distance_positive)						
			return type_boundaries;
		
		-- Returns true if the given point sits on the given circle circumfence.
		-- The optional parameter catch_zone may be used to specifiy the range at
		-- which the point is regarded as sitting on the arc.
		-- If no catch_zone provided, then the minimal possible range
		-- will be used:
		function on_circle (
			point		: in type_point;
			circle		: in type_circle;
			catch_zone	: in type_catch_zone := type_distance'small)
			return boolean;

		-- Gives the status (inside/outside) of a point relative to a circle.
		-- If the point lies exactly at the circumfence then the result is "outside".
		function get_point_to_circle_status (
			point		: in type_point;
			circle		: in type_circle)
			return type_point_status;


	

		
		-- The angle of a tangent to a circle:
		subtype type_tangent_angle_circle is type_rotation range -90.0 .. 90.0;
		
		-- Computes the angle of a tangent that touches a circle
		-- at the given point. The center of the circle is assumed to be the origin.
		-- - If the tangent increases in y as it travels from left to right 
		--   then its angle is positive. 
		-- - If the tangent decreases in y, then its angle is negative.
		-- - If it does not change in y, then the tangent runs horizontally and has zero angle.
		-- - If it is vertical, then its angle is 90 degrees.
		function get_tangent_angle (p : in type_point) 
			return type_tangent_angle_circle;


		
		-- Computes the intersections of a line with a circle.
		-- See more on overloaded function get_intersection (line, arc):
		function get_intersection (
			line	: in type_line_vector;
			circle	: in type_circle)
			return type_intersection_of_line_and_circle;

		type type_ordered_line_circle_intersections is record
			-- The start point of the line that intersects the circle.
			-- The start point must be outside the circle.
			start_point	: type_point;

			-- The point where the line enters and exits the circle:
			entry_point	: type_intersection;
			exit_point	: type_intersection;
		end record;

		-- Sorts the intersections of a line with an arc or a circle in the order
		-- as they appear as the line crosses the circle or the arc:
		-- start point of line, entry point, exit point.
		-- The given intersections must contain two intersections (discrimintant status),
		-- otherwise a constraint error will be raised.
		-- If the given intersections have same distance to start point then
		-- a constraint error will be raised.
		function order_intersections (
			-- The start point of the line that intersects the circle.
			-- The start point must be outside the circle.
			start_point		: in type_point;

			intersections	: in type_intersection_of_line_and_circle)
			return type_ordered_line_circle_intersections;
		
			
		function to_string (line : in type_line) return string;
		-- Returns the start and end point of the given line as string.

		function to_string (arc : in type_arc) return string;
		-- Returns the start, end point and angle of the given arc as string.
		
		function to_string (circle : in type_circle) return string;
		-- Returns the center and radius of the given circle as string.



		




		-- Returns the end of a line that is on the left.
		-- If no boundaries given, then they will be computed.
		-- If boundaries given then the execution is slightly faster.
		-- If boundaries where provided and neither start nor end point
		-- of line match then an exception will be raised.
		function get_left_end (
			line		: in type_line;
			boundaries	: in type_boundaries := boundaries_default)
			return type_point;

		-- Returns the end of a line that is on the right.
		-- See comments on function get_left_end.
		function get_right_end (
			line		: in type_line;
			boundaries	: in type_boundaries := boundaries_default)
			return type_point;

		-- Returns the lower end of a line.
		-- See comments on function get_left_end.
		function get_lower_end (
			line		: in type_line;
			boundaries	: in type_boundaries := boundaries_default)
			return type_point;

		-- Returns the upper end of a line.
		-- See comments on function get_left_end.
		function get_upper_end (
			line		: in type_line;
			boundaries	: in type_boundaries := boundaries_default)
			return type_point;


		
	-- POLYGON
		
		-- IMPORTANT NOTE: 
		-- In contrast to the common definition of a polygon, a polygon
		-- in this world is described as a finite number of elements 
		-- like lines, arcs and even circles that form a polygonal circuit.
		-- In this world a polygon may consist of just a single circle.
		-- In that case no other segments are allowed.
		-- On the other hand, a polygon may consist of lines and arcs. In that
		-- case no circle is allowed:
		type type_polygon_segment_shape is (LINE, ARC);
		
		type type_polygon_segment (shape : type_polygon_segment_shape) is record
			case shape is
				when LINE	=> segment_line : type_line;
				when ARC	=> segment_arc  : type_arc;
			end case;
		end record;
		
		package pac_polygon_segments is new indefinite_doubly_linked_lists (type_polygon_segment);
		
		type type_polygon_segments (circular : boolean := false) is record
			case circular is
				when TRUE	=> circle   : type_circle;
				when FALSE	=> segments : pac_polygon_segments.list;
			end case;
		end record;

		type type_polygon_base is abstract tagged record
			contours	: type_polygon_segments;
		end record;

		-- Returns the segments of a polygon in human readable form:
		function to_string (
			polygon	: in type_polygon_base)
			return string;

		
		-- Returns the corner point nearest to the given
		-- reference point.
		-- If the given polygon consists of just a single
		-- circle then a exception is raised:		
		function get_nearest_corner_point (
			polygon		: in type_polygon_base;
			reference	: in type_point)
			return type_point;

		-- Returns the distance from the given reference point to
		-- to the nearest point on the polygon edges.
		function get_shortest_distance (
			polygon	: in type_polygon_base;
			point	: in type_point)
			return type_distance_polar;


		
		
		-- Returns the segments of a polygon that start or end 
		-- at the given corner point:
		--function get_segments_on_corner_point (
			--polygon	: in type_polygon_base;
			--corner	: in type_point)
			--return type_polygon_segments;

		
		-- Loads the given segments into given polygon.
		-- NOTE: Overwrites already existing segments in the polygon.
		procedure load_segments (
			polygon		: in out type_polygon_base;
			segments	: in type_polygon_segments);
		
		procedure delete_segments (
			polygon : in out type_polygon_base);

		procedure append_segment (
			polygon	: in out type_polygon_base;
			segment	: in type_polygon_segment);

		procedure set_circle (
			polygon	: in out type_polygon_base;
			circle	: in type_circle'class);
			
		function get_segments (
			polygon : in type_polygon_base) 
			return type_polygon_segments;

		-- Returns 1 if the polygon contours consist of just a single circle.
		-- Returns the number of segments if the contours consist of lines
		-- and/or arcs:
		function get_segments_total (
			polygon : in type_polygon_base)
			return count_type;


		--type type_dimensions is record
			--greatest : type_point := far_lower_left;
			--smallest : type_point := far_upper_right;
		--end record;
		
		-- Returns the greatest and smallest x and y values
		-- used by the polygon:
		--function get_dimensions (
			--polygon : in type_polygon_base)
			--return type_dimensions;


		
		-- Transposes a polygon in Y direction.
		-- Each point of each segment gets shifted by
		-- the formula new_y = offset - old_y:
		procedure transpose_polygon (
			polygon	: in out type_polygon_base'class;
			offset	: in type_distance);



		-- Reads the segments provided in a row of
		-- arguments in a form like:
		-- "line 0 0 line 160 0 line 160 80 line 0 80"
		-- or:
		-- line 50 50 70 50 arc 60 50 70 50 50 50 ccw
		-- and builds a polygon.
		-- 1. The end point of a segment must not be specified.
		--    It is deduced from the start point of the successor segment.
		-- 2. A circle can only be read if it is the only shape.
		--    Mixing a circle with lines and arcs is not allowed.
		--    There must be only one circle.
		--    Examples:
		--     - valid  : circle 9 4 10
		--     - invalid: circle 34 45 30 circle 9 4 10
		--     - invalid: line 0 0 circle 9 4 10
		function to_polygon (
			arguments : in type_fields_of_line)
			return type_polygon_base'class;
		
		-- Returns the boundaries of the given polygon.
		function get_boundaries (
			polygon		: in type_polygon_base;
			line_width	: in type_distance_positive)
			return type_boundaries;

		-- A polygon must have a properly closed outline.
		-- The outline check returns a list of points (where the gaps are):
		package pac_polygon_gaps is new doubly_linked_lists (type_point); 

		-- Returns the points where gaps of a polygon outline begin:
		function to_string (
			gaps : in pac_polygon_gaps.list)
			return string;
		

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

		-- Moves a polygon by the given offset. 
		procedure move_by (
			polygon	: in out type_polygon_base;
			offset	: in type_distance_relative);

		-- Mirrors a polygon along the given axis.
		procedure mirror (
			polygon	: in out type_polygon_base;
			axis	: in type_axis_2d);

		-- Rotates a polygon about the origin by the given rotation.
		procedure rotate_by (
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
		-- CS: Largely incomplete !!!
		procedure offset_polygon (
			polygon		: in out type_polygon_base;
			offset		: in type_offset);

		


		-- In order to get the status of a point relative to
		-- a polygon we need this stuff:
		-- The general approach is:
		-- A ray that starts at point and travels in zero degees 
		-- may intersect the polygon edges.
		-- The result of such a query is the type_inside_polygon_query_result
		-- that contains a status flag (inside/outside) and a list
		-- of x values where the ray intersects the polygon. For completeness
		-- the original point where the probe line has started is also provided.
		-- This list provides the x values ordered according to their
		-- distance to the start point of the ray. Lowest value first.

		type type_intersected_segment (shape : type_shape := LINE) is record
			case shape is 
				when LINE	=> segment_line : type_line;
				when ARC	=> segment_arc : type_arc;
				when CIRCLE	=> segment_circle : type_circle;
			end case;
		end record;

		-- The intersection of a probe line with the polygon side can
		-- be described as:
		type type_probe_line_intersection (
			curvature : type_curvature)
		is record
			
			x_position	: type_distance;
			angle		: type_rotation := zero_rotation;

			segment : type_intersected_segment;
			
			case curvature is
				when STRAIGHT => null;
				
				when CONVEX | CONCAVE =>
					-- The center and radius of an imaginary circle:
					center	: type_point := origin;
					radius	: type_distance_positive := zero;
			end case;

		end record;

		
		-- Subtracts 180 degree from the given angle if it is
		-- greater 90 degrees and returns the absolute value of the difference.
		-- Otherwise returns the given angle unchanged.		
		function subtract_180_if_greater_90 (
			angle : in type_rotation)
			return type_rotation;

		
		function "<" (left, right : in type_probe_line_intersection)
			return boolean;
		

		
		package pac_probe_line_intersections is new
			indefinite_doubly_linked_lists (type_probe_line_intersection);

		package pac_probe_line_intersections_sorting is new pac_probe_line_intersections.generic_sorting;
			
		type type_inside_polygon_query_result is record
			-- the point where the probe line has started:
			start			: type_point; 

			status			: type_point_status := OUTSIDE;		

			-- the intersections of the probe line with the polygon edges:
			intersections	: pac_probe_line_intersections.list;
		end record;

		function invert_status (
			intersections	: in type_inside_polygon_query_result)
			return type_inside_polygon_query_result;
		
		package pac_intersections is new 
			doubly_linked_lists (type_inside_polygon_query_result);
			
		-- Merges the intersections of query_2 with query_1.
		-- Updates the status accordingly.
		-- Sorts the intersections in query_1 finally by their 
		-- x-position (from left to right):
		procedure merge_intersections (
			query_1 : in out type_inside_polygon_query_result;
			query_2 : in type_inside_polygon_query_result);

		
		-- Returns the query result as a human readable string:
		function to_string (
			i : in type_inside_polygon_query_result)
			return string;
		
		-- Detects whether the given point is inside or outside
		-- the polygon. The point is regarded as "outside" if
		-- it sits exactly on the edge of the polygon.
		-- A probe line will be formed which starts at the given point
		-- and runs to the right (direction zero degree).
		-- The places where the probe line intersects the polygon 
		-- edges is returned in a list.
		-- If a segment of the polygon crosses the imaginary probe line,
		-- then it is regarded as intersection.
		-- NOTE: A line segment that runs exactly along the probe line
		-- is NOT regarded as "crossing" the probe line.
		function in_polygon_status (
			polygon		: in type_polygon_base;	
			point		: in type_point)
			return type_inside_polygon_query_result;

							   
		-- Returns true if the given query result contains at least
		-- one x-value of an intersection:
		function intersections_found (
			i : in type_inside_polygon_query_result)
			return boolean;

		-- Returns the first intersection of the given query result.
		-- Raises constraint error if given query result does
		-- not contain any intersections:
		function get_first_intersection (
			i : in type_inside_polygon_query_result)
			return type_probe_line_intersection;
		
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
		function get_lower_left_corner (
			polygon	: in type_polygon_base)
			return type_lower_left_corner;


		type type_polygon is new type_polygon_base with null record;

		
	private
		type type_vector is	record
			x, y, z : type_position_axis := zero;
		end record;

		null_vector		: constant type_vector := (others => zero);
		unity_vector	: constant type_vector := (others => 1.0);
		
		type type_distance_point_line is record
			sits_on_start	: boolean := false;
			sits_on_end		: boolean := false;
			out_of_range	: boolean := true;

			-- A virtual line runs from the given point perpendicular
			-- to the given line. This is where the virtual line intersects
			-- the given line:
			intersection	: type_point := origin;
			distance		: type_distance_positive := zero;
			direction		: type_rotation := zero_rotation;
		end record;
		
	end generic_pac_shapes;
	
	
end et_geometry;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
