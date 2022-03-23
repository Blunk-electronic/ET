------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              GEOMETRY 1                                  --
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
with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;

with ada.numerics;
with ada.numerics.generic_elementary_functions;

with et_geometry;				use et_geometry;
with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;


generic
	type type_distance is delta <> digits <>;
	type type_distance_coarse is delta <> digits <>;
	type type_float_internal is digits <>;
	axis_min, axis_max : type_distance;
	type type_rotation is delta <> digits <>;


package et_geometry_1 is

	function to_string (f : in type_float_internal) return string;
	
	subtype type_float_internal_positive is type_float_internal range 0.0 .. type_float_internal'last;

	rounding_threshold : constant type_float_internal := 1.0E-17;


	-- The number of decimal places when rounding or type_float_internal
	-- is required:
	subtype type_rounding_accuracy is positive 
		range 1 .. type_float_internal'digits;
	
	procedure round (
		f : in out type_float_internal;	-- the number to be rounded
		a : in type_rounding_accuracy);	-- the accuracy, the number of decimal places

	function round (
		f : in type_float_internal;
		a : in type_rounding_accuracy)
		return type_float_internal;

	
	zero 		: constant type_distance := 0.0;
	far_left	: constant type_distance := axis_min;
	far_right	: constant type_distance := axis_max;


	package pac_functions_distance is new 
		ada.numerics.generic_elementary_functions (type_float_internal);
	use pac_functions_distance;

	package pac_distance_io is new ada.text_io.float_io (type_float_internal);

	
	
	function get_info (editor: in string)
		return string;


	-- Returns 1.0 if given x is greater or equal zero.
	-- Returns -1.0 if x less than zero.
	function sgn (x : type_float_internal) return type_float_internal;

	
	
	function to_distance (distance : in string) 
		return type_distance;		
	
	function to_string (distance : in type_distance)
		return string;

	function to_string (d_coarse : in type_distance_coarse)
		return string;


	
	-- The position along an axis:
	subtype type_position_axis is type_distance 
		range axis_min .. axis_max;



	
	function to_distance (f : in type_float_internal)
		return type_distance;

	function to_rotation (f : in type_float_internal)
		return type_rotation;

	
	
	type type_rounding_mode is (
		DOWN,			-- down to nearest multiple of type_distance_coarse'small
		BANKERS_RULE,	-- 0..4 down, 5..9 up				
		UP);			-- up to nearest multiple of type_distance_coarse'small

	rounding_mode_default : constant type_rounding_mode := BANKERS_RULE;

		
	-- Rounds the given distance to the nearest multiple of 
	-- type_distance_coarse'small.
	function round (
		d_fine	: in type_distance;
		mode	: in type_rounding_mode := rounding_mode_default) 
		return type_distance_coarse;
	
	function clip_distance (d : in type_distance)
		return type_position_axis;

	procedure clip_distance (d : in out type_distance);

	
	-- The distance between two objects:
	subtype type_distance_positive is type_distance 
		range zero .. type_distance'last;

	
	subtype type_catch_zone is type_distance_positive
		range zero .. type_distance_positive'last/100.0;

	
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
	
	
	-- For collecting and sorting distances:
	package pac_distances_positive is new doubly_linked_lists (type_distance_positive);
	package pac_distances_positive_sorting is new pac_distances_positive.generic_sorting;
	
	package pac_distances is new doubly_linked_lists (type_distance);
	package pac_distances_sorting is new pac_distances.generic_sorting;


	-- Returns the greatest distance from a list of positive distances:
	function get_greatest (
		distances	: in pac_distances_positive.list)
		return type_distance_positive;
	
	
	grid_max : constant type_distance_positive := axis_max * 0.1;
	subtype type_distance_grid is type_distance_positive range zero .. grid_max;
	grid_default : constant type_distance_grid := 2.5;


	-- Returns a position along an axis rounded according to given grid.		
	function round (
		distance	: in type_position_axis;
		grid		: in type_distance_grid)
		return type_position_axis;

	
	type type_grid is record
		x,y	: type_distance_grid := grid_default;
	end record;

	
	
	
	procedure scale_grid (
		grid	: in out type_grid;
		scale	: in type_distance_positive);			
	
	function to_string (grid : in type_grid) return string;



	type type_distance_relative is record
		x, y : type_distance := zero;
	end record;

	function to_string (distance : in type_distance_relative)
		return string;


	
-- TYPE POINT:

	type type_point is tagged record
		x, y : type_position_axis := zero;
	end record;

	
	origin			: constant type_point;		
	far_upper_left	: constant type_point;
	far_upper_right	: constant type_point;
	far_lower_left	: constant type_point;
	far_lower_right	: constant type_point;

	
	function "<" (left, right : in type_point) return boolean;

	--function "=" (left, right : in type_point) return boolean;


	function to_string (point : in type_point) return string;





	

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
	
	
	function round (
		point : in type_point)
		return type_point'class;

	
	procedure round (
		point : in out type_point);

		
	function to_distance_relative (
		p : in type_point)
		return type_distance_relative;



	-- Returns the rotation of the given point about the origin.
	-- If for example point is (1/1) then the return is 45 degree.
	-- if point is (-1/-1) then the return is -135 degree.
	function get_rotation (
		point : in type_point)
		return type_rotation;

	
	-- Rotates the given point BY the given angle about the origin.
	-- Changes point.x and point.y only.
	procedure rotate_by (
		point		: in out type_point;
		rotation	: in type_rotation);
	
	
	-- Rotates the given point TO the given angle about the origin.
	-- Changes point.x and point.y only.
	procedure rotate_to (
		point		: in out type_point;
		rotation	: in type_rotation);

	
	
	function to_point (
		d 		: in type_distance_relative;
		clip	: in boolean := false)
		return type_point'class;


	-- Inverts the given relative distance by 
	-- multiplying x by -1 and y by -1.
	function invert (
		d : in type_distance_relative) 
		return type_distance_relative;

	
	
	
	
	function get_x (
		point : in type_point)
		return type_position_axis;
	
	function get_y (
		point : in type_point)
		return type_position_axis;	


	
	
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

	-- Returns true if the given boundaries intersect each other.
	-- Returns false if boundaries do not intersect and if they
	-- touch each other:
	function intersect (
		boundaries_one : in type_boundaries; -- CS rename to b1,b2. do so with other functions below
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

	
	-- Moves the boundaries by the given offset.
	-- If clip is true, then the boundaries will assume
	-- the min or max value as defined by type_position_axis.
	-- If clip is false, then a constraint error may arise
	-- if the boundaries exceed the limits defined by type_position_axis:
	procedure move_by (
		boundaries	: in out type_boundaries;
		offset		: in type_distance_relative;
		clip		: in boolean := false);

	
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



	
	function set (
		x, y : in type_position_axis) 
		return type_point'class;

	
	procedure set (
		point	: in out type_point;
		axis 	: in type_axis_2d;
		value	: in type_position_axis);

	
	procedure set (
		point	: in out type_point'class;
		position: in type_point);

	
	-- The quadrants of the coordinate system are numbered counter clockwise.
	-- Quadrant ONE is top right.
	type type_quadrant is (ONE, TWO, THREE, FOUR);

	-- Returns the quadrant the point is located in.
	-- ONE  : point is on the right of the y-axis or on it AND above the x-axis or on it
	-- TWO  : point is left of the y-axis AND above the x-axis or on top of it
	-- THREE: point is left of the y-axis AND below the x-axis
	-- FOUR : point is right of the y-axis or on top of it AND below the x-axis
	function get_quadrant (
		point : in type_point) 
		return type_quadrant;

	
	-- Inverts the given point by multiplying x by -1 and y by -1.
	function invert (
		point : in type_point'class) 
		return type_point'class;

	
	-- Inverts the point on the given axis.
	function invert (
		point	: in type_point;
		axis	: in type_axis_2d)
		return type_point'class;

	
	-- Moves the given point to the origin (0/0).
	procedure reset (
		point : in out type_point);

	
	-- Moves a point by the given offset.
	procedure move_by (
		point	: in out type_point;
		offset	: in type_distance_relative);

	
	-- Moves a point to the given destination:
	procedure move_to (
		point		: in out type_point;
		destination	: in type_point'class);

	
	-- Moves a point into direction by distance.
	function move (
		point		: in type_point;
		direction	: in type_rotation;
		distance	: in type_distance_positive;
		clip		: in boolean := false)
		return type_point'class;

	
	-- If axis is Y then it swaps right x with left x.
	-- If axis is X then it swaps upper y with lower y.
	procedure mirror (
		point	: in out type_point;
		axis	: in type_axis_2d);	

	
	-- Returns the distance along the given axis between the given points.
	function get_distance (
		point_1	: in type_point;
		point_2	: in type_point;
		axis	: in type_axis_2d) 
		return type_distance;

	
	-- Returns the absolute distance along the given axis between the given points.
	-- NOTE: The result in both x and y is always greater or equal zero.
	function get_distance_abs (
		point_1	: in type_point;
		point_2	: in type_point;
		axis	: in type_axis_2d) 
		return type_distance_positive;

	
	-- Adds x and y of given points as:
	-- result.x = point_one.x + point_two.x and
	-- result.y = point_one.y + point_two.y and
	function "+" (point_one, point_two : in type_point) return type_point'class;

	-- Subtracts x and y of given points as:
	-- result.x = point_one.x - point_two.x and
	-- result.y = point_one.y - point_two.y and
	function "-" (point_one, point_two : in type_point) return type_point'class;


	
	
	-- Returns the relative distance of point_two to point_one.	
	-- Subtracts point_one.x from point_two.y and point_one.y from point_two.y
	-- returns	d.x := point_two.x - point_one.x
	--			d.y := point_two.y - point_one.y;
	function get_distance_relative (
		point_one, point_two : in type_point) 
		return type_distance_relative;

	
	
	-- Computes the total distance between point_one and point_two.
	function get_distance_total (
		point_one, point_two : in type_point)
		return type_distance_positive;

	
	-- Returns true if point_2 is within the 
	-- catch zone around point_1:
	function in_catch_zone (
		point_1		: in type_point; -- the reference point
		catch_zone	: in type_catch_zone; -- zone around reference point
		point_2 	: in type_point) -- the point being tested
		return boolean;
	
	-- Adds two angles.
	-- If result greater 360 degree then 360 degree is subtracted from result.
	-- If result less than 360 degree then 360 degree is added to the result.
	function add (left, right : in type_rotation) return type_rotation;


	
	type type_distance_polar is private;

	function to_string (
		distance : in type_distance_polar)
		return string;
	
	function to_polar (
		absolute	: in type_distance_positive;
		angle		: in type_rotation)
		return type_distance_polar;

	-- Sets the absolute component of a polar distance.
	-- If the given absolute is zero, then the angle component
	-- is NOT changed.
	procedure set_absolute (
		distance : in out type_distance_polar;
		absolute : in type_distance_positive);

	procedure set_angle (
		distance : in out type_distance_polar;
		angle    : in type_rotation);

	-- Adds 180 degree to the angle of the given polar distance.
	-- Example: changes 45 degrees to 225 degrees:
	procedure reverse_angle (
		distance : in out type_distance_polar);
								
	
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



	
-- TYPE POSITION:
	
	type type_position is new type_point with private;

	overriding function to_string (point : in type_position) return string;
	
	function to_rotation (rotation : in string) return type_rotation;
	function to_string (rotation : in type_rotation) return string;

	
	type type_direction_of_rotation is (
		CW,		-- clockwise
		CCW);	-- counterclockwise

	function to_string (direction : in type_direction_of_rotation) return string;
	function to_direction (direction : in string) return type_direction_of_rotation;

	-- Returns CW if rotation is negative. 
	-- Returns CCW if rotation is positive or zero.
	function get_direction (rotation : in type_rotation) 
		return type_direction_of_rotation;

	-- Changes CCW to CW and vice versa.
	function reverse_direction (direction : in type_direction_of_rotation)
		return type_direction_of_rotation;
	
	origin_zero_rotation : constant type_position;

	-- A position at the greatest distance in
	-- x and y from the origin:
	far_upper_right_zero_rotation : constant type_position;
	
	units_per_cycle : constant type_float_internal := 360.0;

	zero_rotation : constant type_rotation := 0.0;

	radians_max : constant type_float_internal := - 2.0 * ada.numerics.pi;
	
	subtype type_radians is type_float_internal range (- radians_max) .. radians_max;
	
	function to_radians (degrees : in type_rotation) return type_float_internal;
	-- Converts degrees to radians.

	function to_degrees (radians : in type_float_internal) return type_rotation;
	-- Converts radians to degrees.


	function to_position (
		point		: in type_point;
		rotation	: in type_rotation)
		return type_position'class;

	
	-- Sets the rotation of a position. (position.rotation)
	procedure set (
		position	: in out type_position;
		rotation	: in type_rotation);

	
	-- Returns the rotation of the given position.
	function get_rotation (
		position : in type_position) 
		return type_rotation;
	
	
	-- Changes the rotation of the given position by the given offset.
	-- Preserves x/y. Changes position.rotation only.
	procedure rotate_about_itself (
		position	: in out type_position;
		offset		: in type_rotation);



	
	package pac_points is new doubly_linked_lists (type_point);


	-- Returns a human readable string of points:
	function to_string (points : in pac_points.list) return string;

	
	-- Appends all points of source to the target.
	procedure splice_points (
		points_target : in out pac_points.list;
		points_source : in pac_points.list);


	-- Removes points which are stored multiple times
	-- from the given list:
	procedure remove_redundant_points (
		points : in out pac_points.list);
										  

	-- Sorts the given list of points by their distance to
	-- the given reference point. The first point in the result
	-- will be the one closest to the reference point:
	procedure sort_by_distance (
		points 		: in out pac_points.list;
		reference	: in type_point'class);
	
	
private
	
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
	

	
end et_geometry_1;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
