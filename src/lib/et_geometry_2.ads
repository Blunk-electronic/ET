------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              GEOMETRY 2                                  --
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

with et_geometry;				use et_geometry;
with et_geometry_1;
with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;


generic
	
	with package pac_geometry_1 is new et_geometry_1 (<>);

	type type_distance is delta <> digits <>;
	--type type_distance_coarse is delta <> digits <>;
	axis_min, axis_max : type_distance;

	type type_rotation is delta <> digits <>;
	
package et_geometry_2 is
	

	use pac_geometry_1;
	use pac_float_numbers_functions;


	zero 		: constant type_distance := 0.0;
	far_left	: constant type_distance := axis_min;
	far_right	: constant type_distance := axis_max;


	
	function get_info (editor: in string)
		return string;
	

	-- The distance between two objects:
	subtype type_distance_positive is type_distance 
		range zero .. type_distance'last;

	-- The position along an axis:
	subtype type_position_axis is type_distance 
		range axis_min .. axis_max;


	function clip_distance (d : in type_distance)
		return type_position_axis;

	procedure clip_distance (d : in out type_distance);


	
	-- Converts a mil number (given as a string) to millimeters.	
	function mil_to_distance (mil : in string) return type_distance;

	function distance_to_mil (distance : in type_distance) return string;

	
	function to_distance (dd : in string) 
		return type_distance;		


	-- Converts a float number to type_distance by rounding
	-- according to the bankers rule:
	-- Use it !!!!!
	function to_distance (f : in type_float_internal)
		return type_distance;
	

	function to_string (distance : in type_distance)
		return string;

	--function to_string (d_coarse : in type_distance_coarse)
		--return string;




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

	
	

	type type_rounding_mode is (
		DOWN,			-- down to nearest multiple of type_distance_coarse'small
		BANKERS_RULE,	-- 0..4 down, 5..9 up				
		UP);			-- up to nearest multiple of type_distance_coarse'small

	rounding_mode_default : constant type_rounding_mode := BANKERS_RULE;

	-- Rounds the given distance to the nearest multiple of 
	-- type_distance_coarse'small.
	--function round (
		--d_fine	: in type_distance;
		--mode	: in type_rounding_mode := rounding_mode_default) 
		--return type_distance_coarse;



-- BOUNDARIES:
	

	boundaries_default : constant type_boundaries;




	
	-- Moves the boundaries by the given offset.
	-- If clip is true, then the boundaries will assume
	-- the min or max value as defined by type_position_axis.
	-- If clip is false, then a constraint error may arise
	-- if the boundaries exceed the limits defined by type_position_axis:
	procedure move_by (
		boundaries	: in out type_boundaries;
		offset		: in type_offset;
		clip		: in boolean := false);

	
	
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



	-- For collecting and sorting distances:
	package pac_distances_positive is new doubly_linked_lists (type_distance_positive);
	package pac_distances_positive_sorting is new pac_distances_positive.generic_sorting;
	
	package pac_distances is new doubly_linked_lists (type_distance);
	package pac_distances_sorting is new pac_distances.generic_sorting;


	-- Returns the greatest distance from a list of positive distances:
	function get_greatest (
		distances	: in pac_distances_positive.list)
		return type_distance_positive;

	
	
	
	type type_point_status is (
		OUTSIDE,	-- point is outside a certain area
		INSIDE);	-- point is inside a certain area

	function to_string (status : in type_point_status) return string;
	
	procedure toggle_status (status : in out type_point_status);

	
	

	
	


----------------

-- ROTATION / ANGLE

	zero_rotation : constant type_rotation := 0.0;
	
	

	
	function to_rotation (rotation : in string) return type_rotation;
	function to_string (rotation : in type_rotation) return string;


	-- Converts a float number to type_rotation by rounding
	-- according to the bankers rule:
	-- Use it !!!!!
	function to_rotation (
		f : in type_float_internal)
		return type_rotation;


	function to_angle (
		a : in type_rotation)
		return type_float_internal;
	
	
	--subtype type_rotation_positive is type_rotation
		--range 0.0 .. type_rotation'last;
	
	---- Converts an angle like -90.0 degrees to 270 degrees.
	---- Converts an angle like -1.0 degrees to 359 degrees.
	---- Leaves a positive angle like 135 degree as it is and returns it.
	--function to_positive_rotation (
		--rotation : in type_rotation)
		--return type_rotation_positive;

	

	
	-- Adds two angles.
	-- If result greater 360 degree then 360 degree is subtracted from result.
	-- If result less than 360 degree then 360 degree is added to the result.
	function add (
		left, right : in type_rotation) 
		return type_rotation;


	
-- RELATIVE DISTANCE:
	
	type type_distance_relative is record
		x, y : type_distance := zero;
	end record;


	-- Inverts the given relative distance by 
	-- multiplying x by -1 and y by -1.
	function invert (
		d : in type_distance_relative) 
		return type_distance_relative;

	
	function to_string (
		distance : in type_distance_relative)
		return string;


	function to_distance_relative (
		x,y : in type_distance)
		return type_distance_relative;

	
	function to_distance_relative (
		v : in type_vector)
		return type_distance_relative;


	
---- POLAR:

	---- Returns the angle of the given polar distance:
	--function get_angle (
		--distance : in type_distance_polar)
		--return type_rotation;

	
	---- Returns the absolute of the given polar distance:
	--function get_absolute (
		--distance : in type_distance_polar)
		--return type_distance_positive;

	
	
-- POINT:

	type type_point is record
		x, y : type_position_axis := zero;
	end record;

	
	origin			: constant type_point;		
	far_upper_left	: constant type_point;
	far_upper_right	: constant type_point;
	far_lower_left	: constant type_point;
	far_lower_right	: constant type_point;


	function to_vector (
		point	: in type_point)
		return type_vector;
	
	
	function to_point (
		v	: in type_vector)
		return type_point;


	function to_offset (
		p : in type_point)
		return type_offset;
	

	function to_offset (
		x, y : in type_distance)
		return type_offset;



	
	function to_distance_relative (
		p : in type_point)
		return type_distance_relative;

	
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




	
	function get_x (
		point : in type_point)
		return type_position_axis;
	
	function get_y (
		point : in type_point)
		return type_position_axis;	



	function set (
		x, y : in type_position_axis) 
		return type_point;

	
	procedure set (
		point	: in out type_point;
		axis 	: in type_axis_2d;
		value	: in type_position_axis);

	
	procedure set (
		point	: in out type_point;
		position: in type_point);



	-- Inverts the given point by multiplying x by -1 and y by -1.
	function invert (
		point : in type_point) 
		return type_point;

	
	-- Inverts the point on the given axis.
	function invert (
		point	: in type_point;
		axis	: in type_axis_2d)
		return type_point;

	
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
		destination	: in type_point);


	-- Moves a point into direction by distance.
	function move (
		point		: in type_point;
		direction	: in type_rotation;
		distance	: in type_distance_positive;
		clip		: in boolean := false)
		return type_point;

	
	-- If axis is Y then it swaps right x with left x.
	-- If axis is X then it swaps upper y with lower y.
	procedure mirror (
		point	: in out type_point;
		axis	: in type_axis_2d);	
	

	function get_distance_total ( -- CS rename to get_distance_absolute
		point	: in type_point;
		vector	: in type_vector)
		return type_float_internal_positive;

	
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
	function "+" (point_one, point_two : in type_point) return type_point;

	-- Subtracts x and y of given points as:
	-- result.x = point_one.x - point_two.x and
	-- result.y = point_one.y - point_two.y and
	function "-" (point_one, point_two : in type_point) return type_point;


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
		return type_float_internal_positive;
	


	
	subtype type_catch_zone is type_float_internal_positive
		range 0.0 .. type_float_internal (type_distance_positive'last/100.0);


	function catch_zone_to_string (
		c : in type_catch_zone)
		return string;

	function to_catch_zone (
		c : in string)
		return type_catch_zone;

	
	-- Returns true if point_2 is within the 
	-- catch zone around point_1:
	function in_catch_zone (
		point_1		: in type_point; -- the reference point
		catch_zone	: in type_catch_zone; -- zone around reference point
		point_2 	: in type_point) -- the point being tested
		return boolean;


	
	point_preamble_with_rotation : constant string := 
		" (x"
		& axis_separator
		& "y"
		& axis_separator
		& "rotation)";

	
	function to_string (
		point : in type_point) 
		return string;

	
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
		return type_point;
	
	
	--function round (
		--point : in type_point)
		--return type_point;

	
	--procedure round (
		--point : in out type_point);








	-- Returns the rotation of the given point about the origin.
	-- If for example point is (1/1) then the return is 45 degree.
	-- if point is (-1/-1) then the return is -135 degree.
	function get_rotation ( -- CS rename to get_rotation_about_origin
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
		return type_point;


	function to_point (
		x,y : in string)
		return type_point;


	-- Unites the point with the boundaries. boundaries is updated.
	procedure union (
		boundaries	: in out type_boundaries;
		point		: in type_point);

	
	-- Calculates the boundaries of the given points
	-- connected with a line that has the
	-- given width. The boundaries are extended
	-- by half the given width.
	function get_boundaries (
		point_one	: in type_point;
		point_two	: in type_point;
		width		: in type_distance_positive) 
		return type_boundaries;
	-- CS obsolete ?
	
	
	-- Compares two points by their distance to the origin:
	function "<" (left, right : in type_point) return boolean;

	
	-- Use this package when lists of points must be handled:
	package pac_points is new doubly_linked_lists (type_point);


	-- Returns from a list of point the one that is closest to
	-- the given reference point:
	function get_nearest (
		points		: in pac_points.list;
		reference	: in type_point := origin)
		return type_point;

	
	-- Converts a list of points to vectors:
	function to_vectors (
		points : in pac_points.list)
		return pac_vectors.list;

	
	-- Returns a human readable string of points:
	--function to_string (points : in pac_points.list) return string;

	
	-- Appends all points of source to the target.
	--procedure splice_points (
		--points_target : in out pac_points.list;
		--points_source : in pac_points.list);

	
	-- Removes points which are stored multiple times
	-- from the given list:
	procedure remove_redundant_points (
		points : in out pac_points.list);

	
	-- Sorts the given list of points by their distance to
	-- the given reference point. The first point in the result
	-- will be the one closest to the reference point:
	--procedure sort_by_distance (
		--points 		: in out pac_points.list;
		--reference	: in type_point);
	





-- LINE
	type type_line_base is abstract tagged record
		start_point 	: type_point;
		end_point   	: type_point;
	end record;

	
	type type_line is new type_line_base with null record;


	function to_line_fine (
		line : in type_line)
		return pac_geometry_1.type_line;

	
	function to_line_coarse (
		line : in pac_geometry_1.type_line)
		return type_line'class;


	-- Returns the start and end point of the given line as string.
	function to_string (line : in type_line) return string;

	
	-- Returns the location vector of the start point of a line:
	function get_start_vector (
		line	: in type_line)
		return type_vector; -- CS should be type_point

	
	-- Returns the location vector of the end point of a line:
	function get_end_vector (
		line	: in type_line)
		return type_vector; -- CS should be type_point

	
	-- Returns the direction vector of a line:
	function get_direction_vector (
		line	: in type_line)
		return type_vector; -- CS should be type_distance_relative ?


	-- Returns the direction in degrees of a line.
	-- Example: If a line runs from 0/0 to 1/1 then the result is 45 degree.
	-- Example: If a line runs from -1/-1 to -4/-4 then the result is 225 degree.
	function get_direction (
		line	: in type_line)
		return type_angle;

	
	-- Converts a line (consisting of start and end point)
	-- to a line vector consisting of start vector and
	-- direction vector.
	-- The start vector of the result will be directly derived 
	--  from the start point of the given line.
	-- The direction vector of the result will be computed as:
	--  dx = line.end_point.x - line.start_point.x
	--  dy = line.end_point.y - line.start_point.y
	--  dz = zero
	function to_line_vector (
		line	: in type_line)
		return type_line_vector;



	
	function make_line (
		start_x, start_y, end_x, end_y : in type_distance)
		return type_line'class;

	
	function make_line (
		start_point, end_point : in type_point)
		return type_line'class;



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

	
	
	--function round (line : in type_line)
		--return type_line'class;

	--procedure round (line : in out type_line);
	
	
	-- Returns the length of a line:
	function get_length (line : in type_line)
		return type_float_internal_positive;


	-- Returns the length of the longest line.
	-- If both have equal length, then the length of 
	-- the second line will be returned:
	function get_greatest_length (l1, l2 : in type_line)
		return type_float_internal_positive;

	
	-- Returns the longest of the given lines.
	-- If both have equal length then l2 is returned:
	function get_longest (l1, l2 : in type_line)
		return type_line'class;


	
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


	
	subtype type_tangent_angle is type_rotation range 0.0 .. 180.0 - type_rotation'small;
	
	-- Converts the angle of a tangent to a line direction.
	function get_tangent_direction (
		angle : in type_tangent_angle)
		return type_line_direction;

	
	-- Returns the direction of a line:
	function get_direction (line : in type_line)
		return type_line_direction;


	-- Computes the distance between a location vector and a line.
	-- This computation does not care about end or start point of the line.
	-- It assumes an indefinite long line without start or end point.
	function get_distance (
		line	: in type_line;
		vector	: in type_vector)
		return type_float_internal_positive;



	
	-- Computes the shortest distance (perpendicular) of a 
	-- location vector to a line. 
	-- CS insufficient ! More details !!! especially on the out_of_range flag
	function get_distance (
		line		: in type_line;
		vector		: in type_vector; 
		line_range	: in type_line_range)
		return type_distance_point_line;

	
	-- Computes the shortest distance (perpendicular) of a
	-- point to a line. 		
	function get_distance (
		line		: in type_line;
		point		: in type_point; 
		line_range	: in type_line_range)
		return type_distance_point_line;


	-- Returns true if the given location vector lies on the given line.
	function on_line (
		line	: in type_line;
		vector	: in type_vector)
		return boolean; 

	
	-- Returns true if the given point lies on the given line.
	function on_line (
		line	: in type_line;
		point	: in type_point)
		return boolean;

	
	-- Returns the shortest distance from the given point to the
	-- given line:
	function get_shortest_distance (
		line	: in type_line;
		point	: in type_point)
		return type_float_internal_positive;

	
	function get_shortest_distance (
		line	: in type_line;
		point	: in type_vector)
		return type_float_internal_positive;
	
	
	-- Tests whether the given line_vector intersects the given 
	-- candidate line.
	-- If there is an intersection between start and end point
	-- of the candidate line (start and end point included),
	-- then returns the location vector of the intersection.
	-- If the intersection is before start point or
	-- beyond end point of the given line, return NOT_EXISTENT.
	-- NOTE: The angle of intersection is measured between the 
	-- start points of the two lines. It is always positive.
	function get_intersection (
		line		: in type_line;
		line_vector	: in type_line_vector)
		return type_intersection_of_two_lines;


	-- Tests whether the given two lines intersect or overlap
	-- each other. 
	-- Independend of start and end points, both lines are regarded as infinitely
	-- long beyond their start and end points:
	function get_intersection (
		line_1, line_2 : in type_line)
		return type_intersection_of_two_lines;



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



	-- Returns the boundaries of the given line.
	-- The line has the given width. 
	-- The boundaries are extended by half the given width.
	function get_boundaries (
		line	: in type_line;	
		width	: in type_distance_positive)
		return type_boundaries;



-- ZONES OF A LINE

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
		line	: in type_line'class;
		point	: in type_point)
		return type_line_zone;



-- ARC
	
	type type_arc_base is abstract tagged record  -- CS should be private ?
		center			: type_point;
		start_point		: type_point;
		end_point		: type_point;
		direction		: type_direction_of_rotation := CW;
	end record;

	type type_arc is new type_arc_base with null record;
	-- CS use this type wherever a type_arc is declared unnessecarily.


	
	function to_arc_fine (
		arc : in type_arc)
		return pac_geometry_1.type_arc;

	function to_arc_coarse (
		arc : in pac_geometry_1.type_arc)
		return type_arc'class;

	
	
	-- Returns the start, end point and angle of the given arc as string.
	function to_string (arc : in type_arc) return string;

	
	--function round (arc : in type_arc)
		--return type_arc'class;

	--procedure round (arc : in out type_arc);
	

	
	-- Swaps start and end point of an arc. Reverses the direction of the arc:
	function reverse_arc (arc : in type_arc) return type_arc'class;
	procedure reverse_arc (arc : in out type_arc);

	-- Changes the direction of an arc to CCW (mathematical sense)
	-- by swapping start and end point. If direction is already CCW
	-- then nothing happens.
	function normalize_arc (arc: in type_arc) return type_arc'class;

	-- Returns true if start and end point of arc are equal:
	function zero_length (arc : in type_arc) return boolean;

	
	-- Returns the shortest distance between a point and an arc.
	-- If the point is on the center of the arc, then the return is
	-- absolute zero and angle zero degree:
	-- CS: wrong, should be absolute distance to start and angle of start point.
	function get_shortest_distance (
		arc		: in type_arc;
		point	: in type_point)
		return type_distance_polar;


	-- CS: INCOMPLETE !!! Returns always zero currently.
	function get_shortest_distance (
		arc		: in type_arc;
		point	: in type_vector)
		return type_float_internal;

	
	-- If start/end point of the candidate arc is ABOVE-OR-ON the 
	-- threshold AND if the end/start point of the candidate arc is BELOW the
	-- threshold then we consider the arc to be threshold-crossing.
	--function crosses_threshold ( -- CS remove ?
		--arc			: in type_arc;
		--y_threshold	: in type_distance)
		--return boolean;

	
	
	-- Returns the distance between the start point and the center of the arc.
	function get_radius_start (
		arc : in type_arc) 
		return type_float_internal_positive;
	
	
	-- Returns the distance between the end point and the center of the arc.
	function get_radius_end (
		arc : in type_arc) 
		return type_float_internal_positive;

	
	-- Test whether the given arc is valid. The arc is valid if:
	-- - start and end point have equal distance to center
	-- - radius is greater zero
	function is_valid (
		arc : in type_arc)
		return boolean;
	



	function to_string (
		arc : in type_arc_angles)
		return string;
	
		
	-- Returns the start and end angles of an arc.
	function to_arc_angles (
		arc : in type_arc) 
		return type_arc_angles;

	
	-- Returns the start and end points of an arc.
	function to_arc (
		arc : in type_arc_angles) 
		return type_arc'class;



	
	-- Returns the boundaries of the given arc.
	-- The arc has the given width. 
	-- The boundaries are extended by half the given width.
	function get_boundaries (
		arc			: in type_arc;
		line_width	: in type_distance_positive) 
		return type_boundaries;

	
	-- Returns true if the given point sits on the given arc.
	function on_arc (
		arc			: in type_arc;
		vector		: in type_vector)
		return boolean; 

	
	function on_arc (
		arc			: in type_arc;
		point		: in type_point)
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
		arc		: in type_arc;
		line	: in type_line_vector)
		return type_intersection_of_line_and_circle;

	
	
	-- Computes the end point of an arc.
	function arc_end_point (
		center		: in type_point;
		start_point	: in type_point;	
		angle 		: in type_angle) -- CS: type_angle_positive ?
		return type_point;
	

	-- Moves an arc by the given offset. 
	procedure move_by (
		arc		: in out type_arc;
		offset	: in type_distance_relative);

	
	-- Moves an arc to the given position. 
	procedure move_to (
		arc			: in out type_arc;
		position	: in type_point);

	
	function move_to (
		arc			: in type_arc;
		position	: in type_point)
		return type_arc'class;

	
	-- Mirrors an arc along the given axis.
	procedure mirror (
		arc			: in out type_arc;
		axis		: in type_axis_2d);
	

	-- Rotates an arc about the origin by the given rotation.
	procedure rotate_by (
		arc			: in out type_arc;
		rotation	: in type_rotation);




	-- Splits an arc in 2 or 3 smaller arcs.
	-- The arc will be split by a vertical line that crosses the center.
	-- The resulting arcs have all the same center, radius  and direction as the
	-- given arc.
	--function split_arc (arc_in : in type_arc) 
		--return type_arcs;

	
	
-- CIRCLE
	
	type type_circle_base is abstract tagged record
		center			: type_point;
		radius  		: type_float_internal_positive := 0.0;
		-- CS locked : type_locked;
	end record;

	type type_circle is new type_circle_base with null record;
	-- CS use this type wherever a type_circle is declared unnessecarily.


	-- Returns the center and radius of the given circle as string.
	function to_string (circle : in type_circle) return string;


	function to_radius (
		r : in string)
		return type_float_internal_positive;
	

	function to_diameter (
		d : in string)
		return type_float_internal_positive;
	

	
	-- This type is required when an arc or a circle is to be 
	-- split into smaller arc segments:
	type type_arcs is array (positive range <>) of type_arc;
	
	-- Splits a circle into 2 arcs.
	-- The circle will be split by a vertical line that crosses the center.
	-- The resulting arcs have all the same center and radius as the given circle.
	-- The resulting arcs have both direction CCW.
	-- The left arc runs from the top to the bottom.
	-- The right arc runs from bottom to top:
	function split_circle (
		circle_in : in type_circle) 
		return type_arcs;

	
	-- Converts a circle to an arc.
	-- The start and end point of the arc will be the same:
	--function to_arc (
		--c : in type_circle)
		--return type_arc'class;
	
		
	-- Returns the distance of point to circumfence of circle.
	-- Assumes the point is INSIDE the circle or ON the circumfence of the circle.
	-- The point must not be OUTSIDE the circle !
	function get_distance_to_circumfence (
		circle	: in type_circle;
		point	: in type_point)
		return type_distance_polar;
		
	
	-- Returns the shortest distance from the given point to the
	-- given circle. The point may be inside or outside the circle.
	-- However, the return is the distance to the circumfence of the circle.
	function get_shortest_distance (
		circle	: in type_circle;
		point	: in type_point)
		return type_distance_polar;


	-- CS: INCOMPLETE !!! Returns always zero currently.
	function get_shortest_distance (
		point	: in type_vector;
		circle	: in type_circle)
		return type_float_internal;


	
	-- Moves a circle by the given offset. 
	procedure move_by (
		circle	: in out type_circle;
		offset	: in type_distance_relative);
	

	-- Moves a circle to the given position. 
	procedure move_to (
		circle		: in out type_circle;
		position	: in type_point);

	
	function move_to (
		circle		: in type_circle;
		position	: in type_point)
		return type_circle'class;

	
	-- Mirrors the center of a circle along the given axis.
	procedure mirror (
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
	function on_circle (
		circle		: in type_circle;
		point		: in type_point)
		return boolean;

	
	-- Gives the status (inside/outside) of a point relative to a circle.
	-- If the point lies exactly at the circumfence then the result is "outside".
	function get_point_to_circle_status (
		circle		: in type_circle;
		point		: in type_point)
		return type_point_status;
	

	-- Returns true if the given circle and line do intersect
	-- in some way.
	function intersect (
		circle	: in type_circle'class;
		line	: in type_line'class)
		return boolean;

	
	-- Returns the shortest distance between a circle and a line.
	-- If circle and line do intersect in some way, then the return
	-- is a negative number.
	function get_distance (
		circle	: in type_circle'class;
		line	: in type_line'class)
		return type_float_internal_positive;

	
	-- Returns the shortest distance between a circle and an arc.
	-- If circle and arc do intersect in some way, then the return
	-- is a negative number.
	function get_distance (
		circle	: in type_circle'class;
		arc		: in type_arc'class)
		return type_float_internal_positive;

	
	-- Returns the shortest distance between two circles.
	-- If the circles do intersect in some way, then the return
	-- is a negative number.
	function get_distance (
		circle_1	: in type_circle'class;
		circle_2	: in type_circle'class)
		return type_float_internal_positive;

	
	-- The angle of a tangent to a circle:
	subtype type_tangent_angle_circle is type_angle range -90.0 .. 90.0;

	
	-- Computes the angle of a tangent that touches a circle
	-- at the given point. The center of the circle is assumed to be the origin.
	-- - If the tangent increases in y as it travels from left to right 
	--   then its angle is positive. 
	-- - If the tangent decreases in y, then its angle is negative.
	-- - If it does not change in y, then the tangent runs horizontally and has zero angle.
	-- - If it is vertical, then its angle is 90 degrees.
	function get_tangent_angle (p : in type_vector) 
		return type_tangent_angle_circle;


	
	-- Computes the intersections of a line with a circle.
	-- See more on overloaded function get_intersection (line, arc):
	function get_intersection (
		circle	: in type_circle;
		line	: in type_line_vector)
		return type_intersection_of_line_and_circle;

	
	type type_ordered_line_circle_intersections is record
		-- The start point of the line that intersects the circle.
		-- The start point must be outside the circle.
		start_point	: type_vector;

		-- The point where the line enters and exits the circle:
		entry_point	: type_intersection;
		exit_point	: type_intersection;
	end record;

	
	-- Sorts the intersections of a line with an arc or a circle in the order
	-- as they occur as the line crosses the circle or the arc:
	-- start point of line, entry point, exit point.
	-- The given intersections must contain two intersections (discrimintant status),
	-- otherwise a constraint error will be raised.
	-- If the given intersections have same distance to start point then
	-- a constraint error will be raised.
	function order_intersections (
		-- The start point of the line that intersects the circle.
		-- The start point must be outside the circle.
		start_point		: in type_vector;

		intersections	: in type_intersection_of_line_and_circle)
		return type_ordered_line_circle_intersections;



	

-- PATH FROM POINT TO POINT
	
	-- When creating a path from one point to another use this type.
	-- NOTE: This is general stuff. This does apply to all kinds of lines
	-- from one point to another (nets, documentation, tracks, ...) !
	-- If no bend, then we have just a start and an end point which 
	--  will result in a direct line between the two points.
	-- If bended, then we get an extra point where the bending takes place
	--  which will result in two lines that connect the two points:
	type type_path (bended : type_bended) is record
		start_point, end_point : type_point;
		case bended is
			when NO		=> null; -- no bend
			when YES	=> bend_point : type_point;
		end case;
	end record;

	
	-- Computes a path between two points according to the given bend style:
	function to_path (
		start_point, end_point	: in type_point;
		style					: in type_bend_style)
		return type_path;

	
	-- When a path is being drawn from one point to another
	-- then we speak about a path from start point to end point
	-- and optionally a bending point where the path changes
	-- direction.
	-- This type is required for all kinds of lines (nets, documentation, tracks, ...)
	-- when being drawn via the GUI.
	-- The path being drawn must provide information about the tool it is
	-- being drawn with (mouse, touchpad, keyboard).
	type type_path_live is record
		being_drawn	: boolean := false;

		start_point	: type_point;
		end_point	: type_point;

		bended		: type_bended := NO;
		bend_point	: type_point;
		bend_style	: type_bend_style := HORIZONTAL_THEN_VERTICAL;
		
		tool		: type_tool := MOUSE;
	end record;

	
	-- Switches to the next bend style of the given live path:
	procedure next_bend_style (path : in out type_path_live);


	
-- POSITION:

	-- The position of an object is a composite
	-- of the place (x/y) and the rotation of the object about
	-- its own center:
	type type_position is tagged record
		place 		: type_point := origin;
		rotation	: type_rotation := zero_rotation;
	end record;

	
	function to_string (
		position : in type_position)
		return string;
	
	
	origin_zero_rotation : constant type_position;

	-- A position at the greatest distance in
	-- x and y from the origin:
	far_upper_right_zero_rotation : constant type_position;

	
	function to_position (
		point		: in type_point;
		rotation	: in type_rotation)
		return type_position'class;


	procedure set (
		position	: in out type_position;
		axis 		: in type_axis_2d;
		value		: in type_position_axis);

	
	procedure set (
		position	: in out type_position;
		place		: in type_point);
	
	
	-- Sets the rotation of a position. (position.rotation)
	procedure set ( -- CS rename to set_rotation
		position	: in out type_position;
		rotation	: in type_rotation);


	function get_x (
		position : in type_position)
		return type_distance;
	

	function get_y (
		position : in type_position)
		return type_distance;

	
	-- Returns the rotation of the given position.
	function get_rotation (
		position : in type_position) 
		return type_rotation;
	
	
	-- Changes the rotation of the given position by the given offset.
	-- Preserves x/y. Changes position.rotation only.
	procedure rotate_about_itself (
		position	: in out type_position;
		offset		: in type_rotation);


	
private


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
	
	
	origin_zero_rotation : constant type_position := (others => <>);

	far_upper_right_zero_rotation : constant type_position :=
		(far_upper_right, zero_rotation);


	boundaries_default : constant type_boundaries := (others => <>);
	
end et_geometry_2;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
