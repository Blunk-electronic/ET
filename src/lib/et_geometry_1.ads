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
	type type_float_internal is digits <>; -- CS rename to type_float ?

package et_geometry_1 is

	accuracy : constant type_float_internal := 1.0E-16;
	-- CS must be a package parameter

	function equal (left, right : in type_float_internal) return boolean renames "=";
	--function less_than (left, right : in type_float_internal) return boolean renames "<";
	--function greater_than (left, right : in type_float_internal) return boolean renames ">";	
	
	--function is_zero (f : in type_float_internal) return boolean;
	--function is_not_zero (f : in type_float_internal) return boolean;
	
	-- Returns true if the given arguments are equal.
	-- Considers them as equal if their difference is less or equal 
	-- the constant "accuracy":
	function "=" (left, right : in type_float_internal) return boolean;
	--function "<" (left, right : in type_float_internal) return boolean;
	--function ">" (left, right : in type_float_internal) return boolean;

	
	function get_average (
		f1, f2 : in type_float_internal)
		return type_float_internal;

	
	-- Converts a mil number (given as a string) to millimeters.	
	function mil_to_distance (mil : in string) return type_float_internal;
	-- CS rename to mil_to_float

	function distance_to_mil (d : in type_float_internal) return string;

	
	function to_string (f : in type_float_internal) return string;

	function to_float (s : in string) return type_float_internal;
	
	subtype type_float_internal_positive is type_float_internal range 0.0 .. type_float_internal'last;


	
	subtype type_angle is type_float_internal range -720.0 .. 720.0;
	subtype type_angle_positive is type_angle range 0.0 .. (360.0 - type_angle'small);

	function to_angle (a : in string) return type_angle;
	
	units_per_cycle : constant type_float_internal := 360.0;


	radians_max : constant type_float_internal := - 2.0 * ada.numerics.pi;
	-- CS should be: radians_max : constant type_float_internal := 2.0 * ada.numerics.pi;

	subtype type_radians is type_float_internal range (- radians_max) .. radians_max;

	
	--Converts degrees to radians.
	function to_radians (degrees : in type_angle) return type_float_internal;

	--Converts radians to degrees.
	function to_degrees (radians : in type_float_internal) return type_angle;


	
	
	-- Returns CW if rotation is negative. 
	-- Returns CCW if rotation is positive or zero.
	function get_direction (rotation : in type_angle) 
		return type_direction_of_rotation;


	

	


	-- The number of decimal places when rounding or type_float_internal
	-- is required:
	--subtype type_rounding_accuracy is positive 
		--range 1 .. type_float_internal'digits;
	
	--procedure round (
		--f : in out type_float_internal;	-- the number to be rounded
		--a : in type_rounding_accuracy);	-- the accuracy, the number of decimal places

	--function round (
		--f : in type_float_internal;
		--a : in type_rounding_accuracy)
		--return type_float_internal;

	


	package pac_functions_distance is new 
		ada.numerics.generic_elementary_functions (type_float_internal);
	use pac_functions_distance;

	package pac_distance_io is new ada.text_io.float_io (type_float_internal);

	
	



	-- Returns 1.0 if given x is greater or equal zero.
	-- Returns -1.0 if x less than zero.
	function sgn (x : type_float_internal) return type_float_internal;



	-- Returns the greatest of the given numbers. 
	-- If both are equal then "right" will be returned.
	function get_greatest (
		left, right : in type_float_internal)
		return type_float_internal;

	
	-- Returns the smallest of the given numbers. 
	-- If both are equal then "right" will be returned.
	function get_smallest (
		left, right : in type_float_internal)
		return type_float_internal;



	function to_distance (df : in string)
		return type_float_internal;
	
	--function to_distance (f : in type_float_internal)
		--return type_distance;

	--function to_rotation (f : in type_float_internal)
		--return type_rotation;

	
	
	--type type_rounding_mode is (
		--DOWN,			-- down to nearest multiple of type_distance_coarse'small
		--BANKERS_RULE,	-- 0..4 down, 5..9 up				
		--UP);			-- up to nearest multiple of type_distance_coarse'small

	--rounding_mode_default : constant type_rounding_mode := BANKERS_RULE;

		
	---- Rounds the given distance to the nearest multiple of 
	---- type_distance_coarse'small.
	--function round (
		--d_fine	: in type_distance;
		--mode	: in type_rounding_mode := rounding_mode_default) 
		--return type_distance_coarse;
	


	
	---- Converts an angle like -90.0 degrees to 270 degrees.
	---- Converts an angle like -1.0 degrees to 359 degrees.
	---- Leaves a positive angle like 135 degree as it is and returns it.
	--function to_positive_rotation (
		--rotation	: in type_rotation)
		--return type_rotation_positive;

	--subtype type_tangent_angle is type_rotation range 0.0 .. 180.0 - type_rotation'small;
	
	--subtype type_rotation_0_90 is type_rotation range 0.0 .. 90.0;
	




	type type_offset is record
		x, y : type_float_internal := 0.0;
		-- CS z ?
	end record;

	
	function to_offset (
		x, y : in type_float_internal)
		-- CS z zero as default
		return type_offset;




	-- Inverts the given relative distance by 
	-- multiplying x by -1 and y by -1.
	function invert (
		d : in type_offset) 
		return type_offset;

	
	
	-- Adds two angles.
	-- If result greater 360 degree then 360 degree is subtracted from result.
	-- If result less than 360 degree then 360 degree is added to the result.
	function add (
		left, right : in type_angle) 
		return type_angle;


	-- Converts an angle like -90.0 degrees to 270 degrees.
	-- Converts an angle like -1.0 degrees to 359 degrees.
	-- Leaves a positive angle like 135 degree as it is and returns it.
	function to_angle_positive (
		rotation : in type_angle)
		return type_angle_positive;

	
	
	type type_distance_polar is private;

	
	function to_string (
		distance : in type_distance_polar)
		return string;

	
	function to_polar (
		absolute	: in type_float_internal_positive;
		angle		: in type_angle)
		return type_distance_polar;

	
	-- Sets the absolute component of a polar distance.
	-- If the given absolute is zero, then the angle component
	-- is NOT changed.
	procedure set_absolute (
		distance : in out type_distance_polar;
		absolute : in type_float_internal_positive);

	
	procedure set_angle (
		distance : in out type_distance_polar;
		angle    : in type_angle);
	

	-- Adds 180 degree to the angle of the given polar distance.
	-- Example: changes 45 degrees to 225 degrees:
	procedure reverse_angle (
		distance : in out type_distance_polar);


	
	-- Returns the angle of the given polar distance:
	function get_angle (
		distance : in type_distance_polar)
		return type_angle;

	
	-- Returns the absolute of the given polar distance:
	function get_absolute (
		distance : in type_distance_polar)
		return type_float_internal_positive;

	

	-- The quadrants of the coordinate system are numbered counter clockwise.
	-- Quadrant ONE is top right.
	type type_quadrant is (ONE, TWO, THREE, FOUR);

	
	
-- VECTORS

	type type_vector is	record
		x, y, z : type_float_internal := 0.0;
	end record;

	
	-- Returns true if the given two location vectors are equal:
	function "=" (
		left, right : in type_vector)
		return boolean;

	
	null_vector		: constant type_vector := (others => 0.0);
	unity_vector	: constant type_vector := (others => 1.0);


	function get_average (
		v1, v2 : in type_vector)
		return type_vector;
	
	
	function get_offset (
		v1, v2 : in type_vector)
		return type_offset;

	
	function to_offset (
		v : in type_vector)
		return type_offset;

	
	function to_string (
		v	: in type_vector)
		return string;

	
	-- Returns the quadrant the point is located in.
	-- ONE  : point is on the right of the y-axis or on it AND above the x-axis or on it
	-- TWO  : point is left of the y-axis AND above the x-axis or on top of it
	-- THREE: point is left of the y-axis AND below the x-axis
	-- FOUR : point is right of the y-axis or on top of it AND below the x-axis
	function get_quadrant ( -- CS move to et_geometry_1
		point : in type_vector) 
		return type_quadrant;

	
	function set (
		x : in type_float_internal;
		y : in type_float_internal;
		z : in type_float_internal := 0.0)
		return type_vector;
	
	
	function get_x (
		v	: in type_vector)
		return type_float_internal;

	function get_y (
		v	: in type_vector)
		return type_float_internal;

	function get_z (
		v	: in type_vector)
		return type_float_internal;


	function get_absolute (
		vector	: in type_vector)
		return type_float_internal;


	function get_sum_of_squared_components (
		vector	: in type_vector)
		return type_float_internal;

	
	-- Compares two location vectors by their distance to the origin:
	function "<" (
		left, right : in type_vector) 
		return boolean;

	
	function scale (
		v	: in type_vector;
		s	: in type_float_internal)
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
		return type_float_internal;

	
	function mixed_product ( -- german: Spatprodukt
		a, b, c	: in type_vector)
		return type_float_internal;
	-- NOTE: Also called scalar triple product or box product.

								
	-- Divides the components x,y,z of vector a
	-- by one of the components of vector b.
	-- At least one of the components of b 
	-- must not be zero.
	function divide (
		a, b	: in type_vector)
		return type_float_internal;


	-- Returns the total distance between the given two location vectors.
	-- Ignores z of both vectors because this is a 2D world:
	function get_distance_total ( -- CS rename to get_distance_absolute
		v1 : in type_vector;
		v2 : in type_vector)
		return type_float_internal_positive;


	-- Returns the distance of location vector_one to vector_two.	
	-- Subtracts vector_one.x from vector_two.x and vector_one.y from vector_two.y
	-- returns	total := sqrt ((vector_two.x - vector_one.x)**2 + (vector_two.y - vector_one.y)**2)
	--			angle := arctan ((vector_two.y - vector_one.y) / (vector_two.x - vector_one.x)
	-- Ignores z of both vectors because this is a 2D world.
	-- NOTE 1: The angle ranges from -180 to 180 degrees.
	-- NOTE 2: If the total distance between the location vectors is zero then
	--         the returned angle is zero. So it is wise to test the two vectors
	--         for equality befor calling this function.
	function get_distance (
		v1, v2 : in type_vector)
		return type_distance_polar;


	-- Rotates the given location vector BY the given angle about the origin.
	-- Changes vector.x and vector.y only.
	procedure rotate_by (
		vector		: in out type_vector;
		rotation	: in type_angle;
		debug		: in boolean := false);


	-- Moves the given vector by given offset.
	-- Leaves z unchanged.
	function move_by (
		v		: in type_vector;
		offset	: in type_offset)
		return type_vector;


	procedure move_by (
		v		: in out type_vector;
		offset	: in type_offset);


	
	-- Moves a location vector into given direction
	-- by given distance. Leaves z unchanged.
	procedure move_by (
		v			: in out type_vector;
		direction	: in type_angle;
		distance	: in type_float_internal); -- CS type_float_internal_positive ?


	function move_by (
		v			: in type_vector;
		direction	: in type_angle;
		distance	: in type_float_internal) -- CS type_float_internal_positive ?
		return type_vector;


	-- Mirrors the location vector along the given axis:
	procedure mirror (
		v		: in out type_vector;
		axis	: in type_axis_2d);


	-- Returns the displacement vector from v1 to v2.
	-- by doing v2.x - v1.x, v2.y - v1.y, v2.z - v1.z
	function get_displacement (
		v1, v2 : in type_vector)
		return type_vector;


	
	package pac_vectors is new doubly_linked_lists (type_vector);


	-- Appends all location vectors of source to the target:
	procedure splice_vectors (
		v_target : in out pac_vectors.list;
		v_source : in pac_vectors.list);


	-- Removes vectors which are stored multiple times from the given list:
	procedure remove_redundant_vectors (
		vectors : in out pac_vectors.list);

	
-- RAY
	
	-- A ray has a fixed starting point, a direction and
	-- no end point:
	type type_ray is record
		start_point	: type_vector;
		direction	: type_angle;
	end record;

	
	-- Returns the location vector of the start point of a ray:
	function start_vector (ray : in type_ray) 
		return type_vector;

	-- Returns the direction vector of a ray:
	function direction_vector (ray : in type_ray) 
		return type_vector;



-- LINE VECTOR

	type type_line_vector is record
		v_start		: type_vector; -- start vector of line
		v_direction	: type_vector; -- direction vector of line
	end record;

	
	function to_string (
		lv : in type_line_vector)
		return string;

	
	-- Moves a line vector by the given offset.
	-- Changes only the start vector. Leaves the
	-- direction vector unchanged.
	function move_by (
		lv		: in type_line_vector;
		offset	: in type_offset)
		return type_line_vector;
	

	-- Returns the direction of travel of the given line in degrees:
	function get_angle (
		line	: in type_line_vector)
		return type_angle;


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
		angle	: in type_angle)
		return type_line_vector;


	

-- INTERSECTIONS

	type type_intersection_status_of_two_lines is (
		-- The two lines do NOT intersect each other:
		NOT_EXISTENT,

		-- The two lines DO intersect each other:
		EXISTS,

		-- The two lines are parallel to each other.
		-- They overlap each other. 
		-- One line lies exactly on the other. 
		-- The distance between them is zero everywhere:
		OVERLAP);

	
	-- In general an intersection is composed of a 
	-- location vector, where the two objects meet,
	-- and and the angle at which they intersect:
	type type_intersection is record
		vector	: type_vector; -- location vector
		angle	: type_angle := 0.0;
	end record;

	
	function to_string (intersection : in type_intersection)
		return string;
	

	-- When finding intersections of two lines this type is required:
	type type_intersection_of_two_lines (
		status : type_intersection_status_of_two_lines) 
	is record
		case status is
			when EXISTS => 
				intersection : type_intersection;
				
			when NOT_EXISTENT | OVERLAP => 
				null;
		end case;
	end record;

	

	function get_angle_of_itersection (
		line_1, line_2	: in type_line_vector)
		return type_angle;

	
	-- Tests whether the given two lines intersect.
	-- If there is an intersection, returns the location vector.
	function get_intersection (
		line_1, line_2	: in type_line_vector)
		return type_intersection_of_two_lines;



-- BOUNDARIES:
	
	-- The area (a rectangular box around an object)
	-- occupied by the object.
	-- The boundaries are always relative to a certain origin that
	-- sits somewhere inside the rectangular box. 
	type type_boundaries is record
		smallest_x, smallest_y : type_float_internal := type_float_internal'last;
		greatest_x, greatest_y : type_float_internal := type_float_internal'first;
		distance_of_topleft_to_default : type_float_internal := 0.0;
	end record;

	function to_string (boundaries : in type_boundaries) return string;
	
	--boundaries_default : constant type_boundaries := (others => <>);

	-- Unites the boundaries "right" with boundaries "left". "left" is updated.
	procedure union (
		left	: in out type_boundaries;
		right	: in type_boundaries);
	

	-- Returns the height of the given boundaries by
	-- calculating boundaries.greatest_y - boundaries.smallest_y:
	function get_height (boundaries : in type_boundaries)
		return type_float_internal_positive;

	
	-- Returns the width of the given boundaries by
	-- calculating boundaries.greatest_x - boundaries.smallest_x:
	function get_width (boundaries : in type_boundaries)
		return type_float_internal_positive;

	
	-- Returns true if the given boundaries intersect each other.
	-- Returns false if boundaries do not intersect and if they
	-- touch each other:
	function intersect (
		b1 : in type_boundaries;
		b2 : in type_boundaries)
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
		b1 : in type_boundaries;
		b2 : in type_boundaries)
		return type_boundaries_intersection;

	
	-- Adds two boundaries.
	procedure add (
		b1 : in out type_boundaries;
		b2 : in type_boundaries);



	

-- LINE
	
	type type_line is record
		start_point	: type_vector;
		end_point	: type_vector;
	end record;


	line_length_max : constant type_float_internal_positive := type_float_internal_positive'last;
	line_length_min : constant type_float_internal_positive := 1.0E-10; -- CS refine

	
	function get_length (
		line : in type_line)
		return type_float_internal_positive;

	
	function to_line_vector (
		line : in type_line)
		return type_line_vector;

	
	function to_string (
		line	: in type_line)
		return string;
	
						   
	function make_line (
		start_point, end_point : in type_vector)
		return type_line;
	
		
	-- Moves the start and end points of a 
	-- line by the given offset:
	procedure move_by (
		line	: in out type_line;
		offset	: in type_offset);


	-- Rotates the start and end points of a line 
	-- about the origin (in the z-plane):
	procedure rotate_by (
		line	: in out type_line;
		offset	: in type_angle);


	-- Mirrors a line along the given axis:
	procedure mirror ( -- CS remove
		line	: in out type_line;
		axis	: in type_axis_2d);

	procedure mirror_line (
		line	: in out type_line;
		axis	: in type_axis_2d);

	
	-- Swaps start and end point of a line:
	function reverse_line (
		line	: in type_line)
		return type_line;
	

	-- Returns the point on the given line
	-- that is right between its start and end point:
	function get_center (
		line : in type_line)
		return type_vector;

	
	-- Returns the direction in degrees of a line.
	-- Example: If a line runs from 0/0 to 1/1 then the result is 45 degree.
	-- Example: If a line runs from -1/-1 to -4/-4 then the result is 225 degree.
	function get_direction (
		line : in type_line)
		return type_angle;
	

	-- Returns the boundaries of a line:
	function get_boundaries (
		line : in type_line)
		return type_boundaries;


	-- Moves a line into given direction by given distance:
	procedure move_by (
		line		: in out type_line;
		direction	: in type_angle;
		distance	: in type_float_internal_positive);


	-- Tests whether the given line intersects the given candidate line.
	-- If there is an intersection between start and end point
	-- of the candidate line (start and end point included),
	-- then returns the location vector of the intersection.
	-- If the intersection is before start point or
	-- beyond end point of the given line, return NOT_EXISTENT.
	-- NOTE: The angle of intersection is measured between the 
	-- start points of the two lines. It is always positive.
	function get_intersection (
		line_vector : in type_line_vector;
		line : in type_line)
		return type_intersection_of_two_lines;

	
	-- Tests whether the given two lines intersect or overlap each other. 
	-- Independend of start and end points, both lines are regarded as 
	-- infinitely long beyond their start and end points:
	function get_intersection (
		line_1 : in type_line;
		line_2 : in type_line)
		return type_intersection_of_two_lines;
	
	
	-- Returns true if the given two lines overlap each other.
	-- Independend of start and end points, both lines are regarded as infinitely
	-- long beyond their start and end points:
	function lines_overlap (
		line_1, line_2 : in type_line)
		return boolean;
	
	
	-- CS Find more subprograms on type_line see et_geometry_2.polygons.
	-- Move them there so that they can be used by other callers.
	

-- ARC
	
	type type_arc is record
		center		: type_vector;
		start_point	: type_vector;
		end_point	: type_vector;
		direction	: type_direction_of_rotation := CW;
	end record;


	function to_string (
		arc : in type_arc)
		return string;


	-- Returns the distance between the start point and the center of the arc.
	function get_radius_start (
		arc : in type_arc) 
		return type_float_internal_positive;
	
	
	-- Returns the distance between the end point and the center of the arc.
	function get_radius_end (
		arc : in type_arc) 
		return type_float_internal_positive;

	
	-- Swaps start and end point of an arc. Reverses the direction of the arc:
	function reverse_arc (
		arc : in type_arc) 
		return type_arc;
	
	procedure reverse_arc (
		arc : in out type_arc);


	-- Changes the direction of an arc to CCW (mathematical sense)
	-- by swapping start and end point. If direction is already CCW
	-- then nothing happens.
	function normalize_arc (
		arc: in type_arc) 
		return type_arc;

	
	-- Returns true if start and end point of arc are equal:
	function zero_length (
		arc : in type_arc) 
		return boolean;


	-- Returns the total span in degree between start and end of an arc:
	function get_span (
		arc	: type_arc)
		return type_angle;

	
	type type_arc_angles is record -- CS should be private ?
		center		: type_vector;
		radius		: type_float_internal_positive;
		angle_start	: type_angle; -- CS type_angle_positive ?
		angle_end	: type_angle; -- CS type_angle_positive ?
		direction	: type_direction_of_rotation := CW; -- CS default should be mathematical sense CCW ?
	end record;


	-- Returns the total span in degree between start and end of an arc:
	function get_span (
		arc	: type_arc_angles)
		return type_angle;

	
	-- Moves an arc to the given position. 
	procedure move_to (
		arc			: in out type_arc;
		position	: in type_vector);
	
	
	function to_arc_angles (
		arc : in type_arc) 
		return type_arc_angles;

	
	function to_arc (
		arc : in type_arc_angles) 
		return type_arc;
	


-- DISTANCE LOCATION VECTOR TO LINE

	-- Computes the distance between a location vector and a line.
	-- This computation does not care about end or start point of the line.
	-- It assumes an indefinite long line without start or end point.
	function get_distance (
		line	: in type_line;
		vector	: in type_vector)
		return type_float_internal;
	

	-- Returns the shortest distance from a given location vector
	-- to a line. This is about ANY direction from the location vector
	-- to the line:
	function get_shortest_distance (
		vector	: in type_vector;
		line	: in type_line)
		return type_float_internal;


	-- Returns true if the given location vector is on
	-- the given line:
	function on_line (
		vector	: in type_vector;
		line	: in type_line)
		return boolean;
	
	
	type type_distance_point_line is record -- CS make private ?
		sits_on_start	: boolean := false;
		sits_on_end		: boolean := false;
		out_of_range	: boolean := true;

		-- A virtual line runs from the given point perpendicular
		-- to the given line. This is where the virtual line intersects
		-- the given line:
		intersection	: type_vector := null_vector;
		distance		: type_float_internal_positive := 0.0;
	end record;


	-- Computes the shortest distance (perpendicular) of a
	-- point to a line. 		
	function get_distance (
		vector		: in type_vector; 
		line		: in type_line;
		line_range	: in type_line_range)
		return type_distance_point_line;

	
	
	-- These functions return the components of the given type_distance_point_line:
	function out_of_range (d : in type_distance_point_line) return boolean;

	
	function get_distance (d : in type_distance_point_line) 
		return type_float_internal;

	
	function get_intersection (d : in type_distance_point_line) 
		return type_vector;

	
	function on_start_point (d : in type_distance_point_line) return boolean;
	function on_end_point (d : in type_distance_point_line) return boolean;
	

	
	
private
	
	type type_distance_polar is record
		absolute: type_float_internal_positive := 0.0;
		angle	: type_angle := 0.0;
	end record;



	
end et_geometry_1;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
