------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              GEOMETRY 1                                  --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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

with ada.text_io;				use ada.text_io;
with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;

with ada.numerics;
with ada.numerics.generic_elementary_functions;

with et_primitive_objects;			use et_primitive_objects;
with et_mirroring;					use et_mirroring;
with et_coordinates_formatting;		use et_coordinates_formatting;
with et_object_status;				use et_object_status;


generic
	type type_float is digits <>;
	accuracy : type_float;
	
package et_geometry_1 is

	-- Redefined equality test.
	-- Returns true if the given arguments are equal.
	-- Considers them as equal if their difference is less or equal 
	-- the constant "accuracy":
	function "=" (
		left, right : in type_float) 
		return boolean;

	
	
	function get_average (
		f1, f2 : in type_float)
		return type_float;

	
	-- Converts a mil number (given as a string) to millimeters.	
	function mil_to_distance (mil : in string) return type_float;
	-- CS rename to mil_to_float

	function distance_to_mil (d : in type_float) return string;

	
	function to_string (f : in type_float) return string;

	function to_float (s : in string) return type_float;
	
	subtype type_float_positive is type_float range 0.0 .. type_float'last;


	
	subtype type_angle is type_float range -720.0 .. 720.0; -- CS derive new type ?
	subtype type_angle_positive is type_angle range 0.0 .. 360.0;

	function to_angle (a : in string) return type_angle;
	
	units_per_cycle : constant type_float := 360.0;


	radians_max : constant type_float := - 2.0 * ada.numerics.pi;
	-- CS should be: radians_max : constant type_float := 2.0 * ada.numerics.pi;

	subtype type_radians is type_float range (- radians_max) .. radians_max;

	
	--Converts degrees to radians.
	function to_radians (degrees : in type_angle) return type_float;

	--Converts radians to degrees.
	function to_degrees (radians : in type_float) return type_angle;


	
	
	-- Returns CW if rotation is negative. 
	-- Returns CCW if rotation is positive or zero.
	function get_direction (rotation : in type_angle) 
		return type_direction_of_rotation;


	




	package pac_float_numbers_functions is new 
		ada.numerics.generic_elementary_functions (type_float);
	
	use pac_float_numbers_functions;


	
	package pac_float_numbers_io is new 
		ada.text_io.float_io (type_float);


	

	package pac_float_numbers is new 
		doubly_linked_lists (type_float);



	

	function to_string (
		numbers : in pac_float_numbers.list)
		return string;
	

	
	package pac_float_numbers_sorting is new 
		pac_float_numbers.generic_sorting;


		

	
	
	type type_clean_up_mode is (
		-- This mode converts a list like "4.0 4.0 4.0 6.3 12.0 12.0"
		-- to "4.0 6.3 12.0":
		REDUCE_TO_ONE,
								   
		-- This mode converts a list like "4.0 4.0 4.0 6.3 12.0 12.0 -3.3"
		-- to "6.3 -3.3":
		REMOVE_REDUNDANT);
	

	-- Cleans up a list of float numbers according to the given mode.
	-- Leaves the list unchanged if it is empty or if it contains only
	-- one number:
	procedure clean_up (
		numbers	: in out pac_float_numbers.list;
		mode	: in type_clean_up_mode);				  


	

	-- Returns 1.0 if given x is greater or equal zero.
	-- Returns -1.0 if x less than zero.
	function sgn (x : type_float) return type_float;



	-- Returns the greatest of the given numbers. 
	-- If both are equal then "right" will be returned.
	function get_greatest (
		left, right : in type_float)
		return type_float;

	
	-- Returns the smallest of the given numbers. 
	-- If both are equal then "right" will be returned.
	function get_smallest (
		left, right : in type_float)
		return type_float;



	function to_distance (df : in string)
		return type_float;
	

	
	
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
		x, y : type_float := 0.0;
		-- CS z ?
	end record;

	
	function to_offset (
		x, y : in type_float)
		-- CS z zero as default
		return type_offset;




	-- Inverts the given relative distance by 
	-- multiplying x by -1 and y by -1.
	function invert (
		d : in type_offset) 
		return type_offset;

	
	
	-- Adds two angles.
	-- If the result of adding right to left is greater or equal
	-- 360 degrees then 360 degrees are subtracted from result.
	-- If the result of adding is less or equal -360 degrees then
	-- 360 degrees are added to the result.
	-- So the return of this function is always between -360 
	-- and +360 degrees: 
	function add (
		left, right : in type_angle) 
		return type_angle;


	-- Similar to function add (see above) but 
	-- modifies left. Adds right to left:
	procedure add (
		left	: in out type_angle;
		right	: in type_angle);
	

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
		absolute	: in type_float_positive;
		angle		: in type_angle)
		return type_distance_polar;

	
	-- Sets the absolute component of a polar distance.
	-- If the given absolute is zero, then the angle component
	-- is NOT changed.
	procedure set_absolute (
		distance : in out type_distance_polar;
		absolute : in type_float_positive);

	
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
		return type_float_positive;

	

	-- The quadrants of the coordinate system are numbered counter clockwise.
	-- Quadrant ONE is top right.
	type type_quadrant is (ONE, TWO, THREE, FOUR);

	
	
-- VECTORS

	-- A location vector:
	type type_vector is	record
		x, y, z : type_float := 0.0;
	end record;

	
	-- Returns true if the given two location vectors are equal.
	-- Compares x,y and z:
	function "=" (
		left, right : in type_vector)
		return boolean;

	
	-- Returns the vector that is most left downwards.
	-- Compares first x, then y. Ignores z:
	function get_lower_left (
		left, right : in type_vector)
		return boolean;


	
	null_vector		: constant type_vector := (others => 0.0);
	unity_vector	: constant type_vector := (others => 1.0);

	bottom_left		: constant type_vector := (x => type_float'first, y => type_float'first, z => 0.0);
	bottom_right	: constant type_vector := (x => type_float'last,  y => type_float'first, z => 0.0);
	top_right		: constant type_vector := (x => type_float'last,  y => type_float'last,  z => 0.0);
	top_left		: constant type_vector := (x => type_float'first, y => type_float'last,  z => 0.0);

	
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
		v		: in type_vector;
		show_z	: in boolean := false)
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
		x : in type_float;
		y : in type_float;
		z : in type_float := 0.0)
		return type_vector;
	
	
	function get_x (
		v	: in type_vector)
		return type_float;

	function get_y (
		v	: in type_vector)
		return type_float;

	function get_z (
		v	: in type_vector)
		return type_float;


	function get_absolute (
		vector	: in type_vector)
		return type_float;


	function get_sum_of_squared_components (
		vector	: in type_vector)
		return type_float;

	
	-- Compares two location vectors by their distance to the origin:
	function "<" (
		left, right : in type_vector) 
		return boolean;


	-- Multiplies the components of the given
	-- vector by the scaling factor s:
	function scale (
		v	: in type_vector;
		s	: in type_float)
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
		return type_float;

	
	function mixed_product ( -- german: Spatprodukt
		a, b, c	: in type_vector)
		return type_float;
	-- NOTE: Also called scalar triple product or box product.

								
	-- Divides the components x,y,z of vector a
	-- by one of the components of vector b.
	-- At least one of the components of b 
	-- must not be zero.
	function divide (
		a, b	: in type_vector)
		return type_float;


	-- Returns the total distance between the given two location vectors.
	-- Ignores z of both vectors because this is a 2D world:
	function get_distance_total ( -- CS rename to get_distance_absolute
		v1 : in type_vector;
		v2 : in type_vector)
		return type_float_positive;


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
		distance	: in type_float); -- CS type_float_positive ?


	function move_by (
		v			: in type_vector;
		direction	: in type_angle;
		distance	: in type_float) -- CS type_float_positive ?
		return type_vector;


	-- Mirrors the location vector along the given axis:
	procedure mirror (
		v		: in out type_vector;
		axis	: in type_mirror);


	-- Returns the displacement vector from v1 to v2.
	-- by doing v2.x - v1.x, v2.y - v1.y, v2.z - v1.z
	function get_displacement (
		v1, v2 : in type_vector)
		return type_vector;



	-- Location vectors can be stored in arrays:
	type type_vector_array is array (natural range <>) of type_vector;

	
	-- Location vectors can be stored in simple lists:	
	package pac_vectors is new doubly_linked_lists (type_vector);
	use pac_vectors;


	-- Iterates the vectors. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		vectors	: in pac_vectors.list;
		process	: not null access procedure (position : in pac_vectors.cursor);
		proceed	: not null access boolean);

	
	-- Outputs x/x of the given list of vectors to standard output:
	procedure put_vectors (
		vectors	: in pac_vectors.list);
	

	-- Converts an array of vectors to a list.
	-- The first element of the given array becomes the 
	-- first element in the returned list:
	function to_list (
		vectors : in type_vector_array)
		return pac_vectors.list;


	-- Scales a list of vectors by the given factor:
	procedure scale (
		vectors	: in out pac_vectors.list;
		factor	: in type_float_positive);
	

	-- Moves a list of location vectors by the given offset:
	procedure move_by (
		vectors	: in out pac_vectors.list;
		offset	: in type_offset);				  


	-- Rotates a list of location vectors by the given angle about the origin:
	procedure rotate_by (
		vectors	: in out pac_vectors.list;
		angle	: in type_angle);				  


	-- Mirrors a list of location vectors along the given axis:
	procedure mirror_vectors (
		vectors	: in out pac_vectors.list;
		axis	: in type_mirror);  

	
	-- Appends all location vectors of source to the target:
	procedure splice_vectors (
		v_target : in out pac_vectors.list;
		v_source : in pac_vectors.list);


	-- Removes vectors which are stored multiple times from the given list:
	procedure remove_redundant_vectors (
		vectors : in out pac_vectors.list);


	-- Removes from the given list vectors_1 the vectors given in
	-- vectors_2:
	procedure remove (
		vectors_1 : in out pac_vectors.list; -- primary list
		vectors_2 : in pac_vectors.list); -- vectors to be removed
	

	-- Sorts the list of vectors by the distance of the vectors
	-- to the given reference point. Nearest comes first:
	procedure sort_by_distance (
		vectors		: in out pac_vectors.list;
		reference	: in type_vector);


	-- Returns the location vector that is
	-- furthest left and downwards in the coordinates plane:
	function get_lowest_left (
		vectors		: in pac_vectors.list)
		return type_vector;
	
	-- Returns the location vector that is
	-- furthest right and upwards in the coordinates plane:
	--function get_hightest_right(
		--vectors		: in pac_vectors.list)
		--return type_vector;

	
-- RAY
	
	-- A ray has a fixed starting point, a direction and
	-- no end point:
	type type_ray is record
		A			: type_vector; -- the start point
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

	
	-- Returns a normal vector to a given line vector.
	-- The returned normal vector crosses the given
	-- point on the given line vector:
	function get_normal_vector (
		line	: in type_line_vector;
		point	: in type_vector)
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


	

	-- When finding intersections of two lines this type is required:
	type type_line_vector_intersection (
		status : type_intersection_status_of_two_lines) 
	is record
		case status is
			when EXISTS => 
				intersection : type_vector;
				
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
		return type_line_vector_intersection;



-- BOUNDARIES:
	
	-- The area (a rectangular box around an object)
	-- occupied by the object.
	-- The boundaries are always relative to a certain origin that
	-- sits somewhere inside the rectangular box. 
	type type_boundaries is record
		smallest_x, smallest_y : type_float := type_float'last;
		greatest_x, greatest_y : type_float := type_float'first;
		distance_of_topleft_to_default : type_float := 0.0;
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
		return type_float_positive;

	
	-- Returns the width of the given boundaries by
	-- calculating boundaries.greatest_x - boundaries.smallest_x:
	function get_width (boundaries : in type_boundaries)
		return type_float_positive;


	function get_left (boundaries : in type_boundaries)
		return type_float;

	
	function get_right (boundaries : in type_boundaries)
		return type_float;

	
	-- Returns the bottom of the given boundaries:
	function get_bottom (boundaries : in type_boundaries)
		return type_float;

	
	-- Returns the topof the given boundaries:
	function get_top (boundaries : in type_boundaries)
		return type_float;

	
	
	-- Returns true if the given boundaries overlap or touch each other.
	-- Returns false if boundaries do not overlap:
	function overlap (
		b1 : in type_boundaries;
		b2 : in type_boundaries)
		return boolean;


	--type type_boundaries_intersection (exists : boolean := true) is record
		--case exists is
			--when TRUE => intersection : type_boundaries;
			--when FALSE => null;
		--end case;
	--end record;

	
	-- Returns the intersection area (german: Schnittmenge) of two
	-- boundaries. If the boundaries do not overlap each other
	-- then a constraint error is raised:
	--function get_intersection (
		--b1 : in type_boundaries;
		--b2 : in type_boundaries)
		--return type_boundaries_intersection;

	
	-- Adds two boundaries.
	procedure add (
		b1 : in out type_boundaries;
		b2 : in type_boundaries);



	

-- LINE
	
	type type_line_fine is record
		A, B 	: type_vector; -- start and end point
		status	: type_object_status;
	end record;


	line_length_max : constant type_float_positive := type_float_positive'last;
	line_length_min : constant type_float_positive := 1.0E-10; -- CS refine


	-- Returns the start or the end point of the given line,
	-- depending on which of them is closer to the given point:
	function get_nearest (
		line	: in type_line_fine;
		point	: in type_vector)
		return type_vector;
	
	
	function get_length (
		line : in type_line_fine)
		return type_float_positive;

	
	function to_line_vector (
		line : in type_line_fine)
		return type_line_vector;

	
	function to_string (
		line	: in type_line_fine)
		return string;
	
						   
	function make_line (
		A, B : in type_vector)
		return type_line_fine;


	procedure scale (
		line	: in out type_line_fine;
		factor	: in type_float_positive);
	
		
	-- Moves the start and end points of a 
	-- line by the given offset:
	procedure move_by (
		line	: in out type_line_fine;
		offset	: in type_offset);


	-- Rotates the start and end points of a line 
	-- about the origin (in the z-plane):
	procedure rotate_by (
		line	: in out type_line_fine;
		offset	: in type_angle);


	-- Mirrors a line along the given axis:
	procedure mirror_line (
		line	: in out type_line_fine;
		axis	: in type_mirror);

	
	-- Swaps start and end point of a line:
	function reverse_line (
		line	: in type_line_fine)
		return type_line_fine;
	
	procedure reverse_line (
		line	: in out type_line_fine);

	
	-- Returns the point on the given line
	-- that is right between its start and end point:
	function get_center (
		line : in type_line_fine)
		return type_vector;

	
	-- Returns the direction in degrees of a line.
	-- Example: If a line runs from 0/0 to 1/1 then the result is 45 degree.
	-- Example: If a line runs from -1/-1 to -4/-4 then the result is 225 degree.
	function get_direction (
		line : in type_line_fine)
		return type_angle;


	function is_selected (
		line : in type_line_fine)
		return boolean;

	procedure set_selected (
		line : in out type_line_fine);

	procedure clear_selected (
		line : in out type_line_fine);

	
	function is_proposed (
		line : in type_line_fine)
		return boolean;

	procedure set_proposed (
		line : in out type_line_fine);

	procedure clear_proposed (
		line : in out type_line_fine);

	
	function is_moving (
		line : in type_line_fine)
		return boolean;

	procedure set_moving (
		line : in out type_line_fine);

	procedure clear_moving (
		line : in out type_line_fine);

	
	procedure modify_status (
		line 		: in out type_line_fine;
		operation	: in type_status_operation);

	

	procedure reset_status (
		line : in out type_line_fine);
	
	
	-- Returns true if the given two lines run in
	-- opposide directions. In other words, if the difference
	-- of their directions is 180 degrees.
	-- Example: The result is true if one line runs 
	-- in 45 degrees and the other one is oncoming in 225 degrees:
	function opposide_direction (
		right, left : in type_line_fine)
		return boolean;		
	
	
	-- Returns the boundaries of a line:
	function get_boundaries (
		line : in type_line_fine)
		return type_boundaries;


	-- Moves a line into given direction by given distance:
	procedure move_by (
		line		: in out type_line_fine;
		direction	: in type_angle;
		distance	: in type_float_positive);

	function move_by (
		line		: in type_line_fine;
		direction	: in type_angle;
		distance	: in type_float_positive)
		return type_line_fine;
	
	
	-- Tests whether the given line intersects the given candidate line.
	-- If there is an intersection between start and end point
	-- of the candidate line (start and end point included),
	-- then returns the location vector of the intersection.
	-- If the intersection is before start point or
	-- beyond end point of the given line, return NOT_EXISTENT.
	function get_intersection (
		line_vector : in type_line_vector;
		line		: in type_line_fine;
		debug		: in boolean := false)
		return type_line_vector_intersection;

	
	-- Tests whether the given ray intersects the given candidate line.
	-- If there is an intersection between start and end point
	-- of the candidate line (start and end point included),
	-- then returns the location vector of the intersection.
	-- If the intersection is before start point or
	-- beyond end point of the given line, return NOT_EXISTENT.
	function get_intersection (
		ray			: in type_ray;
		line		: in type_line_fine;
		debug		: in boolean := false)
		return type_line_vector_intersection;

	
	-- Tests whether the given two lines intersect or overlap each other. 
	-- Independendly of start and end points, both lines are regarded as 
	-- infinitely long beyond their start and end points:
	function get_intersection (
		line_1 : in type_line_fine;
		line_2 : in type_line_fine)
		return type_line_vector_intersection;
	
	
	-- Returns true if the given two lines overlap each other.
	-- Independend of start and end points, both lines are regarded as infinitely
	-- long beyond their start and end points:
	function lines_overlap (
		line_1, line_2 : in type_line_fine)
		return boolean;
	
	
	-- CS Find more subprograms on type_line_fine see et_geometry_2.polygons.
	-- Move them there so that they can be used by other callers.
	



	
-- ARC (EXPRESSED BY START AND END POINT):

	arc_direction_default : constant type_direction_of_rotation := CCW;

	type type_arc_fine is private;
	

	function to_arc_fine (
		center		: in type_vector;
		A, B 		: in type_vector; -- start and end point
		direction	: in type_direction_of_rotation)
		return type_arc_fine;
	

	function to_string (
		arc : in type_arc_fine)
		return string;


	function get_center (
		arc : in type_arc_fine)
		return type_vector;
	

	function get_A (
		arc : in type_arc_fine)
		return type_vector;
	

	function get_B (
		arc : in type_arc_fine)
		return type_vector;


	function get_direction (
		arc : in type_arc_fine)
		return type_direction_of_rotation;
	
	
	-- Returns the distance between the start point and the center of the arc.
	function get_radius_start (
		arc : in type_arc_fine) 
		return type_float_positive;
	
	
	-- Returns the distance between the end point and the center of the arc.
	function get_radius_end (
		arc : in type_arc_fine) 
		return type_float_positive;

	
	-- Swaps start and end point of an arc. Reverses the direction of the arc:
	function reverse_arc (
		arc : in type_arc_fine) 
		return type_arc_fine;
	
	procedure reverse_arc (
		arc : in out type_arc_fine);


	-- Changes the direction of an arc to CCW (mathematical sense)
	-- by swapping start and end point. If direction is already CCW
	-- then nothing happens.
	function normalize_arc (
		arc: in type_arc_fine) 
		return type_arc_fine;

	
	-- Returns true if start and end point of arc are equal:
	function zero_length (
		arc : in type_arc_fine) 
		return boolean;


	-- Returns the total span in degree between start and end of an arc:
	function get_span (
		arc	: type_arc_fine)
		return type_angle;


	-- Returns true if the given point sits on the given arc.
	-- It is assumed that the distance of the vector is equal to
	-- the radius of the arc. Otherwise the result is false:
	function on_arc (
		arc			: in type_arc_fine;
		vector		: in type_vector)
		return boolean; 


	

-- ARC (EXPRESSED BY START AND END ANGLES):
	
	type type_arc_angles is private;
	

	function to_arc_angles (
		center		: in type_vector;
		radius		: in type_float_positive;
		angle_start	: in type_angle; -- can be negative
		angle_end	: in type_angle; -- can be negative
		direction	: in type_direction_of_rotation := arc_direction_default)
		return type_arc_angles;
	

	procedure set_center (
		arc		: in out type_arc_angles;
		center	: in type_vector);					 


	procedure set_angle_start (
		arc			: in out type_arc_angles;
		angle_start	: in type_angle);					 


	procedure set_angle_end (
		arc			: in out type_arc_angles;
		angle_end	: in type_angle);

	
	procedure set_radius (
		arc		: in out type_arc_angles;
		radius	: in type_float_positive);					 


	procedure set_direction (
		arc			: in out type_arc_angles;
		direction	: in type_direction_of_rotation);					 
	
	
	function get_center (
		arc		: in type_arc_angles)
		return type_vector;


	function get_angle_start (
		arc		: in type_arc_angles)
		return type_angle;


	function get_angle_end (
		arc		: in type_arc_angles)
		return type_angle;

	
	function get_radius (
		arc		: in type_arc_angles)
		return type_float_positive;


	function get_direction (
		arc		: in type_arc_angles)
		return type_direction_of_rotation;

	
	function to_string (
		arc : in type_arc_angles)
		return string;


	-- Changes the direction of an arc to CCW (mathematical sense)
	-- by swapping start and end angles. Converts all angles to
	-- positive values (example: -90 degree turns to 270 degree):
	function normalize_arc (
		arc: in type_arc_angles) 
		return type_arc_angles;


	-- Rotates the given arc by the given angle
	-- about its center:
	function rotate (
		arc		: in type_arc_angles;
		angle	: in type_angle)
		return type_arc_angles;
	
	
	-- Returns the total span in degree between start and end of an arc.
	-- If full is true, then an arc having the same start and end angle
	-- is regarded as a full circle with a span of 360 degree.
	-- If full is true, then for such an arc the returned span is zero:
	function get_span (
		arc		: in type_arc_angles;
		full	: in boolean := true)
		return type_angle;

	
	-- Moves an arc to the given position. 
	procedure move_to (
		arc			: in out type_arc_fine;
		position	: in type_vector);

	
	function move_to (
		arc			: in type_arc_fine;
		position	: in type_vector)
		return type_arc_fine;
	

	-- Converts a type_arc_fine to a type_arc_fine_angles.
	-- By default an arc having same start and end point
	-- is regarded as a full circle. So the resulting arc will
	-- have a span of 360 degrees.
	-- If argument allow_full_circle is false, then such an arc
	-- will be regarded as having a span of zero degree.
	-- The direction remains unchanged. Use normalize_arc if required:
	function to_arc_angles (
		arc					: in type_arc_fine;
		allow_full_circle	: in boolean := true) 
		return type_arc_angles;

	
	function to_arc (
		arc : in type_arc_angles) 
		return type_arc_fine;
	


	-- If an arc is to be broken down into smaller fragments,
	-- then the outcome is an array of arcs:
	type type_arc_segments is array (positive range <>) of type_arc_fine;


	-- Reverses the order of the given arcs and the
	-- direction of each arc:
	procedure reverse_arc_segments (
		segments : in out type_arc_segments);



	-- Returns the arc segment that is nearest
	-- to the given point:
	function get_nearest (
		segments	: in type_arc_segments;
		point		: in type_vector)
		return positive;
	
	
	-- type type_nearest_arc_segment is record
	-- 	index : positive;
	-- 	distance	: type_float_positive);
	-- end record;


	
	
	-- Splits an arc into the given number of fragments.
	-- In the return, the direction of the fragments
	-- is the same as of the given arc.
	-- The first segment will be at the start point 
	-- of the arc with index 1,
	-- and the last segment will be at the end point of 
	-- the arc with the highest index:
	function split_arc (
		arc		: in type_arc_fine;
		count	: in positive)
		return type_arc_segments;


	

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



	
-- CIRCLE:
	
	type type_circle_fine is private;


	procedure set_center (
		circle	: in out type_circle_fine;
		center	: in type_vector);
	

	procedure set_radius (
		circle	: in out type_circle_fine;
		radius	: in type_float_positive);



	function get_center (
		circle	: in type_circle_fine)
		return type_vector;


	function get_radius (
		circle	: in type_circle_fine)
		return type_float_positive;

	
	
	function to_string (
		circle	: in type_circle_fine)
		return string;
	


	-- Returns true if the given point sits on the given circle circumfence.
	function on_circle (
		circle		: in type_circle_fine;
		point		: in type_vector)
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
				intersection	: type_vector;
				tangent_status	: type_tangent_status;
			
			when TWO_EXIST	=> 
				intersection_1	: type_vector;
				intersection_2	: type_vector;
				
		end case;
	end record;




	type type_ordered_line_circle_intersections is record
		-- The start point of the line that intersects the circle.
		-- The start point must be outside the circle.
		start_point	: type_vector;

		-- The point where the line enters and exits the circle:
		entry_point	: type_vector;
		exit_point	: type_vector;
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


	
	
	-- Computes the intersections of a line with a circle.	
	-- - If there is no intersection then it returns NONE_EXIST.
	-- - If there is only one intersection then the given line is a tangent.
	--   The return status will then be ONE_EXISTS and the 
	--   actual intersection. The tangent status will be TANGENT.
	-- - If there are two intersections then the given line is a SECANT.
	--   The return status will be TWO_EXIST along with the two intersections.
	--   NOTE: There is no information about the order of the two intersections
	--   as the line travels through the arc/circle. Use function order_intersections
	--   to get the intersections ordered.
	function get_intersection (
		circle	: in type_circle_fine;
		line	: in type_line_vector)
		return type_intersection_of_line_and_circle;

	

	-- Returns the distance of point to circumfence of circle.
	-- Assumes the point is INSIDE the circle or ON the circumfence of the circle.
	-- The point must not be OUTSIDE the circle !
	function get_distance_to_circumfence (
		circle	: in type_circle_fine;
		point	: in type_vector)
		return type_distance_polar;

	

	-- Returns the shortest distance from the given point to the
	-- given circle. The point may be inside or outside the circle.
	-- However, the return is the distance to the circumfence of the circle.
	function get_shortest_distance (
		circle	: in type_circle_fine;
		point	: in type_vector)
		return type_distance_polar;


	
	-- Returns the shortest distance between a point and an arc.
	-- If the point is equal the center of the arc, then the return is
	-- the radius of the arc and the angle to the start point of the arc:
	function get_shortest_distance (
		arc		: in type_arc_fine;
		point	: in type_vector)
		return type_distance_polar;


	

	-- Computes the intersections of a line with an arc:
	-- - If there is no intersection then it returns NONE_EXIST.
	-- - If there is only one intersection then the given line is a tangent.
	--   The return status will then be ONE_EXISTS and the 
	--   actual intersection. The tangent status will be TANGENT.
	-- - If there are two intersections then the given line is a SECANT.
	--   The return status will be TWO_EXIST along with the two intersections.
	--   NOTE: There is no information about the order of the two intersections
	--   as the line travels through the arc/circle. Use function order_intersections
	--   to get the intersections ordered.
	function get_intersection (
		arc		: in type_arc_fine;
		line	: in type_line_vector)
		return type_intersection_of_line_and_circle;

	
	
-- DISTANCE LOCATION VECTOR TO LINE

	-- Computes the distance between a location vector and a line.
	-- This computation does not care about end or start point of the line.
	-- It assumes an indefinite long line without start or end point.
	function get_distance (
		line	: in type_line_fine;
		vector	: in type_vector)
		return type_float_positive;
	

	-- Returns the shortest distance from a given location vector
	-- to a line. This is about ANY direction from the location vector
	-- to the line:
	function get_shortest_distance (
		vector	: in type_vector;
		line	: in type_line_fine)
		return type_float_positive;


	-- Returns true if the given location vector is on
	-- the given line:
	function on_line (
		vector	: in type_vector;
		line	: in type_line_fine;
		debug	: in boolean := false)
		return boolean;
	
	
	type type_distance_point_line is record -- CS make private ?
		sits_on_start	: boolean := false; -- intersection vector !
		sits_on_end		: boolean := false; -- intersection vector !
		
		out_of_range	: boolean := true;

		-- A virtual line runs from the given point perpendicular
		-- to the given line. This is where the virtual line intersects
		-- the given line:
		intersection	: type_vector := null_vector;
		distance		: type_float_positive := 0.0;
	end record;



	type type_line_range is (
		BETWEEN_END_POINTS,	-- start and end point excluded
		WITH_END_POINTS,	-- start and end point included
		BEYOND_END_POINTS	-- indefinite long line assumed. extends beyond both start and end point into infinity
		);


	
	-- Computes the shortest distance (perpendicular) of a
	-- point to a line. 
	-- Raises exception if the given line has zero length.
	-- CS insufficient ! More details !!! especially on the out_of_range flag
	function get_distance (
		vector		: in type_vector; 
		line		: in type_line_fine;
		line_range	: in type_line_range;
		debug		: in boolean := false)
		return type_distance_point_line;

	
	
	-- These functions return the components of the given type_distance_point_line:
	function out_of_range (d : in type_distance_point_line) return boolean;

	
	function get_distance (d : in type_distance_point_line) 
		return type_float;

	
	function get_intersection (d : in type_distance_point_line) 
		return type_vector;

	
	function on_A (d : in type_distance_point_line) return boolean;
	function on_B (d : in type_distance_point_line) return boolean;
	
	
	

	
	
private
	
	type type_distance_polar is record
		absolute: type_float_positive := 0.0;
		angle	: type_angle := 0.0;
	end record;


	type type_circle_fine is record
		center	: type_vector;
		radius	: type_float_positive;
	end record;

	
	type type_arc_fine is record
		center		: type_vector;
		A, B		: type_vector; -- start and end point
		direction	: type_direction_of_rotation := CW; 
		-- CS should be arc_direction_default
	end record;


	type type_arc_angles is record
		center		: type_vector;
		radius		: type_float_positive;
		angle_start	: type_angle; -- can be negative
		angle_end	: type_angle; -- can be negative
		direction	: type_direction_of_rotation := arc_direction_default;
	end record;

	
end et_geometry_1;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
