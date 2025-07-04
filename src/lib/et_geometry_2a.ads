	------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             GEOMETRY 2a                                  --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.containers; 				use ada.containers;
with ada.containers.doubly_linked_lists;


with ada.numerics;
with ada.numerics.generic_elementary_functions;

with et_axes;						use et_axes;
with et_primitive_objects;			use et_primitive_objects;
with et_geometry_1;

with et_string_processing;			use et_string_processing;
with et_logging;					use et_logging;
with et_object_status;				use et_object_status;
with et_mirroring;					use et_mirroring;
with et_coordinates_formatting;		use et_coordinates_formatting;


generic
	with package pac_geometry_1 is new et_geometry_1 (<>);
	
	type type_distance is delta <> digits <>;

	axis_min, axis_max : type_distance;

	type type_rotation is delta <> digits <>;
	
	
package et_geometry_2a is

	use pac_geometry_1;
	use pac_float_numbers_functions;

	zero 		: constant type_distance := 0.0;
	far_left	: constant type_distance := axis_min;
	far_right	: constant type_distance := axis_max;

	

	function get_info (editor: in string)
		return string;

	



	-- CS use prefix ?
	type type_coordinates is (RELATIVE, ABSOLUTE);

	function to_string (coordinates : in type_coordinates) return string;
	function to_coordinates (coordinates : in string) return type_coordinates;

	

		
-- DISTANCE:

	-- For collecting and sorting distances:
	package pac_distances is new doubly_linked_lists (type_distance);
	package pac_distances_sorting is new pac_distances.generic_sorting;


	-- Tests whether the given value is in the given range.
	-- 1. If include_limits is true then:
	--    Returns true if the given value is greater or equal than lower 
	--    AND if less or equal upper.
	-- 2. If include_limits is false then:
	--    Returns true if the given value is greater than lower 
	--    AND if less than upper.
	function in_range (
		lower, upper	: in type_distance;
		value			: in type_distance;
		include_limits	: in boolean)
		return boolean;

	
	
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


	
	-- Limits a distance to a given maximum.
	-- Examples: 
	-- 1. distance = 100.0, maximum = 80.0 -> distance becomes 80.0
	-- 2. distance =  70.0, maximum = 80.0 -> distance remains 70.0
	procedure limit_to_maximum (
		distance	: in out type_distance;
		maximum		: in type_distance);

	-- Limits a distance to a given minimum.
	-- Examples: 
	-- 1. distance =  80.0, minimum = 70.0 -> distance remains 80.0
	-- 2. distance =  60.0, minimum = 70.0 -> distance becomes 70.0
	procedure limit_to_minimum (
		distance	: in out type_distance;
		minimum		: in type_distance);


	

	-- Converts a mil number (given as a string) to millimeters.	
	function mil_to_distance (
		mil : in string) 
		return type_distance;


	
	function distance_to_mil (
		distance : in type_distance) 
		return string;



	
	-- Use this type for distances, lengths, ...
	-- Because those things require positive numbers:
	subtype type_distance_positive is type_distance
		range 0.0 .. type_distance'last;


	-- For collecting and sorting distances:
	package pac_distances_positive is new doubly_linked_lists (type_distance_positive);
	package pac_distances_positive_sorting is new pac_distances_positive.generic_sorting;


	-- Returns the greatest distance from a list of positive distances:
	function get_greatest (
		distances	: in pac_distances_positive.list)
		return type_distance_positive;



		
	
	-- This function returns the given distance as string:	
	function to_string (
		distance : in type_distance)
		return string;


	-- The position along an axis:
	subtype type_position_axis is type_distance 
		range axis_min .. axis_max;


	-- Converts a float number to type_distance by rounding
	-- according to the bankers rule:
	-- Use it !!!!!
	function to_distance (f : in type_float)
		return type_distance;


	function to_distance (dd : in string) 
		return type_distance;		



	function clip_distance (d : in type_distance)
		return type_position_axis;

	
	procedure clip_distance (d : in out type_distance);


	

-- ROTATION / ANGLE:
	
	zero_rotation : constant type_rotation := 0.0;

	
	-- Converts the given rotation/angle to a string:
	function to_string (
		rotation : in type_rotation)
		return string;


	function to_rotation (
		rotation : in string) 
		return type_rotation;


	-- Converts a float number to type_rotation by rounding
	-- according to the bankers rule:
	-- Use it !!!!!
	function to_rotation (
		f : in type_float)
		return type_rotation;


	function to_angle (
		a : in type_rotation)
		return type_float;

	
	
	-- Adds two angles.
	-- If result greater 360 degree then 360 degree is subtracted from result.
	-- If result less than 360 degree then 360 degree is added to the result.
	function add (
		left, right : in type_rotation) 
		return type_rotation;


	-- See comments of function add above:
	procedure add (
		rotation	: in out type_rotation;
		offset		: in type_rotation);
	


	
	
-- POINT / POSITION / LOCATION / LOCATION VECTOR / DISTANCE VECTOR:
	
	type type_vector_model is record
		x, y : type_distance := 0.0;
	end record;

	
	-- The origin is a small cross at model position (0;0).
	origin : constant type_vector_model := (0.0, 0.0);

	far_upper_left	: constant type_vector_model;
	far_upper_right	: constant type_vector_model;
	far_lower_left	: constant type_vector_model;
	far_lower_right	: constant type_vector_model;
	

	-- This function returns the given vector
	-- as string formatted as follows:
	-- FORMAT_1 : "x/y 4.5 / 5.6" 
	-- FORMAT_2 : x 4.5 y 5.6
	-- FORMAT_3 : 4.5 5.6
	function to_string (
		v 		: in type_vector_model;
		format	: in type_output_format := FORMAT_1)
		return string;
	-- CS apply this scheme to other to_string functions !


	function to_vector_model (
		x, y : in type_distance)
		return type_vector_model;
	

	-- Moves the given point to the origin (0/0).
	procedure reset (
		point : in out type_vector_model);
	

	-- This function inverts a vector by multiplying
	-- its components by -1:
	function invert (
		point	: in type_vector_model)
		return type_vector_model;
	

	-- Inverts the point on the given axis.
	function invert (
		point	: in type_vector_model;
		axis	: in type_mirror)
		return type_vector_model;


	-- Adds the given two location vectors:
	function "+" (
		left, right : in type_vector_model)
		return type_vector_model;
	

	procedure add (
		v1 : in out type_vector_model;
		v2 : in type_vector_model);
	

	-- Computes left - right:
	function "-" (
		left, right : in type_vector_model)
		return type_vector_model;

	
	
	-- Moves a location vector by the given offset:
	procedure move_by (
		point	: in out type_vector_model;
		offset	: in type_vector_model);


	-- Rotates the given point BY the given angle about the origin.
	-- Changes point.x and point.y only.
	procedure rotate_by (
		point		: in out type_vector_model;
		rotation	: in type_rotation);


	-- Rotates the given point TO the given angle about the origin.
	-- Changes point.x and point.y only.
	procedure rotate_to (
		point		: in out type_vector_model;
		rotation	: in type_rotation);
	

	

	-- Returns the distance of point_two to point_one.	
	-- Subtracts point_one.x from point_two.x and point_one.y from point_two.y
	-- returns	total := sqrt ((point_two.x - point_one.x)**2 + (point_two.y - point_one.y)**2)
	--			angle := arctan ((point_two.y - point_one.y) / (point_two.x - point_one.x)
	-- NOTE 1: The angle ranges from -180 to 180 degrees.
	-- NOTE 2: If the total distance between the points is zero then
	--         the returned angle is zero. So it is wise to test the two points
	--         for equality befor calling this function.
	function get_distance (
		point_one, point_two : in type_vector_model)
		return type_distance_polar;


	function get_distance (
		point	: in type_vector_model;
		vector	: in type_vector)
		return type_distance_polar;
	
	
	-- Returns the absolute distance between the given
	-- model points. Uses internally a float type:
	function get_distance (
		p1, p2 : in type_vector_model)
		return type_distance_positive;
	

	-- Returns the distance along the given axis between the given points.
	function get_distance (
		point_1	: in type_vector_model;
		point_2	: in type_vector_model;
		axis	: in type_axis_2d) 
		return type_distance;


	
	-- Returns the angle of direection from the given 
	-- point p1 to the point p2. Uses internally a float type:
	function get_angle (
		p1, p2 : in type_vector_model)
		return type_rotation;


	-- Returns the rotation of the given point about the origin.
	-- If for example point is (1/1) then the return is 45 degree.
	-- if point is (-1/-1) then the return is -135 degree.
	function get_rotation ( -- CS rename to get_rotation_about_origin
		point : in type_vector_model)
		return type_rotation;


	

	function set (
		x, y : in type_position_axis) 
		return type_vector_model;
	

	procedure set (
		point	: in out type_vector_model;
		axis 	: in type_axis_2d;
		value	: in type_position_axis);


	procedure set (
		point	: in out type_vector_model;
		position: in type_vector_model);


	
	function get_x (
		point : in type_vector_model)
		return type_position_axis;

	
	function get_y (
		point : in type_vector_model)
		return type_position_axis;	

	
	function to_vector (
		point	: in type_vector_model)
		return type_vector;


	function to_vector_model (
		v	: in type_vector)
		return type_vector_model;


	function to_vector_model (
		d 		: in type_vector_model;
		clip	: in boolean := false)
		return type_vector_model;
	
	
	function to_vector_model (
		x,y : in string)
		return type_vector_model;


	
	

	function to_offset (
		p : in type_vector_model)
		return type_offset;

	
	function to_offset (
		x, y : in type_distance)
		return type_offset;


	

	function get_distance_absolute (
		point	: in type_vector_model;
		vector	: in type_vector)
		return type_float_positive;


	-- Returns the relative distance of point_two to point_one.	
	-- Subtracts point_one.x from point_two.y and point_one.y from point_two.y
	-- returns	d.x := point_two.x - point_one.x
	--			d.y := point_two.y - point_one.y;
	function get_distance_relative (
		point_one, point_two : in type_vector_model) -- CS rename to reference and point
		return type_vector_model;


	
	-- Computes the absolute distance between 
	-- two location vectors:
	function get_distance_absolute (
		point_one, point_two : in type_vector_model)
		return type_float_positive;



	-- Returns the absolute distance along the given axis between the given points.
	-- NOTE: The result in both x and y is always greater or equal zero.
	function get_distance_absolute (
		point_1	: in type_vector_model;
		point_2	: in type_vector_model;
		axis	: in type_axis_2d) 
		return type_distance_positive;
	
	

	
	-- Moves a point to the given destination:
	procedure move_to (
		point		: in out type_vector_model;
		destination	: in type_vector_model);


	-- Moves a point into direction by distance.
	function move (
		point		: in type_vector_model;
		direction	: in type_rotation;
		distance	: in type_distance_positive;
		clip		: in boolean := false)
		return type_vector_model;


	
	-- If axis is Y then it swaps right x with left x.
	-- If axis is X then it swaps upper y with lower y.
	procedure mirror_point (
		point	: in out type_vector_model;
		axis	: in type_mirror);	

	

	-- Compares two points by their distance to the origin:
	function "<" (left, right : in type_vector_model) return boolean;

	
	-- Use this package when lists of points must be handled:
	package pac_points is new doubly_linked_lists (type_vector_model);



	
	-- Sorts points by their distance to the reference point:
	procedure sort_by_distance (
		points		: in out pac_points.list;
		reference	: in type_vector_model;
		mode		: in type_sort_mode := SORT_ASCENDING);
		-- CS currently mode is ignored. sort ascending is default.
	
	
	
	-- Returns the number of points that the given list contains:
	function get_length (
		points : in pac_points.list)
		return natural;


	procedure move_points (
		points 	: in out pac_points.list;
		offset	: in type_vector_model);						

	
	procedure rotate_points (
		points 		: in out pac_points.list;
		rotation	: in type_rotation);						
	

	procedure mirror_points (
		points 	: in out pac_points.list;
		mirror	: in type_mirror);						

	
	
	-- Returns from a list of point the one that is closest to
	-- the given reference point:
	function get_nearest (
		points		: in pac_points.list;
		reference	: in type_vector_model := origin)
		return type_vector_model;

	
	-- Converts a list of points to vectors:
	function to_vectors (
		points : in pac_points.list)
		return pac_vectors.list;


	-- Converts a list of vectors to points:
	function to_points (
		vectors : in pac_vectors.list)
		return pac_points.list;

	
	-- Removes points which are stored multiple times
	-- from the given list:
	procedure remove_redundant_points (
		points : in out pac_points.list);



	
-- AREA (rectangular):
	
	type type_area is record
		width		: type_distance_positive := 0.0;
		height		: type_distance_positive := 0.0;
		position	: type_vector_model; -- lower left corner
	end record;

	
	-- Returns the position and dimensions of the given area as string:
	function to_string (
		box : in type_area)
		return string;


	-- Swaps width and height of the given area:
	procedure swap_edges (
		area : in out type_area);
	

	-- In order to handle the four corners of an
	-- area this type is required:
	type type_area_corners is record
		BL, BR, TL, TR : type_vector_model;
	end record;


	-- Returns the four corners of the given area.
	-- The area is given in model coordinates:
	function get_corners (
		area	: in type_area)
		return type_area_corners;

	
	-- Returns the center of the given area:
	function get_center (
		area	: in type_area)
		return type_vector_model;
	

	-- Returns the diagonal of the given area:
	function get_diagonal (
		area	: in type_area)
		return type_distance_positive;
		
	
	-- Returns true if the given point lies inside the given
	-- area or on its border. 
	function in_area (
		point	: in type_vector_model;
		area	: in type_area)
		return boolean;


	-- Returns true if the given areas overlap each other:
	function areas_overlap (
		A, B : in type_area)
		return boolean;


	-- Merges the given area B into area A:
	procedure merge_areas (
		A : in out type_area;
		B : in type_area);



-- BOUNDING-BOX:

	-- This is the bounding-box of the model. It is a rectangle
	-- that encloses all objects of the model and the margins 
	-- around the model:
	-- CS bounding_box : type_area;
	bounding_box : type_area := (
		width => 300.0,
		height	=> 200.0,							
		others => <>);
	
	
	-- These are the system limits for the width and height
	-- of the bounding-box of the model:
	bounding_box_width_max  : constant 
		type_distance_positive := 2_000.0;
	
	bounding_box_height_max : constant 
		type_distance_positive := 1_000.0;


	
	-- Indicates that the bounding_box has changed after calling procedure 
	-- compute_bounding_box:
	bounding_box_changed : boolean := false;

	
	-- In order to handle bouding-box related errors this 
	-- composite type is required:
	type type_bounding_box_error is record
		size_exceeded	: boolean := false;
		width			: type_distance_positive := 0.0;
		height			: type_distance_positive := 0.0;
		-- CS ? position : type_vector_model;
	end record;


	-- Here we store bounding-box related errors:
	bounding_box_error : type_bounding_box_error;



-- OBJECT ORIGINAL POSITION AND DISPLACEMENT


	-- If an object being dragged, then we backup 
	-- here its original position.
	-- It is used in order to calculate 
	-- the displacement of other attached objects:
	object_original_position : type_vector_model := origin;


	-- If an object is begin dragged, then other objects
	-- might be dragged along (like net segments, or track segments)
	-- These objects are subjected to a certain displacement:
	object_displacement : type_vector_model := origin;


-- START AND END POINT OF LINES AND ARCS:	

	type type_start_end_point is (A, B);


	-- If A given, then the return is B and vise versa:
	function get_opposide_end (
		AB_end	: in type_start_end_point)
		return type_start_end_point;
	
	
	function to_string (
		point : in type_start_end_point)
		return string;

	
	function to_start_end_point (
		point : in string)
		return type_start_end_point;

	
	
-- LINE

	
	type type_line_base is abstract tagged record
		A, B 		: type_vector_model; -- start and end point
		status		: type_object_status;
		status_AB	: type_AB_moving_status;
	end record;

	
	type type_line is tagged private;

	type type_line_array is array (natural range <>) of type_line;

	

	procedure reset_line (
		line : in out type_line);


	function to_line (
		A, B : in type_vector_model)
		return type_line'class;
						 
	
	-- Returns the start and end point of the given 
	-- line as string in the form "A: x/y B: x/y":
	function to_string (
		line : in type_line) 
		return string;


	-- Returns the end point of the given line
	-- as requested by argument AB_end:
	function get_end_point (
		line 	: in type_line;
		AB_end	: in type_start_end_point)
		return type_vector_model;


	-- Returns for a given direction the end of a line.
	-- IMPORTANT: The orientation of the line must be known beforehand.
	-- If the line orientation is horizontal:
	-- 1. If side is west, then it returns the end point that 
	--    is on the west end of the line.
	-- 2. If side is east, then it returns the end point that 
	--    is on the east end of the line.
	-- 3. Only west or east end can be inquired. 
	--    Otherwise an exception is raised because a horizontal line
	--    has no north or south end.
	--
	-- If the line orientation is vertical:
	-- 3. If side is north, then it returns the end point that 
	--    is on the north end of the line.
	-- 4. If side is south, then it returns the end point that 
	--    is on the south end of the line.
	-- 5. Only north or south end can be inquired. 
	--    Otherwise an exception is raised because a vertical line
	--    has no west or east end.
	--
	-- If the line orientation is sloping then all four ends
	-- can be inquired:
	function to_AB_end (
		line		: in type_line;
		NSWE_end	: in type_direction_NSWE)
		return type_start_end_point;
	

	-- This is the reverse of to_AB_end. 
	-- This function maps from a given A/B end to the
	-- north, south, west east end.
	-- If the line is horizontal, then the return is west or east.
	-- If the line is vertical, then the return is north or south.
	-- CS: If the line is a slope, then it will be handled like
	-- a horizontal line:
	function to_NSWE_end (
		line		: in type_line;
		AB_end		: in type_start_end_point)
		return type_direction_NSWE;


	-- Maps from the A/B end of a line to a rotation.
	-- The rotation is a multiple of 90 degrees.
	-- It bases on the function to_NSWE_end and translates
	-- the NSWE_end to an angle. See body for details:
	function to_rotation (
		line		: in type_line;
		AB_end		: in type_start_end_point)
		return type_rotation;
							 

	-- Returns the start point of the given line:
	function get_A (
		line : in type_line)
		return type_vector_model;

	
	-- Returns the end point of the given line:
	function get_B (
		line : in type_line)
		return type_vector_model;


	function get_center (
		line : in type_line)
		return type_vector_model;

	
	-- Sets the start point of a line:
	procedure set_A (
		line	: in out type_line;
		A		: in type_vector_model);

	
	-- Sets the end point of a line:
	procedure set_B (
		line	: in out type_line;
		B		: in type_vector_model);


	
	procedure move_A_by (
		line	: in out type_line;
		offset	: in type_vector_model);

	
	procedure move_B_by (
		line	: in out type_line;
		offset	: in type_vector_model);


	
	function is_selected (
		line : in type_line)
		return boolean;
	
	procedure set_selected (
		line : in out type_line);

	procedure clear_selected (
		line : in out type_line);


	
	function is_proposed (
		line : in type_line)
		return boolean;

	procedure set_proposed (
		line : in out type_line);
	
	procedure clear_proposed (
		line : in out type_line);


	
	function is_moving (
		line : in type_line)
		return boolean;

	procedure set_moving (
		line : in out type_line);

	procedure clear_moving (
		line : in out type_line);



	procedure modify_status (
		line 		: in out type_line;
		operation	: in type_status_operation);



	-- Returns true if the start point of the 
	-- given line is set as "moving":
	function is_A_moving (
		line : in type_line)
		return boolean;

	
	-- Returns true if the end point of the 
	-- given line is set as "moving":
	function is_B_moving (
		line : in type_line)
		return boolean;


	-- Sets the start point of the 
	-- given line as "moving":
	procedure set_A_moving (
		line	: in out type_line);
	

	-- Sets the end point of the 
	-- given line as "moving":
	procedure set_B_moving (
		line	: in out type_line);

		
	-- Resets all status flags to default:
	procedure reset_status (
		line 	: in out type_line);
	
	
	-- Moves a line by the given offset. 
	-- This moves both start and end point by the given offset:
	procedure move_by (
		line	: in out type_line;
		offset	: in type_vector_model);



	-- Moves the start point of a line by the given offset. 
	procedure move_start_by (
		line	: in out type_line;
		offset	: in type_vector_model);

	
	-- Moves the end point of a line by the given offset. 
	procedure move_end_by (
		line	: in out type_line;
		offset	: in type_vector_model);

	

	-- Mirrors a line along the given axis.
	procedure mirror_line (
		line : in out type_line;
		axis : in type_mirror);



	-- Rotates a line about the origin by the given rotation.
	procedure rotate_line_by (
		line		: in out type_line;
		rotation	: in type_rotation);

	
	-- Converts a "coarse line" as defined in this package
	-- to a "fine line" as defined in package pac_geometry_1.
	-- Discards the status of the given line and 
	-- applies default value to the status of the result:
	function to_line_fine (
		line : in type_line)
		return type_line_fine;


	-- Converts a "fine line" as defined in package pac_geometry_1
	-- to a "coarse line" as defined in this package.
	function to_line_coarse (
		line : in type_line_fine)
		return type_line'class;
	

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
		point		: in type_vector_model; 
		line_range	: in type_line_range)
		return type_distance_point_line;



	
	
	-- Returns true if the given location vector 
	-- lies on the given line. The A and B ends
	-- are included in the test:
	function on_line (
		line	: in type_line;
		vector	: in type_vector)
		return boolean; 

	
	-- Returns true if the given point lies on the given line.
	-- The A and B ends are included in the test:
	function on_line (
		line	: in type_line;
		point	: in type_vector_model)
		return boolean;

	
	-- Returns true if the given point lies on the
	-- given line AND between A and B. If the point lies
	-- either on A or B then result is false:
	function between_A_and_B (
		line	: in type_line;
		point	: in type_vector_model)
		return boolean;
	
	

	-- Returns the location vector of the start point of a line:
	function get_start_vector (
		line	: in type_line)
		return type_vector;

	
	-- Returns the location vector of the end point of a line:
	function get_end_vector (
		line	: in type_line)
		return type_vector;


	-- Returns the direction vector of a line:
	function get_direction_vector (
		line	: in type_line)
		return type_vector;
	

	-- Converts a line (consisting of start and end point)
	-- to a line vector consisting of start vector and
	-- direction vector.
	-- The start vector of the result will be directly derived 
	--  from the start point of the given line.
	-- The direction vector of the result will be computed as:
	--  dx = line.B.x - line.A.x
	--  dy = line.B.y - line.A.y
	--  dz = zero
	function to_line_vector (
		line	: in type_line)
		return type_line_vector;

	

	-- Returns the shortest distance from the given point to the
	-- given line:
	function get_shortest_distance (
		line	: in type_line;
		point	: in type_vector_model)
		return type_float_positive;

	
	function get_shortest_distance (
		line	: in type_line;
		point	: in type_vector)
		return type_float_positive;


	-- Returns the start or the end point of the given line,
	-- depending on which of them is closer to the given point:
	function get_nearest (
		line	: in type_line_fine;
		point	: in type_vector_model)
		return type_vector_model;
	
		
	-- Returns the bounding-box of the given line.
	-- It respects the linewidth and assumes that the line ends
	-- have round caps:
	function get_bounding_box (
		line	: in type_line;
		width	: in type_distance_positive)
		return type_area;

	

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
		return type_line_vector_intersection;



	-- A line may run in those directions:
	type type_line_orientation is (
		ORIENT_HORIZONTAL,
		ORIENT_VERTICAL,
		ORIENT_SLOPING);
	


	-- Returns the orientation of a line.
	function get_orientation (
		line : in type_line) 
		return type_line_orientation;



	-- Returns true if the given lines overlap each other
	-- and if they have the same orientation.
	-- If both lines are equal in their A and B ends, then
	-- we regard them as overlapping.
	-- Otherwise, the return is always false if any of the given lines
	-- has orientation ORIENT_SLOPING. In this case we assume
	-- they are not overlapping:
	function lines_overlap (
		line_1, line_2 : in type_line)
		return boolean;
	
	

	-- When a line is to be split in two or more
	-- fragments, then this type should be used for
	-- the output of a split operation.
	-- It is a parameterized type because the split
	-- operation may result in a single line in case
	-- the split point is the same as the A or B end
	-- of the line:
	type type_split_line (count : positive) is record
		segments : type_line_array (1 .. count);
	end record;


	-- This function splits a line at the given point.
	-- The result is two new lines which join each 
	-- other at the given point.
	-- Since the given point is not required to be on 
	-- the given line, the resulting lines may run in 
	-- to different directions.
	-- If either A or B of the given line is the same
	-- as the given split point, then the result is
	-- a single line, namely the given line without any change:
	function split_line (
		line 	: in type_line;
		point	: in type_vector_model)
		return type_split_line;

	

	-- This function splits a line into two or more
	-- shorter segments. The given list of points
	-- specifies the places where the line is to be split.
	-- Since the given points are not required to be on 
	-- the given line, the resulting line fragments may run in 
	-- to different directions.
	-- CS: Currently it is assumed that none of the given
	-- split points is equal to the A or B end of the line:
	function split_line (
		line 	: in type_line;
		points	: in pac_points.list)
		return type_split_line;


	-- Merges secondary line with primary line.
	-- The ends that are to be connected must be specified
	-- by primary_end and secondary_end. The other end of
	-- the lines are open. It is not required that primary
	-- and secondary line join each other in a common point.
	-- Nor is it required that both lines run into the same direction.
	-- So, the result is a line that runs from the open end
	-- of the primary line to the open end of the secondary line:
	procedure merge_lines (
		primary			: in out type_line;
		primary_end		: in type_start_end_point; -- the end of the primary line
		secondary		: in type_line;
		secondary_end	: in type_start_end_point); -- the end of the secondary line



	-- Merges two overlapping lines to a single one.
	-- "Overlapping" means that both have same orientation
	-- and do overlap in some way.
	-- An exception is raised if:
	-- 1. any of the given lines is a slope
	-- 2. the two lines have different orientation
	function merge_lines (
		primary			: in type_line;
		secondary		: in type_line)
		return type_line'class;
	
	
-- ARC


	type type_arc_base is abstract tagged record
		center		: type_vector_model;
		A, B		: type_vector_model; -- start and end point
		direction	: type_direction_of_rotation := CW;
		status		: type_object_status;
		-- CS status_AB	: type_AB_moving_status;
	end record;

	
	type type_arc is tagged private;



	function to_arc (
		center		: in type_vector_model;
		A			: in type_vector_model;			
		B			: in type_vector_model;
		direction	: in type_direction_of_rotation)
		return type_arc'class;
	

	procedure reset_arc (
		arc		: in out type_arc);
	

	-- Sets the center of the given arc:
	procedure set_center (
		arc		: in out type_arc;
		center	: in type_vector_model);


	-- Sets the start point of the given arc:
	procedure set_A (
		arc	: in out type_arc;
		A	: in type_vector_model);

	
	-- Sets the end point of the given arc:	
	procedure set_B (
		arc	: in out type_arc;
		B	: in type_vector_model);


	-- Sets the direction of the given arc:
	procedure set_direction (
		arc			: in out type_arc;
		direction	: in type_direction_of_rotation);

	
	-- Returns the center of the given arc:
	function get_center (
		arc : in type_arc)
		return type_vector_model;
	

	-- Returns the start point of the given arc:
	function get_A (
		arc : in type_arc)
		return type_vector_model;

	
	-- Returns the end point of the given arc:
	function get_B (
		arc : in type_arc)
		return type_vector_model;


	-- Returns the direction of the given arc:
	function get_direction (
		arc : in type_arc)
		return type_direction_of_rotation;


	
	
	function to_arc_fine (
		arc : in type_arc)
		return pac_geometry_1.type_arc_fine;

	
	function to_arc_coarse (
		arc : in pac_geometry_1.type_arc_fine)
		return type_arc'class;


	
	-- Returns the start, end point and angle of the given arc as string
	-- in the form "A: x/y B: x/y" C: x/y D: x/y).
	-- C is the center, D is the direction:
	function to_string (
		arc : in type_arc) 
		return string;

	

	function get_intersection (
		arc		: in type_arc;
		line	: in type_line_vector)
		return type_intersection_of_line_and_circle;


	

	function is_selected (
		arc : in type_arc)
		return boolean;
	
	procedure set_selected (
		arc : in out type_arc);

	procedure clear_selected (
		arc : in out type_arc);


	
	function is_proposed (
		arc : in type_arc)
		return boolean;

	procedure set_proposed (
		arc : in out type_arc);
	
	procedure clear_proposed (
		arc : in out type_arc);


	
	function is_moving (
		arc : in type_arc)
		return boolean;

	procedure set_moving (
		arc : in out type_arc);

	procedure clear_moving (
		arc : in out type_arc);



	procedure modify_status (
		arc 		: in out type_arc;
		operation	: in type_status_operation);


	procedure reset_status (
		arc 		: in out type_arc);


	
	-- Swaps start and end point of an arc. Reverses the direction of the arc:
	function reverse_arc (arc : in type_arc) return type_arc'class;
	procedure reverse_arc (arc : in out type_arc);

	
	-- Changes the direction of an arc to CCW (mathematical sense)
	-- by swapping start and end point. If direction is already CCW
	-- then nothing happens.
	function normalize_arc (arc: in type_arc) return type_arc'class;

	-- Returns true if start and end point of arc are equal:
	function zero_length (arc : in type_arc) return boolean;

	
	
	-- Moves an arc by the given offset. 
	procedure move_by (
		arc		: in out type_arc;
		offset	: in type_vector_model);


	-- Moves an arc to the given position. 
	procedure move_to (
		arc			: in out type_arc;
		position	: in type_vector_model);
	

	-- Mirrors an arc along the given axis.
	procedure mirror_arc (
		arc			: in out type_arc;
		axis		: in type_mirror);


	-- Rotates an arc about the origin by the given rotation.
	procedure rotate_arc_by (
		arc			: in out type_arc;
		rotation	: in type_rotation);


	
	
	-- Returns the distance between the start point 
	-- and the center of the arc.
	function get_radius_start (
		arc : in type_arc) 
		return type_float_positive;
	
	
	-- Returns the distance between the end point 
	-- and the center of the arc.
	function get_radius_end (
		arc : in type_arc) 
		return type_float_positive;



	-- Test whether the given arc is valid. The arc is valid if:
	-- - start and end point have equal distance to center
	-- - radius is greater zero
	function is_valid (
		arc : in type_arc)
		return boolean;


	

	-- Computes the end point of an arc
	-- based on a given center and start point:
	function get_arc_B (
		center	: in type_vector_model;
		A		: in type_vector_model;	-- start point
		angle 	: in type_angle) -- CS: type_angle_positive ?
		return type_vector_model;

	


	

	-- Returns the start and end angles of an arc.
	function to_arc_angles (
		arc : in type_arc) 
		return type_arc_angles;

	



	
	function get_shortest_distance (
		arc		: in type_arc;
		point	: in type_vector_model)
		return type_distance_polar;

	
	function get_shortest_distance (
		arc		: in type_arc;
		point	: in type_vector)
		return type_float_positive;

	


	function on_arc (
		arc		: in type_arc;
		point	: in type_vector_model)
		return boolean; 

	

	-- Returns the bounding-box of the given circle.
	-- It respects the linewidth of the circumfence.
	-- CS: For simlicity the given arc is theated like a circle.
	function get_bounding_box (
		arc 	: in type_arc;
		width	: in type_distance_positive)				  
		return type_area;


		

-- CIRCLE
	
	type type_circle_base is abstract tagged record
		center	: type_vector_model;
		radius  : type_distance_positive := 0.0;
		status	: type_object_status;
	end record;

	
	type type_circle is tagged private;


	function to_circle (
		center	: in type_vector_model;
		radius	: in type_distance_positive)
		return type_circle'class;


	procedure reset_circle (
		c : in out type_circle);
		

	procedure set_center (
		c : in out type_circle;
		e : in type_vector_model);
	

	procedure set_radius (
		c : in out type_circle;
		r : in type_distance_positive);

	

	function get_center (
		c : in type_circle)
		return type_vector_model;
	

	function get_radius (
		c : in type_circle)
		return type_distance_positive;

	
		
	function to_circle_fine (
		circle : in type_circle)
		return type_circle_fine;

	

	-- Returns the center and radius of the given circle as string
	-- in the form "C: x/y R: r". C is the center, R is the radius:
	function to_string (
		circle : in type_circle) 
		return string;



	function get_intersection (
		circle	: in type_circle;
		line	: in type_line_vector)
		return type_intersection_of_line_and_circle;

	

	
	function is_selected (
		circle : in type_circle)
		return boolean;
	
	procedure set_selected (
		circle : in out type_circle);

	procedure clear_selected (
		circle : in out type_circle);


	
	function is_proposed (
		circle : in type_circle)
		return boolean;

	procedure set_proposed (
		circle : in out type_circle);
	
	procedure clear_proposed (
		circle : in out type_circle);


	
	function is_moving (
		circle : in type_circle)
		return boolean;

	procedure set_moving (
		circle : in out type_circle);

	procedure clear_moving (
		circle : in out type_circle);



	procedure modify_status (
		circle 		: in out type_circle;
		operation	: in type_status_operation);


	procedure reset_status (
		circle 		: in out type_circle);



	
	function to_radius (
		r : in string)
		return type_distance_positive;
	

	function to_radius (
		r : in type_distance_positive)
		return type_float_positive;

	
	-- Moves a circle by the given offset. 
	procedure move_by (
		circle	: in out type_circle;
		offset	: in type_vector_model);


	-- Mirrors the center of a circle along the given axis.
	procedure mirror_circle (
		circle		: in out type_circle;
		axis		: in type_mirror);
	

	-- Rotates the center of acirlce about the origin:
	procedure rotate_circle_by (
		circle		: in out type_circle;
		rotation	: in type_rotation);

	



	function get_shortest_distance (
		circle	: in type_circle;
		point	: in type_vector_model)
		return type_distance_polar;


	function get_shortest_distance (
		circle	: in type_circle;
		point	: in type_vector)
		return type_float_positive;

	
	
	-- Returns the bounding-box of the given circle.
	-- It respects the linewidth of the circumfence:
	function get_bounding_box (
		circle 	: in type_circle;
		width	: in type_distance_positive)
		return type_area;



	

-- POSITION:
	


	-- The position of an object is a composite
	-- of the place (x/y) and the rotation of the object about
	-- its own center:
	type type_position is tagged record -- CS make private ?
		place 		: type_vector_model := origin;
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
		point		: in type_vector_model;
		rotation	: in type_rotation)
		return type_position'class;


	procedure set (
		position	: in out type_position;
		axis 		: in type_axis_2d;
		value		: in type_position_axis);

	
	procedure set (
		position	: in out type_position;
		place		: in type_vector_model);
	
	
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



	-- This is more than just adding 
	-- location vectors (place) and rotation.
	-- This function does the follwing:
	-- If mirror is MIRROR_NO, then
	--  1. Rotates the given position.place about its 
	--     own origin by the offset.rotation.
	--  2. Adds position.place and offset.place.
	--  3. Adds position.rotation and offset.rotation.
	--
	-- If mirror is MIRROR_ALONG_Y_AXIS, then
	--  1. Mirrors position.place along Y-axis.
	--  2. Rotates the given position.place about its 
	--     own origin by the negative offset.rotation.
	--  3. Adds position.place and offset.place.
	--  4. Adds position.rotation and offset.rotation.
	-- 
	-- If mirror is something else, then nothing happens:
	procedure add (
		position	: in out type_position;
		offset		: in type_position;
		mirror		: in type_mirror := MIRROR_NO);

	
	
-- CATCH ZONE:

	-- When searching objects within a circular zone 
	-- around a certain point the concept of a catch zone
	-- is required:
	
	subtype type_zone_radius is type_float_positive
		range 0.0 .. type_float_positive (type_distance_positive'last/100.0);


	type type_catch_zone is private;
	
	
	function set_catch_zone (
		center	: in type_vector_model;
		radius	: in type_zone_radius)
		return type_catch_zone;


	function get_center (
		zone	: in type_catch_zone)
		return type_vector_model;
	

	function get_radius (
		zone	: in type_catch_zone)
		return type_zone_radius;

	
	function to_string (
		zone : in type_catch_zone)
		return string;

	
	function to_zone_radius (
		c : in string)
		return type_zone_radius;



	

	-- Returns true if the given distance is 
	-- less or equal the given zone radius:
	function in_radius (
		distance : in type_float_positive;
		radius	 : in type_zone_radius)
		return boolean;
							   
	
	-- Returns true if point is inside the given catch zone:
	function in_catch_zone (
		zone	: in type_catch_zone;
		point	: in type_vector_model)
		return boolean;
	

	-- Returns true if the given line is in the given catch zone.
	-- Optionally the linewidth can be specified so that it
	-- is taken into account. If a linewidth greater zero
	-- is given, then we assume, that the line has round caps:
	function in_catch_zone (
		zone	: in type_catch_zone;
		line	: in type_line;
		width	: in type_distance_positive := 0.0)
		return boolean;


	
	-- Returns true if the given arc is in the given catch zone.
	-- Optionally the linewidth can be specified so that it
	-- is taken into account. If a linewidth greater zero
	-- is given, then we assume, that the arc has round caps:
	function in_catch_zone (
		zone	: in type_catch_zone;
		arc		: in type_arc;
		width	: in type_distance_positive := 0.0)
		return boolean;


	-- Returns true if the given circle is in the given catch zone.
	-- Optionally the linewidth can be specified so that it
	-- is taken into account:
	function in_catch_zone (
		zone	: in type_catch_zone;
		circle	: in type_circle;
		width	: in type_distance_positive := 0.0)
		return boolean;


	
	procedure nothing_found (
		zone	: in type_catch_zone); 





	
	

-- ZONES OF A LINE:

	-- A line is divided into three zones. Their width is the ratio
	-- of line length and the zone_division_factor.
	-- 
	--    S---|---center---|---E
	--
	-- The position of the bar (|) in this drawing depends on the zone_division_factor.
	-- The center length is twice the length of start/end point.
	type type_line_zone is (START_POINT, END_POINT, CENTER); -- CS rename to ZONE_A, ZONE_B, ZONE_CENTER
	line_zone_division_factor : constant positive := 4;

	

	function to_string (
		zone : in type_line_zone)
		return string;
	
		
	-- Calculates the zone of the line where point is nearest.
	-- Point is not required to sit exactly on the line.
	function get_zone (
		line	: in type_line;
		point	: in type_vector_model)
		return type_line_zone;


	-- Moves the start point, the end point or both ends of a line
	-- according to the zone where the line is being attacked.
	-- If start or end point is affected, then the point is
	-- moved to the given destination.
	-- If center of the the line is affected, then start and end point
	-- of the line is moved by the relative distance of point_of_attack
	-- to the destination:
	procedure attack (
		line			: in out type_line;
		point_of_attack	: in type_vector_model;
		destination		: in type_vector_model);



	-- Calculates the zone of the arc where point is nearest.
	-- Point is not required to sit exactly on the arc.
	function get_zone (
		arc		: in type_arc;
		point	: in type_vector_model)
		return type_line_zone;
	
	
	procedure attack (
		arc				: in out type_arc;
		point_of_attack	: in type_vector_model;
		destination		: in type_vector_model);

	

-- EXTENDED BOUNDING BOX COMPUTATIONS:

	-- Computes the bounding-box of a given line
	-- taking into account the given offsets, rotation
	-- and mirror style:
	function get_bounding_box (
		line		: in type_line'class;
		width		: in type_distance_positive;
		offset_1	: in type_vector_model;
		offset_2	: in type_vector_model := origin;
		rotation	: in type_rotation;
		mirror		: in type_mirror := MIRROR_NO)	
		return type_area;


	-- Computes the bounding-box of a given arc
	-- taking into account the given offsets, rotation
	-- and mirror style:
	function get_bounding_box (
		arc			: in type_arc'class;
		width		: in type_distance_positive;
		offset_1	: in type_vector_model;
		offset_2	: in type_vector_model := origin;
		rotation	: in type_rotation;
		mirror		: in type_mirror := MIRROR_NO)	
		return type_area;

	
	-- Computes the bounding-box of a given circle
	-- taking into account the given offsets, rotation
	-- and mirror style:
	function get_bounding_box (
		circle		: in type_circle'class;
		width		: in type_distance_positive;
		offset_1	: in type_vector_model;
		offset_2	: in type_vector_model := origin;
		rotation	: in type_rotation;
		mirror		: in type_mirror := MIRROR_NO)	
		return type_area;

	

private


	far_upper_left : constant type_vector_model :=
		(x => type_position_axis'first,
			y => type_position_axis'last);
	
	far_upper_right : constant type_vector_model :=
		(x => type_position_axis'last,
			y => type_position_axis'last);

	far_lower_left : constant type_vector_model :=
		(x => type_position_axis'first,
			y => type_position_axis'first);
	
	far_lower_right : constant type_vector_model :=
		(x => type_position_axis'last,
			y => type_position_axis'first);
	
	
	origin_zero_rotation : constant type_position := (others => <>);
 
	far_upper_right_zero_rotation : constant type_position :=
		(far_upper_right, zero_rotation);


	-- boundaries_default : constant type_boundaries := (others => <>);

	type type_line is new type_line_base with null record;
	
	type type_arc is new type_arc_base with null record;
	
	type type_circle is new type_circle_base with null record;
	

	

	type type_catch_zone is record
		center	: type_vector_model;
		radius	: type_zone_radius;
	end record;

	
end et_geometry_2a;

