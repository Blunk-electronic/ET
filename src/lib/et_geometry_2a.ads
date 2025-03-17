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

with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;


with ada.numerics;
with ada.numerics.generic_elementary_functions;

with et_axes;					use et_axes;
with et_primitive_objects;		use et_primitive_objects;
with et_geometry_1;

with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;
with et_object_status;			use et_object_status;
with et_mirroring;				use et_mirroring;


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


	
	-- The directions into which the an object can be moved
	-- by means of the cursor keys (arrow keys):
	type type_direction is (DIR_RIGHT, DIR_LEFT, DIR_UP, DIR_DOWN);



	-- CS use prefix ?
	type type_coordinates is (RELATIVE, ABSOLUTE);

	function to_string (coordinates : in type_coordinates) return string;
	function to_coordinates (coordinates : in string) return type_coordinates;

	

		
-- DISTANCE:

	-- For collecting and sorting distances:
	package pac_distances is new doubly_linked_lists (type_distance);
	package pac_distances_sorting is new pac_distances.generic_sorting;


	
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



	type type_output_format is (
		FORMAT_1,
		FORMAT_2,
		FORMAT_3,
		FORMAT_4);
		
	
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

	

-- RELATIVE DISTANCE:
	
	type type_distance_relative is record
		x, y : type_distance := zero;
	end record;


	function to_string (
		distance : in type_distance_relative)
		return string;

	

	function to_distance_relative (
		x,y : in type_distance)
		return type_distance_relative;

	
	function to_distance_relative (
		v : in type_vector)
		return type_distance_relative;
	

	
	
	
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
	function add (
		v1, v2 : in type_vector_model)
		return type_vector_model;
	

	procedure add (
		v1 : in out type_vector_model;
		v2 : in type_vector_model);
	

	-- Subtracts v2 from v1. Computes v2 - v2:
	function subtract (
		v1, v2 : in type_vector_model)
		return type_distance_relative;

	
	-- Moves a model point by the given offset:
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


	function to_point (
		v	: in type_vector)
		return type_vector_model;


	function to_point (
		d 		: in type_distance_relative;
		clip	: in boolean := false)
		return type_vector_model;
	
	
	function to_point (
		x,y : in string)
		return type_vector_model;


	
	

	function to_offset (
		p : in type_vector_model)
		return type_offset;

	
	function to_offset (
		x, y : in type_distance)
		return type_offset;


	function to_offset (
		distance : in type_distance_relative)
		return type_offset;



	function to_distance_relative (
		p : in type_vector_model)
		return type_distance_relative;

	
	

	function get_distance_total ( -- CS rename to get_distance_absolute
		point	: in type_vector_model;
		vector	: in type_vector)
		return type_float_positive;


	-- Returns the relative distance of point_two to point_one.	
	-- Subtracts point_one.x from point_two.y and point_one.y from point_two.y
	-- returns	d.x := point_two.x - point_one.x
	--			d.y := point_two.y - point_one.y;
	function get_distance_relative (
		point_one, point_two : in type_vector_model) -- CS rename to reference and point
		return type_distance_relative;


	
	-- Computes the total distance between point_one and point_two.
	function get_distance_total (
		point_one, point_two : in type_vector_model)
		return type_float_positive;



	-- Returns the absolute distance along the given axis between the given points.
	-- NOTE: The result in both x and y is always greater or equal zero.
	function get_distance_abs (
		point_1	: in type_vector_model;
		point_2	: in type_vector_model;
		axis	: in type_axis_2d) 
		return type_distance_positive;
	
	

	-- Moves a point by the given offset.
	procedure move_by (
		point	: in out type_vector_model;
		offset	: in type_distance_relative);
	

	
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
	procedure mirror (
		point	: in out type_vector_model;
		axis	: in type_mirror);	

	

	-- Compares two points by their distance to the origin:
	function "<" (left, right : in type_vector_model) return boolean;

	
	-- Use this package when lists of points must be handled:
	package pac_points is new doubly_linked_lists (type_vector_model);


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

	


-- LINE
	
	type type_line_base is abstract tagged record
		start_point	: type_vector_model;
		end_point	: type_vector_model;
		status		: type_object_status;
	end record;

	
	type type_line is new type_line_base with null record;
	
	
	-- Returns the start and end point of the given line as string.
	function to_string (line : in type_line) return string;


	
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


	procedure reset_status (
		line 		: in out type_line);
	
	
	-- Moves a line by the given offset. 
	-- This moves both start and end point by the given offset:
	procedure move_by (
		line	: in out type_line;
		offset	: in type_distance_relative);



	-- Moves the start point of a line by the given offset. 
	procedure move_start_by (
		line	: in out type_line;
		offset	: in type_distance_relative);

	
	-- Moves the end point of a line by the given offset. 
	procedure move_end_by (
		line	: in out type_line;
		offset	: in type_distance_relative);

	

	-- Mirrors a line along the given axis.
	procedure mirror (
		line		: in out type_line;
		axis		: in type_mirror);



	-- Rotates a line about the origin by the given rotation.
	procedure rotate_by (
		line		: in out type_line;
		rotation	: in type_rotation);

	
	-- Converts a "coarse line" as defined in this package
	-- to a "fine line" as defined in package pac_geometry_1:
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



	
	
	-- Returns true if the given location vector lies on the given line.
	function on_line (
		line	: in type_line;
		vector	: in type_vector)
		return boolean; 

	
	-- Returns true if the given point lies on the given line.
	function on_line (
		line	: in type_line;
		point	: in type_vector_model)
		return boolean;



	

	-- Returns the location vector of the start point of a line:
	function get_start_vector (
		line	: in type_line)
		return type_vector; -- CS should be type_vector_model

	
	-- Returns the location vector of the end point of a line:
	function get_end_vector (
		line	: in type_line)
		return type_vector; -- CS should be type_vector_model


	-- Returns the direction vector of a line:
	function get_direction_vector (
		line	: in type_line)
		return type_vector; -- CS should be type_distance_relative ?
	

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



	
-- ARC
	
	type type_arc_base is abstract tagged record  -- CS should be private ?
		center			: type_vector_model;
		start_point		: type_vector_model;
		end_point		: type_vector_model;
		direction		: type_direction_of_rotation := CW;
		status			: type_object_status;
	end record;

	type type_arc is new type_arc_base with null record;
	-- CS use this type wherever a type_arc is declared unnessecarily.


	
	function to_arc_fine (
		arc : in type_arc)
		return pac_geometry_1.type_arc_fine;

	
	function to_arc_coarse (
		arc : in pac_geometry_1.type_arc_fine)
		return type_arc'class;


	
	-- Returns the start, end point and angle of the given arc as string.
	function to_string (arc : in type_arc) return string;



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
		offset	: in type_distance_relative);


	-- Moves an arc to the given position. 
	procedure move_to (
		arc			: in out type_arc;
		position	: in type_vector_model);
	

	-- Mirrors an arc along the given axis.
	procedure mirror (
		arc			: in out type_arc;
		axis		: in type_mirror);


	-- Rotates an arc about the origin by the given rotation.
	procedure rotate_by (
		arc			: in out type_arc;
		rotation	: in type_rotation);


	
	
	-- Returns the distance between the start point and the center of the arc.
	function get_radius_start (
		arc : in type_arc) 
		return type_float_positive;
	
	
	-- Returns the distance between the end point and the center of the arc.
	function get_radius_end (
		arc : in type_arc) 
		return type_float_positive;



	-- Test whether the given arc is valid. The arc is valid if:
	-- - start and end point have equal distance to center
	-- - radius is greater zero
	function is_valid (
		arc : in type_arc)
		return boolean;


	

	-- Computes the end point of an arc.
	function arc_end_point (
		center		: in type_vector_model;
		start_point	: in type_vector_model;	
		angle 		: in type_angle) -- CS: type_angle_positive ?
		return type_vector_model;

	


	

	-- Returns the start and end angles of an arc.
	function to_arc_angles (
		arc : in type_arc) 
		return type_arc_angles;

	


	-- Returns the arc segment that is nearest
	-- to the given point:
	function get_nearest (
		segments	: in type_arc_segments;
		point		: in type_vector_model)
		return positive;

	
	
	
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


	

	-- Returns the shortest distance between a point and an arc.
	-- If the point is equal the center of the arc, then the return is
	-- the radius of the arc and the angle to the start point of the arc:
	function get_shortest_distance (
		arc		: in type_arc;
		point	: in type_vector_model)
		return type_distance_polar;


	
	-- CS: INCOMPLETE !!! Returns always zero currently.
	function get_shortest_distance (
		arc		: in type_arc;
		point	: in type_vector)
		return type_float;

	

	-- Returns true if the given point sits on the given arc.
	function on_arc (
		arc			: in type_arc;
		vector		: in type_vector)
		return boolean; 


	function on_arc (
		arc			: in type_arc;
		point		: in type_vector_model)
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
		--radius  : type_float_positive := 0.0;
		radius  : type_distance_positive := 0.0;
		status	: type_object_status;
	end record;

	type type_circle is new type_circle_base with null record;
	-- CS use this type wherever a type_circle is declared unnessecarily.


	-- Returns the center and radius of the given circle as string.
	function to_string (circle : in type_circle) return string;


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


	procedure set_radius (
		c : in out type_circle;
		r : in type_distance_positive);
	
		
	-- Moves a circle by the given offset. 
	procedure move_by (
		circle	: in out type_circle;
		offset	: in type_distance_relative);


	-- Mirrors the center of a circle along the given axis.
	procedure mirror (
		circle		: in out type_circle;
		axis		: in type_mirror);
	

	procedure rotate_by (
		circle		: in out type_circle;
		rotation	: in type_rotation);

	
	-- Returns true if the given point sits on the given circle circumfence.
	function on_circle (
		circle		: in type_circle;
		point		: in type_vector_model)
		return boolean;


	
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
	-- CS move this stuff to et_geometry_1 ?

	

	-- Computes the intersections of a line with a circle.
	-- See more on overloaded function get_intersection (line, arc):
	function get_intersection (
		circle	: in type_circle;
		line	: in type_line_vector)
		return type_intersection_of_line_and_circle;

	
	-- Returns the distance of point to circumfence of circle.
	-- Assumes the point is INSIDE the circle or ON the circumfence of the circle.
	-- The point must not be OUTSIDE the circle !
	function get_distance_to_circumfence (
		circle	: in type_circle;
		point	: in type_vector_model)
		return type_distance_polar;


	-- Returns the shortest distance from the given point to the
	-- given circle. The point may be inside or outside the circle.
	-- However, the return is the distance to the circumfence of the circle.
	function get_shortest_distance (
		circle	: in type_circle;
		point	: in type_vector_model)
		return type_distance_polar;


	-- CS: INCOMPLETE !!! Returns always zero currently.
	function get_shortest_distance (
		point	: in type_vector;
		circle	: in type_circle)
		return type_float;


	-- Returns the bounding-box of the given circle.
	-- It respects the linewidth of the circumfence:
	function get_bounding_box (
		circle 	: in type_circle;
		width	: in type_distance_positive)
		return type_area;



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

	

-- POSITION:
	


	-- The position of an object is a composite
	-- of the place (x/y) and the rotation of the object about
	-- its own center:
	type type_position is tagged record
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
	type type_line_zone is (START_POINT, END_POINT, CENTER);
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


	type type_catch_zone is record
		center	: type_vector_model;
		radius	: type_zone_radius;
	end record;

	
end et_geometry_2a;

