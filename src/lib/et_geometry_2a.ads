------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             GEOMETRY 2a                                  --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2024                                                       --
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

with et_geometry;				use et_geometry;

with et_geometry_1;

with et_string_processing;		use et_string_processing;
with et_object_status;			use et_object_status;


generic
	with package pac_geometry_1 is new et_geometry_1 (<>);
	
	type type_distance_model is delta <> digits <>;

	axis_min, axis_max : type_distance_model;

	type type_rotation_model is delta <> digits <>;
	
	
package et_geometry_2a is

	use pac_geometry_1;
	use pac_float_numbers_functions;

	zero 		: constant type_distance_model := 0.0;
	far_left	: constant type_distance_model := axis_min;
	far_right	: constant type_distance_model := axis_max;

	

	function get_info (editor: in string)
		return string;


	
	-- The directions into which the an object can be moved
	-- by means of the cursor keys (arrow keys):
	type type_direction is (DIR_RIGHT, DIR_LEFT, DIR_UP, DIR_DOWN);





		
-- DISTANCE:
	

	-- Converts a mil number (given as a string) to millimeters.	
	function mil_to_distance (
		mil : in string) 
		return type_distance_model;


	
	function distance_to_mil (
		distance : in type_distance_model) 
		return string;



	
	-- Use this type for distances, lengths, ...
	-- Because those things require positive numbers:
	subtype type_distance_model_positive is type_distance_model
		range 0.0 .. type_distance_model'last;

	
	-- This function returns the given distance as string:	
	function to_string (
		distance : in type_distance_model)
		return string;


	-- The position along an axis:
	subtype type_position_axis is type_distance_model 
		range axis_min .. axis_max;


	-- Converts a float number to type_distance by rounding
	-- according to the bankers rule:
	-- Use it !!!!!
	function to_distance (f : in type_float)
		return type_distance_model;


	function to_distance (dd : in string) 
		return type_distance_model;		

	

-- ROTATION / ANGLE:
	
	zero_rotation : constant type_rotation_model := 0.0;

	
	-- Converts the given rotation/angle to a string:
	function to_string (
		rotation : in type_rotation_model)
		return string;


	function to_rotation (
		rotation : in string) 
		return type_rotation_model;


	-- Converts a float number to type_rotation_model by rounding
	-- according to the bankers rule:
	-- Use it !!!!!
	function to_rotation (
		f : in type_float)
		return type_rotation_model;


	
	-- Adds two angles.
	-- If result greater 360 degree then 360 degree is subtracted from result.
	-- If result less than 360 degree then 360 degree is added to the result.
	function add (
		left, right : in type_rotation_model) 
		return type_rotation_model;

	

-- RELATIVE DISTANCE:
	
	type type_distance_relative is record -- CS rename to type_distance_model_relative
		x, y : type_distance_model := zero;
	end record;


	
	
	
-- POINT / POSITION / LOCATION / LOCATION VECTOR / DISTANCE VECTOR:
	
	type type_vector_model is record
		x, y : type_distance_model := 0.0;
	end record;

	
	-- The origin is a small cross at model position (0;0).
	origin : constant type_vector_model := (0.0, 0.0);

	-- far_upper_left	: constant type_vector_model;
	-- far_upper_right	: constant type_vector_model;
	-- far_lower_left	: constant type_vector_model;
	-- far_lower_right	: constant type_vector_model;

	

	-- This function returns the given vector
	-- as string:
	function to_string (
		v : in type_vector_model)
		return string;


	-- This function inverts a vector by multiplying
	-- its components by -1:
	function invert (
		point	: in type_vector_model)
		return type_vector_model;
	
						 
	-- Moves a model point by the given offset:
	procedure move_by (
		point	: in out type_vector_model;
		offset	: in type_vector_model);


	-- Rotates the given point BY the given angle about the origin.
	-- Changes point.x and point.y only.
	procedure rotate_by (
		point		: in out type_vector_model;
		rotation	: in type_rotation_model);



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
		return type_distance_model_positive;
	

	-- Returns the angle of direection from the given 
	-- point p1 to the point p2. Uses internally a float type:
	function get_angle (
		p1, p2 : in type_vector_model)
		return type_rotation_model;


	-- Returns the rotation of the given point about the origin.
	-- If for example point is (1/1) then the return is 45 degree.
	-- if point is (-1/-1) then the return is -135 degree.
	function get_rotation ( -- CS rename to get_rotation_about_origin
		point : in type_vector_model)
		return type_rotation_model;


	

	function set (
		x, y : in type_position_axis) 
		return type_vector_model;
	

	procedure set (
		point	: in out type_vector_model;
		axis 	: in type_axis_2d;
		value	: in type_position_axis);

	
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
		x,y : in string)
		return type_vector_model;


	
	

	function to_offset (
		p : in type_vector_model)
		return type_offset;

	
	function to_offset (
		x, y : in type_distance_model)
		return type_offset;


	function to_offset (
		distance : in type_distance_relative)
		return type_offset;


	

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



	-- Moves a point by the given offset.
	procedure move_by (
		point	: in out type_vector_model;
		offset	: in type_distance_relative);
	

	
	-- Moves a point to the given destination:
	procedure move_to (
		point		: in out type_vector_model;
		destination	: in type_vector_model);



	-- If axis is Y then it swaps right x with left x.
	-- If axis is X then it swaps upper y with lower y.
	procedure mirror (
		point	: in out type_vector_model;
		axis	: in type_axis_2d);	

	

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

	



	
-- AREA:
	
	type type_area is record
		width		: type_distance_model_positive := 0.0;
		height		: type_distance_model_positive := 0.0;
		position	: type_vector_model; -- lower left corner
	end record;

	
	-- Returns the position and dimensions of the given area as string:
	function to_string (
		box : in type_area)
		return string;


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
		type_distance_model_positive := 2_000.0;
	
	bounding_box_height_max : constant 
		type_distance_model_positive := 1_000.0;


	
	-- Indicates that the bounding_box has changed after calling procedure 
	-- compute_bounding_box:
	bounding_box_changed : boolean := false;

	
	-- In order to handle bouding-box related errors this 
	-- composite type is required:
	type type_bounding_box_error is record
		size_exceeded	: boolean := false;
		width			: type_distance_model_positive := 0.0;
		height			: type_distance_model_positive := 0.0;
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


	-- Moves a line by the given offset. 
	-- This moves both start and end point by the given offset:
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
		rotation	: in type_rotation_model);

	

	function to_line_fine (
		line : in type_line)
		return type_line_fine;

	
	
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
		width	: in type_distance_model_positive)
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

	
	-- Returns the start, end point and angle of the given arc as string.
	function to_string (arc : in type_arc) return string;


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
		axis		: in type_axis_2d);


	-- Rotates an arc about the origin by the given rotation.
	procedure rotate_by (
		arc			: in out type_arc;
		rotation	: in type_rotation_model);


	
	
	-- Returns the distance between the start point and the center of the arc.
	function get_radius_start (
		arc : in type_arc) 
		return type_float_positive;
	
	
	-- Returns the distance between the end point and the center of the arc.
	function get_radius_end (
		arc : in type_arc) 
		return type_float_positive;






	

	-- Returns the start and end angles of an arc.
	function to_arc_angles (
		arc : in type_arc) 
		return type_arc_angles;

	

	
	
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


	

	-- Returns the shortest distance between a point and an arc.
	-- If the point is on the center of the arc, then the return is
	-- absolute zero and angle zero degree:
	-- CS: wrong, should be absolute distance to start and angle of start point.
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



	-- Returns the bounding-box of the given circle.
	-- It respects the linewidth of the circumfence:
	function get_bounding_box (
		arc 	: in type_arc;
		width	: in type_distance_model_positive)				  
		return type_area;
	-- CS INCOMPLETE !

		

-- CIRCLE
	
	type type_circle_base is abstract tagged record
		center	: type_vector_model;
		radius  : type_float_positive := 0.0;
		status	: type_object_status;
	end record;

	type type_circle is new type_circle_base with null record;
	-- CS use this type wherever a type_circle is declared unnessecarily.


	-- Returns the center and radius of the given circle as string.
	function to_string (circle : in type_circle) return string;


	-- Moves a circle by the given offset. 
	procedure move_by (
		circle	: in out type_circle;
		offset	: in type_distance_relative);


	-- Mirrors the center of a circle along the given axis.
	procedure mirror (
		circle		: in out type_circle;
		axis		: in type_axis_2d);
	

	procedure rotate_by (
		circle		: in out type_circle;
		rotation	: in type_rotation_model);

	
	
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
		width	: in type_distance_model_positive)
		return type_area;



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

	

-- POSITION:
	
	point_preamble_with_rotation : constant string := 
		" (x"
		& axis_separator
		& "y"
		& axis_separator
		& "rotation)";


	-- The position of an object is a composite
	-- of the place (x/y) and the rotation of the object about
	-- its own center:
	type type_position is tagged record
		place 		: type_vector_model := origin;
		rotation	: type_rotation_model := zero_rotation;
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
		rotation	: in type_rotation_model)
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
		rotation	: in type_rotation_model);


	function get_x (
		position : in type_position)
		return type_distance_model;
	

	function get_y (
		position : in type_position)
		return type_distance_model;

	

	
	-- Returns the rotation of the given position.
	function get_rotation (
		position : in type_position) 
		return type_rotation_model;



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

	
end et_geometry_2a;
