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



generic
	
	with package pac_geometry_1 is new et_geometry_1 (<>);
	
package et_geometry_2 is
	

	use pac_geometry_1;
	use pac_functions_distance;

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

	
	


	
-- VECTOR OPERATIONS
	
	type type_vector is private;

	null_vector : constant type_vector;

	unity_vector : constant type_vector;

	function to_string (
		v	: in type_vector)
		return string;
	
	function get_x (
		v	: in type_vector)
		return type_float_internal;

	function get_y (
		v	: in type_vector)
		return type_float_internal;

	function get_z (
		v	: in type_vector)
		return type_float_internal;

	
	function get_distance_total (
		point	: in type_point;
		vector	: in type_vector)
		return type_float_internal;
	
	
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
		vector_one, vector_two : in type_vector)
		return type_distance_polar;
	
	-- Moves the given vector by given offset.
	-- Leaves z unchanged.
	function move_by (
		v		: in type_vector;
		offset	: in type_distance_relative)
		return type_vector;

	-- Moves a location vector into given direction
	-- by given distance. Leaves z unchanged.
	procedure move_by (
		v			: in out type_vector;
		direction	: in type_rotation;
		distance	: in type_float_internal);
									
	function to_vector (
		point	: in type_point)
		return type_vector;

	
	
	function to_point (
		v	: in type_vector)
		return type_point;
	
	function absolute (
		vector	: in type_vector)
		return type_float_internal;

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

	

	-- A ray has a fixed starting point, a direction and
	-- no end point:
	type type_ray is record
		start_point	: type_vector;
		direction	: type_rotation;
	end record;

	
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

	-- Moves a line vector by the given offset.
	-- Changes only the start vector. Leaves the
	-- direction vector unchanged.
	function move_by (
		lv		: in type_line_vector;
		offset	: in type_distance_relative)
		return type_line_vector;
	
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

	
	-- In general an intersection is composed of a 
	-- location vector, where the two objects meet,
	-- and and the angle at which they intersect:
	type type_intersection is record
		vector	: type_vector; -- location vector
		angle	: type_rotation := zero_rotation;
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
	end record;

	type type_line is new type_line_base with null record;

	
	function make_line (
		start_x, start_y, end_x, end_y : in type_distance)
		return type_line'class;

	
	function make_line (
		start_point, end_point : in type_point)
		return type_line'class;



	
	function round (line : in type_line)
		return type_line'class;

	procedure round (line : in out type_line);
	
	
	-- Returns the length of a line:
	function get_length (line : in type_line)
		return type_distance_positive;


	-- Returns the length of the longest line.
	-- If both have equal length, then the length of 
	-- the second line will be returned:
	function get_greatest_length (l1, l2 : in type_line)
		return type_distance_positive;

	
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



	
	type type_nearest is (BEFORE, AFTER);
	
	-- Returns the nearest point after/before
	-- a given point on the given line.
	-- The argument "after" determines whether to return
	-- a point before or after the given point:
	function get_nearest (
		line	: in type_line;
		point	: in type_point;
		place	: in type_nearest := AFTER)
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

	
	function get_intersection (
		line_1, line_2 : in type_line)
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
		
	-- Computes the distance between a location vector and a line.
	-- This computation does not care about end or start point of the line.
	-- It assumes an indefinite long line without start or end point.
	--function get_distance (
		--line	: in type_line;
		--vector	: in type_vector)
		--return type_distance_positive;

	-- Computes the distance between a location vector and a line.
	-- This computation does not care about end or start point of the line.
	-- It assumes an indefinite long line without start or end point.
	function get_distance (
		line	: in type_line;
		vector	: in type_vector)
		return type_float_internal;

	
	-- Returns the direction in degrees of a line.
	-- Example: If a line runs from 0/0 to 1/1 then the result is 45 degree.
	-- Example: If a line runs from -1/-1 to -4/-4 then the result is 225 degree.
	function get_direction (
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
	
	function get_distance (d : in type_distance_point_line) 
		return type_float_internal;
	
	function get_intersection (d : in type_distance_point_line) 
		return type_vector;
	
	function get_direction (d : in type_distance_point_line) return type_rotation;
	function on_start_point (d : in type_distance_point_line) return boolean;
	function on_end_point (d : in type_distance_point_line) return boolean;
	
	type type_line_range is (
		BETWEEN_END_POINTS,	-- start and end point excluded
		WITH_END_POINTS,	-- start and end point included
		BEYOND_END_POINTS	-- indefinite long line assumed. extends beyond both start and end point into infinity
		);
	
	-- Computes the shortest distance (perpendicular) of a 
	-- location vector to a line. 
	-- CS insufficient ! More details !!! especially on the out_of_range flag
	function get_distance (
		vector		: in type_vector; 
		line		: in type_line;
		line_range	: in type_line_range)
		return type_distance_point_line;

	-- Computes the shortest distance (perpendicular) of a
	-- point to a line. 		
	function get_distance (
		point		: in type_point; 
		line		: in type_line;
		line_range	: in type_line_range)
		return type_distance_point_line;

	
	-- Returns true if the given location vector points
	-- on the given line.
	function on_line (
		vector	: in type_vector;
		line	: in type_line)
		return boolean; 

	-- Returns true if the given point is on the given line.
	function on_line (
		point	: in type_point;
		line	: in type_line)
		return boolean;

	
	-- Returns the shortest distance from the given point to the
	-- given line:
	function get_shortest_distance (
		point	: in type_point;
		line	: in type_line)
		return type_distance_polar;
		
	
-- ARC
	type type_arc_base is abstract tagged record  -- CS should be private ?
		center			: type_point;
		start_point		: type_point;
		end_point		: type_point;
		direction		: type_direction_of_rotation := CW;
	end record;

	type type_arc is new type_arc_base with null record;
	-- CS use this type wherever a type_arc is declared unnessecarily.

	
	function round (arc : in type_arc)
		return type_arc'class;

	procedure round (arc : in out type_arc);
	

	
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

	
	
	-- Returns the distance between the start point and the center of the arc.
	function get_radius_start (
		arc : in type_arc) 
		return type_distance_positive;
	
	-- Returns the distance between the end point and the center of the arc.
	function get_radius_end (
		arc : in type_arc) 
		return type_distance_positive;

	-- Test whether the given arc is valid. The arc is valid if:
	-- - start and end point have equal distance to center
	-- - radius is greater zero
	function is_valid (
		arc : in type_arc)
		return boolean;
	
	-- Sometimes (for example with cairo) an arc must be
	-- expressed in terms of start and end angle:
	type type_arc_angles is record -- CS should be private ?
		center		: type_point;
		radius		: type_float_internal;
		angle_start	: type_rotation;
		angle_end	: type_rotation;
		direction	: type_direction_of_rotation := CW;
	end record;

	-- Returns the start and end angles of an arc.
	function to_arc_angles (arc : in type_arc) return type_arc_angles;

	-- Returns the start and end points of an arc.
	function to_arc (arc : in type_arc_angles) return type_arc'class;

	
	-- Returns the boundaries of the given arc.
	-- The arc has the given width. 
	-- The boundaries are extended by half the given width.
	function get_boundaries (
		arc			: in type_arc;
		line_width	: in type_distance_positive) 
		return type_boundaries;

	
	-- Returns true if the given point sits on the given arc.
	function on_arc (
		vector		: in type_vector;
		arc			: in type_arc)
		return boolean; 

	function on_arc (
		point		: in type_point;
		arc			: in type_arc)
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

	
	
	-- Computes the end point of an arc.
	function arc_end_point (
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



	-- This type is required when an arc or a circle is to be 
	-- split into smaller arc segments:
	type type_arcs is array (positive range <>) of type_arc;

	-- Splits an arc in 2 or 3 smaller arcs.
	-- The arc will be split by a vertical line that crosses the center.
	-- The resulting arcs have all the same center, radius  and direction as the
	-- given arc.
	function split_arc (arc_in : in type_arc) 
		return type_arcs;

	
	
-- CIRCLE
	type type_circle_base is abstract tagged record
		center			: type_point;
		radius  		: type_distance_positive := zero;
		-- CS locked : type_locked;
	end record;

	type type_circle is new type_circle_base with null record;
	-- CS use this type wherever a type_circle is declared unnessecarily.

	-- Splits a circle in 2 arcs.
	-- The circle will be split by a vertical line that crosses the center.
	-- The resulting arcs have all the same center and radius as the given circle.
	-- The resulting arcs have both direction CCW:
	function split_circle (circle_in : in type_circle) 
		return type_arcs;

	
	-- Returns the distance of point to circumfence of circle.
	-- Assumes the point is INSIDE the circle or ON the circumfence of the circle.
	-- The point must not be OUTSIDE the circle !
	function get_distance_to_circumfence (
		point	: in type_point;
		circle	: in type_circle)
		return type_distance_polar;
		
	
	-- Returns the shortest distance from the given point to the
	-- given circle. The point may be inside or outside the circle.
	-- However, the return is the distance to the circumfence of the circle.
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
	function on_circle (
		point		: in type_point;
		circle		: in type_circle)
		return boolean;

	-- Gives the status (inside/outside) of a point relative to a circle.
	-- If the point lies exactly at the circumfence then the result is "outside".
	function get_point_to_circle_status (
		point		: in type_point;
		circle		: in type_circle)
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
		return type_distance;

	-- Returns the shortest distance between a circle and an arc.
	-- If circle and arc do intersect in some way, then the return
	-- is a negative number.
	function get_distance (
		circle	: in type_circle'class;
		arc		: in type_arc'class)
		return type_distance;

	-- Returns the shortest distance between two circles.
	-- If the circles do intersect in some way, then the return
	-- is a negative number.
	function get_distance (
		circle_1	: in type_circle'class;
		circle_2	: in type_circle'class)
		return type_distance;

	
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
	-- as they occur as the line crosses the circle or the arc:
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



	
private
	type type_vector is	record
		x, y, z : type_float_internal := 0.0;
	end record;

	null_vector		: constant type_vector := (others => 0.0);
	unity_vector	: constant type_vector := (others => 1.0);
	
	type type_distance_point_line is record
		sits_on_start	: boolean := false;
		sits_on_end		: boolean := false;
		out_of_range	: boolean := true;

		-- A virtual line runs from the given point perpendicular
		-- to the given line. This is where the virtual line intersects
		-- the given line:
		--intersection	: type_point := origin; -- CS type_vector ?
		intersection	: type_vector := null_vector;
		distance		: type_float_internal := 0.0;
		direction		: type_rotation := zero_rotation;
	end record;
	

end et_geometry_2;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
