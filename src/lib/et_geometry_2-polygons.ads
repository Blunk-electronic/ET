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
with ada.containers.indefinite_doubly_linked_lists;

with et_geometry;				use et_geometry;
with et_geometry_1;
with et_string_processing;		use et_string_processing;



generic
	
package et_geometry_2.polygons is
	
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
	

	type type_polygon_base is abstract tagged record -- CS rename to type_polygon ?
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
	-- The point may be inside or outside the polygon.
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
	--type type_polygon_scale is new float range 0.1 .. 10.0; -- less than 1.0 -> downscaling, greater 1.0 -> upscaling
	--for type_polygon_scale'small use 0.1;

	polygon_scale_default : constant type_polygon_scale := 1.0;

	
	function to_string (scale : in type_polygon_scale) return string;
	function to_scale (scale : in string) return type_polygon_scale;



	procedure scale_polygon (
		polygon	: in out type_polygon_base;
		scale	: in type_polygon_scale);

	
	procedure offset_polygon (
		polygon		: in out type_polygon_base;
		offset		: in type_distance);

	


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
	type type_probe_line_intersection is record
		x_position	: type_float_internal;
		angle		: type_rotation := zero_rotation;
		segment		: type_intersected_segment;
	end record;

	
	-- Subtracts 180 degree from the given angle if it is
	-- greater 90 degrees and returns the absolute value of the difference.
	-- Otherwise returns the given angle unchanged.		
	--function subtract_180_if_greater_90 (
		--angle : in type_rotation)
		--return type_rotation;

	
	function "<" (left, right : in type_probe_line_intersection)
		return boolean;
	

	
	package pac_probe_line_intersections is new
		doubly_linked_lists (type_probe_line_intersection);

		
		
	type type_inside_polygon_query_result is record
		-- the point where the probe line has started:
		start			: type_point; 

		status			: type_point_status := OUTSIDE;		

		-- the intersections of the probe line with the polygon edges:
		intersections	: pac_probe_line_intersections.list;
	end record;

	
		
	
	-- Returns the query result as a human readable string:
	function to_string (
		i : in type_inside_polygon_query_result)
		return string;

	
	-- Detects whether the given point is inside or outside
	-- the polygon. The point is regarded as "outside" if
	-- it sits exactly on the edge of the polygon.
	function in_polygon_status (
		polygon		: in type_polygon_base;	
		point		: in type_point)
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
	function get_lower_left_corner (
		polygon	: in type_polygon_base)
		return type_lower_left_corner;


	type type_polygon is new type_polygon_base with null record;
	

end et_geometry_2.polygons;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
