------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      GEOMETRY 2 / POLYGONS                               --
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
	-- like lines, arcs or a single circle that form a polygonal circuit.
	-- If a polygon consist of just a single circle then no other 
	-- segments are allowed.
	-- On the other hand, a polygon may consist of lines and arcs. In that
	-- case no circle is allowed:
	type type_polygon_segment_shape is (LINE, ARC);

	
	type type_polygon_segment (shape : type_polygon_segment_shape) is record
		case shape is
			when LINE	=> segment_line : type_line;
			when ARC	=> segment_arc  : type_arc;
		end case;
	end record;


	function to_string (
		segment	: in type_polygon_segment)
		return string;
	

	-- IMPORTANT: The segments of the polygon are assumend to be
	-- ordered counter-clock-wise !
	package pac_polygon_segments is new indefinite_doubly_linked_lists (type_polygon_segment);
	
	
	
	-- Iterates the segments. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		segments	: in pac_polygon_segments.list;
		process		: not null access procedure (position : in pac_polygon_segments.cursor);
		proceed		: not null access boolean);


	
	type type_polygon_segments (circular : boolean := false) is record
		case circular is
			when TRUE	=> circle   : type_circle;
			when FALSE	=> segments : pac_polygon_segments.list;
		end case;
	end record;
	

	type type_polygon_base is abstract tagged record -- CS rename to type_polygon ?
		contours	: type_polygon_segments;
	end record;

	
	-- Returns the cursor to the given edge of the
	-- given polygon. If the edge does not exist, then the
	-- return is no_element.
	-- If the polygon consist of just a circle then an exception is raised:
	function get_segment_edge (
		polygon	: in type_polygon_base;
		edge	: in type_line)
		return pac_polygon_segments.cursor;
	

	function get_segment_edge (
		polygon	: in type_polygon_base;
		point	: in type_vector)
		return pac_polygon_segments.cursor;

	
	-- Returns the cursor to the edge
	-- where the given point lies on.
	-- If the given point is a vertex then an
	-- exception is raised.
	-- If the polygon consists of just a circle then an
	-- exception is raised.
	-- If the given point does not lie on an edge then
	-- the return is no_element:
	function get_segment_edge (
		polygon	: in type_polygon_base;
		point	: in type_point)
		return pac_polygon_segments.cursor;

	

	type type_neigboring_edges is record
		-- The edge segment before a vertex.
		-- This edge ENDS on the vertex:
		edge_1 : pac_polygon_segments.cursor;

		-- The edge segment after a vertex:
		-- This edge STARTS on the vertex:
		edge_2 : pac_polygon_segments.cursor;
	end record;

	-- Returns the cursors to the two neigboring
	-- edges of the given vertex.
	-- The given point must be a vertex. Otherwise an exception is raised.
	-- If the polygon consists of just a circle then an
	-- exception is raised.
	function get_neigboring_edges (
		polygon	: in type_polygon_base;
		vertex	: in type_point)
		return type_neigboring_edges;


	
	
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

	function get_shortest_distance (
		polygon	: in type_polygon_base;
		point	: in type_vector)
		return type_float_internal;

	
	
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

	



	-- In order to get the status of a point relative to
	-- a polygon we need this stuff:
	-- The general approach is:
	-- A ray that starts at point and travels in zero degees 
	-- may intersect the polygon edges.
	-- The result of such a query is the type_point_to_polygon_status
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



	-- The location of a point relative to a polygon:
	type type_location is (
		ON_EDGE,
		INSIDE,
		OUTSIDE,
		ON_VERTEX);
	
	
	function to_string (status : in type_location) return string;



	type type_point_to_polygon_status (location : type_location) is record
		-- The point where the probe line has started:
		start			: type_vector; 

		-- The intersections of the probe line with the polygon edges:
		intersections	: pac_probe_line_intersections.list;

		case location is
			when OUTSIDE | INSIDE =>
				-- The shortest distance of the start point (of the probe line)
				-- to the polygon:
				distance		: type_float_internal;
				
			when ON_EDGE =>
				edge : pac_polygon_segments.cursor;

			when ON_VERTEX =>
				edges : type_neigboring_edges;

		end case;
	end record;

	
		
	
	-- Returns the query result as a human readable string:
	function to_string (
		i : in type_point_to_polygon_status)
		return string;

	

	-- Detects whether the given point is inside or outside
	-- the polygon of whether the point lies on an edge:
	function get_point_to_polygon_status (
		polygon		: in type_polygon_base;	
		point		: in type_vector)
		return type_point_to_polygon_status;

	
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


	-- Returns true if the given point is a vertex
	-- of the given polygon:
	function is_vertex (
		polygon	: in type_polygon_base;
		point	: in type_point)
		return boolean;

	function is_vertex (
		polygon	: in type_polygon_base;
		point	: in type_vector)
		return boolean;




	
	subtype type_line_center is type_location range ON_EDGE .. OUTSIDE;
	
	type type_intersection_direction is (
		-- The edge of the clipped polygon (A) 
		-- enters the clipping polygon (B):
		ENTERING, 

		-- The edge of the clipped polygon (A)
		-- leaves the clipping polygon (B): 
		LEAVING);

	
	procedure toggle_direction (
		d : in out type_intersection_direction);




	-- When a line intersects an edge of a polygon,
	-- or when a line runs through a vertex of a polygon
	-- then this type tells whether it is a real intersection
	-- or just a touch point:
	type type_point_of_contact (is_intersection : boolean) is record
		case is_intersection is
			when TRUE =>
				direction : type_intersection_direction;

			when FALSE =>
				null;
		end case;
	end record;

	-- Returns the direction of a supposed intersection of
	-- the given line on the given point.
	-- The given point:
	-- - must lie on the given line,
	-- - must lie on an edge or a vertex of the given polygon
	-- If the given point is an intersection, then its direction is
	-- returned.
	function get_direction (
		polygon	: in type_polygon_base;
		line	: in type_line;
		point	: in type_vector)
		return type_point_of_contact;




	type type_intersection_base is tagged record
		--position	: type_point; 	  -- x/y
		position	: type_vector;
		direction	: type_intersection_direction := ENTERING;
	end record;

	
	type type_intersection_line_edge is new type_intersection_base with record
		edge		: pac_polygon_segments.cursor;
	end record;

	package pac_line_edge_intersections is new doubly_linked_lists (type_intersection_line_edge);

	-- CS iterator procedure for pac_line_edge_intersections
	
	
	-- Returns true if the given list of line-edge-intersections contains
	-- a line-edge-intersection that lies on the given place:
	function contains (
		intersections	: in pac_line_edge_intersections.list;
		place			: in type_vector)
		return boolean;
	
		
	-- Sorts the list of intersections by the distance of the intersections
	-- to the given reference point. Nearest comes first:
	procedure sort_by_distance (
		intersections	: in out pac_line_edge_intersections.list;
		reference		: in type_point);
	
		

	-- When a line runs through a polygon, then the start and end point
	-- can lie inside, outside, on a vertex or on an edge. Depending
	-- on this location the start or end point gets more properties.
	-- This type is applied to both the start and the end point of the line,
	-- despite of the confusing name "type_line_end":
	type type_line_end (location : type_location := OUTSIDE) is record
		case location is
			when OUTSIDE | INSIDE => null;
			
			when ON_EDGE =>
				-- The edge where the line starts or ends:
				edge				: pac_polygon_segments.cursor;

				-- The direction of the line: Whether it is
				-- entering or leaving the polygon on the start or end point:
				direction_on_edge	: type_intersection_direction;

			when ON_VERTEX =>
				-- The two neigboring edges that are 
				-- connected by the start or end point:
				edges				: type_neigboring_edges;

				-- The direction of the line: Whether it is
				-- entering or leaving the polygon at the start or end point:
				direction_on_vertex	: type_intersection_direction;
		end case;
	end record;
	
		
	type type_line_to_polygon_status is record
		-- The properties of the start and end point of the line:
		start_point	: type_line_end;
		end_point	: type_line_end;

		-- The intersections with the polygon BETWEEN start and 
		-- end point of the line:
		intersections : pac_line_edge_intersections.list;

		-- The location of the center of the line:
		center_point : type_line_center := OUTSIDE;		
		-- NOTE: Valid only if there are NO intersections !
	end record;
	

	-- Returns true if the given two statuses are equal.
	-- The x,y,z components of intersections are regarded as equal if
	-- their difference is less or equal the rounding_threshold:
	function equals (left, right : in type_line_to_polygon_status)
		return boolean;
	

	function get_line_to_polygon_status (
		polygon	: in type_polygon_base;
		line	: in type_line)
		return type_line_to_polygon_status;

	
	
	type type_polygon is new type_polygon_base with null record;




	
private
	-- An indicator that tells whether it is about the
	-- A or the B polygon when a clip or union operation is
	-- performed:
	type type_AB_polygon is (A, B);


	type type_intersection is new type_intersection_base with record
		-- This is supportive information. It helps
		-- to find the edges that intersect:
		edge_A		: type_line;
		edge_B		: type_line;
	end record;

	
	function to_string (intersection : in type_intersection)
		return string;

	
	package pac_intersections is new doubly_linked_lists (type_intersection);
	use pac_intersections;

	
	
	-- Returns true if x/y of the given two intersections are equal:
	function same_position (
		intersection_1, intersection_2 : in pac_intersections.cursor)
		return boolean;

	
	-- Removes from the given list of intersections those where
	-- polygon A touches polygon B.
	function get_real_intersections (
		intersections	: in pac_intersections.list)
		return pac_intersections.list;
	

	-- Returns true if the given intersection have equal
	-- position and direction:
	function are_redundant (
		i1, i2 : in pac_intersections.cursor)
		return boolean;


	
	
	-- Returns true if all vertices of polygon A lie
	-- inside polygon B. If a vertex lies on an edge
	-- of polygon A then it is regarded as inside.
	function all_vertices_of_A_inside_B (
		polygon_A	: in type_polygon'class; -- the clipped polygon
		polygon_B	: in type_polygon'class) -- the clipping polygon
		return boolean;



	
	-- The category of a vertex:
	type type_category is (
		-- A vertex as it is a part of the original polygon:
		REGULAR,

		-- The vertex is an intersection of two edges of the A and B polygon:
		INTERSECTION);

	
	
	type type_vertex (category : type_category) is record
		position	: type_vector;
		case category is
			when INTERSECTION =>	direction	: type_intersection_direction;
			when REGULAR => 		null;
		end case;
	end record;


	function to_string (vertex : in type_vertex)
		return string;

	
	package pac_vertices is new indefinite_doubly_linked_lists (type_vertex);
	use pac_vertices;


	function to_string (vertices : in pac_vertices.list) 
		return string;


	procedure sort_by_distance (
		vertices	: in out pac_vertices.list;
		reference	: in type_point);

	

	function is_entering (v : pac_vertices.cursor) return boolean;
	function is_leaving (v : pac_vertices.cursor) return boolean;
	function is_regular (v : pac_vertices.cursor) return boolean;
	
	
	-- Returns true if x/y of the given two vertices are equal:
	function same_position (
		vertex_1, vertex_2 : in pac_vertices.cursor)
		return boolean;
	

	-- Converts a list of vertices to a polygon:
	function to_polygon (vertices : in pac_vertices.list)
		return type_polygon'class;



	-- Searches for intersections of the given two polygons
	-- and stores them in container "intersection".
	-- An intersection is where the edge of polygon A intersects
	-- an edge of polygon B. The intersection has direction "entering"
	-- if the start point of the edge (of polygon A) is outside of polygon B.
	-- In other words, if polygon A enters polygon B.
	-- If there are no intersections then the result is an empty list:
	function get_intersections (
		polygon_A	: in type_polygon;
		polygon_B	: in type_polygon;
		debug		: in boolean := false)
		return pac_intersections.list;
	

	
	type type_overlap_status is (
		A_INSIDE_B,
		B_INSIDE_A,							
		A_OVERLAPS_B,
		A_DOES_NOT_OVERLAP_B);

	function get_overlap_status (
		polygon_A, polygon_B	: in type_polygon;
		intersections			: in pac_intersections.list;
		debug					: in boolean := false)
		return type_overlap_status;


	-- When the start point of an edge lies on an edge of the other polygon
	-- then we got a regular vertex right AFTER an intersection. Both have 
	-- the same x/y-position. The regular vertex must be deleted 
	-- so that just the intersection is left:
	procedure delete_regular_after_intersection (
		vertices : in out pac_vertices.list);

	
	-- When the start point of an edge lies on an edge of the other polygon
	-- then we got a regular vertex right BEFORE an intersection. Both have 
	-- the same x/y-position. The regular vertex must be deleted 
	-- so that just the intersection is left:
	procedure delete_regular_before_intersection (
		vertices : in out pac_vertices.list);
	

	-- Returns a list of regular vertices of the given 
	-- polygon merged with the intersections (leaving or entering) with the other
	-- polygon. The returned vertices are ordered counter-clockwise.
	-- If the given polygon is A then the argument AB must be B.
	-- If the given polygon is B then the argument AB must be A.
	-- The required list of intersections should be generated by function
	-- get_intersections (see above):
	function get_vertices (
		polygon			: in type_polygon;
		intersections	: in pac_intersections.list;
		AB				: in type_AB_polygon)
		return pac_vertices.list;
	
end et_geometry_2.polygons;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
