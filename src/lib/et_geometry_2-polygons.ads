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
	type type_contour_segment_shape is (LINE, ARC);

	
	type type_contour_segment (shape : type_contour_segment_shape) is record
		case shape is
			when LINE	=> segment_line : type_line;
			when ARC	=> segment_arc  : type_arc;
		end case;
	end record;


	function to_string (
		segment	: in type_contour_segment)
		return string;
	


	-- As system wide default for all kinds of polygons or contours:
	winding_default : constant type_direction_of_rotation := CCW;

	-- IMPORTANT: The segments of the polygon are assumend to be
	-- ordered as defined in constant winding_default !	
	package pac_contour_segments is new indefinite_doubly_linked_lists (type_contour_segment);
	use pac_contour_segments;
	
	
	-- Iterates the segments. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		segments	: in pac_contour_segments.list;
		process		: not null access procedure (position : in pac_contour_segments.cursor);
		proceed		: not null access boolean);


	
	type type_contour_segments (circular : boolean := false) is record
		case circular is
			when TRUE	=> circle   : type_circle;
			when FALSE	=> segments : pac_contour_segments.list;
		end case;
	end record;
	

	type type_contour is tagged record
		contour : type_contour_segments;
	end record;

	
	-- Returns the segments of a contour in human readable form:
	function to_string (
		contour	: in type_contour)
		return string;

	


	package pac_edges is new doubly_linked_lists (type_line);
	use pac_edges;
	
	-- Iterates the edges. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		edges	: in pac_edges.list;
		process	: not null access procedure (position : in pac_edges.cursor);
		proceed	: not null access boolean);

	
	type type_polygon is record
		edges : pac_edges.list;
	end record;


	
	-- Returns the winding of a polygon. 
	-- This function works with concave polygons and uses the approach discussed in
	-- https://stackoverflow.com/questions/1165647/how-to-determine-if-a-list-of-polygon-points-are-in-clockwise-order/1165943#1165943
	-- and http://blog.element84.com/polygon-winding.html
	function get_winding (
		polygon : in type_polygon)
		return type_direction_of_rotation;

	
	-- This procedure changes the winding of a polygon:
	procedure set_winding (
		polygon : in out type_polygon;
		winding	: in type_direction_of_rotation := winding_default);
		
	
	-- Returns true if the given two polygons are congruent,
	-- means if they are equal in outline and size.
	-- The start points of the two polygons does not matter.
	-- CS: Assumes the polygon consists of line-edges only !
	function are_congruent (
		polygon_A, polygon_B : in type_polygon)
		return boolean;
	
	
	-- Returns the cursor to the given edge of the
	-- given polygon. If the edge does not exist, then the
	-- return is no_element.
	-- If the polygon consist of just a circle then an exception is raised: -- CS remove
	function get_segment_edge ( -- CS rename to get_edge
		polygon	: in type_polygon;
		edge	: in type_line)
		return pac_edges.cursor;


	
	-- Returns the cursor to the edge
	-- where the given point lies on.
	-- If the given point is a vertex then an
	-- exception is raised.
	-- If the polygon consists of just a circle then an
	-- exception is raised. -- CS remove
	-- If the given point does not lie on an edge then
	-- the return is no_element:
	function get_segment_edge ( -- CS rename to get_edge
		polygon	: in type_polygon;
		point	: in type_point)
		return pac_edges.cursor;


	function get_segment (
		contour	: in type_contour;
		point	: in type_point)
		return pac_contour_segments.cursor;
	

	type type_neigboring_edges is record
		-- The edge segment before a vertex.
		-- This edge ENDS on the vertex:
		edge_1 : pac_edges.cursor;

		-- The edge segment after a vertex:
		-- This edge STARTS on the vertex:
		edge_2 : pac_edges.cursor;
	end record;

	-- Returns the cursors to the two neigboring
	-- edges of the given vertex.
	-- The given point must be a vertex. Otherwise an exception is raised.
	-- If the polygon consists of just a circle then an
	-- exception is raised.
	function get_neigboring_edges (
		polygon	: in type_polygon;
		vertex	: in type_point)
		return type_neigboring_edges;



	type type_neigboring_segments is record
		-- The segment before a vertex.
		-- This segment ENDS on the vertex:
		segment_1 : pac_contour_segments.cursor;

		-- The segment after a vertex:
		-- This segment STARTS on the vertex:
		segment_2 : pac_contour_segments.cursor;
	end record;

	function get_neigboring_segments (
		contour	: in type_contour;
		vertex	: in type_point)
		return type_neigboring_segments;

	
	
	-- Returns the edges of a polygon in human readable form:
	function to_string (
		polygon	: in type_polygon)
		return string;

	
	-- Returns the corner point nearest to the given
	-- reference point.
	-- If the given polygon consists of just a single
	-- circle then a exception is raised: -- CS remove
	function get_nearest_corner_point (
		polygon		: in type_polygon;
		reference	: in type_point)
		return type_point;
	

	-- Returns the distance from the given reference point to
	-- to the nearest point on the contour.
	-- The point may be inside or outside the contour.
	function get_shortest_distance (
		contour	: in type_contour;
		point	: in type_point)
		return type_distance_polar;


	


	
	-- Loads the given segments into the given contour.
	-- NOTE: Overwrites already existing segments in the contour.
	procedure load_segments (
		contour		: in out type_contour;
		segments	: in type_contour_segments);
	
	
	procedure delete_segments (
		contour	: in out type_contour);


	
	procedure append_segment (
		contour	: in out type_contour;
		segment	: in type_contour_segment);
	

	procedure set_circle (
		contour	: in out type_contour;
		circle	: in type_circle'class);
	
		
	function get_segments (
		contour	: in type_contour)
		return type_contour_segments;
	

	-- Returns 1 if the polygon contours consist of just a single circle.
	-- Returns the number of segments if the contours consist of lines
	-- and/or arcs:
	function get_segments_total (
		contour : in type_contour)
		return count_type;

		
	function get_edges_total (
		polygon : in type_polygon)
		return count_type;



	
	---- Transposes a polygon in Y direction.
	---- Each point of each segment gets shifted by
	---- the formula new_y = offset - old_y:
	--procedure transpose_polygon (
		--polygon	: in out type_polygon'class;
		--offset	: in type_distance);

	-- Transposes a contour in Y direction.
	-- Each point of each segment gets shifted by
	-- the formula new_y = offset - old_y:
	procedure transpose_contour (
		contour	: in out type_contour'class;
		offset	: in type_distance);


	-- Reads the segments provided in a row of
	-- arguments in a form like:
	-- "line 0 0 line 160 0 line 160 80 line 0 80"
	-- or:
	-- line 50 50 70 50 arc 60 50 70 50 50 50 ccw
	-- and builds a contour.
	-- 1. The end point of a segment must not be specified.
	--    It is deduced from the start point of the successor segment.
	-- 2. A circle can only be read if it is the only shape.
	--    Mixing a circle with lines and arcs is not allowed.
	--    There must be only one circle.
	--    Examples:
	--     - valid  : circle 9 4 10
	--     - invalid: circle 34 45 30 circle 9 4 10
	--     - invalid: line 0 0 circle 9 4 10
	function to_contour (
		arguments : in type_fields_of_line)
		return type_contour'class;
	
	
	-- Returns the boundaries of the given contour.
	function get_boundaries (
		contour		: in type_contour;
		line_width	: in type_distance_positive)
		return type_boundaries;
	

	-- A contour must have a properly closed outline.
	-- The outline check returns a list of points (where the gaps are):
	package pac_contour_gaps is new doubly_linked_lists (type_point); 

	
	-- Returns the points where gaps of a contour begin:
	function to_string (
		gaps : in pac_contour_gaps.list)
		return string;
	

	-- The result of an outline check is a parameterized type:
	type type_contour_status (closed : boolean) is record
		case closed is
			when TRUE	=> null;
			when FALSE	=> gaps : pac_contour_gaps.list;
		end case;
	end record;
				
	-- Returns true if the given contour is properly closed.
	-- If there are gaps, a list of points is returned where the gaps are.
	-- The test iterates the segments of the contour and tests whether
	-- the end point of a segment matches the start point of the next segment.
	-- CS: Special threatment for circle segments: Since a circle does not have 
	-- start and end point, only the center point of the circle must be in 
	-- the chain of segments. 
	-- CS: Improvement required: It is sufficient if the circle
	-- touches one of the other segments (lines and arcs) to regard it as connected
	-- with the contour.
	function is_closed (
		contour	: in type_contour)
		return type_contour_status;
	

	-- Moves a contour by the given offset. 
	procedure move_by (
		contour	: in out type_contour;
		offset	: in type_distance_relative);
	

	-- Mirrors a contour along the given axis.
	procedure mirror (
		contour	: in out type_contour;
		axis	: in type_axis_2d);
	

	-- Rotates a contour about the origin by the given rotation.
	procedure rotate_by (
		contour		: in out type_contour;
		rotation	: in type_rotation);

	


	--type type_polygon_scale is delta 0.1 range 0.1 .. 10.0; -- less than 1.0 -> downscaling, greater 1.0 -> upscaling
	--for type_polygon_scale'small use 0.1;
	----type type_polygon_scale is new float range 0.1 .. 10.0; -- less than 1.0 -> downscaling, greater 1.0 -> upscaling
	----for type_polygon_scale'small use 0.1;

	--polygon_scale_default : constant type_polygon_scale := 1.0;

	
	--function to_string (scale : in type_polygon_scale) return string;
	--function to_scale (scale : in string) return type_polygon_scale;



	--procedure scale_polygon (
		--polygon	: in out type_polygon;
		--scale	: in type_polygon_scale);

	



	---- In order to get the status of a point relative to
	---- a polygon we need this stuff:
	---- The general approach is:
	---- A ray that starts at point and travels in zero degees 
	---- may intersect the polygon edges.
	---- The result of such a query is the type_point_to_polygon_status
	---- that contains a status flag (inside/outside) and a list
	---- of x values where the ray intersects the polygon. For completeness
	---- the original point where the probe line has started is also provided.
	---- This list provides the x values ordered according to their
	---- distance to the start point of the ray. Lowest value first.

	type type_intersected_segment (shape : type_shape := LINE) is record
		case shape is 
			when LINE	=> segment_line : type_line;
			when ARC	=> segment_arc : type_arc;
			when CIRCLE	=> segment_circle : type_circle;
		end case;
	end record;

	




	
	-- For finding the lower left corner of a contour this type
	-- is required. The lower left corner can be a point somewhere
	-- on the edge of the contour. In that case the point is REAL.
	-- If the point is outside the contour then the point is VIRTUAL.
	type type_lower_left_corner_status is (
		REAL,	-- the corner point is somewhere on the contour
		VIRTUAL	-- the corner point is outside the contour (where
				-- the lowest x and lowest y are)
		);

	
	-- When the lower left corner is to be found, then
	-- the result of such a search operation is formed by
	-- this type:
	type type_lower_left_corner is record
		point	: type_point := origin;
		status	: type_lower_left_corner_status := REAL;
	end record;

	


	-- Returns true if the given point is a vertex
	-- of the given polygon:
	function is_vertex (
		polygon	: in type_polygon;
		point	: in type_point)
		return boolean;

	function is_vertex (
		contour	: in type_contour;
		point	: in type_point)
		return boolean;

	

	-- Searches the lower left corner of a contour:
	function get_lower_left_corner (
		contour	: in type_contour)
		return type_lower_left_corner;

	

	
private
					   
	function get_shortest_distance (
		contour	: in type_contour;
		point	: in type_vector)
		return type_float_internal;

		
	function get_shortest_distance (
		polygon	: in type_polygon;
		point	: in type_vector)
		return type_float_internal;

		
	function is_vertex (
		polygon	: in type_polygon;
		point	: in type_vector)
		return boolean;

	function get_segment_edge (
		polygon	: in type_polygon;
		point	: in type_vector)
		return pac_edges.cursor;

	
	function get_neigboring_edges (
		polygon	: in type_polygon;
		vertex	: in type_vector)
		return type_neigboring_edges;


	-- The location of a point relative to a polygon:
	type type_location is (
		ON_EDGE,
		INSIDE,
		OUTSIDE,
		ON_VERTEX);
	
	function to_string (status : in type_location) return string;


	

	-- The intersection of a probe line with the polygon side can
	-- be described as:
	type type_probe_line_intersection_contour is record
		x_position	: type_float_internal;
		angle		: type_rotation := zero_rotation;
		segment		: type_intersected_segment;
	end record;

	-- The intersection of a probe line with the polygon edge can
	-- be described as:
	type type_probe_line_intersection_polygon is record
		x_position	: type_float_internal;
		angle		: type_rotation := zero_rotation;
		edge		: type_line;
	end record;

	
	-- Subtracts 180 degree from the given angle if it is
	-- greater 90 degrees and returns the absolute value of the difference.
	-- Otherwise returns the given angle unchanged.		
	--function subtract_180_if_greater_90 (
		--angle : in type_rotation)
		--return type_rotation;

	
	function "<" (left, right : in type_probe_line_intersection_contour)
		return boolean;

	function "<" (left, right : in type_probe_line_intersection_polygon)
		return boolean;

		
	package pac_probe_line_intersections_contour is new
		doubly_linked_lists (type_probe_line_intersection_contour);

	package pac_probe_line_intersections_polygon is new
		doubly_linked_lists (type_probe_line_intersection_polygon);
		
	

	type type_point_to_polygon_status (location : type_location) is record
		-- The point where the probe line has started:
		start			: type_vector; 

		-- The intersections of the probe line with the polygon edges:
		intersections	: pac_probe_line_intersections_polygon.list;

		case location is
			when OUTSIDE | INSIDE =>
				-- The shortest distance of the start point (of the probe line)
				-- to the polygon:
				distance : type_float_internal;
				
			when ON_EDGE =>
				edge : pac_edges.cursor;

			when ON_VERTEX =>
				edges : type_neigboring_edges;

		end case;
	end record;

	
	type type_point_to_contour_status (location : type_location) is record
		-- The point where the probe line has started:
		start			: type_point;

		-- The intersections of the probe line with the polygon edges:
		intersections	: pac_probe_line_intersections_contour.list;

		case location is
			when OUTSIDE | INSIDE =>
				-- The shortest distance of the start point (of the probe line)
				-- to the polygon:
				distance : type_float_internal;
				
			when ON_EDGE =>
				edge : pac_contour_segments.cursor;

			when ON_VERTEX =>
				edges : type_neigboring_segments;

		end case;
	end record;


	
	
	-- Returns the query result as a human readable string:
	function to_string (
		i : in type_point_to_polygon_status)
		return string;

		
	function to_string (
		i : in type_point_to_contour_status)
		return string;
	

	-- Detects whether the given point is inside or outside
	-- the polygon of whether the point lies on an edge:
	function get_point_to_polygon_status (
		polygon		: in type_polygon;	
		point		: in type_vector)
		return type_point_to_polygon_status;

		
	function get_point_to_contour_status (
		contour		: in type_contour;	
		point		: in type_point)
		return type_point_to_contour_status;


		
	-- Detects whether the given point is inside or outside
	-- the polygon of whether the point lies on an edge.
	-- Similar to get_point_to_polygon_status but the result is reduced
	-- to INSIDE, OUTSIDE, ON_EDGE or ON_VERTEX:
	function get_location (
		polygon		: in type_polygon;	
		point		: in type_vector)
		return type_location;

	
	
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
		polygon	: in type_polygon;
		line	: in type_line;
		point	: in type_vector)
		return type_point_of_contact;


	
	type type_intersection_base is tagged record
		position	: type_vector;
		direction	: type_intersection_direction := ENTERING;
	end record;

	
	type type_intersection_line_edge is new type_intersection_base with record
		edge : pac_edges.cursor;
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
				edge				: pac_edges.cursor;

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
		polygon	: in type_polygon;
		line	: in type_line)
		return type_line_to_polygon_status;


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


	-- Iterates the intersections. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		intersections	: in pac_intersections.list;
		process			: not null access procedure (position : in pac_intersections.cursor);
		proceed			: not null access boolean);

	
	
	-- Returns true if x/y of the given two intersections are equal:
	function same_position (
		intersection_1, intersection_2 : in pac_intersections.cursor)
		return boolean;

	
	-- Removes from the given list of intersections those where
	-- polygon A touches polygon B. The result are those nodes where
	-- the polygon edges truly intersect each other:
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
		polygon_A	: in type_polygon; -- the clipped polygon
		polygon_B	: in type_polygon) -- the clipping polygon
		return boolean;



	
	-- The category of a vertex:
	type type_category is (
		-- A vertex as it is a part of the original polygon:
		REGULAR,

		-- The vertex is an intersection of two edges of the A and B polygon:
		INTERSECTION);

	
	-- In connection with polygon operation like clipping or union
	-- there are always two polygon we operate with. In this package 
	-- we refer to them as polygon A and polygon B.
	-- A vertex of a polygon can be described as follows:
	type type_vertex (category : type_category) is record
		position	: type_vector; -- x/y position

		case category is
			-- If the vertex is a node where two edges of the polygon
			-- intersect then it has a direction. This is a place where
			-- an edge of polygon A enters or leaves the area of polygon B:
			when INTERSECTION =>
				direction : type_intersection_direction;

			-- If the vertex is not an intersection of two edges then
			-- it is a regular vertex. It can be inside, outside, on edge
			-- or on a vertex of the other polygon:
			when REGULAR =>
				location : type_location;
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
	function is_inside (v : pac_vertices.cursor) return boolean;
	function is_outside (v : pac_vertices.cursor) return boolean;
	
	
	-- Returns true if x/y of the given two vertices are equal:
	function same_position (
		vertex_1, vertex_2 : in pac_vertices.cursor)
		return boolean;
	

	-- Converts a list of vertices to a polygon.
	-- The first vertex becomes the end point of the first edge.
	-- The last vertex becomes the start point of the first edge.
	function to_polygon (vertices : in pac_vertices.list)
		return type_polygon;



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
		CONGRUENT,
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
	

	-- Returns a list of regular vertices of the given primary
	-- polygon merged with the intersections (leaving or entering) with the secondary
	-- polygon. The returned vertices are ordered counter-clockwise.
	-- If the given primary polygon is A then the argument AB must be B.
	-- If the given primary polygon is B then the argument AB must be A.
	-- The required list of intersections should be generated by function
	-- get_intersections (see above).
	-- NOTE: The solely purpose of the argument "polygon_secondary" is to help
	-- figuring out whether a certain vertex of the primary polygon is inside, 
	-- outside, on an edge or on an vertex of the secondary polygon.
	function get_vertices (
		polygon_primary 	: in type_polygon;
		polygon_secondary	: in type_polygon;
		intersections		: in pac_intersections.list;
		AB					: in type_AB_polygon)
		return pac_vertices.list;

	
	-- Returns a cursor to the first entering/leaving intersection in
	-- given list of vertices.
	-- If no entering intersection found, returns no_element:
	function get_first (
		direction	: in type_intersection_direction;
		vertices	: in pac_vertices.list)
		return pac_vertices.cursor;


	-- Returns a cursor to the first outside vertex in
	-- given list of vertices.
	-- If no outside vertex found, returns no_element:
	function get_first (
		location	: in type_location;
		vertices	: in pac_vertices.list)
		return pac_vertices.cursor;


	-- Returns a list of all vertices according to
	-- the given location:
	function get_vertices (
		location	: in type_location;
		vertices	: in pac_vertices.list)
		return pac_vertices.list;

	
	-- Returns the vertices (of given list of vertices) after
	-- the given start vertex 
	-- to (and including) the next leaving/entering vertex. 
	-- If argument "delete_visited" is true then all
	-- these vertices and the given start vertex 
	-- are removed from vertices so that they won't be visited again.
	-- CS: Not implemented yet: Delete visited when direction of search is CW.
	function get_until (
		vertices					: in out pac_vertices.list;
		start_vertex				: in pac_vertices.cursor;
		direction_of_intersection	: in type_intersection_direction; -- entering/leaving
		direction_of_search			: in type_direction_of_rotation; -- CW, CCW
		delete_visited				: in boolean := true)
		return pac_vertices.list;


	-- Increments the safety counter. Raises constraint error if limit is
	-- reached or exceeded:

	--CS : ? subtype type_safety_counter is natural range 0 .. 10000;
	
	procedure increment_safety_counter (
		count	: in out natural;
		limit	: in natural);
	
end et_geometry_2.polygons;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
