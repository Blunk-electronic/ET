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
--
--
--  description:
-- 	This package and its child packages base on the Weiler-Atherton algorithm. 
--  Find basics here:
-- 	- <https://www.geeksforgeeks.org/weiler-atherton-polygon-clipping-algorithm>
--  - <https://www.cs.drexel.edu/~david/Classes/CS430/HWs/p214-weiler.pdf>
--  - <https://www.cs.drexel.edu/~david/Classes/CS430/Lectures/L-05_Polygons.6.pdf>
--  - <https://www.cs.drexel.edu/~david/Classes/CS430/Lectures/L-05_Polygons.pdf>
--
--   history of changes:
--

--   to do:
--   - Some functions and procedures here can be applied to et_geometry_1.type_line.
--     If required, move them to et_geometry_1 so that other callers can make use of them.


with ada.containers; 			use ada.containers;
with ada.containers.ordered_sets;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;


generic
	
package et_geometry_1.et_polygons is


	--pragma suppress (all_checks);
	
	
	-- As system wide default for all polygons.
	winding_default : constant type_direction_of_rotation := CCW;

	-- Use subprograms set_winding and get_winding to inquire or modify the winding.
	-- IMPORTANT: For all other subprograms here the winding
	-- of a given polygon MUST BE CCW !

	
	type type_edge is new type_line;

	edge_length_min : constant type_float_internal_positive := 0.1;
	
	
	overriding function to_string (
		edge : in type_edge)
		return string;



	
	package pac_edges is new doubly_linked_lists (type_edge);
	use pac_edges;

	
	-- Iterates the edges. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		edges	: in pac_edges.list;
		process	: not null access procedure (position : in pac_edges.cursor);
		proceed	: not null access boolean);


	-- The mode at which an arc is approximated by many edges:
	type type_approximation_mode is (
		-- The edges are inside, between arc and arc center.
		-- The start and end points of the edges are on the given arc:
		SHRINK,		

		-- The edges are outside.
		-- The start and end points of the edges are on a virtual
		-- outer arc. The outer arc radius equals to arc.radius + tolerance:
		EXPAND);	
	

	-- Converts an arc to a list of edges.
	-- Approximates the arc by many edges according to the given mode.
	-- The accuracy is determined by the given tolerance.
	-- The tolerance is the maximum allowed deviation from
	-- the ideal arc.
	-- The edges start on the start point of the given arc and
	-- end on the end point of the given arc:
	function to_edges (
		arc			: in type_arc;
		tolerance	: in type_float_internal_positive;
		mode		: in type_approximation_mode;
		debug		: in boolean := false)				  
		return pac_edges.list;

	

	-- A polygon in this world is a list of edges.
	-- For fast and rough tests (overlapping, inside, outside status)
	-- the boundaries are sometimes useful. Boundaries provide
	-- the greatest and smallest x and y value of the polygon.
	-- Call procedure update_boundaries if the boundaries are required:
	type type_polygon is record
		edges		: pac_edges.list;
		boundaries	: type_boundaries;
	end record;


	-- Updates the selector "boundaries" of the given polygon:
	procedure update_boundaries (
		polygon	: in out type_polygon);

	
	

	-- Rotates the edges of a polygon according to the given direction.
	-- NOTE: The polygon does not change its appearance. Only the order
	-- of the edges is changed !
	procedure rotate (
		polygon 	: in out type_polygon;
		direction	: in type_direction_of_rotation := CCW);



	type type_clean_up is new boolean;
	
	
	-- Merges successive edges that have the same direction
	-- to a single edge:
	procedure optimize_edges (
		polygon : in out type_polygon;
		debug	: in boolean := false);

	function optimize_edges (
		polygon : in type_polygon;
		debug	: in boolean := false)
		return type_polygon;


	-- Successive edges of a polygon may overlap.
	-- This is a result of polygon operations or contours drawn carelessly by the operator.
	-- These mishaps must be ironed out.
	-- This procedure merges successive edges overlaping each other
	-- and running in opposide directions to a single edge.
	-- As a final clean-up measure the procedure optimize_edges is
	-- called:
	procedure merge_overlapping_edges (
		polygon : in out type_polygon;
		debug	: in boolean := false);
	
	function merge_overlapping_edges (
		polygon : in type_polygon;
		debug	: in boolean := false)
		return type_polygon;


	-- If polygons are to be stored, then use this package:
	package pac_polygon_list is new doubly_linked_lists (type_polygon);


	-- Updates the boundaries of the given list of polygons:
	procedure update_boundaries (
		polygons : in out pac_polygon_list.list);
		

	-- Returns true if:
	-- - number of polygons in left equals those in right AND
	-- - the polygons in left are in the same order as those in right AND
	-- - the polygons in left are congruent to those in right:
	function "=" (
		left, right : in pac_polygon_list.list)
		return boolean;


	
	
	-- Iterates the polygons. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		holes	: in pac_polygon_list.list;
		process	: not null access procedure (position : in pac_polygon_list.cursor);
		proceed	: not null access boolean);
	
	

	-- A polygon requires at least 3 vertices. 
	-- Use this string when outputting error messages related to 
	-- this requirement:
	error_message_too_few_vertices : constant string := "At least 3 vertices required to create a polygon !";
	
	-- Converts a list of vertices to a polygon.
	-- The first vertex becomes the end point of the first edge.
	-- The last vertex becomes the start point of the first edge.
	-- The vertices must be given in a form like:
	-- "0.0 0.0   1.0 0.0   1.0 1.0   0.0 1.0" (This example describes a square);
	-- Successive redundant vertices will be ignored. 
	-- So "0.0 0.0   7.0 0.0  7.0 0.0   1.0 1.0   0.0 1.0" will be handled like
	-- "0.0 0.0   7.0 0.0   1.0 1.0   0.0 1.0"
	-- Raises exception if less than 3 vertices are given.
	-- If clean_up is true (default), then successive edges running
	-- into the same or opposide direction will be merged:
	function to_polygon (
		vertices	: in string;
		clean_up	: in type_clean_up := true)
		return type_polygon;

	
	-- Converts a list of location vectors to a polygon.
	-- Successive redundant vertices will be ignored:
	-- Raises exception if less than 3 vertices are given.
	-- If clean_up is true (default), then successive edges running
	-- into the same or opposide direction will be merged:
	function to_polygon (
		vectors		: in pac_vectors.list;
		clean_up	: in type_clean_up := true)					
		return type_polygon;


	-- Returns the vertices of a polygon in a list of location vectors:
	function get_vertices (polygon : in type_polygon)
		return pac_vectors.list;


	-- Mirrors a polygon along the given axis:
	procedure mirror_polygon (
		polygon	: in out type_polygon;
		axis	: in type_axis_2d);
	

	-- Rotates a polygon about the given center by the given angle:
	function rotate (
		polygon	: in type_polygon;
		center	: in type_vector;
		angle	: in type_angle)
		return type_polygon;

	
	-- Returns the boundaries of the given polygon.
	function get_boundaries (
		polygon : in type_polygon)
		return type_boundaries;

	
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
	-- The start points of the two polygons do not matter.
	function are_congruent (
		polygon_A 	: in type_polygon;
		polygon_B 	: in type_polygon;
		debug		: in boolean := false)
		return boolean;
	
	
	-- Returns the cursor to the given edge of the
	-- given polygon. If the edge does not exist, then the
	-- return is no_element.
	function get_edge (
		polygon	: in type_polygon;
		edge	: in type_edge)
		return pac_edges.cursor;


	-- Returns the cursor to the shortest edge of the given polygon:
	function get_shortest_edge (
		polygon	: in type_polygon)
		return pac_edges.cursor;
	

	-- Returns the length of the shortest edge of the given polygon:
	function get_shortest_edge (
		polygon	: in type_polygon)
		return type_float_internal_positive;


	-- Checks the length of the edges of the given polygon.
	-- If an edge is shorter than edge_length_min a warning is issued
	-- to the current output:
	procedure check_length (
		polygon	: in type_polygon);
	
		
	

	type type_neigboring_edges is record
		-- The edge segment before a vertex.
		-- This edge ENDS on the vertex:
		edge_1 : pac_edges.cursor;

		-- The edge segment after a vertex:
		-- This edge STARTS on the vertex:
		edge_2 : pac_edges.cursor;
	end record;


	
	
	-- Returns the edges of a polygon in human readable form:
	function to_string (
		polygon	: in type_polygon)
		return string;


		
	function get_edges_total (
		polygon : in type_polygon)
		return count_type;


			   
		
	function get_shortest_distance (
		polygon	: in type_polygon;
		point	: in type_vector)
		return type_float_internal;

		
	function is_vertex (
		polygon	: in type_polygon;
		point	: in type_vector)
		return boolean;


	-- Returns the edge that runs across the given point:
	function get_edge (
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




	type type_point_status (location : type_location) is record
		-- The point where the probe line has started.
		-- It is the point that was passed to function get_point_status.
		start			: type_vector; 

		-- The intersections of the probe line with the polygon edges.
		-- If the probe line starts on an edge or on a vertex, then the
		-- first intersection in this list is the start point of the probe line.
		x_intersections	: pac_float_numbers.list;

		case location is
			when OUTSIDE | INSIDE =>
				-- The shortest distance of the start point (of the probe line)
				-- to the polygon:
				distance : type_float_internal; -- CS type_float_internal_positive ?
				
			when ON_EDGE =>
				edge : pac_edges.cursor;

			when ON_VERTEX =>
				edges : type_neigboring_edges;

		end case;
	end record;

	
	
	-- Returns the query result as a human readable string:
	function to_string (
		i : in type_point_status)
		return string;


	-- Returns the intersections of the probe line with the 
	-- polygon edges. Since the probe line is a horizontal line, that runs
	-- toward the right infinitely, all y-values are equal. The y-value
	-- assumes the y-value of the start point.
	-- Since this is a 2D world, all z-values are zero.
	-- The argument "after" specifies the x-value on the
	-- probe line where the extraction starts. x-values greater
	-- than "after" are extracted.
	-- Likewise the argument "before". x-Values less than "before" are extracted.
	function get_intersections (
		status	: in type_point_status;
		after	: in type_float_internal := type_float_internal'first;
		before	: in type_float_internal := type_float_internal'last)
		return pac_vectors.list;

	

	-- Detects whether the given point is inside or outside
	-- the polygon or whether the point lies on an edge.
	-- See details in body of this function:
	function get_point_status (
		polygon	: in type_polygon;	
		point	: in type_vector;
		debug	: in boolean := false)
		return type_point_status;

		


		
	-- Detects whether the given point is inside or outside
	-- the polygon or whether the point lies on an edge.
	-- Similar to get_point_status but the result is reduced
	-- to just INSIDE, OUTSIDE, ON_EDGE or ON_VERTEX:
	function get_location (
		polygon	: in type_polygon;	
		point	: in type_vector)
		return type_location;

	

	
	type type_intersection_direction is (
		-- The edge of the clipped polygon (A) 
		-- enters the clipping polygon (B):
		ENTERING, 

		-- The edge of the clipped polygon (A)
		-- leaves the clipping polygon (B): 
		LEAVING);

	
	procedure toggle_direction (
		d : in out type_intersection_direction);

	

	
	type type_intersection_base is tagged record
		position	: type_vector;
		direction	: type_intersection_direction := ENTERING;
	end record;

	
	type type_intersection_line_edge is new type_intersection_base with record
		-- The intersected edge of the polygon.
		-- If intersection is on a vertex, then this is the edge after the vertex.
		edge : pac_edges.cursor; --  CS really required ?		
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
		reference		: in type_vector);
	


	
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
				edge				: pac_edges.cursor; -- CS really required ?

			when ON_VERTEX =>
				-- The two neigboring edges that are 
				-- connected by the start or end point:
				edges				: type_neigboring_edges;  -- CS really required ?

		end case;
	end record;


	function to_string (
		line_end : in type_line_end)
		return string;
	
		
	type type_edge_status is record
		-- The affected edge itself:
		edge		: type_edge;
		
		-- The properties of the start and end point of the line:
		start_point	: type_line_end;
		end_point	: type_line_end;

		-- The intersections (incl position, intersected edge and direction)
		-- with the polygon BETWEEN start and end point of the line.
		-- Edges that overlap the line are ignored.
		intersections : pac_line_edge_intersections.list; 
	end record;

	
	function to_string (
		status	: in type_edge_status)
		return string;

	
	package pac_edge_status_list is new doubly_linked_lists (type_edge_status);


	-- Returns the edge-to-polygon status before the given candidate status.
	-- If the candidate is the first in status_list then the last status of the
	-- status_list will be returned:
	function get_previous_status (
		status_list	: in pac_edge_status_list.list;
		candidate	: in pac_edge_status_list.cursor)
		return pac_edge_status_list.cursor;
	

	function get_next_status (
		status_list	: in pac_edge_status_list.list;
		candidate	: in pac_edge_status_list.cursor)
		return pac_edge_status_list.cursor;

	
	-- Returns the first edge that
	-- - starts outside or 
	-- - enters the outside:
	-- CS: incomplete, probably no need ?
	--function get_first_outside (
		--polygon	: in type_polygon;
		--edges	: in pac_edge_status_list.list)		
		--return pac_edge_status_list.cursor;
	

	
	type type_section is (FIRST, LAST);

	-- Returns the location of the first or last
	-- section of the given edge-status. If the status does not
	-- contain any intersections then just the section between 
	-- start and end point is investigated:
	function get_section_location (
		polygon			: in type_polygon;
		status_cursor	: in pac_edge_status_list.cursor;
		section			: in type_section)		
		return type_location;


	
	function get_edge_status (
		polygon	: in type_polygon;
		edge	: in type_edge)
		return type_edge_status;


	-- An indicator that tells whether it is about the
	-- A or the B polygon when a clip or union operation is
	-- performed:
	type type_AB_polygon is (A, B);


	type type_intersection is new type_intersection_base with record
		-- In addition to x/y position and direction,
		-- this is supportive information that tells which
		-- edges are actually intersecting each other:
		edge_A		: type_edge;
		edge_B		: type_edge;
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
	--function get_real_intersections (
		--intersections	: in pac_intersections.list)
		--return pac_intersections.list;
	

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
	-- there are always two polygons we are operating with. In this package 
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
		reference	: in type_vector);

	

	function is_entering (v : pac_vertices.cursor) return boolean;
	function is_leaving (v : pac_vertices.cursor) return boolean;
	function is_regular (v : pac_vertices.cursor) return boolean;
	function is_inside (v : pac_vertices.cursor) return boolean;
	function is_outside (v : pac_vertices.cursor) return boolean;
	

	

	-- Removes successive vertices which have the same x/y position.
	-- Example: 
	-- given vertices: 0/0 0/0 3/4 -4/9 -4/9
	-- result        : 0/0 4/4 -4/9
	procedure remove_redundant_positions (vertices : in out pac_vertices.list);

	
	-- Converts a list of vertices to a polygon.
	-- The first vertex becomes the end point of the first edge.
	-- The last vertex becomes the start point of the first edge.
	-- If clean_up is true (default), then successive edges running
	-- into the same or opposide direction will be merged:
	function to_polygon (
		vertices : in pac_vertices.list;
		clean_up : in type_clean_up := true)
		return type_polygon;


	
	-- Searches for intersections of the given two polygons.
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
		-- NOTE: This includes the case when edges overlap each other.
		--       The polygons just touch each other via a common edge.

		A_DOES_NOT_OVERLAP_B);


	-- For lists of overlap statuses:
	package pac_overlap_status is new ordered_sets (type_overlap_status);

	
	
	-- Returns the overlap status of polygon A and B.
	-- Use function get_intersections to generate the required
	-- intersections of edges.
	function get_overlap_status (
		polygon_A, polygon_B	: in type_polygon;
		intersections			: in pac_intersections.list;
		debug					: in boolean := false)
		return type_overlap_status;


	-- Returns the overlap status of polygon A and B.
	-- Computes the required intersections by its own, thus
	-- consuming more computation power and time. If intersections
	-- of edges are already available, use function get_overlap_status
	-- above instead:
	function get_overlap_status (
		polygon_A, polygon_B	: in type_polygon;
		debug					: in boolean := false)
		return type_overlap_status;

	
	-- Returns from the given list of polygons those which
	-- have a given overlap status relative to the area.
	-- The overlap status here is a set of statuses.
	-- Removes the affected polygons from the given list
	-- if argument "delete" is true:
	function get_polygons (
		area		: in type_polygon; -- polygon A
		polygons	: in out pac_polygon_list.list; -- B-polygons
		status		: in pac_overlap_status.set; -- B_INSIDE_A, A_OVERLAPS_B, ...
		delete		: in boolean := true)
		return pac_polygon_list.list;
	

	-- Returns a list of vertices of the given primary
	-- polygon merged with the intersections (leaving or entering) with the secondary
	-- polygon. The returned vertices are ordered counter-clockwise.
	-- If the given primary polygon is A then the argument AB must be B.
	-- If the given primary polygon is B then the argument AB must be A.
	-- The required list of intersections should be generated by function
	-- get_intersections (see above).
	-- NOTE: The solely purpose of the argument "polygon_secondary" is to help
	-- figuring out whether a certain vertex of the primary polygon is inside, 
	-- outside, on an edge or on a vertex of the secondary polygon.
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


	-- Converts a line with a given width to a polygon
	-- with round caps on the line ends:
	function to_polygon (
		line		: in type_line;
		linewidth	: in type_float_internal_positive;
		tolerance	: in type_float_internal_positive;
		mode		: in type_approximation_mode)
		return type_polygon;
	

	-- Converts an arc with a given width to a polygon
	-- with round caps on the arc ends:
	function to_polygon (
		arc			: in type_arc;
		linewidth	: in type_float_internal_positive;
		tolerance	: in type_float_internal_positive;
		mode		: in type_approximation_mode)
		return type_polygon;


	-- Returns the distance of a given point in given 
	-- direction to the border. The function assumes
	-- that the point is INSIDE the polygon !
	-- If the point is outside of the polygon, then an exception
	-- will be raised.
	-- The function builds a ray that starts at point and runs
	-- into the given direction. The iteration stops at the first 
	-- edge that intersects the ray.
	function get_distance_to_border (
		polygon		: in type_polygon;
		point		: in type_vector;
		direction	: in type_angle)
		return type_float_internal_positive;
	
end et_geometry_1.et_polygons;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
