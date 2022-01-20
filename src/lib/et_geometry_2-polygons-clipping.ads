------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      GEOMETRY 2 / POLYGONS / CLIPPING                    --
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
--  Description:
--
-- 	This package bases on the Weiler-Atherton algorithm. Find basics here:
-- 	- <https://www.geeksforgeeks.org/weiler-atherton-polygon-clipping-algorithm>
--  - <https://www.cs.drexel.edu/~david/Classes/CS430/HWs/p214-weiler.pdf>
--  - <https://www.cs.drexel.edu/~david/Classes/CS430/Lectures/L-05_Polygons.6.pdf>
--  - <https://www.cs.drexel.edu/~david/Classes/CS430/Lectures/L-05_Polygons.pdf>
--
--  The Weiler-Atherton algorithm has been extended/improved here to handle
--  this special case. We call it the "Shot-Through-Case" or just STC:
--  An edge of the clipped polygon (A) runs entirely through the whole 
--  clipping polygon (B). Both vertices (start and end point) of the edge are 
--  outside the clipping polygon.

--  Other approaches of interest could be:
--  - Two-Ears Theorem by Gary H. Meisters
--  - Sutherland–Hodgman Algorithm
--  - <https://www.tutorialandexample.com/polygon-clipping>
--
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
	
package et_geometry_2.polygons.clipping is

	-- The result of a polygon clipping operation is a list
	-- of polygons:
	package pac_clipped is new doubly_linked_lists (type_polygon);


	type type_direction is private;
	type type_intersection is private;


	function to_string (intersection : in type_intersection)
		return string;
	

	-- Returns true if all vertices of polygon A lie
	-- inside polygon B. If a vertex lies on an edge
	-- of polygon A then it is regarded as inside.
	function all_vertices_of_A_inside_B (
		polygon_A	: in type_polygon'class; -- the clipped polygon
		polygon_B	: in type_polygon'class) -- the clipping polygon
		return boolean;
	
	
	-- Clips polygon A by polygon B.
	-- If the two polygons do not overlap, then the return is an empty list.
	-- If debug is true then a lot of debug messages is output.
	function clip (
		polygon_A	: in type_polygon'class; -- the clipped polygon
		polygon_B	: in type_polygon'class; -- the clipping polygon
		debug		: in boolean := false)
		return pac_clipped.list;

	


private

	-- An indicator that tells whether it is about the
	-- A or the B polygon:
	type type_AB_polygon is (A, B);
	
	
	type type_direction is (
		-- The edge of the clipped polygon (A) 
		-- enters the clipping polygon (B):
		ENTERING, 

		-- The edge of the clipped polygon (A)
		-- leaves the clipping polygon (B): 
		LEAVING);

	
	type type_intersection is record
		position	: type_point; 	  -- x/y
		direction	: type_direction; -- Whether an edge of A enters or leaves B.

		-- This is supportive information. It helps
		-- to find the edges that intersect:
		edge_A		: type_line;
		edge_B		: type_line;
	end record;

	package pac_intersections is new doubly_linked_lists (type_intersection);
	use pac_intersections;


	-- Returns true if x/y of the given two intersections are equal:
	function same_position (
		intersection_1, intersection_2 : in pac_intersections.cursor)
		return boolean;

	
	-- Removes from the given list of intersections those where
	-- polygon A touches polygon B:
	function get_real_intersections (
		intersections	: in pac_intersections.list)
		return pac_intersections.list;

	
	-- Returns true if the given intersection have equal
	-- position and direction:
	function are_redundant (
		i1, i2 : in pac_intersections.cursor)
		return boolean;
	

	
	subtype type_redundant_intersection_count is natural range 0 .. 2; -- CS correct ?

	-- Returns the number of redundant intersections.
	-- intersections are redundant if their position and direction 
	-- are equally:
	function count (
		intersections	: in pac_intersections.list;
		intersection	: in type_intersection)
		return type_redundant_intersection_count;


	
	-- The category of a vertex:
	type type_category is (
		-- A vertex as it is a part of the original polygon:
		REGULAR,

		-- The vertex is an intersection of two edges of the A and B polygon:
		INTERSECTION);

	
	
	type type_vertex (category : type_category) is record
		position	: type_point;
		case category is
			when INTERSECTION =>	direction	: type_direction;
			when REGULAR => 		null;
		end case;
	end record;

	
	package pac_vertices is new indefinite_doubly_linked_lists (type_vertex);
	use pac_vertices;


	function is_entering (v : pac_vertices.cursor) return boolean;
	function is_leaving (v : pac_vertices.cursor) return boolean;
	function is_regular (v : pac_vertices.cursor) return boolean;
	
	
	-- Returns true if x/y of the given two vertices are equal:
	function same_position (
		vertex_1, vertex_2 : in pac_vertices.cursor)
		return boolean;

	
	-- Converts a list of vertices to a polygon:
	function to_polygon (vertices : in pac_vertices.list)
		return type_polygon;

	
end et_geometry_2.polygons.clipping;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
