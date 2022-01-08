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
	
	package pac_clipped is new doubly_linked_lists (type_polygon);


	type type_direction is private;
	type type_intersection is private;


	function to_string (intersection : in type_intersection)
		return string;
	
	
	-- Clips polygon A by polygon B.
	-- If there are intersections of the two polygons, then the
	-- return is an empty list.
	-- CS: What if a polygon is completely inside the other ?
	function clip (
		polygon_A	: in type_polygon'class; -- the clipped polygon
		polygon_B	: in type_polygon'class) -- the clipping polygon
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
	

	-- The category of a vertex:
	type type_category is (
		-- A vertex as it is a part of the original polygon:
		REGULAR,

		-- The vertex is an intersection of two edges of the A and B polygon:
		INTERSECTION);

	
	
	type type_vertex is record -- CS discriminant should be category
		position	: type_point;
		category	: type_category;
		direction	: type_direction; -- don't care if category is REGULAR
	end record;

	
	package pac_vertices is new doubly_linked_lists (type_vertex);
	use pac_vertices;


	function is_entering (v : pac_vertices.cursor) return boolean;
	function is_leaving (v : pac_vertices.cursor) return boolean;


	-- Converts a list of vertices to a polygon:
	function to_polygon (vertices : in pac_vertices.list)
		return type_polygon;

	
end et_geometry_2.polygons.clipping;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
