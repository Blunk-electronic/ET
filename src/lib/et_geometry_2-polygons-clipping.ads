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

	-- Polygon Clipping
	-- Weiler-Atherton
	-- https://www.geeksforgeeks.org/weiler-atherton-polygon-clipping-algorithm/
	-- https://www.cs.drexel.edu/~david/Classes/CS430/HWs/p214-weiler.pdf
	-- https://www.cs.drexel.edu/~david/Classes/CS430/Lectures/L-05_Polygons.6.pdf
	-- https://www.cs.drexel.edu/~david/Classes/CS430/Lectures/L-05_Polygons.pdf

	type type_direction is private;
	type type_intersection_node is private;

	
	-- Clips polygon A by polygon B.
	function clip (
		polygon_A	: in type_polygon'class; -- the clipped polygon
		polygon_B	: in type_polygon'class) -- the clipping polygon
		return pac_clipped.list;

	
		
	-- Two-Ears Theorem by Gary H. Meisters
	
	-- Sutherland–Hodgman Algorithm
	-- https://www.tutorialandexample.com/polygon-clipping/

private
	type type_direction is (INSIDE, OUTSIDE);

	type type_intersection_node is record
		point		: type_point;
		direction	: type_direction;
	end record;

	package pac_intersection_nodes is new doubly_linked_lists (type_intersection_node);

	
end et_geometry_2.polygons.clipping;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
