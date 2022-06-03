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
--	- "to clip" german: kappen, begrenzen.
--    Im Zusammenhang mit zwei Polygonen A und B: 
--			- "ueberlappende/gemeinsame Bereiche beider Polygone ermitteln"
--			- "Schnittmenge zweier Polygone ermitteln"

generic
	
package et_geometry_2.polygons.clipping is

	-- The result of a polygon clipping operation is a list
	-- of sub-polygons:
	package pac_clipped is new doubly_linked_lists (type_polygon);

	
	-- Clips polygon A by polygon B.
	-- If the two polygons do not overlap, then the return is an empty list.
	-- If debug is true then a lot of debug messages is output.
	-- To describe the operation in another way: The result is the
	-- area shared by polygon A and polygon B.
	-- In general it does not matter which polygon is clipping and which
	-- is being clipped. The resulting area will be the same but the order
	-- of sub-polygons and their vertices will differ.
	function clip (
		polygon_A	: in type_polygon; -- the clipped polygon
		polygon_B	: in type_polygon; -- the clipping polygon
		debug		: in boolean := false)
		return pac_clipped.list;


	
end et_geometry_2.polygons.clipping;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
