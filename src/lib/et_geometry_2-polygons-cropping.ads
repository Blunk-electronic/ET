------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      GEOMETRY 2 / POLYGONS / CROPPING                    --
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
--
--   history of changes:
--


generic
	
package et_geometry_2.polygons.cropping is

	-- The result of a polygon cropping operation is a list
	-- of sub-polygons:
	package pac_cropped is new doubly_linked_lists (type_polygon);


	type type_crop (exists : boolean := true) is record
		case exists is
			when TRUE => crop : pac_cropped.list;
			when FALSE => null;
		end case;
	end record;
	

	-- CS
	-- function "=" (
	--	left, right : in type_crop)
	-- return boolean;
	-- -- Use function are_congruent to compare the polygon edges
	
	
	-- Crops polygon A by polygon B:
	function crop (
		polygon_A	: in type_polygon'class; -- the cropping polygon
		polygon_B	: in type_polygon'class; -- the cropped polygon
		debug		: in boolean := false)
		return type_crop;


	
	
	
end et_geometry_2.polygons.cropping;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
