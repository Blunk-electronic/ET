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
--	- "to crop" german: stutzen, abschneiden
--    Im Zusammenhang zwei Polygonen A und B: 
--			- "den ueberlappenden Bereich beider Polygone ermitteln
--			  und diesen vom zu bescheidenen Polygon B abziehen"
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
		polygon_A	: in type_polygon; -- the cropping polygon
		polygon_B	: in type_polygon; -- the cropped polygon / zu bescheidendes Polygon
		debug		: in boolean := false)
		return type_crop;


	
	
	
end et_geometry_2.polygons.cropping;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
