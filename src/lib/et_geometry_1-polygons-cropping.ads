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
	
package et_geometry_1.polygons.cropping is

	-- The result of a polygon cropping operation is a list
	-- of sub-polygons:
	--package pac_cropped is new doubly_linked_lists (type_polygon);
	use pac_polygon_list;
	

	type type_crop (exists : boolean := true) is record
		-- The status of polygon A in relation to polygon B:
		status : type_overlap_status;
		
		case exists is
			when TRUE =>
				-- The list of sub-polygons:
				fragments : pac_polygon_list.list; 

				-- The number of fragments:
				count : count_type;
				
			when FALSE => null;
		end case;
	end record;
	

	function "=" (
		left, right : in type_crop)
		return boolean;
	

	
	function to_string (cr : in type_crop) return string;
	
	
	-- Crops polygon B by polygon A.
	-- These scenarios may exist:
	-- 1. A and B are congruent. Result: B is cropped to zero area. List "cropped" is empty.
	-- 2. A does not overlap B. Result: B is returned unchanged as the one and only polygon
	--    in list "cropped".
	-- 3. A inside B. Result: no crop. List "cropped" does not exist in the result.
	-- 4. B inside A. Result: B is cropped to zero area. List "cropped" is empty.
	-- 5. A overlaps B. Result: B is cropped by A. List "cropped" contains at 
	--    least one polygon.
	function crop (
		polygon_A	: in type_polygon; -- the cropping polygon
		polygon_B	: in type_polygon; -- the cropped polygon / zu bescheidendes Polygon
		debug		: in boolean := false)
		return type_crop;

	-- CS improve function crop:
	-- parameter for already existing overlap status
	-- parameter for already existing intersections


	-- Crops a single polygon by a number of polygons:
	function multi_crop_1 (
		polygon_B		: in type_polygon; -- the cropped polygon / zu bescheidendes Polygon
		polygon_A_list	: in pac_polygon_list.list; -- the cropping polygons
		debug			: in boolean := false)
		return pac_polygon_list.list;

	
	function multi_crop_2 (
		polygon_B_list	: in pac_polygon_list.list; -- the cropped polygons / zu bescheidende Polygone
		polygon_A_list	: in pac_polygon_list.list; -- the cropping polygons
		debug			: in boolean := false)
		return pac_polygon_list.list;
	
	
end et_geometry_1.polygons.cropping;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
