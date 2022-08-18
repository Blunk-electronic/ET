------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      GEOMETRY 2 / POLYGONS / UNION                       --
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
--
--   history of changes:
--


generic
	
package et_geometry_1.polygons.union is

	use pac_polygon_list;
	

	-- Returns from the given list of polygons the one that encloses
	-- all others in the list
	function get_greatest (
		polygons	: in pac_polygon_list.list)
		return pac_polygon_list.cursor;
	
	
	-- The result of a polygon union operation:
	type type_union (exists : boolean := true) is record
		case exists is
			when TRUE => union : type_polygon;

			when FALSE => null;
		end case;
	end record;


	-- CS
	--function "=" (
		--left, right : in type_union)
		--return boolean;
	
	
	
	-- Unions polygon A with polygon B.
	-- If debug is true then a lot of debug messages is output.
	function union (
		polygon_A	: in type_polygon; -- the first polygon
		polygon_B	: in type_polygon; -- the second polygon
		debug		: in boolean := false)
		return type_union;


	-- Unions as many as possible polygons with each other:
	procedure multi_union (
		polygons	: in out pac_polygon_list.list;
		debug		: in boolean := false);
							 
	
end et_geometry_1.polygons.union;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
