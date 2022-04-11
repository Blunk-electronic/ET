------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      GEOMETRY 2 / POLYGONS / OFFSETTING                  --
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


generic
	
package et_geometry_2.polygons.offsetting is
	
	-- See:
	-- <https://gis.stackexchange.com/questions/61786/how-to-scale-reduce-my-polygon-without-changing-the-central-lat-long>
	-- <https://stackoverflow.com/questions/54033808/how-to-offset-polygon-edges>

	-- CS subtype for offset

	-- Offsets (the edges of) a polygon. 
	-- If offset is positive then the edges are moved toward the outside
	-- of the polygon. If offset is negative then the edges are moved inside.
	procedure offset_polygon (
		polygon		: in out type_polygon;
		offset		: in type_distance);


	-- Offsets a list of polygons:
	function offset_polygons (
		polygons	: in pac_polygons.list;
		offset		: in type_distance)
		return pac_polygons.list;

	
	procedure offset_polygons (
		polygons	: in out pac_polygons.list;
		offset		: in type_distance);

	
end et_geometry_2.polygons.offsetting;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
