------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       VIA RESTRICT PACKAGES                              --
--                                                                          --
--                              B o d y                                     --
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
--   to do:


package body et_via_restrict.packages is
	

	procedure mirror_via_restrict_objects (
		restrict	: in out type_one_side;
		axis		: in type_axis_2d := Y)
	is begin
		mirror_lines (restrict.lines);
		mirror_arcs (restrict.arcs);
		mirror_circles (restrict.circles);
		-- CS zones
	end mirror_via_restrict_objects;
	

	procedure rotate_via_restrict_objects (
		restrict	: in out type_one_side;
		angle		: in type_rotation_model)
	is begin
		rotate_lines (restrict.lines, angle);
		rotate_arcs (restrict.arcs, angle);
		rotate_circles (restrict.circles, angle);
		-- CS zones
	end rotate_via_restrict_objects;


	procedure move_via_restrict_objects (
		restrict	: in out type_one_side;
		offset		: in type_distance_relative)
	is begin
		move_lines (restrict.lines, offset);
		move_arcs (restrict.arcs, offset);
		move_circles (restrict.circles, offset);
		-- CS zones
	end move_via_restrict_objects;

	
	function to_polygons (
		restrict	: in type_one_side;
		tolerance	: in type_distance_positive)
		return pac_polygon_list.list
	is
		scratch, result : pac_polygon_list.list;
		
	begin
		-- lines:
		result := to_polygons (restrict.lines, tolerance);

		-- arcs:
		scratch := to_polygons (restrict.arcs, tolerance);
		result.splice (before => pac_polygon_list.no_element, source => scratch);

		-- circles:
		scratch := to_polygons_outside (restrict.circles, tolerance);
		result.splice (before => pac_polygon_list.no_element, source => scratch);

		-- zones:
		-- CS
		return result;
	end to_polygons;

	
end et_via_restrict.packages;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
