------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       VIA RESTRICT / PACKAGES                            --
--                                                                          --
--                              B o d y                                     --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
		axis		: in type_mirror := MIRROR_ALONG_Y_AXIS)
	is begin
		null;
		-- CS zones
	end mirror_via_restrict_objects;
	

	procedure rotate_via_restrict_objects (
		restrict	: in out type_one_side;
		angle		: in type_rotation_model)
	is begin
		null;
		-- CS zones
	end rotate_via_restrict_objects;


	procedure move_via_restrict_objects (
		restrict	: in out type_one_side;
		offset		: in type_vector_model)
	is begin
		null;
		-- CS zones
	end move_via_restrict_objects;

	
	function to_polygons (
		restrict	: in type_one_side;
		tolerance	: in type_distance_positive)
		return pac_polygon_list.list
	is
		scratch, result : pac_polygon_list.list;
		
	begin
		-- -- circles:
		-- scratch := to_polygons_outside (restrict.circles, tolerance);
		-- result.splice (before => pac_polygon_list.no_element, source => scratch);

		-- zones:
		-- CS
		null;
		
		return result;
	end to_polygons;



	

	procedure add_zone (
		via_restrict	: in out type_via_restrict;
		zone			: in type_via_restrict_zone;
		face			: in type_face)
	is 
		use pac_via_restrict_zones;
	begin
		case face is
			when TOP =>
				append (via_restrict.top.zones, zone);

			when BOTTOM =>
				append (via_restrict.bottom.zones, zone);
		end case;
	end add_zone;

				


	
	
	procedure add_cutout (
		via_restrict	: in out type_via_restrict;
		cutout			: in type_via_restrict_cutout;
		face			: in type_face)
	is
		use pac_via_restrict_cutouts;
	begin
		case face is
			when TOP =>
				append (via_restrict.top.cutouts, cutout);

			when BOTTOM =>
				append (via_restrict.bottom.cutouts, cutout);
		end case;
	end add_cutout;

	
	
end et_via_restrict.packages;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
