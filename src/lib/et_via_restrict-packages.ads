------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       VIA RESTRICT PACKAGES                              --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
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

with et_conductor_text.packages;

package et_via_restrict.packages is
	
	type type_one_side is record
		zones		: pac_via_restrict_zones.list;
		cutouts		: pac_via_restrict_cutouts.list;
		-- CS not sure whether this is really required.

		-- CS texts : 
		-- This must not be derived from from conductor text because
		-- it is not fabrication relevant.
		-- It should contain notes of the designer exclusively.
	end record;


	-- Mirrors the given via restrict objects along the given axis:
	procedure mirror_via_restrict_objects (
		restrict	: in out type_one_side;
		axis		: in type_mirror := MIRROR_ALONG_Y_AXIS);
	
	-- Rotates the given via restrict objects by the given angle
	-- about the origin:
	procedure rotate_via_restrict_objects (
		restrict	: in out type_one_side;
		angle		: in type_rotation_model);

	-- Moves the given via restrict objects by the given offset:
	procedure move_via_restrict_objects (
		restrict	: in out type_one_side;
		offset		: in type_vector_model);


	-- Converts the given restrict objects to a list of polygons.
	function to_polygons (
		restrict	: in type_one_side;
		tolerance	: in type_distance_positive)
		return pac_polygon_list.list;

	
	type type_via_restrict is record
		top		: type_one_side;
		bottom	: type_one_side;
	end record;


	
end et_via_restrict.packages;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
