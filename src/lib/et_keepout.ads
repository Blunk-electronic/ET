------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                              KEEPOUT                                     --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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


with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;

with et_board_coordinates;		use et_board_coordinates;
with et_mirroring;				use et_mirroring;
with et_pcb_stack;				use et_pcb_stack;
with et_logging;				use et_logging;


package et_keepout is
	
	use pac_geometry_2;
	use pac_contours;



	type type_keepout_zone is new type_contour with null record;

	package pac_keepout_zones is new doubly_linked_lists (type_keepout_zone);
	use pac_keepout_zones;


	-- Returns true if the given zone consists of a circle:
	function is_circular (
		zone	: in pac_keepout_zones.cursor)
		return boolean;

	
	
	-- Iterates the zones.
	-- Aborts the process when the proceed-flag goes false:
	procedure iterate (
		zones	: in pac_keepout_zones.list;
		process	: not null access procedure (position : in pac_keepout_zones.cursor);
		proceed	: not null access boolean);


	
	-- Mirrors a list of zones along the given axis:
	procedure mirror_zones (
		zones	: in out pac_keepout_zones.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS);
	
	-- Rotates a list of zones by the given angle about the origin:
	procedure rotate_zones (
		zones	: in out pac_keepout_zones.list;
		angle	: in type_rotation_model);

	-- Moves a list of zones by the given offset:
	procedure move_zones (
		zones	: in out pac_keepout_zones.list;
		offset	: in type_vector_model);

	

	type type_keepout_cutout is new type_contour with null record;
	package pac_keepout_cutouts is new doubly_linked_lists (type_keepout_cutout);	
	use pac_keepout_cutouts;
	-- CS not sure whether this is really required
	
	type type_keepout is tagged record
		zones	: pac_keepout_zones.list;
		cutouts : pac_keepout_cutouts.list;
	end record;


	-- Mirrors the given objects along the given axis:
	procedure mirror_keepout_objects (
		keepout	: in out type_keepout;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS);
	
	-- Rotates the given objects by the given angle
	-- about the origin:
	procedure rotate_keepout_objects (
		keepout	: in out type_keepout;
		angle	: in type_rotation_model);

	-- Moves the given objects by the given offset:
	procedure move_keepout_objects (
		keepout	: in out type_keepout;
		offset	: in type_vector_model);

	

	type type_keepout_both_sides is record
		top 	: type_keepout;
		bottom	: type_keepout;
	end record;
	

	
end et_keepout;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
