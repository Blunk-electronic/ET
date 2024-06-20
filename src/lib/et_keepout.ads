------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                              KEEPOUT                                     --
--                                                                          --
--                              S p e c                                     --
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


with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;

with et_pcb_coordinates_2;		use et_pcb_coordinates_2;
with et_geometry;				use et_geometry;
with et_pcb_stack;				use et_pcb_stack;
with et_board_shapes_and_text;	use et_board_shapes_and_text;
with et_logging;				use et_logging;


package et_keepout is
	
	use pac_geometry_2;
	use pac_contours;


	
	-- GUI relevant only: The line width of keepout:
	keepout_line_width : constant type_general_line_width := linewidth_fab_min;

	type type_keepout_zone is new type_contour with null record;

	package pac_keepout_zones is new doubly_linked_lists (type_keepout_zone);
	use pac_keepout_zones;

	
	-- Mirrors a list of zones along the given axis:
	procedure mirror_zones (
		zones	: in out pac_keepout_zones.list;
		axis	: in type_axis_2d := Y);
	
	-- Rotates a list of zones by the given angle about the origin:
	procedure rotate_zones (
		zones	: in out pac_keepout_zones.list;
		angle	: in type_rotation_model);

	-- Moves a list of zones by the given offset:
	procedure move_zones (
		zones	: in out pac_keepout_zones.list;
		offset	: in type_distance_relative);

	

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
		axis	: in type_axis_2d := Y);
	
	-- Rotates the given objects by the given angle
	-- about the origin:
	procedure rotate_keepout_objects (
		keepout	: in out type_keepout;
		angle	: in type_rotation_model);

	-- Moves the given objects by the given offset:
	procedure move_keepout_objects (
		keepout	: in out type_keepout;
		offset	: in type_distance_relative);

	

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
