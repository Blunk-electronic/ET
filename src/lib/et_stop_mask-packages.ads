------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         STOPMASK IN PACKAGES                             --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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

package et_stop_mask.packages is

	
	type type_stopmask_both_sides is record
		top		: type_stopmask;
		bottom	: type_stopmask;
	end record;


	-- Mirrors a list of stopmask objects along the given axis:
	procedure mirror_stopmask_objects (
		stopmask	: in out type_stopmask;
		axis		: in type_axis_2d := Y);

	-- Rotates a list of stopmask objects by the given angle:
	procedure rotate_stopmask_objects (
		stopmask	: in out type_stopmask;
		angle		: in type_rotation);

	-- Moves a list of stopmask objects by the given offset:
	procedure move_stopmask_objects (
		stopmask	: in out type_stopmask;
		offset		: in type_distance_relative);


	
end et_stop_mask.packages;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
