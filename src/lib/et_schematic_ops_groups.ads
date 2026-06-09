------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    SCHEMATIC OPERATIONS ON GROUPS                        --
--                                                                          --
--                               S p e c                                    --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
-- To Do: 
--
--
--

with et_schematic_geometry;				use et_schematic_geometry;
use et_schematic_geometry.pac_geometry_2;

with et_sheets;							use et_sheets;

with et_generic_modules;				use et_generic_modules;
with et_object_status;					use et_object_status;

with et_logging;						use et_logging;

with et_cmd_origin_to_commit;			use et_cmd_origin_to_commit;



package et_schematic_ops_groups is

	use pac_generic_modules;
	

	-- This procedure resets nets, devices and units:
	procedure reset_objects (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);


	-- This procedure sets the "selected"-flag of all
	-- objects which are inside the given zone on the
	-- given sheet:
	procedure define_group_rectangular (
		module_cursor	: in pac_generic_modules.cursor;
		sheet			: in type_sheet;
		area			: in type_area;
		log_threshold	: in type_log_level);


	-- This deletes all objects which are in the
	-- current group. This affects all objects whose
	-- "selected"-flag is set:
	procedure delete_group (
		module_cursor	: in pac_generic_modules.cursor;
		commit_design	: in type_commit_design := DO_COMMIT;
		log_threshold	: in type_log_level);

		
end et_schematic_ops_groups;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
