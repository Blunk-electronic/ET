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
with et_schematic_coordinates;			use et_schematic_coordinates;


with et_sheets;							use et_sheets;

with et_generic_modules;				use et_generic_modules;

with et_logging;						use et_logging;

with et_cmd_origin_to_commit;			use et_cmd_origin_to_commit;



package et_schematic_ops_groups is

	use pac_generic_modules;




	-- This procedure resets nets, devices and units:
	procedure reset_objects (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);


	group_reference_point : type_object_position;



	-- This procedure:
	-- 1. sets the "selected"-flag of all
	--    objects which are inside the given zone on the
	--    given sheet.
	-- 2. It sets the sheet of group_reference_point
	--    as specified in argument sheet. This is only relevant
	--    if the clipboard is used for copying.
	procedure define_group_rectangular (
		module_cursor	: in pac_generic_modules.cursor;
		sheet			: in type_sheet;
		area			: in type_area;
		log_threshold	: in type_log_level);


	-- Returns the geometrical center of a group.
	-- It iterates through all selected objects which
	-- are on the sheet as given by group_reference_point.
	-- The group can be only on one sheet.
	-- In order to speed up the process, the affected
	-- sheet number must also be specified:
	function get_center_of_group (
		module_cursor	: in pac_generic_modules.cursor;
		sheet			: in type_sheet;
		log_threshold	: in type_log_level)
		return type_vector_model;



	-- This deletes all objects which are in the
	-- current group. This affects all objects whose
	-- "selected"-flag is set:
	procedure delete_group (
		module_cursor	: in pac_generic_modules.cursor;
		commit_design	: in type_commit_design := DO_COMMIT;
		log_threshold	: in type_log_level);


	-- This procedure drags a group of objects by the
	-- given offset. Dragging from one sheet to another
	-- is not possible.
	-- This is a relative movement.
	-- This affects all objects whose "selected"-flag is set:
	procedure drag_group (
		module_cursor	: in pac_generic_modules.cursor;
		offset			: in type_vector_model; -- x/y
		commit_design	: in type_commit_design := DO_COMMIT;
		log_threshold	: in type_log_level);


	-- This procedure sets the "moving" flag of all
	-- objects which are selected (which are in the group):
	procedure set_group_as_moving (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);


	-- This procedure clears the "moving" flag of all
	-- objects (regardless whether they are selcted or not):
	procedure set_group_as_not_moving (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);



	-- This procedure copies a group of objects
	-- This affects all objects whose "selected"-flag is set:
	procedure copy_group_simple (
		module_cursor	: in pac_generic_modules.cursor;
		sheet			: in type_sheet_relative;
		offset			: in type_vector_model; -- x/y
		commit_design	: in type_commit_design := DO_COMMIT;
		log_threshold	: in type_log_level);






	-- This procedure copies the current group of objects
	-- into the clipboard.
	-- 1. If auto_center is true, then
	--    the center of the group is used as reference point.
	-- 2. If auto_center is false, then the explicitly given
	--    reference_point is used.
	-- 3. The x/y component of the global group_reference_point
	--    is set according to the specified reference_point
	--    or as the auto genereated center of the group.
	-- 4. The sheet where the group is, can be taken from
	--    the group_reference_point.
	procedure copy_group_to_clipboard (
		module_cursor	: in pac_generic_modules.cursor;
		auto_center		: in boolean := true;
		reference_point	: in type_vector_model := origin;
		log_threshold	: in type_log_level);



	-- This procedure pastes the content of the clipboard
	-- at the given sheet and place.
	-- The global group_reference_point (set by procedures
	-- define_group_rectangular and copy_group_to_clipboard)
	-- is used to compute the offset by which the group is
	-- to be pasted:
	procedure paste_group (
		module_cursor	: in pac_generic_modules.cursor;
		sheet			: in type_sheet;
		place			: in type_vector_model; -- x/y
		commit_design	: in type_commit_design := DO_COMMIT;
		log_threshold	: in type_log_level);



end et_schematic_ops_groups;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
