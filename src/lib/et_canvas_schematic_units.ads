------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        CANVAS SCHEMATIC UNITS                            --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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
--                                                                          --
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
-- DESCRIPTION:
-- 

with ada.containers;            use ada.containers;
with ada.containers.doubly_linked_lists;

with et_coordinates;				use et_coordinates;
use et_coordinates.pac_geometry_sch;

with et_general;					use et_general;
with et_canvas_general;				use et_canvas_general;
with et_project.modules;			use et_project.modules;
with et_schematic;
with et_schematic_ops;				use et_schematic_ops;
with et_schematic_ops.units;		use et_schematic_ops.units;
with et_string_processing;			use et_string_processing;

package et_canvas_schematic_units is

	use et_project.modules.pac_generic_modules;

	-- Whenever a unit is selected via the GUI, we store its
	-- parent device and the unit itself via this type:
	type type_selected_unit is record
		device	: et_schematic.type_devices.cursor;
		unit	: et_schematic.type_units.cursor;
	end record;

	package pac_proposed_units is new doubly_linked_lists (type_selected_unit);
	use pac_proposed_units;

	-- These variables are used by the GUI when the operator selects a unit:
	proposed_units	: pac_proposed_units.list;
	selected_unit	: pac_proposed_units.cursor;

	procedure clear_proposed_units;
	
	-- Collects all units in the vicinity of the given point:
	function collect_units (
		module			: in pac_generic_modules.cursor;
		place			: in et_coordinates.type_position; -- sheet/x/y
		catch_zone		: in type_catch_zone; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_proposed_units.list;

	-- Advances cursor selected_unit to next unit in list selected_units.
	procedure clarify_unit;


-- DELETE UNIT

	status_delete : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to delete unit." 
		& status_hint_for_abort;
	
	-- Deletes a unit in the vicinity of given point.
	-- If more than one unit near point found, then it sets the
	-- cursor selected_unit to the first unit and requests
	-- for clarification.
	-- In case the last unit of a device has been deleted, then the device is 
	-- deleted entirely from the module.
	procedure delete_unit (point : in type_point);

	-- Deletes the unit being pointed at by cursor selected_unit.
	-- Call this procedure after a clarification.
	procedure delete_selected_unit;



-- MOVE UNIT

	type type_unit is record
		being_moved	: boolean := false;
		tool		: type_tool := MOUSE;
	end record;

	unit : type_unit;

	status_move : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to move unit." 
		& status_hint_for_abort;

	status_drag : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to drag unit." 
		& status_hint_for_abort;

	
	-- This procedure:
	-- - Clears list of proposed units.
	-- - Sets global variable selected_unit to no_element.
	-- - resets global variable "unit" to its default values
	procedure reset_unit;

	-- Assigns the final position after the move to the selected unit.
	-- Resets the global variable "unit".
	procedure finalize_move (
		destination		: in type_point;
		log_threshold	: in type_log_level);

	-- Assigns the final position after the drag to the selected unit.
	-- Resets the global variable "unit".
	procedure finalize_drag (
		destination		: in type_point;
		log_threshold	: in type_log_level);
	
	-- Locates all units in the vicinity of given point.
	-- If more than one unit near point found, then it sets the
	-- cursor selected_unit to the first unit and requests
	-- for clarification.
	-- If there is only one unite, sets global variable selected_unit accordingly.
	-- If there is no unit, then selected_unit is set to no_element.
	procedure find_units (point : in type_point);

	-- Locates net segments attached to the unit indicated by
	-- cursor selected_unit:
	procedure find_attached_segments;
	
-- 	-- Moves the unit being pointed at by cursor selected_unit.
-- 	-- Call this procedure after a clarification.
-- 	procedure move_selected_unit;
	
end et_canvas_schematic_units;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
