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
with et_project.modules;			use et_project.modules;
with et_schematic;
with et_schematic_ops;				use et_schematic_ops;
with et_schematic_ops.units;
with et_string_processing;			use et_string_processing;

package et_canvas_schematic_units is

	use et_project.modules.pac_generic_modules;

	-- Whenever a unit is selected via the GUI, we store its
	-- parent device and the unit itself via this type:
	type type_selected_unit is record
		device	: et_schematic.type_devices.cursor;
		unit	: et_schematic.type_units.cursor;
	end record;

	package pac_selected_units is new doubly_linked_lists (type_selected_unit);

	-- These variables are used by the GUI when the operator selects a unit:
	selected_units	: pac_selected_units.list;
	selected_unit	: pac_selected_units.cursor;

	-- Collects all units in the vicinity of the given point:
	function collect_units (
		module			: in pac_generic_modules.cursor;
		place			: in et_coordinates.type_position; -- sheet/x/y
		catch_zone		: in type_catch_zone; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_selected_units.list;

	-- Advances cursor selected_unit to next unit in list selected_units.
	procedure clarify_unit;


-- DELETE UNIT
	
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
		position	: type_point;
	end record;

	unit : type_unit;
	
	-- Moves a unit in the vicinity of given point.
	-- If more than one unit near point found, then it sets the
	-- cursor selected_unit to the first unit and requests
	-- for clarification.
	procedure move_unit (point : in type_point);

	-- Moves the unit being pointed at by cursor selected_unit.
	-- Call this procedure after a clarification.
	procedure move_selected_unit;
	
end et_canvas_schematic_units;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
