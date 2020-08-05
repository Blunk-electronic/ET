------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    SCHEMATIC OPERATIONS ON UNITS                         --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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
--   ToDo: 

package et_schematic_ops.units is
	
	-- Deletes a unit of a device. 
	-- In case the last unit has been deleted, then the device is 
	-- deleted entirely from module.devices.
	procedure delete_unit (
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_name; -- IC45
		unit_name		: in type_unit_name.bounded_string; -- A
		log_threshold	: in type_log_level);

	-- Deletes a unit of a device. 
	-- In case the last unit has been deleted, then the device is 
	-- deleted entirely from module.devices.
	procedure delete_unit (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in et_schematic.type_devices.cursor;
		unit_cursor		: in et_schematic.type_units.cursor;
		log_threshold	: in type_log_level);

	procedure move_unit (
	-- Moves the given unit within the schematic. Disconnects the unit from
	-- start or end points of net segments BEFORE the move. 
	-- Connects unit ports with segment end or strart points AFTER the move.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_name; -- IC45
		unit_name		: in type_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		sheet			: in type_sheet_relative; -- -3/0/2
		point			: in type_point; -- x/y
		log_threshold	: in type_log_level);

	procedure rotate_unit (
	-- Rotates the given unit. Disconnects the unit from
	-- start or end points of net segments.
	-- Rotates the placeholders around the unit.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_name; -- IC45
		unit_name		: in type_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute		
		rotation		: in et_coordinates.type_rotation; -- 90
		log_threshold	: in type_log_level);

	-------------------

	type type_selected_unit is record
		device	: et_schematic.type_devices.cursor;
		unit	: et_schematic.type_units.cursor;
	end record;

	package pac_selected_units is new doubly_linked_lists (type_selected_unit);

	selected_units	: pac_selected_units.list;
	selected_unit	: pac_selected_units.cursor;

	-- Deletes a selected unit of a device.
	procedure delete_selected_unit (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		unit			: in type_selected_unit; -- device/unit
		log_threshold	: in type_log_level);
	
	-- Collects all units in the vicinity of the given point:
	function collect_units (
		module			: in pac_generic_modules.cursor;
		place			: in et_coordinates.type_position; -- sheet/x/y
		catch_zone		: in type_catch_zone; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_selected_units.list;

	
end et_schematic_ops.units;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
