------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    SCHEMATIC OPERATIONS ON UNITS                         --
--                                                                          --
--                               S p e c                                    --
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
	-- deleted entirely from the module.
	procedure delete_unit (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		log_threshold	: in type_log_level);

	-- Deletes a unit of a device. 
	-- In case the last unit has been deleted, then the device is 
	-- deleted entirely from the module.
	procedure delete_unit (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		log_threshold	: in type_log_level);


	-- Moves the given unit within the schematic. Disconnects the unit from
	-- start or end points of net segments BEFORE the move. 
	-- Connects unit ports with segment end or strart points AFTER the move.
	procedure move_unit (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		sheet			: in type_sheet_relative; -- -3/0/2
		point			: in type_point; -- x/y -- CS rename to destination
		log_threshold	: in type_log_level);

	
	-- Tests whether the given unit ports at their individual location are movable. 
	-- The criteria for movement are: no netchanger port, no device port, no submodule ports there.
	-- The only port allowed at an individual drag point is the port-to-be-dragged itself.
	-- CS: Might become obsolete once ports at the same x/y position are prevented.
	function is_movable (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name;
		unit_name		: in pac_unit_name.bounded_string;
		location 		: in et_coordinates.type_position; -- only sheet number matters
		unit_ports		: in et_symbols.pac_ports.map;
		log_threshold	: in type_log_level)
		return boolean;

	
	procedure drag_unit (
	-- Drags the given unit within the schematic.
	-- Already existing connections with net segments are kept.
	-- Net segment positions are modified.
	-- Net segment positions are modified.
	-- CS: Before the drag: If a port of the unit sits at the same place
	--     where a port of another unit is, then a net segment should be
	--     inserted between them ?
	-- This operation applies to a single sheet. Dragging from one sheet
	-- to another is not possible.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_point; -- x/y -- CS rename to destination
		log_threshold	: in type_log_level);

	
	-- Rotates the given unit. 
	-- Disconnects the unit from attached net segments before the rotation.
	-- Connects the unit with net segments after the rotation.
	-- Rotates the placeholders about the unit center.
	procedure rotate_unit (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute		
		rotation		: in et_coordinates.type_rotation; -- 90
		log_threshold	: in type_log_level);
	
end et_schematic_ops.units;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
