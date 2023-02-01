------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       BOARD OPERATIONS / DEVICES                         --
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


with et_packages;						use et_packages;
with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.packages;


package et_board_ops.devices is

	-- CS rework procedures so that a module cursor
	-- is used instead the module_name.

	-- Collects all devices in the vicinity of the given point:	
	function get_devices (
		module			: in pac_generic_modules.cursor;
		place			: in type_point; -- x/y
		catch_zone		: in type_catch_zone; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_devices_sch.map;


	function get_devices (
		module			: in pac_generic_modules.cursor;
		place			: in type_point;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
		return pac_devices_non_electric.map;

	
	-- Adds a non-electric device to the board:
	procedure add_device (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		package_model	: in et_packages.pac_package_model_file_name.bounded_string; -- ../lbr/packages/fiducial.pac
		position		: in type_package_position; -- x,y,rotation,face
		prefix			: in pac_device_prefix.bounded_string; -- FD
		log_threshold	: in type_log_level);

	-- CS procedure add_device with explicit device name like MH1
	-- CS procedure copy_device

	
	-- Moves a device in the board layout in x/y direction.
	-- Leaves rotation and face (top/bottom) as it is.
	procedure move_device (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		coordinates		: in type_coordinates; -- relative/absolute		
		point			: in type_point; -- x/y
		log_threshold	: in type_log_level);

	
	-- Rotates a device in the board layout.
	-- Leaves x/y and face (top/bottom) as it is.
	procedure rotate_device (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		coordinates		: in type_coordinates; -- relative/absolute		
		rotation		: in et_pcb_coordinates.type_rotation; -- 90 -- CS default rotation ?
		log_threshold	: in type_log_level);

	
	-- Deletes a non-electric device in the board layout.
	-- Electric devices must be deleted in the schematic domain !
	procedure delete_device (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- FD1 -- CS cursor insted ?
		log_threshold	: in type_log_level);

	
	-- Renames a non-electric device in the board layout.
	procedure rename_device (
		module_name			: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name_before	: in type_device_name; -- FD1
		device_name_after	: in type_device_name; -- FD3
		log_threshold		: in type_log_level);

	
	-- Flips a device in the board layout from top to bottom or vice versa.
	-- Leaves x/y and rotation as it is.
	-- Warns operator if device already on desired face of board.
	-- Determines whether the given device is electrical or non-electrical
	-- by its own:
	procedure flip_device (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		face			: in type_face; -- top/bottom
		log_threshold	: in type_log_level);


	-- CS
	--procedure flip_device (
		--module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		--device_cursor	: in pac_devices_sch.cursor; -- IC45
		--face			: in type_face; -- top/bottom
		--log_threshold	: in type_log_level);

	
	-- Returns the positions (x/y) of the terminals of
	-- devices, netchangers and submodules of the given net.
	-- The default assembly variant is assumed (means all devices are mounted).
	function get_terminal_positions (
		module_cursor	: in pac_generic_modules.cursor;
		net_cursor		: in et_schematic.pac_nets.cursor)
		return pac_geometry_brd.pac_vectors.list;


end et_board_ops.devices;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
