------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD DEVICES                             --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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

with ada.containers;   	         use ada.containers;
with ada.containers.doubly_linked_lists;

with et_general;					use et_general;
with et_geometry;					use et_geometry;
with et_canvas_general;				use et_canvas_general;

with et_pcb_coordinates;			use et_pcb_coordinates;
use et_pcb_coordinates.pac_geometry_2;

--with et_terminals;				use et_terminals;
--with et_packages;
with et_project.modules;			use et_project.modules;
with et_schematic;
with et_devices;					use et_devices;

with et_string_processing;			use et_string_processing;
with et_logging;					use et_logging;


package et_canvas_board_devices is

	use et_project.modules.pac_generic_modules;
	

	type type_electrical_device_being_moved is record
		being_moved			: boolean := false;
		tool				: type_tool := MOUSE;
		device				: type_device_name := (others => <>); -- IC45
	end record;

	electrical_device_move : type_electrical_device_being_moved;

	
	-- to be output in the status bar:
	status_flip : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to flip device." 
		& status_hint_for_abort;

	status_move : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to move device." 
		& status_hint_for_abort;

	status_rotate : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to rotate device." 
		& status_hint_for_abort;


	-- Whenever a device is selected via the GUI, 
	-- we store its cursor via this type.
	-- This type is to be used for a device that
	-- is already placed in the schematic and board. So we use it
	-- for example for moving, mirroring or rotating of a device.
	type type_selected_electrical_device is record
		device	: et_schematic.pac_devices_sch.cursor;
	end record;

	package pac_proposed_electrical_devices is new 
		doubly_linked_lists (type_selected_electrical_device);
	use pac_proposed_electrical_devices;
	
	proposed_devices_electrical	: pac_proposed_electrical_devices.list;
	selected_device_electrical	: pac_proposed_electrical_devices.cursor;


	-- Clears the list proposed_devices_electrical.
	-- Resets selected_device_electrical to no_element.
	procedure clear_proposed_electrical_devices;
	
	
	-- Collects all units in the vicinity of the given point:	
	function collect_devices (
		module			: in pac_generic_modules.cursor;
		place			: in type_point; -- x/y
		catch_zone		: in type_catch_zone; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_proposed_electrical_devices.list;


	-- Advances cursor selected_device_electrical to next device
	-- in list proposed_devices_electrical:
	procedure clarify_electrical_device;
	
	
	-- This procedure:
	-- - Clears list of proposed electrical devices.
	-- - Sets global variable selected_device_electrical to no_element.
	-- - resets global variable electrical_device_move to its default values
	procedure reset_electrical_device_move;
	
	
	-- Locates all devices in the vicinity of given point.
	-- If more than one device near point found, then it sets the
	-- cursor selected_device_electrical to the device and requests
	-- for clarification.
	procedure find_electrical_devices_for_move (
		point : in type_point);


	-- Assigns the final position after the move to the selected 
	-- electrical device.
	-- Resets global variable electrical_device_move:
	procedure finalize_move_electrical (
		destination		: in type_point;
		log_threshold	: in type_log_level);


	
	default_rotation : constant type_rotation := 90.0;
	
	-- Rotates the selected electrical device by default_rotation.
	-- Resets global variable electrical_device_move:
	procedure finalize_rotate_electrical (
		rotation		: in type_rotation := default_rotation;
		log_threshold	: in type_log_level);


	-- Flips the selected electrical device.
	-- Resets global variable electrical_device_move:
	procedure finalize_flip_electrical (
		log_threshold	: in type_log_level);

	
end et_canvas_board_devices;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
