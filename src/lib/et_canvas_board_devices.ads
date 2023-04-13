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

with et_general;					use et_general;
with et_geometry;					use et_geometry;
with et_canvas_general;				use et_canvas_general;

with et_pcb_coordinates;			use et_pcb_coordinates;
use et_pcb_coordinates.pac_geometry_2;

with et_project.modules;			use et_project.modules;
with et_schematic;					use et_schematic;
with et_pcb;						use et_pcb;
with et_devices;					use et_devices;
with et_logging;					use et_logging;


package et_canvas_board_devices is


	-- Before placing, moving, deleting or other operations we
	-- collect preliminary information using this type:
	type type_preliminary_electrical_device is record
		-- This flag indicates that the device has been
		-- clarified among the proposed devices:
		ready	: boolean := false;

		-- This tells the GUI whether the mouse or the
		-- cursor position is to be used when drawing the device:
		tool	: type_tool := MOUSE;

		device	: type_device_name := (others => <>); -- IC45
	end record;

	-- The place where preliminary information of
	-- an electrical device is stored:
	preliminary_electrical_device : type_preliminary_electrical_device;


	-- This procedure:
	-- - Clears list of proposed electrical devices.
	-- - Sets global variable selected_electrical_device to no_element.
	-- - resets global variable preliminary_electrical_device 
	--   to its default values
	procedure reset_preliminary_electrical_device;




	
	-- Before placing, moving, deleting or other operations we
	-- collect preliminary information using this type:
	type type_preliminary_non_electrical_device is record
		-- This flag indicates that the device has been
		-- clarified among the proposed device:
		ready	: boolean := false;

		-- This tells the GUI whether the mouse or the
		-- cursor position is to be used when drawing the device:
		tool	: type_tool := MOUSE;
		
		device	: type_device_name := (others => <>); -- FD1 -- CS: use cursor instead ?
	end record;

	-- The place where preliminary information of
	-- a non-electrical device is stored:
	preliminary_non_electrical_device : type_preliminary_non_electrical_device;


	-- This procedure:
	-- - Clears list of proposed non-electrical devices.
	-- - Sets global variable selected_non_electrical_device to no_element.
	-- - resets global variable preliminary_non_electrical_device 
	--   to its default values
	procedure reset_preliminary_non_electrical_device;

	


-- ELECTRICAL DEVICES:
	
	use pac_devices_sch;
	--proposed_electrical_devices	: pac_devices_sch.map;
	selected_electrical_device	: pac_devices_sch.cursor;

	
	-- Returns true if the given electrical device matches the device 
	-- indicated by cursor "selected_electrical_device":
	-- function electrical_device_is_selected (
	-- 	d : in pac_devices_sch.cursor)
	-- 	return boolean;


	
-- NON-ELECTRICAL DEVICES:
	
	use pac_devices_non_electric;
	proposed_non_electrical_devices	: pac_devices_non_electric.map;
	selected_non_electrical_device	: pac_devices_non_electric.cursor;

	
	-- Returns true if the given non-electrical device matches the device 
	-- indicated by cursor "selected_non_electrical_device":
	function non_electrical_device_is_selected (
		d : in et_pcb.pac_devices_non_electric.cursor)
		return boolean;

	

	
	-- Clears the list proposed_electrical_device.
	-- Resets selected_electrical_device to no_element.
	procedure clear_proposed_electrical_devices;

	-- Clears the list proposed_non_electrical_devices.
	-- Resets selected_non_electrical_device to no_element.
	procedure clear_proposed_non_electrical_devices;
	


	-- Advances cursor selected_electrical_device to next device
	-- in list proposed_electrical_device:
	procedure select_electrical_device;

	-- Advances cursor selected_non_electrical_device to next device
	-- in list proposed_non_electrical_devices:
	procedure select_non_electrical_device;
	

	
	
	-- Locates all devices in the vicinity of given point.
	-- If more than one device near point found, then it sets the
	-- cursor selected_electrical_device to the first device and requests
	-- for clarification.
	procedure find_electrical_devices (
		point : in type_point);

	-- Locates all non-electrical devices in the vicinity of given point.
	-- If more than one device near point found, then it sets the
	-- cursor selected_non_electrical_device to the first device and requests
	-- for clarification.
	procedure find_non_electrical_devices (
		point : in type_point);


	
-- MOVE:	

	status_move_device : constant string :=
		status_click_left 
		& "or "
		& status_press_space
		& "to move device." 
		& status_hint_for_abort;

	
	-- Locates devices in the vicinity of the given point.
	-- Depending on how many devices have been found, the behaviour is:
	-- - If only one device found, then it is selected and 
	--   the flag preliminary_electrical_device.ready will be set.
	--   This causes the selected device to be drawn at the tool position.
	-- - If more than one device found, then clarification is requested.
	--   No device will be moved.
	--   The next call of this procedure sets preliminary_electrical_device.ready
	--   so that the selected device will be drawn at the tool position.
	--   The next call of this procedure assigns the final position 
	--   to the selected_electrical_device:
	procedure move_electrical_device (
		tool	: in type_tool;
		point	: in type_point);

	-- Similar to procedure move_electrical_device but works 
	-- on non-electrical devices:
	procedure move_non_electrical_device (
		tool	: in type_tool;
		point	: in type_point);


	
-- ROTATION:

	status_rotate_device : constant string :=
		status_click_left 
		& "or "
		& status_press_space
		& "to rotate device." 
		& status_hint_for_abort;

	
	default_rotation : constant type_rotation := 90.0;
	
	-- Locates electrical devices in the vicinity of the given point.
	-- Depending on how many devices have been found, the behaviour is:
	-- - If only one device found, then it is rotated immediately.
	-- - If more than one device found, then clarification is requested.
	--   No device will be rotated.
	--   The next call of this procedure rotates the device indicated
	--   by the cursor selected_electrical_device.
	-- The rotation is always by 90 degree counter-clockwise:
	procedure rotate_electrical_device (
		tool	: in type_tool;
		point	: in type_point);

	-- Similar to procedure rotate_electrical_device but works 
	-- on non-electrical devices:
	procedure rotate_non_electrical_device (
		tool	: in type_tool;
		point	: in type_point);

	


-- FLIP / MIRROR:

	-- to be output in the status bar:
	status_flip_device : constant string :=
		status_click_left 
		& "or "
		& status_press_space
		& "to flip device." 
		& status_hint_for_abort;
	

	-- Locates electrical devices in the vicinity of the given point.
	-- Depending on how many devices have been found, the behaviour is:
	-- - If only one device found, then it is flipped immediately.
	-- - If more than one device found, then clarification is requested.
	--   No device will be flipped.
	--   The next call of this procedure flips the device indicated
	--   by the cursor selected_electrical_device.
	procedure flip_electrical_device (
		tool	: in type_tool;
		point	: in type_point);
	
	-- Similar to procedure flip_electrical_device but works 
	-- on non-electrical devices:
	procedure flip_non_electrical_device (
		tool	: in type_tool;
		point	: in type_point);



-- DELETE:

	status_delete_device : constant string :=
		status_click_left 
		& "or "
		& status_press_space
		& "to delete device." 
		& status_hint_for_abort;

	
	-- Locates non-electrical devices in the vicinity of the given point.
	-- Depending on how many devices have been found, the behaviour is:
	-- - If only one device found, then it is flipped immediately.
	-- - If more than one device found, then clarification is requested.
	--   No device will be flipped.
	--   The next call of this procedure flips the device indicated
	--   by the cursor selected_non_electrical_device.
	procedure delete_non_electrical_device (
		tool	: in type_tool;
		point	: in type_point);

	
	-- NOTE: Electrical devices must be deleted in the schematic !
	-- For this reason there is no procedure here to delete an electrical device.

	
	
end et_canvas_board_devices;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
