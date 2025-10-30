------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       BOARD OPERATIONS / DEVICES                         --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                -- 
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
--   ToDo: 


with et_package_model_name;				use et_package_model_name;
with et_package_name;					use et_package_name;
with et_package_library;				use et_package_library;
with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.packages;
with et_device_prefix; 					use et_device_prefix;
with et_device_name;					use et_device_name;
with et_device_category_electrical_non_electrical;	use et_device_category_electrical_non_electrical;
with et_device_property_level;			use et_device_property_level;
with et_devices_electrical;				use et_devices_electrical;
with et_devices_non_electrical;			use et_devices_non_electrical;


package et_board_ops.devices is


-- ELECTICAL DEVICES:
	
	
	-- Collects all devices in the vicinity of the given point:	
	function get_devices ( -- CS remove
		module			: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
		return pac_devices_electrical.map;


	-- Modifies that status flag of a device (see package et_object_status):
	procedure modify_status ( -- CS remove
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in pac_devices_electrical.cursor;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);

	
	type type_object_electrical is record
		cursor : pac_devices_electrical.cursor;
	end record;


	-- Modifies that status flag of an electrical device:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		device			: in type_object_electrical;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);

	
	
	-- Sets the proposed-flag of the package of
	-- all real electrical devices which are in the
	-- given zone around the given place.
	-- Adds to count the number of devices that have been found:
	procedure propose_electrical_devices (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural; -- the number of affected devices
		log_threshold	: in type_log_level);

	

	
	-- Returns the first device according to the given flag.
	-- If no device has been found,
	-- then the return is no_element:
	function get_first_device (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return pac_devices_electrical.cursor; -- CS remove
	

	-- Returns the first device according to the given flag.
	-- If no device has been found,
	-- then the return is no_element:
	function get_first_device (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_electrical;





	-- Moves a device in the board layout in x/y direction.
	-- Leaves rotation and face (top/bottom) as it is.
	-- Automatically detects whether the given device is
	-- electrical or non-electrical:
	procedure move_device (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		coordinates		: in type_coordinates; -- relative/absolute		
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level);

	
	-- Rotates a device in the board layout.
	-- Leaves x/y and face (top/bottom) as it is.
	-- Automatically detects whether the given device is
	-- electrical or non-electrical:
	procedure rotate_device (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		coordinates		: in type_coordinates; -- relative/absolute		
		rotation		: in et_board_geometry.type_rotation_model; -- 90 -- CS default rotation ?
		log_threshold	: in type_log_level);



	-- Flips a device in the board layout from top to bottom or vice versa.
	-- Leaves x/y and rotation as it is.
	-- Automatically detects whether the given device is
	-- electrical or non-electrical.
	-- 1. If toggle is false, then the destination face must be
	-- provided. 
	-- 2. If toggle is true, then the face of the targeted device
	--    is changed from top to bottom or vice versa.
	--    The argument face is then ignored.
	procedure flip_device (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		toggle			: in boolean := false;
		face			: in type_face := TOP; -- top/bottom
		log_threshold	: in type_log_level);


	-- CS
	--procedure flip_device (
		--module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		--device_cursor	: in pac_devices_electrical.cursor; -- IC45
		--face			: in type_face; -- top/bottom
		--log_threshold	: in type_log_level);

	
--------------------------------------------------------------------------
	
-- NON-ELECTRICAL DEVICES:

	-- Collects all non-electrical devices in the vicinity of the given point:	
	function get_devices ( -- CS remove
		module			: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
		return pac_devices_non_electrical.map;


	-- Returns the cursor to the given non-electrical device
	-- in the given module.
	-- If the device does not exist, then no_element is returned:
	function get_non_electrical_device (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- FD1
		return pac_devices_non_electrical.cursor;


	


	
-- SHOW DEVICE:
	
	-- Sets the given non-electrical device as selected.
	-- If the given device does not exist, then a warning
	-- is written in the log and the error flag is set.
	-- If output_warning is false then no warning will be logged:
	procedure show_non_electrical_device (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- FD1, MH2
		error			: out boolean;
		log_warning		: in boolean := true;
		log_threshold	: in type_log_level);


	-- Returns properties of the given non-electrical device. 
	-- 1. If the given device does not exist, then error is set and an empty 
	--    string will be returned.
	-- 2. Level determines the degree and amount of information to be returned.
	-- 3. By default no linebreaks are inserted in the output,
	--    so that the result is a single line.
	-- 4. If linebreaks is true, then linebreaks are inserted.
	--    This is useful when the output is to be displayed
	--    in a window or if it is to be written in a file:	
	function get_device_properties (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name;
		level			: in type_properties_level;
		linebreaks		: in boolean := false;
		error			: out boolean;
		log_threshold	: in type_log_level)
		return string;

	
		
	-- Modifies that status flag of a device (see package et_object_status):
	procedure modify_status ( -- CS remove
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in pac_devices_non_electrical.cursor;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);


	
	type type_object_non_electrical is record
		cursor : pac_devices_non_electrical.cursor;
	end record;


	-- Modifies that status flag of a non-electrical device:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		device			: in type_object_non_electrical;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);

	
	-- Sets the proposed-flag of all real non-electrical 
	-- devices which are in the
	-- given zone around the given place.
	-- Adds to count the number of devices that have been found:
	procedure propose_non_electrical_devices (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural; -- the number of affected devices
		log_threshold	: in type_log_level);

	
	-- Clears the proposed-flag and the selected-flag of all real devices:
	procedure reset_proposed_non_electrical_devices (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);

	
	-- Returns the first non-electrical device according to the given flag.
	-- If no device has been found,
	-- then the return is no_element:
	function get_first_non_electrical_device (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return pac_devices_non_electrical.cursor;


	-- Returns the first device according to the given flag.
	-- If no device has been found,
	-- then the return is no_element:
	function get_first_non_electrical_device (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_non_electrical;
	
	
	-- Advances to the next proposed device, starting at
	-- the device given by device_cursor. If there are no
	-- proposed devices, then device_cursor is set to no_element.
	-- If there is only one proposed device, then device_cursor
	-- is unchanged. 
	procedure next_proposed_non_electrical_device ( -- CS remove
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in out pac_devices_non_electrical.cursor;							
		log_threshold	: in type_log_level);


	-- Adds a non-electrical device to the board:
	procedure add_device (
		module_cursor	: in pac_generic_modules.cursor;
		package_model	: in pac_package_model_file_name.bounded_string; -- ../lbr/packages/fiducial.pac
		position		: in type_package_position; -- x,y,rotation,face
		prefix			: in pac_device_prefix.bounded_string; -- FD
		log_threshold	: in type_log_level);

	-- CS procedure add_device with explicit device name like MH1
	-- CS procedure copy_device


	-- Deletes a non-electric device in the board layout.
	-- Electric devices must be deleted in the schematic domain !
	procedure delete_device (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- FD1 -- CS cursor insted ?
		log_threshold	: in type_log_level);

	
	-- Renames a non-electric device in the board layout.
	procedure rename_device (
		module_cursor		: in pac_generic_modules.cursor;
		device_name_before	: in type_device_name; -- FD1
		device_name_after	: in type_device_name; -- FD3
		log_threshold		: in type_log_level);


	
------------------------------------------------------------------------


-- OBJECTS:
	
	type type_object_category is (
		CAT_VOID,
		CAT_ELECTRICAL_DEVICE,
		CAT_NON_ELECTRICAL_DEVICE);


	-- This type wraps all kinds of devices into a single type:
	type type_object (cat : type_object_category) is record
		case cat is
			when CAT_VOID => null;
			
			when CAT_ELECTRICAL_DEVICE =>
				electrical_device		: type_object_electrical;
				
			when CAT_NON_ELECTRICAL_DEVICE => 
				non_electrical_device	: type_object_non_electrical;

		end case;
	end record;

	
	package pac_objects is new indefinite_doubly_linked_lists (type_object);



	-- Returns the number of items stored in the given list:
	function get_count (
		objects : in pac_objects.list)
		return natural;
	


	-- Returns the first object (electrical or non-electrical device)
	-- according to the given flag.
	-- If nothing found, then the return is a void object (CAT_VOID):
	function get_first_object (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object;

	
	-- Collects all objects (electrical and non-electrical devices)
	-- according to the given flag and returns them in a list:
	function get_objects (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return pac_objects.list;

	

	-- Modifies the status flag of an object:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);



	-- Modifies the status flag of an object indicated by a cursor:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		object_cursor	: in pac_objects.cursor;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);


	-- Resets the status flags of 
	-- all non-electrical devices:
	procedure reset_status_objects (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);

	

-- MOVE, DELETE, FLIP:
	
	procedure move_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level);


	procedure rotate_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level);

	
	
	procedure delete_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level);


	procedure flip_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level);
	
	

-- SHOW:
	
	procedure show_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level);

	

	
	-- Returns the positions (x/y) of the terminals of
	-- devices, netchangers and submodules of the given net.
	-- The default assembly variant is assumed (means all devices are mounted).
	-- If the argument "observe_techno" is true, then the technology (SMT, THT)
	-- given in argument "technology" is used to filter out terminals:
	-- If "observe_techno" is false (default) then all terminals are processed
	-- and "technology" has no meaning (don't care):
	function get_terminal_positions (
		module_cursor	: in pac_generic_modules.cursor;
		net_cursor		: in et_nets.pac_nets.cursor;
		observe_techno	: in boolean := false;
		technology		: in type_assembly_technology := assembly_technology_default;
		log_threshold	: in type_log_level)
		return pac_geometry_brd.pac_vectors.list;





	-- Returns the position of a terminal of the given device in the board.
	-- The device must be real (appearance SCH_PCB).
	function get_terminal_position (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in pac_devices_electrical.cursor; -- IC45
		terminal_name	: in pac_terminal_name.bounded_string) -- H7, 14
		return type_terminal_position_fine;

	
	-- CS ?
	-- Same as above function but takes a terminal cursor instead of a terminal name
	--function get_terminal_position (
		--module_cursor	: in pac_generic_modules.cursor;
		--device_cursor	: in et_schematic.pac_devices_electrical.cursor; -- IC45
		--terminal_cursor	: in pac_terminals.cursor) -- H7, 14
		--return type_terminal_position_fine;




	-- This controlled type is used by the functon to_polygon below:
	type type_terminal_polygon (exists : boolean) is record
		case exists is
			when TRUE	=> 
				polygon		: et_board_geometry.pac_polygons.type_polygon;
				position	: type_terminal_position_fine;
				
			when FALSE	=> null;
		end case;
	end record;

	
	-- Returns the position of a terminal and its contour as a polygon.
	-- If the terminal does not affect the given layer category,
	-- then nothing happens here -> returns just a "false".
	-- See specification of type_terminal_polygon above.
	function to_polygon (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in pac_devices_electrical.cursor;
		terminal_cursor	: in pac_terminals.cursor;
		layer_category	: in type_signal_layer_category;
		tolerance		: in type_distance_positive)
		return type_terminal_polygon;

	


	-- Returns the unconnected terminals of the given device
	-- in the given module. This query assumes the default assembly
	-- variant, means the device of interest exists in any case.
	-- If the given device is virtual, then an empty list will be returned:
	function get_unconnected_terminals (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in pac_devices_electrical.cursor) -- IC45
		return pac_terminals.map;

	
	
end et_board_ops.devices;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
