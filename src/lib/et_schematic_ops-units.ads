------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    SCHEMATIC OPERATIONS ON UNITS                         --
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


with et_device_placeholders;			use et_device_placeholders;
with et_object_status;					use et_object_status;


package et_schematic_ops.units is


	-- Returns the sheet/x/y position of the given device,
	-- unit and port:
	function get_position (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC34
		port_name		: in pac_port_name.bounded_string; -- CE
		log_threshold	: in type_log_level)
		return type_object_position;


	

	-- Moves the name placeholder of the given unit.
	procedure move_unit_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model; -- x/y
		meaning			: in type_placeholder_meaning; -- name, value, purpose
		log_threshold	: in type_log_level);




	
	-- Rotates the given unit placeholder about its origin.
	-- The rotation is absolute.										  
	procedure rotate_unit_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		rotation		: in et_text.type_rotation_documentation; -- absolute ! -- 90
		meaning			: in type_placeholder_meaning; -- name, value, purpose		
		log_threshold	: in type_log_level);



	
	-- Locates the given unit of the given device in the 
	-- given module and returns the cursor to the unit.
	-- If the unit does not exist, returns no_element.
	-- Raises exception if device does not exist.
	function locate_unit (
		module_cursor	: in pac_generic_modules.cursor;
		device			: in type_device_name; -- R2
		unit			: in pac_unit_name.bounded_string)
		return pac_units.cursor;
	


	
	-- Returns true if the unit of the given device in the 
	-- given module has been deployed somewhere.
	-- If the unit has not been deployed yet, returns false.
	-- Raises exception if device does not exist.
	function is_deployed (
		module_cursor	: in pac_generic_modules.cursor;
		device			: in type_device_name; -- R2
		unit			: in pac_unit_name.bounded_string)
		return boolean;


	
	-- Returns true if given device with the given port exists in module indicated by module_cursor.
	function device_port_exists (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		device_name		: in type_device_name; -- IC45
		port_name		: in pac_port_name.bounded_string) -- CE
		return boolean;

	
	
	-- Returns true if given device exists in module indicated by module_cursor.
	-- The unit and port names are optionally.
	function device_unit_port_exists (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string := to_unit_name (""); -- A
		port_name		: in pac_port_name.bounded_string := to_port_name ("")) -- CE		
		return boolean;						


	
	-- Returns the names of available units of the given device in the 
	-- given generic module. "Available" means the unit exists and is
	-- not already placed somewhere in the schematic:
	function get_available_units (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC1
		log_threshold	: in type_log_level)
		return pac_unit_names.list;


	
	-- Returns true if the given unit is available.
	-- Raises constraint error if device does not exist or
	-- unit is not defined in device model of given device:
	function unit_available (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC1
		unit_name		: in pac_unit_name.bounded_string)
		return boolean;


	
	-- Returns the names of units of the given device in the 
	-- given generic module on the given sheet.
	function get_units_on_sheet (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC1
		sheet			: in type_sheet;
		log_threshold	: in type_log_level)
		return pac_unit_names.list;


	
	-- Returns the position (x/y/sheet) of the given unit.
	-- Raises constraint error if device or unit does not exist.
	function get_position (
		module_cursor	: in pac_generic_modules.cursor;
		device			: in type_device_name; -- R2
		unit			: in pac_unit_name.bounded_string)
		return type_object_position;



	
	
	
	-- Returns the sheet number of the given unit.
	-- Raises constraint error if device or unit does not exist.
	function get_sheet (
		module_cursor	: in pac_generic_modules.cursor;
		device			: in type_device_name; -- R2
		unit			: in pac_unit_name.bounded_string)
		return type_sheet;



	

	-- Fetches a unit from a device into the schematic.
	procedure fetch_unit (
		module_cursor 	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC1
		unit_name		: in pac_unit_name.bounded_string; -- A, B, IO_BANK_2
		destination		: in type_object_position; -- sheet/x/y/rotation
		log_threshold	: in type_log_level);

	-- CS procedure fetch_unit that takes module cursor and model cursor ?


	

	-- Returns true if no unit sits on top of another.
	function unit_positions_valid (
		module_cursor 	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
		return boolean;


	
	-- Returns properties of the given device port in module indicated by module_cursor.
	-- Properties are things like: terminal name, direction, sensitivity, power level, ...
	-- Assumes the default assembly variant (means ALL devices are mounted).
	-- See et_libraries.type_port for detail.
	-- The device must exist in the module and must be real. Run intergrity check
	-- in case exception occurs here.
	function get_port_properties (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A, B, IO_BANK_2
		port_name		: in pac_port_name.bounded_string) -- CE
		return type_port_properties_access;


	


	-- CS type_port_query

	-- CS function port_position 


	
	-- Deletes a unit of a device. 
	-- In case the last unit has been deleted, then the device is 
	-- deleted entirely from the module.
	procedure delete_unit (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		log_threshold	: in type_log_level);


	-- Deletes a whole device (incl. all its units)
	-- in the module:
	procedure delete_device (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		log_threshold	: in type_log_level);

	

	-- Moves the given unit within the schematic. Disconnects the unit from
	-- start or end points of net segments BEFORE the move. 
	-- Connects unit ports with segment end or start points AFTER the move.
	procedure move_unit (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		sheet			: in type_sheet_relative; -- -3/0/2
		destination		: in type_vector_model; -- x/y
		log_threshold	: in type_log_level);



	-- Returns the position of given unit. If the unit_name is empty ("")
	-- then the position of the first unit is returned.
	-- This is useful when a device has only one unit.
	-- If the given device or unit does not exist, then the return is false.
	function get_unit_position (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string) -- C
-- 		port_name		: in et_symbols.pac_port_name.bounded_string) -- CE
		return type_unit_query;

	
	
	-- Tests whether the given unit ports at their individual location are movable. 
	-- The criteria for movement are: no netchanger port, no device port, no submodule ports there.
	-- The only port allowed at an individual drag point is the port-to-be-dragged itself.
	-- CS: Might become obsolete once ports at the same x/y position are prevented.
	function is_movable (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name;
		unit_name		: in pac_unit_name.bounded_string;
		location 		: in type_object_position; -- only sheet number matters
		unit_ports		: in pac_ports.map;
		log_threshold	: in type_log_level)
		return boolean;

	
	-- Drags the given unit within the schematic.
	-- Already existing connections with net segments are kept.
	-- Net segment positions are modified.
	-- Net segment positions are modified.
	-- CS: Before the drag: If a port of the unit sits at the same place
	--     where a port of another unit is, then a net segment should be
	--     inserted between them ?
	-- This operation applies to a single sheet. Dragging from one sheet
	-- to another is not possible.
	procedure drag_unit (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model;
		log_threshold	: in type_log_level);


	-- Rotates the given unit. 
	-- Disconnects the unit from attached net segments before the rotation.
	-- Connects the unit with net segments after the rotation.
	-- Rotates the placeholders about the unit center.
	procedure rotate_unit (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute		
		rotation		: in type_rotation_model; -- 90
		log_threshold	: in type_log_level);





	-- This composite type is meant to identify a unit
	-- and its parent device in the schematic:
	type type_object_unit is record
		device_cursor	: pac_devices_sch.cursor;

		-- If the cursor to the unit is no_element then
		-- the whole device is addressed:
		unit_cursor		: pac_units.cursor;
	end record;



	-- Returns the device name of the given object
	-- as string in the form like "IC12":
	function get_device_name (
		object	: in type_object_unit)
		return string;
	
							   
	-- Returns the full name of the given object
	-- as string in the form like "IC12.B":
	function get_object_name (
		object	: in type_object_unit)
		return string;

	

	-- Modifies the status flag of a unit.
	-- If the unit is set as moving, then its
	-- original position will be backup
	-- in global variable object_original_position:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		unit			: in type_object_unit;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);

	
	
	-- Sets the proposed-flag of all units which are in the
	-- given zone around the given place on the currently active sheet.
	-- Adds to count the number of units that have been found:
	procedure propose_units (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level);
								


	-- Clears the proposed-flag and the selected-flag of all units:
	procedure reset_proposed_units (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);



	-- Returns the first unit according to the given flag.
	-- If no unit has been found,
	-- then the return is no_element:
	function get_first_unit (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_unit;



------------------------------------------------------------------------------------------

-- OBJECTS:


	type type_object_category is (
		CAT_VOID,
		CAT_UNIT);


	-- This type wraps all kinds of objects into a single type:
	type type_object (cat : type_object_category) is record
		case cat is
			when CAT_VOID => null;
			
			when CAT_UNIT =>
				unit : type_object_unit;

			-- CS CAT_PLACEHOLDER_VALUE, NAME, PURPOSE ?
		end case;
	end record;


	
	
	package pac_objects is new indefinite_doubly_linked_lists (type_object);


	-- Returns the number of items stored in the given list:
	function get_count (
		objects : in pac_objects.list)
		return natural;
	


	-- Returns the first object
	-- according to the given flag.
	-- If nothing found, then the return is a void object (CAT_VOID):
	function get_first_object (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object;



	-- Collects all objects 
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
	

	-- This is a collective procedure that resets
	-- the proposed-flag and the selected-flag 
	-- of all objects:
	procedure reset_proposed_objects (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);


	-- Moves an object to the given destination:
	procedure move_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level);


	-- Rotates an object by 90 degrees:
	procedure rotate_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level);


	-- Sets the start or end points of net segments which are 
	-- connected with ports of selected units to "moving":
	procedure set_segments_moving (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);
	
	
	procedure drag_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level);

	
	procedure delete_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level);

	
	
end et_schematic_ops.units;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
