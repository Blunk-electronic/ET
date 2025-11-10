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

with et_units;							use et_units;
with et_unit_name;						use et_unit_name;
with et_device_placeholders;			use et_device_placeholders;
with et_object_status;					use et_object_status;
with et_device_category_electrical_non_electrical;	use et_device_category_electrical_non_electrical;
with et_device_property_level;			use et_device_property_level;


package et_schematic_ops.units is


-- BASIC QUERY OPERATIONS ON DEVICES AND DEVICE MODELS:
	

	-- Returns true if the device exists in the given module:
	function electrical_device_exists (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name)
		return boolean;

	
	-- Returns the cursor to the given electrical device
	-- in the given module.
	-- If the device does not exist, then no_element is returned:
	function get_electrical_device (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- R2
		return pac_devices_electrical.cursor;

	
	-- Returns the cursor to the device model
	-- for the given device in the given module.
	-- If the device does not exist, then no_element is returned:
	function get_device_model (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- R2
		return pac_devices_lib.cursor;
	
	
	-- Returns the name of the device model of the
	-- given device in the given module.
	-- The model name is the name of the file (like 7400.dev)
	-- that contains the actual model.
	-- Raises constraint error if the device does not exist.
	function get_device_model (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- R2
		return pac_device_model_file.bounded_string; -- 7400.dev



	


-- ASSEMBLY VARIANT:
	
	-- Returns true if the given module and assembly variant 
	-- provides the given device.
	-- Assumptions: 
	-- - The module being searched in must be in the rig already.
	-- - The assembly variant must exist in the module.
	-- - The device must exist in the module.
	function device_exists (
		module	: in pac_generic_modules.cursor; -- the module like motor_driver
		variant	: in pac_assembly_variant_name.bounded_string; -- low_cost				
		device	: in type_device_name)
		return boolean;



	-- Returns a cursor to the alternative device in the given module
	-- and given assembly variant.
	-- Assumptions: 
	-- - The module being searched in must be in the rig already.
	-- - The assembly variant must exist in the module.
	-- - The device must exist in the module.
	-- - The device must have an entry in the given assembly variant,
	--   otherwise the return is no_element.
	function get_alternative_device (
		module	: in pac_generic_modules.cursor; -- the module like motor_driver
		variant	: in pac_assembly_variant_name.bounded_string; -- low_cost				
		device	: in type_device_name)
		return pac_device_variants.cursor;




	-- Sets the value, partcode and (optionally the purpose) of a device in 
	-- An already existing device will be overwritten without warning.
	procedure mount_device (
		module_name		: in pac_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in pac_assembly_variant_name.bounded_string; -- low_cost
		device			: in type_device_name; -- R1
		value			: in pac_device_value.bounded_string; -- 220R
		partcode		: in pac_device_partcode.bounded_string; -- R_PAC_S_0805_VAL_220R
		purpose			: in pac_device_purpose.bounded_string := pac_device_purpose.to_bounded_string (""); -- set temperature
		log_threshold	: in type_log_level);

	
	-- Sets the given device as not mounted in the given assembly variant.
	-- Sets the gvien device as not mounted in 
	-- the given assembly variant. An already existing device will be overwritten
	-- without warning.
	procedure unmount_device (
		module_name		: in pac_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in pac_assembly_variant_name.bounded_string; -- low_cost
		device			: in type_device_name; -- R1
		log_threshold	: in type_log_level);

	
	-- Removes the given device from the given assembly variant.
	procedure remove_device (
		module_name		: in pac_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in pac_assembly_variant_name.bounded_string; -- low_cost
		device			: in type_device_name; -- R1
		log_threshold	: in type_log_level);




-- SHOW DEVICE:

	-- Sets all units or an explicitly given unit as selected.
	-- The search mode is specified by "all_units".
	-- If all_units is true, then the parameter "unit" is ignored
	-- and all units adressed.
	-- If all_units is false, then only the unit specified by "unit_name"
	-- will be addressed.
	-- In any case the package will be set as selected so that is
	-- becomes highlighted in the board domain.
	-- If the given device does not exist, then a warning
	-- is written in the log and the error flag is set.
	-- If output_warning is false then no warning will be logged:
	procedure show_device (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- R2, IC4
		all_units		: in boolean;
		unit_name		: in pac_unit_name.bounded_string := unit_name_default;
		error			: out boolean;
		log_warning		: in boolean := true;
		log_threshold	: in type_log_level);


	-- Returns properties of the given device. 
	-- 1. If the given device does not exist, then error is set and an empty 
	--    string will be returned.
	-- 2. Level determines the degree and amount of information to be returned.
	-- 3. If all_units is true, then no special focus is on a certain unit
	--    and information about all units is returned.
	-- 4. If all_units is false, then only properties of the given unit
	--    (via unit_name) is returned. If a unit named after unit_name does
	--    not exist, then error is set and an empty string returned.
	-- 5. By default no linebreaks are inserted in the output,
	--    so that the result is a single line.
	-- 6. If linebreaks is true, then linebreaks are inserted.
	--    This is useful when the output is to be displayed
	--    in a window or if it is to be written in a file:	
	function get_device_properties (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name;
		level			: in type_properties_level;
		all_units		: in boolean := true;
		unit_name		: in pac_unit_name.bounded_string := unit_name_default;
		linebreaks		: in boolean := false;
		error			: out boolean;
		log_threshold	: in type_log_level)
		return string;


	
	

-- VALUE, PURPOSE, PARTCODE:
	
	
	-- Sets the value of a device.
	procedure set_value (
		module_cursor		: in pac_generic_modules.cursor;
		device_name			: in type_device_name; -- R2
		value				: in pac_device_value.bounded_string; -- 470R
		log_threshold		: in type_log_level);

	-- CS procedure set_value that takes a device cursor.

	
	-- Sets the purpose of a device.
	procedure set_purpose (
		module_cursor		: in pac_generic_modules.cursor;
		device_name			: in type_device_name; -- R2
		purpose				: in pac_device_purpose.bounded_string; -- brightness_control
		log_threshold		: in type_log_level);

	-- CS procedure set_purpose that takes a device cursor.

	
	-- Sets the partcode of a device.
	procedure set_partcode (
		module_cursor		: in pac_generic_modules.cursor;
		device_name			: in type_device_name; -- R2
		partcode			: in pac_device_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
		log_threshold		: in type_log_level);

	-- CS procedure set_partcode that takes a device cursor.





	
-- PACKAGE VARIANT:
	
	-- Returns the package variants available for the
	-- given device.
	-- The device must be real. Otherwise constraint error rises.
	function get_available_package_variants (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- R2
		return pac_package_variants.map;


	
	-- Returns the name of the package variant of the device.
	-- The device must exist and it must be a real device.
	-- Otherwise an exception will be raised:
	function get_package_variant (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- R2
		return pac_package_variant_name.bounded_string; -- D, N

	
	
	-- Sets the package variant of a device.
	-- The device must exist, it must be a real device and
	-- the given package variant must be available.
	-- Otherwise a warning will be issued:
	procedure set_package_variant (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- R2
		variant			: in pac_package_variant_name.bounded_string; -- N, D
		log_threshold	: in type_log_level);






	
-- DEVICE ADD and COPY:


	-- Returns all electrical devices that have the given prefix:
	function get_electrical_devices_by_prefix (
		module_cursor	: in pac_generic_modules.cursor;
		prefix			: in pac_device_prefix.bounded_string; -- C
		log_threshold	: in type_log_level)
		return pac_devices_electrical.map;
	
	

	-- Returns for the given device prefix the next available 
	-- device name in the module.
	-- Example: prefix is C. If there are C1, C12, C1034 and C1035 
	-- then the return will be C2.
	-- Devices names are also used by non-electrical devices. So this
	-- function also looks into the non-electrical devices and returns
	-- a name that is not used by both electrical and non-electrical devices:
	function get_next_available_device_name (
		module_cursor	: in pac_generic_modules.cursor;
		prefix			: in pac_device_prefix.bounded_string; -- C
		log_threshold	: in type_log_level)
		return type_device_name; -- C2

	
	
	-- Adds a device to the schematic. 
	-- Determines the name of the device automatically by its
	-- prefix and the device indexes that are available (after IC43 follows IC44).
	-- The unit to be added is determined by the add levels of the units.
	-- If the given variant is empty (zero length) then
	-- the the device is assumed to be virtual (like a GND symbol). 
	-- CS: Reject the selected unit if a port overlaps a port of another
	-- existing unit.
	procedure add_electrical_device (
		module_cursor	: in pac_generic_modules.cursor;
		device_model	: in pac_device_model_file.bounded_string; -- ../libraries/devices/logic_ttl/7400.dev
		variant			: in pac_package_variant_name.bounded_string; -- N, D, S_0805
		destination		: in type_object_position; -- sheet/x/y/rotation
		log_threshold	: in type_log_level);

	-- CS procedure add_electrical_device with explicit device name like R12

	-- CS procedure add_electrical_device that takes model cursor


	
	-- Copies the given device. Places the first unit of 
	-- the device (according to add level)
	-- at the given destination in the schematic.
	procedure copy_device (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		destination		: in type_object_position; -- sheet/x/y
		log_threshold	: in type_log_level);



	

	


-- UNIT and PORT:
	
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



	-- Deletes ports of the given device and unit in the
	-- net segments on the given sheet:
	procedure delete_ports (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name;
		unit_name		: in pac_unit_name.bounded_string;
		ports			: in pac_symbol_ports.map;
		sheet			: in type_sheet;
		log_threshold	: in type_log_level);

	
	-- Inserts the given device ports in the net segments.
	-- If a port lands on either the start or end point of a segment, it will
	-- be regarded as "connected" with the segment.
	-- If a ports lands between start or end point of a segment, nothing happens
	-- because the docking to net segments is possible on segment ends/starts only.
	-- CS: Automatic splitting the segment into two and placing a junction is not supported
	-- jet and probably not a good idea.
	procedure insert_ports (
		module_cursor	: in pac_generic_modules.cursor;		-- the module
		device_name		: in type_device_name;					-- the device
		unit_name		: in pac_unit_name.bounded_string;	-- the unit name like A, C, PWR
		ports			: in pac_symbol_ports.map; -- the ports to be inserted
		sheet			: in type_sheet;				-- the sheet to look at
		log_threshold	: in type_log_level);


	
	

	-- Fetches a unit from a device into the schematic.
	-- CS: Reject unit if a port overlaps a port of another
	-- existing unit.
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




	
	


-- DEVICE DELETE and RENAME:


	
	-- Deletes a whole device (incl. all its units)
	-- in the module:
	procedure delete_electrical_device (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		log_threshold	: in type_log_level);



	-- Renames the device ports of the net segments.
	-- Leaves the unit and port names as they are because 
	-- this is solely about changing device names:
	procedure rename_device_ports (
		module_cursor	: in pac_generic_modules.cursor;
		device_before	: in type_device_name;
		device_after	: in type_device_name;
		unit_positions	: in pac_unit_positions.map;
		log_threshold	: in type_log_level);


	

	-- Renames a device.
	-- Changing the prefix is not allowed. A warning will be issued.
	-- For example renaming from R1 to C1 is forbidden as this would 
	-- change the device category:
	procedure rename_electrical_device (
		module_cursor		: in pac_generic_modules.cursor;
		device_name_before	: in type_device_name; -- IC1
		device_name_after	: in type_device_name; -- IC23
		log_threshold		: in type_log_level);




	

-- UNIT MOVE, DRAG, FETCH, DELETE:
		

	-- Moves the given unit within the schematic. Disconnects the unit from
	-- start or end points of net segments BEFORE the move. 
	-- Connects unit ports with segment end or start points AFTER the move.
	-- CS: Reject unit if a port overlaps a port of another
	-- existing unit.
	procedure move_unit (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		sheet			: in type_sheet_relative; -- -3/0/2
		destination		: in type_vector_model; -- x/y
		log_threshold	: in type_log_level);



	-- Returns the position of given unit. If the unit_name is empty ("")
	-- then the position of the first unit of the device is returned.
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
		unit_ports		: in pac_symbol_ports.map;
		log_threshold	: in type_log_level)
		return boolean;



	-- Drags the net segments according to the given drag list of a unit.
	-- Changes the position of start or end points of segments.
	-- 1. It does NOT create new connections with segments if a port
	--    lands on the start or end point of another segment.
	-- 2. It does NOT create a new connection with a segments if a port
	-- l  ands between start and end point.
	procedure drag_net_segments (
		module_cursor	: in pac_generic_modules.cursor;
		port_drag_list	: in type_port_drag_list; -- the old and new port positions
		log_threshold	: in type_log_level);


	
	-- Drags the given unit about the sheet.
	-- Already existing connections with net segments are kept.
	-- This operation applies to a single sheet. Dragging from one sheet
	-- to another is not possible.
	-- CS: Reject unit if a port overlaps a port of another
	-- existing unit.
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
	-- Rotates the placeholders about the unit origin.
	-- CS: Reject unit if a port overlaps a port of another
	-- existing unit.	
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
		device_cursor	: pac_devices_electrical.cursor;

		-- If the cursor to the unit is no_element then
		-- the whole device is addressed:
		unit_cursor		: pac_units.cursor;
	end record;



	-- Returns the device name of the given object
	-- as string in the form like "IC12":
	function get_device_name (
		object	: in type_object_unit)
		return string;


	function get_device_name (
		object	: in type_object_unit)
		return type_device_name;


	
	function get_unit_name (
		object	: in type_object_unit)
		return string;


	function get_unit_name (
		object	: in type_object_unit)
		return pac_unit_name.bounded_string;

	
	
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
	-- Adds to count the number of units that have been found.
	-- If real_only is true, then only real devices (which have a physical
	-- representation in the board drawing) are adressed:
	procedure propose_units (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		real_only		: in boolean := false;
		log_threshold	: in type_log_level);
								


	-- Resets the status flags of 
	-- all devices, their units and packages:
	procedure reset_status_units (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);



	-- Resets the status of packages of all real devices:
	procedure reset_status_devices (
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





	

-- PLACEHOLDERS:


	-- Moves the a unit placeholder of the given unit.
	procedure move_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model; -- x/y
		meaning			: in type_placeholder_meaning; -- name, value, purpose
		log_threshold	: in type_log_level);


	
	-- Rotates the given unit placeholder about its origin.
	-- The rotation is either horizontal or vertical.
	-- If toggle is true, then rotation is ignored and
	-- the rotation of the placeholder toggels between
	-- horizontal and vertical:
	procedure rotate_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		toggle			: in boolean := false;
		rotation		: in et_text.type_rotation_documentation := et_text.HORIZONTAL;
		meaning			: in type_placeholder_meaning; -- name, value, purpose		
		log_threshold	: in type_log_level);



	
	-- This composite type is meant to identify a 
	-- placeholder of a unit
	-- and its parent device in the schematic:
	type type_object_placeholder is record
		device_cursor	: pac_devices_electrical.cursor;
		unit_cursor		: pac_units.cursor;
		meaning			: type_placeholder_meaning;
	end record;


	function get_device_name (
		object : in type_object_placeholder)
		return type_device_name;
	

	function get_unit_name (
		object : in type_object_placeholder)
		return pac_unit_name.bounded_string;


	function get_meaning (
		object : in type_object_placeholder)
		return type_placeholder_meaning;



	function get_device_name (
		object : in type_object_placeholder)
		return string;
	

	function get_unit_name (
		object : in type_object_placeholder)
		return string;

	
	-- Returns the full name of the given object
	-- as string in the form like "IC12.B":
	function get_object_name (
		object	: in type_object_placeholder)
		return string;

	

	function get_meaning (
		object : in type_object_placeholder)
		return string;

	
	
	-- Modifies the status flag of a placeholder.
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		placeholder		: in type_object_placeholder;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);

	
	
	-- Sets the proposed-flag of all placeholders which are in the
	-- given zone around the given place on the currently active sheet.
	-- Adds to count the number of placeholders that have been found:
	procedure propose_placeholders (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level);
								


	-- Resets the status flags of all placeholders:
	procedure reset_status_placeholders (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);



	-- Returns the first placeholder according to the given flag.
	-- If no placeholder has been found,
	-- then the return is no_element:
	function get_first_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_placeholder;


	

------------------------------------------------------------------------------------------

-- OBJECTS:


	type type_object_category is (
		CAT_VOID,
		CAT_UNIT,
		CAT_PLACEHOLDER
		);


	-- This type wraps all kinds of objects into a single type:
	type type_object (cat : type_object_category) is record
		case cat is
			when CAT_VOID => null;
			
			when CAT_UNIT =>
				unit : type_object_unit;

			when CAT_PLACEHOLDER =>
				placeholder : type_object_placeholder;
				
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
	-- the status flags of all 
	-- objects (electrical devices, units, placeholders, nets, net labels):
	procedure reset_status_objects (
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


	procedure show_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level);

	
	procedure rename_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		new_name_device	: in type_device_name;
		-- CS add argument for new names of other kinds of objects
		log_threshold	: in type_log_level);
	

	procedure copy_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		destination		: in type_position;		
		log_threshold	: in type_log_level);

	
	procedure set_value (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		new_value		: in pac_device_value.bounded_string;
		log_threshold	: in type_log_level);


	procedure set_purpose (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		new_purpose		: in pac_device_purpose.bounded_string;
		log_threshold	: in type_log_level);


	procedure set_partcode (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		new_partcode	: in pac_device_partcode.bounded_string;
		log_threshold	: in type_log_level);


	procedure set_package_variant (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		new_variant		: in pac_package_variant_name.bounded_string;
		log_threshold	: in type_log_level);

	
end et_schematic_ops.units;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
