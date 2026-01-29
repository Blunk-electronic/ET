------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     SCHEMATIC OPERATIONS / DEVICE                        --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
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


with ada.exceptions;			use ada.exceptions;

with et_schematic_coordinates;	use et_schematic_coordinates;
with et_module_names;			use et_module_names;

with et_logging;				use et_logging;

with et_generic_modules;		use et_generic_modules;

with et_assembly_variants;		use et_assembly_variants;
with et_assembly_variant_name;	use et_assembly_variant_name;
with et_material;

with et_device_purpose;			use et_device_purpose;
with et_device_value;			use et_device_value;
with et_device_name;			use et_device_name;
with et_device_partcode;		use et_device_partcode;

with et_unit_name;				use et_unit_name;

with et_device_prefix;			use et_device_prefix;
with et_device_model_names;		use et_device_model_names;
with et_device_library;			use et_device_library;
with et_devices_electrical;		use et_devices_electrical;
with et_numbering;				use et_numbering;

with et_package_variant_name;	use et_package_variant_name;
with et_package_variant;		use et_package_variant;

with et_device_property_level;	use et_device_property_level;
with et_logging;				use et_logging;


package et_schematic_ops_device is

	use pac_generic_modules;


	-- CS rework procedures so that a module cursor
	-- is used instead the module_name.
	

	
	
	-- procedure device_not_found (name : in type_device_name);
	-- procedure device_already_exists (name : in type_device_name);



	-- CS description !
	function sort_by_coordinates_2 (
		module_cursor 	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
		return et_numbering.pac_devices.map;



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
		return pac_device_models.cursor;
	
	
	-- Returns the name of the device model of the
	-- given device in the given module.
	-- The model name is the name of the file (like 7400.dev)
	-- that contains the actual model.
	-- Raises constraint error if the device does not exist.
	function get_device_model (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- R2
		return pac_device_model_file.bounded_string; -- 7400.dev



	


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






	
-- ADD and COPY:


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





	
	
	
	-- Renumbers devices according to the sheet number.
	procedure renumber_devices (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		step_width		: in type_name_index;
		log_threshold	: in type_log_level);




	
end et_schematic_ops_device;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
