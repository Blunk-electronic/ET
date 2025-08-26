------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         SCHEMATIC OPERATIONS                             --
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

with ada.text_io;				use ada.text_io;
with ada.characters.latin_1;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;

with ada.containers;            use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with ada.exceptions;			use ada.exceptions;

with et_meta;
with et_net_names;				use et_net_names;
with et_module_names;			use et_module_names;
with et_module_instance;		use et_module_instance;
with et_sheets;					use et_sheets;
with et_schematic_coordinates;	use et_schematic_coordinates;
use et_schematic_coordinates.pac_geometry_2;

with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;
with et_nets;					use et_nets;
with et_net_junction;			use et_net_junction;
with et_net_ports;				use et_net_ports;
with et_net_segment;			use et_net_segment;
with et_net_labels;				use et_net_labels;
with et_project;				use et_project;
with et_generic_module;			use et_generic_module;
with et_text;
with et_submodules;
with et_assembly_variants;		use et_assembly_variants;
with et_assembly_variant_name;	use et_assembly_variant_name;
with et_numbering;
with et_material;
with et_netlists;
with et_terminals;
with et_port_names;				use et_port_names;
with et_symbol_ports;			use et_symbol_ports;
with et_symbols;
with et_device_library;			use et_device_library;
with et_package_variant;		use et_package_variant;
with et_device_purpose;			use et_device_purpose;
with et_device_model_names;		use et_device_model_names;
with et_device_value;			use et_device_value;
with et_device_prefix;			use et_device_prefix;
with et_device_name;			use et_device_name;
with et_device_partcode;		use et_device_partcode;
with et_conventions;

with et_schematic_text;			use et_schematic_text;
with et_unit_name;				use et_unit_name;
with et_units;					use et_units;
with et_devices_electrical;		use et_devices_electrical;

with et_logging;				use et_logging;
with et_exceptions;				use et_exceptions;


package et_schematic_ops is

	use pac_generic_modules;

	use pac_net_name;

	-- CS rework procedures so that a module cursor
	-- is used instead the module_name.
	

	-- Fetches the basic meta information of the schematic:
	function get_basic_meta_information (
		module : in pac_generic_modules.cursor)
		return et_meta.type_basic;


	
	-- Returns the list of preferred schematic libraries:
	function get_preferred_libraries (
		module : in pac_generic_modules.cursor)
		return et_meta.pac_preferred_libraries_schematic.list;

	
	
	procedure device_not_found (name : in type_device_name);
	procedure device_already_exists (name : in type_device_name);
	procedure relative_rotation_invalid;
	procedure net_not_found (name : in pac_net_name.bounded_string);
	procedure assembly_variant_not_found (variant : in pac_assembly_variant_name.bounded_string);



	
	-- Deletes ports of the given device in module.nets.
	-- The port names are relevant here. Their x/x positions are irrelevant.
	procedure delete_ports (
		module			: in pac_generic_modules.cursor; -- the module
		device			: in type_device_name;			-- the device
		ports			: in pac_ports.map := pac_ports.empty_map; -- the ports (if empty, all ports of the device will be deleted)
		sheets			: in pac_unit_positions.map;	-- the sheet numbers where the units can be found. CS implementation required
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
		ports			: in pac_ports.map; -- the ports to be inserted
		sheet			: in type_sheet;				-- the sheet to look at
		log_threshold	: in type_log_level);

	
	
	procedure delete_device (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		log_threshold	: in type_log_level);

	


	-- Returns the name of the active assembly variant of the given module:
	function get_active_assembly_variant (
		module_cursor	: in pac_generic_modules.cursor)
		return pac_assembly_variant_name.bounded_string;

	
	-- Returns a cursor to the active assembly variant of the given module:	
	function get_active_assembly_variant (
		module_cursor	: in pac_generic_modules.cursor)
		return et_assembly_variants.pac_assembly_variants.cursor;


	
	-- Returns a cursor to the net that is connected with the given device and terminal.
	-- If there is no net connected, then the return is no_element.
	-- Assumes the default assembly variant:
	function get_net (
		module		: in pac_generic_modules.cursor;
		device		: in pac_devices_sch.cursor;
		terminal	: in et_terminals.pac_terminal_name.bounded_string) -- H7, 1, 16
		return pac_nets.cursor;

	
	

	type type_drag is record
		before		: type_vector_model;
		after		: type_vector_model;
	end record;


	package type_drags_of_ports is new ada.containers.ordered_maps (
		key_type		=> pac_port_name.bounded_string,
		"<"				=> pac_port_name."<",
		element_type	=> type_drag);

	
	
	-- Returns lists of device, netchanger and 
	-- submodule ports at the given place:
	function get_ports ( 
		module_cursor	: in pac_generic_modules.cursor;
		place			: in type_object_position;
		log_threshold	: in type_log_level)		
		return type_ports;	
	

	

	procedure dragging_not_possible (
		port 		: in string;
		position	: in type_object_position);

	
	-- Renames a device.
	-- Raises exception if name before equals name after.
	-- Raises exception if prefix changes. For example renaming from
	-- R1 to C1 is forbidden as this would change the device category.
	procedure rename_device (
		module_name			: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name_before	: in type_device_name; -- IC1
		device_name_after	: in type_device_name; -- IC23
		log_threshold		: in type_log_level);

	-- CS procedure rename_device that takes a module cursor and a device 
	-- cursor for device_name_before.

	
	-- Sets the value of a device.
	procedure set_value (
		module_name			: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name			: in type_device_name; -- R2
		value				: in pac_device_value.bounded_string; -- 470R
		log_threshold		: in type_log_level);

	-- CS procedure set_value that takes a module cursor and a device cursor.
	
	-- Sets the purpose of a device.
	procedure set_purpose (
		module_name			: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name			: in type_device_name; -- R2
		purpose				: in pac_device_purpose.bounded_string; -- brightness_control
		log_threshold		: in type_log_level);

	-- CS procedure set_purpose that takes a module cursor and a device cursor.

	
	-- Sets the partcode of a device.
	procedure set_partcode (
		module_name			: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name			: in type_device_name; -- R2
		partcode			: in pac_device_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
		log_threshold		: in type_log_level);

	-- CS procedure set_partcode that takes a module cursor and a device cursor.

	
	-- Returns true if the given module provides the given device.
	-- The module being searched in must be in the rig already.						
	function exists ( -- CS rename to device_exists
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name)
		return boolean;

	
	-- Locates the given device in the given module and returns
	-- the cursor to the device.
	-- If the device does not exist, returns no_element.
	function locate_device (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- R2
		return pac_devices_sch.cursor;

	
	-- Returns the cursor of the device model
	-- for the given device in the module.
	-- Raises exception if device does not exist.
	function locate_device (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- R2
		return pac_devices_lib.cursor;



	
	-- Locates the given device in the given module and returns
	-- the name of the device model (like 7400.dev).
	-- Raises constraint error if the device does not exist.
	function device_model_name ( -- CS get_device_model_name
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- R2
		return pac_device_model_file.bounded_string; -- 7400.dev

	
	-- Returns the package variants available for the
	-- given device.
	-- The device must be real. Otherwise constraint error rises.
	function get_available_variants (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- R2
		return pac_variants.map;

	
	-- Returns the name of the package variant name of the device.
	-- Raises constraint error if the device does not exist.
	-- Raises semantic error if the device is virtual.
	function get_variant (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- R2
		return pac_package_variant_name.bounded_string; -- D, N

	
	-- Sets the package variant of a device.
	-- Raises constraint error if the device does not exist.
	-- Raises semantic error if the device is virtual.
	-- Raises semantic error if the variant is not defined
	-- in the according device model:
	procedure set_variant (
		module	: in pac_generic_modules.cursor;
		device	: in pac_devices_sch.cursor;
		variant	: in pac_package_variant_name.bounded_string);

	
	-- Sets the package variant of a device.
	-- Raises semantic error if the device does not exist.
	-- Raises semantic error if the device is virtual.
	-- Raises semantic error if the variant is not defined
	-- in the according device model:
	procedure set_variant (
		module			: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device			: in type_device_name; -- R2
		variant			: in pac_package_variant_name.bounded_string; -- N, D
		log_threshold	: in type_log_level);


	
	-- Locates the given device in the given module and returns
	-- the cursor to the device model.
	-- Raises constraint error if the device does not exist.
	function device_model_cursor ( -- CS rename to get_device_model_cursor
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- R2
		return pac_devices_lib.cursor;


	


	-- To distinguish between electrical and non-electrical devices
	-- use this type:
	type type_device_category is (ELECTRICAL, NON_ELECTRICAL);
	-- CS: This type and the function get_next_device_name (see below)
	-- seem misplaced here.	
	
	-- Returns for the given device prefix the next available device name in the module.
	-- Example: prefix is C. If there are C1, C12, C1034 and C1035 the return will be C2.
	function get_next_device_name (
		module_cursor	: in pac_generic_modules.cursor;
		prefix			: in pac_device_prefix.bounded_string; -- C
		category		: in type_device_category := ELECTRICAL)
		return type_device_name; -- C2

	
	
	-- Adds a device to the schematic. The unit is determined by the unit add levels.
	-- If the given variant is empty (zero length) the the device is assumed to be virtual.							 
	procedure add_device (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_model	: in pac_device_model_file.bounded_string; -- ../libraries/devices/logic_ttl/7400.dev
		variant			: in pac_package_variant_name.bounded_string; -- N, D, S_0805
		destination		: in type_object_position; -- sheet/x/y/rotation
		log_threshold	: in type_log_level);

	-- CS procedure add_device with explicit device name like R12

	-- CS procedure add_device that takes module cursor and model cursor
	
	-- Copies the given device. Places the first unit of the device (according to add level)
	-- at the given destination in the schematic.
	procedure copy_device (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		destination		: in type_object_position; -- sheet/x/y
		log_threshold	: in type_log_level);



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


	

	
	-- Creates a new assembly variant.
	procedure create_assembly_variant (
		module_name		: in pac_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in pac_assembly_variant_name.bounded_string; -- low_cost
		log_threshold	: in type_log_level);

	
	-- Deletes an assembly variant.
	procedure delete_assembly_variant (
		module_name		: in pac_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in pac_assembly_variant_name.bounded_string; -- low_cost
		log_threshold	: in type_log_level);

	
	-- Describes an assembly variant. Overwrites the previous description.
	procedure describe_assembly_variant (
		module_name		: in pac_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in pac_assembly_variant_name.bounded_string; -- low_cost
		description		: in et_assembly_variants.type_description; -- "this is the low budget variant"
		log_threshold	: in type_log_level);

	
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

	
	
	function sort_by_coordinates_2 (
		module_cursor 	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
		return et_numbering.pac_devices.map;

	
	

	
	-- Renumbers devices according to the sheet number.
	procedure renumber_devices (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		step_width		: in type_name_index;
		log_threshold	: in type_log_level);


	
end et_schematic_ops;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
