------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         SCHEMATIC OPERATIONS                             --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
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

with et_net_names;				use et_net_names;
with et_general;				use et_general;
with et_geometry;				use et_geometry;
with et_sheets;					use et_sheets;
with et_coordinates_2;			use et_coordinates_2;
use et_coordinates_2.pac_geometry_2;

with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;
with et_nets;					use et_nets;
with et_net_labels;				use et_net_labels;
with et_schematic;				use et_schematic;
with et_project;				use et_project;
with et_project.modules;		use et_project.modules;
with et_text;
with et_submodules;
with et_assembly_variants;		use et_assembly_variants;
with et_numbering;
with et_material;
with et_netlists;
with et_terminals;
with et_symbols;
with et_devices;				use et_devices;
with et_conventions;

with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.packages;

with et_schematic_shapes_and_text;		use et_schematic_shapes_and_text;


package et_schematic_ops is

	use pac_generic_modules;
	use pac_text_schematic;

	use pac_net_name;

	-- CS rework procedures so that a module cursor
	-- is used instead the module_name.
	

	
	procedure device_not_found (name : in type_device_name);
	procedure device_already_exists (name : in type_device_name);
	procedure relative_rotation_invalid;
	procedure netchanger_not_found (index : in et_submodules.type_netchanger_id);
	procedure submodule_not_found (name : in pac_module_instance_name.bounded_string);	
	procedure net_not_found (name : in pac_net_name.bounded_string);
	procedure assembly_variant_not_found (variant : in pac_assembly_variant_name.bounded_string);

	procedure unit_not_found (name : in pac_unit_name.bounded_string);
	
	-- Writes the positions of the device units in the log file.
	procedure log_unit_positions (
		positions 		: in pac_unit_positions.map;
		log_threshold	: in type_log_level);

	
	-- Returns a map of ports of the given device and unit.
	-- The coordinates of the ports are default xy-positions relative
	-- to the center of the unit as they are defined in the symbol model.
	function get_ports_of_unit (
		device_cursor	: in pac_devices_sch.cursor;
		unit_name		: in pac_unit_name.bounded_string)
		return et_symbols.pac_ports.map;

	
	-- Deletes ports of the given device in module.nets.
	-- The port names are relevant here. Their x/x positions are irrelevant.
	procedure delete_ports (
		module			: in pac_generic_modules.cursor;		-- the module
		device			: in type_device_name;			-- the device
		ports			: in et_symbols.pac_ports.map := et_symbols.pac_ports.empty_map; -- the ports (if empty, all ports of the device will be deleted)
		sheets			: in pac_unit_positions.map;	-- the sheet numbers where the units can be found. CS implementation required
		log_threshold	: in type_log_level);

	
	-- Moves the given unit ports by given offset.
	procedure move_ports ( -- CS move to et_symbols ?
		ports	: in out et_symbols.pac_ports.map; -- the portlist
		offset	: in et_coordinates_2.type_position); -- the offset (only x/y matters)

	
	-- Inserts the given device ports in the net segments.
	-- If a port lands on either the start or end point of a segment, it will
	-- be regarded as "connected" with the segment.
	-- If a ports lands between start or end point of a segment, nothing happens
	-- because the docking to net segments is possible on segment ends/starts only.
	-- CS: Automatic splitting the segment into two and placing a junction is not supported
	-- jet and probably not a good idea.
	procedure insert_ports (
		module			: in pac_generic_modules.cursor;		-- the module
		device			: in type_device_name;					-- the device
		unit			: in pac_unit_name.bounded_string;	-- the unit name like A, C, PWR
		ports			: in et_symbols.pac_ports.map; -- the ports to be inserted
		sheet			: in type_sheet;				-- the sheet to look at
		log_threshold	: in type_log_level);

	
	-- Rotates the given unit ports by given angle about the origin.
	procedure rotate_ports ( -- CS move to et_symbols ?
		ports	: in out et_symbols.pac_ports.map; -- the portlist
		angle	: in et_coordinates_2.type_rotation_model); -- 90

	
	-- Sets the grid of the module.
	-- Sets the grid in the database and on the canvas:
	procedure set_grid (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		grid			: in pac_grid.type_grid;
		log_threshold	: in type_log_level);		


	-- Sets the grid of the module.
	-- Sets the grid in the database and on the canvas:
	procedure set_grid (
		module_cursor	: in pac_generic_modules.cursor;
		grid			: in pac_grid.type_grid;
		log_threshold	: in type_log_level);


	-- Returns the grid settings of the module:
	function get_grid (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
		return pac_grid.type_grid;

	
	
	procedure delete_device (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		log_threshold	: in type_log_level);

	
	-- Returns the sheet/x/y position of the given device and port.
	function position (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC34
		port_name		: in et_symbols.pac_port_name.bounded_string; -- CE
		log_threshold	: in type_log_level)
		return et_coordinates_2.type_position;

	
	-- Returns the sheet/x/y position of the given submodule port.
	function position (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		submod_name		: in pac_module_instance_name.bounded_string; -- MOT_DRV_3
		port_name		: in pac_net_name.bounded_string; -- RESET
		log_threshold	: in type_log_level)
		return et_coordinates_2.type_position;

	
	-- Returns the sheet/x/y position of the given netchanger port.
	function position (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in et_submodules.type_netchanger_id; -- 1,2,3,...
		port			: in et_submodules.type_netchanger_port_name; -- SLAVE/MASTER
		log_threshold	: in type_log_level)
		return et_coordinates_2.type_position;

	
	-- Moves the name placeholder of the given unit.
	procedure move_unit_placeholder (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model; -- x/y
		meaning			: in type_placeholder_meaning; -- name, value, purpose
		log_threshold	: in type_log_level);

	
	-- Returns the default positions of placeholders and texts of a unit
	-- as they are defined in the symbol model.
	function default_text_positions (
		device_cursor	: in pac_devices_sch.cursor;
		unit_name		: in pac_unit_name.bounded_string)
		return et_symbols.type_default_text_positions;

	
	-- Rotates the given unit placeholder about its origin.
	-- The rotation is absolute.										  
	procedure rotate_unit_placeholder (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		rotation		: in et_text.type_rotation_documentation; -- absolute ! -- 90
		meaning			: in type_placeholder_meaning; -- name, value, purpose		
		log_threshold	: in type_log_level);

	
	-- Returns a cursor to the requested net in the given module. If the net could
	-- not be found, returns no_element.
	function locate_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string)
		return pac_nets.cursor;


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

	
	
	-- CS move to et_schematic ?
	type type_drag is record
		before		: type_vector_model;
		after		: type_vector_model;
	end record;

	-- CS move to et_schematic ?	
	package type_drags_of_ports is new ada.containers.ordered_maps (
		key_type		=> et_symbols.pac_port_name.bounded_string,
		"<"				=> et_symbols.pac_port_name."<",
		element_type	=> type_drag);

	
	
	-- Returns lists of device, netchanger and submodule ports at the given place.
	function ports_at_place (
		module_cursor	: in pac_generic_modules.cursor;
		place			: in et_coordinates_2.type_position;
		log_threshold	: in type_log_level)		
		return type_ports;	
	

	-- Returns true if at given place a net segment starts or ends.
	function net_segment_at_place (
		module_cursor	: in pac_generic_modules.cursor;
		place			: in et_coordinates_2.type_position)
		return boolean;
	

	procedure dragging_not_possible (
		port 		: in string;
		position	: in et_coordinates_2.type_position);

	
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
	function exists (
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

	
	-- Locates the given unit of the given device in the 
	-- given module and returns the cursor to the unit.
	-- If the unit does not exist, returns no_element.
	-- Raises exception if device does not exist.
	function locate_unit (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name; -- R2
		unit	: in pac_unit_name.bounded_string)
		return et_schematic.pac_units.cursor;
	

	-- Returns true if the unit of the given device in the 
	-- given module has been deployed somewhere.
	-- If the unit has not been deployed yet, returns false.
	-- Raises exception if device does not exist.
	function deployed (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name; -- R2
		unit	: in pac_unit_name.bounded_string)
		return boolean;

	
	-- Locates the given device in the given module and returns
	-- the name of the device model (like 7400.dev).
	-- Raises constraint error if the device does not exist.
	function device_model_name (
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
		device			: in et_devices.type_device_name; -- R2
		variant			: in pac_package_variant_name.bounded_string; -- N, D
		log_threshold	: in type_log_level);
	
	-- Locates the given device in the given module and returns
	-- the cursor to the device model.
	-- Raises constraint error if the device does not exist.
	function device_model_cursor (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- R2
		return pac_devices_lib.cursor;
	
	function exists_device_port (
	-- Returns true if given device with the given port exists in module indicated by module_cursor.
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		device_name		: in type_device_name; -- IC45
		port_name		: in et_symbols.pac_port_name.bounded_string) -- CE
		return boolean;
	
	function exists_device_unit_port (
	-- Returns true if given device exists in module indicated by module_cursor.
	-- The unit and port names are optionally.
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string := to_unit_name (""); -- A
		port_name		: in et_symbols.pac_port_name.bounded_string := et_symbols.to_port_name ("")) -- CE		
		return boolean;						

	function exists_submodule_port (
	-- Returns true if given submodule with the given port exists in module indicated by module_cursor.
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		submod_instance : in pac_module_instance_name.bounded_string; -- MOT_DRV_3
		port_name		: in pac_net_name.bounded_string) -- RESET
		return boolean;

	function exists_netchanger (
	-- Returns true if given netchanger exists in module indicated by module_cursor.
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		index			: in et_submodules.type_netchanger_id) -- 1, 2, 3, ...
		return boolean;

	function next_device_name (
	-- Returns for the given device prefix the next available device name in the module.
	-- Example: prefix is C. If there are C1, C12, C1034 and C1035 the return will be C2.
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
		destination		: in et_coordinates_2.type_position; -- sheet/x/y/rotation
		log_threshold	: in type_log_level);

	-- CS procedure add_device with explicit device name like R12

	-- CS procedure add_device that takes module cursor and model cursor
	
	-- Copies the given device. Places the first unit of the device (according to add level)
	-- at the given destination in the schematic.
	procedure copy_device (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		destination		: in et_coordinates_2.type_position; -- sheet/x/y
		log_threshold	: in type_log_level);

	
	-- Returns the names of available units of the given device in the 
	-- given generic module. "Available" means the unit exists and is
	-- not already placed somewhere in the schematic:
	function available_units (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC1
		log_threshold	: in type_log_level)
		return et_devices.pac_unit_names.list;

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
	function units_on_sheet (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC1
		sheet			: in type_sheet;
		log_threshold	: in type_log_level)
		return et_devices.pac_unit_names.list;

	-- Returns the position (x/y/sheet) of the given unit.
	-- Raises constraint error if device or unit does not exist.
	function position (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name; -- R2
		unit	: in pac_unit_name.bounded_string)
		return et_coordinates_2.type_position;

	-- Returns the position (x/y/sheet) of the given unit.
	-- Raises constraint error if device or unit does not exist.
	function position (
		device	: in pac_devices_sch.cursor; -- R2
		unit	: in et_schematic.pac_units.cursor)
		return et_coordinates_2.type_position;

	
	-- Returns the position (x/y) of the given placeholder.
	-- Raises constraint error if device or unit does not exist.
	function position ( -- CS rename to get_position
		device		: in pac_devices_sch.cursor; -- R2
		unit		: in et_schematic.pac_units.cursor;
		category	: in type_placeholder_meaning)
		return type_vector_model;

	
	-- Returns the sheet number of the given unit.
	-- Raises constraint error if device or unit does not exist.
	function sheet (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name; -- R2
		unit	: in pac_unit_name.bounded_string)
		return type_sheet;

	
	procedure invoke_unit (
	-- Invokes a unit of a device into the schematic.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC1
		unit_name		: in pac_unit_name.bounded_string; -- A, B, IO_BANK_2
		destination		: in et_coordinates_2.type_position; -- sheet/x/y/rotation
		log_threshold	: in type_log_level);

	-- CS procedure invoke_unit that takes module cursor and model cursor

	procedure add_netchanger (
	-- Adds a netchanger to the schematic.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		place			: in et_coordinates_2.type_position; -- sheet/x/y
		log_threshold	: in type_log_level);

	
	-- Deletes a netchanger.
	procedure delete_netchanger (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in et_submodules.type_netchanger_id; -- 1,2,3,...
		log_threshold	: in type_log_level);

	
	-- Moves the given netchanger. Disconnects the netchanger from
	-- start or end points of net segments.
	procedure move_netchanger (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in et_submodules.type_netchanger_id; -- 1,2,3,...
		coordinates		: in type_coordinates; -- relative/absolute
		sheet			: in type_sheet_relative; -- -3/0/2
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level);

	
	-- Drags the given netchanger within the schematic.
	-- Already existing connections with net segments are kept.
	-- Net segment positions are modified.
	-- This operation applies to a single sheet. Dragging from one sheet
	-- to another is not possible.
	procedure drag_netchanger (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in et_submodules.type_netchanger_id; -- 1,2,3,...
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level);

	
	-- Rotates the given netchanger. Disconnects it from
	-- start or end points of net segments.
	procedure rotate_netchanger (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in et_submodules.type_netchanger_id; -- 1,2,3,...
		coordinates		: in type_coordinates; -- relative/absolute
		rotation		: in et_coordinates_2.type_rotation_model; -- 90
		log_threshold	: in type_log_level);

	
	-- Adds a submodule instance to the schematic.
	procedure add_submodule (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		file			: in et_submodules.pac_submodule_path.bounded_string; -- the file name of the submodule like templates/oscillator.mod
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		position		: in et_coordinates_2.type_position; -- sheet, lower left corner x/y 
		size			: in et_submodules.type_submodule_size; -- the size of the box in x and y
		log_threshold	: in type_log_level);

	
	-- Adds a port to a submodule instance.
	procedure add_port (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		port_name		: in pac_net_name.bounded_string; -- clk_out
		position		: in type_vector_model; -- x/y along the edge of the box

		direction		: in et_submodules.type_netchanger_port_name; -- master/slave. 
		-- NOTE: has nothing to do with direction of energy flow. It is relevant when 
		-- a netlist is exported. See specification et_submodules.type_submodule_port.
		
		log_threshold	: in type_log_level);

	
	-- Deletes a port of a submodule instance (the box in the parent sheet).
	procedure delete_port (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		port_name		: in pac_net_name.bounded_string; -- clk_out
		log_threshold	: in type_log_level);

	
	-- Moves the given submodule port. Disconnects the port from
	-- start or end points of net segments BEFORE the move. 
	-- Connects submodule port with segment end or start points AFTER the move.
	procedure move_port (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string; -- OSC
		port_name		: in pac_net_name.bounded_string; -- clock_output
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level);

	
	-- Drags the given submodule port along the edge of the box.
	-- Already existing connections with net segments are kept.
	-- Net segment positions are modified.
	-- This operation applies to a single sheet. Dragging from one sheet
	-- to another is not possible.
	procedure drag_port (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string; -- OSC
		port_name		: in pac_net_name.bounded_string; -- clock_output
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level);

	
	-- Removes a submodule instance from the schematic.
	procedure delete_submodule (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		log_threshold	: in type_log_level);

	
	-- Moves the given submodule instance (the box). Disconnects the ports from
	-- start or end points of net segments BEFORE the move. 
	-- Connects submodule ports with segment end or start points AFTER the move.
	procedure move_submodule (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		coordinates		: in type_coordinates; -- relative/absolute
		sheet			: in type_sheet_relative; -- -3/0/2
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level);

	
	-- Drags the given submodule instance (the box) within the schematic.
	-- Already existing connections with net segments are kept.
	-- Net segment positions are modified.
	-- This operation applies to a single sheet. Dragging from one sheet
	-- to another is not possible.
	procedure drag_submodule (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level);

	
	-- Copies a submodule instance.
	procedure copy_submodule (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance_origin	: in pac_module_instance_name.bounded_string; -- OSC1
		instance_new	: in pac_module_instance_name.bounded_string; -- CLOCK_GENERATOR
		destination		: in et_coordinates_2.type_position; -- sheet/x/y
		log_threshold	: in type_log_level);

	
	-- Renames a submodule instance.
	procedure rename_submodule (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance_old	: in pac_module_instance_name.bounded_string; -- OSC1
		instance_new	: in pac_module_instance_name.bounded_string; -- CLOCK_GENERATOR
		log_threshold	: in type_log_level);

	
	-- Sets the file name of a submodule instance.
	procedure set_submodule_file (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		file			: in et_submodules.pac_submodule_path.bounded_string; -- the file name of the submodule like templates/oscillator.mod
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		log_threshold	: in type_log_level);

	
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

	
	-- Sets the assembly variant of a submodule instance. An already existing submodule
	-- will be overwritten without warning.
	procedure mount_submodule (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		variant_parent	: in pac_assembly_variant_name.bounded_string; -- low_cost								  
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		variant_submod	: in pac_assembly_variant_name.bounded_string; -- fixed_frequency
		log_threshold	: in type_log_level);

	
	-- Removes the assembly variant of a submodule. This results in all devices
	-- of the submodule being mounted.
	procedure remove_submodule (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		variant_parent	: in pac_assembly_variant_name.bounded_string; -- low_cost								   
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		log_threshold	: in type_log_level);

	
	function sort_by_coordinates_2 (
		module_cursor 	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
		return et_numbering.pac_devices.map;

	
	-- Returns true if no unit sits on top of another.
	function unit_positions_valid (
		module_cursor 	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
		return boolean;

	
	-- Renumbers devices according to the sheet number.
	procedure renumber_devices (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		step_width		: in type_name_index;
		log_threshold	: in type_log_level);

	
	-- Calculates the device index ranges of the given top module and all its submodules.
	-- Assigns the device names offset of the instantiated submodules.
	-- Assumes that all devices of the modules are mounted -> assembly variants ignored.
	procedure autoset_device_name_offsets (
		module_name		: in pac_module_name.bounded_string; -- the top module like motor_driver (without extension *.mod)
		log_threshold	: in type_log_level);

	
	-- Dumps submodule names, instances and device name offsets:
	procedure dump_tree (
		module_name		: in pac_module_name.bounded_string;
		log_threshold	: in type_log_level);

	
	-- Re(builds) the submodule tree of the given parent module.
	procedure build_submodules_tree (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		log_threshold	: in type_log_level);

	
	-- Adds the offset to the device index of the given device_name.
	procedure apply_offset (
		device_name		: in out type_device_name; -- IC3
		offset			: in type_name_index; -- 100
		log_threshold	: in type_log_level);

	
	-- Generates the BOM files of all assembly variants from the given top module.
	-- The files are named after the module name and the variant name:
	procedure make_boms (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		log_threshold	: in type_log_level);

	
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
		port_name		: in et_symbols.pac_port_name.bounded_string) -- CE
		return type_port_properties_access;
	
	
	procedure check_integrity (
	-- Performs an in depth check on the schematic of the given module.
	-- Tests:
	-- 1. for device/submodule/netchanger ports that do not have a same named device/submodule/netchanger.
	-- 2. for device/submodule/netchanger ports that occur more than once.
	-- 3. CS: for net junctions sitting on top of each other
	-- 4. CS: for device/submodule/netchanger port that do not have a visual connection to the net
	-- 5. CS: for overlapping net segments
	-- 6. CS: unconnected ports of R, C, L (category depended)
	-- 6.1 CS: unconnected inputs
	-- 7. CS: devices with empty values
	-- 8. CS: interactive devices with empty purpose
	-- 9. CS: check partcode (conventions.validate_partcode)								  
	-- 10. units sitting on to of each other (same origin position)
	-- 11. CS: warning (or error ?) if any ports sit on top of each other. This would make the movable_tests obsolete.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		log_threshold	: in type_log_level);

	
	-- The result of a unit query is of this type:
	type type_unit_query (exists : boolean := false) is record
		case exists is
			when true => position : et_coordinates_2.type_position; -- x/y, rotation, sheet
			when false => null;
		end case;
	end record;

	
	-- Returns the result of a unit query in human readable form.
	-- If the unit_name is empty (""), then the result does not contain
	-- any reference to a unit. This is useful when a device has only one unit.
	function to_string (
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- C
		query_result	: in type_unit_query)
		return string;
	
	
	-- Returns the position of given unit. If the unit_name is emtpty ("")
	-- then the position of the first unit is returned.
	-- This is useful when a device has only one unit.
	-- If the given device or unit does not exist, then the return is false.
	function unit_position (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string) -- C
-- 		port_name		: in et_symbols.pac_port_name.bounded_string) -- CE
		return type_unit_query;

	-- CS type_port_query

	-- CS function port_position 


	
end et_schematic_ops;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
