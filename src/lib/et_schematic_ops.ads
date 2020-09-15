------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         SCHEMATIC OPERATIONS                             --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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

with et_general;				use et_general;
with et_geometry;				use et_geometry;
with et_coordinates;			use et_coordinates;
use et_coordinates.pac_geometry_sch;

with et_string_processing;		use et_string_processing;
with et_schematic;				use et_schematic;
with et_project;				use et_project;
with et_project.modules;		use et_project.modules;
with et_text;
with et_submodules;
with et_assembly_variants;
with et_numbering;
with et_material;
with et_netlists;
with et_symbols;
with et_devices;				use et_devices;
with et_conventions;

package et_schematic_ops is
	
	procedure device_not_found (name : in type_name);
	procedure device_already_exists (name : in type_name);
	procedure relative_rotation_invalid;
	procedure netchanger_not_found (index : in et_submodules.type_netchanger_id);
	procedure submodule_not_found (name : in et_general.type_module_instance_name.bounded_string);	
	procedure net_not_found (name : in et_general.type_net_name.bounded_string);
	procedure assembly_variant_not_found (variant : in et_general.type_variant_name.bounded_string);

	procedure unit_not_found (name : in type_unit_name.bounded_string);
	
	-- Writes the positions of the device units in the log file.
	procedure log_unit_positions (
		positions 		: in type_unit_positions.map;
		log_threshold	: in type_log_level);

	-- Returns a map of ports of the given device and unit.
	-- The coordinates of the ports are default xy-positions relative
	-- to the center of the unit.
	function ports_of_unit (
		device_cursor	: in et_schematic.type_devices.cursor;
		unit_name		: in type_unit_name.bounded_string)
		return et_symbols.type_ports.map;

	-- Deletes ports of the given device in nets.
	procedure delete_ports (
		module			: in pac_generic_modules.cursor;		-- the module
		device			: in type_name;			-- the device
		ports			: in et_symbols.type_ports.map := et_symbols.type_ports.empty_map; -- the ports (if empty, all ports of the device will be deleted)
		sheets			: in type_unit_positions.map;	-- the sheet numbers where the units can be found. CS implementation required
		log_threshold	: in type_log_level);

	-- Moves the given unit ports by given offset.
	procedure move_ports ( -- CS move to et_symbols ?
		ports	: in out et_symbols.type_ports.map; -- the portlist
		offset	: in et_coordinates.type_position); -- the offset (only x/y matters)

	-- Inserts the given device ports in the net segments.
	-- If a port lands on either the start or end point of a segment, it will
	-- be regarded as "connected" with the segment.
	-- If a ports lands between start or end point of a segment, nothing happens
	-- because the docking to net segments is possible on segment ends/starts only.
	-- CS: Automatic splitting the segment into two and placing a junction is not supported
	-- jet and probably not a good idea.
	procedure insert_ports (
		module			: in pac_generic_modules.cursor;		-- the module
		device			: in type_name;					-- the device
		unit			: in type_unit_name.bounded_string;	-- the unit name like A, C, PWR
		ports			: in et_symbols.type_ports.map; -- the ports to be inserted
		sheet			: in type_sheet;				-- the sheet to look at
		log_threshold	: in type_log_level);

	-- Rotates the given unit ports by given angle around the origin.
	procedure rotate_ports ( -- CS move to et_symbols ?
		ports	: in out et_symbols.type_ports.map; -- the portlist
		angle	: in et_coordinates.type_rotation); -- 90

	
	procedure set_grid (
	-- Sets the grid of the module.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		grid			: in type_grid;
		log_threshold	: in type_log_level);		
	
	procedure delete_device (
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_name; -- IC45
		log_threshold	: in type_log_level);

	-- Returns the sheet/x/y position of the given device and port.
	function position (
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_name; -- IC34
		port_name		: in et_symbols.type_port_name.bounded_string; -- CE
		log_threshold	: in type_log_level)
		return et_coordinates.type_position;

	-- Returns the sheet/x/y position of the given submodule port.
	function position (
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		submod_name		: in et_general.type_module_instance_name.bounded_string; -- MOT_DRV_3
		port_name		: in type_net_name.bounded_string; -- RESET
		log_threshold	: in type_log_level)
		return et_coordinates.type_position;

	-- Returns the sheet/x/y position of the given netchanger port.
	function position (
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in et_submodules.type_netchanger_id; -- 1,2,3,...
		port			: in et_submodules.type_netchanger_port_name; -- SLAVE/MASTER
		log_threshold	: in type_log_level)
		return et_coordinates.type_position;


	
-- 	procedure delete_unit (
-- 	-- Deletes a unit of a device. 
-- 	-- In case the last unit has been deleted, then the device is 
-- 	-- deleted entirely from module.devices.
-- 		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
-- 		device_name		: in type_name; -- IC45
-- 		unit_name		: in type_unit_name.bounded_string; -- A
-- 		log_threshold	: in type_log_level);
	
-- 	type type_coordinates is (RELATIVE, ABSOLUTE);
-- 
-- 	function to_string (coordinates : in type_coordinates) return string;
-- 	function to_coordinates (coordinates : in string) return type_coordinates;

-- 	procedure move_unit (
-- 	-- Moves the given unit.
-- 		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
-- 		device_name		: in type_name; -- IC45
-- 		unit_name		: in type_unit_name.bounded_string; -- A
-- 		coordinates		: in type_coordinates; -- relative/absolute
-- 		sheet			: in type_sheet_relative; -- -3/0/2
-- 		point			: in type_point; -- x/y
-- 		log_threshold	: in type_log_level);

	procedure move_unit_placeholder (
	-- Moves the name placeholder of the given unit.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_name; -- IC45
		unit_name		: in type_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_point; -- x/y
		meaning			: in et_symbols.type_placeholder_meaning; -- name, value, purpose
		log_threshold	: in type_log_level);

	function default_text_positions (
	-- Returns the default positions of placeholders and texts of a unit
	-- as they are defined in the symbol model.
		device_cursor	: in et_schematic.type_devices.cursor;
		unit_name		: in type_unit_name.bounded_string)
		return et_symbols.type_default_text_positions;
	
-- 	procedure rotate_unit (
-- 	-- Rotates the given unit. Disconnects the unit from
-- 	-- start or end points of net segments.
-- 	-- Rotates the placeholders around the unit.
-- 		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
-- 		device_name		: in type_name; -- IC45
-- 		unit_name		: in type_unit_name.bounded_string; -- A
-- 		coordinates		: in type_coordinates; -- relative/absolute		
-- 		rotation		: in et_coordinates.type_rotation; -- 90
-- 		log_threshold	: in type_log_level);

	procedure rotate_unit_placeholder (
	-- Rotates the given unit placeholder around its origin.
	-- The rotation is absolute.										  
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_name; -- IC45
		unit_name		: in type_unit_name.bounded_string; -- A
		rotation		: in et_text.type_rotation_documentation; -- absolute ! -- 90
		meaning			: in et_symbols.type_placeholder_meaning; -- name, value, purpose		
		log_threshold	: in type_log_level);

	-- CS move to et_schematic ?
	type type_drag is record
		before		: type_point;
		after		: type_point;
	end record;

	-- CS move to et_schematic ?	
	package type_drags_of_ports is new ada.containers.ordered_maps (
		key_type		=> et_symbols.type_port_name.bounded_string,
		"<"				=> et_symbols.type_port_name."<",
		element_type	=> type_drag);

	
	
	-- If ports at a certain position in a schematic are inquired this type is required:
	type type_ports is record
		devices		: type_ports_device.set;
		submodules	: type_ports_submodule.set;
		netchangers	: et_netlists.type_ports_netchanger.set;
	end record;

	function ports_at_place (
	-- Returns lists of device, netchanger and submodule ports at the given place.
		module_name		: in type_module_name.bounded_string;
		place			: in et_coordinates.type_position;
		log_threshold	: in type_log_level)		
		return type_ports;	

	
	procedure drag_unit ( -- CS move to et_schematic_ops.nets
	-- Drags the given unit within the schematic.
	-- Already existing connections with net segments are kept.
	-- Net segment positions are modified.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_name; -- IC45
		unit_name		: in type_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_point; -- x/y -- CS rename to destination
		log_threshold	: in type_log_level);

	procedure rename_device (
	-- Renames the given device.
		module_name			: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name_before	: in type_name; -- IC1
		device_name_after	: in type_name; -- IC23
		log_threshold		: in type_log_level);

	procedure set_value (
	-- Sets the value of a device.
		module_name			: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name			: in type_name; -- R2
		value				: in type_value.bounded_string; -- 470R
		log_threshold		: in type_log_level);

	procedure set_purpose (
	-- Sets the purpose of a device.
		module_name			: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name			: in type_name; -- R2
		purpose				: in type_purpose.bounded_string; -- brightness_control
		log_threshold		: in type_log_level);

	procedure set_partcode (
	-- Sets the partcode of a device.
		module_name			: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name			: in type_name; -- R2
		partcode			: in et_material.type_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
		log_threshold		: in type_log_level);
	
	function exists_device_port (
	-- Returns true if given device with the given port exists in module indicated by module_cursor.
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		device_name		: in type_name; -- IC45
		port_name		: in et_symbols.type_port_name.bounded_string) -- CE
		return boolean;
	
	function exists_device_unit_port (
	-- Returns true if given device exists in module indicated by module_cursor.
	-- The unit and port names are optionally.
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		device_name		: in type_name; -- IC45
		unit_name		: in type_unit_name.bounded_string := to_name (""); -- A
		port_name		: in et_symbols.type_port_name.bounded_string := et_symbols.to_port_name ("")) -- CE		
		return boolean;						

	function exists_submodule_port (
	-- Returns true if given submodule with the given port exists in module indicated by module_cursor.
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		submod_instance : in et_general.type_module_instance_name.bounded_string; -- MOT_DRV_3
		port_name		: in et_general.type_net_name.bounded_string) -- RESET
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
		prefix			: in type_prefix.bounded_string; -- C
		category		: in type_device_category := ELECTRICAL)
		return type_name; -- C2
	
	procedure add_device (
	-- Adds a device to the schematic. The unit is determined by the unit add levels.
	-- If the given variant is empty (zero length) the the device is assumed to be virtual.							 
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_model	: in type_device_model_file.bounded_string; -- ../libraries/devices/logic_ttl/7400.dev
		variant			: in et_devices.type_variant_name.bounded_string; -- N, D, S_0805
		destination		: in et_coordinates.type_position; -- sheet/x/y/rotation
		log_threshold	: in type_log_level);

	-- CS procedure add_device with explicit device name like R12
	
	procedure copy_device (
	-- Copies the given device. Places the first unit of the device (according to add level)
	-- at the given destination in the schematic.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_name; -- IC45
		destination		: in et_coordinates.type_position; -- sheet/x/y
		log_threshold	: in type_log_level);
	
	procedure invoke_unit (
	-- Invokes a unit of a device into the schematic.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_name; -- IC1
		unit_name		: in type_unit_name.bounded_string; -- A, B, IO_BANK_2
		destination		: in et_coordinates.type_position; -- sheet/x/y
		log_threshold	: in type_log_level);

	procedure add_netchanger (
	-- Adds a netchanger to the schematic.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		place			: in et_coordinates.type_position; -- sheet/x/y
		log_threshold	: in type_log_level);

	procedure delete_netchanger (
	-- Deletes a netchanger.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in et_submodules.type_netchanger_id; -- 1,2,3,...
		log_threshold	: in type_log_level);

	procedure move_netchanger (
	-- Moves the given netchanger. Disconnects the netchanger from
	-- start or end points of net segments.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in et_submodules.type_netchanger_id; -- 1,2,3,...
		coordinates		: in type_coordinates; -- relative/absolute
		sheet			: in type_sheet_relative; -- -3/0/2
		point			: in type_point; -- x/y
		log_threshold	: in type_log_level);

	procedure drag_netchanger (
	-- Drags the given netchanger within the schematic.
	-- Already existing connections with net segments are kept.
	-- Net segment positions are modified.
	-- This operation applies to a single sheet. Dragging from one sheet
	-- to another is not possible.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in et_submodules.type_netchanger_id; -- 1,2,3,...
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_point; -- x/y
		log_threshold	: in type_log_level);

	procedure rotate_netchanger (
	-- Rotates the given netchanger. Disconnects it from
	-- start or end points of net segments.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in et_submodules.type_netchanger_id; -- 1,2,3,...
		coordinates		: in type_coordinates; -- relative/absolute
		rotation		: in et_coordinates.type_rotation; -- 90
		log_threshold	: in type_log_level);
	
	procedure add_submodule (
	-- Adds a submodule instance to the schematic.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		file			: in et_submodules.type_submodule_path.bounded_string; -- the file name of the submodule like templates/oscillator.mod
		instance		: in et_general.type_module_instance_name.bounded_string; -- OSC1
		position		: in et_coordinates.type_position; -- sheet, lower left corner x/y 
		size			: in et_submodules.type_submodule_size; -- the size of the box in x and y
		log_threshold	: in type_log_level);
	
	procedure add_port (
	-- Adds a port to a submodule instance.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in et_general.type_module_instance_name.bounded_string; -- OSC1
		port_name		: in et_general.type_net_name.bounded_string; -- clk_out
		position		: in type_point; -- x/y along the edge of the box

		direction		: in et_submodules.type_netchanger_port_name; -- master/slave. 
		-- NOTE: has nothing to do with direction of energy flow. It is relevant when 
		-- a netlist is exported. See specification et_submodules.type_submodule_port.
		
		log_threshold	: in type_log_level);

	procedure delete_port (
	-- Deletes a port of a submodule instance (the box in the parent sheet).
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in et_general.type_module_instance_name.bounded_string; -- OSC1
		port_name		: in et_general.type_net_name.bounded_string; -- clk_out
		log_threshold	: in type_log_level);

	procedure move_port (
	-- Moves the given submodule port. Disconnects the port from
	-- start or end points of net segments BEFORE the move. 
	-- Connects submodule port with segment end or start points AFTER the move.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in et_general.type_module_instance_name.bounded_string; -- OSC
		port_name		: in et_general.type_net_name.bounded_string; -- clock_output
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_point; -- x/y
		log_threshold	: in type_log_level);

	procedure drag_port (
	-- Drags the given submodule port along the edge of the box.
	-- Already existing connections with net segments are kept.
	-- Net segment positions are modified.
	-- This operation applies to a single sheet. Dragging from one sheet
	-- to another is not possible.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in et_general.type_module_instance_name.bounded_string; -- OSC
		port_name		: in et_general.type_net_name.bounded_string; -- clock_output
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_point; -- x/y
		log_threshold	: in type_log_level);

	procedure delete_submodule (
	-- Removes a submodule instance from the schematic.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance		: in et_general.type_module_instance_name.bounded_string; -- OSC1
		log_threshold	: in type_log_level);

	procedure move_submodule (
	-- Moves the given submodule instance (the box). Disconnects the ports from
	-- start or end points of net segments BEFORE the move. 
	-- Connects submodule ports with segment end or start points AFTER the move.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance		: in et_general.type_module_instance_name.bounded_string; -- OSC1
		coordinates		: in type_coordinates; -- relative/absolute
		sheet			: in type_sheet_relative; -- -3/0/2
		point			: in type_point; -- x/y
		log_threshold	: in type_log_level);

	procedure drag_submodule (
	-- Drags the given submodule instance (the box) within the schematic.
	-- Already existing connections with net segments are kept.
	-- Net segment positions are modified.
	-- This operation applies to a single sheet. Dragging from one sheet
	-- to another is not possible.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance		: in et_general.type_module_instance_name.bounded_string; -- OSC1
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_point; -- x/y
		log_threshold	: in type_log_level);

	procedure copy_submodule (
	-- Copies a submodule instance.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance_origin	: in et_general.type_module_instance_name.bounded_string; -- OSC1
		instance_new	: in et_general.type_module_instance_name.bounded_string; -- CLOCK_GENERATOR
		destination		: in et_coordinates.type_position; -- sheet/x/y
		log_threshold	: in type_log_level);

	procedure rename_submodule (
	-- Renames a submodule instance.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance_old	: in et_general.type_module_instance_name.bounded_string; -- OSC1
		instance_new	: in et_general.type_module_instance_name.bounded_string; -- CLOCK_GENERATOR
		log_threshold	: in type_log_level);
	
	procedure set_submodule_file (
	-- Sets the file name of a submodule instance.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		file			: in et_submodules.type_submodule_path.bounded_string; -- the file name of the submodule like templates/oscillator.mod
		instance		: in et_general.type_module_instance_name.bounded_string; -- OSC1
		log_threshold	: in type_log_level);

	procedure create_assembly_variant (
	-- Creates a new assembly variant.
		module_name		: in type_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in et_general.type_variant_name.bounded_string; -- low_cost
		log_threshold	: in type_log_level);

	procedure delete_assembly_variant (
	-- Deletes an assembly variant.
		module_name		: in type_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in et_general.type_variant_name.bounded_string; -- low_cost
		log_threshold	: in type_log_level);
	
	procedure describe_assembly_variant (
	-- Describes an assembly variant. Overwrites the previous description.
		module_name		: in type_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in et_general.type_variant_name.bounded_string; -- low_cost
		description		: in et_assembly_variants.type_description; -- "this is the low budget variant"
		log_threshold	: in type_log_level);

	procedure mount_device (
	-- Sets the value, partcode and (optionally the purpose) of a device in 
	-- An already existing device will be overwritten without warning.
		module_name		: in type_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in et_general.type_variant_name.bounded_string; -- low_cost
		device			: in type_name; -- R1
		value			: in type_value.bounded_string; -- 220R
		partcode		: in et_material.type_partcode.bounded_string; -- R_PAC_S_0805_VAL_220R
		purpose			: in type_purpose.bounded_string := type_purpose.to_bounded_string (""); -- set temperature
		log_threshold	: in type_log_level);

	procedure unmount_device (
	-- Sets the given device as not mounted in the given assembly variant.
		module_name		: in type_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in et_general.type_variant_name.bounded_string; -- low_cost
		device			: in type_name; -- R1
		log_threshold	: in type_log_level);

	procedure remove_device (
	-- Removes the given device from the given assembly variant.
		module_name		: in type_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in et_general.type_variant_name.bounded_string; -- low_cost
		device			: in type_name; -- R1
		log_threshold	: in type_log_level);

	procedure mount_submodule (
	-- Sets the assembly variant of a submodule instance. An already existing submodule
	-- will be overwritten without warning.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		variant_parent	: in et_general.type_variant_name.bounded_string; -- low_cost								  
		instance		: in et_general.type_module_instance_name.bounded_string; -- OSC1
		variant_submod	: in et_general.type_variant_name.bounded_string; -- fixed_frequency
		log_threshold	: in type_log_level);

	procedure remove_submodule (
	-- Removes the assembly variant of a submodule. This results in all devices
	-- of the submodule being mounted.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		variant_parent	: in et_general.type_variant_name.bounded_string; -- low_cost								   
		instance		: in et_general.type_module_instance_name.bounded_string; -- OSC1
		log_threshold	: in type_log_level);

	function sort_by_coordinates (
		module_cursor 	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
		return et_numbering.type_devices.map;

	function unit_positions_valid (
	-- Returns true if no unit sits on top of another.
		module_cursor 	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
		return boolean;
	
	procedure renumber_devices (
	-- Renumbers devices according to the sheet number.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		step_width		: in type_name_index;
		log_threshold	: in type_log_level);

	procedure autoset_device_name_offsets (
	-- Calculates the device index ranges of the given top module and all its submodules.
	-- Assigns the device names offset of the instantiated submodules.
	-- Assumes that all devices of the modules are mounted -> assembly variants ignored.
		module_name		: in type_module_name.bounded_string; -- the top module like motor_driver (without extension *.mod)
		log_threshold	: in type_log_level);

	procedure dump_tree (
	-- Dumps submodule names, instances and device name offsets:
		module_name		: in type_module_name.bounded_string;
		log_threshold	: in type_log_level);

	
	procedure build_submodules_tree (
	-- Re(builds) the submodule tree of the given parent module.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		log_threshold	: in type_log_level);
	
	procedure apply_offset (
	-- Adds the offset to the device index of the given device_name.
		device_name		: in out type_name; -- IC3
		offset			: in type_name_index; -- 100
		log_threshold	: in et_string_processing.type_log_level);
	
	procedure make_boms (
	-- Generates the BOM files of all assembly variants from the given top module.
	-- The files are named after the module name and the variant name.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		log_threshold	: in type_log_level);
	
	function port_properties (
	-- Returns properties of the given device port in module indicated by module_cursor.
	-- Properties are things like: terminal name, direction, sensitivity, power level, ...
	-- See et_libraries.type_port for detail.
	-- The device must exist in the module and must be real. Run intergrity check
	-- in case exception occurs here.
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		device_name		: in type_name; -- IC45
		unit_name		: in type_unit_name.bounded_string; -- A, B, IO_BANK_2
		port_name		: in et_symbols.type_port_name.bounded_string) -- CE
		return type_port_properties_access;
	
	procedure make_netlists (
	-- Generates the netlist files of all assembly variants from the given top module.
	-- The netlist files are named after the module name and the variant name.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		log_threshold	: in type_log_level);
	
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
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		log_threshold	: in type_log_level);

	-- The result of a unit query is of this type:
	type type_unit_query (exists : boolean := false) is record
		case exists is
			when true => position : et_coordinates.type_position; -- x/y, rotation, sheet
			when false => null;
		end case;
	end record;

	-- Returns the result of a unit query in human readable form.
	-- If the unit_name is empty (""), then the result does not contain
	-- any reference to a unit. This is useful when a device has only one unit.
	function to_string (
		device_name		: in type_name; -- IC45
		unit_name		: in type_unit_name.bounded_string; -- C
		query_result	: in type_unit_query)
		return string;
	
	function unit_position (
	-- Returns the position of given unit. If the unit_name is emtpty ("")
	-- then the position of the first unit is returned.
	-- This is useful when a device has only one unit.
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		device_name		: in type_name; -- IC45
		unit_name		: in type_unit_name.bounded_string) -- C
-- 		port_name		: in et_symbols.type_port_name.bounded_string) -- CE
		return type_unit_query;

	-- CS type_port_query

	-- CS function port_position 


	
end et_schematic_ops;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
