------------------------------------------------------------------------------
--                                                                          --
--                     SYSTEM ET SCHEMATIC OPERATIONS                       --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

--   The two letters "CS" indicate a "construction side" where things are not
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

with et_general;
with et_coordinates;			use et_coordinates;
with et_libraries;				use et_libraries;
with et_string_processing;		use et_string_processing;
with et_schematic;				use et_schematic;
with et_project;				use et_project;
with submodules;

package schematic_ops is

	procedure delete_device (
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		log_threshold	: in type_log_level);

	procedure delete_unit (
	-- Deletes a unit of a device. 
	-- In case the last unit has been delete, then the device is 
	-- deleted entirely from module.devices.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in type_unit_name.bounded_string; -- A
		log_threshold	: in type_log_level);

	type type_coordinates is (RELATIVE, ABSOLUTE);

	function to_string (coordinates : in type_coordinates) return string;
	function to_coordinates (coordinates : in string) return type_coordinates;

	procedure move_unit (
	-- Moves the given unit.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in type_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		sheet			: in type_sheet_relative; -- -3/0/2
		point			: in et_coordinates.type_point; -- x/y
		log_threshold	: in type_log_level);

	procedure move_unit_placeholder (
	-- Moves the name placeholder of the given unit.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in type_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in et_coordinates.type_point; -- x/y
		meaning			: in et_libraries.type_text_meaning; -- name, value, purpose
		log_threshold	: in type_log_level);

	procedure rotate_unit (
	-- Rotates the given unit. Disconnects the unit from
	-- start or end points of net segments.
	-- Rotates the placeholders around the unit.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in type_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute		
		rotation		: in et_coordinates.type_rotation; -- 90
		log_threshold	: in type_log_level);

	procedure rotate_unit_placeholder (
	-- Rotates the given unit placeholder around its origin.
	-- The rotation is absolute.										  
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in type_unit_name.bounded_string; -- A
		rotation		: in et_coordinates.type_rotation_text; -- absolute ! -- 90
		meaning			: in et_libraries.type_text_meaning; -- name, value, purpose		
		log_threshold	: in type_log_level);
	
	type type_drag is record
		before		: et_coordinates.type_point;
		after		: et_coordinates.type_point;
	end record;

	package type_drags_of_ports is new ada.containers.ordered_maps (
		key_type		=> type_port_name.bounded_string,
		"<"				=> type_port_name."<",
		element_type	=> type_drag);
	
	procedure drag_unit (
	-- Drags the given unit within the schematic.
	-- Already existing connections with net segments are kept.
	-- Net segment positions are modified.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in type_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in et_coordinates.type_point; -- x/y
		log_threshold	: in type_log_level);

	procedure rename_device (
	-- Renames the given device.
		module_name			: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name_before	: in type_device_name; -- IC1
		device_name_after	: in type_device_name; -- IC23
		log_threshold		: in type_log_level);

	procedure set_value (
	-- Sets the value of a device.
		module_name			: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name			: in type_device_name; -- R2
		value				: in type_value.bounded_string; -- 470R
		log_threshold		: in type_log_level);

	procedure set_purpose (
	-- Sets the purpose of a device.
		module_name			: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name			: in type_device_name; -- R2
		purpose				: in type_device_purpose.bounded_string; -- brightness_control
		log_threshold		: in type_log_level);

	procedure set_partcode (
	-- Sets the partcode of a device.
		module_name			: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name			: in type_device_name; -- R2
		partcode			: in type_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
		log_threshold		: in type_log_level);
	
	function exists_device_port (
	-- Returns true if given device with the given port exists in module indicated by module_cursor.
		module_cursor	: in type_modules.cursor; -- motor_driver
		device_name		: in type_device_name; -- IC45
		port_name		: in type_port_name.bounded_string) -- CE
		return boolean;
	
	function exists_device_unit_port (
	-- Returns true if given device exists in module indicated by module_cursor.
	-- The unit and port names are optionally.
		module_cursor	: in type_modules.cursor; -- motor_driver
		device_name		: in type_device_name; -- IC45
		unit_name		: in type_unit_name.bounded_string := to_unit_name (""); -- A
		port_name		: in type_port_name.bounded_string := to_port_name ("")) -- CE		
		return boolean;						

	function exists_submodule_port (
	-- Returns true if given submodule with the given port exists in module indicated by module_cursor.
		module_cursor	: in type_modules.cursor; -- motor_driver
		submod_instance : in et_general.type_module_instance_name.bounded_string; -- MOT_DRV_3
		port_name		: in et_general.type_net_name.bounded_string) -- RESET
		return boolean;

	function exists_netchanger (
	-- Returns true if given netchanger exists in module indicated by module_cursor.
		module_cursor	: in type_modules.cursor; -- motor_driver
		index			: in submodules.type_netchanger_id) -- 1, 2, 3, ...
		return boolean;

	procedure place_junction (
	-- Places a net junction at the given position.
	-- If the junction is to be placed between start and end point of a segment, then the segment 
	-- is split in two new segments with the junction between them.
	-- If there is no net segment at the given position, no junction is placed and warning issued.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		place			: in et_coordinates.type_coordinates; -- sheet/x/y
		log_threshold	: in type_log_level);

	function next_device_name (
	-- Returns for the given device prefix the next available device name in the module.
	-- Example: prefix is C. If there are C1, C12, C1034 and C1035 the return will be C2.
		module_cursor	: in type_modules.cursor;
		prefix			: in et_libraries.type_device_name_prefix.bounded_string) -- C
		return et_libraries.type_device_name; -- C2
	
	procedure add_device (
	-- Adds a device to the schematic. The unit is determined by the unit add levels.
	-- If the given variant is empty (zero length) the the device is assumed to be virtual.							 
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_model	: in type_device_model_file.bounded_string; -- ../libraries/devices/logic_ttl/7400.dev
		variant			: in et_libraries.type_component_variant_name.bounded_string; -- N, D, S_0805
		place			: in et_coordinates.type_coordinates; -- sheet/x/y
		rotation		: in et_coordinates.type_rotation; -- 90		
		log_threshold	: in type_log_level);

	procedure invoke_unit (
	-- Invokes a unit of a device into the schematic.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC1
		unit_name		: in type_unit_name.bounded_string; -- A, B, IO_BANK_2
		place			: in et_coordinates.type_coordinates; -- sheet/x/y
		rotation		: in et_coordinates.type_rotation; -- 90		
		log_threshold	: in type_log_level);

	type type_net_scope is (
		STRAND,
		SHEET,
		EVERYWHERE
		);
	
	procedure rename_net (
	-- Renames a net. The scope determines whether to rename a certain strand,
	-- all strands on a certain sheet or on all sheets.
	-- CS If a particular strand on a sheet is to be renamed, the argument "place"
	-- must provide sheet and x/y start position of strand. In the future x/y can be
	-- any point on any segment of the strand.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name_before	: in et_general.type_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		net_name_after	: in et_general.type_net_name.bounded_string; -- RESET_N, MOTOR_ON_OFF_N	
		scope			: in type_net_scope; -- strand, sheet, everywhere
		place			: in et_coordinates.type_coordinates; -- sheet/x/y
		log_threshold	: in type_log_level);

	procedure delete_net (
	-- Deletes a net. The scope determines whether to delete a certain strand,
	-- all strands on a certain sheet or on all sheets.
	-- CS If a particular strand on a sheet is to be deleted, the argument "place"
	-- must provide sheet and x/y start position of strand. In the future x/y can be
	-- any point on any segment of the strand.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in et_general.type_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		scope			: in type_net_scope; -- strand, sheet, everywhere
		place			: in et_coordinates.type_coordinates; -- sheet/x/y
		log_threshold	: in type_log_level);

	procedure delete_segment (
	-- Deletes a segment of a net.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in et_general.type_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		place			: in et_coordinates.type_coordinates; -- sheet/x/y
		log_threshold	: in type_log_level);

	-- If ports at a certain position in a schematic are inquired this type is required:
	type type_ports is record
		devices		: type_ports_device.set;
		submodules	: type_ports_submodule.set;
		netchangers	: type_ports_netchanger.set;
	end record;

	function ports_at_place (
	-- Returns lists of device, netchanger and submodule ports at the given place.
		module_name		: in type_module_name.bounded_string;
		place			: in et_coordinates.type_coordinates;
		log_threshold	: in type_log_level)		
		return type_ports;	
	
	procedure drag_segment (
	-- Drags a segment of a net.
	-- Place adresses the segment within the schematic. 
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in et_general.type_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		place			: in et_coordinates.type_coordinates; -- sheet/x/y, this addresses the segment
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in et_coordinates.type_point; -- x/y, the new position 
		log_threshold	: in type_log_level);

	procedure draw_net (
	-- Draws a segment of a net.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in et_general.type_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		start_point		: in et_coordinates.type_coordinates; -- sheet/x/y
		end_point		: in et_coordinates.type_point; -- x/y
		log_threshold	: in type_log_level);
	
	procedure check_integrity (
	-- Performs an in depth check on the schematic of the given module.
	-- Tests:
	-- 1. for device/submodule/netchanger port that do not have a same named device/submodule/netchanger.
	-- 2. for device/submodule/netchanger port that occur more than once.
	-- 3. CS: for net junctions sitting on top of each other
	-- 4. CS: for device/submodule/netchanger port that do not have a visual connection to the net
	-- 5. CS: for overlapping net segments
	-- 6. CS: unconnected ports of R, C, L (category depended)
	-- 6.1 CS: unconnected inputs
	-- 7. CS: devices with empty values
	-- 8. CS: interactive devices with empty purpose
	-- 9. CS: check partcode (conventions.validate_partcode)								  
	-- 10. CS: units sitting on to of each other (same origin position)
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		log_threshold	: in type_log_level);
								  
end schematic_ops;

-- Soli Deo Gloria
