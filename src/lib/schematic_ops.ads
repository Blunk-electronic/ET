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
with assembly_variants;

package schematic_ops is

	procedure delete_device (
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		log_threshold	: in type_log_level);

	procedure delete_unit (
	-- Deletes a unit of a device. 
	-- In case the last unit has been deleted, then the device is 
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

	-- CS move to et_schematic ?
	type type_drag is record
		before		: et_coordinates.type_point;
		after		: et_coordinates.type_point;
	end record;

	-- CS move to et_schematic ?	
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

	procedure copy_device (
	-- Copies the given device. Places the first unit of the device (according to add level)
	-- at the given destination in the schematic.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		destination		: in et_coordinates.type_coordinates; -- sheet/x/y
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

	procedure add_netchanger (
	-- Adds a netchanger to the schematic.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		place			: in et_coordinates.type_coordinates; -- sheet/x/y
		rotation		: in et_coordinates.type_rotation; -- 90				
		log_threshold	: in type_log_level);

	procedure delete_netchanger (
	-- Deletes a netchanger.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in submodules.type_netchanger_id; -- 1,2,3,...
		log_threshold	: in type_log_level);

	procedure move_netchanger (
	-- Moves the given netchanger. Disconnects the netchanger from
	-- start or end points of net segments.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in submodules.type_netchanger_id; -- 1,2,3,...
		coordinates		: in type_coordinates; -- relative/absolute
		sheet			: in type_sheet_relative; -- -3/0/2
		point			: in et_coordinates.type_point; -- x/y
		log_threshold	: in type_log_level);

	procedure drag_netchanger (
	-- Drags the given netchanger within the schematic.
	-- Already existing connections with net segments are kept.
	-- Net segment positions are modified.
	-- This operation applies to a single sheet. Dragging from one sheet
	-- to another is not possible.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in submodules.type_netchanger_id; -- 1,2,3,...
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in et_coordinates.type_point; -- x/y
		log_threshold	: in type_log_level);

	procedure rotate_netchanger (
	-- Rotates the given netchanger. Disconnects it from
	-- start or end points of net segments.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in submodules.type_netchanger_id; -- 1,2,3,...
		coordinates		: in type_coordinates; -- relative/absolute
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

	package type_net_names is new doubly_linked_lists (
		element_type	=> et_general.type_net_name.bounded_string,
		"="				=> et_general.type_net_name."="
		);
	
	procedure draw_net (
	-- Draws a segment of a net. If the start or end point of the new segment
	-- meets a port then the port will be connected with the segment.
	-- 1. If the segment is part of a new net, the net is created with a single segment
	--  specified by start_point and end_point. If the new segment collides with a foreign
	--  net, an error is raised.
	-- 2. If the net_name is a name of an already existing net, the given net segment (specified
	--  by start_point and end_point) will be added to the existing net.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in et_general.type_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		start_point		: in et_coordinates.type_coordinates; -- sheet/x/y
		end_point		: in et_coordinates.type_point; -- x/y
		log_threshold	: in type_log_level);

	procedure place_net_label (
	-- Places a label next to a segment at position.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		segment_position: in et_coordinates.type_coordinates; -- sheet/x/y
		label_position	: in et_coordinates.type_point; -- x/y
		rotation		: in et_coordinates.type_rotation; -- 0 / 90 degree
		appearance 		: in type_net_label_appearance; -- simple/tag label		
		direction		: in et_schematic.type_net_label_direction; -- INPUT, OUTPUT, PASSIVE, ...
		log_threshold	: in type_log_level);

	procedure delete_net_label (
	-- Deletes a label.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		position		: in et_coordinates.type_coordinates; -- sheet/x/y
		log_threshold	: in type_log_level);
	
	procedure add_submodule (
	-- Adds a submodule instance to the schematic.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		file			: in submodules.type_submodule_path.bounded_string; -- the file name of the submodule like templates/oscillator.mod
		instance		: in et_general.type_module_instance_name.bounded_string; -- OSC1
		position		: in et_coordinates.type_coordinates; -- sheet, lower left corner x/y 
		size			: in submodules.type_submodule_size; -- the size of the box in x and y
		log_threshold	: in type_log_level);
	
	procedure add_port (
	-- Adds a port to a submodule instance.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in et_general.type_module_instance_name.bounded_string; -- OSC1
		port_name		: in et_general.type_net_name.bounded_string; -- clk_out
		position		: in et_coordinates.type_point; -- x/y along the edge of the box
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
		point			: in et_coordinates.type_point; -- x/y
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
		point			: in et_coordinates.type_point; -- x/y
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
		point			: in et_coordinates.type_point; -- x/y
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
		point			: in et_coordinates.type_point; -- x/y
		log_threshold	: in type_log_level);

	procedure copy_submodule (
	-- Copies a submodule instance.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance_origin	: in et_general.type_module_instance_name.bounded_string; -- OSC1
		instance_new	: in et_general.type_module_instance_name.bounded_string; -- CLOCK_GENERATOR
		destination		: in et_coordinates.type_coordinates; -- sheet/x/y
		log_threshold	: in type_log_level);

	procedure set_submodule_file (
	-- Sets the file name of a submodule instance.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		file			: in submodules.type_submodule_path.bounded_string; -- the file name of the submodule like templates/oscillator.mod
		instance		: in et_general.type_module_instance_name.bounded_string; -- OSC1
		log_threshold	: in type_log_level);

	procedure create_assembly_variant (
	-- Creates a new assembly variant.
		module_name		: in type_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in assembly_variants.type_variant_name.bounded_string; -- low_cost
		log_threshold	: in type_log_level);

	procedure delete_assembly_variant (
	-- Deletes an assembly variant.
		module_name		: in type_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in assembly_variants.type_variant_name.bounded_string; -- low_cost
		log_threshold	: in type_log_level);
	
	procedure describe_assembly_variant (
	-- Describes an assembly variant. Overwrites the previous description.
		module_name		: in type_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in assembly_variants.type_variant_name.bounded_string; -- low_cost
		description		: in assembly_variants.type_description; -- "this is the low budget variant"
		log_threshold	: in type_log_level);

	procedure mount_device (
	-- Sets the value, partcode and (optionally the purpose) of a device in 
	-- the given assembly variant.
		module_name		: in type_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in assembly_variants.type_variant_name.bounded_string; -- low_cost
		device			: in type_device_name; -- R1
		value			: in type_value.bounded_string; -- 220R
		partcode		: in type_partcode.bounded_string; -- R_PAC_S_0805_VAL_220R
		purpose			: in type_device_purpose.bounded_string := to_purpose ("");
		log_threshold	: in type_log_level);

	procedure unmount_device (
	-- Sets the gvien device as not mounted in 
	-- the given assembly variant.
		module_name		: in type_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in assembly_variants.type_variant_name.bounded_string; -- low_cost
		device			: in type_device_name; -- R1
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
	-- 10. CS: units sitting on to of each other (same origin position)
	-- 11. CS: warning (or error ?) if any ports sit on top of each other. This would make the movable_tests obsolete.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		log_threshold	: in type_log_level);
								  
end schematic_ops;

-- Soli Deo Gloria
