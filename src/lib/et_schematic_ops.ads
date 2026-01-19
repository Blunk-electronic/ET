------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         SCHEMATIC OPERATIONS                             --
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
with et_schematic_geometry;		use et_schematic_geometry;
with et_schematic_coordinates;	use et_schematic_coordinates;
use et_schematic_geometry.pac_geometry_2;

with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;
with et_nets;					use et_nets;
with et_net_junction;			use et_net_junction;
with et_net_ports;				use et_net_ports;
with et_net_segment;			use et_net_segment;
with et_net_labels;				use et_net_labels;
with et_project;				use et_project;

with et_generic_modules;		use et_generic_modules;
with et_module;					use et_module;
with et_module_board;			use et_module_board;

with et_text;
with et_netchangers;
with et_submodules;
with et_assembly_variants;		use et_assembly_variants;
with et_assembly_variant_name;	use et_assembly_variant_name;
with et_numbering;
with et_material;
with et_netlists;
with et_terminal_name;			use et_terminal_name;
with et_terminals;
with et_port_names;				use et_port_names;
with et_symbol_ports;			use et_symbol_ports;
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

with et_devices_electrical;			use et_devices_electrical;
with et_devices_electrical.units;	use et_devices_electrical.units;

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
		device		: in pac_devices_electrical.cursor;
		terminal	: in pac_terminal_name.bounded_string) -- H7, 1, 16
		return pac_nets.cursor;

	

	
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

	
	
	
	function sort_by_coordinates_2 (
		module_cursor 	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
		return et_numbering.pac_devices.map;

	
	

	
	-- Renumbers devices according to the sheet number.
	procedure renumber_devices (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		step_width		: in type_name_index;
		log_threshold	: in type_log_level);
	-- CS move to separate package


	
end et_schematic_ops;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
