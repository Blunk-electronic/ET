------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             CONVENTIONS                                  --
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

--   For correct displaying set tab with in your edtior to 4.

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

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers;            use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.ordered_sets;

with ada.directories;			use ada.directories;

with et_schematic_geometry;
with et_schematic_coordinates;
-- with et_import;
with et_material;
with et_device_partcode;		use et_device_partcode;
with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;
with et_package_name;			use et_package_name;
with et_device_category;		use et_device_category;
with et_device_value;			use et_device_value;
with et_device_prefix;			use et_device_prefix;
with et_device_name;			use et_device_name;
with et_units_of_measurement;	use et_units_of_measurement;


package et_conventions is

	comment_mark : constant string := "#";


	-- conventions file section headers
	section_component_prefixes						: constant string (1..20)	:= "[COMPONENT_PREFIXES]";
	section_component_units							: constant string (1..22)	:= "[UNITS_OF_MEASUREMENT]";
	section_components_with_operator_interaction 	: constant string (1..31)	:= "[OPERATOR_INTERACTION_REQUIRED]";
	section_partcode_keywords						: constant string (1..20)	:= "[PART_CODE_KEYWORDS]";
	-- CS: section_connector_gnd_terminal					: constant string (1..24) := "[CONNECTOR_GND_TERMINAL]";
	--section_import_modules							: constant string (1..16)	:= "[IMPORT_MODULES]";
	--section_module_interconnections					: constant string (1..25)	:= "[MODULE_INTERCONNECTIONS]";
	section_text_sizes_schematic					: constant string (1..22)	:= "[TEXT_SIZES_SCHEMATIC]";

-- 	option_module_interconnections_comparator_off	: constant string (1..18)	:= "net_comparator_off";
-- 	option_module_interconnections_comparator_on	: constant string (1..17)	:= "net_comparator_on";
-- 	option_module_interconnections_warn_only		: constant string (1..9) 	:= "warn_only";	
	
-- 	-- A module to be imported has a name, an abbrevation, a CAD format and a certain 
-- 	-- number of instances.
-- 	-- Example: nucleo_core NCC kicad_v4 1
-- 	-- We collect these modules in a simple list because the order must be kept.
-- 	type type_import_module is record
-- 		name		: et_coordinates.type_submodule_name.bounded_string; -- MOTOR_DRIVER
-- 		abbrevation	: et_coordinates.type_submodule_abbrevation.bounded_string; -- MOT
-- 		format		: et_import.type_cad_format; -- KICAD_V4, EAGLE_V7
-- 		instances	: et_coordinates.type_submodule_instance; -- 4 
-- 	end record;
-- 
-- 	package type_import_modules is new doubly_linked_lists (
-- 		element_type => type_import_module);
-- 
-- 	-- Finally the container where the modules to be imported are stored is this:
-- 	import_modules : type_import_modules.list;


	-- Module interconnections are collected in a simple list:
	-- CS type type_connector_gender is (UNKNOWN, FEMALE, MALE);
	
-- 	type type_connector is record
-- 		-- CS: gender ?
-- 		abbrevation	: et_coordinates.type_submodule_abbrevation.bounded_string; -- MOT
-- 		instance	: et_coordinates.type_submodule_instance; -- 4 
-- 		purpose		: et_libraries.type_component_purpose.bounded_string; -- MOTOR_CTRL_IN
-- 	end record;

-- 	type type_net_comparator_on_off is (ON, OFF);
	
-- 	function to_string (net_comparator_on_off : in type_net_comparator_on_off) return string;
-- 	-- Returns the given net comparator status as string.
	
-- 	type type_net_comparator_warn_only is (ON, OFF);

-- 	function to_string (net_comparator_warn : in type_net_comparator_warn_only) return string;
-- 	-- Returns the given net comparator warning status as string.
	
-- 	type type_module_interconnection_options is record
-- 		comparator	: type_net_comparator_on_off := ON; -- net_comparator_off
-- 		warn_only 	: type_net_comparator_warn_only := OFF; -- warn_only
-- 	end record;
-- 	
-- 	type type_module_interconnection is record
-- 		peer_A	: type_connector;
-- 		peer_B	: type_connector;
-- 		options	: type_module_interconnection_options;
-- 	end record;
-- 
-- 	-- This is the container with the module interconnections:
-- 	package type_module_interconnections is new doubly_linked_lists (
-- 		element_type => type_module_interconnection);
-- 
-- 	module_interconnections : type_module_interconnections.list;


-- 	procedure validate_module_interconnection (connection : in type_module_interconnection);
-- 	-- checks if something like "NCC 1 MOTOR_CTRL_OUT_2 MOT 2 MOTOR_CTRL_IN" makes sense
-- 	-- in connection with entries in section import_modules

-- 	procedure validate_module_interconnections (log_threshold: in type_log_level);
-- 	-- Tests if module interconnections at net level make sense.
-- 	-- NOTE: call AFTER modules have been imported !


	
	subtype type_net_label_text_size is et_schematic_geometry.type_distance_model range 1.0 .. 5.0; -- unit is mm
	net_label_text_size_default : constant type_net_label_text_size := 1.3;
	-- CS: no longer required ?

	
	-- Converts a string to type_net_label_text_size.
	function to_net_label_text_size (text : in string) return type_net_label_text_size;
	-- CS: no longer required ?
	



	
	-- Device prefixes and their category are liked via this map:
	package pac_device_prefixes is new ordered_maps (
		key_type 		=> pac_device_prefix.bounded_string, -- IC
		element_type 	=> type_device_category, -- INTEGRATED_CIRCUIT
		"<" 			=> pac_device_prefix."<");

	-- After reading the conventions, we store the 
	-- allowed device prefixes for the design here:
	device_prefixes : pac_device_prefixes.map;


	

	-- Returns true if any component prefixes are specified via conventions file.
	function component_prefixes_specified return boolean;


	
	-- Returns the category of the given device prefix. If no category could be
	-- found, returns category UNKNOWN.
	function category (prefix : in pac_device_prefix.bounded_string) return
		type_device_category;

		
	-- Returns the category of the given component reference. If no category could be
	-- found, returns category UNKNOWN.
	function category (reference : in type_device_name) return
		type_device_category;



		
-- 	function ports_in_net (
-- 		module 			: in et_schematic_coordinates.type_submodule_name.bounded_string;	-- led_matrix_2
-- 		net				: in et_schematic.pac_net_name.bounded_string;			-- motor_on_off
-- 		category		: in type_device_category;				-- netchanger, connector
-- 		log_threshold	: in type_log_level)
-- 		return et_kicad.type_ports_with_reference.set;
-- 	-- Returns a set of component ports that are connected with the given net.
-- 	-- Returns only components of given category.

-- 	-- Handling routing information requires this type:
-- 	type type_net is record
-- 		module	: et_schematic_coordinates.type_submodule_name.bounded_string;
-- 		net		: et_schematic.pac_net_name.bounded_string;
-- 	end record;
-- 
-- 	function to_string (
-- 		net			: in type_net;
-- 		separator	: in character := '.') return string;
-- 	-- Returns the given net as string. In a form like "led_matrix.master_clock"
-- 	
-- 	function compare_nets (left, right : in type_net) return boolean;
-- 	-- Returns true if left net comes before right net.
	
-- 	-- If nets of type_net are collected in a set, we have a type_route.
-- 	package type_route is new ordered_sets (element_type => type_net, "<" => compare_nets);
-- 	--use type_route;
-- 
-- 	-- Lots of routes are collected in a simple list.
-- 	-- So we have a list of routes.
-- 	package type_routing_table is new doubly_linked_lists (
-- 		element_type => type_route.set,
-- 		"=" => type_route."=");
-- 
-- 	-- This is the rig wide routing table:
-- 	routing_table : type_routing_table.list;
-- 
-- 	-- The minimum length of a route is 2 nets.
-- 	-- The maximum length is set here to a reasonable value. CS: increase if neccessary
-- 	route_length_max : constant positive := 50; 
-- 	type type_route_length is range 2..route_length_max;
-- 
-- 	function to_string (route_length : in type_route_length) return string;
-- 	-- Returns the given route length as string;
	
-- 	function longest_route (table : in type_routing_table.list) return type_route_length;
-- 	-- Returns the lenght of the longest route in the given routing table.
-- 	-- NOTE: assumes that the given routing table is not empty. Raises error othewise.
	
-- 	routing_table_file_name_length : constant positive := 100; -- CS: should suffice for now
-- 	package type_routing_table_file_name is new generic_bounded_length (routing_table_file_name_length);
	
-- 	procedure make_routing_tables (log_threshold : in type_log_level);
-- 	-- Creates the routing table for the whole rig in global variable routin_table.
-- 	-- CS: create routing tables for projects separately.
	
-- 	procedure export_routing_tables (log_threshold : in type_log_level);
-- 	-- Exports/Writes the routing table of the rig in a csv file.
-- 	-- Reads the global rig wide routing table variable routing_table. 
-- 	-- Requires that procedure make_routing_tables has been executed before.
-- 	-- CS: Export routing tables for projects separately.


		

	-- After reading the conventions, we store the units of 
	-- measurement for the design here:
	units_of_measurement : pac_units_of_measurement.map;


	
	-- Translates from given unit_of_measurement (like OHM or VOLT) to the
	-- actual abbrevation like R or V.
	function to_abbrevation (unit : in type_unit_of_measurement) 
		return pac_unit_abbrevation.bounded_string;

	
	
	-- Component categories that requires operator interaction are stored in a set.
	package type_categories_with_operator_interacton is new ordered_sets (
		element_type => type_device_category);

	
	-- After reading the conventions, we store them here:
	component_categories_with_operator_interaction : type_categories_with_operator_interacton.set;


	
	type type_component_requires_operator_interaction is (YES, NO);

	
	-- Returns YES is given prefix requires operator interaction.
	-- Returns NO if prefixs does not require interaction or if no prefixes
	-- specified at all (in conventions file section COMPONENT_PREFIXES).
	function requires_operator_interaction (
		prefix : in pac_device_prefix.bounded_string) 
		return type_component_requires_operator_interaction;

	
-- 	function requires_operator_interaction (
-- 		reference : in et_libraries.type_name)
-- 		return type_component_requires_operator_interaction;	

	-- This type is required for handling text sizes:
	type type_text_schematic is (
		NET_LABEL,
		PORT_NAME,
		TERMINAL_NAME,
		COMPONENT_ATTRIBUTE,
		SHEET_NAME,
		FILE_NAME
		);

	
	-- Converts a string to type_text_schematic.
	function to_text (text : in string) return type_text_schematic;

	
	-- Text sizes of various categories are collected in a map:
	package type_text_sizes_schematic is new ordered_maps (
		key_type		=> type_text_schematic,
		element_type	=> et_schematic_geometry.pac_geometry_2.type_distance_positive,
		"="				=> et_schematic_geometry."=");

	
	-- After reading the conventions file, text sizes are collected here:
	text_sizes_schematic : type_text_sizes_schematic.map;

	
	-- returns the given text type as string.
	function to_string (text : in type_text_schematic) return string;

	
	-- Checks the given text size by its category. Does nothing if no text sizes
	-- specified in configuration file in section TEXT_SIZES_SCHEMATIC.
	procedure check_schematic_text_size (
		category 	: in type_text_schematic;
		size		: in et_schematic_geometry.pac_geometry_2.type_distance_positive);



	
	-- PARTCODE
	
	type type_partcode_section is (
		COMPONENT_PACKAGE,
		COMPONENT_VALUE,
		TOLERANCE,
		MAXIMUM_VOLTAGE,
		MAXIMUM_POWER,
		PART_NUMBER,
		PART_TYPE);

	partcode_keyword_length_max : constant positive := 5;
	partcode_keyword_characters : character_set := to_set (span => ('A','Z')); 
	package type_partcode_keyword is new generic_bounded_length (partcode_keyword_length_max);

	partcode_keyword_argument_lenght_max : constant positive := 10;
	package type_partcode_keyword_argument is new generic_bounded_length (partcode_keyword_argument_lenght_max);

	
	-- Converts a string to a type_partcode_keyword_argument.
	function to_partcode_keyword_argument (argument : in string) return type_partcode_keyword_argument.bounded_string;

	
	-- Converts a type_partcode_keyword_argument to a string.
	function to_string (argument : in type_partcode_keyword_argument.bounded_string) return string;

	
	-- Converts a type_partcode_keyword to a string.
	function to_string (keyword : in type_partcode_keyword.bounded_string) return string;

	
	-- Tests if the given partcode keyword is longer than allowed.
	procedure check_partcode_keyword_length (keyword : in string);

	
	-- Tests if the given keyword contains only valid characters as specified
	-- by given character set.
	-- Raises exception if invalid character found.
	procedure check_partcode_keyword_characters (
		keyword		: in type_partcode_keyword.bounded_string;
		characters	: in character_set := partcode_keyword_characters);

	
	-- Checks whehter given keyword is specified in 
	-- in the conventions file section [PART_CODE_KEYWORDS].
	-- NOTE: Assumes there are keywords specified at all.
	procedure validate_partcode_keyword (keyword : in type_partcode_keyword.bounded_string);

	
	-- Converts a string to a type_partcode_keyword.
	function to_partcode_keyword (keyword : in string) return type_partcode_keyword.bounded_string;

	
	package type_partcode_keywords is new ordered_maps (
		key_type => type_partcode_keyword.bounded_string, -- VMAX, PMAX, TOL
		element_type => type_partcode_section, -- MAXIMUM_VOLTAGE, MAXIMUM_POWER, ...
		"<" => type_partcode_keyword."<");

	partcode_keyword_separator : constant character := '_';

	
	-- Converts a string to a type_partcode_section:
	function to_partcode_section (text : in string) return type_partcode_section;

	
	-- Converts a type_partcode_section to a string:
	function to_string (partcode_section : in type_partcode_section) return string;
	
	partcode_keywords : type_partcode_keywords.map;

	-- Returns true if any part code keywords are specified via conventions file.
	function partcode_keywords_specified return boolean;

	
	-- Returns for the given partcode section the corresponding keyword as specified
	-- in the conventions file section [PART_CODE_KEYWORDS].
	-- If no keyword specified (or no conf. file applied) returns an empty string.
	function to_partcode_keyword (section : in type_partcode_section) return string;

	
	-- Tests if the given partcode of a device is correct.
	-- The given properties are assumed to be those of a real device.
	--  - If partcode keywords are not specified in the 
	--    conventions file, nothing is validated. It is the users responsibility 
	--    to specify a correct partcode.
	--  - If partcode keywords are specified in the conventions file,
	--    the root part (like R_PAC_S_0805_VAL_) is validated.
	procedure validate_partcode (
		partcode		: in pac_device_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
		device_name		: in type_device_name; -- R45
		packge			: in pac_package_name.bounded_string;	-- S_0805
		value 			: in pac_device_value.bounded_string; -- 100R
		log_threshold	: in type_log_level);

	

	
	-- The name of the conventions file may have 100 characters which seems sufficient for now.
 	file_name_length_max : constant natural := 100;
	package pac_file_name is new generic_bounded_length (file_name_length_max); 
	use pac_file_name;

	function to_file_name (file : in string) return pac_file_name.bounded_string;
	function to_string (file : in pac_file_name.bounded_string) return string;

	
	-- Creates a default conventions file:
	procedure make_default_conventions (
		file_name		: in pac_file_name.bounded_string;
		log_threshold	: in type_log_level);

	
	-- Reads the given conventions file:
	procedure read_conventions (
		file_name		: in pac_file_name.bounded_string;
		log_threshold	: in type_log_level);
	-- CS separate body !
	
	
	-- Tests if the given device value meets certain conventions.
	-- This test depends on the category of the device. If no prefixes specified
	-- in the conventions file, this test does nothing.
	-- Returns false if any violation has been detected.
	function value_valid (
		value 	: in pac_device_value.bounded_string;
		prefix	: in pac_device_prefix.bounded_string)
		return boolean;

	
	-- Tests if the given reference has a valid prefix as specified in the conventions file.
	-- Raises warning if not and returns false. 
	-- Returns true if no prefixes specified or if prefix is valid.
	function prefix_valid (prefix : in pac_device_prefix.bounded_string) return boolean;

	
	-- Tests if the given device name has a valid prefix as specified in the conventions file.
	-- Raises warning if not and returns false. 
	-- Returns true if no prefixes specified or if prefix is valid.
	function prefix_valid (device_name : in type_device_name) return boolean;

	
end et_conventions;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
