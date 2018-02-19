------------------------------------------------------------------------------
--                                                                          --
--                        SYSTEM ET CONFIGURATION                           --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2018 Mario Blunk, Blunk electronic                 --
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

--   For correct displaying set tab with in your edtior to 4.

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

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers;            use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.ordered_sets;

with ada.text_io;				use ada.text_io;
with ada.directories;			use ada.directories;

with et_coordinates;
with et_libraries;
with et_schematic;
with et_import;
with et_string_processing;

package et_configuration is

	-- configuration file section headers
	section_component_prefixes						: constant string (1..20)	:= "[COMPONENT_PREFIXES]";
	section_component_units							: constant string (1..22)	:= "[UNITS_OF_MEASUREMENT]";
	section_components_with_operator_interaction 	: constant string (1..31)	:= "[OPERATOR_INTERACTION_REQUIRED]";
	-- CS: section_connector_gnd_terminal					: constant string (1..24) := "[CONNECTOR_GND_TERMINAL]";
	section_import_modules							: constant string (1..16)	:= "[IMPORT_MODULES]";
	section_module_interconnections					: constant string (1..25)	:= "[MODULE_INTERCONNECTIONS]";
	section_text_sizes_schematic					: constant string (1..22)	:= "[TEXT_SIZES_SCHEMATIC]";

	option_module_interconnections_comparator_off	: constant string (1..18)	:= "net_comparator_off";
	option_module_interconnections_comparator_on	: constant string (1..17)	:= "net_comparator_on";
	option_module_interconnections_warn_only		: constant string (1..9) 	:= "warn_only";	
	
	-- A module to be imported has a name, an abbrevation, a CAD format and a certain 
	-- number of instances.
	-- Example: nucleo_core NCC kicad_v4 1
	-- We collect these modules in a simple list because the order must be kept.
	type type_import_module is record
		name		: et_coordinates.type_submodule_name.bounded_string; -- MOTOR_DRIVER
		abbrevation	: et_coordinates.type_submodule_abbrevation.bounded_string; -- MOT
		format		: et_import.type_cad_format; -- KICAD_V4, EAGLE_V7
		instances	: et_coordinates.type_submodule_instance; -- 4 
	end record;

	package type_import_modules is new doubly_linked_lists (
		element_type => type_import_module);

	-- Finally the container where the modules to be imported are stored is this:
	import_modules : type_import_modules.list;


	-- Module interconnections are collected in a simple list:
	-- CS type type_connector_gender is (UNKNOWN, FEMALE, MALE);
	
	type type_connector is record
		-- CS: gender ?
		abbrevation	: et_coordinates.type_submodule_abbrevation.bounded_string; -- MOT
		instance	: et_coordinates.type_submodule_instance; -- 4 
		purpose		: et_libraries.type_component_purpose.bounded_string; -- MOTOR_CTRL_IN
	end record;

	type type_net_comparator_on_off is (ON, OFF);
	
	function to_string (net_comparator_on_off : in type_net_comparator_on_off) return string;
	-- Returns the given net comparator status as string.
	
	type type_net_comparator_warn_only is (ON, OFF);

	function to_string (net_comparator_warn : in type_net_comparator_warn_only) return string;
	-- Returns the given net comparator warning status as string.
	
	type type_module_interconnection_options is record
		comparator	: type_net_comparator_on_off := ON; -- net_comparator_off
		warn_only 	: type_net_comparator_warn_only := OFF; -- warn_only
	end record;
	
	type type_module_interconnection is record
		peer_A	: type_connector;
		peer_B	: type_connector;
		options	: type_module_interconnection_options;
	end record;

	-- This is the container with the module interconnections:
	package type_module_interconnections is new doubly_linked_lists (
		element_type => type_module_interconnection);

	module_interconnections : type_module_interconnections.list;

	type type_component_category is (
		ANTENNA,
		BATTERY,
		BUZZER,
-- 		CABLE,
		CAPACITOR,
		CAPACITOR_ADJUSTABLE,	-- adjustable capacitor (also known as trimmer)
		CONNECTOR,				-- component where another component of opposide gender is plugged
		DIAC,
		DIODE,
		DIODE_PHOTO,			-- light sensitive diode
		DISPLAY,				-- display, LCD, LED, VFD, ...
		FUSE,
		HEATSINK,				-- a bulk of metal that absorbs and dissipates excessive heat
		INDUCTOR,
		INDUCTOR_ADJUSTABLE,
		INTEGRATED_CIRCUIT,
		JUMPER,					-- a component that allows tieing nets via a removable bridge
		KEYPAD,					-- array of push buttons, keys, switches, ...
		LIGHT_EMMITTING_DIODE,	-- an LED, LASER-diode, IRED-LED, ...
		LIGHT_EMMITTING_DIODE_ARRAY,
		LOUDSPEAKER,
		MICROPHONE,
		NETCHANGER,				-- ties two nets together
		MOTOR,
		OPTOCOUPLER,
		QUARTZ,					-- quartz crystal resonators
		POTENTIOMETER,			-- variable resistor
		RELAY,
		RESISTOR,				-- varistors, trimmers, potentiometers, photoresistors
		RESISTOR_ADJUSTABLE,	-- adjustable resistor
		RESISTOR_NETWORK,		-- a collection of resistors in a single housing
		RESISTOR_PHOTO,			-- light sensitive resistor
		SWITCH,					-- push buttons, breakers, makers, rotary encoders, ...
		TESTPOINT,				-- a point where measurements can be taken
		THYRISTOR,
		THYRISTOR_PHOTO,		-- light sensitive thyristor
		TRANSFORMER,
		TRANSISTOR,				-- NPN, PNP, NFET, MOSFET, ...
		TRANSISTOR_PHOTO,		-- light sensitive transistor
		TRIAC,
		TUBE,					-- triodes, pentodes, thyratrons, klystrons, ...
		UNKNOWN					-- not specified
-- 		WIRE
		);

	procedure multiple_purpose_error (
	-- Outputs an error message on multiple usage of a purpose of a component category.
		category : in type_component_category; -- CONNECTOR, LIGHT_EMMITTING_DIODE, ...
		purpose : in et_libraries.type_component_purpose.bounded_string); -- PWR_IN, SYS_FAIL, ...
	
	function multiple_purpose (
	-- Returns the number of occurences of components with the given purpose and category.
	-- Example: If there are two connectors with purpose "PWR_IN" the return is 2.
		category : in type_component_category; -- CONNECTOR, LIGHT_EMMITTING_DIODE, ...
		purpose : in et_libraries.type_component_purpose.bounded_string; -- PWR_IN, SYS_FAIL, ...
		log_threshold : in et_string_processing.type_log_level)
		return natural;
	
	procedure validate_module_interconnection (connection : in type_module_interconnection);
	-- checks if something like "NCC 1 MOTOR_CTRL_OUT_2 MOT 2 MOTOR_CTRL_IN" makes sense
	-- in connection with entries in section import_modules

	procedure validate_module_interconnections (log_threshold: in et_string_processing.type_log_level);
	-- Tests if module interconnections at net level make sense.
	-- NOTE: call AFTER modules have been imported !
	
	function to_string (cat : in type_component_category) return string;
	-- returns the given component category as string

	function to_category (category : in string) return type_component_category;
	-- Converts a string to type_component_category.
	
	-- component prefixes and their category are stored in a map:
	package type_component_prefixes is new ordered_maps (
		key_type => et_libraries.type_component_prefix.bounded_string, -- IC
		element_type => type_component_category, -- INTEGRATED_CIRCUIT
		"<" => et_libraries.type_component_prefix."<");

	-- After reading the configuration, we store the component prefixes for the design here:
	component_prefixes : type_component_prefixes.map;

	function category (prefix : in et_libraries.type_component_prefix.bounded_string) return
		type_component_category;
	-- Returns the category of the given component prefix. If no category could be
	-- found, returns category UNKNOWN.
	
	function category (reference : in et_libraries.type_component_reference) return
		type_component_category;
	-- Returns the category of the given component reference. If no category could be
	-- found, returns category UNKNOWN.

	function ports_in_net (
		module 			: in et_coordinates.type_submodule_name.bounded_string;	-- led_matrix_2
		net				: in et_schematic.type_net_name.bounded_string;			-- motor_on_off
		category		: in type_component_category;				-- netchanger, connector
		log_threshold	: in et_string_processing.type_log_level)
		return et_schematic.type_ports_with_reference.set;
	-- Returns a set of component ports that are connected with the given net.
	-- Returns only components of given category.

	-- Handling routing information requires this type:
	type type_net is record
		module	: et_coordinates.type_submodule_name.bounded_string;
		net		: et_schematic.type_net_name.bounded_string;
	end record;

	function to_string (
		net			: in type_net;
		separator	: in character := '.') return string;
	-- Returns the given net as string. In a form like "led_matrix.master_clock"
	
	function compare_nets (left, right : in type_net) return boolean;
	-- Returns true if left net comes before right net.
	
	-- If nets of type_net are collected in a set, we have a type_route.
	package type_route is new ordered_sets (element_type => type_net, "<" => compare_nets);
	--use type_route;

	-- Lots of routes are collected in a simple list.
	-- So we have a list of routes.
	package type_routing_table is new doubly_linked_lists (
		element_type => type_route.set,
		"=" => type_route."=");

	-- This is the rig wide routing table:
	routing_table : type_routing_table.list;

	-- The minimum length of a route is 2 nets.
	-- The maximum length is set here to a reasonable value. CS: increase if neccessary
	route_length_max : constant positive := 50; 
	type type_route_length is range 2..route_length_max;

	function to_string (route_length : in type_route_length) return string;
	-- Returns the given route length as string;
	
	function longest_route (table : in type_routing_table.list) return type_route_length;
	-- Returns the lenght of the longest route in the given routing table.
	-- NOTE: assumes that the given routing table is not empty. Raises error othewise.
	
	routing_table_file_name_length : constant positive := 100; -- CS: should suffice for now
	package type_routing_table_file_name is new generic_bounded_length (routing_table_file_name_length);
	
	procedure make_routing_tables (log_threshold : in et_string_processing.type_log_level);
	-- Creates the routing table for the whole rig in global variable routin_table.
	-- CS: create routing tables for projects separately.
	
	procedure export_routing_tables (log_threshold : in et_string_processing.type_log_level);
	-- Exports/Writes the routing table of the rig in a csv file.
	-- Reads the global rig wide routing table variable routing_table. 
	-- Requires that procedure make_routing_tables has been executed before.
	-- CS: Export routing tables for projects separately.

	
	type type_unit_of_measurement is (
		MILLIOHM,
		OHM,
		KILOOHM,
		MEGAOHM,
		GIGAOHM,

		PICOFARAD,
		NANOFARAD,
		MICROFARAD,
		MILLIFARAD,
		FARAD,
		
		NANOHENRY,
		MICROHENRY,		
		MILLIHENRY,	
		HENRY,

		VOLT,

		MILLIAMPERE,
		AMPERE,

		KILOHERTZ,
		MEGAHERTZ,
		GIGAHERTZ
		);

	function to_unit_of_measurement (unit : in string) return type_unit_of_measurement;
	-- Converts a string to type_unit_of_measurement.
	
	function to_string (unit : in type_unit_of_measurement) return string;
	-- returns the given unit of measurement as string. (things like OHM, KILOOHM, MEGAOHM, ...)

	-- The abbrevations of units of measurement are limited to two characters.
	-- So the user could define units like uF or mH. More than two characters are not common.
	-- However, the recommendation is to use just one character like u, m, k, M. Reason: how to express
	-- something like 3.3Ohms since the Ohm character is a special character ?
	-- By the category of the component we can reason that it is about Ohms, Henry or Farad.
	unit_abbrevation_characters : character_set := to_set (ranges => (('A','Z'),('a','z')));
	unit_abbrevation_length_max : constant positive := 2;
	package type_unit_abbrevation is new generic_bounded_length (unit_abbrevation_length_max);

	procedure check_abbrevation_of_unit_characters (
		abbrevation : in type_unit_abbrevation.bounded_string;
		characters : in character_set);
	-- Tests if the given abbrevation contains only valid characters as specified
	-- by given character set. Raises exception if invalid character found.
	
	-- Units of measurement and their abbrevation are stored in a map:
	package type_units_of_measurement is new ordered_maps (
		key_type => type_unit_of_measurement, -- OHMS, KILOOHM, MEGAOHM, ...
		element_type => type_unit_abbrevation.bounded_string, -- R, m, k, ...
		"=" => type_unit_abbrevation."=");

	-- After reading the configuration, we store the units of measurement for the design here:
	component_units : type_units_of_measurement.map;


	-- Component categories that requires operator interaction are stored in a set.
	package type_categories_with_operator_interacton is new ordered_sets (
		element_type => type_component_category);
	-- After reading the configuration, we store them here:
	component_categories_with_operator_interaction : type_categories_with_operator_interacton.set;

	type type_component_requires_operator_interaction is (YES, NO);
	
	function requires_operator_interaction (
		prefix : in et_libraries.type_component_prefix.bounded_string) 
		return type_component_requires_operator_interaction;
	-- Returns YES is given prefix requires operator interaction.
	
-- 	function requires_operator_interaction (
-- 		reference : in et_libraries.type_component_reference)
-- 		return type_component_requires_operator_interaction;	

	-- This type is required for handling text sizes:
	type type_text_schematic is (
		NET_LABEL,
		PORT_NAME,
		TERMINAL_NAME);

	function to_string (text : in type_text_schematic) return string;
	-- returns the given text type as string.

	configuration_file_handle : ada.text_io.file_type;
	
	-- The name of the configuration file may have 100 characters which seems sufficient for now.
 	configuraton_file_name_length : constant natural := 100;
	package type_configuration_file_name is new generic_bounded_length (configuraton_file_name_length); 
	use type_configuration_file_name;
	
	procedure make_default_configuration (
		file_name		: in type_configuration_file_name.bounded_string;
		log_threshold	: in et_string_processing.type_log_level);
	-- Creates a default configuration file.

	procedure read_configuration (
		file_name		: in type_configuration_file_name.bounded_string;
		log_threshold	: in et_string_processing.type_log_level);
	-- Reads the given configuration file.
	-- Fills component_prefixes.

	procedure validate_prefix (prefix : in et_libraries.type_component_prefix.bounded_string); 
	-- Tests if the given prefix is valid as specified in the configuration file.
	-- Raises exception if not.
	
	procedure validate_prefix (reference : in et_libraries.type_component_reference);
	-- Tests if the given reference has a valid prefix as specified in the configuration file.
	-- Raises exception if not.

end et_configuration;

-- Soli Deo Gloria
