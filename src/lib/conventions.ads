------------------------------------------------------------------------------
--                                                                          --
--                        SYSTEM ET CONVENTIONS                             --
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

package conventions is

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

-- 	procedure validate_module_interconnection (connection : in type_module_interconnection);
-- 	-- checks if something like "NCC 1 MOTOR_CTRL_OUT_2 MOT 2 MOTOR_CTRL_IN" makes sense
-- 	-- in connection with entries in section import_modules

-- 	procedure validate_module_interconnections (log_threshold: in et_string_processing.type_log_level);
-- 	-- Tests if module interconnections at net level make sense.
-- 	-- NOTE: call AFTER modules have been imported !
	
	function to_string (cat : in type_component_category) return string;
	-- returns the given component category as string

	function to_category (category : in string) return type_component_category;
	-- Converts a string to type_component_category.
	
	-- component prefixes and their category are stored in a map:
	package type_component_prefixes is new ordered_maps (
		key_type => et_libraries.type_device_name_prefix.bounded_string, -- IC
		element_type => type_component_category, -- INTEGRATED_CIRCUIT
		"<" => et_libraries.type_device_name_prefix."<");

	-- After reading the conventions, we store the device prefixes for the design here:
	component_prefixes : type_component_prefixes.map;

	function component_prefixes_specified return boolean;
	-- Returns true if any component prefixes are specified via conventions file.
	
	function category (prefix : in et_libraries.type_device_name_prefix.bounded_string) return
		type_component_category;
	-- Returns the category of the given component prefix. If no category could be
	-- found, returns category UNKNOWN.
	
	function category (reference : in et_libraries.type_device_name) return
		type_component_category;
	-- Returns the category of the given component reference. If no category could be
	-- found, returns category UNKNOWN.

-- 	function ports_in_net (
-- 		module 			: in et_coordinates.type_submodule_name.bounded_string;	-- led_matrix_2
-- 		net				: in et_schematic.type_net_name.bounded_string;			-- motor_on_off
-- 		category		: in type_component_category;				-- netchanger, connector
-- 		log_threshold	: in et_string_processing.type_log_level)
-- 		return et_kicad.type_ports_with_reference.set;
-- 	-- Returns a set of component ports that are connected with the given net.
-- 	-- Returns only components of given category.

-- 	-- Handling routing information requires this type:
-- 	type type_net is record
-- 		module	: et_coordinates.type_submodule_name.bounded_string;
-- 		net		: et_schematic.type_net_name.bounded_string;
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
	
-- 	procedure make_routing_tables (log_threshold : in et_string_processing.type_log_level);
-- 	-- Creates the routing table for the whole rig in global variable routin_table.
-- 	-- CS: create routing tables for projects separately.
	
-- 	procedure export_routing_tables (log_threshold : in et_string_processing.type_log_level);
-- 	-- Exports/Writes the routing table of the rig in a csv file.
-- 	-- Reads the global rig wide routing table variable routing_table. 
-- 	-- Requires that procedure make_routing_tables has been executed before.
-- 	-- CS: Export routing tables for projects separately.

	
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

	-- After reading the conventions, we store the units of measurement for the design here:
	component_units : type_units_of_measurement.map;

	function to_abbrevation (unit : in type_unit_of_measurement) 
	-- Translates from given unit_of_measurement (like OHM or VOLT) to the
	-- actual abbrevation like R or V.
		return type_unit_abbrevation.bounded_string;
	
	-- Component categories that requires operator interaction are stored in a set.
	package type_categories_with_operator_interacton is new ordered_sets (
		element_type => type_component_category);
	-- After reading the conventions, we store them here:
	component_categories_with_operator_interaction : type_categories_with_operator_interacton.set;

	type type_component_requires_operator_interaction is (YES, NO);
	
	function requires_operator_interaction (
		prefix : in et_libraries.type_device_name_prefix.bounded_string) 
		return type_component_requires_operator_interaction;
	-- Returns YES is given prefix requires operator interaction.
	-- Returns NO if prefixs does not require interaction or if no prefixes
	-- specified at all (in conventions file section COMPONENT_PREFIXES).

	
-- 	function requires_operator_interaction (
-- 		reference : in et_libraries.type_device_name)
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

	function to_text (text : in string) return type_text_schematic;
	-- Converts a string to type_text_schematic.

	-- Text sizes of various categories are collected in a map:
	package type_text_sizes_schematic is new ordered_maps (
		key_type => type_text_schematic,
		element_type => et_coordinates.type_distance,
		"=" => et_coordinates."=");

	-- After reading the conventions file, text sizes are collected here:
	text_sizes_schematic : type_text_sizes_schematic.map;

	function to_string (text : in type_text_schematic) return string;
	-- returns the given text type as string.

	procedure check_schematic_text_size (
		category 	: in type_text_schematic;
		size		: in et_libraries.type_text_size);
	-- Checks the given text size by its category.



	
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

	function to_partcode_keyword_argument (argument : in string) return type_partcode_keyword_argument.bounded_string;
	-- Converts a string to a type_partcode_keyword_argument.

	function to_string (argument : in type_partcode_keyword_argument.bounded_string) return string;
	-- Converts a type_partcode_keyword_argument to a string.
	
	function to_string (keyword : in type_partcode_keyword.bounded_string) return string;
	-- Converts a type_partcode_keyword to a string.
	
	procedure check_partcode_keyword_length (keyword : in string);
	-- Tests if the given partcode keyword is longer than allowed.
	
	procedure check_partcode_keyword_characters (
		keyword		: in type_partcode_keyword.bounded_string;
		characters	: in character_set := partcode_keyword_characters);
	-- Tests if the given keyword contains only valid characters as specified
	-- by given character set.
	-- Raises exception if invalid character found.

	procedure validate_partcode_keyword (keyword : in type_partcode_keyword.bounded_string);
	-- Checks whehter given keyword is specified in 
	-- in the conventions file section [PART_CODE_KEYWORDS].
	-- NOTE: Assumes there are keywords specified at all.
	
	function to_partcode_keyword (keyword : in string) return type_partcode_keyword.bounded_string;
	-- Converts a string to a type_partcode_keyword.
	
	package type_partcode_keywords is new ordered_maps (
		key_type => type_partcode_keyword.bounded_string, -- VMAX, PMAX, TOL
		element_type => type_partcode_section, -- MAXIMUM_VOLTAGE, MAXIMUM_POWER, ...
		"<" => type_partcode_keyword."<");

	partcode_keyword_separator : constant character := '_';

	function to_partcode_section (text : in string) return type_partcode_section;
	-- converts a string to a type_partcode_section.

	function to_string (partcode_section : in type_partcode_section) return string;
	-- converts a type_partcode_section to a string.
	
	partcode_keywords : type_partcode_keywords.map;

	function partcode_keywords_specified return boolean;
	-- Returns true if any part code keywords are specified via conventions file.
	
	function to_partcode_keyword (section : in type_partcode_section) return string;
	-- Returns for the given partcode section the corresponding keyword as specified
	-- in the conventions file section [PART_CODE_KEYWORDS].
	-- If no keyword specified (or no conf. file applied) returns an empty string.

	procedure validate_partcode (
	-- Tests if the given partcode of a schematic device is correct.
	-- The given properties are assumed to be those of a real device.
	--  - If partcode keywords are not specified in the 
	--    conventions file, nothing is validated. It is the users responsibility 
	--    to specify a correct partcode.
	--  - If partcode keywords are specified in the conventions file,
	--    the root part (like R_PAC_S_0805_VAL_) is validated.
		partcode		: in et_libraries.type_partcode.bounded_string;		-- R_PAC_S_0805_VAL_100R
		reference		: in et_libraries.type_device_name;						-- R45
		packge			: in et_libraries.type_component_package_name.bounded_string;	-- S_0805
		value 			: in et_libraries.type_value.bounded_string; 			-- 100R
		log_threshold	: in et_string_processing.type_log_level);

	

	
	-- The name of the conventions file may have 100 characters which seems sufficient for now.
 	conventions_file_name_length_max : constant natural := 100;
	package type_conventions_file_name is new generic_bounded_length (conventions_file_name_length_max); 
	use type_conventions_file_name;
	
	procedure make_default_conventions (
		file_name		: in type_conventions_file_name.bounded_string;
		log_threshold	: in et_string_processing.type_log_level);
	-- Creates a default conventions file.

	procedure read_conventions (
		file_name		: in type_conventions_file_name.bounded_string;
		log_threshold	: in et_string_processing.type_log_level);
	-- Reads the given conventions file.

	function value_valid (
	-- Tests if the given device value meets certain conventions.
	-- This test depends on the category of the device. If no prefixes specified
	-- in the conventions file, this test does nothing.
	-- Returns false if any violation has been detected.
		value 	: in et_libraries.type_value.bounded_string;
		prefix	: in et_libraries.type_device_name_prefix.bounded_string)
		return boolean;
	
	function prefix_valid (prefix : in et_libraries.type_device_name_prefix.bounded_string) return boolean;
	-- Tests if the given reference has a valid prefix as specified in the conventions file.
	-- Raises warning if not and returns false. 
	-- Returns true if no prefixes specified or if prefix is valid.
	
	function prefix_valid (reference : in et_libraries.type_device_name) return boolean;
	-- Tests if the given reference has a valid prefix as specified in the conventions file.
	-- Raises warning if not and returns false. 
	-- Returns true if no prefixes specified or if prefix is valid.

end conventions;

-- Soli Deo Gloria
