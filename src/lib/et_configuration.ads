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
-- with ada.strings.unbounded; 	use ada.strings.unbounded;
with ada.containers;            use ada.containers;
-- with ada.containers.vectors;
-- with ada.containers.doubly_linked_lists;
-- with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
-- with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with ada.text_io;				use ada.text_io;
with ada.directories;			use ada.directories;

-- with et_coordinates;
with et_libraries;
-- with et_schematic;
with et_string_processing;

package et_configuration is

	-- configuration file section headers
	section_component_prefixes						: constant string (1..20) := "[COMPONENT_PREFIXES]";
	section_component_units							: constant string (1..22) := "[UNITS_OF_MEASUREMENT]";
	section_components_with_operator_interaction 	: constant string (1..31) := "[OPERATOR_INTERACTION_REQUIRED]";
	-- CS: section_connector_gnd_terminal					: constant string (1..24) := "[CONNECTOR_GND_TERMINAL]";
	section_import_modules							: constant string (1..16) := "[IMPORT_MODULES]";

-- 	package type_import_modules is new ordered_maps (
-- 		key_type => et_schematic.type_component_prefix.bounded_string, -- IC
-- 		element_type => type_component_category, -- INTEGRATED_CIRCUIT
	
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
		INTEGRATED_CIRCUIT,
		JUMPER,					-- a component that allows tieing nets via a removable bridge
		KEYPAD,					-- array of push buttons, keys, switches, ...
		LIGHT_EMMITTING_DIODE,	-- an LED, LASER-diode, IRED-LED, ...
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
	
	type type_unit_of_measurement is ( -- CS: rename to type_unit_of_measurement
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
