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

-- with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
-- with ada.strings.unbounded; 	use ada.strings.unbounded;
with ada.containers;            use ada.containers;
-- with ada.containers.vectors;
-- with ada.containers.doubly_linked_lists;
-- with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
-- with ada.containers.indefinite_ordered_maps;
-- with ada.containers.ordered_sets;

with ada.text_io;				use ada.text_io;
with ada.directories;			use ada.directories;

-- with et_coordinates;
with et_libraries;
-- with et_schematic;
with et_string_processing;

package et_configuration is

	section_component_prefixes	: constant string (1..20) := "[COMPONENT_PREFIXES]";
	section_component_values	: constant string (1..18) := "[COMPONENT_VALUES]";
	section_components_with_operator_interaction : constant string (1..31) := "[OPERATOR_INTERACTION_REQUIRED]";
	section_connector_gnd_terminal : constant string (1..24) := "[CONNECTOR_GND_TERMINAL]";

	type type_component_category is (
		RESISTOR,
		CAPACITOR,
		INDUCTOR,
		TRANSFORMER,
		DIODE,
		TRANSISTOR,
		LIGHT_EMMITTING_DIODE,
		INTEGRATED_CIRCUIT,
		NETCHANGER,
-- 		WIRE,
-- 		CABLE,
		CONNECTOR,
		JUMPER,
		SWITCH,
		RELAY,
-- 		CONTACTOR,
		MOTOR,
		BUZZER,
		LOUDSPEAKER,
		MICROPHONE
		);
	
	function to_string (cat : in type_component_category) return string;
	-- returns the given component category as string

	package type_configuration_component_prefixes is new ordered_maps (
		key_type => et_libraries.type_component_prefix.bounded_string, -- IC
		element_type => type_component_category, -- INTEGRATED_CIRCUIT
		"<" => et_libraries.type_component_prefix."<");

	-- After reading the configuration, we store the component prefixes for the design here:
	configuration_component_prefixes : type_configuration_component_prefixes.map;
	
	type type_component_value is (
		OHM,
		MILLIOHM,
		KILOOHM,
		MEGAOHM,
		FARAD,
		MILLIFARAD,
		MICROFARAD,
		NANOFARAD,
		PICOFARAD,
		HENRY,
		MILLIHENRY,
		MICROHENRY,
		NANOHENRY
		);

	function to_string (unit : in type_component_value) return string;
	-- returns the given value as string
	
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
	
end et_configuration;

-- Soli Deo Gloria
