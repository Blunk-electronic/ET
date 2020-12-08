------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                              DEVICES                                     --
--                                                                          --
--                              S p e c                                     --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_coordinates;			use et_coordinates;
with et_geometry;
with et_string_processing;
with et_general;
with et_text;
with et_symbols;				use et_symbols;
with et_terminals;
with et_packages;				use et_packages;

package et_devices is

-- DEVICE PURPOSE
	-- Devices that require operator interaction like connectors, LEDs or switches 
	-- MUST have a purpose assigned.
	-- Example: The purpose of connector X44 is "power in". The purpose of LED5 is "system fail":

	keyword_purpose : constant string := "purpose";
	
	purpose_characters : character_set := to_set 
		(ranges => (('a','z'),('A','Z'),('0','9'))) or to_set ("_- "); 

	purpose_length_max : constant positive := 50;

	package pac_device_purpose is new generic_bounded_length (purpose_length_max);
	purpose_default : constant string := "dummy";

	function to_string (purpose : in pac_device_purpose.bounded_string) return string;
	
	function purpose_length_valid (purpose : in string) return boolean;
	-- Returns true if given purpose is too long. Issues warning.	
	
	function purpose_characters_valid (
		purpose		: in pac_device_purpose.bounded_string;
		characters	: in character_set := purpose_characters) 
		return boolean;
	-- Tests if the given value contains only valid characters as specified
	-- by given character set. Returns false if invalid character found.

	procedure purpose_invalid (purpose : in string);
	-- Issues error message and raises constraint error.

	function to_purpose (
	-- Tests the given purpose for length and invalid characters.
		purpose 					: in string;
		error_on_invalid_character	: in boolean := true)
		return pac_device_purpose.bounded_string;

	-- Returns true if purpose is empty ("").
	function is_empty (purpose : in pac_device_purpose.bounded_string) return boolean;

	
	-- To handle names of package models like libraries/packages/smd/SOT23.pac use this:
	keyword_package_model : constant string := "package_model";

	device_model_file_name_length_max : constant positive := 200;
	package pac_device_model_file is new generic_bounded_length (device_model_file_name_length_max); -- ../lbr/logic_ttl/7400.dev
	function to_string (name : in pac_device_model_file.bounded_string) return string;
	function to_file_name (name : in string) return pac_device_model_file.bounded_string;




	-- The device value is something like 330R or 100n or 74LS00
	keyword_value : constant string := "value";
	
	value_length_max : constant positive := 50;

	-- Define the characters that are allowed for a value:
	value_characters : character_set := 
		to_set (ranges => (('A','Z'),('a','z'),('0','9'))) 
		or to_set ('_')
		or to_set ('-');
	
	package pac_device_value is new generic_bounded_length (value_length_max);

	function to_string (value : in pac_device_value.bounded_string) return string;
	function to_value (value : in string) return pac_device_value.bounded_string;
	
	function value_length_valid (value : in string) return boolean;
	-- Tests if the given value is longer than allowed. Returns false if too long.
	-- Returns true if length is in allowed range.

	function truncate (value : in string) return pac_device_value.bounded_string;
	
	function value_characters_valid (
		value		: in pac_device_value.bounded_string;
		characters	: in character_set := value_characters)
		return boolean;
	-- Tests if the given value contains only valid characters as specified
	-- by given character set. Returns false if invalid character found.
	-- Issues warning.	

	procedure value_invalid (value : in string);
	-- Issues error message and raises constraint error.

	function to_value_with_check (
	-- Tests the given value for length and invalid characters.
		value						: in string;
		error_on_invalid_character	: in boolean := true)
		return pac_device_value.bounded_string;

	-- Returns true if value is empty ("").
	function is_empty (value : in pac_device_value.bounded_string) return boolean;



	-- A device name consists of a prefix (like R, C, IC, ..)
	-- and a consecutive number. Both form something like "IC702"
	prefix_characters : character_set := to_set (span => ('A','Z'));
	prefix_length_max : constant natural := 10; -- CS: there is no reason for longer prefixes.
	package pac_device_prefix is new generic_bounded_length (prefix_length_max);
	use pac_device_prefix;
	
	function to_string (prefix : in pac_device_prefix.bounded_string) return string;
	function to_prefix (prefix : in string) return pac_device_prefix.bounded_string;

	procedure check_prefix_length (prefix : in string);
	-- Tests if the given prefix is longer than allowed.
	
	procedure check_prefix_characters (prefix : in pac_device_prefix.bounded_string);
	-- Tests if the given prefix contains only valid characters.
	-- Raises exception if invalid character found.

	-- Predefined prefixes:
	-- - for power symbols:
	prefix_pwr : constant string := "PWR";
	-- NOTE: When adding more predefined prefixes here, mind
	-- to update function prefix_valid in et_conventions.
	-- CS distinguish between power symbols with prefixes 
	-- like PWR_POS, PWR_GND, PWR_NEG ? Probably already solved by
	-- the signal direction of the port ?
	


-- DEVICE NAMES
	keyword_device : constant string := "device";
	
	prefix_default : constant pac_device_prefix.bounded_string := pac_device_prefix.to_bounded_string ("?");

	subtype type_name_index is natural range natural'first .. 99_999; -- R1..R99999, IC1..IC99999 should be enough
	name_index_default : constant type_name_index := 0;

	function to_string (index : in type_name_index) return string;
	function to_index (index : in string) return type_name_index;

	subtype type_index_width is positive range positive'first .. 5; -- see number of digits of type_device_name_index
	
	type type_device_name is record -- CS: should be private
		prefix		: pac_device_prefix.bounded_string := prefix_default; -- like "IC"
		id			: type_name_index := name_index_default; -- like "303"
		id_width	: type_index_width := type_index_width'first; -- the number of digits of the id. 
		-- Example: id_width is 3 in case of an id like of 937
		-- NOTE: This allows something like R091 or IC0 (there are reasons for such strange things ...)
	end record;

	-- Returns true if the prefixes of left and right are equal:
	function same_prefix (left, right : in type_device_name) return boolean;
	
	-- Converts a string like "IC303" to a composite type_device_name.
	-- Raises constraint error if prefix contains invalid characters.
	-- Raises constraint error if id contains non-digit characters.
	-- Leading zeroes in the index are removed. R002 becomes R2.
	function to_device_name (text_in : in string) return type_device_name;

	function "<" (left, right : in type_device_name) return boolean;
	-- Returns true if left comes before right.
	-- If left equals right, the return is false.	

	function "=" (left, right : in type_device_name) return boolean;
	-- Returns true if left equals right.
	
	function to_string (name : in type_device_name) return string;
	-- Returns the given device name as string.
	-- Prepends leading zeros according to name.id_width.
	
	function prefix (name : in type_device_name) return pac_device_prefix.bounded_string;
	-- Returns the prefix of the given device name.

	function index (name : in type_device_name) return type_name_index;
	-- Returns the index of the given device name.

	-- Builds a device name by given prefix (like R) and index (like 23) to a device name (like R23).
	-- If width is not provided, then the width of the index is calculated automatically. In case of R23 the width is 2.
	-- If width is provided, then it is set accordingly.
	function to_device_name (
		prefix	: in pac_device_prefix.bounded_string; 	-- R, C, L
		index	: in type_name_index;				-- 1, 20, ..
		width	: in type_index_width := type_index_width'first) -- the number of digits
		return type_device_name;

	procedure offset_index (
	-- Adds to the device index the given offset. 
	-- Example: given name is R4, given offset is 100. Result R104.
		name	: in out type_device_name;
		offset	: in type_name_index);

	

	
-- TERMINALS	

	type type_terminal_count is new count_type; -- CS: limit to a reasonable range ?

	function to_string (terminals : in type_terminal_count) return string;
	-- Returns the given number of terminals as string.
	
	


	


	

	keyword_unit		: constant string := "unit";		
	keyword_swap_level	: constant string := "swap_level";
	keyword_add_level	: constant string := "add_level";

	unit_name_length_max : constant natural := 50;	
	-- CS unit_name_characters, length check, character check
	package pac_unit_name is new generic_bounded_length (unit_name_length_max);
	use pac_unit_name;

	unit_name_default : constant pac_unit_name.bounded_string := pac_unit_name.to_bounded_string ("");
	
	function to_string (unit_name : in pac_unit_name.bounded_string) return string;
	function to_unit_name (unit_name : in string) return pac_unit_name.bounded_string; 
	
	device_unit_separator : constant character := '.';

	-- A device may have up to 1000 units. CS: seems to be reasonable limit
	subtype type_unit_count is positive range 1 .. 1000;
	
	-- This function concatenates the device name and unit name, separated
	-- by the device_unit_separator. If the given unit_count is 1 then just
	-- the device name will be returned as string.
	function to_full_name (
		device_name	: in type_device_name; -- IC34
		symbol_name	: in pac_unit_name.bounded_string; -- PWR
		unit_count	: in type_unit_count) -- the total number of units
		return string; -- IC34.PWR


	
	swap_level_max : constant natural := 10;
	type type_swap_level is new natural range 0 .. swap_level_max;
	swap_level_default : constant := type_swap_level'first;

	function to_string (swap_level : in type_swap_level) return string;
	function to_swap_level (swap_level : in string) return type_swap_level;	

	type type_add_level is (
		NEXT, 		-- should be default. for things like logic gates, multi-OP-Amps, ...
		REQUEST, 	-- for power supply 
		CAN,		-- OPTIONAl units. things like relay contacts
		ALWAYS,		-- units that SHOULD be used always
		MUST);		-- units that MUST be used. things like relay coils.

	add_level_default : constant type_add_level := type_add_level'first;
	
	function to_string (add_level : in type_add_level) return string;
	function to_add_level (add_level : in string) return type_add_level;
	
	-- An internal unit is a symbol with a swap level.
	-- An internal unit is owned by the particular device exclusively.
	type type_unit_internal (appearance : type_appearance) is record
		symbol		: type_symbol (appearance);
		position	: pac_geometry_sch.type_point; -- the position of the unit inside the device editor
		swap_level	: type_swap_level := swap_level_default;
		add_level	: type_add_level := add_level_default;
	end record;

	-- Internal units are collected in a map:
	package pac_units_internal is new indefinite_ordered_maps (
		key_type		=> pac_unit_name.bounded_string, -- like "I/O-Bank 3" "A" or "B"
		element_type	=> type_unit_internal);

	-- An external unit has a reference and a swap level.
    type type_unit_external is record
        -- file is the link to the symbol in container "symbols":
        model		: pac_symbol_model_file.bounded_string; -- like /libraries/symbols/NAND.sym
       	position	: pac_geometry_sch.type_point := pac_geometry_sch.origin; -- the position within the device editor
		swap_level	: type_swap_level := swap_level_default;
		add_level	: type_add_level := type_add_level'first;
	end record;

	-- External units are collected in a map;
	package pac_units_external is new ordered_maps (
		key_type		=> pac_unit_name.bounded_string, -- like "I/O-Bank 3"
		element_type	=> type_unit_external);





	


	
-- PACKAGE VARIANTS
	-- The variant is usually a suffix in a device value, given by its manufacturer. The variant is a manufacturer
	-- specific abbrevation for the package a device comes with.
	-- Example: An opamp made by TI can be the type TL084N or TL084D. N means the NDIP14 package
	-- whereas D means the SO14 package.
	-- If a device has package variants, a suffix after the value indicates the package
	-- The variant name is manufacturer specific. example: TL084D or TL084N
	-- device package variant names like "N" or "D" are stored in bounded strings.
	variant_name_characters : character_set := 
		to_set (ranges => (('A','Z'),('a','z'),('0','9'))) or to_set ("_-");
	
	variant_name_length_max : constant positive := 50;
	package pac_package_variant_name is new generic_bounded_length (variant_name_length_max);
	use pac_package_variant_name;

	function to_string (package_variant : in pac_package_variant_name.bounded_string) return string;
	-- converts a pac_package_variant_name to a string.
	
	function to_variant_name (variant_name : in string) return pac_package_variant_name.bounded_string;

	procedure check_variant_name_length (variant_name : in string);
	-- tests if the given variant name is not longer than allowed
	
	procedure check_variant_name_characters (
		variant		: in pac_package_variant_name.bounded_string;
		characters	: in character_set := variant_name_characters);
	-- Tests if the given variant name contains only valid characters as specified
	-- by given character set.
	-- Raises exception if invalid character found.

	type type_port_in_terminal_port_map is record
		name	: pac_port_name.bounded_string; -- CLK, CE, VSS
		unit	: pac_unit_name.bounded_string; -- GPIO_BANK_3
	end record;
	
	package pac_terminal_port_map is new ordered_maps (
		key_type 		=> et_terminals.type_terminal_name.bounded_string, -- H7, 14
		"<"				=> et_terminals.type_terminal_name."<",
		element_type 	=> type_port_in_terminal_port_map); -- unit A, OE1


	
	type type_variant is record
		package_model		: type_package_model_file.bounded_string; -- libraries/packages/smd/SOT23.pac
		terminal_port_map	: pac_terminal_port_map.map; -- which port is connected with with terminal
	end record;

	package pac_variants is new ordered_maps (
		key_type 		=> pac_package_variant_name.bounded_string, -- D, N
		element_type 	=> type_variant);

	type type_terminal is record
		name	: et_terminals.type_terminal_name.bounded_string; -- H7
		unit	: pac_unit_name.bounded_string; -- IO-BANK1
		port	: pac_port_name.bounded_string; -- GPIO3
	end record;

	function to_string (
		terminal	: in type_terminal;
		show_unit	: in boolean := false;
		preamble	: in boolean := true)
		return string;
	-- Returns the given terminal as string. 
	-- If show_unit is true, the unit name is output.
	-- If preamble is true, each property of the terminal is headed by a short preamble.
	


-- DEVICES
	type type_device (appearance : type_appearance) is record -- CS rename to type_device_lib
		prefix			: pac_device_prefix.bounded_string; -- R, C, IC, ...
		units_internal	: pac_units_internal.map := pac_units_internal.empty_map;
		units_external	: pac_units_external.map := pac_units_external.empty_map;

		case appearance is

			-- If a device appears in the schematic only, it is a virtual component 
			-- and thus does not have any package variants.
			-- Such components are power symbols. Later when building netlists
			-- those components enforce net names (like GND or P3V3).
			when VIRTUAL => 
				null;

			-- If a device appears in both schematic and layout it comes 
			-- with at least one package/footprint variant. We store variants in a map.
			when PCB => 
				value		: pac_device_value.bounded_string; -- 74LS00
				--partcode	: type_component_partcode.bounded_string;
				variants	: pac_variants.map;
				
		end case;

	end record;




	
	-- Use this function to adopt placeholder position and rotation 
	-- of a internal symbol.
	-- Rotates the positions of placeholders and their rotation about
	-- their own origin according to rotation given by destination:
	function rotate_placeholders (
		symbol_cursor	: in pac_units_internal.cursor;
		destination		: in et_coordinates.type_position)
		return type_rotated_placeholders;


	
	
	package pac_devices_lib is new indefinite_ordered_maps (
		key_type 		=> pac_device_model_file.bounded_string, -- ../libraries/devices/logic_ttl/7400.dev
		"<"				=> pac_device_model_file."<",
		element_type	=> type_device);

	device_model_file_extension : constant string := "dev";

	-- HERE RIG WIDE DEVICES ARE KEPT:
	devices : pac_devices_lib.map;


	
	-- When querying units of a device this type is required:
	type type_device_units is record
		int : pac_units_internal.cursor;
		ext : pac_units_external.cursor;
	end record;

	-- Returns true if the given device (via a cursor) 
	-- does provide the given unit.
	function provides_unit (
		device_cursor	: in pac_devices_lib.cursor;
		unit_name		: in pac_unit_name.bounded_string)
		return boolean;
	
	-- Returns the cursor of the first internal or external unit.
	-- Searches first in internal and then in external units. 
	--  The search order is further-on determined
	-- by the add levels of the units. Priority is add level MUST,
	-- then ALWAYS, then NEXT, then REQUEST, then CAN.
	--  If no suitable internal unit found, the cursor of internal 
	-- units in the return is no_element.
	--  If no suitable external unit found, the cursor of external
	-- units in the return is no_element.
	function first_unit (
		device_cursor : in pac_devices_lib.cursor) 
		return type_device_units;

	-- Returns the name of the first unit.
	-- It can be an internal or an external unit.
	function first_unit (
		device_cursor : in pac_devices_lib.cursor) 
		return pac_unit_name.bounded_string;

							
	-- Returns the cursor of the desired internal or external unit.
	function any_unit (
		device_cursor	: in pac_devices_lib.cursor;
		unit_name		: in pac_unit_name.bounded_string)
		return type_device_units;

	package pac_unit_names is new doubly_linked_lists (pac_unit_name.bounded_string);

	-- Returns a list of all unit names of the given device:
	function all_units (
		device_cursor	: in pac_devices_lib.cursor)
		return pac_unit_names.list;

	
	-- Returns the total number of units the given device provides:
	function units_total (
		device_cursor	: in pac_devices_lib.cursor)
		return type_unit_count;

	
	function variant_available (
	-- Returns true if given device provides the given package variant.								   
	-- The given device must be real. Means appearance SCH_PCB.
		device_cursor	: in pac_devices_lib.cursor;
		variant			: in pac_package_variant_name.bounded_string)  -- D, N
		return boolean;

	-- Returns a list of available variants of the given device.
	-- If the device is virtual, then an empty list will be returned.
	function available_variants (
		device_cursor	: in pac_devices_lib.cursor)
		return pac_variants.map;

	
	-- Locates the given generic device in container "devices".
	function locate_device (model : in pac_device_model_file.bounded_string) -- ../libraries/devices/transistor/pnp.dev
		return pac_devices_lib.cursor;

	-- For locating units this type is required by function locate_unit.
	-- A unit can either be external (most likely) or internal to the device:
	type type_unit_ext_int is (EXT, INT);
	type type_unit_cursors (ext_int : type_unit_ext_int) is record
		case ext_int is
			when EXT => 
				external	: pac_units_external.cursor;
			when INT =>
				internal	: pac_units_internal.cursor;
		end case;
	end record;

	-- Searches the given unit in the given device. Returns a cursor to 
	-- either the internal or external unit.
	function locate_unit (
		device_cursor	: in pac_devices_lib.cursor;
		unit_name		: in pac_unit_name.bounded_string) -- like "I/O-Bank 3"
		return type_unit_cursors;
	
	function package_model (
	-- Returns the name of the package model of the given device according to the given variant.
	-- The given device must be real. Means appearance SCH_PCB.							  
		device_cursor	: in pac_devices_lib.cursor;
		variant			: in pac_package_variant_name.bounded_string) -- D, N
		return type_package_model_file.bounded_string; -- libraries/packages/smd/SOT23.pac
		
-- 	function terminal_name (
-- 	-- Returns the name of the terminal name of the given device according to the given variant.
-- 	-- The given device must be real. Means appearance SCH_PCB.							  
-- 		device_cursor	: in pac_devices_lib.cursor;
-- 		port_name		: in pac_port_name.bounded_string;
-- 		variant			: in pac_package_variant_name.bounded_string) -- D, N
-- 		return type_terminal_name.bounded_string; -- 14, H4

	-- Used for netlists:
	type type_port_properties (direction : type_port_direction) is record
		terminal	: et_terminals.type_terminal_name.bounded_string; -- H4, 1, 16
		properties	: type_port (direction);
	end record;

	function properties (
	-- Returns the poperties of the given port of the given device.
		device_cursor	: in pac_devices_lib.cursor;
		port_name		: in pac_port_name.bounded_string)
		return pac_ports.cursor;

	type type_port_properties_access is access type_port_properties;	
	
	
		
end et_devices;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
