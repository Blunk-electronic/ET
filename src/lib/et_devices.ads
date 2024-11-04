------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         DEVICES IN LIBRARIES                             --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
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


with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;

with et_coordinates_2;			use et_coordinates_2;
with et_logging;				use et_logging;
with et_port_direction;			use et_port_direction;
with et_port_names;				use et_port_names;
with et_symbol_ports;			use et_symbol_ports;
with et_symbols;				use et_symbols;
with et_terminals;				use et_terminals;
with et_device_appearance;		use et_device_appearance;
with et_package_names;			use et_package_names;
with et_device_purpose;			use et_device_purpose;
with et_device_model_names;		use et_device_model_names;
with et_device_value;			use et_device_value;
with et_device_prefix;			use et_device_prefix;
with et_device_name;			use et_device_name;
with et_unit_name;				use et_unit_name;
with et_unit_swap_level;		use et_unit_swap_level;
with et_unit_add_level;			use et_unit_add_level;
with et_package_variant;		use et_package_variant;


package et_devices is

	use pac_geometry_2;

	-- To handle names of package models like libraries/packages/smd/SOT23.pac use this:
	keyword_package_model : constant string := "package_model";

	keyword_device : constant string := "device";	

	keyword_unit		: constant string := "unit";		


	
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


	

-- INTERNAL UNITS:
	
	-- An internal unit is a symbol with a swap level.
	-- An internal unit is owned by the particular device exclusively.
	type type_unit_internal (appearance : type_appearance) is record
		symbol		: type_symbol (appearance);
		position	: type_vector_model; -- the position of the unit inside the device editor
		swap_level	: type_swap_level := swap_level_default;
		add_level	: type_add_level := add_level_default;
	end record;


	
	use pac_unit_name;

	
	-- Internal units are collected in a map:
	package pac_units_internal is new indefinite_ordered_maps (
		key_type		=> pac_unit_name.bounded_string, -- like "I/O-Bank 3" "A" or "B"
		element_type	=> type_unit_internal);




-- EXTERNAL UNITS:
	
	-- An external unit has a reference and a swap level.
    type type_unit_external is record
        -- file is the link to the symbol in container "symbols":
        model		: pac_symbol_model_file.bounded_string; -- like /libraries/symbols/NAND.sym
       	position	: type_vector_model := origin; -- the position within the device editor
		swap_level	: type_swap_level := swap_level_default;
		add_level	: type_add_level := type_add_level'first;
	end record;
	

	-- External units are collected in a map;
	package pac_units_external is new ordered_maps (
		key_type		=> pac_unit_name.bounded_string, -- like "I/O-Bank 3"
		element_type	=> type_unit_external);



	
	
	


-- DEVICES:
	
	type type_device_lib (appearance : type_appearance) is record -- CS rename to type_device_model ?
		prefix			: pac_device_prefix.bounded_string; -- R, C, IC, ...
		units_internal	: pac_units_internal.map := pac_units_internal.empty_map;
		units_external	: pac_units_external.map := pac_units_external.empty_map;

		case appearance is

			-- If a device appears in the schematic only, it is a virtual component 
			-- and thus does not have any package variants.
			-- Such components are power symbols. Later when building netlists
			-- those components enforce net names (like GND or P3V3).
			when APPEARANCE_VIRTUAL => 
				null;

			-- If a device appears in both schematic and layout it comes 
			-- with at least one package/footprint variant. We store variants in a map.
			when APPEARANCE_PCB => 
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
		destination		: in et_coordinates_2.type_position)
		return type_rotated_placeholders;



	--function hash_device_model (
		--model	: in pac_device_model_file.bounded_string)
		--return hash_type;

	--function equivalent_models (
		--d1, d2 : in type_device_lib)
		--return boolean;

	--subtype type_t is type_device_lib (PCB);
	
	--package pac_devices_lib2 is new hashed_maps (
		--key_type 		=> pac_device_model_file.bounded_string, -- ../libraries/devices/logic_ttl/7400.dev
		--element_type	=> type_t,
		--hash			=> hash_device_model,
		--equivalent_keys	=> pac_device_model_file."=",
		--"="				=> equivalent_models);

	-- https://github.com/PiEqThree/Ada_Hash_Map/blob/main/main.adb

	
	package pac_devices_lib is new indefinite_ordered_maps (
		key_type 		=> pac_device_model_file.bounded_string, -- ../libraries/devices/logic_ttl/7400.dev
		"<"				=> pac_device_model_file."<",
		element_type	=> type_device_lib);

	use pac_devices_lib;


	


	-- HERE RIG WIDE DEVICES ARE KEPT:
	devices : pac_devices_lib.map;


	-- Returns true if the given device has a physical counterpart in 
	-- the layout, means if it is not virtual:
	function is_real (
		device_cursor : in pac_devices_lib.cursor)
		return boolean;

	
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


	-- Returns full information about the given package variant.
	-- If the package variant is not defined in the model, then
	-- then the result is no_element:
	function get_package_variant (
		device_cursor	: in pac_devices_lib.cursor;
		variant			: in pac_package_variant_name.bounded_string)  -- D, N
		return pac_variants.cursor;

	
	-- Returns true if given device provides the given package variant.								   
	-- The given device must be real. Means appearance SCH_PCB.
	function variant_available (
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

	
	-- Returns the name of the package model of the given device according to the given variant.
	-- The given device must be real. Means appearance SCH_PCB.							  
	function get_package_model (
		device_cursor	: in pac_devices_lib.cursor;
		variant			: in pac_package_variant_name.bounded_string) -- D, N
		return pac_package_model_file_name.bounded_string; -- libraries/packages/smd/SOT23.pac

	
	-- 	function terminal_name (
-- 	-- Returns the name of the terminal name of the given device according to the given variant.
-- 	-- The given device must be real. Means appearance SCH_PCB.							  
-- 		device_cursor	: in pac_devices_lib.cursor;
-- 		port_name		: in pac_port_name.bounded_string;
-- 		variant			: in pac_package_variant_name.bounded_string) -- D, N
-- 		return pac_terminal_name.bounded_string; -- 14, H4

	
	-- Used for netlists and ratsnest:
	type type_port_properties (direction : type_port_direction) is record
		terminal	: et_terminals.pac_terminal_name.bounded_string; -- H4, 1, 16
		properties	: type_port (direction);
	end record;

	
	-- Returns the properties of the given port of the given device.
	function properties (
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
