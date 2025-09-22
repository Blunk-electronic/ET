------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                            DEVICE MODEL                                  --
--                                                                          --
--                              S p e c                                     --
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
-- DESCRIPTION:
--
-- This is about so called "electrical" device models.
-- A device is called "electrical" because it ALWAYS
-- has an abstract representation in the schematic and mostly 
-- a physical representation in the board. 
--
-- To Do:
-- 1. Rename everything containing the term "unit" to "symbol".


with ada.containers; 			use ada.containers;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;

with et_schematic_geometry;		use et_schematic_geometry;
with et_schematic_coordinates;	use et_schematic_coordinates;
with et_symbol_name;			use et_symbol_name;
with et_symbol_library;			use et_symbol_library;
with et_symbol_model;			use et_symbol_model;
with et_symbol_ports;			use et_symbol_ports;
with et_device_appearance;		use et_device_appearance;
with et_device_model_names;		use et_device_model_names;
with et_device_value;			use et_device_value;
with et_device_prefix;			use et_device_prefix;
with et_device_name;			use et_device_name;
with et_unit_name;				use et_unit_name;
with et_unit_swap_level;		use et_unit_swap_level;
with et_unit_add_level;			use et_unit_add_level;
with et_package_variant;		use et_package_variant;


package et_device_model is

	use pac_geometry_2;

	
	-- A device may have up to 1000 units. CS: seems to be reasonable limit
	subtype type_unit_count is positive range 1 .. 1000;



	-- NOTE: Devices can be composed of internal and/or external units.
	--
	-- An internal unit is modelled directly inside the device model, 
	-- is fixed to the that device model
	-- and can be used by that device model exclusively.
	-- For example after importing a KiCad project there will only be 
	-- internal units.
	--
	-- External units in turn provide much more flexibilty as they can 
	-- be used by many device models. There is no fixed connection between 
	-- device model and unit..


	-- A device has one or more units. A unit is a subsection of a device.
	-- There are internal units, which exist for the particular device exclusively. 
	-- An internal unit has a symbol and further properties like a swap level.
	-- There are external units, which are used for frequently used symbols like resistors or capacitors.
	-- An external unit is just a reference to a symbol library, the symbol name therein and other properties
	-- like swap level.	
	-- The unit name is something like "I/O Bank 3", "PWR" or "Switch 1" "Switch 2"

	
	

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

	use pac_units_internal;


	-- Returns the ports of the given internal unit:
	function get_ports_internal (
		unit_cursor	: in pac_units_internal.cursor)
		return pac_symbol_ports.map;

	
						   
	-- Returns the default x/y-positions of the 
	-- given internal unit. If the given
	-- cursor of the unit is no_element then the
	-- return is an empty list:
	function get_port_positions (
		unit	: in pac_units_internal.cursor)
		return pac_points.list;

	

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

	use pac_units_external;
	

	-- Returns the ports of the given external unit:
	function get_ports_external (
		unit_cursor	: in pac_units_external.cursor)
		return pac_symbol_ports.map;


	
	
	function get_symbol_model_file (
		unit	: in pac_units_external.cursor)
		return pac_symbol_model_file.bounded_string;


	-- Maps from an external unit to the symbol
	-- in the symbol library:
	function get_symbol (
		unit	: in pac_units_external.cursor)
		return pac_symbols.cursor;
	


	

	-- Returns the default x/y-positions 
	-- of the given external unit. If the given
	-- cursor of the unit is no_element then the
	-- return is an empty list:
	function get_port_positions (
		unit	: in pac_units_external.cursor)
		return pac_points.list;
	
	
	

	
	
	type type_device_model (appearance : type_appearance) is record
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
				variants	: pac_package_variants.map;
				
		end case;

	end record;



	-- When querying units of a device this type is required:
	type type_device_units is record
		int : pac_units_internal.cursor;
		ext : pac_units_external.cursor;
	end record;


	-- Returns true if the given units provide
	-- an internal unit:
	function has_internal_unit (
		units : in type_device_units)
		return boolean;
	

	-- Returns true if the given units provide
	-- an external unit:
	function has_external_unit (
		units : in type_device_units)
		return boolean;

	
	-- Returns the name of the internal unit.
	-- If no internal unit exists, then an exception is raised:
	function get_name_internal (
		units : in type_device_units)
		return pac_unit_name.bounded_string;
	

	-- Returns the name of the external unit.
	-- If no external unit exists, then an exception is raised:
	function get_name_external (
		units : in type_device_units)
		return pac_unit_name.bounded_string;



	

	-- Locates the given unit by its name among the
	-- internal units of the given device model.
	-- The result is the cursor set accordingly.
	-- If the unit does not exist among the internal
	-- units then the result is no_element:
	procedure locate_internal (
		model	: in type_device_model;
		unit	: in pac_unit_name.bounded_string;
		cursor	: in out pac_units_internal.cursor);
	

	-- Locates the given unit by its name among the
	-- external units of the given device model.
	-- The result is the cursor set accordingly.
	-- If the unit does not exist among the external
	-- units then the result is no_element:
	procedure locate_external (
		model	: in type_device_model;
		unit	: in pac_unit_name.bounded_string;
		cursor	: in out pac_units_external.cursor);


	

	-- Returns the total number of units that the
	-- given device model contains.
	-- It is the sum of internal and external units:
	function get_unit_count (
		device_model : in type_device_model)
		return type_unit_count;
	

	-- Returns the name of the first package variant
	-- of the given device model.
	-- The model must be a model of a real device. Otherwise
	-- an exception will be raised:
	function get_first_package_variant (
		device_model : in type_device_model)
		return pac_package_variant_name.bounded_string;


	-- Returns the default value as it is 
	-- specified in the device model.
	-- The model must be a model of a real device. Otherwise
	-- an exception will be raised:
	function get_default_value (
		device_model : in type_device_model)
		return pac_device_value.bounded_string;

								   
	
end et_device_model;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
