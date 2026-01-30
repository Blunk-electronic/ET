------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     DEVICE MODEL / INTERNAL UNIT                         --
--                                                                          --
--                              S p e c                                     --
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
with ada.containers.indefinite_ordered_maps;

with et_schematic_geometry;		use et_schematic_geometry;
with et_schematic_coordinates;	use et_schematic_coordinates;
with et_symbol_library;			use et_symbol_library;
with et_symbol_model;			use et_symbol_model;
with et_symbol_ports;			use et_symbol_ports;
with et_device_placeholders.symbols;	use et_device_placeholders.symbols;
with et_device_appearance;		use et_device_appearance;
with et_unit_name;				use et_unit_name;
with et_unit_swap_level;		use et_unit_swap_level;
with et_unit_add_level;			use et_unit_add_level;


package et_device_model_unit_internal is

	use pac_geometry_2;

	use pac_unit_name;
	

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

	
	
	
	-- An internal unit is a symbol with a swap level.
	-- An internal unit is owned by the particular device exclusively.
	type type_unit_internal (appearance : type_appearance) is record
		symbol		: type_symbol (appearance);
		position	: type_vector_model; -- the position of the unit inside the device editor
		swap_level	: type_swap_level := swap_level_default;
		add_level	: type_add_level := add_level_default;
	end record;


	-- Returns the placeholders of an internal unit.
	-- If the units is part of a virtual device,
	-- then default placeholders are returned:	
	function get_placeholders (
		unit	: in type_unit_internal)
		return type_text_placeholders;


	


	
	-- Internal units are collected in a map:
	package pac_units_internal is new indefinite_ordered_maps (
		key_type		=> pac_unit_name.bounded_string, -- like "I/O-Bank 3" "A" or "B"
		element_type	=> type_unit_internal);

	use pac_units_internal;


	-- Returns the placeholders of an internal unit.
	-- If the units is part of a virtual device,
	-- then default placeholders are returned:	
	function get_placeholders (
		unit	: in pac_units_internal.cursor)
		return type_text_placeholders;


	-- In the symbol, the placeholders have a rotation (about itself)
	-- and a position relative to the origin of the symbol.
	-- On instanciating a symbol in the schematic, it becomes a unit
	-- which may have a rotation of its own.
	-- This function translates from the rotation of placeholders
	-- described in the internal symbol of the device model to the rotation of
	-- placeholders of a unit in the schematic.
	-- It translates according to the rotation given by destination:
	function get_default_placeholders (
		symbol_cursor	: in pac_units_internal.cursor;
		destination		: in type_object_position)
		return type_text_placeholders;


	
	function get_symbol (
		unit	: in pac_units_internal.cursor)
		return type_symbol;

	
	
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
								   
	
end et_device_model_unit_internal;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
