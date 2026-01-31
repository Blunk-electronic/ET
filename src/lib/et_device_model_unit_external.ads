------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     DEVICE MODEL / EXTERNAL UNIT                         --
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
--
--
--
-- To Do:
-- - Rename everything containing the term "unit" to "symbol".


with ada.containers; 					use ada.containers;
with ada.containers.ordered_maps;

with et_schematic_geometry;				use et_schematic_geometry;
with et_schematic_coordinates;			use et_schematic_coordinates;
with et_symbol_name;					use et_symbol_name;
with et_symbol_library;					use et_symbol_library;
with et_symbol_model;					use et_symbol_model;
with et_symbol_ports;					use et_symbol_ports;
with et_unit_name;						use et_unit_name;
with et_unit_swap_level;				use et_unit_swap_level;
with et_unit_add_level;					use et_unit_add_level;


package et_device_model_unit_external is

	use pac_geometry_2;
	use pac_unit_name;	

	
	
	-- An external unit has a reference and a swap level.
    type type_unit_external is record

		-- This is the link to the symbol in symbol library:
		model_cursor : pac_symbol_models.cursor;

		-- IMPORTANT: When reading the device model file, the
		-- symbol model must have been read beforehand.
		-- Otherwise a valid cursor can not be assigned
		-- to the unit !
		
		-- The position within the device editor:
		position	: type_vector_model := origin;
		
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


	
	
	function get_symbol_model_file ( -- CS rename to get_symbol_model_name
		unit	: in pac_units_external.cursor)
		return pac_symbol_model_file.bounded_string;


	function get_symbol_model_name (
		unit	: in pac_units_external.cursor)
		return string;

	
	
	-- Maps from an external unit to the symbol
	-- in the symbol library:
	function get_symbol (
		unit	: in pac_units_external.cursor)
		return pac_symbol_models.cursor;
	


	

	-- Returns the default x/y-positions 
	-- of the given external unit. If the given
	-- cursor of the unit is no_element then the
	-- return is an empty list:
	function get_port_positions (
		unit	: in pac_units_external.cursor)
		return pac_points.list;
	
								   
	
end et_device_model_unit_external;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
