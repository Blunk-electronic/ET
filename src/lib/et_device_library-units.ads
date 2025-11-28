------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        DEVICE LIBRARY UNITS                              --
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
--
--
--
-- ToDo:
--
--


with et_port_direction;			use et_port_direction;
with et_port_names;				use et_port_names;
with et_symbol_ports;			use et_symbol_ports;
with et_symbol_model;			use et_symbol_model;
with et_unit_name;				use et_unit_name;
with et_unit_swap_level;		use et_unit_swap_level;
with et_unit_add_level;			use et_unit_add_level;



package et_device_library.units is

	use pac_unit_name;

	
	
	-- Returns true if the given device (via a cursor) 
	-- does provide the given unit.
	function provides_unit (
		device_cursor	: in pac_device_models.cursor;
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
	function get_first_unit (
		device_cursor : in pac_device_models.cursor) 
		return type_device_units;


	
	-- Returns the name of the first unit.
	-- It can be an internal or an external unit.
	function get_first_unit (
		device_cursor : in pac_device_models.cursor) 
		return pac_unit_name.bounded_string;


	
							
	-- Returns the cursor of the desired internal or external unit.
	function get_unit (
		device_cursor	: in pac_device_models.cursor;
		unit_name		: in pac_unit_name.bounded_string)
		return type_device_units;


	-- If unit names are to be stored in lists:
	package pac_unit_names is new 
		doubly_linked_lists (pac_unit_name.bounded_string);


	
	-- Returns a list of all unit names of the given device:
	function get_all_units (
		device_cursor	: in pac_device_models.cursor)
		return pac_unit_names.list;


	
	-- Returns the total number of units
	-- that the given device model provides:
	function get_unit_count (
		device_cursor	: in pac_device_models.cursor)
		return type_unit_count;

	



	

	
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
		device_cursor	: in pac_device_models.cursor;
		unit_name		: in pac_unit_name.bounded_string) -- like "I/O-Bank 3"
		return type_unit_cursors;


	-- Maps from the given symbol cursor to the actual symbol:
	function get_symbol (
		unit : in type_unit_cursors)
		return type_symbol;
		
	

	
	-- Used for netlists and ratsnest:
	type type_port_properties (direction : type_port_direction) is record
		terminal	: et_terminals.pac_terminal_name.bounded_string; -- H4, 1, 16
		properties	: type_symbol_port (direction);
	end record;

	
	-- Returns the properties of the given port of the given device.
	function get_properties (
		device_cursor	: in pac_device_models.cursor;
		port_name		: in pac_port_name.bounded_string)
		return pac_symbol_ports.cursor;

	type type_port_properties_access is access type_port_properties;	
	

	

	-- Returns the ports of the given device and unit.
	-- The coordinates of the ports are the default x/y-positions relative
	-- to the origin of the unit as they are defined in the symbol model.
	function get_ports_of_unit (
		device_cursor	: in pac_device_models.cursor;
		unit_name		: in pac_unit_name.bounded_string)
		return pac_symbol_ports.map;


	
end et_device_library.units;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
