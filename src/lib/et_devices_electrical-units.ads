------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      DEVICES ELECTRICAL / UNITS                          --
--                                                                          --
--                               S p e c                                    --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo: 
--
-- 
-- DESCRIPTION:
-- 
-- This package is about the type, basic properties and subprograms related
-- to so units of electrical devices as they appear in the schematic drawing.


with et_schematic_geometry;				use et_schematic_geometry;
with et_schematic_coordinates;			use et_schematic_coordinates;
with et_sheets;							use et_sheets;

with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.symbols;	use et_device_placeholders.symbols;

with et_coordinates_formatting;			use et_coordinates_formatting;
with et_symbol_model;					use et_symbol_model;
with et_terminal_name;					use et_terminal_name;
with et_port_names;						use et_port_names;
with et_device_model;					use et_device_model;
with et_device_model_names;				use et_device_model_names;
with et_device_name;					use et_device_name;
with et_device_library;					use et_device_library;
with et_device_library.units;			use et_device_library.units;
with et_device_property_level;			use et_device_property_level;
with et_commit;
with et_object_status;					use et_object_status;
with et_unit_name;						use et_unit_name;
with et_units;							use et_units;
with et_symbol_ports;					use et_symbol_ports;
with et_terminals;						use et_terminals;
with et_logging;						use et_logging;


package et_devices_electrical.units is

	use pac_geometry_2;
	
	use pac_units;


	

	-- Returns the names of all deployed units:
	function get_unit_names_deployed (
		device : in type_device_electrical)
		return pac_unit_names.list;
	
								
	-- Returns the total number of units that
	-- the device provides according to its model:
	function get_unit_count (
		device : in type_device_electrical)
		return natural;

	
	-- Returns the number of units that are deployed:
	function get_unit_count_deployed (
		device : in type_device_electrical)
		return natural;

	


	

	-- Returns the cursor to the first deployed unit
	-- of the given device:
	function get_first_unit (
		device : in type_device_electrical)
		return pac_units.cursor;


	
	


	-- Locates the given unit in the given device.
	-- If the unit can not be located (because it is not
	-- deployed yet or does not exist at all), then 
	-- the result is no_element:
	function locate_unit (
		device	: in type_device_electrical;
		unit	: in pac_unit_name.bounded_string)
		return pac_units.cursor;

	

	-- The result of a unit query is of this type:
	type type_unit_query (exists : boolean := false) is record
		case exists is
			when true => 
				position : type_object_position;
				-- x/y, rotation, sheet
				
			when false => 
				null;
		end case;
	end record;



	-- Returns the result of a unit query in human readable form.
	-- If the unit_name is empty (""), then the result does not contain
	-- any reference to a unit. This is useful when a device has only one unit.
	function to_string (
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- C
		query_result	: in type_unit_query)
		return string;



	-- Returns the names of deployed units:
	function get_unit_names_deployed (
		device : in pac_devices_electrical.cursor)
		return pac_unit_names.list;

	
	
	-- Returns the total number of units that the
	-- given device provides according to the device model:
	function get_unit_count (
		device : in pac_devices_electrical.cursor)
		return type_unit_count;
	

	-- Returns the number of units that are deployed:
	function get_unit_count_deployed (
		device : in pac_devices_electrical.cursor)
		return natural;

	
	
	

	device_unit_separator : constant character := '.';


	-- This function concatenates the device name and unit name, separated
	-- by the device_unit_separator like IC4.C.
	-- If the device has only one unit, then only the device name 
	-- will be returned as string like IC4:
	function get_full_name (
		device	: in pac_devices_electrical.cursor;
		unit	: in pac_units.cursor)
		return string;

	

	-- This function concatenates the device name and unit name, separated
	-- by the device_unit_separator. If the given unit_count is 1 then just
	-- the device name will be returned as string.
	function get_full_name (
		device		: in type_device_name; -- IC34
		unit		: in pac_unit_name.bounded_string; -- PWR
		unit_count	: in type_unit_count) -- the total number of units
		return string; -- IC34.PWR






	-- Returns the position (x/y/sheet) of the given unit.
	-- Raises constraint error if device or unit does not exist.
	function get_position (
		device	: in pac_devices_electrical.cursor; -- R2
		unit	: in pac_units.cursor) -- A, B, C
		return type_object_position;


	
	-- Returns the sheet of the given unit.
	-- Raises constraint error if device or unit does not exist.
	function get_sheet (
		device	: in pac_devices_electrical.cursor; -- R2
		unit	: in pac_units.cursor) -- A, B, C
		return type_sheet;



	-- Returns the position of the unit inqured for.
	-- If the unit does not exist,
	-- then the return is false (see specs of
	-- type type_unit_query above).
	-- The function just inquires for a unit with the given
	-- name. So it does not distinguish between "not deployed"
	-- or "not available or "not existing":
	function get_unit_position (
		device_cursor	: in pac_devices_electrical.cursor;
		unit_name		: in pac_unit_name.bounded_string)
		return type_unit_query;

	

	
	-- Collects the positions of all units (in schematic) of 
	-- the given device and returns them in a list.
	function get_unit_positions (
		device_cursor : in pac_devices_electrical.cursor) 
		return pac_unit_positions.map;



	-- Writes the positions of the device units in the log file.
	procedure log_unit_positions (
		positions 		: in pac_unit_positions.map;
		log_threshold	: in type_log_level);


	-- Returns the positions of the ports
	-- of a given unit of a given device in
	-- the schematic. The position, mirror style and rotation
	-- the the unit in the schematic is taken into account:
	function get_port_positions (
		device	: in pac_devices_electrical.cursor;
		unit	: in pac_units.cursor)
		return pac_points.list;
	
	
	
	-- Returns a map of ports of the given device and unit.
	-- The coordinates of the ports are the default x/y-positions relative
	-- to the origin of the unit as they are defined in the symbol model:
	function get_ports_from_symbol_model (
		device_cursor	: in pac_devices_electrical.cursor;
		unit_name		: in pac_unit_name.bounded_string)
		return pac_symbol_ports.map;


	-- Returns a map of ports of the given device and unit.
	-- The coordinates of the ports are the
	-- absolute x/y-positions as they are in the schematic:
	function get_ports_from_schematic (
		device_cursor	: in pac_devices_electrical.cursor;
		unit_cursor		: in pac_units.cursor)
		return pac_symbol_ports.map;





	-- Returns the position (x/y) of the given placeholder
	-- of the given unit.
	-- Raises constraint error if device or unit does not exist.
	function get_position (
		device		: in pac_devices_electrical.cursor; -- R2
		unit		: in pac_units.cursor;
		category	: in type_placeholder_meaning)
		return type_vector_model;



-- CS	
	-- procedure add_first_available_unit (
	-- 	device			: in pac_devices_electrical.cursor; -- IC2
	-- 	log_threshold	: in type_log_level);
	
	
	









-- PLACEHOLDERS:
	



	-- In the schematic, when a unit is rotated to a certain absolute rotation,
	-- or if the placeholders are to be restored (kind of un-smash),
	-- the default positions of texts and placeholders are required. For this
	-- reason we define here the type type_default_text_positions:
	package pac_text_positions is new doubly_linked_lists (type_vector_model);

	

	type type_default_text_positions (appearance : type_appearance) is record

		-- For texts, we need only their positions (x/y):
		texts : pac_text_positions.list; -- same order as the texts in type_symbol_base

		-- The placeholders are copies of those in the symbol (see type_symbol_model):
		case appearance is
			when APPEARANCE_PCB =>
				placeholders : type_text_placeholders;
				
			when APPEARANCE_VIRTUAL => null;
		end case;
	end record;

	
	
	-- Returns the default positions of placeholders and texts of a unit
	-- as they are defined in the symbol model.
	function get_default_text_positions (
		device_cursor	: in pac_devices_electrical.cursor;
		unit_name		: in pac_unit_name.bounded_string)
		return type_default_text_positions;

	

	

-- PROPERTIES QUERIES:
	
	

	-- Returns properties of the given unit.
	-- 1. By default no linebreaks are inserted in the output,
	--    so that the result is a single line.
	-- 2. If linebreaks is true, then linebreaks are inserted.
	--    This is useful when the output is to be displayed
	--    in a window or if it is to be written in a file:
	function get_unit_properties (
		unit_cursor	: in pac_units.cursor;
		level		: in type_properties_level;
		linebreaks	: in boolean := false)
		return string;



	-- Returns properties of the given device and its units.
	-- See comments of function get_unit_properties regarding linebreaks:
	function get_device_properties (
		device		: in type_device_electrical;
		level		: in type_properties_level;
		linebreaks	: in boolean := false)
		return string;

	

	
	-- function get_properties (
	-- 	device		: in type_device_electrical;
	-- 	level		: in type_properties_level;
	-- 	all_units	: in boolean := true;
	-- 	unit		: in pac_unit_name.bounded_string := unit_name_default)
	-- 	-- CS format ?
	-- 	return string;



	-- Returns properties of the given device. 
	-- 1. Level determines the degree and amount of information to be returned.
	-- 2. If all_units is true, then no special focus is on a certain unit
	--    and information about all units is returned.
	-- 3. If all_units is false, then only properties of the given unit
	--    (via unit_cursor) is returned.
	-- 4. See comments of function get_unit_properties regarding linebreaks:
	function get_properties (
		device_cursor	: in pac_devices_electrical.cursor;
		level			: in type_properties_level;
		all_units		: in boolean := true;
		unit_cursor		: in pac_units.cursor := pac_units.no_element;
		linebreaks		: in boolean := false)
		return string;



	
	

	-- Maps from the given terminal to the linked port and unit.
	-- The given device must be real. Otherwise a constraint error
	-- will be raised:
	function get_port (
		device		: in pac_devices_electrical.cursor;
		terminal	: in pac_terminal_name.bounded_string) -- H7, 1, 14
		return type_get_port_result;






	


-- SEARCH AND FIND:

	-- For showing and finding devices and units:
	type type_device_search_mode is (
		SEARCH_MODE_FIRST_UNIT,
		SEARCH_MODE_BY_UNIT_NAME,
		SEARCH_MODE_FIRST_UNIT_ON_CURRENT_SHEET);

	
	function to_string (
		mode : in type_device_search_mode)
		return string;
	


	-- Sets all units or an explicitly given unit as selected.
	-- If all_units is true, then the parameter "unit" is ignored: 
	procedure select_unit (
		device		: in out type_device_electrical;
		all_units	: in boolean;
		unit_name	: in pac_unit_name.bounded_string);



		
end et_devices_electrical.units;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
