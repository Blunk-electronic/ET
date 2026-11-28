------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      DEVICE ELECTRICAL / UNITS                           --
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
-- To Do:
--
--
--
--
-- DESCRIPTION:
--
-- The subprograms in this package address the units
-- of an electrical device. So the targeted device must be real.
-- Otherwise exceptions wlll be raised.
-- 1. Here it is about coordinates, location vectors
--    places, angles and rotation as they are used in
--    the schematic domain exclusively.


with ada.containers;					use ada.containers;
with ada.containers.doubly_linked_lists;

with et_coordinates_formatting;			use et_coordinates_formatting;
with et_coordinates_abs_rel;			use et_coordinates_abs_rel;

with et_sheets;							use et_sheets;

with et_schematic_geometry;				use et_schematic_geometry;
with et_schematic_coordinates;			use et_schematic_coordinates;

with et_device_model;					use et_device_model;
with et_device_property_level;			use et_device_property_level;
with et_device_placeholders.symbols;	use et_device_placeholders.symbols;

with et_units;							use et_units;



package et_device_electrical.units is

	use pac_geometry_2;


	device_unit_separator : constant character := '.';


	-- This function returns a string like
	-- "device IC1 unit C". The output is straigth
	-- forward, independed of how many units the device
	-- has. So the output can also be"device R1 unit 1".
	-- Use this function for log messages and debugging:
	function to_string (
		device		: in type_device_name; -- IC34
		unit		: in type_unit_name) -- PWR
		return string;



	-- This function concatenates the device name and unit name, separated
	-- by the device_unit_separator. If the given unit_count is 1 then just
	-- the device name will be returned as string.
	function get_full_name (
		device		: in type_device_name; -- IC34
		unit		: in type_unit_name; -- PWR
		unit_count	: in type_unit_count) -- the total number of units
		return string; -- IC34.PWR





	-- The result of a unit query is of this type:
	type type_unit_query (exists : boolean := false) is record
		case exists is
			when true =>
				position : type_schematic_object;
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
		unit_name		: in type_unit_name; -- C
		query_result	: in type_unit_query)
		return string;








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




	-- Locates the given unit in the given device.
	-- If the unit can not be located (because it is not
	-- deployed yet or does not exist at all), then
	-- the result is no_element:
	function locate_unit (
		device	: in type_device_electrical;
		unit	: in type_unit_name)
		return pac_units.cursor;


	-- Returns true if the specified unit
	-- is deployed:
	function is_unit_deployed (
		device	: in type_device_electrical;
		unit	: in type_unit_name)
		return boolean;


	procedure set_units (
		device	: in out type_device_electrical;
		units	: in pac_units.map);


	function get_units (
		device	: in type_device_electrical)
		return pac_units.map;


	procedure add_unit (
		device	: in out type_device_electrical;
		name	: in type_unit_name; -- A, B, C, GPIO1
		unit	: in type_unit);


	procedure delete_unit (
		device	: in out type_device_electrical;
		unit	: in type_unit_name);


	-- Returns the cursor to the first deployed unit
	-- of the given device:
	function get_first_unit (
		device	: in type_device_electrical)
		return pac_units.cursor;


	-- Iterates the units of the given device
	-- and calls for each unit the specified procedure:
	procedure iterate_units (
		device	: in type_device_electrical;
		process	: not null access procedure (position : in pac_units.cursor));


	procedure iterate_units (
		device	: in type_device_electrical;
		process	: not null access procedure (position : in pac_units.cursor);
		proceed	: not null access boolean);





-- POSITION:

	-- Returns the position (x/y/sheet) of the given unit.
	-- Raises constraint error if device or unit does not exist.
	function get_position (
		device	: in type_device_electrical; -- R2
		unit	: in type_unit_name) -- A, B, C
		return type_schematic_object;


	function get_unit_positions (
		device	: in type_device_electrical)
		return pac_unit_positions.map;


	-- Writes the positions of the device units in the log file.
	procedure log_unit_positions (
		positions		: in pac_unit_positions.map;
		log_threshold	: in type_log_level);



	procedure move_unit (
		device		: in out type_device_electrical;
		unit_name	: in type_unit_name;
		coordinates	: in type_coordinates;
		destination	: in type_vector_model;
		sheet		: in type_sheet_relative);


	procedure drag_unit (
		device		: in out type_device_electrical;
		unit_name	: in type_unit_name;
		coordinates	: in type_coordinates;
		destination	: in type_vector_model;
		sheet		: in type_sheet_relative);


	procedure rotate_unit (
		device		: in out type_device_electrical;
		unit_name	: in type_unit_name;
		coordinates	: in type_coordinates;
		rotation	: in type_rotation_model);


	-- Toggles the mirror status:
	procedure mirror_unit (
		device		: in out type_device_electrical;
		unit_name	: in type_unit_name);



	-- Moves all units which are on the given sheet
	-- by the given offset:
	procedure move_units_by_sheets (
		device		: in out type_device_electrical;
		sheet		: in type_sheet;
		offset		: in type_sheet_relative);




	-- Copies a unit into the given device. The copy
	-- will be placed by the given number of sheets and
	-- the given offset (x/y) away from the original unit.
	-- CS:
	-- 1. Currently the caller must ensure that the unit
	--    can become a part of the given device as specified.
	--    The source device and the target device must have the
	--    same device model.
	-- 2. If the unit already exists in the given device,
	--    then an exception is raised.
	procedure copy_unit_to_device (
		source_unit_cursor	: in pac_units.cursor;
		sheet				: in type_sheet_relative;
		offset				: in type_vector_model;
		target_device		: in out type_device_electrical);


	-- Sets all units or an explicitly given unit as selected.
	-- If all_units is true, then the parameter "unit" is ignored:
	procedure select_unit (
		device		: in out type_device_electrical;
		all_units	: in boolean;
		unit_name	: in type_unit_name);


	-- Modifies the status of a unit:
	procedure modify_status (
		device		: in out type_device_electrical;
		unit_cursor	: in pac_units.cursor;
		operation	: in type_status_operation);


	-- Calls a procedure that modifies the given
	-- unit:
	procedure modify_unit (
		device		: in out type_device_electrical;
		unit_cursor	: in pac_units.cursor;
		process		: not null access procedure (
				name	: in type_unit_name;
				unit	: in out type_unit));




-- PLACEHOLDERS:


	-- In the schematic, when a unit is rotated to a
	-- certain absolute rotation,
	-- or if the placeholders are to be restored (kind of un-smash),
	-- the default positions of texts and placeholders are required.
	-- For this reason we define here the type type_default_text_positions:
	package pac_text_positions is new doubly_linked_lists (type_vector_model);



	type type_default_text_positions (
		appearance : type_appearance)
	is record

		-- For texts, we need only their positions (x/y):
		texts : pac_text_positions.list; -- same order as the texts in type_symbol_base

		-- The placeholders are copies of those in the symbol (see type_symbol_model):
		case appearance is
			when APPEARANCE_PCB =>
				placeholders : type_text_placeholders;

			when APPEARANCE_VIRTUAL => null;
		end case;
	end record;



	-- CS:
	-- Returns the default positions of placeholders
	-- and texts of a unit
	-- as they are defined in the symbol model.
	-- function get_default_text_positions (
	-- 	device		: in type_device_electrical;
	-- 	unit_name	: in type_unit_name)
	-- 	return type_default_text_positions



-- PROPERTIES QUERIES:


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
	-- 	unit		: in type_unit_name := unit_name_default)
	-- 	-- CS format ?
	-- 	return string;




-- SEARCH AND FIND:

	-- For showing and finding devices and units:
	type type_device_search_mode is (
		SEARCH_MODE_FIRST_UNIT,
		SEARCH_MODE_BY_UNIT_NAME,
		SEARCH_MODE_FIRST_UNIT_ON_CURRENT_SHEET);


	function to_string (
		mode : in type_device_search_mode)
		return string;


end et_device_electrical.units;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
