------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          DEVICE UNITS IN SCHEMATIC                       --
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
-- DESCRIPTION:
--
--
--
--   history of changes:
--
--   ToDo:

with ada.containers;		        use ada.containers;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;

with et_schematic_geometry;				use et_schematic_geometry;
with et_schematic_coordinates;			use et_schematic_coordinates;
with et_sheets;							use et_sheets;
with et_port_names;						use et_port_names;
with et_symbol_ports;					use et_symbol_ports;

with et_device_unit;					use et_device_unit;
with et_unit_name;						use et_unit_name;
with et_mirroring;						use et_mirroring;
-- with et_device_appearance;				use et_device_appearance;
with et_object_status;					use et_object_status;
with et_coordinates_abs_rel;			use et_coordinates_abs_rel;


package et_units is

	use pac_unit_name;

	use pac_geometry_2;




	-- Units of a device are collected in a map.
	-- A unit is accessed by its name like "I/O Bank 3" or "PWR" or "A" or "B" ...
	package pac_units is new indefinite_ordered_maps (
		key_type		=> type_unit_name,
		element_type	=> type_unit);

	use pac_units;


	-- Returns a unit as indicated by unit cursor:
	function get_unit (
		unit_cursor	: in pac_units.cursor)
		return type_unit;



	-- function get_position (
	-- 	unit	: in pac_units.cursor)
	-- 	return type_object_position;


	function get_rotation (
		unit	: in pac_units.cursor)
		return type_rotation_model;



	function get_mirror_status (
		unit	: in pac_units.cursor)
		return type_mirror;





	-- Iterates the units.
	-- Aborts the process when the proceed-flag goes false:
	procedure iterate (
		units	: in pac_units.map;
		process	: not null access procedure (position : in pac_units.cursor);
		proceed	: not null access boolean);



	function get_unit_name (
		unit : in pac_units.cursor)
		return type_unit_name;



	-- Returns the name of a unit as a string:
	function get_unit_name (
		unit : in pac_units.cursor)
		return string;


	-- Returns a string that tells the name
	-- and position of given unit.
	function to_string (
		unit : in pac_units.cursor)
		return string;



	function is_proposed (
		unit : in pac_units.cursor)
		return boolean;


	function is_selected (
		unit : in pac_units.cursor)
		return boolean;


	function is_moving (
		unit : in pac_units.cursor)
		return boolean;



	package pac_unit_positions is new ordered_maps (
		key_type		=> type_unit_name, -- A, B, IO_BANK_1
		element_type	=> type_object_position); -- sheet, x, y



	-- -- Extracts from the given list of object positions a list
	-- -- of sheet numbers. A sheet number occurs only once
	-- -- in the returned list:
	-- function extract_sheets (
	-- 	positions	: in pac_unit_positions.map)
	-- 	return pac_sheet_numbers.list;

	-- Extracts from the given list of units a list
	-- of sheet numbers. A sheet number occurs only once
	-- in the returned list:
	function extract_sheets (
		units	: in pac_units.map)
		return pac_sheet_numbers.list;




		-- -- Returns a list of units and their
		-- -- coordinates in the schematic:
		-- function get_unit_positions (
		-- 	units : in pac_units.map)
		-- 	return pac_unit_positions.map;




	-- When units are dragged about the sheet then connected
	-- net segments must be dragged along.
	-- So a list of port names with their old an new positions
	-- is required.
	-- The list tells which port is to be moved from an
	-- old to a new position:
	package pac_dragged_ports is new ordered_maps (
		key_type		=> type_port_name,
		element_type	=> type_drag);

	use pac_dragged_ports;


	function get_port_name (
		port : in pac_dragged_ports.cursor)
		return type_port_name;


	-- Creates from two portlists a list of ports to be dragged:
	function make_drag_list (
		ports_old : in pac_symbol_ports.map;
		ports_new : in pac_symbol_ports.map)
		return pac_dragged_ports.map;


	-- When ports of a unit are dragged, then the sheet
	-- where all that takes place must also be provided:
	type type_port_drag_list is record
		sheet	: type_sheet;
		ports	: pac_dragged_ports.map;
	end record;


end et_units;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
