------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          DEVICE UNITS IN SCHEMATIC                       --
--                                                                          --
--                               S p e c                                    --
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
--  This package is about so called "units". A unit is a subset of an
--  electrical device. The blueprint of a unit is a symbol. As soon as
--  a symbol is instantiated in the schematic, it becomes a unit.
--
--   history of changes:
--
--   ToDo: 

with ada.containers;    		        use ada.containers;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;

with et_schematic_geometry;				use et_schematic_geometry;
with et_schematic_coordinates;			use et_schematic_coordinates;
with et_sheets;							use et_sheets;
with et_port_names;						use et_port_names;
with et_symbol_ports;					use et_symbol_ports;
with et_symbol_library;					use et_symbol_library;
with et_symbol_model;					use et_symbol_model;
with et_rotation_docu;					use et_rotation_docu;
with et_text;							use et_text;
with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.symbols;	use et_device_placeholders.symbols;

with et_unit_name;						use et_unit_name;
with et_mirroring;						use et_mirroring;
with et_device_model;					use et_device_model;
with et_device_appearance;				use et_device_appearance;
with et_object_status;					use et_object_status;


package et_units is

	use pac_unit_name;

	use pac_geometry_2;

	
	
	-- In a schematic we handle only virtual devices (like GND symbols)
	-- and those which appear in both schematic an layout (so called real devices):
	subtype type_appearance_schematic is type_appearance 
		range APPEARANCE_VIRTUAL .. APPEARANCE_PCB;

	
	-- In a schematic we find units spread all over.
	-- A unit is a subset of a device.
	-- Placeholders are available if the device appears in both schematic and layout:
	type type_unit (appearance : type_appearance_schematic) is record
		position	: type_object_position; -- incl. rotation and sheet number
		mirror		: type_mirror := MIRROR_NO; -- CS rename to mirror_status
		status		: type_object_status;
		case appearance is
			when APPEARANCE_VIRTUAL => 
				null; -- CS

			when APPEARANCE_PCB =>
				placeholders : type_text_placeholders;
		end case;
		-- The plaeholders have only x/y positions relative 
		-- to the unit position.
	end record;



	function is_real (
		unit : in type_unit)
		return boolean;

	
	

	function get_position (
		unit	: in type_unit)
		return type_object_position;


	procedure set_position (
		unit		: in out type_unit;
		position	: in type_object_position);



	function get_mirror_status (
		unit	: in type_unit)
		return type_mirror;


	procedure set_mirror_status (
		unit	: in out type_unit;
		mirror	: in type_mirror);

	
	
	function get_rotation (
		unit	: in type_unit)
		return type_rotation_model;


	-- Sets the absolute rotation of a unit:
	procedure set_rotation (
		unit		: in out type_unit;
		rotation	: in type_rotation_model);

	
	-- Rotates the unit by a given angle:
	procedure rotate_by (
		unit		: in out type_unit;
		rotation	: in type_rotation_model);

	
	
	function get_sheet (
		unit	: in type_unit)
		return type_sheet;


	procedure set_sheet (
		unit	: in out type_unit;
		sheet	: in type_sheet);
	
	

	-- Returns true if the given unit is in
	-- the given catch zone:
	function in_catch_zone (
		unit	: in type_unit;
		zone	: in type_catch_zone;
		sheet	: in type_sheet)
		return boolean;
		
	

	procedure unit_not_found (
		name : in pac_unit_name.bounded_string);
	
	


	procedure set_selected (
		unit : in out type_unit);
	

	procedure clear_selected (
		unit : in out type_unit);
	

	function is_selected (
		unit : in type_unit)
		return boolean;
	

	
	procedure set_proposed (
		unit : in out type_unit);
	

	procedure clear_proposed (
		unit : in out type_unit);

	
	function is_proposed (
		unit : in type_unit)
		return boolean;



	
	procedure set_moving (
		unit : in out type_unit);
	

	procedure clear_moving (
		unit : in out type_unit);

	
	function is_moving (
		unit : in type_unit)
		return boolean;

	

	
	procedure modify_status (
		unit 		: in out type_unit;
		operation	: in type_status_operation);
	


	procedure reset_status (
		unit : in out type_unit);
	

	

-- PLACEHOLDERS:
	
	-- Returns the placeholders of the unit.
	-- If the unit is virtual, then default placeholders
	-- are returned:
	function get_placeholders (
		unit : in type_unit)
		return type_text_placeholders;
	
								  
	-- Moves the placeholder given by meaning.
	-- If coordinates is absolute, then the placeholder
	-- is moved to the given point.
	-- If coordinates is relative, then the placeholder
	-- is moved by the x/y-distance given by point:
	procedure move_placeholder (
		unit		: in out type_unit;
		meaning		: in type_placeholder_meaning;					 
		coordinates	: in type_coordinates; -- relative/absolute
		point		: in type_vector_model); -- x/y


	
	-- Rotates the placeholder given by meaning.
	-- If toggle is true, then the rotation toggles between
	-- horizonal and vertical. Otherwise, the rotation is
	-- set as given by rotation:
	procedure rotate_placeholder (
		unit		: in out type_unit;
		meaning		: in type_placeholder_meaning;					 
		toggle		: in boolean;
		rotation	: in type_rotation_documentation);

	
	
	-- Units of a device are collected in a map.
	-- A unit is accessed by its name like "I/O Bank 3" or "PWR" or "A" or "B" ...	
	package pac_units is new indefinite_ordered_maps (
		key_type		=> pac_unit_name.bounded_string,
		element_type 	=> type_unit);

	use pac_units;



	function get_position (
		unit	: in pac_units.cursor)
		return type_object_position;


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
		return pac_unit_name.bounded_string;

	

	-- Returns the name of a unit as a string:
	function get_unit_name (
		unit : in pac_units.cursor)
		return string;
	
	
	-- Returns a string that tells the name and position of given unit.
	function to_string (unit : in pac_units.cursor) return string;
	


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
		key_type		=> pac_unit_name.bounded_string, -- A, B, IO_BANK_1
		element_type	=> type_object_position); -- sheet, x, y


	
	-- Extracts from the given list of object positions a list
	-- of sheet numbers. A sheet number occurs only once
	-- in the returned list:
	function extract_sheets (
		positions	: in pac_unit_positions.map)
		return pac_sheet_numbers.list;

	
	
	-- Returns a list of units and their 
	-- coordinates in the schematic:
	function get_unit_positions (
		units : in pac_units.map) 
		return pac_unit_positions.map;



	use pac_port_name;

	-- When units are dragged about the sheet then connected
	-- net segments must be dragged along.
	-- So a list of port names with their old an new positions
	-- is required.
	-- The list tells which port is to be moved from an
	-- old to a new position:
	package pac_dragged_ports is new ordered_maps (
		key_type		=> pac_port_name.bounded_string,
		element_type	=> type_drag);

	use pac_dragged_ports;

	
	function get_port_name (
		port : in pac_dragged_ports.cursor)
		return pac_port_name.bounded_string;
	
	
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
