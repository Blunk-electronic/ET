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
--   history of changes:
--
--   ToDo: 

with ada.containers;    		        use ada.containers;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;

with et_schematic_coordinates;			use et_schematic_coordinates;
with et_sheets;							use et_sheets;
with et_device_placeholders.symbols;	use et_device_placeholders.symbols;

with et_unit_name;						use et_unit_name;
with et_mirroring;						use et_mirroring;
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
		mirror		: type_mirror := MIRROR_NO;
		status		: type_object_status;
		case appearance is
			when APPEARANCE_VIRTUAL => null; -- CS
			when APPEARANCE_PCB =>
				name	: type_text_placeholder (meaning => et_device_placeholders.NAME);
				value	: type_text_placeholder (meaning => et_device_placeholders.VALUE);
				purpose	: type_text_placeholder (meaning => et_device_placeholders.PURPOSE); -- to be filled in schematic later by the user
		end case;
		-- NOTE: The placeholders are defined in et_symbols. Thus they have only
		-- basic coordinates (x/y relative to the unit position).
		-- Via the unit position the sheet number can be obtained.
	end record;



	function get_position (
		unit	: in type_unit)
		return type_object_position;


	procedure set_position (
		unit		: in out type_unit;
		position	: in type_object_position);
	

	
	function get_rotation (
		unit	: in type_unit)
		return type_rotation_model;


	procedure set_rotation (
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
	

	
	-- Units of a device are collected in a map.
	-- A unit is accessed by its name like "I/O Bank 3" or "PWR" or "A" or "B" ...	
	package pac_units is new indefinite_ordered_maps (
		key_type		=> pac_unit_name.bounded_string,
		element_type 	=> type_unit);

	use pac_units;


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


	
	--Returns a list of units and their coordinates in the schematic.	
	function unit_positions (units : in pac_units.map) return pac_unit_positions.map;





	
end et_units;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
