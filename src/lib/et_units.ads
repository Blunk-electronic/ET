------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                UNITS                                     --
--                                                                          --
--                               S p e c                                    --
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

with et_coordinates_2;					use et_coordinates_2;
with et_device_placeholders.symbols;	use et_device_placeholders.symbols;

with et_symbols;						use et_symbols;
with et_devices;						use et_devices;


package et_units is

	use pac_unit_name;

	use pac_geometry_2;

	
	
	
	-- Units can be placed mirrored along the x or y axis or not at all.
	type type_mirror is (NO, X_AXIS, Y_AXIS);

	
	function to_string (
		mirror	: in type_mirror;
		verbose	: in boolean)
		return string;
	-- returns the given mirror style as string

	function to_mirror_style (style : in string) return type_mirror;

	
	
	-- In a schematic we handle only virtual devices (like GND symbols)
	-- and those which appear in both schematic an layout (so called real devices):
	subtype type_appearance_schematic is et_symbols.type_appearance 
		range et_symbols.VIRTUAL .. et_symbols.PCB;

	
	-- In a schematic we find units spread all over.
	-- A unit is a subset of a device.
	-- Placeholders are available if the device appears in both schematic and layout:
	type type_unit (appearance : type_appearance_schematic) is record
		position	: et_coordinates_2.type_position; -- incl. rotation and sheet number
		mirror		: type_mirror := NO;
		case appearance is
			when et_symbols.VIRTUAL => null; -- CS
			when et_symbols.PCB =>
				name	: type_text_placeholder (meaning => et_device_placeholders.NAME);
				value	: type_text_placeholder (meaning => et_device_placeholders.VALUE);
				purpose	: type_text_placeholder (meaning => et_device_placeholders.PURPOSE); -- to be filled in schematic later by the user
		end case;
		-- NOTE: The placeholders are defined in et_symbols. Thus they have only
		-- basic coordinates (x/y relative to the unit position).
		-- Via the unit position the sheet number can be obtained.
	end record;

	
	-- Units of a device are collected in a map.
	-- A unit is accessed by its name like "I/O Bank 3" or "PWR" or "A" or "B" ...	
	package pac_units is new indefinite_ordered_maps (
		key_type		=> pac_unit_name.bounded_string,
		element_type 	=> type_unit);

	
	-- Returns a string that tells the name and position of given unit.
	function to_string (unit : in pac_units.cursor) return string;
	

	package pac_unit_positions is new ordered_maps (
		key_type		=> pac_unit_name.bounded_string, -- A, B, IO_BANK_1
		element_type	=> et_coordinates_2.type_position); -- sheet, x, y


	
	--Returns a list of units and their coordinates in the schematic.	
	function unit_positions (units : in pac_units.map) return pac_unit_positions.map;

	
end et_units;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16