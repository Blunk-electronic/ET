------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             NUMBERING                                    --
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

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo: 

with ada.text_io;				use ada.text_io;
with ada.containers;            use ada.containers;
with ada.containers.ordered_maps;
with ada.containers.multiway_trees;

with et_general;				use et_general;
with et_module_names;			use et_module_names;
with et_coordinates_2;			use et_coordinates_2;
with et_string_processing;		use et_string_processing;

with et_devices;				use et_devices;


package et_numbering is

	type type_device is record
		name	: type_device_name; -- R56, IC4
		unit	: pac_unit_name.bounded_string; -- 1, A, B, ...
		done	: boolean := false; -- indicates whether the device has been renumbered
	end record;


	package pac_devices is new ordered_maps (
		key_type		=> type_position, -- sheet/x/y
		element_type	=> type_device);

	type type_index_range is record
		lowest	: type_name_index := type_name_index'last; -- "last" is not a bug
		highest	: type_name_index := type_name_index'first; -- "first" is not a bug	
	end record;

	function to_index_range (
	-- Returns a string like "module 'templates/clock_generator' range 78 .. 133"
		module_name	: in pac_module_name.bounded_string;
		index_range	: in type_index_range) return string;

	function below (left, right : in type_index_range) return boolean;
	-- Returns true if left index range is below right index range.
		
	function above (left, right : in type_index_range) return boolean;
	-- Returns true if left index range is above right index range.		
	
	type type_module is record
		name				: pac_module_name.bounded_string; -- amplifier, $ET_TEMPLATES/motor_driver
		instance			: pac_module_instance_name.bounded_string; -- AMP_2, DRV1
		device_names_offset	: type_name_index := type_name_index'first;	-- R88 turns to R1088
	end record;

	function "<" (left, right : in type_module) return boolean;


	package pac_modules is new multiway_trees (type_module);

	
end et_numbering;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
