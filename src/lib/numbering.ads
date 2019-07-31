------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             NUMBERING                                    --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers;            use ada.containers;
-- with ada.containers.vectors;
with ada.containers.doubly_linked_lists;
-- with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.multiway_trees;
-- with ada.containers.indefinite_ordered_maps;
-- with ada.containers.ordered_sets;

with et_general;				use et_general;

with et_coordinates;
with et_libraries;				use et_libraries;
with et_string_processing;		use et_string_processing;

package numbering is

	procedure dummy;
	
	type type_device is record
		name	: type_device_name; -- R56, IC4
		unit	: type_unit_name.bounded_string; -- 1, A, B, ...
		done	: boolean := false; -- indicates whether the device has been renumbered
	end record;


	package type_devices is new ordered_maps (
		key_type		=> et_coordinates.type_coordinates, -- sheet/x/y
		"<"				=> et_coordinates."<",
		element_type	=> type_device);

	type type_index_range is record
		lowest	: et_libraries.type_device_name_index := et_libraries.type_device_name_index'last; -- "last" is not a bug
		highest	: et_libraries.type_device_name_index := et_libraries.type_device_name_index'first; -- "first" is not a bug	
	end record;

	function to_index_range (
	-- Returns a string like "module 'templates/clock_generator' range 78 .. 133"
		module_name	: in type_module_name.bounded_string;
		index_range	: in type_index_range) return string;

	function below (left, right : in type_index_range) return boolean;
	-- Returns true if left index range is below right index range.
		
	function above (left, right : in type_index_range) return boolean;
	-- Returns true if left index range is above right index range.		
	
	type type_module is record
		name				: type_module_name.bounded_string; -- amplifier, $ET_TEMPLATES/motor_driver
		instance			: type_module_instance_name.bounded_string; -- AMP_2, DRV1
		device_names_offset	: et_libraries.type_device_name_index := et_libraries.type_device_name_index'first;	-- R88 turns to R1088
	end record;

	function "<" (left, right : in type_module) return boolean;


	package type_modules is new multiway_trees (type_module);

	
end numbering;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
