------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET KICAD GENERAL                        --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2018 Mario Blunk, Blunk electronic                 --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.text_io;				use ada.text_io;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.ordered_sets;
with ada.containers.indefinite_ordered_maps;
with ada.containers.vectors;

with et_libraries;

package et_kicad_general is

	system_name	: constant string (1..5) := "KiCad";
	
	-- TIMESTAMP
	timestamp_characters : character_set := to_set (ranges => (('A','F'),('0','9'))); -- CS: upper case letters only	
	type type_timestamp is new string (1..8); -- like "3459A3C1"
	timestamp_default : type_timestamp := "00000000";
	procedure check_timestamp (timestamp : in type_timestamp);
	-- Checks the given timestamp for valid characters and plausible time.

-- LIBRARIES
	-- To handle library paths we (mis)use type_device_model_file 
	-- and type_package_model_file under different names:
	package type_package_library_name renames et_libraries.type_package_model_file;
	package type_device_library_name renames et_libraries.type_device_model_file;

	library_name_length_max : constant natural := 100; -- CS: increase if necessary
	
	-- For storing BARE library names like "bel_primitives" we use this bounded string:
	package type_library_name is new generic_bounded_length (library_name_length_max); 
	use type_library_name;

	function to_library_name (library_name : in string) return type_library_name.bounded_string;
	-- converts a string to a type_library_name
	
	-- CS: for type_library_name: character set, check characters, check length
	
	function to_string (library_name : in type_library_name.bounded_string) return string;
	-- Returns the given library name as string.

end et_kicad_general;

-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
