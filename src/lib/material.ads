------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             MATERIAL                                     --
--                                                                          --
--                              S p e c                                     --
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
with ada.containers.vectors;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_general;				use et_general;
with et_string_processing;		use et_string_processing;
with et_devices;				use et_devices;
with et_packages;

package material is


	-- The part code is THE key into the ERP system of the user. It can be a cryptic SAP number
	-- or something human readable like "R_PAC_S_0805_VAL_100R_PMAX_125_TOL_5".
	-- The keywords for the latter can be specified via the conventions file. See package "convention".
	partcode_characters : character_set := to_set
		(ranges => (('a','z'),('A','Z'),('0','9'))) or to_set ('_'); 
	partcode_length_max : constant positive := 100;
	package type_partcode is new generic_bounded_length (partcode_length_max);
	partcode_default : constant string := "dummy";
	
	function to_string (partcode : in type_partcode.bounded_string) return string;

	function partcode_length_valid (partcode : in string) return boolean;
	-- Returns true if length of given partcode is ok. Issues warning if not.
	
	function partcode_characters_valid (
		partcode	: in type_partcode.bounded_string;
		characters	: in character_set := partcode_characters) return boolean;
	-- Tests if the given partcode contains only valid characters as specified
	-- by given character set. Returns false if not. Issues warning.

	procedure partcode_invalid (partcode : in string);
	-- Issues error message and raises constraint error.
	
	function to_partcode (
	-- Tests the given value for length and invalid characters.							 
		partcode 					: in string;
		error_on_invalid_character	: in boolean := true) 
		return type_partcode.bounded_string;



	
	-- Whenever we deal with BOM files this type should be used:
	file_name_length_max : constant positive := 100; -- CS: should suffice for now
	package type_file_name is new generic_bounded_length (file_name_length_max); 

	extension_bom : constant string := "csv";

	function to_string (name : in type_file_name.bounded_string) return string;
	function to_file_name (name : in string) return type_file_name.bounded_string;

	type type_device is record
		value		: type_value.bounded_string;	-- 7400
		packge		: et_packages.type_package_model_file.bounded_string; -- libraries/packages/smd/SOT23.pac
		partcode	: type_partcode.bounded_string; -- IC_PAC_S_SO16_VAL7400
		purpose		: type_purpose.bounded_string; 	-- brightness_control
	end record;

	package type_devices is new ordered_maps (
		key_type		=> type_name, -- IC4
		element_type	=> type_device);

	type type_bom_format is (
		NATIVE,
		EAGLE,
		KICAD
		-- CS others ?
		);		
	
	procedure write_bom (
	-- Creates the BOM file (which inevitably and intentionally overwrites the previous file).
	-- Writes the content of the given container bom in the file.
	-- - The BOM file will be named after the module name and the assembly variant.
	-- - Exports the BOM of the given module to the export/CAM directory.
		bom				: in type_devices.map;
		module_name		: in type_module_name.bounded_string; -- motor_driver 
		variant_name	: in type_variant_name.bounded_string; -- low_cost
		format			: in type_bom_format;
		log_threshold	: in type_log_level);
	
end material;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
