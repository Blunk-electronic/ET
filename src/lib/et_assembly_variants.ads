------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          ASSEMBLY VARIANTS                               --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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

with ada.text_io;				use ada.text_io;
with ada.strings;				use ada.strings;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.strings.unbounded;     use ada.strings.unbounded;

with ada.containers;            use ada.containers;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;

with et_general;				use et_general;
--with et_material;
with et_string_processing;		use et_string_processing;
with et_devices;				use et_devices;
with et_logging;				use et_logging;

package et_assembly_variants is

	use pac_module_instance_name;

	-- The name of an assembly variant is a text like "low_cost" or "with temperature sensor" or just a number like V345:
	variant_name_length_max : constant positive := 100;
	package pac_assembly_variant_name is new generic_bounded_length (variant_name_length_max);
	use pac_assembly_variant_name;

	default : constant pac_assembly_variant_name.bounded_string := pac_assembly_variant_name.to_bounded_string ("");
	
	function is_default (variant : in pac_assembly_variant_name.bounded_string) return boolean;
	-- Returns true if the given variant name is empty.
	
	function to_variant (variant : in pac_assembly_variant_name.bounded_string) return string;
	function to_variant (variant : in string) return pac_assembly_variant_name.bounded_string;

	
	
	keyword_active	: constant string := "active";	
	
	-- An assembly variant should be described more or less detailled by the operator:
	type type_description is new unbounded_string;



	-- The part code is THE key into the ERP system of the user. It can be a cryptic SAP number
	-- or something human readable like "R_PAC_S_0805_VAL_100R_PMAX_125_TOL_5".
	-- The keywords for the latter can be specified via the conventions file. See package "convention".
	keyword_partcode : constant string := "partcode";	

	partcode_characters : character_set := to_set
		(ranges => (('a','z'),('A','Z'),('0','9'))) or to_set ("_/"); 
	partcode_length_max : constant positive := 100;
	package type_partcode is new generic_bounded_length (partcode_length_max); -- CS rename to pac_device_partcode
	partcode_default : constant string := "N/A"; -- means not assigned
	
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

	function is_empty (partcode : in type_partcode.bounded_string) return boolean;
	
	function to_partcode (
	-- Tests the given value for length and invalid characters.							 
		partcode 					: in string;
		error_on_invalid_character	: in boolean := true) 
		return type_partcode.bounded_string;



	
	

	
	type type_mounted is (YES, NO);

	function to_mounted (mounted : in string) return type_mounted;
	function to_mounted (mounted : in type_mounted) return string;

	-- If a device is mounted, then it has a value, partcode and purpose
	-- that overrides those specified in the default assembly variant.
	type type_device (mounted : type_mounted) is record
		case mounted is
			when YES =>
				value		: pac_device_value.bounded_string; -- 470R
				partcode	: type_partcode.bounded_string;
				purpose		: pac_device_purpose.bounded_string;

			when NO =>
				null;
		end case;
	end record;

	-- Variants of devices are collected in a map.
	package pac_device_variants is new indefinite_ordered_maps (
		key_type		=> type_device_name, -- something like "IC43"
 		element_type	=> type_device);

	use pac_device_variants;

	-- Submodules may come with their own assembly variants. 
	-- NOTE: In contrast to type_device there is no option to not mount a submodule.
	-- There might be furhter extensions in the future, so we use a record:
	type type_submodule is record -- CS rename to type_submodule_variant ?
		variant : pac_assembly_variant_name.bounded_string; -- low_cost, fixed_frequency
	end record;

	-- Variants of submodules are collected in a map.	
	package pac_submodule_variants is new ordered_maps (
		key_type		=> pac_module_instance_name.bounded_string, -- MOT_DRV_3
		element_type	=> type_submodule);

	-- The final assembly variant is composed of a description and the affected devices:
	type type_assembly_variant is record
		description	: type_description;
		devices		: pac_device_variants.map;
		submodules	: pac_submodule_variants.map;
	end record;

	-- Since a board may have lots of variants, we keep them in a map.
	-- NOTE: The default variant ("") is never inserted here.
	package pac_assembly_variants is new ordered_maps (
		key_type		=> pac_assembly_variant_name.bounded_string, -- "low_cost"
		element_type	=> type_assembly_variant);

	use pac_assembly_variants;
	
	
	-- Returns true if the given device is to be mounted according to given assembly variant.
	-- If variant points to no element the default variant is assumed and the device regarded as mounted.
	function is_mounted (
		device	: in type_device_name; -- IC1
		variant	: in pac_assembly_variants.cursor)
		return boolean;
	
end et_assembly_variants;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
