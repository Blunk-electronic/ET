------------------------------------------------------------------------------
--                                                                          --
--                       SYSTEM ET ASSEMBLY VARIANTS                        --
--                                                                          --
--                                 ET                                       --
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
with ada.characters.latin_1;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.strings.unbounded;     use ada.strings.unbounded;

with ada.containers;            use ada.containers;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;

with et_general;
with et_libraries;				--use et_libraries;
with et_string_processing;		use et_string_processing;

package assembly_variants is

	-- The name of an assembly variant is a text like "low_cost" or "with temperature sensor" or just a number like V345:
	variant_name_length_max : constant positive := 100;
	package type_variant_name is new generic_bounded_length (variant_name_length_max);
	use type_variant_name;

	function to_variant (variant : in type_variant_name.bounded_string) return string;
	function to_variant (variant : in string) return type_variant_name.bounded_string;

	-- An assembly variant should be described more or less detailled by the operator:
	type type_description is new unbounded_string;
	
	type type_mounted is (YES, NO);

	function to_mounted (mounted : in string) return type_mounted;
	function to_mounted (mounted : in type_mounted) return string;

	-- If a device is mounted, then it has a value, partcode and purpose
	-- that owerrides those specified in the default assembly variant.
	-- The default value, partcode and purpose is specified in et_schematic.type_device.
	type type_device (mounted : type_mounted) is record
		case mounted is
			when YES =>
				value		: et_libraries.type_value.bounded_string; -- 470R
				partcode	: et_libraries.type_partcode.bounded_string;
				purpose		: et_libraries.type_device_purpose.bounded_string;

			when NO =>
				null;
		end case;
	end record;

	-- Variants of devices are collected in a map.
	package type_devices is new indefinite_ordered_maps (
		key_type		=> et_libraries.type_device_name, -- something like "IC43"
		"<"				=> et_libraries.compare_name,
 		element_type	=> type_device);

	use type_devices;

	-- Submodules may come with their own assembly variants. 
	-- NOTE: In contrast to type_device there is no option to not mount a submodule.
	-- There might be furhter extensions in the future, so we use a record:
	type type_submodule is record
		variant : type_variant_name.bounded_string; -- low_cost, fixed_frequency
	end record;

	-- Variants of submodules are collected in a map.	
	package type_submodules is new ordered_maps (
		key_type		=> et_general.type_module_instance_name.bounded_string, -- MOT_DRV_3
		"<" 			=> et_general.type_module_instance_name."<",
		element_type	=> type_submodule);

	-- The final assembly variant is composed of a description and the affected devices:
	type type_variant is record
		description	: type_description;
		devices		: type_devices.map;
		--submodules	: type_submodules.map;
	end record;

	-- Since a board may have lots of variants, we keep them in a map:
	package type_variants is new ordered_maps (
		key_type		=> type_variant_name.bounded_string, -- "low_cost"
		element_type	=> type_variant);
	
end assembly_variants;

-- Soli Deo Gloria
