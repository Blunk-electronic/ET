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
with et_schematic;				--use et_schematic;
with et_string_processing;		use et_string_processing;
-- with schematic_ops;
-- with board_ops;

package assembly_variants is
	
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
				purpose		: et_schematic.type_device_purpose.bounded_string;

			when NO =>
				null;
		end case;
	end record;

	-- Variants for devices are collected in a map.
	package type_devices is new indefinite_ordered_maps (
		key_type		=> et_libraries.type_device_name, -- something like "IC43"
		"<"				=> et_schematic.compare_reference,
 		element_type	=> type_device);

	use type_devices;

	-- An assembly variant should be described more or less detailled by the operator:
	type type_description is new unbounded_string;

	-- The final assembly variant is composed of a description and the affected devices:
	type type_variant is record
		description	: type_description;
		devices		: type_devices.map;
	end record;
	
	-- The name of an assembly variant is a text like "low cost" or "with temperature sensor":
	variant_name_length_max : constant positive := 100;
	package type_variant_name is new generic_bounded_length (variant_name_length_max);
	use type_variant_name;

	function to_variant (variant : in type_variant_name.bounded_string) return string;
	function to_variant (variant : in string) return type_variant_name.bounded_string;

	-- Since a board may have lots of variants, we keep them in a map:
	package type_variants is new ordered_maps (
		key_type		=> type_variant_name.bounded_string, -- "low cost"
		element_type	=> type_variant);
	
end assembly_variants;

-- Soli Deo Gloria
