------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          ASSEMBLY VARIANTS                               --
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

with et_general;
with et_material;
with et_string_processing;		use et_string_processing;
with et_devices;				use et_devices;

package et_assembly_variants is

	keyword_active	: constant string := "active";	
	
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
				value		: pac_device_value.bounded_string; -- 470R
				partcode	: et_material.type_partcode.bounded_string;
				purpose		: pac_device_purpose.bounded_string;

			when NO =>
				null;
		end case;
	end record;

	-- Variants of devices are collected in a map.
	package type_devices is new indefinite_ordered_maps (
		key_type		=> type_device_name, -- something like "IC43"
 		element_type	=> type_device);

	use type_devices;

	-- Submodules may come with their own assembly variants. 
	-- NOTE: In contrast to type_device there is no option to not mount a submodule.
	-- There might be furhter extensions in the future, so we use a record:
	type type_submodule is record
		variant : et_general.pac_assembly_variant_name.bounded_string; -- low_cost, fixed_frequency
	end record;

	-- Variants of submodules are collected in a map.	
	package type_submodules is new ordered_maps (
		key_type		=> et_general.type_module_instance_name.bounded_string, -- MOT_DRV_3
		"<" 			=> et_general.type_module_instance_name."<",
		element_type	=> type_submodule);

	-- The final assembly variant is composed of a description and the affected devices:
	type type_assembly_variant is record
		description	: type_description;
		devices		: type_devices.map;
		submodules	: type_submodules.map;
	end record;

	-- Since a board may have lots of variants, we keep them in a map.
	-- NOTE: The default variant ("") is never inserted here.
	package pac_variants is new ordered_maps (
		key_type		=> et_general.pac_assembly_variant_name.bounded_string, -- "low_cost"
		"<"				=> et_general.pac_assembly_variant_name."<",
		element_type	=> type_assembly_variant);

	function is_mounted (
		device	: in type_device_name; -- IC1
		variant	: in pac_variants.cursor)
		return boolean;
	-- Returns true if the given device is to be mounted according to given assembly variant.
	-- If variant points to no element the default variant is assumed and the device regarded as mounted.
	
end et_assembly_variants;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
