------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          ASSEMBLY VARIANTS                               --
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


with ada.strings.unbounded;     use ada.strings.unbounded;

with ada.containers;            use ada.containers;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;

with et_module_instance;		use et_module_instance;
with et_device_purpose;			use et_device_purpose;
with et_device_value;			use et_device_value;
with et_device_partcode;		use et_device_partcode;
with et_device_name;			use et_device_name;
with et_assembly_variant_name;	use et_assembly_variant_name;


package et_assembly_variants is

	use pac_module_instance_name;

	use pac_assembly_variant_name;

	
	
	keyword_active	: constant string := "active";	
	
	-- An assembly variant should be described more or less detailled by the operator:
	type type_description is new unbounded_string;

	

	
	type type_mounted is (YES, NO);

	function to_mounted (mounted : in string) return type_mounted;
	function to_mounted (mounted : in type_mounted) return string;

	-- If a device is mounted, then it has a value, partcode and purpose
	-- that overrides those specified in the default assembly variant.
	type type_device_variant (mounted : type_mounted) is record
		case mounted is
			when YES =>
				value		: pac_device_value.bounded_string; -- 470R
				partcode	: pac_device_partcode.bounded_string;
				purpose		: pac_device_purpose.bounded_string;

			when NO =>
				null;
		end case;
	end record;

	

	-- Variants of devices are collected in a map.
	package pac_device_variants is new indefinite_ordered_maps (
		key_type		=> type_device_name, -- something like "IC43"
 		element_type	=> type_device_variant);

	use pac_device_variants;

	
	
	-- Submodules may come with their own assembly variants. 
	-- NOTE: In contrast to a device, there is no option not to mount a submodule.
	-- There might be further extensions in the future, so we use a record:
	type type_submodule_variant is record
		variant : pac_assembly_variant_name.bounded_string; -- low_cost, fixed_frequency
	end record;

	
	
	-- Variants of submodules are collected in a map.	
	package pac_submodule_variants is new ordered_maps (
		key_type		=> pac_module_instance_name.bounded_string, -- MOT_DRV_3
		element_type	=> type_submodule_variant);


	
	
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
