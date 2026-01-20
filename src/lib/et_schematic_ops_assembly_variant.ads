------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   SCHEMATIC OPERATIONS / ASSEMBLY VARIANT                --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
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


with ada.exceptions;			use ada.exceptions;

with et_module_names;			use et_module_names;

with et_logging;				use et_logging;

with et_generic_modules;		use et_generic_modules;

with et_assembly_variants;		use et_assembly_variants;
with et_assembly_variant_name;	use et_assembly_variant_name;
with et_material;

with et_device_purpose;			use et_device_purpose;
with et_device_value;			use et_device_value;
with et_device_name;			use et_device_name;
with et_device_partcode;		use et_device_partcode;

with et_logging;				use et_logging;


package et_schematic_ops_assembly_variant is

	use pac_generic_modules;


	-- CS rework procedures so that a module cursor
	-- is used instead the module_name.
	

	
	
	procedure device_not_found (name : in type_device_name);
	procedure device_already_exists (name : in type_device_name);
	procedure assembly_variant_not_found (variant : in pac_assembly_variant_name.bounded_string);





	-- Returns the name of the active assembly variant of the given module:
	function get_active_assembly_variant (
		module_cursor	: in pac_generic_modules.cursor)
		return pac_assembly_variant_name.bounded_string;

	
	-- Returns a cursor to the active assembly variant of the given module:	
	function get_active_assembly_variant (
		module_cursor	: in pac_generic_modules.cursor)
		return et_assembly_variants.pac_assembly_variants.cursor;


	

	
	-- Creates a new assembly variant.
	procedure create_assembly_variant (
		module_name		: in pac_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in pac_assembly_variant_name.bounded_string; -- low_cost
		log_threshold	: in type_log_level);

	
	-- Deletes an assembly variant.
	procedure delete_assembly_variant (
		module_name		: in pac_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in pac_assembly_variant_name.bounded_string; -- low_cost
		log_threshold	: in type_log_level);

	
	-- Describes an assembly variant. Overwrites the previous description.
	procedure describe_assembly_variant (
		module_name		: in pac_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in pac_assembly_variant_name.bounded_string; -- low_cost
		description		: in et_assembly_variants.type_description; -- "this is the low budget variant"
		log_threshold	: in type_log_level);

	



	-- Returns true if the given module and assembly variant 
	-- provides the given device.
	-- Assumptions: 
	-- - The module being searched in must be in the rig already.
	-- - The assembly variant must exist in the module.
	-- - The device must exist in the module.
	function device_exists (
		module	: in pac_generic_modules.cursor; -- the module like motor_driver
		variant	: in pac_assembly_variant_name.bounded_string; -- low_cost				
		device	: in type_device_name)
		return boolean;




	-- Returns a cursor to the alternative device in the given module
	-- and given assembly variant.
	-- Assumptions: 
	-- - The module being searched in must be in the rig already.
	-- - The assembly variant must exist in the module.
	-- - The device must exist in the module.
	-- - The device must have an entry in the given assembly variant,
	--   otherwise the return is no_element.
	function get_alternative_device (
		module	: in pac_generic_modules.cursor; -- the module like motor_driver
		variant	: in pac_assembly_variant_name.bounded_string; -- low_cost				
		device	: in type_device_name)
		return pac_device_variants.cursor;




	-- Sets the value, partcode and (optionally the purpose) of a device in 
	-- An already existing device will be overwritten without warning.
	procedure mount_device (
		module_name		: in pac_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in pac_assembly_variant_name.bounded_string; -- low_cost
		device			: in type_device_name; -- R1
		value			: in pac_device_value.bounded_string; -- 220R
		partcode		: in pac_device_partcode.bounded_string; -- R_PAC_S_0805_VAL_220R
		purpose			: in pac_device_purpose.bounded_string := pac_device_purpose.to_bounded_string (""); -- set temperature
		log_threshold	: in type_log_level);

	
	-- Sets the given device as not mounted in the given assembly variant.
	-- Sets the gvien device as not mounted in 
	-- the given assembly variant. An already existing device will be overwritten
	-- without warning.
	procedure unmount_device (
		module_name		: in pac_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in pac_assembly_variant_name.bounded_string; -- low_cost
		device			: in type_device_name; -- R1
		log_threshold	: in type_log_level);

	
	-- Removes the given device from the given assembly variant.
	procedure remove_device (
		module_name		: in pac_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in pac_assembly_variant_name.bounded_string; -- low_cost
		device			: in type_device_name; -- R1
		log_threshold	: in type_log_level);


	
end et_schematic_ops_assembly_variant;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
