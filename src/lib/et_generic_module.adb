------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           GENERIC MODULE                                 --
--                                                                          --
--                               B o d y                                    --
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
--  ToDo: 
--  


with et_string_processing;		use et_string_processing;

with et_meta;
with et_exceptions;				use et_exceptions;


package body et_generic_module is


	function get_count (
		modules : in pac_generic_modules.map)
		return natural
	is begin
		return natural (length (modules));
	end get_count;


	
	function get_active_module return string is
		use pac_module_name;
	begin
		return pac_module_name.to_string (key (active_module)); -- motor_driver (without extension)
	end get_active_module;


	
	function generic_module_exists (
		module : in pac_module_name.bounded_string) 
		return boolean
	is begin
		return pac_generic_modules.contains (generic_modules, module);
	end;


	
	function locate_module (name : in pac_module_name.bounded_string) -- motor_driver (without extension *.mod)
		return pac_generic_modules.cursor 
	is
		cursor : pac_generic_modules.cursor := find (generic_modules, name);
	begin
		if cursor = pac_generic_modules.no_element then
			raise semantic_error_1 with
				"ERROR: Module " & enclose_in_quotes (to_string (name)) 
				& " does not exist !";
		else
			return find (generic_modules, name);
		end if;
	end locate_module;



	function get_meta_information (
		module : in pac_generic_modules.cursor)
		return et_meta.type_meta
	is begin
		return element (module).meta;
	end get_meta_information;
	

	



	function assembly_variant_exists (
		module		: in pac_generic_modules.cursor;
		variant		: in pac_assembly_variant_name.bounded_string) -- low_cost
		return boolean 
	is
		use pac_assembly_variants;

		result : boolean := false; -- to be returned

		procedure query_variants (
			module_name	: in pac_module_name.bounded_string;
			module		: in et_schematic.type_module) is
		begin
			result := contains (module.variants, variant);
		end;
		
	begin
		if pac_assembly_variant_name.length (variant) = 0 then
			result := true;
		else
			
			pac_generic_modules.query_element (
				position	=> module,
				process		=> query_variants'access);

		end if;
					
		return result;
	end assembly_variant_exists;

	
end et_generic_module;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
