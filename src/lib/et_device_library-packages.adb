------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       DEVICE LIBRARY PACKAGES                            --
--                                                                          --
--                              B o d y                                     --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.text_io;				use ada.text_io;

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;

with et_string_processing;		use et_string_processing;
with et_exceptions;				use et_exceptions;

with et_package_library;


package body et_device_library.packages is

	

	function get_package_variant (
		device_cursor	: in pac_device_models.cursor;
		variant			: in pac_package_variant_name.bounded_string)
		return pac_package_variants.cursor
	is 
		result : pac_package_variants.cursor;

		procedure query_variants (
			device_name	: in pac_device_model_file.bounded_string;
			device		: in type_device_model) 
		is 
			use pac_package_variants;
			--vc : constant pac_package_variants.cursor := find (device.variants, variant);
		begin
			result := find (device.variants, variant);
			--if vc /= pac_package_variants.no_element then
				--result := vc;
			--else
				--raise semantic_error_1 with "Package variant " 
					--& enclose_in_quotes (to_string (variant)) &
					--" not defined."; 
					---- CS output model name. Mind max. string length of error message.
			--end if;
		end query_variants;
		
	begin
		query_element (device_cursor, query_variants'access);
		return result;
	end get_package_variant;


	
	
	
	function is_variant_available (
		device_cursor	: in pac_device_models.cursor;
		variant			: in pac_package_variant_name.bounded_string)  -- D, N
		return boolean is
		
		result : boolean := false; -- to be returned
		
		procedure query_variants (
			device_name	: in pac_device_model_file.bounded_string;
			device		: in type_device_model) is
		begin
			if pac_package_variants.contains (device.variants, variant) then
				result := true;
			end if;
		end;
		
	begin
		pac_device_models.query_element (
			position	=> device_cursor,
			process		=> query_variants'access);
		
		return result;
	end is_variant_available;


	

	function get_available_variants (
		device_cursor	: in pac_device_models.cursor)
		return pac_package_variants.map
	is
		result : pac_package_variants.map; -- to be returned
	begin
		case element (device_cursor).appearance is
			when APPEARANCE_PCB		=> result := element (device_cursor).variants;
			when APPEARANCE_VIRTUAL	=> null;
		end case;
		
		return result;
	end get_available_variants;



	


	function get_first_package_variant (
		device_cursor : in pac_device_models.cursor)
		return pac_package_variant_name.bounded_string
	is
		device_model : type_device_model renames element (device_cursor);
	begin
		return get_first_package_variant (device_model);
	end;






	
	
	function get_package_model (
		device_cursor	: in pac_device_models.cursor;
		variant			: in pac_package_variant_name.bounded_string)
		return pac_package_model_file.bounded_string 
	is
		package_model : pac_package_model_file.bounded_string; -- to be returned (packages/smd/SOT23.pac)
		
		procedure query_variants (
			device_name	: in pac_device_model_file.bounded_string;
			device		: in type_device_model) 
		is
			use et_package_library;
			use pac_package_variants;
			variant_cursor : pac_package_variants.cursor;
		begin
			variant_cursor := pac_package_variants.find (device.variants, variant);
			package_model := get_package_model_file (element (variant_cursor).model_cursor);
		end;
		
	begin
		pac_device_models.query_element (
			position	=> device_cursor,
			process		=> query_variants'access);

		return package_model;
	end get_package_model;


	





	function get_default_placeholders (
		device	: in pac_device_models.cursor;
		variant	: in pac_package_variant_name.bounded_string)
		return type_text_placeholders
	is
		package_model : pac_package_model_file.bounded_string;
		-- like ../lbr/smd/SO15.pac

		use et_package_library;
		package_cursor : pac_package_models.cursor;
	begin
		-- Get the package model name:		
		package_model := get_package_model (device, variant);

		-- Locate the package model in the package library:
		package_cursor := get_package_model (package_model);

		return get_default_placeholders (package_cursor);
	end get_default_placeholders;

	
		
end et_device_library.packages;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
