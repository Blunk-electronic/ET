------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          ASSEMBLY VARIANTS                               --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.characters;			use ada.characters;
with ada.characters.handling;	use ada.characters.handling;

with et_string_processing;		use et_string_processing;
with et_exceptions;				use et_exceptions;

package body et_assembly_variants is


	function to_string (partcode : in pac_device_partcode.bounded_string) return string is begin
		return pac_device_partcode.to_string (partcode);
	end to_string;

	
	function partcode_length_valid (partcode : in string) return boolean is
		-- Returns true if length of given partcode is ok. Issues warning if not.	
	begin
		if partcode'length > partcode_length_max then
			log (WARNING, "partcode " & enclose_in_quotes (partcode) & " is longer than" 
				 & positive'image (partcode_length_max) & " characters !");
			return false;
		else
			return true;
		end if;
	end;

	
	function partcode_characters_valid (
		partcode	: in pac_device_partcode.bounded_string;
		characters	: in character_set := partcode_characters) return boolean is
	-- Tests if the given partcode contains only valid characters as specified
	-- by given character set. Returns false if not. Issues warning.
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source	=> partcode,
			set		=> characters,
			test	=> outside);

		if invalid_character_position > 0 then
			log (WARNING, "partcode " & enclose_in_quotes (to_string (partcode))
				 & " has invalid character at position"
				 & natural'image (invalid_character_position));
			return false;
		else
			return true;
		end if;
	end;

	
	procedure partcode_invalid (partcode : in string) is 
	begin
		--log (ERROR, "partcode " & enclose_in_quotes (partcode) &
			 --" invalid !", console => true);
		--raise constraint_error;
		raise syntax_error_1 with
			"ERROR: Partcode " & enclose_in_quotes (partcode) & " invalid !";
	end partcode_invalid;

	
	function is_empty (partcode : in pac_device_partcode.bounded_string) return boolean is begin
		if pac_device_partcode.length (partcode) = 0 then
			return true;
		else
			return false;
		end if;
	end is_empty;

	
	function to_partcode (
	-- Tests the given value for length and invalid characters.							 
		partcode 					: in string;
		error_on_invalid_character	: in boolean := true) 
		return pac_device_partcode.bounded_string is

		partcode_out : pac_device_partcode.bounded_string; -- to be returned
	begin
		-- Test length of given partcode
		if partcode_length_valid (partcode) then
			partcode_out := pac_device_partcode.to_bounded_string (partcode);
		else
			partcode_invalid (partcode);
		end if;

		-- Test characters
		if partcode_characters_valid (partcode_out) then
			null;
		else
			partcode_invalid (partcode);
		end if;

		return partcode_out;
	end to_partcode;

	

	
	
	function to_mounted (mounted : in string) return type_mounted is begin
		return type_mounted'value (mounted);
	end;
		
	function to_mounted (mounted : in type_mounted) return string is begin
		return space & to_lower (type_mounted'image (mounted));
	end;

	
	function is_mounted (
		device	: in type_device_name; -- IC1
		variant	: in pac_assembly_variants.cursor)
		return boolean 
	is		
		cursor : pac_device_variants.cursor;
		
		procedure query_devices (
			variant_name	: in pac_assembly_variant_name.bounded_string; -- low_cost
			variant			: in type_assembly_variant) 
		is begin
			cursor := find (variant.devices, device);
		end query_devices;
		
	begin -- is_mounted
		if variant = pac_assembly_variants.no_element then -- assume default variant
			return true; -- device is to be mounted
		else
			
			query_element (
				position	=> variant,
				process		=> query_devices'access);

			if cursor = pac_device_variants.no_element then
				-- Device has no entry in assembly variant and thus is to be mounted
				-- as it is in the default variant:
				return true;
			else
				-- Device has an entry in assembly variant. The question now
				-- is whether the entry requests the device mounted or not.
				if element (cursor).mounted = YES then
					return true;
				else
					return false;
				end if;
				
			end if;

		end if;
		
-- 		exception
-- 			when event: others =>
-- 				log_indentation_reset;
-- 				log (text => "B " & ada.exceptions.exception_information (event), console => true);
-- 				raise;
		
	end is_mounted;
	
end et_assembly_variants;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
