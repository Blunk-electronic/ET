------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           DEVICE PARTCODE                                --
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
--   ToDo: 


with ada.text_io;				use ada.text_io;
with ada.strings;				use ada.strings;

with et_logging;				use et_logging;
with et_string_processing;		use et_string_processing;
with et_exceptions;				use et_exceptions;


package body et_device_partcode is

	
	function get_length (
		partcode : in pac_device_partcode.bounded_string)
		return natural
	is begin
		return natural (length (partcode));
	end get_length;



	
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

	
	
end et_device_partcode;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
