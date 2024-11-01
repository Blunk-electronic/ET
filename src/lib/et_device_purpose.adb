------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           DEVICE PURPOSE                                 --
--                                                                          --
--                              B o d y                                     --
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
with ada.strings; 				use ada.strings;

with et_logging;				use et_logging;
with et_string_processing;		use et_string_processing;
with et_exceptions;				use et_exceptions;


package body et_device_purpose is

	function to_string (purpose : in pac_device_purpose.bounded_string) return string is begin
		return pac_device_purpose.to_string (purpose);
	end to_string;


	
	function purpose_length_valid (purpose : in string) return boolean is 
	begin
		if purpose'length > purpose_length_max then
			log (WARNING, "purpose " & enclose_in_quotes (purpose) & " is longer than" 
				 & positive'image (purpose_length_max) & " characters !", 
				console => true);
			return false;
		else
			return true;
		end if;
	end;


	function get_length (
		purpose : in pac_device_purpose.bounded_string)
		return natural
	is begin
		return natural (length (purpose));
	end get_length;

	
	
	function purpose_characters_valid (
	-- Tests if the given value contains only valid characters as specified
	-- by given character set. Returns false if invalid character found.
		purpose		: in pac_device_purpose.bounded_string;
		characters	: in character_set := purpose_characters) 
		return boolean 
	is
		use pac_device_purpose;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source	=> purpose,
			set 	=> characters,
			test 	=> outside);

		if invalid_character_position > 0 then
			log (WARNING, "purpose " & enclose_in_quotes (to_string (purpose))
				 & " has invalid character at position"
				 & natural'image (invalid_character_position)
				);
			return false;
		else
			return true;
		end if;
	end purpose_characters_valid;


	
	procedure purpose_invalid (purpose : in string) is 
	begin
		--log (ERROR, "purpose " & enclose_in_quotes (purpose) &
			 --" invalid !", console => true);
		--raise constraint_error;
		raise syntax_error_1 with
			"ERROR: Purpose " & enclose_in_quotes (purpose) & " invalid !";
	end purpose_invalid;

	

	function to_purpose (
	-- Tests the given purpose for length and invalid characters.
		purpose 					: in string;
		error_on_invalid_character	: in boolean := true)
		return pac_device_purpose.bounded_string is

		purpose_out : pac_device_purpose.bounded_string; -- to be returned
	begin
		-- Test length of given purpose:
		if purpose_length_valid (purpose) then
			purpose_out := pac_device_purpose.to_bounded_string (purpose);
		else
			purpose_invalid (purpose);
		end if;

		-- Test characters:
		if purpose_characters_valid (purpose_out) then
			null;
		else
			purpose_invalid (purpose);
		end if;

		return purpose_out;
	end to_purpose;

	

	function is_empty (purpose : in pac_device_purpose.bounded_string) return boolean is 
		use pac_device_purpose;
	begin
		if length (purpose) = 0 then return true;
		else return false;
		end if;
	end is_empty;


	
		
end et_device_purpose;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
