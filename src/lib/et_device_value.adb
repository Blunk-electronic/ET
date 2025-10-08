------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                            DEVICE VALUE                                  --
--                                                                          --
--                              B o d y                                     --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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
with ada.strings;				use ada.strings;
with ada.strings.maps;			use ada.strings.maps;

with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;

with et_exceptions;				use et_exceptions;



package body et_device_value is


	function to_string (value : in pac_device_value.bounded_string) return string is begin
		return pac_device_value.to_string (value);
	end;


	
	function to_value (value : in string) return pac_device_value.bounded_string is begin
		return pac_device_value.to_bounded_string (value);
	end;


	
	function value_length_valid (value : in string) return boolean is
	-- Tests if the given value is longer than allowed. Returns false if too long.
	-- Returns true if length is in allowed range.		
	begin
		if value'length > value_length_max then
			log (WARNING, "value " & enclose_in_quotes (value) & " is longer than" 
				 & positive'image (value_length_max) & " characters !");
			return false;
		else
			return true;
		end if;
	end value_length_valid;

	

	function truncate (value : in string) return pac_device_value.bounded_string is
		value_out : string (1 .. value_length_max);
	begin
		value_out := value ((value'first) .. value'first - 1 + value_length_max);

		log (WARNING, "value will be truncated to " & enclose_in_quotes (value_out));
		return pac_device_value.to_bounded_string (value_out);
	end truncate;


	
	
	function value_characters_valid (
		value		: in pac_device_value.bounded_string;
		characters	: in character_set := value_characters) 
		return boolean is
	-- Tests if the given value contains only valid characters as specified
	-- by given character set. Returns false if invalid character found.
	-- Issues warning.
		use pac_device_value;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source	=> value,
			set 	=> characters,
			test 	=> outside);

		if invalid_character_position > 0 then
			log (WARNING, "value " &
				 enclose_in_quotes (pac_device_value.to_string (value))
				 & " has invalid character at position"
				 & natural'image (invalid_character_position) & " !");
			return false;
		else
			return true;
		end if;
	end value_characters_valid;


	

	procedure value_invalid (value : in string) is 
	begin
		--log (ERROR, "value " & enclose_in_quotes (value) &
			 --" invalid !", console => true);
		--raise constraint_error;
		raise syntax_error_1 with
			"ERROR: Value " & enclose_in_quotes (value) & " invalid !";
	end value_invalid;



	
	function to_value_with_check (
	-- Tests the given value for length and invalid characters.
		value						: in string;
		error_on_invalid_character	: in boolean := true)
		return pac_device_value.bounded_string is
		
		value_out : pac_device_value.bounded_string; -- to be returned		
	begin
		-- Test length of given value. truncate if too long:
		if value_length_valid (value) then
			value_out := pac_device_value.to_bounded_string (value);
		else
			value_out := truncate (value);
		end if;

		-- Test characters in (truncated) value. If error_on_invalid_character 
		-- is required by caller, abort on invalid character (default).
		if value_characters_valid (value_out) then
			null;
		else
			if error_on_invalid_character then
				value_invalid (pac_device_value.to_string (value_out));
			end if;
		end if;
			
		return value_out;
	end to_value_with_check;


	


	function is_empty (
		value : in pac_device_value.bounded_string) 
		return boolean 
	is begin
		if value = empty_value then
			return true;
		else 
			return false;
		end if;
	end is_empty;
		

	
		
end et_device_value;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
