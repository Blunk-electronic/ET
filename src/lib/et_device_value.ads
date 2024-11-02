------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                            DEVICE VALUE                                  --
--                                                                          --
--                              S p e c                                     --
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

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;



package et_device_value is

	

	-- The device value is something like 330R or 100n or 74LS00
	keyword_value : constant string := "value";
	
	value_length_max : constant positive := 50;

	-- Define the characters that are allowed for a value:
	value_characters : character_set := 
		to_set (ranges => (('A','Z'),('a','z'),('0','9'))) 
		or to_set ('_')
		or to_set ('-');
	
	package pac_device_value is new generic_bounded_length (value_length_max);

	function to_string (value : in pac_device_value.bounded_string) return string;
	function to_value (value : in string) return pac_device_value.bounded_string;
	
	function value_length_valid (value : in string) return boolean;
	-- Tests if the given value is longer than allowed. Returns false if too long.
	-- Returns true if length is in allowed range.

	function truncate (value : in string) return pac_device_value.bounded_string;
	
	function value_characters_valid (
		value		: in pac_device_value.bounded_string;
		characters	: in character_set := value_characters)
		return boolean;
	-- Tests if the given value contains only valid characters as specified
	-- by given character set. Returns false if invalid character found.
	-- Issues warning.	

	procedure value_invalid (value : in string);
	-- Issues error message and raises constraint error.

	function to_value_with_check (
	-- Tests the given value for length and invalid characters.
		value						: in string;
		error_on_invalid_character	: in boolean := true)
		return pac_device_value.bounded_string;

	-- Returns true if value is empty ("").
	function is_empty (value : in pac_device_value.bounded_string) return boolean;

		
end et_device_value;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
