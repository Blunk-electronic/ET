------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           DEVICE PURPOSE                                 --
--                                                                          --
--                              S p e c                                     --
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
-- ToDo:
-- - clean up, more comments


with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;


package et_device_purpose is

	-- Devices that require operator interaction 
	-- like connectors, LEDs or switches 
	-- MUST have a purpose explicitly assigned.
	-- The purpose will be visible in the schematic, in
	-- the silkscreen and other manufcaturing related documentation.
	
	-- Example: The purpose of connector X44 
	-- is "power in". The purpose of LED5 is "system fail":

	
	keyword_purpose : constant string := "purpose";
	
	purpose_characters : character_set := to_set 
		(ranges => (('a','z'),('A','Z'),('0','9'))) or to_set ("_- "); 

	purpose_length_max : constant positive := 50;

	
	package pac_device_purpose is new generic_bounded_length (purpose_length_max);
	use pac_device_purpose;
	


	-- If no purpose is required, then this default should be used:
	empty_purpose : constant pac_device_purpose.bounded_string := to_bounded_string ("");
	

	function to_string (purpose : in pac_device_purpose.bounded_string) return string;
	
	function purpose_length_valid (purpose : in string) return boolean;
	-- Returns true if given purpose is too long. Issues warning.	


	function get_length (
		purpose : in pac_device_purpose.bounded_string)
		return natural;

		

	
	function purpose_characters_valid (
		purpose		: in pac_device_purpose.bounded_string;
		characters	: in character_set := purpose_characters) 
		return boolean;
	-- Tests if the given value contains only valid characters as specified
	-- by given character set. Returns false if invalid character found.

	procedure purpose_invalid (purpose : in string);
	-- Issues error message and raises constraint error.

	function to_purpose (
	-- Tests the given purpose for length and invalid characters.
		purpose 					: in string;
		error_on_invalid_character	: in boolean := true)
		return pac_device_purpose.bounded_string;

	
	-- Returns true if purpose is an empty string:
	function is_empty (
		purpose : in pac_device_purpose.bounded_string) 
		return boolean;
	
	
		
end et_device_purpose;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
