------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           DEVICE PARTCODE                                --
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


with ada.strings.bounded;       use ada.strings.bounded;
with ada.strings.maps;			use ada.strings.maps;


package et_device_partcode is



	-- The part code is THE key into the ERP system of the user. It can be a cryptic SAP number
	-- or something human readable like "R_PAC_S_0805_VAL_100R_PMAX_125_TOL_5".
	-- The keywords for the latter can be specified via the conventions file. See package "convention".
	keyword_partcode : constant string := "partcode";	

	partcode_characters : character_set := to_set
		(ranges => (('a','z'),('A','Z'),('0','9'))) or to_set ("_/"); 
	partcode_length_max : constant positive := 100;
	
	package pac_device_partcode is new generic_bounded_length (partcode_length_max);
	use pac_device_partcode;


	function get_length (
		partcode : in pac_device_partcode.bounded_string)
		return natural;

	
		
	partcode_default : constant string := "N/A"; -- means not assigned
	
	function to_string (partcode : in pac_device_partcode.bounded_string) return string;

	function partcode_length_valid (partcode : in string) return boolean;
	-- Returns true if length of given partcode is ok. Issues warning if not.
	
	function partcode_characters_valid (
		partcode	: in pac_device_partcode.bounded_string;
		characters	: in character_set := partcode_characters) return boolean;
	-- Tests if the given partcode contains only valid characters as specified
	-- by given character set. Returns false if not. Issues warning.

	procedure partcode_invalid (partcode : in string);
	-- Issues error message and raises constraint error.

	function is_empty (partcode : in pac_device_partcode.bounded_string) return boolean;
	
	function to_partcode (
	-- Tests the given value for length and invalid characters.							 
		partcode 					: in string;
		error_on_invalid_character	: in boolean := true) 
		return pac_device_partcode.bounded_string;



	
end et_device_partcode;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
