------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           DEVICE PREFIX                                  --
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

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;


package et_device_prefix is


	-- A device name consists of a prefix (like R, C, IC, ..)
	-- and a consecutive number. Both form something like "IC702"
	prefix_characters : character_set := to_set (span => ('A','Z'));
	prefix_length_max : constant natural := 10; -- CS: there is no reason for longer prefixes.
	
	package pac_device_prefix is new generic_bounded_length (prefix_length_max);
	use pac_device_prefix;

	
	function to_string (
		prefix : in pac_device_prefix.bounded_string) 
		return string;

	
	function to_prefix (
		prefix : in string) 
		return pac_device_prefix.bounded_string;

	
	-- Tests if the given prefix is longer than allowed.
	procedure check_prefix_length (
		prefix : in string);

	
	-- Tests if the given prefix contains only valid characters.
	-- Raises exception if invalid character found.
	procedure check_prefix_characters (
		prefix : in pac_device_prefix.bounded_string);

	
	-- Predefined prefixes:
	-- - for power symbols:
	prefix_pwr : constant string := "PWR";
	-- NOTE: When adding more predefined prefixes here, mind
	-- to update function prefix_valid in et_conventions.
	-- CS distinguish between power symbols with prefixes 
	-- like PWR_POS, PWR_GND, PWR_NEG ? Probably already solved by
	-- the signal direction of the port ?

	
		
end et_device_prefix;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
