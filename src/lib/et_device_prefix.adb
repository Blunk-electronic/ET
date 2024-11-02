------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           DEVICE PREFIX                                  --
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


package body et_device_prefix is



	function to_string (prefix : in pac_device_prefix.bounded_string) return string is
	begin
		return pac_device_prefix.to_string (prefix); -- leading space not allowd !
	end to_string;


	
	function to_prefix (prefix : in string) return pac_device_prefix.bounded_string is begin
		return pac_device_prefix.to_bounded_string (prefix);
	end to_prefix;


	
	procedure check_prefix_length (prefix : in string) is
	-- Tests if the given prefix is longer than allowed.
	begin
		if prefix'length > prefix_length_max then
			log (ERROR, "max. number of characters for device name prefix is" 
				 & positive'image (prefix_length_max) & " !",
				console => true);
			raise constraint_error;
		end if;
	end check_prefix_length;

	
	
	procedure check_prefix_characters (prefix : in pac_device_prefix.bounded_string) is
	-- Tests if the given prefix contains only valid characters.
	-- Raises exception if invalid character found.
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source	=> prefix,
			set		=> prefix_characters,
			test	=> outside);

		if invalid_character_position > 0 then
			log (ERROR, "device prefix " & to_string (prefix) 
				 & " has invalid character at position"
				 & natural'image (invalid_character_position),
				console => true
				);
			raise constraint_error;
		end if;
	end check_prefix_characters;


	
		
end et_device_prefix;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
