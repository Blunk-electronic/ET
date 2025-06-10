------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         DEVICE APPEARANCE                                --
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

with ada.characters;			use ada.characters;
with ada.characters.handling;	use ada.characters.handling;

-- with ada.strings; 				use ada.strings;
-- with ada.strings.fixed; 		use ada.strings.fixed;
-- with ada.strings.bounded; 		use ada.strings.bounded;


package body et_device_appearance is

	

	function to_string (
		appearance	: in type_appearance;
		verbose		: in boolean := false)
		return string 
	is 
		-- Remove the prefix from appearance and 
		-- return the remainder as string.
		-- APPEARANCE_PCB becomes PCB.
		s : string := type_appearance'image (appearance);
	begin
		if verbose then
			case appearance is
				when APPEARANCE_VIRTUAL =>
					return ("appears in schematic only (virtual device)");
				when APPEARANCE_PCB =>
					return ("appears in schematic and layout");
			end case;
		else
			return s (appearance_prefix'length + 1 .. s'last);
		end if;
	end;


	
	function to_appearance (
		appearance : in string) 
		return type_appearance 
	is begin
		return type_appearance'value (appearance_prefix & appearance);
	end;	

	

	
		
end et_device_appearance;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
