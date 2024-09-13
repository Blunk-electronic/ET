------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       SCHEMATIC OPERATING MODES                          --
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

package body et_modes.schematic is

	function to_string (verb : in type_verb) return string is 
	-- Removes the domain_prefix from verb and returns the remainder as string.
	-- VERB_ADD becomes ADD.
		s : string := type_verb'image (verb);
	begin
		return s (verb_prefix'length + 1 .. s'last);
	end;

	function to_verb (verb : in string) return type_verb is begin
	-- Prepends the verb_prefix to the given string and returns a type_verb.
	-- ADD becomes VERB_ADD.
		return type_verb'value (verb_prefix & verb);
	
		exception when event: others => 
			log (ERROR, "verb " & enclose_in_quotes (verb) & " invalid !", console => true);
			raise;
	end;
	


	function to_string (noun : in type_noun) return string is 
		s : string := type_noun'image (noun);
	begin
		return s (noun_prefix'length + 1 .. s'last);
	end;

	function to_noun (noun : in string) return type_noun is begin
		return type_noun'value (noun_prefix & noun);
		exception when event: others => 
			log (ERROR, "noun " & enclose_in_quotes (noun) & " invalid !", console => true);
			raise;
	end;
	


	procedure reset_mode is begin
		verb := verb_default;
		noun := noun_default;
	end;
		

	
end et_modes.schematic;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
