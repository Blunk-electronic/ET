------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         BOARD OPERATING MODES                            --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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

package body et_modes.board is

	function to_string (verb : in type_verb_board) return string is 
	-- Removes the verb_prefix from verb and returns the remainder as string.
	-- VERB_ADD becomes ADD.
		s : string := type_verb_board'image (verb);
	begin
		return s (verb_prefix'length + 1 .. s'last);
	end;

	function to_verb (verb : in string) return type_verb_board is begin
	-- Prepends the verb_prefix to the given string and returns a type_verb_board.
	-- ADD becomes VERB_ADD.
		return type_verb_board'value (verb_prefix & verb);
	
		exception when event: others => 
			log (ERROR, "verb " & enclose_in_quotes (verb) & " invalid !", console => true);
			raise;
	end;
	
	
end et_modes.board;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
