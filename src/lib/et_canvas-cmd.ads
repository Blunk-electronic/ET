------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           CANVAS COMMANDS                                --
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

with ada.text_io;					use ada.text_io;

with et_general;					use et_general;
with et_string_processing;			use et_string_processing;
with et_logging;					use et_logging;

with et_cmd_sts;



generic

package et_canvas.cmd is

	type type_canvas_verb is (
		-- VERB_ZOOM,
		VERB_SET);

	-- verb : type_canvas_verb := type_canvas_verb'first;
	

	type type_canvas_noun is (
		NOUN_CURSOR,
		-- NOUN_FIT,
		-- NOUN_LEVEL,
		-- NOUN_CENTER,
		NOUN_ZOOM,
		NOUN_GRID);


	-- noun : type_canvas_noun := type_canvas_noun'first;
	
		
	procedure dummy;


	-- The number of fields of the given command:
	cmd_field_count : type_field_count;


	-- This function is a shortcut to get a single field
	-- from the current command:
	-- function f (place : in type_field_count) 
	-- 	return string;

	
	procedure command_too_long (
		cmd		: in type_fields_of_line;
		from	: in type_field_count);


	-- This procedure is a shortcut. Call it in case
	-- the given command is too long:
	procedure too_long;

	
	procedure command_incomplete;

	
	procedure invalid_noun (
		noun : in string);


	


	-- This procedure parses a canvas related command.
	-- If the runmode is non-graphical (like headless) then
	-- nothing will be done here:
	procedure parse_canvas_command (
		verb	: in type_canvas_verb;
		noun	: in type_canvas_noun);



	
end et_canvas.cmd;

