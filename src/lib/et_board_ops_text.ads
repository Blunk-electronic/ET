------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                      BOARD OPERATIONS / TEXT                             --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
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

with et_module_names;					use et_module_names;
with et_generic_modules;				use et_generic_modules;
with et_board_layer_category;			use et_board_layer_category;

with et_stopmask;
with et_silkscreen;
with et_assy_doc;

with et_board_text;						use et_board_text;
with et_text;
with et_text_content;					use et_text_content;

with et_pcb_placeholders.non_conductor;	use et_pcb_placeholders.non_conductor;



package et_board_ops_text is

	use pac_generic_modules;
	use pac_text_board;

	
	-- Maps from the meaning of a text placeholder
	-- to its actutal content:
	function to_placeholder_content (
		module_cursor	: in pac_generic_modules.cursor;
		meaning 		: in type_placeholder_meaning_non_conductor)										
		return pac_text_content.bounded_string;
	
	
											
end et_board_ops_text;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
