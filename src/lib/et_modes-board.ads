------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        BOARD OPERATING MODES                             --
--                                                                          --
--                               S p e c                                    --
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

package et_modes.board is
	
-- 	type type_mode is (
-- 		MODE_SHOW,
-- 		MODE_DELETE
-- 		);
	
-- 	default : constant type_mode := MODE_SHOW;

-- 	function to_string (mode : in type_mode) return string;
-- 	function to_mode (mode : in string) return type_mode;

-- 	op_mode : type_mode := default;

------------
	type type_verb_board is (
		VERB_ADD,
		VERB_DELETE,
		VERB_DISPLAY,
		--DRAG,
		VERB_DRAW,	
		VERB_EXECUTE,
		VERB_EXIT,
		VERB_FLIP,
		VERB_MAKE,
		VERB_MOVE,
		--PLACE,
		VERB_POSITION,
		VERB_QUIT,
		VERB_RIPUP,
		VERB_RENAME,
		VERB_ROTATE,
		VERB_ROUTE,
		VERB_SET,
		--VERB_SHOW CS
		VERB_ZOOM
		--WRITE
		);

	default : constant type_verb_board := VERB_ADD; -- CS show

	op_mode : type_verb_board := default;
	
	function to_string (verb : in type_verb_board) return string;
	function to_verb (verb : in string) return type_verb_board;
	
	
end et_modes.board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
