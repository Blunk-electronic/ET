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
	
	type type_verb is (
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

	verb_default : constant type_verb := VERB_ADD; -- CS show

	verb : type_verb := verb_default;
	
	function to_string (verb : in type_verb) return string;
	function to_verb (verb : in string) return type_verb;
	

	
	type type_noun is (
		NOUN_ASSY, -- assembly documentation
		NOUN_BOARD,
		NOUN_CENTER,
		NOUN_CONDUCTORS,
		NOUN_CURSOR,
		NOUN_DEVICE,
		NOUN_FREETRACK,
		NOUN_FIT,
		NOUN_GRID,
		NOUN_KEEPOUT,
		NOUN_LAYER, 	-- signal layer with dielectic
		NOUN_LEVEL,
		NOUN_NAME,
		NOUN_NET,
		NOUN_ORIGINS, -- the center of a package
		NOUN_OUTLINE,
		NOUN_PARTCODE,
		NOUN_PNP, -- pick & place
		NOUN_PURPOSE,
		NOUN_RESTRICT,
		NOUN_ROUTE_RESTRICT,
		NOUN_SCRIPT,
		NOUN_SILKSCREEN,
		NOUN_STENCIL, -- solder mask or solder paste
		NOUN_STOP, -- solder stop mask
		NOUN_SUBMODULE,
		NOUN_TEXT,
		NOUN_TEXT_SIZE,
		NOUN_TEXT_LINE_WIDTH,
-- 		TRACK,
		NOUN_VALUE,
		NOUN_VIA,
		NOUN_VIAS,
		NOUN_VIA_DRILL,
		NOUN_VIA_RESTRICT
		);

	noun_default : constant type_noun := NOUN_NAME;
	
	noun : type_noun := noun_default;

	
	function to_string (noun : in type_noun) return string;
	function to_noun (noun : in string) return type_noun;



	expect_entry : type_expect_entry := expect_entry_default;
	
end et_modes.board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
