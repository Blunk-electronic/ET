------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        BOARD OPERATING MODES                             --
--                                                                          --
--                               S p e c                                    --
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
		VERB_NONE, -- means no verb entered

		VERB_ADD,
		VERB_CLEAR,
		VERB_DELETE,
		VERB_DISPLAY,
		--DRAG,
		VERB_DRAW,	
		VERB_EXECUTE,
		VERB_EXIT,
		VERB_FILL,
		VERB_FLIP,
		VERB_MAKE,
		VERB_MOVE,
		VERB_PLACE,
		VERB_QUIT,
		VERB_RENAME,
		VERB_ROTATE,
		VERB_ROUTE,
		VERB_SAVE,
		VERB_SET,
		VERB_SHOW,
		VERB_UPDATE,
		VERB_ZOOM
		--WRITE
		);

	verb_default : constant type_verb := VERB_NONE;

	verb : type_verb := verb_default;
	
	function to_string (verb : in type_verb) return string;
	function to_verb (verb : in string) return type_verb;
	

	
	type type_noun is (
		NOUN_NONE, -- means no noun entered
						  
		NOUN_ALL,
		NOUN_ASSY, -- assembly documentation
		NOUN_ARC,
		NOUN_FRAME,
		NOUN_CENTER,
		NOUN_CONDUCTORS,
		NOUN_CURSOR,
		NOUN_DEVICE,
		NOUN_NON_ELECTRICAL_DEVICE,
		NOUN_FREETRACK,
		NOUN_GRID,
		NOUN_HOLE,
		NOUN_KEEPOUT,
		NOUN_LAYER, 	-- signal layer with dielectic
		NOUN_LEVEL,
		NOUN_LINE,
		NOUN_NAME,
		NOUN_MODULE,
		NOUN_NET,
		NOUN_ORIGINS, -- the center of a package
		NOUN_OUTLINE,
		NOUN_PARTCODE,
		NOUN_PLACEHOLDER,
		NOUN_PNP, -- pick & place
		NOUN_PURPOSE,
		NOUN_RATSNEST,
		NOUN_RESTRICT,
		NOUN_ROUTE_RESTRICT,
		NOUN_SCALE,
		NOUN_SCRIPT,
		NOUN_SILKSCREEN,
		NOUN_STENCIL, -- solder mask or solder paste
		NOUN_STOPMASK, -- solder stopmask
		NOUN_SUBMODULE,
		NOUN_TEXT,
		NOUN_TEXT_SIZE,
		NOUN_TEXT_LINE_WIDTH,
		NOUN_TRACK,
		NOUN_VALUE,
		NOUN_VIA,
		NOUN_VIAS,
		NOUN_VIA_DRILL,
		NOUN_VIA_RESTRICT,
		NOUN_ZONE,
		NOUN_ZOOM
		);

	noun_default : constant type_noun := NOUN_NONE;
	
	noun : type_noun := noun_default;

	
	function to_string (noun : in type_noun) return string;
	function to_noun (noun : in string) return type_noun;

	
	-- Resets verb and noun to default values:
	procedure reset_verb_and_noun;
	-- NOTE: Mind updating the mode display afterward.
	

	expect_entry : type_expect_entry := expect_entry_default;
	
end et_modes.board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
