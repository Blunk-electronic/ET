------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        BOARD OPERATING MODES                             --
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

package et_modes.board is

	type type_verb is (
		VERB_NONE, -- means no verb entered

		VERB_ADD,

		VERB_CLEAR,
		VERB_COPY,

		VERB_DELETE,
		VERB_DEFINE,
		VERB_DISPLAY,
		--DRAG,
		VERB_DRAW,

		VERB_EXECUTE,
		VERB_EXIT,

		VERB_FILL,
		VERB_FLIP,

		VERB_MAKE, -- CS alternatively VERB_EXPORT ?
		VERB_MOVE,

		VERB_PLACE,

		VERB_QUIT,

		VERB_RENAME,
		VERB_REMOVE,
		VERB_RESTORE,
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

	function verb return type_verb;
	procedure set_verb (verb : type_verb);

	function to_string (verb : in type_verb) return string;
	function to_verb (verb : in string) return type_verb;



	type type_noun is (
		NOUN_NONE, -- means no noun entered

		NOUN_ALL,
		NOUN_ASSY, -- assembly documentation
		NOUN_ARC,

		NOUN_BOM,

		NOUN_CENTER,
		NOUN_COLOR,
		NOUN_CONDUCTORS,
		NOUN_CURSOR,

		NOUN_DEVICE,

		NOUN_FRAME,
		NOUN_FREETRACK,

		NOUN_GRID,
		NOUN_GROUP,

		NOUN_HOLE,

		NOUN_KEEPOUT,

		NOUN_LAYER, 	-- signal layer with dielectic
		NOUN_LEVEL,
		NOUN_LIBRARY,
		NOUN_LINE,

		NOUN_MODULE,

		NOUN_NAME,
		NOUN_NET,
		NOUN_NETCHANGER,

		NOUN_ORIGINS, -- the center of a package
		NOUN_OUTLINE,

		NOUN_PARTCODE,
		NOUN_PLACEHOLDER,
		NOUN_PLACEHOLDERS,
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

	function noun return type_noun;
	procedure set_noun (noun : type_noun);


	function to_string (noun : in type_noun) return string;
	function to_noun (noun : in string) return type_noun;


	-- Resets verb and noun to default values:
	procedure reset_verb_and_noun;
	-- NOTE: Mind updating the mode display afterward.


	expect_entry : type_expect_entry := expect_entry_default;

	type type_noun_array_of_boolean is array (type_noun) of boolean;

	show_nouns_for_verb : constant array (type_verb) of type_noun_array_of_boolean := (
		VERB_NONE		=> (NOUN_NONE => true,								others => false),
		VERB_ADD		=> (NOUN_DEVICE .. NOUN_VALUE => true,				others => false),
		VERB_CLEAR		=> (NOUN_NONE => true,								others => false),
		VERB_COPY		=> (NOUN_DEVICE .. NOUN_VALUE => true,				others => false),
		VERB_DELETE		=> (NOUN_DEVICE .. NOUN_VALUE => true,				others => false),
		VERB_DEFINE		=> (NOUN_NONE => true,								others => false),
		VERB_DISPLAY	=> (NOUN_NONE => true,								others => false),
		VERB_DRAW		=> (NOUN_NONE => true,								others => false),
		VERB_EXECUTE	=> (NOUN_NONE => true,								others => false),
		VERB_EXIT		=> (NOUN_NONE => true,								others => false),
		VERB_FILL		=> (NOUN_NONE => true,								others => false),
		VERB_FLIP		=> (NOUN_NONE => true,								others => false),
		VERB_MAKE		=> (NOUN_NONE => true,								others => false),
		VERB_MOVE		=> (NOUN_NONE => true,								others => false),
		VERB_PLACE		=> (NOUN_NONE => true,								others => false),
		VERB_QUIT		=> (NOUN_NONE => true,								others => false),
		VERB_REMOVE		=> (NOUN_NONE => true,								others => false),
		VERB_RENAME		=> (NOUN_NONE => true,								others => false),
		VERB_RESTORE	=> (NOUN_NONE => true,								others => false),
		VERB_ROUTE		=> (NOUN_NONE => true,								others => false),
		VERB_ROTATE		=> (NOUN_NONE => true,								others => false),
		VERB_SAVE		=> (NOUN_NONE => true,								others => false),
		VERB_SET		=> (NOUN_NONE => true,								others => false),
		VERB_SHOW		=> (NOUN_NONE => true,								others => false),
		VERB_UPDATE		=> (NOUN_NONE => true,								others => false),
		VERB_ZOOM		=> (NOUN_ZOOM => true,								others => false));

	noun_last : array (type_verb) of type_noun := (
		VERB_NONE		=> NOUN_NONE,
		VERB_ADD		=> NOUN_DEVICE,
		VERB_CLEAR		=> NOUN_NONE,
		VERB_COPY		=> NOUN_VALUE,
		VERB_DELETE		=> NOUN_DEVICE,
		VERB_DEFINE		=> NOUN_NONE,
		VERB_DISPLAY	=> NOUN_NONE,
		VERB_DRAW		=> NOUN_NONE,
		VERB_EXECUTE	=> NOUN_NONE,
		VERB_EXIT		=> NOUN_NONE,
		VERB_FILL		=> NOUN_NONE,
		VERB_FLIP		=> NOUN_NONE,
		VERB_MAKE		=> NOUN_NONE,
		VERB_MOVE		=> NOUN_NONE,
		VERB_PLACE		=> NOUN_NONE,
		VERB_QUIT		=> NOUN_NONE,
		VERB_REMOVE		=> NOUN_NONE,
		VERB_RENAME		=> NOUN_NONE,
		VERB_RESTORE	=> NOUN_NONE,
		VERB_ROUTE		=> NOUN_NONE,
		VERB_ROTATE		=> NOUN_NONE,
		VERB_SAVE		=> NOUN_NONE,
		VERB_SET		=> NOUN_NONE,
		VERB_SHOW		=> NOUN_NONE,
		VERB_UPDATE		=> NOUN_NONE,
		VERB_ZOOM		=> NOUN_ZOOM);

end et_modes.board;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
