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

	-- These tables are derived from the noun handling of each verb
	-- in procedure parse of et_cp_board.adb:
	show_nouns_for_verb : constant array (type_verb) of type_noun_array_of_boolean := (
		VERB_NONE		=> (NOUN_NONE => true,								others => false),
		VERB_ADD		=> (NOUN_DEVICE | NOUN_LAYER | NOUN_LIBRARY => true,	others => false),
		VERB_CLEAR		=> (NOUN_ZONE => true,								others => false),
		VERB_COPY		=> (NOUN_DEVICE => true,							others => false),
		VERB_DELETE		=> (NOUN_DEVICE | NOUN_GROUP | NOUN_LAYER | NOUN_HOLE | NOUN_OUTLINE | NOUN_SILKSCREEN
							| NOUN_ASSY | NOUN_KEEPOUT | NOUN_STENCIL | NOUN_STOPMASK | NOUN_VIA
							| NOUN_ROUTE_RESTRICT | NOUN_VIA_RESTRICT | NOUN_FREETRACK | NOUN_TRACK => true,
							others => false),
		VERB_DEFINE		=> (NOUN_GROUP => true,							others => false),
		VERB_DISPLAY	=> (NOUN_SILKSCREEN | NOUN_ASSY | NOUN_KEEPOUT | NOUN_STOPMASK | NOUN_STENCIL
							| NOUN_ORIGINS | NOUN_CONDUCTORS | NOUN_OUTLINE | NOUN_RATSNEST | NOUN_RESTRICT
							| NOUN_VIAS => true,							others => false),
		VERB_DRAW		=> (NOUN_HOLE | NOUN_OUTLINE | NOUN_SILKSCREEN | NOUN_ASSY | NOUN_KEEPOUT
							| NOUN_ROUTE_RESTRICT | NOUN_STENCIL | NOUN_STOPMASK | NOUN_VIA_RESTRICT => true,
							others => false),
		VERB_EXECUTE	=> (NOUN_SCRIPT => true,							others => false),
		VERB_EXIT		=> (NOUN_NONE => true,								others => false),
		VERB_FILL		=> (NOUN_ZONE => true,								others => false),
		VERB_FLIP		=> (NOUN_DEVICE => true,							others => false),
		VERB_MAKE		=> (NOUN_BOM | NOUN_PNP => true,					others => false),
		VERB_MOVE		=> (NOUN_FRAME | NOUN_CURSOR | NOUN_DEVICE | NOUN_NAME | NOUN_VALUE | NOUN_PARTCODE
							| NOUN_PURPOSE | NOUN_NETCHANGER | NOUN_SUBMODULE | NOUN_VIA => true,
							others => false),
		VERB_PLACE		=> (NOUN_VIA | NOUN_TEXT | NOUN_PLACEHOLDER => true,	others => false),
		VERB_QUIT		=> (NOUN_NONE => true,								others => false),
		VERB_REMOVE		=> (NOUN_LIBRARY => true,							others => false),
		VERB_RENAME		=> (NOUN_DEVICE => true,							others => false),
		VERB_RESTORE	=> (NOUN_PLACEHOLDERS => true,						others => false),
		VERB_ROUTE		=> (NOUN_FREETRACK | NOUN_NET => true,				others => false),
		VERB_ROTATE		=> (NOUN_DEVICE | NOUN_NAME | NOUN_VALUE | NOUN_PARTCODE | NOUN_PURPOSE => true,
							others => false),
		VERB_SAVE		=> (NOUN_MODULE => true,							others => false),
		VERB_SET		=> (NOUN_GRID | NOUN_COLOR | NOUN_CURSOR | NOUN_ZOOM | NOUN_SCALE | NOUN_ZONE
							| NOUN_VIA | NOUN_NETCHANGER => true,			others => false),
		VERB_SHOW		=> (NOUN_MODULE | NOUN_DEVICE | NOUN_NET | NOUN_NETCHANGER => true,
							others => false),
		VERB_UPDATE		=> (NOUN_RATSNEST => true,							others => false),
		VERB_ZOOM		=> (NOUN_ZOOM => true,								others => false));

	-- Initialized to the first noun listed for the verb above:
	noun_last : array (type_verb) of type_noun := (
		VERB_NONE		=> NOUN_NONE,
		VERB_ADD		=> NOUN_DEVICE,
		VERB_CLEAR		=> NOUN_ZONE,
		VERB_COPY		=> NOUN_DEVICE,
		VERB_DELETE		=> NOUN_DEVICE,
		VERB_DEFINE		=> NOUN_GROUP,
		VERB_DISPLAY	=> NOUN_SILKSCREEN,
		VERB_DRAW		=> NOUN_HOLE,
		VERB_EXECUTE	=> NOUN_SCRIPT,
		VERB_EXIT		=> NOUN_NONE,
		VERB_FILL		=> NOUN_ZONE,
		VERB_FLIP		=> NOUN_DEVICE,
		VERB_MAKE		=> NOUN_BOM,
		VERB_MOVE		=> NOUN_FRAME,
		VERB_PLACE		=> NOUN_VIA,
		VERB_QUIT		=> NOUN_NONE,
		VERB_REMOVE		=> NOUN_LIBRARY,
		VERB_RENAME		=> NOUN_DEVICE,
		VERB_RESTORE	=> NOUN_PLACEHOLDERS,
		VERB_ROUTE		=> NOUN_FREETRACK,
		VERB_ROTATE		=> NOUN_DEVICE,
		VERB_SAVE		=> NOUN_MODULE,
		VERB_SET		=> NOUN_GRID,
		VERB_SHOW		=> NOUN_MODULE,
		VERB_UPDATE		=> NOUN_RATSNEST,
		VERB_ZOOM		=> NOUN_ZOOM);

end et_modes.board;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
