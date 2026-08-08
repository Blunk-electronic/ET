------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       SCHEMATIC OPERATING MODES                          --
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

package et_modes.schematic is

	type type_verb is (
		VERB_NONE, -- means no verb entered

		VERB_ADD,

		VERB_BUILD,

		VERB_CHECK,
		VERB_CLEAR,
		VERB_COPY,
		VERB_CREATE,

		VERB_DELETE,
		VERB_DESCRIBE,
		VERB_DEFINE,
		VERB_DISPLAY,
		VERB_DISSOLVE,
		VERB_DRAG,
		VERB_DRAW,

		VERB_EXECUTE,
		VERB_EXIT,
		VERB_FETCH,

		VERB_MAKE,
		VERB_MIRROR,
		VERB_MOVE,
		VERB_MOUNT,

		VERB_PASTE,
		VERB_PLACE,

		VERB_QUIT,

		VERB_REMOVE,
		VERB_RENAME,
		VERB_RENUMBER,
		VERB_ROTATE,

		VERB_SAVE,
		VERB_SET,
		VERB_SHOW,

		VERB_UNMOUNT,

		VERB_WRITE,

		VERB_ZOOM
		);

	verb_default : constant type_verb := VERB_NONE;

	function verb return type_verb;
	procedure set_verb (verb : type_verb);

	function to_string (verb : in type_verb) return string;
	function to_verb (verb : in string) return type_verb;


	type type_noun is (
		NOUN_NONE, -- means no noun entered

		NOUN_ALL,

		NOUN_CENTER,
		NOUN_COLOR,
		NOUN_CURSOR,
		NOUN_CLASS,

		NOUN_DEVICE,
		NOUN_DEVICES,
		--DEVICE_PARTCODE,
		--DEVICE_PURPOSE,
		--DEVICE_VALUE,

		NOUN_GRID,
		NOUN_GROUP,

		NOUN_INTEGRITY,

		NOUN_NET_LABEL,
		NOUN_NET_CONNECTOR,

		NOUN_LEVEL,
		NOUN_LIBRARY,

		NOUN_MODULE,

		NOUN_NAME,
		NOUN_NAMES,
		NOUN_NET,
		NOUN_NETS,
		NOUN_NETCHANGER,
		NOUN_NETLIST,
		-- NOUN_NETLISTS,

		NOUN_PLACEHOLDER,
		NOUN_PARTCODE,
		NOUN_PORT, -- of a submodule instance
		NOUN_PORTS,
		NOUN_PURPOSE,
		NOUN_PURPOSES,

		NOUN_SCALE,
		NOUN_SCOPE,
		NOUN_SCRIPT,
		NOUN_SEGMENT, -- net segment
		NOUN_SHEET,
		NOUN_STRAND,
		NOUN_SUBMODULE,
		NOUN_SUBMODULE_FILE,
		NOUN_SUBMODULES_TREE,

		NOUN_TEXT,
		NOUN_TEXTS,
		NOUN_TEXT_SIZE,

		NOUN_UNIT,
-- 		UNIT_NAME,
-- 		UNIT_PARTCODE,
-- 		UNIT_PURPOSE,
		-- 		UNIT_VALUE

		NOUN_VARIANT,
		NOUN_VALUE,
		NOUN_VALUES,

		NOUN_ZOOM
		);

	noun_default : constant type_noun := NOUN_NONE;

	function noun return type_noun;
	procedure set_noun (noun : type_noun);

	function to_string (noun : in type_noun) return string;
	function to_noun (noun : in string) return type_noun;


	-- Resets verb and noun to default values:
	procedure reset_verb_and_noun;


	expect_entry : type_expect_entry := expect_entry_default;


	type type_noun_array_of_boolean is array (type_noun) of boolean;

	-- These tables are derived from the noun handling of each verb
	-- in procedure parse of et_cp_schematic.adb:
	show_nouns_for_verb : constant array (type_verb) of type_noun_array_of_boolean := (
		VERB_NONE		=> (NOUN_NONE => true,								others => false),
		VERB_ADD		=> (NOUN_LIBRARY | NOUN_DEVICE | NOUN_NETCHANGER | NOUN_PORT | NOUN_SUBMODULE => true,
							others => false),
		VERB_BUILD		=> (NOUN_SUBMODULES_TREE => true,					others => false),
		VERB_CHECK		=> (NOUN_INTEGRITY => true,						others => false),
		VERB_CLEAR		=> (NOUN_GROUP => true,							others => false),
		VERB_COPY		=> (NOUN_DEVICE | NOUN_GROUP | NOUN_SUBMODULE | NOUN_NETCHANGER => true,
							others => false),
		VERB_CREATE		=> (NOUN_VARIANT | NOUN_MODULE => true,			others => false),
		VERB_DELETE		=> (NOUN_DEVICE | NOUN_GROUP | NOUN_NET_CONNECTOR | NOUN_NET_LABEL | NOUN_MODULE
							| NOUN_NET | NOUN_NETCHANGER | NOUN_PORT | NOUN_SEGMENT | NOUN_STRAND
							| NOUN_SUBMODULE | NOUN_TEXT | NOUN_UNIT | NOUN_VARIANT | NOUN_SHEET => true,
							others => false),
		VERB_DESCRIBE	=> (NOUN_VARIANT => true,							others => false),
		VERB_DEFINE		=> (NOUN_GROUP => true,							others => false),
		VERB_DISPLAY	=> (NOUN_PORTS | NOUN_NETS | NOUN_NAMES | NOUN_VALUES | NOUN_PURPOSES | NOUN_TEXTS => true,
							others => false),
		VERB_DISSOLVE	=> (NOUN_NETCHANGER => true,						others => false),
		VERB_DRAG		=> (NOUN_GROUP | NOUN_UNIT | NOUN_NETCHANGER | NOUN_PORT | NOUN_SEGMENT | NOUN_SUBMODULE => true,
							others => false),
		VERB_DRAW		=> (NOUN_NET => true,								others => false),
		VERB_EXECUTE	=> (NOUN_SCRIPT => true,							others => false),
		VERB_EXIT		=> (NOUN_NONE => true,								others => false),
		VERB_FETCH		=> (NOUN_UNIT => true,								others => false),
		VERB_MAKE		=> (NOUN_NETLIST => true,							others => false),
		VERB_MIRROR		=> (NOUN_UNIT => true,								others => false),
		VERB_MOVE		=> (NOUN_CURSOR | NOUN_NAME | NOUN_VALUE | NOUN_PARTCODE | NOUN_PURPOSE | NOUN_PORT
							| NOUN_NETCHANGER | NOUN_NET_LABEL | NOUN_TEXT | NOUN_SUBMODULE | NOUN_UNIT => true,
							others => false),
		VERB_MOUNT		=> (NOUN_DEVICE | NOUN_SUBMODULE => true,			others => false),
		VERB_PASTE		=> (NOUN_GROUP => true,							others => false),
		VERB_PLACE		=> (NOUN_NET_CONNECTOR | NOUN_NET_LABEL => true,	others => false),
		VERB_QUIT		=> (NOUN_NONE => true,								others => false),
		VERB_REMOVE		=> (NOUN_LIBRARY | NOUN_DEVICE | NOUN_SUBMODULE => true,	others => false),
		VERB_RENAME		=> (NOUN_DEVICE | NOUN_NETCHANGER | NOUN_SUBMODULE | NOUN_NET => true,
							others => false),
		VERB_RENUMBER	=> (NOUN_DEVICES => true,							others => false),
		VERB_ROTATE		=> (NOUN_TEXT | NOUN_UNIT | NOUN_NAME | NOUN_VALUE | NOUN_PURPOSE | NOUN_PARTCODE
							| NOUN_NETCHANGER => true,						others => false),
		VERB_SAVE		=> (NOUN_MODULE => true,							others => false),
		VERB_SET		=> (NOUN_CLASS | NOUN_COLOR | NOUN_GRID | NOUN_CURSOR | NOUN_NETCHANGER | NOUN_ZOOM
							| NOUN_SCALE | NOUN_PARTCODE | NOUN_PURPOSE | NOUN_SCOPE | NOUN_SHEET
							| NOUN_SUBMODULE_FILE | NOUN_VALUE | NOUN_VARIANT | NOUN_TEXT_SIZE => true,
							others => false),
		VERB_SHOW		=> (NOUN_DEVICE | NOUN_NETCHANGER | NOUN_MODULE | NOUN_NET | NOUN_SHEET => true,
							others => false),
		VERB_UNMOUNT	=> (NOUN_DEVICE => true,							others => false),
		VERB_WRITE		=> (NOUN_TEXT => true,								others => false),
		VERB_ZOOM		=> (NOUN_ZOOM => true,								others => false));

	-- Initialized to the first noun listed for the verb above:
	noun_last : array (type_verb) of type_noun := (
		VERB_NONE		=> NOUN_NONE,
		VERB_ADD		=> NOUN_LIBRARY,
		VERB_BUILD		=> NOUN_SUBMODULES_TREE,
		VERB_CHECK		=> NOUN_INTEGRITY,
		VERB_CLEAR		=> NOUN_GROUP,
		VERB_COPY		=> NOUN_DEVICE,
		VERB_CREATE		=> NOUN_VARIANT,
		VERB_DELETE		=> NOUN_DEVICE,
		VERB_DESCRIBE	=> NOUN_VARIANT,
		VERB_DEFINE		=> NOUN_GROUP,
		VERB_DISPLAY	=> NOUN_PORTS,
		VERB_DISSOLVE	=> NOUN_NETCHANGER,
		VERB_DRAG		=> NOUN_GROUP,
		VERB_DRAW		=> NOUN_NET,
		VERB_EXECUTE	=> NOUN_SCRIPT,
		VERB_EXIT		=> NOUN_NONE,
		VERB_FETCH		=> NOUN_UNIT,
		VERB_MAKE		=> NOUN_NETLIST,
		VERB_MIRROR		=> NOUN_UNIT,
		VERB_MOVE		=> NOUN_CURSOR,
		VERB_MOUNT		=> NOUN_DEVICE,
		VERB_PASTE		=> NOUN_GROUP,
		VERB_PLACE		=> NOUN_NET_CONNECTOR,
		VERB_QUIT		=> NOUN_NONE,
		VERB_REMOVE		=> NOUN_LIBRARY,
		VERB_RENAME		=> NOUN_DEVICE,
		VERB_RENUMBER	=> NOUN_DEVICES,
		VERB_ROTATE		=> NOUN_TEXT,
		VERB_SAVE		=> NOUN_MODULE,
		VERB_SET		=> NOUN_CLASS,
		VERB_SHOW		=> NOUN_DEVICE,
		VERB_UNMOUNT	=> NOUN_DEVICE,
		VERB_WRITE		=> NOUN_TEXT,
		VERB_ZOOM		=> NOUN_ZOOM);

end et_modes.schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
