------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       SCHEMATIC OPERATING MODES                          --
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
--   ToDo: 

package et_modes.schematic is

	type type_verb is (
		VERB_ADD,
		VERB_BUILD,
		VERB_CHECK,
		VERB_COPY,
		VERB_CREATE,
		VERB_DELETE,
		VERB_DESCRIBE,
		VERB_DISPLAY,
		VERB_DRAG,
		VERB_DRAW,
		VERB_EXECUTE,
		VERB_EXIT,
		VERB_INVOKE,
		VERB_MAKE,
		VERB_MOVE,
		VERB_MOUNT,
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

	verb_default : constant type_verb := VERB_SHOW;
	
	verb : type_verb := verb_default;
	
	function to_string (verb : in type_verb) return string;
	function to_verb (verb : in string) return type_verb;


	type type_noun is (
		NOUN_ALL,
		NOUN_BOM,
		NOUN_CENTER,
		NOUN_CURSOR,
		NOUN_CLASS,
		NOUN_DEVICE,
		NOUN_DEVICES,
		--DEVICE_PARTCODE,
		--DEVICE_PURPOSE,
		--DEVICE_VALUE,
		NOUN_GRID,
		NOUN_INTEGRITY,
		NOUN_JUNCTION,
		NOUN_LABEL, -- CS rename to NOUN_NET_LABEL
		NOUN_LEVEL,
		NOUN_NAME,
		NOUN_NAMES,
		NOUN_MODULE,
		NOUN_NET,
		NOUN_NETS,
		NOUN_NETCHANGER,
		NOUN_NETLISTS,
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

	noun_default : constant type_noun := NOUN_NAME;
	
	noun : type_noun := noun_default;
	
	function to_string (noun : in type_noun) return string;
	function to_noun (noun : in string) return type_noun;

	

	expect_entry : type_expect_entry := expect_entry_default;
	
	
end et_modes.schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
