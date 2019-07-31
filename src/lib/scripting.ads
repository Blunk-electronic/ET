------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             SCRIPTING                                    --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo: 

with ada.text_io;				use ada.text_io;
with ada.characters.latin_1;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;

with et_general;
with et_string_processing;		use et_string_processing;
with schematic_ops;
-- with board_ops;

package scripting is
	comment_mark : constant string := ("#");

	script_name_length_max : constant positive := 100; -- CS increase if necessary
	package type_script_name is new generic_bounded_length (script_name_length_max);
	
	function to_string (name : in type_script_name.bounded_string) return string;
	function to_script_name (name : in string) return type_script_name.bounded_string;
	
	script_name : type_script_name.bounded_string;
									  

	type type_exit_code is (
		SUCCESSFUL,
		WARNINGS,
		ERROR
		);

	-- This is a workaround in order not to use reserved GNAT keywords:
	domain_prefix : constant string := ("DOM_");
	
	type type_domain is (
-- 		DOM_RIG,
		DOM_SCHEMATIC,
		DOM_BOARD
-- 		DOM_DEVICE,
-- 		DOM_SYMBOL,
-- 		DOM_PACKAGE
		);

	function to_string (domain : in type_domain) return string;
	function to_domain (domain : in string) return type_domain;

	type type_verb_schematic is (
		ADD,
		BUILD,
		CHECK,
		COPY,
		CREATE,
		DELETE,
		DESCRIBE,
		DRAG,
		DRAW,
		INVOKE,
		MAKE,
		MOVE,
		MOUNT,
		PLACE,
		REMOVE,
		RENAME,
		RENUMBER,
		ROTATE,
		SET,
		UNMOUNT,
		WRITE
		);

	function to_string (verb : in type_verb_schematic) return string;
	function to_verb (verb : in string) return type_verb_schematic;

	type type_noun_schematic is (
		BOM,
		DEVICE,
		DEVICES,
		--DEVICE_PARTCODE,
		--DEVICE_PURPOSE,
		--DEVICE_VALUE,
		INTEGRITY,
		JUNCTION,
		LABEL,
		NAME,
		NET,
		NETCHANGER,
		NETLIST,
		PARTCODE,
		PORT, -- of a submodule instance
		PURPOSE,
		SEGMENT, -- net segment
		SUBMODULE,
		SUBMODULE_FILE,		
		SUBMODULES_TREE,
		TEXT,
		TEXT_SIZE,
		UNIT,
-- 		UNIT_NAME,
-- 		UNIT_PARTCODE,
-- 		UNIT_PURPOSE,
		-- 		UNIT_VALUE
		VARIANT,
		VALUE
		);

	function to_string (noun : in type_noun_schematic) return string;
	function to_noun (noun : in string) return type_noun_schematic;

	
	type type_verb_board is (
		ADD,
		DELETE,
		--DRAG,
		DRAW,		
		FLIP,
		MAKE,
		MOVE,
		PLACE,
		ROTATE,
		ROUTE,
		RIPUP,
		SET,
		WRITE
		);

	function to_string (verb : in type_verb_board) return string;
	function to_verb (verb : in string) return type_verb_board;
	

	type type_noun_board is (
		DEVICE,
		NAME,
		NET,
		PARTCODE,
		PNP, -- pick & place
		PURPOSE,
		SUBMODULE,
		TEXT,
		TEXT_SIZE,
		TEXT_LINE_WIDTH,
		VALUE,
		VIA,
		VIA_DRILL
		);

	function to_string (noun : in type_noun_board) return string;
	function to_noun (noun : in string) return type_noun_board;

	
	function execute_script (
		file_name		: in type_script_name.bounded_string;
		log_threshold	: in type_log_level)
		return type_exit_code;

end scripting;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
