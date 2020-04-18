------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             SCRIPTING                                    --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2020 Mario Blunk, Blunk electronic                 --
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

with ada.text_io;				use ada.text_io;
with ada.characters.latin_1;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers;			use ada.containers;

with et_general;
with et_string_processing;		use et_string_processing;
with schematic_ops;
-- with board_ops;
with netlists;

with et_canvas_schematic;
with et_canvas_board;

package scripting is
	comment_mark : constant string := ("#");

	script_name_length_max : constant positive := 100; -- CS increase if necessary
	package type_script_name is new generic_bounded_length (script_name_length_max);
	
	function to_string (name : in type_script_name.bounded_string) return string;
	function to_script_name (name : in string) return type_script_name.bounded_string;
	
	script_name : type_script_name.bounded_string;

	procedure invalid_noun (noun : in string);
	
	procedure command_incomplete (cmd : in type_fields_of_line);
	
	procedure command_too_long (
		cmd		: in type_fields_of_line;
		from	: in count_type);
	
	type type_exit_code is (
		SUCCESSFUL,
		WARNINGS,
		ERROR
		);

	-- Prefixes before enumeration types prevent clashes with gnat keywords
	-- and package names:
	domain_prefix : constant string := ("DOM_");
	verb_prefix : constant string := ("VERB_");
	noun_prefix : constant string := ("NOUN_");
	
	type type_domain is (
		DOM_PROJECT,
--		DOM_RIG,
		DOM_SCHEMATIC,
		DOM_BOARD
-- 		DOM_DEVICE,
-- 		DOM_SYMBOL,
-- 		DOM_PACKAGE
		);
	
	function to_string (domain : in type_domain) return string;
	function to_domain (domain : in string) return type_domain;
	
	type type_verb_project is (
		VERB_CREATE,
		VERB_DELETE,
		VERB_OPEN,
		VERB_SAVE
		);

	
-- PROJECT
	
	function to_string (verb : in type_verb_project) return string;
	function to_verb (verb : in string) return type_verb_project;
	
	type type_noun_project is (
		NOUN_MODULE
		);

	function to_string (noun : in type_noun_project) return string;
	function to_noun (noun : in string) return type_noun_project;


	
-- SCHEMATIC
	
	type type_verb_schematic is (
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
		VERB_INVOKE,
		VERB_MAKE,
		VERB_MOVE,
		VERB_MOUNT,
		VERB_PLACE,
		VERB_POSITION,
		VERB_REMOVE,
		VERB_RENAME,
		VERB_RENUMBER,
		VERB_ROTATE,
		VERB_SET,
		VERB_SHOW,
		VERB_UNMOUNT,
		VERB_WRITE,
		VERB_ZOOM		
		);

	function to_string (verb : in type_verb_schematic) return string;
	function to_verb (verb : in string) return type_verb_schematic;
	
	type type_noun_schematic is (
		NOUN_BOM,
		NOUN_CENTER,
		NOUN_CURSOR,
		NOUN_DEVICE,
		NOUN_DEVICES,
		--DEVICE_PARTCODE,
		--DEVICE_PURPOSE,
		--DEVICE_VALUE,
 		NOUN_FIT,
		NOUN_GRID,
		NOUN_INTEGRITY,
		NOUN_JUNCTION,
		NOUN_LABEL,
		NOUN_LEVEL,
		NOUN_NAME,
		NOUN_MODULE,
		NOUN_NET,
		NOUN_NETCHANGER,
		NOUN_NETLISTS,
		NOUN_PARTCODE,
		NOUN_PORT, -- of a submodule instance
		NOUN_PURPOSE,
		NOUN_SCOPE,
		NOUN_SEGMENT, -- net segment
		NOUN_SHEET,
		NOUN_SUBMODULE,
		NOUN_SUBMODULE_FILE,		
		NOUN_SUBMODULES_TREE,
		NOUN_TEXT,
		NOUN_TEXT_SIZE,
		NOUN_UNIT,
-- 		UNIT_NAME,
-- 		UNIT_PARTCODE,
-- 		UNIT_PURPOSE,
		-- 		UNIT_VALUE
		NOUN_VARIANT,
		NOUN_VALUE
		);

	function to_string (noun : in type_noun_schematic) return string;
	function to_noun (noun : in string) return type_noun_schematic;


	
-- BOARD
	
	type type_verb_board is (
		VERB_ADD,
		VERB_DELETE,
		VERB_DISPLAY,
		--DRAG,
		VERB_DRAW,		
		VERB_FLIP,
		VERB_MAKE,
		VERB_MOVE,
		--PLACE,
		VERB_POSITION,
		VERB_RIPUP,
		VERB_ROTATE,
		VERB_ROUTE,
		VERB_SET,
		--VERB_SHOW CS
		VERB_ZOOM
		--WRITE
		);

	function to_string (verb : in type_verb_board) return string;
	function to_verb (verb : in string) return type_verb_board;
	

	type type_noun_board is (
		NOUN_ASSY, -- assembly documentation
		NOUN_BOARD,
		NOUN_CENTER,
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
		NOUN_OUTLINE,
		NOUN_PARTCODE,
		NOUN_PNP, -- pick & place
		NOUN_PURPOSE,
		NOUN_ROUTE_RESTRICT,
		NOUN_SILK, -- silk screen
		NOUN_STENCIL, -- solder mask
		NOUN_STOP, -- solder stop mask
		NOUN_SUBMODULE,
		NOUN_TEXT,
		NOUN_TEXT_SIZE,
		NOUN_TEXT_LINE_WIDTH,
-- 		TRACK,
		NOUN_VALUE,
		NOUN_VIA,
		NOUN_VIA_DRILL,
		NOUN_VIA_RESTRICT
		);

	function to_string (noun : in type_noun_board) return string;
	function to_noun (noun : in string) return type_noun_board;


-- CANVAS
	
	type type_verb_canvas is (
		VERB_DISPLAY,
		VERB_POSITION,
		VERB_SHOW,
		VERB_ZOOM		
		);

	-- Removes the verb_prefix from given verb and returns the remainder as string.
	-- VERB_DISPLAY becomes DISPLAY:
	function to_string (verb : in type_verb_canvas) return string;

	-- Prepends the verb_prefix to the given string and returns a type_verb_canvas.
	-- DISPLAY becomes VERB_DISPLAY:
	function to_verb (verb : in string) return type_verb_canvas;

	-- Returns true if the given verb (as string) is a canvas related verb.
-- 	function is_canvas_related (verb : in string) return boolean;

	type type_noun_canvas is (
		NOUN_CENTER,
		NOUN_CURSOR,
		NOUN_DEVICE,
		NOUN_FIT,
		NOUN_LEVEL,
		NOUN_MODULE,
		NOUN_SHEET
		);

	-- Removes the noun_prefix from given noun and returns the remainder as string.
	-- NOUNT_FIT becomes FIT:
	function to_string (noun : in type_noun_canvas) return string;

	-- Prepends the noun_prefix to the given string and returns a type_noun_canvas.
	-- FIT becomes NOUN_FIT:
	function to_noun (noun : in string) return type_noun_canvas;




-- 	keyword_from			: constant string := "from";
	keyword_to				: constant string := "to";
	keyword_direction		: constant string := "direction";
-- 	keyword_length			: constant string := "length";

	function schematic_cmd (
		cmd				: in type_fields_of_line;
		log_threshold	: in type_log_level)
		return type_exit_code;

	function board_cmd (
		cmd				: in type_fields_of_line;
		log_threshold	: in type_log_level)
		return type_exit_code;

	-- Executes a script command. 
	-- CS: currently commands related to the canvas are skipped.
	function execute_command (
		file_name		: in type_script_name.bounded_string;
		cmd				: in type_fields_of_line;
		log_threshold	: in type_log_level)
		return type_exit_code;
	
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
