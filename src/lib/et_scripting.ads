------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             SCRIPTING                                    --
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
-- <http://www.gnu.org/licenses/>.
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
with ada.containers;			use ada.containers;
with ada.exceptions;			use ada.exceptions;

with et_device_name;
with et_net_names;				use et_net_names;
with et_text;					use et_text;
with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;

with et_module_ops;				use et_module_ops;
with et_generic_module;			use et_generic_module;
with et_modes;					use et_modes;

with et_cmd_sts;				use et_cmd_sts;
with et_script_names;			use et_script_names;


package et_scripting is -- CS rename to et_command_processor

	use pac_net_name;

	
	comment_mark : constant string := ("#");

	-- Script files have a name like "rename_power_nets.scr":
	script_name : pac_script_name.bounded_string;




	device_missing	: constant string := "Device name missing !";
	module_missing	: constant string := "Module name missing !";
	net_missing		: constant string := "Net name missing !";


	
	
	type type_exit_code is (
		SUCCESSFUL,
		WARNINGS,
		ERROR
		);


	
	type type_verb_project is ( -- CS move to et_modes.project
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





	-- Used when executing a script from inside a script
	-- or
	-- when executing a script from inside the GUI.
	-- Calls procedure execute_command in the course of
	-- executing the given script:
	procedure execute_nested_script (
		file			: in string; -- like "rename_nets.scr"
		log_threshold	: in type_log_level);

	
	
	-- Executes a schematic command.
	-- Is called by procedure execute_command whenever a
	-- schematic related command is to be executed:
	procedure execute_schematic_command (
		module_cursor	: in pac_generic_modules.cursor;
		cmd				: in out type_single_cmd;
		log_threshold	: in type_log_level);

	
	polygon_log_category : type_log_category := log_category_default;
	
	
	-- Executes a board command.
	-- Is called by procedure execute_command whenever a
	-- board related command is to be executed:
	procedure execute_board_command (
		module_cursor	: in pac_generic_modules.cursor;
		cmd				: in out type_single_cmd;
		log_threshold	: in type_log_level);


	-- Executes a project command.
	-- Is called by procedure execute_command whenever a
	-- project related command is to be executed:
	procedure execute_project_command (
		cmd				: in out type_single_cmd;
		verb			: in type_verb_project;
		noun 			: in type_noun_project;
		log_threshold	: in type_log_level);
										  
	
	-- Executes a command like 
	-- "schematic motor_driver draw net motor_on 1 150 100 150 130".
	-- This procedure is called by function execute_script (see below) or
	-- by procedure execute_nested_script.
	-- Ensures that the targeted module (like motor_driver) exists.
	-- Dispatches further to the execution of either schematic, 
	-- board or project commands.
	-- When called, the current working directory must be the
	-- project like my_projects/blood_sample_analyzer.
	procedure execute_command ( -- CS rename to execute_script_command
		-- The script file that contains the command. for debug messages only:
		script_name		: in pac_script_name.bounded_string; 
		-- The text fields like "schematic motor_driver draw net motor_on 1 150 100 150 130":
		cmd				: in out type_single_cmd;
		log_threshold	: in type_log_level);



	
	-- Executes the given script file like "dummy_module/my_script.scr".
	-- Changes into the directory where the script lives and starts
	-- execution there.
	-- 1. This function should be called to execute a script on launching
	--    ET via command line. 
	-- 2. This function is NOT intended to launch a script
	--    from inside a script ! This would be a nested script.
	--    For this mode the procedure execute_nested_script is provided.
	function execute_script (
		script_name		: in pac_script_name.bounded_string;
		log_threshold	: in type_log_level)
		return type_exit_code;


	
end et_scripting;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
