------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          COMMAND PROCESSOR                               --
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

with ada.exceptions;			use ada.exceptions;

with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;

with et_module_ops;				use et_module_ops;
with et_generic_module;			use et_generic_module;
with et_modes;					use et_modes;

with et_cmd_sts;				use et_cmd_sts;
with et_script_names;			use et_script_names;
with et_modes.project;			use et_modes.project;



package et_command_processor is



-- CS move this stuff to a separate package
	device_missing	: constant string := "Device name missing !";
	module_missing	: constant string := "Module name missing !";
	net_missing		: constant string := "Net name missing !";





	-- This procedure parses a command that launches
	-- a script like:
	-- "execute script demo.scr"
	-- This procedure is common to all domains. For this reasone
	-- it is placed in this file.
	-- It calls function execute_nested_script for the final
	-- execution:
	procedure parse_execute_script (
		cmd				: in out type_single_cmd;
		log_threshold	: in type_log_level);



	-- Evaluates the exit code of the given 
	-- command and writes helpful messages in the log file:
	procedure evaluate_command_exit_code (
		cmd				: in type_single_cmd;
		log_threshold	: in type_log_level);


	
	-- Executes a schematic command.
	-- Is called by procedure execute_command whenever a
	-- schematic related command is to be executed:
	procedure execute_schematic_command (
		module_cursor	: in pac_generic_modules.cursor;
		cmd				: in out type_single_cmd;
		log_threshold	: in type_log_level);


		

	
	-- Executes a board command.
	-- Is called by procedure execute_script_command whenever a
	-- board related command is to be executed:
	procedure execute_board_command (
		module_cursor	: in pac_generic_modules.cursor;
		cmd				: in out type_single_cmd;
		log_threshold	: in type_log_level);


	
	-- Executes a project command.
	-- Is called by procedure execute_script_command whenever a
	-- project related command is to be executed:
	procedure execute_project_command (
		cmd				: in out type_single_cmd;
		verb			: in type_verb_project;
		noun 			: in type_noun_project;
		log_threshold	: in type_log_level);
										  


	
	-- Executes a command like 
	-- "schematic motor_driver draw net motor_on 1 150 100 150 130".
	-- Dispatches further to the execution of either schematic, 
	-- board or project commands.
	-- When called, the current working directory must be the
	-- project like my_projects/blood_sample_analyzer.
	procedure execute_script_command (
		-- The script file that contains the command. for debug messages only:
		script_name		: in pac_script_name.bounded_string; 
		-- The text fields like "schematic motor_driver draw net motor_on 1 150 100 150 130":
		cmd				: in out type_single_cmd;
		log_threshold	: in type_log_level);



	
end et_command_processor;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
