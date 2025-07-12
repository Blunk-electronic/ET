------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            COMMAND STATUS                                --
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

with et_string_processing;		use et_string_processing;
with et_script_names;			use et_script_names;
with et_logging;				use et_logging;


package et_cmd_sts is

	-- A commandline command consists of text fields.
	-- It can be entered via:
	-- 1. a script (batch of commands)
	-- 2. the console in the GUI
	--
	-- It is passed to the command processor which in turn
	-- dispatches the command to sub processors for other domains
	-- like project, rig, schematic, board, device, symbol, package, ...
	--

	-- Commands can be entered via:
	-- - the console in the GUI as single command.
	-- - via a script (a batch of commands)
	type type_cmd_entry_mode is (
		MODE_SINGLE_CMD,
		MODE_VIA_SCRIPT);

	
	cmd_entry_mode_default : constant type_cmd_entry_mode := MODE_SINGLE_CMD;


	
	function to_string (
		origin : in type_cmd_entry_mode) 
		return string;


	

	
	type type_single_cmd is record
		origin		: type_cmd_entry_mode := cmd_entry_mode_default;
		
		-- The text fields of the command to be executed like 
		-- "schematic blood_sample_analyzer set value C1 100n"
		fields		: type_fields_of_line;

		-- Goes false if too few arguments given via console:
		complete	: boolean := true;

		-- Indicates that the command is in progress,
		-- but not finalized yet:
		finalization_pending : boolean := false;
	end record;	




	procedure set_origin (
		cmd		: in out type_single_cmd;
		origin	: in type_cmd_entry_mode);
	

	function get_origin (
		cmd		: in type_single_cmd)
		return type_cmd_entry_mode;



	function get_origin (
		cmd		: in type_single_cmd)
		return string;

	
	
	function to_single_cmd (
		fields	: in type_fields_of_line;
		origin	: in type_cmd_entry_mode)
		return type_single_cmd;
	
		
	procedure set_fields (
		cmd		: in out type_single_cmd;
		fields	: in type_fields_of_line);


	function get_field (
		cmd		: in type_single_cmd;
		place	: in type_field_count)
		return string;


	function get_fields (
		cmd		: in type_single_cmd)
		return type_fields_of_line;

	
	function get_all_fields (
		cmd		: in type_single_cmd)
		return string;


	
	function get_field_count (
		cmd		: in type_single_cmd)
		return natural;


	
	function is_complete (
		cmd		: in type_single_cmd)
		return boolean;


	function finalization_is_pending (
		cmd		: in type_single_cmd)
		return boolean;


	procedure set_finalization_pending (
		cmd		: in out type_single_cmd);

										   
	procedure reset_cmd (
		cmd		: in out type_single_cmd);

	
	
	






	procedure invalid_keyword (
		field : in type_field_count);
	
	

	incomplete		: constant string := "Command incomplete ! ";
	

	procedure log_command_incomplete (
		field_count		: in type_field_count;
		log_threshold	: in type_log_level);



	-- A command can be incomplete. 
	-- Depending on the entry mode these actions will be done:
	-- 1. If entry mode is MODE_SINGLE_CMD, then the flag "incomplete" is set
	--    so that further measures can be taken.
	-- 2. If entry mode is MODE_VIA_SCRIPT, then an exception is raised
	--    so that the execution of the current script is aborted.
	procedure command_incomplete (
		cmd	: in out type_single_cmd);
	


	

	procedure command_too_long (
		fields	: in type_fields_of_line;
		from	: in type_field_count);



	

	

	-- In graphical mode, scripts can be nested.
	-- In script mode we register only the first
	-- exception regardless of the nesting depth.
	-- Because the operator needs to know which script
	-- has actually failed at which line.
	-- The failed script will then be output in the status bar.
	-- IN HEADLESS MODE THIS STUFF HAS NO MEANING !
	-- For this reason this type is provided:
	type type_script_cmd is record
		-- the name of the script file like "rename_power_nets.scr":
		script_name	: pac_script_name.bounded_string;

		-- The text fields of the command to be executed like 
		-- "schematic blood_sample_analyzer set value C1 100n"
		fields		: type_fields_of_line;

		-- the flag that indicates whether the command failed
		failed		: boolean := false;
	end record;
	

	-- The global variable that stores the status of the latest
	-- script command.
	-- IN HEADLESS MODE THIS STUFF HAS NO MEANING !
	script_cmd : type_script_cmd;
	

	
end et_cmd_sts;

