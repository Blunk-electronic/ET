------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            COMMAND STATUS                                --
--                                                                          --
--                               B o d y                                    --
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

with ada.text_io;					use ada.text_io;

with et_exceptions;					use et_exceptions;



package body et_cmd_sts is


	

	function to_string (
		origin : in type_cmd_origin) 
		return string
	is begin
		return type_cmd_origin'image (origin);
	end to_string;



	procedure set_origin (
		cmd		: in out type_single_cmd;
		origin	: in type_cmd_origin)
	is begin
		cmd.origin := origin;
	end;
	


	
	function get_origin (
		cmd		: in type_single_cmd)
		return type_cmd_origin
	is begin
		return cmd.origin;
	end;



	function get_origin (
		cmd		: in type_single_cmd)
		return string
	is begin
		return to_string (cmd.origin);
	end;

	

	
	function to_single_cmd (
		fields	: in type_fields_of_line;
		origin	: in type_cmd_origin)
		return type_single_cmd
	is 
		cmd : type_single_cmd := (
			fields => fields, 
			origin	=> origin,						 
			others => <>);
	begin
		return cmd;
	end;




	function get_line_number (
		cmd		: in type_single_cmd)
		return positive
	is begin
		return get_line_number (cmd.fields);
	end;



	function get_line_number (
		cmd		: in type_single_cmd)
		return string
	is begin
		return "line" & positive'image (get_line_number (cmd.fields)) & ": ";
	end;


	
	

	procedure set_fields (
		cmd		: in out type_single_cmd;
		fields	: in type_fields_of_line)
	is begin
		cmd.fields := fields;
	end;



	function get_field (
		cmd		: in type_single_cmd;
		place	: in type_field_count)
		return string
	is begin
		return get_field (cmd.fields, place);
	end;



	
	function get_fields (
		cmd		: in type_single_cmd)
		return type_fields_of_line
	is begin
		return cmd.fields;
	end;


	
	
	function get_all_fields (
		cmd		: in type_single_cmd)
		return string
	is begin
		return to_string (cmd.fields);
	end;


	

	function get_field_count (
		cmd		: in type_single_cmd)
		return natural
	is begin
		return natural (get_field_count (cmd.fields));
	end;


	

	function is_complete (
		cmd		: in type_single_cmd)
		return boolean
	is begin
		return cmd.complete;
	end;



	procedure set_complete (
		cmd		: in out type_single_cmd)
	is begin
		cmd.complete := true;
	end;
	

	procedure set_incomplete (
		cmd		: in out type_single_cmd)
	is begin
		cmd.complete := false;
	end;
	


	function finalization_is_pending (
		cmd		: in type_single_cmd)
		return boolean
	is begin
		return cmd.finalization_pending;
	end;



	procedure set_finalization_pending (
		cmd		: in out type_single_cmd)
	is begin
		cmd.finalization_pending := true;
	end;
	


	function to_string (
		exit_code : in type_exit_code_command)
		return string
	is begin
		return type_exit_code_command'image (exit_code);
	end;


	
	function get_exit_code (
		cmd		: in type_single_cmd)
		return type_exit_code_command
	is begin
		return cmd.exit_code;
	end;


	

	procedure set_exit_code (
		cmd		: in out type_single_cmd;
		code	: in type_exit_code_command)
	is begin
		cmd.exit_code := code;
	end;


	

	procedure reset_cmd (
		cmd		: in out type_single_cmd)
	is begin
		cmd := (others	=> <>);
	end;

	


	procedure invalid_keyword (
		field : in type_field_count) 
	is begin
		log (ERROR, "invalid keyword in field no." 
			 & type_field_count'image (field) & " !",
			 console => true);
		raise constraint_error;
	end;

	

	
	procedure log_command_incomplete (
		field_count		: in type_field_count;
		log_threshold	: in type_log_level)
	is begin
		log (text => incomplete 
			& "Only" & type_field_count'image (field_count) 
			& " arguments provided. "
			& "Proposing arguments ...", 
			level => log_threshold);
	end log_command_incomplete;



	
	procedure command_incomplete (
		cmd : in out type_single_cmd)
	is begin
		case cmd.origin is
			when ORIGIN_CONSOLE =>
				-- If a single command is given, then
				-- clear the "complete" flag so that further
				-- actions are proposed to the operator:
				cmd.complete := false;

			when ORIGIN_SCRIPT =>
				-- If command is executed via script then
				-- raise exception so that the script execution
				-- is stopped:
				raise exception_command_incomplete with "Command not complete !";
		end case;
	end command_incomplete;


	


	
	procedure command_too_long (
		fields	: in type_fields_of_line;
		from	: in type_field_count) 
	is begin
		log (WARNING, "command " & enclose_in_quotes (to_string (fields)) 
			 & " too long !",
			 console => true);
		
		log (text => " -> Excessive arguments after no." 
			 & type_field_count'image (from) & " ignored !",
			 console => true);
	end;



	
end et_cmd_sts;

