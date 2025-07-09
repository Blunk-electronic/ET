------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            COMMAND STATUS                                --
--                                                                          --
--                               B o d y                                    --
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

with ada.text_io;					use ada.text_io;

with et_exceptions;					use et_exceptions;



package body et_cmd_sts is


	function to_string (entry_mode : in type_cmd_entry_mode) return string is begin
		return type_cmd_entry_mode'image (entry_mode);
	end to_string;



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



	
	procedure command_incomplete is begin
		if cmd_entry_mode = MODE_SINGLE_CMD then
			-- If a single command is given, then
			-- clear the "complete" flag so that further
			-- actions are proposed to the operator:
			single_cmd.complete := false;
		else
			-- If command is executed via script then
			-- raise exception so that the script execution
			-- is stopped:
			raise exception_command_incomplete with "Command not complete !";
		end if;
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




	
	procedure too_long is begin
		command_too_long (single_cmd.fields, cmd_field_count - 1);
	end;
	
	
	
	function get_field (place : in type_field_count) 
		return string 
	is begin
		return get_field (single_cmd.fields, place);
	end;


	

	procedure reset_single_cmd is begin
		single_cmd := (others => <>);
	end reset_single_cmd;


	
end et_cmd_sts;

