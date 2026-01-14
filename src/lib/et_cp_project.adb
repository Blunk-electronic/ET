------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       COMMAND PROCESSOR / PROJECT                        --
--                                                                          --
--                               B o d y                                    --
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

--   For correct displaying set tab with in your edtior to 4.

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

with ada.exceptions;
with ada.directories;
with gnat.directory_operations;

with et_runmode;					use et_runmode;
with et_domains;
with et_project;

with et_module_names;				use et_module_names;

with et_modes;						use et_modes;
with et_module_ops;					use et_module_ops;
with et_module_read;				use et_module_read;

with et_script_processor;




package body et_cp_project is

	
	procedure evaluate_command_exit_code (
		cmd				: in type_single_cmd;
		log_threshold	: in type_log_level)
	is 
		code : constant type_exit_code_command := get_exit_code (cmd);
	begin
		case code is
			when 0 => null; -- no errors

			when 1 => -- command incomplete
				log (ERROR, "Command incomplete. Exit code" & to_string (code), 
					level => log_threshold);

			when 2 => -- command too long
				log (ERROR, "Command too long. Exit code" & to_string (code), 
					level => log_threshold);

			when others =>
				log (ERROR, "Other error. Exit code" & to_string (code), 
					level => log_threshold);
		end case;
	end evaluate_command_exit_code;


	
											

	
	

	procedure execute_project_command (
		cmd				: in out type_single_cmd;
		verb			: in type_verb_project;
		noun 			: in type_noun_project;
		log_threshold	: in type_log_level)
	is

		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);


		-- This procedure is a shortcut. 
		-- Call it in case the given command is too long:
		procedure too_long is begin
			command_too_long (cmd, cmd_field_count - 1);
		end;


		-- This procedure is a shortcut. 
		-- Call it in case the given command is incomplete:
		procedure command_incomplete is begin
			command_incomplete (cmd);
		end;

		
		-- This function is a shortcut to get a single field
		-- from the given command:
		function get_field (place : in type_field_count) 
			return string 
		is begin
			return get_field (cmd, place);
		end;
		

		
		-- Parses the given command and dispatches to
		-- further subroutines:
		procedure parse is begin
			case verb is
				when VERB_OPEN =>
					case noun is
						when NOUN_MODULE =>
							case cmd_field_count is
								when 4 =>
									-- The script command provides the module name only.
									-- The extension must be added here:
									read_module (
										file_name		=> ada.directories.compose (
															name		=> get_field (4),
															extension	=> module_file_name_extension),
										log_threshold	=> log_threshold + 1);

								when 5 .. type_field_count'last => too_long;
									
								when others => command_incomplete;
							end case;							
							
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_CREATE =>
					case noun is
						when NOUN_MODULE =>
							case cmd_field_count is
								when 4 =>
									create_module (
										module_name		=> to_module_name (get_field (4)),
										log_threshold	=> log_threshold + 1);
							
								when 5 .. type_field_count'last => too_long;
									
								when others => command_incomplete;
							end case;							
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_SAVE =>
					case noun is
						when NOUN_MODULE =>
							case cmd_field_count is
								when 4 =>
									save_module (
										module_name		=> to_module_name (get_field (4)),
										log_threshold	=> log_threshold + 1);

								when 5 .. type_field_count'last => too_long;
									
								when others => command_incomplete;
							end case;			
							
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_DELETE =>
					case noun is
						when NOUN_MODULE =>
							case cmd_field_count is
								when 4 =>
									delete_module (
										module_name		=> to_module_name (get_field (4)),
										log_threshold	=> log_threshold + 1);

								when 5 .. type_field_count'last => too_long;
									
								when others => command_incomplete;
							end case;			
							
						when others => invalid_noun (to_string (noun));
					end case;
					
			end case;
		end parse;


		
	begin
		log (text => "execute project command: " & enclose_in_quotes (get_all_fields (cmd)),
			level => log_threshold);

		log (text => "command origin: " & get_origin (cmd), level => log_threshold);
		
		
		-- parse the command:
		parse;
		
		
		-- CS propose_arguments;

		evaluate_command_exit_code (cmd, log_threshold);

		
	end execute_project_command;




	
end et_cp_project;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
