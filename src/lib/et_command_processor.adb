------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          COMMAND PROCESSOR                               --
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
with ada.characters;				use ada.characters;
with ada.characters.handling;		use ada.characters.handling;
with ada.strings; 					use ada.strings;
with ada.strings.fixed; 			use ada.strings.fixed;

with ada.exceptions;
with ada.directories;
with gnat.directory_operations;

with et_exceptions;					use et_exceptions;

with et_domains;
with et_project;

with et_module_names;				use et_module_names;
with et_keywords;					use et_keywords;
with et_directions;					use et_directions;

with et_script_processor;


package body et_command_processor is

-- CS move this stuff to a separate package:

	-- procedure expect_number (field : in count_type) is begin
	-- 	log (ERROR, "number expected in field no." & count_type'image (field) & " !", console => true);
	-- 	raise constraint_error;
	-- end;

	
	-- procedure expect_fill_style (style : in type_fill_style; field : in count_type) is begin
	-- 	log (ERROR, "fill style " & enclose_in_quotes (to_string (style)) &
	-- 		 " expected in field no. " & count_type'image (field) & " !" , console => true);
	-- 	raise constraint_error;
	-- end;

	
	-- procedure expect_value_center_x (field : in count_type) is begin
	-- 	log (ERROR, "Expect value for center x in field no." & count_type'image (field) & " !",
	-- 		 console => true);
	-- 	raise constraint_error;
	-- end;

	
	-- procedure expect_keyword_filled (field : in count_type) is begin
	-- 	log (ERROR, "Expect keyword " & enclose_in_quotes (keyword_filled) &
	-- 		" in field no." & count_type'image (field) & " !",
	-- 		 console => true);
	-- 	raise constraint_error;
	-- end;


	


	
	procedure parse_execute_script (
		cmd				: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is 
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);

	
		-- This procedure is to be called when the command is complete.
		-- It lauches the given script and sets the exit code of
		-- the command according to the outcome of the script execution:
		procedure command_complete is
			use et_script_processor;
			exit_code : type_exit_code_script;
		begin
			exit_code := execute_nested_script (
				file			=> get_field (cmd, 5),
				log_threshold	=> log_threshold + 1);

			case exit_code is
				when SUCCESSFUL =>
					set_exit_code (cmd, 0);

				when others =>
					set_exit_code (cmd, 3);
			end case;
		end command_complete;

		
	begin
		case get_origin (cmd) is
			when ORIGIN_CONSOLE =>
				
				case cmd_field_count is
					when 4 => 
						-- Command is incomplete like "execute script"
						command_incomplete (cmd);
						
					when 5 =>
						-- Command is complete like "execute script demo.scr"
						command_complete;
								
					when 6 .. type_field_count'last =>
						command_too_long (cmd, cmd_field_count - 1);


					when others => null;
						-- CS should never happen
						raise constraint_error;
						
				end case;

				
			when ORIGIN_SCRIPT =>
				case cmd_field_count is
					when 5 => 
						-- Command is complete like "execute script demo.scr"
						command_complete;
						
					when 6 .. type_field_count'last =>
						command_too_long (cmd, cmd_field_count - 1);

					when others =>
						command_incomplete (cmd);
						
				end case;
		end case;
	end parse_execute_script;

	


	
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
	
											

	
	
	
	procedure execute_schematic_command (
		module_cursor	: in pac_generic_modules.cursor;
		cmd				: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is separate;



	
	
	procedure execute_board_command (
		module_cursor	: in pac_generic_modules.cursor;
		cmd				: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is separate;




	procedure execute_project_command (
		cmd				: in out type_single_cmd;
		verb			: in type_verb_project;
		noun 			: in type_noun_project;
		log_threshold	: in type_log_level)
	is separate;

	

	
	
	procedure execute_script_command (
		script_name		: in pac_script_name.bounded_string;
		cmd				: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is

		-- Get the number of fields of the given command:
		field_count : constant natural := get_field_count (cmd);


		-- This procedure is a shortcut. 
		-- Call it in case the given command is incomplete:
		procedure command_incomplete is begin
			command_incomplete (cmd);
		end;

		
		use et_domains;
		use et_project;
		
		domain	: type_domain; -- DOM_SCHEMATIC
		module	: pac_module_name.bounded_string; -- motor_driver (without extension *.mod)

		verb_project	: type_verb_project;
		noun_project	: type_noun_project;

		
		
	begin
		log (text => "execute script command: " & enclose_in_quotes (get_all_fields (cmd)),
			 level => log_threshold);
		
		log_indentation_up;
		
		
		-- The command must have at least two fields:
		if field_count >= 2 then

			-- field 1 contains the domain of operation
			domain := to_domain (get_field (cmd, 1));

			-- Dispatch the command to schematic, board or project:
			case domain is
				when DOM_SCHEMATIC =>
					-- CS put the following stuff in a procedure:
					
					module := to_module_name (get_field (cmd, 2));
					-- CS character and length check

					-- We are inside the project directory right now.
					-- The module should be visible either because it is
					-- in the current project directory or because an environment
					-- variable (like $templates/power_supply.mod) directs to
					-- its real location.
					read_module (
						file_name		=> append_extension (to_string (module)), 
						log_threshold	=> log_threshold + 4); 

					
					-- The command must have at least three fields.
					if field_count >= 3 then
						
						execute_schematic_command (
							module_cursor	=> locate_module (module),
							cmd				=> cmd,
							log_threshold	=> log_threshold + 3);

						-- log (text => "schematic command done", level => log_threshold);
					else
						command_incomplete;
					end if;

					
				when DOM_BOARD =>
					-- CS put the following stuff in a procedure:
					
					module := to_module_name (get_field (cmd, 2));
					-- CS character and length check

					-- We are inside the project directory right now.
					-- The module should be visible either because it is
					-- in the current project directory or because a environment
					-- variable (like $templates/power_supply.mod) directs to
					-- its real location.
					read_module (
						file_name		=> append_extension (to_string (module)), 
						log_threshold	=> log_threshold + 4); 

					
					-- The command must have at least three fields.
					if field_count >= 3 then
						
						execute_board_command (
							module_cursor	=> locate_module (module),
							cmd				=> cmd,
							log_threshold	=> log_threshold + 3);

						-- log (text => "board command done", level => log_threshold);
					else
						command_incomplete;
					end if;

					
				when DOM_PROJECT =>
					-- CS put the following stuff in a procedure:
					
					-- CS test minimum field count
					
					verb_project := to_verb (get_field (cmd, 2));
					noun_project := to_noun (get_field (cmd, 3));
					
					-- execute project command
					execute_project_command (cmd, verb_project, noun_project, log_threshold + 3);
			end case;

		else
			command_incomplete;
		end if;


		-- CS do something if command is incomplete and if it
		-- was executed as single command.
		-- like
		-- if not is_complete (cmd) then
		-- 	propose_arguments;
		-- end if;

		-- CS exception handler if command is incomplete
		-- and if it was executed from inside a script
		
		log_indentation_down;


		-- log (text => "done", level => log_threshold);
		
		-- exception when event: others => 
  -- 
		-- 	log_indentation_down;
		-- 	raise;
		
	end execute_script_command;


	
end et_command_processor;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
