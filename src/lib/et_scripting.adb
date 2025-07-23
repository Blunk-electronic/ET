------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             SCRIPTING                                    --
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
with et_primitive_objects;			use et_primitive_objects;
with et_axes;						use et_axes;

with et_display;					use et_display;
with et_display.schematic;
with et_display.board;

with et_canvas_schematic_2;
with et_canvas_board_2;
with et_module_names;				use et_module_names;
with et_keywords;					use et_keywords;


package body et_scripting is


	function to_string (verb : in type_verb_project) return string is 
		s : string := type_verb_project'image (verb);
	begin
		return s (verb_prefix'length + 1 .. s'last);
	end;


	
	function to_verb (verb : in string) return type_verb_project is begin
		return type_verb_project'value (verb_prefix & verb);
	
		exception when event: others => 
			log (ERROR, "verb " & enclose_in_quotes (verb) & " invalid !", console => true);
			raise;
	end;


	
	function to_string (noun : in type_noun_project) return string is 
		s : string := type_noun_project'image (noun);
	begin
		return s (verb_prefix'length + 1 .. s'last);
	end;


	
	function to_noun (noun : in string) return type_noun_project is begin
		return type_noun_project'value (noun_prefix & noun);
	
		exception when event: others => 
			log (ERROR, "noun " & enclose_in_quotes (noun) & " invalid !", console => true);
			raise;
	end;
	

	

	procedure expect_number (field : in count_type) is begin
		log (ERROR, "number expected in field no." & count_type'image (field) & " !", console => true);
		raise constraint_error;
	end;


	
	procedure expect_fill_style (style : in type_fill_style; field : in count_type) is begin
		log (ERROR, "fill style " & enclose_in_quotes (to_string (style)) &
			 " expected in field no. " & count_type'image (field) & " !" , console => true);
		raise constraint_error;
	end;




	
	procedure expect_value_center_x (field : in count_type) is begin
		log (ERROR, "Expect value for center x in field no." & count_type'image (field) & " !",
			 console => true);
		raise constraint_error;
	end;


	
	procedure expect_keyword_filled (field : in count_type) is begin
		log (ERROR, "Expect keyword " & enclose_in_quotes (keyword_filled) &
			" in field no." & count_type'image (field) & " !",
			 console => true);
		raise constraint_error;
	end;


	


	
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
			exit_code : type_exit_code;
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
						set_incomplete (cmd);
						-- NOTE: This is not a failure. For this reason
						-- we do not set an exit code here.
						
					when 5 =>
						-- Command is complete like "execute script demo.scr"
						command_complete;
								
					when 6 .. type_field_count'last =>
						command_too_long (get_fields (cmd), cmd_field_count - 1);

						-- In console mode, too long a command is not accepted.
						-- The "failed" status must be set accordingly:
						set_exit_code (cmd, 2);
						

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
						command_too_long (get_fields (cmd), cmd_field_count - 1);

						-- In script mode, too long a command is not accepted.
						-- The "failed" status must be set accordingly:
						set_exit_code (cmd, 2);
						

					when others => null;
						set_incomplete (cmd);
						
						-- In script mode, an incomplete command is not accepted.
						-- The "failed" status must be set accordingly:
						set_exit_code (cmd, 1);
						
				end case;
		end case;
	end parse_execute_script;

	


	
	procedure evaluate_command_exit_code (
		cmd				: in type_single_cmd;
		log_threshold	: in type_log_level)
	is 
		code : constant et_cmd_sts.type_exit_code := get_exit_code (cmd);
	begin
		case code is
			when 0 => null; -- no errors

			-- when 1 => -- command incomplete
				
			when others =>
				log (text => "exit code" & natural'image (code), 
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

		
		
	begin -- execute_script_command
		log (text => "execute command", level => log_threshold);
		log_indentation_up;
		
		log (text => "command origin: " & get_origin (cmd), level => log_threshold + 1);
		log_indentation_up;
		
		log (text => "fields: " & enclose_in_quotes (get_all_fields (cmd)), level => log_threshold + 2);
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
		log_indentation_down;
		log_indentation_down;

		-- log (text => "done", level => log_threshold);
		
		-- exception when event: others => 
  -- 
		-- 	log_indentation_down;
		-- 	raise;
		
	end execute_script_command;


	



	function read_script (
		file			: in string; -- like "rename_nets.scr"
		log_threshold	: in type_log_level) 
		return type_exit_code
	is
		exit_code : type_exit_code := ERROR;
		
		use ada.directories;
		
		file_handle : file_type;

		-- The text fields of a single command read from the script:
		-- Example: "schematic demo draw net GND 1 90 100 100 100"
		fields : type_fields_of_line;

		-- The single command to be executed:
		cmd : type_single_cmd;
		
		script_name : pac_script_name.bounded_string := to_script_name (file);
		
	begin
		log (text => "read script " & enclose_in_quotes (to_string (script_name)),
			 level => log_threshold);
		
		log_indentation_up;
		

		
		-- make sure the script file exists:
		if exists (to_string (script_name)) then

			-- goto l_end;
			
			-- open script file
			open (
				file => file_handle,
				mode => in_file, 
				name => to_string (script_name)); -- demo.scr

			set_input (file_handle);

			
			-- read the file line by line
			while not end_of_file loop
				
				fields := read_line (
					line 			=> get_line,
					number			=> positive (ada.text_io.line (current_input)),
					comment_mark 	=> comment_mark,
					delimiter_wrap	=> true, -- strings are enclosed in quotations
					ifs 			=> space); -- fields are separated by space

				-- we are interested in lines that contain something. emtpy lines are skipped:
				if get_field_count (fields) > 0 then

					-- Compose and execute the command to be executed.
					-- Since it is launched via a script, its origin
					-- is set accordingly:
					cmd := to_single_cmd (fields, ORIGIN_SCRIPT);

					-- log (text => "A cmd: " & get_all_fields (cmd));
					
					execute_script_command (
						script_name		=> script_name, 
						cmd				=> cmd,
						log_threshold	=> log_threshold + 1);
					
					-- Procedure execute_script_command dispatches to subprograms
					-- that execute the command according to the 
					-- targeted domain (first field in fields) like project, 
					-- schematic, board, ...


					-- CS evaluate cmd status and output line number, hints, etc.
					-- line provides the affected line number
					case get_exit_code (cmd) is
						when 0 => 
							exit_code := SUCCESSFUL; -- no error
							
						when 1 =>
							log (ERROR, "Command incomplete !"); -- CS output line number
							--log (text => "cmd: " & get_all_fields (cmd));
							
							exit; -- abort script execution
							
						when 2 =>
							log (ERROR, "Command too long !"); -- CS output line number
							exit; -- abort script execution

						when 3 =>
							log (ERROR, "Other error."); -- CS output line number
							exit; -- abort script execution

					end case;					
				end if;
			end loop;

			
			-- log (text => "close script file " & to_string (script_name),
				 -- level => log_threshold + 1);
			
			close (file_handle);

			-- << l_end >>
			
		else -- script file not found

			-- raise semantic_error_1 with 
			-- 	"script file " 
			-- 	& enclose_in_quotes (to_string (script_name)) 
			-- 	& " not found !";

			log (ERROR, "script file " 
				 & enclose_in_quotes (to_string (script_name)) 
				 & " not found !", console => true);
					
		end if;

		
		log_indentation_down;


		-- log (text => "done", level => log_threshold);

		return exit_code;
		
		
		exception when event: others =>
			log_indentation_down;
			log (text => ada.exceptions.exception_information (event));
		
			if is_open (file_handle) then 
				close (file_handle); 
			end if;

			return exit_code;
	end read_script;

	

	

	

	function execute_nested_script (
		file			: in string; -- like "rename_nets.scr"
		log_threshold	: in type_log_level) 
		return type_exit_code
	is
		exit_code : type_exit_code := ERROR;
		
		use ada.directories;

		-- Backup previous input:
		previous_input : ada.text_io.file_type renames current_input;
				
	begin
		log (text => "execute nested script in directory: " 
			 & enclose_in_quotes (current_directory),
			 level => log_threshold);
		
		log (text => "executing project internal script: " & file,
			 level => log_threshold);
		
		log_indentation_up;

		exit_code := read_script (file, log_threshold + 1);
						

		-- A script can be executed from inside a script (nested scripts).
		-- When the top level script finishes, then there might be no previous
		-- input to switch to. So we test whether the previous_input is open
		-- before swtiching back to it:
		if is_open (previous_input) then
			--log (text => "reset to previous input " & name (previous_input) & " ...", level => log_threshold + 1);
			set_input (previous_input);
		end if;

		
		log_indentation_down;

		return exit_code;
		

		exception when event: others =>
			log (text => ada.exceptions.exception_information (event));
	
			if is_open (previous_input) then
				set_input (previous_input);
			end if;
  
			return exit_code;
	end execute_nested_script;



	

	
	function execute_script_headless (
		script_name		: in pac_script_name.bounded_string;
		log_threshold	: in type_log_level)
		return type_exit_code 
	is		
		exit_code : type_exit_code := ERROR;

		use ada.directories;

		-- Here we backup the current working directory:
		projects_directory : pac_script_name.bounded_string;

		-- Here we store the directory where the script resides:
		script_directory : pac_script_name.bounded_string;

	begin		
		log (text => row_separator_double, level => log_threshold);
		log (text => "executing script in headless mode: " 
			 & enclose_in_quotes (to_string (script_name)),
			 level => log_threshold, console => true);

		log_indentation_up;

		-- build the directory where the script is located:
		script_directory := to_script_name (full_name (to_string (script_name)));
		script_directory := to_script_name (containing_directory (to_string (script_directory)));
		
		-- backup the current working directory
		projects_directory := to_script_name (containing_directory (to_string (script_directory)));
		--log (text => "projects directory " & to_string (projects_directory), level => log_threshold);

		-- change in directory where the script is:
		log (text => "change to directory " 
			 & enclose_in_quotes (to_string (script_directory)),
			 level => log_threshold + 1);
		
		set_directory (to_string (script_directory));

		-- Do the actual execution of the script:
		log_indentation_up;
		exit_code := read_script (simple_name (to_string (script_name)), log_threshold + 2);
		log_indentation_down;
		
		log (text => "return to directory " 
			 & enclose_in_quotes (to_string (projects_directory)),
			 level => log_threshold + 1);
		
		set_directory (to_string (projects_directory));
		
		set_input (standard_input);
		
		log_indentation_down;

		
		return exit_code;

		
		exception when event: others =>
			set_input (standard_input);

			log (text => ada.exceptions.exception_information (event));
			log (text => exception_name (event), console => true);
			log (text => exception_message (event), console => true);
			
			return exit_code;
		
	end execute_script_headless;



	
end et_scripting;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
