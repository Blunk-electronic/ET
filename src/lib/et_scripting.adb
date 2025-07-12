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

with ada.characters;			use ada.characters;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;

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


	





	-- Used when executing a script from inside a script
	-- or
	-- when executing a script from inside the GUI:
	procedure execute_nested_script (
		file			: in string; -- like "rename_nets.scr"
		log_threshold	: in type_log_level) 
	is
		use ada.directories;

		-- Backup previous input:
		previous_input : ada.text_io.file_type renames current_input;
		
		file_handle : ada.text_io.file_type;

		-- The text fields of a single command read from the script:
		fields : type_fields_of_line;

		-- The single command to be executed:
		single_cmd : type_single_cmd;

		
		script_name : pac_script_name.bounded_string := to_script_name (file);

		
	begin
		log (text => "current directory: " & enclose_in_quotes (current_directory),
			 level => log_threshold);
		
		log (text => "executing project internal script: " & enclose_in_quotes (to_string (script_name)),
			 level => log_threshold);
		
		log_indentation_up;
		
		-- make sure the script file exists:
		if exists (to_string (script_name)) then

			-- open script file
			open (
				file => file_handle,
				mode => in_file, 
				name => to_string (script_name)); -- demo.scr

			set_input (file_handle);

			-- Prepare the script command status in case a command
			-- in the script fails:
			script_cmd := (script_name => script_name, others => <>);

			-- Prepare the handling of the exception in case the script fails.
			-- See procedures schematic_cmd.evaluate_exception or
			-- board_cmd.evaluate_exception for example.
			-- cmd_entry_mode := MODE_VIA_SCRIPT;
			
			
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

					-- Backup the command to be executed in the script command status
					-- in case the command fails:
					script_cmd.fields := fields;

					
					-- Compose and execute the command to be executed.
					-- Since it is launched via a script, its origin
					-- is set accordingly:
					single_cmd := to_single_cmd (fields, MODE_VIA_SCRIPT);
					
					execute_command (
						script_name		=> script_name, 
						cmd				=> single_cmd,
						log_threshold	=> log_threshold + 1);
					
					-- Procedure execute_command dispatches to subprograms
					-- that execute the command according to the 
					-- targeted domain (first field in fields) like project, 
					-- schematic, board, ...
					-- If the command fails and thus raises an exception,
					-- then the flag script_cmd_status.failed
					-- is set and the error registered. See procedures 
					-- schematic_cmd.evaluate_exception or board_cmd.evaluate_exception
					-- for example.
					-- In case of a nested script, the exception itself is further 
					-- propagated to the top level where the parent script
					-- has been started.
					
				end if;
			end loop;

			--log (text => "closing script file " & enclose_in_quotes (to_string (script_name)), level => log_threshold + 1);
			close (file_handle);

			
		else -- script file not found
			log_indentation_down;

			raise semantic_error_1 with 
				"script file " 
				& enclose_in_quotes (to_string (script_name)) 
				& " not found !";
		end if;

		-- A script can be executed from inside a script (nested scripts).
		-- When the top level script finishes then there might be no previous
		-- input to switch to. So we test whether the previous_input is open
		-- before swtiching back to it:
		if is_open (previous_input) then
			--log (text => "reset to previous input " & name (previous_input) & " ...", level => log_threshold + 1);
			set_input (previous_input);
		end if;
		
		log_indentation_down;

		exception when event: others =>
			if is_open (file_handle) then close (file_handle); end if;
			if is_open (previous_input) then set_input (previous_input); end if;
  
			raise;
	end execute_nested_script;



	
	
	
	procedure schematic_cmd (
		module_cursor	: in pac_generic_modules.cursor;
		cmd				: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is separate;



	
	
	procedure board_cmd (
		module_cursor	: in pac_generic_modules.cursor;
		cmd				: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is separate;



	
	
	procedure execute_command (
		script_name		: in pac_script_name.bounded_string;
		cmd				: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is

		-- This function is a shortcut to get a single field
		-- from the given command:
		-- function f (place : in type_field_count) return string is begin
		-- 	return get_field (fields, place);
		-- end;

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

		
-- 		procedure project_cmd (
-- 			verb : in type_verb_project;
-- 			noun : in type_noun_project) 
-- 		is begin
-- 			case verb is
-- 				when VERB_OPEN =>
-- 					case noun is
-- 						when NOUN_MODULE =>
-- 							case field_count is
-- 								when 4 =>
-- 									-- The script command provides the module name only.
-- 									-- The extension must be added here:
-- 									read_module (
-- 										file_name		=> ada.directories.compose (
-- 															name		=> f (4),
-- 															extension	=> module_file_name_extension),
-- 										log_threshold	=> log_threshold + 1
-- 										);
-- 
-- 								when 5 .. type_field_count'last =>
-- 									command_too_long (fields, field_count - 1);
-- 									
-- 								when others => 
-- 									command_incomplete;
-- 							end case;							
-- 						when others => invalid_noun (to_string (noun));
-- 					end case;
-- 
-- 					
-- 				when VERB_CREATE =>
-- 					case noun is
-- 						when NOUN_MODULE =>
-- 							case field_count is
-- 								when 4 =>
-- 
-- 									create_module (
-- 										module_name		=> to_module_name (f (4)),
-- 										log_threshold	=> log_threshold + 1
-- 										);
-- 
-- 								when 5 .. type_field_count'last =>
-- 									command_too_long (fields, field_count - 1);
-- 									
-- 								when others => 
-- 									command_incomplete;
-- 							end case;							
-- 						when others => invalid_noun (to_string (noun));
-- 					end case;
-- 
-- 					
-- 				when VERB_SAVE =>
-- 					case noun is
-- 						when NOUN_MODULE =>
-- 							case field_count is
-- 								when 4 =>
-- 
-- 									save_module (
-- 										module_name		=> to_module_name (f (4)),
-- 										log_threshold	=> log_threshold + 1
-- 										);
-- 
-- 								when 5 .. type_field_count'last =>
-- 									command_too_long (fields, field_count - 1);
-- 									
-- 								when others => 
-- 									command_incomplete;
-- 							end case;							
-- 						when others => invalid_noun (to_string (noun));
-- 					end case;
-- 
-- 					
-- 				when VERB_DELETE =>
-- 					case noun is
-- 						when NOUN_MODULE =>
-- 							case field_count is
-- 								when 4 =>
-- 
-- 									delete_module (
-- 										module_name		=> to_module_name (f (4)),
-- 										log_threshold	=> log_threshold + 1
-- 										);
-- 
-- 								when 5 .. type_field_count'last =>
-- 									command_too_long (fields, field_count - 1);
-- 									
-- 								when others => 
-- 									command_incomplete;
-- 							end case;							
-- 						when others => invalid_noun (to_string (noun));
-- 					end case;
-- 					
-- 			end case;
-- 		end project_cmd;

		
		
	begin -- execute_command
		log (text => "execute command", level => log_threshold);
		log_indentation_up;
		
		log (text => "command entry mode: " & get_origin (cmd), level => log_threshold + 1);
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
						
						schematic_cmd (
							module_cursor	=> locate_module (module),
							cmd				=> cmd,
							log_threshold	=> log_threshold + 3);

						-- log (text => "schematic command done", level => log_threshold);
					else
						command_incomplete;
					end if;

					
				when DOM_BOARD =>
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
						
						board_cmd (
							module_cursor	=> locate_module (module),
							cmd				=> cmd,
							log_threshold	=> log_threshold + 3);

						-- log (text => "board command done", level => log_threshold);
					else
						command_incomplete;
					end if;

					
				when DOM_PROJECT =>

					-- CS test minimum field count
					
					verb_project := to_verb (get_field (cmd, 2));
					noun_project := to_noun (get_field (cmd, 3));
					
					-- execute rig command
					-- CS project_cmd (verb_project, noun_project);
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

		-- put_line ("execute_command done");

		-- log (text => "done", level => log_threshold);
		
		-- exception when event: others => 
  -- 
		-- 	log_indentation_down;
		-- 	raise;
		
	end execute_command;




	

	
	function execute_script (
		script_name		: in pac_script_name.bounded_string;
		log_threshold	: in type_log_level)
		return type_exit_code 
	is
		
		exit_code : type_exit_code := SUCCESSFUL; -- to be returned

		use ada.directories;

		-- Here we backup the current working directory:
		projects_directory : pac_script_name.bounded_string;

		-- Here we store the directory where the script resides:
		script_directory : pac_script_name.bounded_string;
		
		file_handle : ada.text_io.file_type;
		line : type_fields_of_line;

		-- The command to be executed:
		single_cmd : type_single_cmd;
		
	begin
		
		log (text => row_separator_double, level => log_threshold);
		log (text => "executing script " 
			 & enclose_in_quotes (to_string (script_name)),
			 --& "in mode " & to_string (cmd_entry_mode),
			 level => log_threshold, console => true);
		log_indentation_up;

		-- build the directory where the script is located:
		script_directory := to_script_name (full_name (to_string (script_name)));
		script_directory := to_script_name (containing_directory (to_string (script_directory)));
		
		-- backup the current working directory
		projects_directory := to_script_name (containing_directory (to_string (script_directory)));
		--log (text => "projects directory " & to_string (projects_directory), level => log_threshold);

		-- change in directory where the script is:
		log (text => "changing to directory " & enclose_in_quotes (to_string (script_directory)),
			 level => log_threshold + 1, console => true);
		
		set_directory (to_string (script_directory));

		-- make sure the script file exists:
		if exists (simple_name (to_string (script_name))) then

			-- open script file
			open (
				file => file_handle,
				mode => in_file, 
				name => simple_name (to_string (script_name))); -- demo.scr

			set_input (file_handle);
			
			-- read the file line by line
			while not end_of_file loop
				
				line := read_line (
					line 			=> get_line,
					number			=> positive (ada.text_io.line (current_input)),
					comment_mark 	=> comment_mark,
					delimiter_wrap	=> true, -- strings are enclosed in quotations
					ifs 			=> space); -- fields are separated by space

				-- we are interested in lines that contain something. emtpy lines are skipped:
				if get_field_count (line) > 0 then

					-- Compose and execute the command to be executed.
					-- Since it is launched via a script, its origin
					-- is set accordingly:
					single_cmd := to_single_cmd (line, MODE_VIA_SCRIPT);
					
					execute_command (
						script_name		=> script_name, 
						cmd				=> single_cmd,
						log_threshold	=> log_threshold + 1);
				end if;
			end loop;

			
		else -- script file not found
			log (ERROR, "script file " & 
				 enclose_in_quotes (simple_name (to_string (script_name))) &
				 " not found !", console => true);
			raise constraint_error;
		end if;
															   
		log (text => "returning to directory " & enclose_in_quotes (to_string (projects_directory)),
			 level => log_threshold + 1);
		set_directory (to_string (projects_directory));
		
		log_indentation_down;
		set_input (standard_input);
		close (file_handle);
		
		return exit_code;

		
		exception when event: others =>
			if is_open (file_handle) then close (file_handle); end if;
			set_input (standard_input);

			log (text => exception_name (event), console => true);
			log (text => exception_message (event), console => true);
			
			return ERROR;
		
	end execute_script;



	
end et_scripting;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
