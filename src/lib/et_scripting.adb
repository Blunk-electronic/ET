------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             SCRIPTING                                    --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2023                                                -- 
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

with et_exceptions;				use et_exceptions;

with et_project;
with et_project.modules;

with et_coordinates_2;
with et_pcb_coordinates_2;
with et_schematic;
with et_schematic_ops;
with et_schematic_ops.nets;
with et_schematic_ops.units;
with et_terminals;
with et_packages;
with et_vias;
with et_pcb;
with et_pcb_stack;
with et_board_ops;

with et_submodules;
with et_assembly_variants;
with et_pick_and_place;
with et_material;
with et_netlists;
with et_geometry;				use et_geometry; -- due to frequently used keywords
with et_symbols;
with et_devices;				use et_devices;
with et_display;				use et_display;
with et_display.schematic;
with et_display.board;


with glib;
with gtk.main;

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

	procedure invalid_keyword (field : in count_type) is begin
		log (ERROR, "invalid keyword in field no." & count_type'image (field) & " !",
			 console => true);
		raise constraint_error;
	end;

	procedure expect_value_center_x (field : in count_type) is begin
		log (ERROR, "Expect value for center x in field no." & count_type'image (field) & " !",
			 console => true);
		raise constraint_error;
	end;

	procedure expect_keyword_filled (field : in count_type) is begin
		log (ERROR, "Expect keyword " & enclose_in_quotes (et_geometry.keyword_filled) &
			" in field no." & count_type'image (field) & " !",
			 console => true);
		raise constraint_error;
	end;

	procedure invalid_noun (noun : in string) is begin
		raise semantic_error_1 with
			"ERROR: Noun " & enclose_in_quotes (noun) & " invalid for this operation !";
	end invalid_noun;

	procedure log_command_incomplete (
		field_count		: in count_type;
		log_threshold	: in type_log_level)
	is begin
		log (text => incomplete 
			& "Only" & count_type'image (field_count) & " arguments provided. "
			& "Proposing arguments ...", 
			level => log_threshold);
	end log_command_incomplete;
	
	procedure command_incomplete is begin
		--if runmode /= MODE_HEADLESS and cmd_entry_mode = SINGLE_CMD then
			--single_cmd_status.complete := false;
		--else
			raise exception_command_incomplete with "command not complete";
		--end if;
	end command_incomplete;

	procedure command_too_long (
		cmd		: in type_fields_of_line;
		from	: in count_type) 
	is begin
		log (WARNING, "command " & enclose_in_quotes (to_string (cmd)) & " too long !",
			 console => true);
		log (text => " -> Excessive arguments after no." & count_type'image (from) & " ignored !",
			 console => true);
	end;

	procedure skipped_in_this_runmode (log_threshold : in type_log_level) is begin
		log (text => "skipped in current runmode "
			& to_string (runmode), level => log_threshold);
	end skipped_in_this_runmode;
	
	procedure validate_module_name (module : in pac_module_name.bounded_string) is 
		use et_project.modules;
	begin
		if not exists (module) then
			log (ERROR, "module " & to_string (module) &
				" not found !", console => true);
			raise constraint_error;
		end if;
	end;




	-- Used when executing a script from inside a script
	-- or
	-- when executing a script from inside the GUI:
	procedure execute_nested_script (
		file			: in string; -- like "rename_nets.scr"
		log_threshold	: in type_log_level) 
	is
		use ada.directories;
		-- use et_canvas_schematic_2;

		-- Backup previous input:
		previous_input : ada.text_io.file_type renames current_input;
		
		file_handle : ada.text_io.file_type;

		-- The command read from the script:
		cmd : type_fields_of_line;

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
			script_cmd_status := (script_name => script_name, others => <>);

			-- Prepare the handling of the exception in case the script fails.
			-- See procedures schematic_cmd.evaluate_exception or
			-- board_cmd.evaluate_exception for example.
			cmd_entry_mode := VIA_SCRIPT;
			
			-- read the file line by line
			while not end_of_file loop
				
				cmd := read_line (
					line 			=> get_line,
					number			=> ada.text_io.line (current_input),
					comment_mark 	=> comment_mark,
					delimiter_wrap	=> true, -- strings are enclosed in quotations
					ifs 			=> space); -- fields are separated by space

				-- we are interested in lines that contain something. emtpy lines are skipped:
				if field_count (cmd) > 0 then

					-- Backup the command to be executed in the script command status
					-- in case the command fails:
					script_cmd_status.cmd := cmd;
					
					execute_command (script_name, cmd, log_threshold + 1);
					-- Procedure execute_command dispatches to subprograms
					-- that execute the command according to the 
					-- targeted domain (first field in cmd) like project, 
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
		cmd_in			: in type_fields_of_line; -- "schematic motor_driver draw net motor_on 1 150 100 150 130"
		log_threshold	: in type_log_level)
	is separate;

	procedure board_cmd (
		module_cursor	: in pac_generic_modules.cursor;
		cmd_in			: in type_fields_of_line; -- "board tree_1 draw silk top line 2.5 0 0 160 0"
		log_threshold	: in type_log_level)
	is separate;


	
	procedure execute_command (
		file_name		: in pac_script_name.bounded_string; -- for debug messages only
		cmd				: in type_fields_of_line;
		log_threshold	: in type_log_level)
	is
		function f (place : in count_type) return string is begin
			return get_field (cmd, place);
		end;

		function fields return count_type is begin
			return field_count (cmd);
		end;
		
		use et_project;
		use et_project.modules;
		
		domain	: type_domain; -- DOM_SCHEMATIC
		module	: pac_module_name.bounded_string; -- motor_driver (without extension *.mod)

		verb_project	: type_verb_project;
		noun_project	: type_noun_project;

		procedure project_cmd (
			verb : in type_verb_project;
			noun : in type_noun_project) 
		is begin
			case verb is
				when VERB_OPEN =>
					case noun is
						when NOUN_MODULE =>
							case fields is
								when 4 =>
									-- The script command provides the module name only.
									-- The extension must be added here:
									read_module (
										file_name		=> ada.directories.compose (
															name		=> f (4),
															extension	=> module_file_name_extension),
										log_threshold	=> log_threshold + 1
										);

								when 5 .. count_type'last =>
									command_too_long (cmd, fields - 1);
									
								when others => 
									command_incomplete;
							end case;							
						when others => invalid_noun (to_string (noun));
					end case;

				when VERB_CREATE =>
					case noun is
						when NOUN_MODULE =>
							case fields is
								when 4 =>

									create_module (
										module_name		=> to_module_name (f (4)),
										log_threshold	=> log_threshold + 1
										);

								when 5 .. count_type'last =>
									command_too_long (cmd, fields - 1);
									
								when others => 
									command_incomplete;
							end case;							
						when others => invalid_noun (to_string (noun));
					end case;

				when VERB_SAVE =>
					case noun is
						when NOUN_MODULE =>
							case fields is
								when 4 =>

									save_module (
										module_name		=> to_module_name (f (4)),
										log_threshold	=> log_threshold + 1
										);

								when 5 .. count_type'last =>
									command_too_long (cmd, fields - 1);
									
								when others => 
									command_incomplete;
							end case;							
						when others => invalid_noun (to_string (noun));
					end case;
					
				when VERB_DELETE =>
					case noun is
						when NOUN_MODULE =>
							case fields is
								when 4 =>

									delete_module (
										module_name		=> to_module_name (f (4)),
										log_threshold	=> log_threshold + 1
										);

								when 5 .. count_type'last =>
									command_too_long (cmd, fields - 1);
									
								when others => 
									command_incomplete;
							end case;							
						when others => invalid_noun (to_string (noun));
					end case;
					
			end case;
		end project_cmd;
		
	begin -- execute_command
		log (text => "cmd --> " & enclose_in_quotes (to_string (cmd)), level => log_threshold);
		log_indentation_up;

		-- The command must have at least two fields:
		if field_count (cmd) >= 2 then

			-- field 1 contains the domain of operation
			domain := to_domain (f (1));

			-- Dispatch the command to schematic, board or project:
			case domain is
				when DOM_SCHEMATIC =>
					module := to_module_name (f (2));
					-- CS character and length check

					-- We are inside the project directory right now.
					-- The module should be visible either because it is
					-- in the current project directory or because an environment
					-- variable (like $templates/power_supply.mod) directs to
					-- its real location.
					read_module (
						file_name		=> append_extension (to_string (module)), 
						log_threshold	=> log_threshold + 1); 

					-- The command must have at least three fields.
					if field_count (cmd) >= 3 then
						schematic_cmd (
							module_cursor	=> locate_module (module),
							cmd_in			=> cmd,
							log_threshold	=> log_threshold + 1);
					else
						command_incomplete;
					end if;

					
				when DOM_BOARD =>
					module := to_module_name (f (2));
					-- CS character and length check

					-- We are inside the project directory right now.
					-- The module should be visible either because it is
					-- in the current project directory or because a environment
					-- variable (like $templates/power_supply.mod) directs to
					-- its real location.
					read_module (
						file_name		=> append_extension (to_string (module)), 
						log_threshold	=> log_threshold + 1); 

					-- The command must have at least three fields.
					if field_count (cmd) >= 3 then
						board_cmd (
							module_cursor	=> locate_module (module),
							cmd_in			=> cmd,
							log_threshold	=> log_threshold + 1);
					else
						command_incomplete;
					end if;

					
				when DOM_PROJECT =>

					-- CS test minimum field count
					
					verb_project := to_verb (f (2));
					noun_project := to_noun (f (3));
					
					-- execute rig command
					project_cmd (verb_project, noun_project);
			end case;

		else
			command_incomplete;
		end if;
		
		log_indentation_down;

		
		exception when event: others => 

			log_indentation_down;
			raise;
		
	end execute_command;


	function execute_script (
		file_name		: in pac_script_name.bounded_string; -- dummy_module/my_script.scr
		log_threshold	: in type_log_level)
		return type_exit_code is
		
		exit_code : type_exit_code := SUCCESSFUL; -- to be returned

		use ada.directories;

		-- Here we backup the current working directory:
		projects_directory : pac_script_name.bounded_string;

		-- Here we store the directory where the script resides:
		script_directory : pac_script_name.bounded_string;
		
		file_handle : ada.text_io.file_type;
		line : type_fields_of_line;
		
	begin -- execute_script
		log (text => row_separator_double, level => log_threshold);
		log (text => "executing script " 
			 & enclose_in_quotes (to_string (file_name)),
			 --& "in mode " & to_string (cmd_entry_mode),
			 level => log_threshold, console => true);
		log_indentation_up;

		-- build the directory where the script is located:
		script_directory := to_script_name (full_name (to_string (file_name)));
		script_directory := to_script_name (containing_directory (to_string (script_directory)));
		
		-- backup the current working directory
		projects_directory := to_script_name (containing_directory (to_string (script_directory)));
		--log (text => "projects directory " & to_string (projects_directory), level => log_threshold);

		-- change in directory where the script is:
		log (text => "changing to directory " & enclose_in_quotes (to_string (script_directory)),
			 level => log_threshold + 1, console => true);
		
		set_directory (to_string (script_directory));

		-- make sure the script file exists:
		if exists (simple_name (to_string (file_name))) then

			-- open script file
			open (
				file => file_handle,
				mode => in_file, 
				name => simple_name (to_string (file_name))); -- demo.scr

			set_input (file_handle);
			
			-- read the file line by line
			while not end_of_file loop
				
				line := read_line (
					line 			=> get_line,
					number			=> ada.text_io.line (current_input),
					comment_mark 	=> comment_mark,
					delimiter_wrap	=> true, -- strings are enclosed in quotations
					ifs 			=> space); -- fields are separated by space

				-- we are interested in lines that contain something. emtpy lines are skipped:
				if field_count (line) > 0 then

					-- execute the line as command
					execute_command (file_name, line, log_threshold + 1);
					
				end if;
			end loop;

		else -- script file not found
			log (ERROR, "script file " & 
				 enclose_in_quotes (simple_name (to_string (file_name))) &
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
