------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          SCRIPT PROCESSOR                                --
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

with ada.exceptions;				use ada.exceptions;
with ada.directories;
with gnat.directory_operations;

with et_string_processing;			use et_string_processing;
with et_exceptions;					use et_exceptions;

with et_cmd_sts;					use et_cmd_sts;
with et_command_processor;			use et_command_processor;


package body et_script_processor is




	function read_script (
		file			: in string; -- like "rename_nets.scr"
		log_threshold	: in type_log_level) 
		return type_exit_code_script
	is
		exit_code : type_exit_code_script := ERROR;
		
		use ada.directories;
		
		file_handle : file_type;

		-- The text fields of a single command read from the script:
		-- Example: "schematic demo draw net GND 1 90 100 100 100"
		fields : type_fields_of_line;

		-- The single command to be executed:
		cmd : type_single_cmd;
		
		script_name : pac_script_name.bounded_string := to_script_name (file);


		procedure log_command is begin
			log (text => get_line_number (cmd) & "cmd: " 
				& enclose_in_quotes (get_all_fields (cmd)),
				level => log_threshold);
		end;

		
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

					-- Evaluate exit code of the command:
					case get_exit_code (cmd) is
						when 0 => 
							exit_code := SUCCESSFUL; -- no error
							
						when 1 =>
							log_command;							
							log (ERROR, "Script command incomplete !");
							exit; -- abort script execution
							
						when 2 =>
							log_command;
							log (ERROR, "Script command too long !");
							exit; -- abort script execution

						when 3 =>
							log_command;
							log (ERROR, "Other script error.");
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
		return type_exit_code_script
	is
		exit_code : type_exit_code_script := ERROR;
		
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
		return type_exit_code_script 
	is		
		exit_code : type_exit_code_script := ERROR;

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



	
end et_script_processor;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
