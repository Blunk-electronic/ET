------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      PROJECT CONFIGURATON READ                           --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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

separate (et_project.configuration)
	
procedure read_configuration (
	project_name 	: in pac_project_name.bounded_string; -- blood_sample_analyzer
	log_threshold 	: in et_string_processing.type_log_level) 
is
	use et_string_processing;
	use ada.directories;

	-- compose the name of the project file to read like blood_sample_analyzer.prj
	file_name : constant string := 
		compose (current_directory, to_string (project_name), file_extension);

	-- backup the previous input source
	previous_input : ada.text_io.file_type renames current_input;
	
	file_handle : ada.text_io.file_type;

	-- the line fetched from the configuration file:
	line : et_string_processing.type_fields_of_line;


	
	-- This is the section stack of the configuration file. 
	-- Here we track the sections. On entering a section, its name is
	-- pushed onto the stack. When leaving a section the latest section name is popped.
	max_section_depth : constant positive := 2;
	package stack is new general_rw.stack_lifo (
		item	=> type_section_name,
		max 	=> max_section_depth);

	-- VARIABLES FOR TEMPORARILY STORAGE AND ASSOCIATED HOUSEKEEPING SUBPROGRAMS:
	conventions_file_name : et_conventions.pac_file_name.bounded_string;
	
	procedure read_rules is
		use et_conventions;
		kw : constant string := f (line, 1);
	begin
		if kw = keyword_conventions then -- conventions ../conventions.txt
			expect_field_count (line, 2);

			conventions_file_name := to_file_name (f (line, 2));
			
			-- read the conventions file
			et_conventions.read_conventions (conventions_file_name, log_threshold + 1);
		else
			invalid_keyword (kw);
		end if;
	end read_rules;

	procedure set_rules is 
		use et_conventions.pac_file_name;
	begin
		if length (conventions_file_name) > 0 then

			-- assign conventions file name to project configuration:
			project.rules.conventions := conventions_file_name;

		else
			log (WARNING, "No conventions file specified ! Design check limited !");
		end if;
	end set_rules;

	procedure do_it is
	
		procedure process_line is

			procedure execute_section is
			-- Once a section concludes, the temporarily variables are read, evaluated
			-- and finally assembled to actual objects:
				
			begin -- execute_section
				case stack.current is

					when SEC_RULES =>
						case stack.parent is
							when SEC_INIT	=> set_rules;
							when others		=> invalid_section;
						end case;
					
					when SEC_INIT => null;
					
					when others => invalid_section;
				end case;
						
			end execute_section;
			
			function set (
			-- Tests if the current line is a section header or footer. Returns true in both cases.
			-- Returns false if the current line is neither a section header or footer.
			-- If it is a header, the section name is pushed onto the sections stack.
			-- If it is a footer, the latest section name is popped from the stack.
				section_keyword	: in string;
				section			: in type_section_name)
				return boolean is 
			begin
				if f (line, 1) = section_keyword then -- section name detected in field 1
					if f (line, 2) = section_begin then -- section header detected in field 2
						stack.push (section);
						log (text => write_enter_section & to_string (section), level => log_threshold + 7);
						return true;
						
					elsif f (line, 2) = section_end then -- section footer detected in field 2

						-- The section name in the footer must match the name
						-- of the current section. Otherwise abort.
						if section /= stack.current then
							log_indentation_reset;
							invalid_section;
						end if;
						
						-- Now that the section ends, the data collected in temporarily
						-- variables is processed.
						execute_section;
						
						stack.pop;
						if stack.empty then
							log (text => write_top_level_reached, level => log_threshold + 7);
						else
							log (text => write_return_to_section & to_string (stack.current), level => log_threshold + 7);
						end if;
						return true;
						
					else
						log (ERROR, write_missing_begin_end, console => true);
						raise constraint_error;
					end if;
					
				else -- neither a section header nor footer
					return false;
				end if;
			end set;
			
		begin -- process_line
			if set (section_environment_variables, SEC_ENVIRONMENT_VARIABLES) then null;
			elsif set (section_rules, SEC_RULES) then null;
			else
				-- The line contains something else -> the payload data. 
				-- Temporarily this data is to be stored in corresponding variables.

				log (text => "line --> " & to_string (line), level => log_threshold + 7);
				
				case stack.current is

					when SEC_RULES =>
						case stack.parent is
							when SEC_INIT	=> read_rules;
							when others		=> invalid_section;
						end case;
						
					when SEC_INIT => null;

					when others => invalid_section;
				end case;
			end if;

			exception when event: others =>
				log (text => "file " & enclose_in_quotes (file_name) & space & affected_line (line) 
						& to_string (line), console => true);
				raise;
			
		end process_line;

	begin -- do_it
		
		-- read the file line by line
		while not end_of_file loop
			line := et_string_processing.read_line (
				line 			=> get_line,
				number			=> ada.text_io.line (current_input),
				comment_mark 	=> comment_mark, -- comments start with "--"
				delimiter_wrap	=> true, -- strings are enclosed in quotations
				ifs 			=> space); -- fields are separated by space

			-- we are interested in lines that contain something. emtpy lines are skipped:
			if field_count (line) > 0 then
				process_line;
			end if;
		end loop;
		
	end do_it;
	
begin -- read_configuration
	log (text => "reading project configuration file " & enclose_in_quotes (file_name) & " ...",
		 level => log_threshold, console => true);
	log_indentation_up;
	
	if exists (file_name) then

		open (
			file => file_handle,
			mode => in_file, 
			name => file_name);

		set_input (file_handle);

		-- Init section stack.
		stack.init;
		stack.push (SEC_INIT);
		
		do_it;

		-- As a safety measure the top section must be reached:
		if stack.depth > 1 then 
			log (WARNING, write_section_stack_not_empty);
		end if;
		

		set_input (previous_input);
		close (file_handle);
		
	else
		log (ERROR, "Project configuration file " & enclose_in_quotes (file_name) & " not found !",
			 console => true);
		
		raise constraint_error;
	end if;

	log_indentation_down;

	
	exception when event:
		others => 

			if is_open (file_handle) then close (file_handle); end if;
			raise;

end read_configuration;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
