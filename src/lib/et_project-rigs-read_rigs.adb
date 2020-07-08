------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               PROJECT                                    --
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

with et_project.configuration;
with et_project.modules;

separate (et_project.rigs)

procedure read_rigs (
	log_threshold 	: in et_string_processing.type_log_level) is

	use et_string_processing;
	use ada.directories;
	use et_project.modules;

	-- The search of rig module files requires this stuff:
	module_file_search : search_type; -- the state of the search
	module_file_filter : filter_type := (ordinary_file => true, others => false);

	procedure read_module_file_pre (module_file_handle : in directory_entry_type) is 
		file_name : string := simple_name (module_file_handle); -- motor_driver.mod
	begin
		read_module (file_name, log_threshold + 1);
	end;
	
	-- The search of rig configuration files requires this stuff:
	conf_file_search : search_type; -- the state of the search
	conf_file_filter : filter_type := (ordinary_file => true, others => false);

	procedure read_conf_file (conf_file_handle : in directory_entry_type) is
		-- backup the previous input source
		previous_input : ada.text_io.file_type renames current_input;

		file_handle : ada.text_io.file_type;
		file_name : string := simple_name (conf_file_handle); -- my_rig_configuration.conf
		rig_cursor : pac_rigs.cursor;
		rig_inserted : boolean;
		
		line : et_string_processing.type_fields_of_line;

		-- This is the section stack of the configuration file. 
		-- Here we track the sections. On entering a section, its name is
		-- pushed onto the stack. When leaving a section the latest section name is popped.
		max_section_depth : constant positive := 3;
		package stack is new general_rw.stack_lifo (
			item	=> type_section_name,
			max 	=> max_section_depth);

		-- VARIABLES FOR TEMPORARILY STORAGE AND ASSOCIATED HOUSEKEEPING SUBPROGRAMS:
		generic_name : type_module_name.bounded_string; -- motor_driver
		instance_name : type_module_instance_name.bounded_string; -- DRV_1
		assembly_variant : et_general.type_variant_name.bounded_string; -- low_cost

		procedure clear_module_instance is begin
			generic_name := to_module_name ("");
			instance_name := to_instance_name ("");
		end clear_module_instance;
		
		purpose_A, purpose_B : et_devices.type_purpose.bounded_string; -- power_in, power_out
		instance_A, instance_B : type_module_instance_name.bounded_string; -- DRV_1, PWR

		procedure clear_connector is begin
			purpose_A := et_devices.type_purpose.to_bounded_string ("");
			purpose_A := purpose_B;
			instance_A := to_instance_name ("");
			instance_B := instance_A;
		end clear_connector;

		procedure process_line is

			procedure execute_section is
			-- Once a section concludes, the temporarily variables are read, evaluated
			-- and finally assembled to actual objects:

				procedure create_instance (
					rig_name	: in pac_file_name.bounded_string;
					rig			: in out type_rig) is
					instance_created : boolean;
					instance_cursor : type_module_instances.cursor;
				begin
					-- CS: test length of generic name and instance name. must be greater zero

					-- create an instanciated module in the rig
					rig.module_instances.insert (
						key			=> instance_name,
						new_item	=> (generic_name, assembly_variant),
						inserted	=> instance_created,
						position	=> instance_cursor
						);

					-- An instance may exist only once:
					if not instance_created then
						log (ERROR, "module instance '" 
							& to_string (instance_name) & "' already exists !", console => true);
						raise constraint_error;
					end if;

					clear_module_instance; -- clean up for next module instance
				end create_instance;

				procedure create_connection (
					rig_name	: in pac_file_name.bounded_string;
					rig			: in out type_rig) is
					connection_inserted : boolean;
					connection_cursor : type_module_connectors.cursor;
					use et_devices.type_purpose;
					use et_general.type_module_instance_name;
				begin
					-- If NONE of the four elements that make a module connection is specified,
					-- then do nothing. Otherwise ALL of them must be specified.
					if length (instance_A) = 0 and length (instance_B) = 0
						and length (purpose_A) = 0 and length (purpose_B) = 0 then
							null;
					else
						-- If ALL four elements are
						-- specified (means each of them contains something) then do further
						-- checks and create the connection.
						if length (instance_A) > 0 and length (instance_B) > 0
							and length (purpose_A) > 0 and length (purpose_B) > 0 then
							
							-- create a module connector in the rig
							rig.connections.insert (
								new_item	=> (
									instance_A	=> instance_A,
									instance_B	=> instance_B,
									purpose_A	=> purpose_A,
									purpose_B	=> purpose_B),
								inserted	=> connection_inserted,
								position	=> connection_cursor);

							-- A module connection may exist only once:
							if not connection_inserted then
								log (ERROR, "module connection already exists !", console => true);
								raise constraint_error;
							end if;

							clear_connector; -- clean up for next module connector

						-- If one of the four elements is not specified, output error message:
						else
							-- test length of instance_A/B and purpose A/B. must be greater zero
							if length (instance_A) = 0 then
								log (ERROR, "instance A not specified !", console => true);
								raise constraint_error;
							end if;

							if length (purpose_A) = 0 then
								log (ERROR, "purpose A not specified !", console => true);
								raise constraint_error;
							end if;						

							if length (instance_B) = 0 then
								log (ERROR, "instance B not specified !", console => true);
								raise constraint_error;
							end if;

							if length (purpose_B) = 0 then
								log (ERROR, "purpose B not specified !", console => true);
								raise constraint_error;
							end if;						
						end if;
						
					end if;
				end create_connection;
				
			begin -- execute_section
				case stack.current is

					when SEC_INIT => null;

					when SEC_MODULE_INSTANCES => null;
					
					when SEC_MODULE =>
						case stack.parent is
							when SEC_MODULE_INSTANCES =>	

								-- create an instanciated module in the rig
								pac_rigs.update_element (
									container	=> rigs,
									position	=> rig_cursor,
									process		=> create_instance'access);
								
							when others => invalid_section;
						end case;

					when SEC_MODULE_CONNECTIONS => null;
					
					when SEC_CONNECTOR =>
						case stack.parent is
							when SEC_MODULE_CONNECTIONS =>
								
								-- create a module connector in the rig
								pac_rigs.update_element (
									container	=> rigs,
									position	=> rig_cursor,
									process		=> create_connection'access);
								
							when others => invalid_section;
						end case;
						
					when others => invalid_section;
				end case;
						
			end execute_section;
			
			function set (
			-- Tests if the current line is a section header or footer. Returns true in both cases.
			-- Returns false if the current line is neither a section header or footer.
			-- If it is a header, the section name is pushed onto the sections stack.
			-- If it is a footer, the latest section name is popped from the stack.
				section_keyword	: in string; -- [MODULE_INSTANCES
				section			: in type_section_name) -- SEC_MODULE_INSTANCES
				return boolean is 
			begin -- set
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
			if set (section_module_instances, SEC_MODULE_INSTANCES) then null;
			elsif set (section_module, SEC_MODULE) then null;
			elsif set (section_module_connections, SEC_MODULE_CONNECTIONS) then null;
			elsif set (section_connector, SEC_CONNECTOR) then null;
			else
				-- The line contains something else -> the payload data. 
				-- Temporarily this data is to be stored in corresponding variables.

				log (text => "line --> " & to_string (line), level => log_threshold + 7);
				
				case stack.current is

					when SEC_INIT => null;
						
					when SEC_MODULE_INSTANCES =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do
							when others => invalid_section;
						end case;
						
					when SEC_MODULE_CONNECTIONS =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do
							when others => invalid_section;
						end case;

					when SEC_MODULE =>
						case stack.parent is
							when SEC_MODULE_INSTANCES =>							
								declare
									kw : string := f (line, 1);
									module_cursor : pac_generic_modules.cursor;
								begin
									if kw = keyword_generic_name then
										expect_field_count (line, 2);

										-- The generic name does not use the *.mod extension.
										generic_name := type_module_name.to_bounded_string (f (line,2));
										
										-- test whether a module with this generic name exists
										if not exists (generic_name) then
											log (ERROR, "module " & enclose_in_quotes (to_string (generic_name)) &
													" does not exist !", console => true);
											raise constraint_error;
										end if;
										
									elsif kw = keyword_instance_name then
										expect_field_count (line, 2);
										instance_name := to_instance_name (f (line,2));

									elsif kw = keyword_assembly_variant then
										expect_field_count (line, 2);
										assembly_variant := et_general.to_variant (f (line,2));

										-- test whether module provides the assembly variant
										module_cursor := locate_module (generic_name);
										if not exists (module_cursor, assembly_variant) then
											log (ERROR, "module " & enclose_in_quotes (to_string (generic_name)) &
													" does not provide assembly variant " &
													enclose_in_quotes (et_general.to_variant (assembly_variant)) & " !",
												console => true);
											raise constraint_error;
										end if;
									else
										invalid_keyword (kw);
									end if;
								end;
								
							when others => invalid_section;
						end case;

					when SEC_CONNECTOR =>							
						case stack.parent is
							when SEC_MODULE_CONNECTIONS =>
								declare
									use et_devices;
									kw : string := f (line, 1);
								begin
									if kw = keyword_instance_A then
										expect_field_count (line, 2);
										instance_A := to_instance_name (f (line,2));
										-- CS: test if instance exists
									elsif kw = keyword_instance_B then
										expect_field_count (line, 2);
										instance_B := to_instance_name (f (line,2));
										-- CS: test if instance exists
										
									elsif kw = keyword_purpose_A then
										expect_field_count (line, 2);
										purpose_A := to_purpose (f (line,2));
										-- CS: test if a connector with this purpose exists in the instance
										
									elsif kw = keyword_purpose_B then
										expect_field_count (line, 2);
										purpose_B := to_purpose (f (line,2));
										-- CS: test if a connector with this purpose exists in the instance
										
									-- CS: net comparator and warning on/off
									else
										invalid_keyword (kw);
									end if;
								end;
								
							when others => invalid_section;
						end case;

					when others => invalid_section;
				end case;
			end if;

			exception when event: others =>
				log (text => "file " & file_name & space & affected_line (line) 
						& to_string (line), console => true);
				raise;
			
		end process_line;
		
	begin -- read_conf_file
		-- write name of configuration file
		log (text => file_name, level => log_threshold + 1);
		log_indentation_up;

		-- open rig configuration file
		open (
			file => file_handle,
			mode => in_file, 
			name => file_name); -- demo.conf, low_cost.conf, fully_equipped.conf

		set_input (file_handle);

		-- Init section stack.
		stack.init;
		stack.push (SEC_INIT);

		-- create an empty rig - named after the given configuration file but without extension
		pac_rigs.insert (
			container	=> rigs,
			key			=> to_bounded_string (base_name (file_name)), -- demo, low_cost, fully_equipped
			inserted	=> rig_inserted, -- should always be true
			position	=> rig_cursor);
		
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

		-- As a safety measure the top section must be reached:
		if stack.depth > 1 then 
			log (WARNING, write_section_stack_not_empty);
		end if;

		log_indentation_down;
		set_input (previous_input);
		close (file_handle);

		exception when event: others =>
			if is_open (file_handle) then close (file_handle); end if;
			set_input (previous_input);
			raise;
		
	end read_conf_file;
	
begin -- read_rigs
	log (text => "reading rigs ...", level => log_threshold);
	log_indentation_up;
		
	-- CS: It requires discussion whether loading all modules files at this time is reasonable.
	-- Even if a module will not be used it is going to be loaded. This causes more log information than required.
	-- A solution could be to load a module on reading the rig configuration file. The drawback is that the user
	-- would be required to setup a rig configuration even if she wants to design only one board.
	log (text => "looking for module files ...", level => log_threshold + 1);
	log_indentation_up;
	start_search (module_file_search, current_directory, module_file_name_extension_asterisk, module_file_filter);
	if more_entries (module_file_search) then
		search (current_directory, module_file_name_extension_asterisk, module_file_filter, read_module_file_pre'access);
	else
		log (WARNING, "No modules found !"); -- CS: write implications !
	end if;
	end_search (module_file_search);
	log_indentation_down;
	
	log (text => "looking for rig configuration files ...", level => log_threshold + 1);
	log_indentation_up;
	start_search (conf_file_search, current_directory, file_extension_asterisk, conf_file_filter);
	if more_entries (conf_file_search) then
		search (current_directory, file_extension_asterisk, conf_file_filter, read_conf_file'access);
	else
		log (WARNING, "No rig configuration files found !"); -- CS: write implications !
	end if;
	end_search (conf_file_search);
	log_indentation_down;

	log_indentation_down;

end read_rigs;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
