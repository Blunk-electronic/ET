------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           PCB DESIGN RULES                               --
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
--   ToDo: 

separate (et_design_rules)

procedure read_rules (
	file_name		: in pac_file_name.bounded_string;
	log_threshold 	: in et_string_processing.type_log_level)
is 
	previous_input : ada.text_io.file_type renames current_input;
	
	-- Environment variables like $dru-files could be in file name.
	-- In order to test whether the given dru file exists, file name_name must be expanded
	-- so that the environment variables are replaced by the real paths like:
	-- dru-files/JLP_ML4_standard.dru:
	use et_string_processing;
	use pac_design_rules;
	design_rules_cursor : pac_design_rules.cursor;
	rule_inserted : boolean;
	
	expanded_name : constant string := expand (to_string (file_name));

	file_handle : ada.text_io.file_type;

	-- The line read from the the dru file:
	line : et_string_processing.type_fields_of_line;

	-- This is the section stack of the design rules. 
	-- Here we track the sections. On entering a section, its name is
	-- pushed onto the stack. When leaving a section the latest section name is popped.
	max_section_depth : constant positive := 3;
	package stack is new general_rw.stack_lifo (
		item	=> type_section_name,
		max 	=> max_section_depth);


	clearances 	: type_clearances;
	sizes		: type_sizes;
	restring	: type_restring;
	stop_mask	: type_stop_mask;
	rules		: type_design_rules;
	
	procedure read_clearances is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_between_conductors then -- between_conductors 0.2
			expect_field_count (line, 2);

			clearances.between_conductors := to_distance (f (line, 2));

		elsif kw = keyword_between_conductors_same_net then
			expect_field_count (line, 2);

			clearances.between_conductors_same_net := to_distance (f (line, 2));

		elsif kw = keyword_conductor_to_board_edge then
			expect_field_count (line, 2);

			clearances.conductor_to_board_edge := to_distance (f (line, 2));

		elsif kw = keyword_edge_to_edge then
			expect_field_count (line, 2);

			clearances.edge_to_edge := to_distance (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;
	end read_clearances;
	
	procedure read_sizes is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_tracks then -- tracks 0.2
			expect_field_count (line, 2);

			sizes.tracks := to_distance (f (line, 2));

		elsif kw = keyword_drills then
			expect_field_count (line, 2);

			sizes.drills := to_distance (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;
	end read_sizes;

	procedure read_restring is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_inner then -- inner 0.2
			expect_field_count (line, 2);

			restring.inner := to_distance (f (line, 2));

		elsif kw = keyword_outer then
			expect_field_count (line, 2);

			restring.outer := to_distance (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;
	end read_restring;

	procedure read_stop_mask is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_expansion_min then -- expansion_min 0.05
			expect_field_count (line, 2);

			stop_mask.expansion_min := to_distance (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;
	end read_stop_mask;

	
	procedure process_line is

		procedure execute_section is 
		begin
			
			case stack.current is

				when SEC_CLEARANCES =>
					case stack.parent is
						when SEC_INIT	=> rules.clearances := clearances;							
						when others		=> invalid_section;
					end case;

				when SEC_SIZES =>
					case stack.parent is
						when SEC_INIT	=> rules.sizes := sizes;							
						when others		=> invalid_section;
					end case;

				when SEC_RESTRING =>
					case stack.parent is
						when SEC_SIZES	=> sizes.restring := restring;
						when others		=> invalid_section;
					end case;

				when SEC_STOP_MASK =>
					case stack.parent is
						when SEC_INIT	=> rules.stop_mask := stop_mask;
						when others		=> invalid_section;
					end case;
				
				when SEC_INIT => null; -- CS: should never happen

			end case;
			
		end execute_section;

		function set (
		-- Tests if the current line is a section header or footer. Returns true in both cases.
		-- Returns false if the current line is neither a section header or footer.
		-- If it is a header, the section name is pushed onto the sections stack.
		-- If it is a footer, the latest section name is popped from the stack.
			section_keyword	: in string; -- [CLEARANCES
			section			: in type_section_name) -- SEC_CLEARANCES
			return boolean is 
		begin -- set
			if f (line, 1) = section_keyword then -- section name detected in field 1
				if f (line, 2) = section_begin then -- section header detected in field 2
					stack.push (section);
					log (text => write_enter_section & to_string (section), level => log_threshold + 5);
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
						log (text => write_top_level_reached, level => log_threshold + 5);
					else
						log (text => write_return_to_section & to_string (stack.current), level => log_threshold + 5);
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
		if set (section_clearances, SEC_CLEARANCES) then null;
		elsif set (section_sizes, SEC_SIZES) then null;
		elsif set (section_restring, SEC_RESTRING) then null;
		elsif set (section_stop_mask, SEC_STOP_MASK) then null;

		else
			-- The line contains something else -> the payload data. 
			-- Temporarily this data is stored in corresponding variables.

			log (text => "dru line --> " & to_string (line), level => log_threshold + 4);
	
			case stack.current is

				when SEC_CLEARANCES =>
					case stack.parent is
						when SEC_INIT	=> read_clearances;
						when others		=> invalid_section;
					end case;

				when SEC_SIZES =>
					case stack.parent is
						when SEC_INIT	=> read_sizes;
						when others		=> invalid_section;
					end case;

				when SEC_RESTRING =>
					case stack.parent is
						when SEC_SIZES	=> read_restring;
						when others		=> invalid_section;
					end case;

				when SEC_STOP_MASK =>
					case stack.parent is
						when SEC_INIT	=> read_stop_mask;
						when others		=> invalid_section;
					end case;
					
				when SEC_INIT => null; -- CS: should never happen

			end case;
			
		end if;

		exception when event: others =>
			log (text => "file " & enclose_in_quotes (to_string (file_name)) & space 
					& affected_line (line) & to_string (line), console => true);
			raise;
		
	end process_line;
	
begin -- read_rules
	log (text => "reading design rules ...", level => log_threshold);
	log_indentation_up;
	log (text => "expanded name " & enclose_in_quotes (expanded_name), level => log_threshold + 1);

	-- If no path specified, then the DRU file is expected to be in the project directory:
	if exists (expanded_name) then

		if not contains (design_rules, to_file_name (expanded_name)) then

			-- open dru file
			open (
				file => file_handle,
				mode => in_file, 
				name => expanded_name);
			
			set_input (file_handle);
			
			-- Init section stack.
			stack.init;
			stack.push (SEC_INIT);
			
			-- read the file line by line
			while not end_of_file loop
				line := et_string_processing.read_line (
					line 			=> get_line,
					number			=> ada.text_io.line (current_input),
					comment_mark 	=> comment_mark,
					delimiter_wrap	=> true, -- strings are enclosed in quotations
					ifs 			=> space); -- fields are separated by space

				-- we are interested in lines that contain something. emtpy lines are skipped:
				if field_count (line) > 0 then
					process_line;
				end if;
			end loop;

			-- As a safety measure the top section must be reached finally.
			if stack.depth > 1 then 
				log (text => message_warning & write_section_stack_not_empty);
			end if;

			set_input (previous_input);
			close (file_handle);

			-- Now all data for the rules has been collected in rules_tmp.
			-- It will now be inserted in the collection of design rules.
			insert (
				container	=> design_rules,
				key			=> to_file_name (expanded_name),
				position	=> design_rules_cursor,
				inserted	=> rule_inserted,
				new_item	=> rules
				);

			-- CS check rule_inserted flag ? should always be true after inserting
				
		else
			log (text => "already loaded -> no need to load anew", level => log_threshold + 1);
		end if;
		
	else
		log (ERROR, "file " & enclose_in_quotes (expanded_name) & " not found !", console => true);
		raise constraint_error;
	end if;
	
	log_indentation_down;
	
	exception when event: others =>
		if is_open (file_handle) then close (file_handle); end if;
		set_input (previous_input);
		raise;

end read_rules;



-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
