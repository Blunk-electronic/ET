------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             SYMBOL READ                                  --
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

with ada.text_io;				use ada.text_io;
with ada.strings; 				use ada.strings;

with ada.exceptions;

with et_string_processing;			use et_string_processing;
with et_directory_and_file_ops;
with et_file_sections;				use et_file_sections;

with et_symbol_library;				use et_symbol_library;
with et_device_appearance;
with et_keywords;
with et_symbol_read_port;			use et_symbol_read_port;
with et_symbol_read_body;			use et_symbol_read_body;
with et_symbol_read_text;			use et_symbol_read_text;


package body et_symbol_read is

	
	procedure read_symbol (
		file_name 		: in pac_symbol_model_name.bounded_string; -- libraries/symbols/nand.sym
		log_threshold	: in type_log_level) 
	is
		file_handle : ada.text_io.file_type;

		line : type_fields_of_line;

		
		-- This is the section stack of the symbol model:
		max_section_depth : constant positive := 3; -- incl. section init

		package pac_sections_stack is new gen_pac_sections_stack (
			item	=> type_file_section,
			max 	=> max_section_depth);


			
		-- This is the pointer that points to the symbol
		-- being read in the following:
		symbol_model : type_symbol_model_access;

		symbol_cursor		: pac_symbol_models.cursor;
		symbol_inserted		: boolean;
		
		


		
		-- This procedure reads the appearance of the
		-- symbol and creates it where pointer symbol_model
		-- points to:
		procedure read_appearance (
			line : in type_fields_of_line)
		is
			use et_keywords;
			kw : string := f (line, 1);
			
			use et_device_appearance;
			appearance : type_appearance;
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_appearance then -- appearance sch_pcb
				expect_field_count (line, 2);
				appearance := to_appearance (f (line,2));
				-- log (text => "appearance" & to_string (appearance), level => log_threshold + 1);

				-- Create the new symbol:
				case appearance is
					when APPEARANCE_VIRTUAL =>
						symbol_model := new type_symbol_model' (
							appearance	=> APPEARANCE_VIRTUAL,
							others		=> <>);

					when APPEARANCE_PCB =>
						symbol_model := new type_symbol_model' (
							appearance	=> APPEARANCE_PCB,
							others		=> <>);

				end case;
				
			else
				invalid_keyword (kw);
			end if;
		end read_appearance;


		

		
		procedure process_line is 

			procedure execute_section is
			-- Once a section concludes, the temporarily variables are read, evaluated
			-- and finally assembled to actual objects:
			begin
				case pac_sections_stack.current is

					when SEC_DRAW | SEC_TEXTS | SEC_PLACEHOLDERS | SEC_PORTS => 
						case pac_sections_stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

					when SEC_LINE =>					
						case pac_sections_stack.parent is
							when SEC_DRAW => insert_body_line (symbol_model, log_threshold + 1);
							when others => invalid_section;
						end case;
						
					when SEC_ARC =>
						case pac_sections_stack.parent is
							when SEC_DRAW => insert_body_arc (symbol_model, log_threshold + 1);
							when others => invalid_section;
						end case;
						
					when SEC_CIRCLE =>
						case pac_sections_stack.parent is
							when SEC_DRAW => insert_body_circle (symbol_model, log_threshold + 1);
							when others => invalid_section;
						end case;
						
					when SEC_TEXT =>
						case pac_sections_stack.parent is
							when SEC_TEXTS => insert_text (symbol_model, log_threshold + 1);
							when others => invalid_section;
						end case;
						
					when SEC_PLACEHOLDER =>
						case pac_sections_stack.parent is
							when SEC_PLACEHOLDERS => insert_placeholder (symbol_model, log_threshold + 1);
							when others => invalid_section;
						end case;

					when SEC_PORT =>
						case pac_sections_stack.parent is
							when SEC_PORTS => insert_port (symbol_model, log_threshold + 1);
							when others => invalid_section;
						end case;
						
					when SEC_INIT => null; -- CS: should never happen

					when others => invalid_section;
				end case;
			end execute_section;

			
			
			function set (
			-- Tests if the current line is a section header or footer. Returns true in both cases.
			-- Returns false if the current line is neither a section header or footer.
			-- If it is a header, the section name is pushed onto the sections pac_sections_stack.
			-- If it is a footer, the latest section name is popped from the pac_sections_stack.
				section_keyword	: in string; -- [DRAW
				section			: in type_file_section) -- SEC_DRAW
				return boolean 
			is begin
				if f (line, 1) = section_keyword then -- section name detected in field 1
					if f (line, 2) = section_begin then -- section header detected in field 2
						pac_sections_stack.push (section);
						log (text => write_enter_section & to_string (section), level => log_threshold + 3);
						return true;

					elsif f (line, 2) = section_end then -- section footer detected in field 2

						-- The section name in the footer must match the name
						-- of the current section. Otherwise abort.
						if section /= pac_sections_stack.current then
							log_indentation_reset;
							invalid_section;
						end if;
						
						-- Now that the section ends, the data collected in temporarily
						-- variables is processed.
						execute_section;
						
						pac_sections_stack.pop;
						if pac_sections_stack.empty then
							log (text => write_top_level_reached, level => log_threshold + 3);
						else
							log (text => write_return_to_section & to_string (pac_sections_stack.current), level => log_threshold + 3);
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
			if set (section_draw, SEC_DRAW) then null;			
			elsif set (section_line, SEC_LINE) then null;								
			elsif set (section_arc, SEC_ARC) then null;								
			elsif set (section_circle, SEC_CIRCLE) then null;
			elsif set (section_texts, SEC_TEXTS) then null;
			elsif set (section_text, SEC_TEXT) then null;
			elsif set (section_placeholders, SEC_PLACEHOLDERS) then null;
			elsif set (section_placeholder, SEC_PLACEHOLDER) then null;
			elsif set (section_ports, SEC_PORTS) then null;
			elsif set (section_port, SEC_PORT) then null;
			else
				-- The line contains something else -> the payload data. 
				-- Temporarily this data is stored in corresponding variables.

				log (text => "symbol line --> " & to_string (line), level => log_threshold + 3);
		
				case pac_sections_stack.current is

					when SEC_INIT =>
						read_appearance (line);

					when SEC_DRAW | SEC_TEXTS | SEC_PLACEHOLDERS | SEC_PORTS => 
						case pac_sections_stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

					when SEC_LINE =>
						case pac_sections_stack.parent is
							when SEC_DRAW => read_body_line (line);
							when others => invalid_section;
						end case;

					when SEC_ARC =>
						case pac_sections_stack.parent is
							when SEC_DRAW => read_body_arc (line);
							when others => invalid_section;
						end case;
						
					when SEC_CIRCLE =>
						case pac_sections_stack.parent is
							when SEC_DRAW => read_body_circle (line);
							when others => invalid_section;
						end case;
						
					when SEC_TEXT =>
						case pac_sections_stack.parent is
							when SEC_TEXTS => read_text (line);
							when others => invalid_section;
						end case;
						
					when SEC_PLACEHOLDER =>
						case pac_sections_stack.parent is
							when SEC_PLACEHOLDERS => read_placeholder (line);
							when others => invalid_section;
						end case;

					when SEC_PORT =>
						case pac_sections_stack.parent is
							when SEC_PORTS => read_port (line);								
							when others => invalid_section;
						end case;

					when others => invalid_section;
				end case;
			end if;

			
			exception when event: others =>
				log (text => "file " & to_string (file_name) & space 
					 & get_affected_line (line) & to_string (line), console => true);
				raise;
			
		end process_line;

		
		previous_input : ada.text_io.file_type renames current_input;

		
	begin -- read_symbol
		log_indentation_up;
		log (text => "reading symbol model " & to_string (file_name),
			 level => log_threshold);
		
		log_indentation_up;
		
		-- test if container et_symbol_model.symbols already contains the symbol
		-- named "file_name". If so, there would be no need to read the file_name again.
		if pac_symbol_models.contains (symbol_library, file_name) then
			log (text => "already read -> skipped", level => log_threshold + 1);
		else
			
			-- open symbol file
			open (
				file => file_handle,
				mode => in_file, 
				name => et_directory_and_file_ops.expand (to_string (file_name)));

			set_input (file_handle);
			
			-- Init section pac_sections_stack.
			pac_sections_stack.init;
			pac_sections_stack.push (SEC_INIT);

			
			-- read the file line by line
			while not end_of_file loop
				line := read_line (
					line 			=> get_line,
					number			=> positive (ada.text_io.line (current_input)),
					delimiter_wrap	=> true, -- strings are enclosed in quotations
					ifs 			=> space); -- fields are separated by space

				-- we are interested in lines that contain something. emtpy lines are skipped:
				if get_field_count (line) > 0 then
					process_line;
				end if;
			end loop;

			
			-- As a safety measure the top section must be reached finally.
			if pac_sections_stack.depth > 1 then 
				log (WARNING, write_section_stack_not_empty);
			end if;
			

			set_input (previous_input);
			close (file_handle);

			
			-- Insert the symbol in the symbol library:
			pac_symbol_models.insert (
				container	=> symbol_library, 
				key			=> file_name, -- libraries/symbols/nand.sym
				position	=> symbol_cursor,
				inserted	=> symbol_inserted,
				new_item	=> symbol_model.all);
			
			-- CS Check integrity of symbol (style guides, conventions ...)
			-- use symbol_cursor to access the symbol
		end if;

		log_indentation_down;
		log_indentation_down;		

		exception when event: others =>
			if is_open (file_handle) then 
				set_input (previous_input);
				close (file_handle); 
			end if;
			raise;

	end read_symbol;

	
end et_symbol_read;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
