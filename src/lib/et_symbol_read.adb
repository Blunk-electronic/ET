------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             SYMBOL READ                                  --
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

with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.text_io;				use ada.text_io;

with ada.exceptions;

with et_primitive_objects;			--use et_primitive_objects;
with et_directions;					use et_directions;
with et_coordinates_formatting;		use et_coordinates_formatting;
with et_system_info;
with et_directory_and_file_ops;
with et_general_rw;					use et_general_rw;
with et_axes;						use et_axes;
with et_text;
with et_alignment;					use et_alignment;
with et_logic;
with et_power_sources;
with et_port_sensitivity;
with et_port_strength;
with et_port_visibility;
with et_port_direction;
with et_port_names;
with et_symbol_shapes;				use et_symbol_shapes;
with et_symbol_text;				use et_symbol_text;
with et_symbol_library;				use et_symbol_library;
with et_symbol_ports;				use et_symbol_ports;
with et_device_placeholders;		use et_device_placeholders;
with et_time;						use et_time;
with et_keywords;					use et_keywords;
with et_symbol_sections;			use et_symbol_sections;


package body et_symbol_read is


	
	procedure read_symbol (
		file_name 		: in pac_symbol_model_file.bounded_string; -- libraries/symbols/nand.sym
		log_threshold	: in type_log_level) 
	is
		use et_text;
		
		file_handle : ada.text_io.file_type;

		line : type_fields_of_line;

		-- This is the section stack of the symbol model. 
		-- Here we track the sections. On entering a section, its name is
		-- pushed onto the stack. When leaving a section the latest section name is popped.
		max_section_depth : constant positive := 3; -- incl. section init

		package stack is new et_general_rw.stack_lifo (
			item	=> type_symbol_section,
			max 	=> max_section_depth);

		

		
		-- VARIABLES FOR TEMPORARILY STORAGE AND ASSOCIATED HOUSEKEEPING SUBPROGRAMS:
		appearance			: type_appearance;
		symbol				: access type_symbol;
		symbol_line			: type_symbol_line;
		symbol_arc			: type_symbol_arc;
		symbol_circle		: type_symbol_circle;
		symbol_text_base	: type_text_basic;

		symbol_cursor		: pac_symbols.cursor;
		symbol_inserted		: boolean;
		
		symbol_text_position		: type_vector_model;
		symbol_text_content			: et_text.pac_text_content.bounded_string;
		symbol_placeholder_meaning	: type_placeholder_meaning := placeholder_meaning_default;
		
		port					: type_port_base;
		port_name				: et_port_names.pac_port_name.bounded_string;
		port_direction			: et_port_direction.type_port_direction := et_port_direction.port_direction_default;
		port_sensitivity_edge	: et_port_sensitivity.type_sensitivity_edge := et_port_sensitivity.sensitivity_edge_default;
		port_sensitivity_level	: et_port_sensitivity.type_sensitivity_level := et_port_sensitivity.sensitivity_level_default;
		port_output_inverted	: et_logic.type_output_inverted := et_logic.output_inverted_default;
		port_output_tristate	: et_port_strength.type_output_tristate := et_port_strength.output_tristate_default;
		port_output_weakness	: et_port_strength.type_output_weakness := et_port_strength.output_weakness_default;
		port_power_level		: et_power_sources.type_power_level := et_power_sources.port_power_level_default;

		
		procedure insert_port is 
			inserted	: boolean;
			cursor		: pac_symbol_ports.cursor;

			use et_port_names;
			use et_port_direction;
			use et_port_sensitivity;
			use et_port_strength;
			use et_logic;
			use et_power_sources;
		begin
			case port_direction is
				when PASSIVE =>
					pac_symbol_ports.insert (
						container	=> symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> PASSIVE)
						);

				when INPUT_ANALOG =>
					pac_symbol_ports.insert (
						container	=> symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> INPUT_ANALOG)
						);

				when INPUT_DIGITAL =>
					pac_symbol_ports.insert (
						container	=> symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> INPUT_DIGITAL,
							sensitivity_edge		=> port_sensitivity_edge,
							sensitivity_level		=> port_sensitivity_level)
						);

				when OUTPUT_ANALOG =>
					pac_symbol_ports.insert (
						container	=> symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> OUTPUT_ANALOG,
							output_analog_tristate	=> port_output_tristate,
							output_analog_weakness	=> port_output_weakness)
						);

				when OUTPUT_DIGITAL =>
					pac_symbol_ports.insert (
						container	=> symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> OUTPUT_DIGITAL,
							output_digital_inverted	=> port_output_inverted,
							output_digital_tristate	=> port_output_tristate,
							output_digital_weakness	=> port_output_weakness)
						);

				when BIDIR_DIGITAL =>
					pac_symbol_ports.insert (
						container	=> symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> BIDIR_DIGITAL,
							output_inverted			=> port_output_inverted,
							output_tristate			=> port_output_tristate,
							output_weakness			=> port_output_weakness,
							input_sensitivity_edge	=> port_sensitivity_edge,
							input_sensitivity_level	=> port_sensitivity_level)
						);

				when POWER_OUT =>
					pac_symbol_ports.insert (
						container	=> symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> POWER_OUT,
							level					=> port_power_level)
						);

				when POWER_IN =>
					pac_symbol_ports.insert (
						container	=> symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> POWER_IN,
							level					=> port_power_level)
						);

				when NOT_CONNECTED =>
					pac_symbol_ports.insert (
						container	=> symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> NOT_CONNECTED)
						);
			end case;

			-- abort if port name already used:
			if not inserted then
				log (ERROR, "port " & to_string (port_name) & " already in use !", console => true);
				raise constraint_error;
			end if;
			
			-- reset port parameters for next port
			port					:= (others => <>);
			port_name				:= to_port_name ("");
			port_direction			:= port_direction_default;
			port_sensitivity_edge	:= sensitivity_edge_default;
			port_sensitivity_level	:= sensitivity_level_default;
			port_output_inverted	:= output_inverted_default;
			port_output_tristate	:= output_tristate_default;
			port_output_weakness	:= output_weakness_default;
			port_power_level		:= port_power_level_default;
		end insert_port;

		
		procedure process_line is 

			procedure execute_section is
			-- Once a section concludes, the temporarily variables are read, evaluated
			-- and finally assembled to actual objects:
			begin -- execute_section
				case stack.current is

					when SEC_DRAW | SEC_TEXTS | SEC_PLACEHOLDERS | SEC_PORTS => 
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

					when SEC_LINE =>
						case stack.parent is
							when SEC_DRAW => 

								-- append symbol_line to unit_symbol
								pac_symbol_lines.append (
									container	=> symbol.shapes.lines,
									new_item	=> symbol_line);

								-- clean up for next line
								reset_line (symbol_line);
								
							when others => invalid_section;
						end case;

					when SEC_ARC =>
						case stack.parent is
							when SEC_DRAW =>

								-- append symbol_arc to unit_symbol
								pac_symbol_arcs.append (
									container	=> symbol.shapes.arcs,
									new_item	=> symbol_arc);

								-- clean up for next arc
								reset_arc (symbol_arc);
								
							when others => invalid_section;
						end case;
						
					when SEC_CIRCLE =>
						case stack.parent is
							when SEC_DRAW =>

								-- append symbol_circle to unit_symbol
								pac_symbol_circles.append (
									container	=> symbol.shapes.circles,
									new_item	=> symbol_circle);

								-- clean up for next circle
								reset_circle (symbol_circle);
								
							when others => invalid_section;
						end case;
						
					when SEC_TEXT =>
						case stack.parent is
							when SEC_TEXTS =>

								-- append symbol text to symbol
								pac_symbol_texts.append (
									container	=> symbol.texts,
									new_item	=> (symbol_text_base with
										content		=> symbol_text_content,
										position	=> symbol_text_position));

								-- clean up for next symbol text
								symbol_text_base := (others => <>);
								symbol_text_content := to_content ("");
								symbol_text_position := origin;
								
							when others => invalid_section;
						end case;
						
					when SEC_PLACEHOLDER =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>

								-- Assign symbol text placeholder to symbol.
								-- The meaning of the placeholder determines where
								-- the placeholder is to be assigned. 
								-- If meaning is not specified in section PLACEHOLDER,
								-- the default meaning is assumed which raise an error.

								-- CS: warn if placeholder exists multiple times. The latest
								-- placeholder would overwrite the previous one.

								case symbol_placeholder_meaning is
									when NAME =>
										symbol.placeholders.name := (symbol_text_base with 
											position	=> symbol_text_position,
											meaning		=> symbol_placeholder_meaning);

									when VALUE =>
										symbol.placeholders.value := (symbol_text_base with 
											position	=> symbol_text_position,
											meaning		=> symbol_placeholder_meaning);

									when PURPOSE =>
										symbol.placeholders.purpose := (symbol_text_base with 
											position	=> symbol_text_position,
											meaning		=> symbol_placeholder_meaning);

									-- Default meaning causes an error:
									when others => 
										log (ERROR, "meaning of placeholder not specified !",
											 console => true);
										raise constraint_error;
								end case;

								-- clean up for next symbol text placeholder
								symbol_text_base := (others => <>);
								symbol_text_position := origin;
								symbol_placeholder_meaning := placeholder_meaning_default;
							
							when others => invalid_section;
						end case;

					when SEC_PORT =>
						case stack.parent is
							when SEC_PORTS => insert_port;
							when others => invalid_section;
						end case;
						
					when SEC_INIT => null; -- CS: should never happen
				end case;

			end execute_section;

			
			function set (
			-- Tests if the current line is a section header or footer. Returns true in both cases.
			-- Returns false if the current line is neither a section header or footer.
			-- If it is a header, the section name is pushed onto the sections stack.
			-- If it is a footer, the latest section name is popped from the stack.
				section_keyword	: in string; -- [DRAW
				section			: in type_symbol_section) -- SEC_DRAW
				return boolean is 
			begin -- set
				if f (line, 1) = section_keyword then -- section name detected in field 1
					if f (line, 2) = section_begin then -- section header detected in field 2
						stack.push (section);
						log (text => write_enter_section & to_string (section), level => log_threshold + 3);
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
							log (text => write_top_level_reached, level => log_threshold + 3);
						else
							log (text => write_return_to_section & to_string (stack.current), level => log_threshold + 3);
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
		
				case stack.current is

					when SEC_INIT =>
						declare
							kw : string := f (line, 1);
						begin
							-- CS: In the following: set a corresponding parameter-found-flag
							if kw = keyword_appearance then -- appearance sch_pcb
								expect_field_count (line, 2);
								appearance := to_appearance (f (line,2));
-- 								log (text => "appearance" & to_string (appearance), level => log_threshold + 1);								

								-- Create a new symbol where pointer "symbol" is pointing at.
								case appearance is
									when APPEARANCE_VIRTUAL =>
										symbol := new type_symbol' (
											appearance	=> APPEARANCE_VIRTUAL,
											others		=> <>);

									when APPEARANCE_PCB =>
										symbol := new type_symbol' (
											appearance	=> APPEARANCE_PCB,
											others		=> <>);

								end case;
								
							else
								invalid_keyword (kw);
							end if;
						end;

					when SEC_DRAW | SEC_TEXTS | SEC_PLACEHOLDERS | SEC_PORTS => 
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

					when SEC_LINE =>
						case stack.parent is
							when SEC_DRAW =>
								declare
									kw : string := f (line, 1);
									use et_primitive_objects;
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_start then -- start x 1 y 2
										expect_field_count (line, 5);

										-- extract the start position starting at field 2
										set_A (symbol_line, to_position (line, 2));
										
									elsif kw = keyword_end then -- end x 0.00 y 0.00
										expect_field_count (line, 5);

										-- extract the end position starting at field 2
										set_B (symbol_line, to_position (line,2));

									elsif kw = keyword_width then
										expect_field_count (line, 2);
										symbol_line.width := to_distance (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;

					when SEC_ARC =>
						case stack.parent is
							when SEC_DRAW =>
								declare
									use et_primitive_objects;
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_center then -- center x 1 y 2
										expect_field_count (line, 5);

										-- extract the start position starting at field 2
										set_center (symbol_arc, to_position (line, 2));

									elsif kw = keyword_start then -- start x 1 y 2
										expect_field_count (line, 5);

										-- extract the start position starting at field 2
										set_A (symbol_arc, to_position (line, 2));
										
									elsif kw = keyword_end then -- end x 0.00 y 0.00
										expect_field_count (line, 5);

										-- extract the end position starting at field 2
										set_B (symbol_arc, to_position (line, 2));

									elsif kw = keyword_direction then -- direction ccw
										expect_field_count (line, 2);

										set_direction (symbol_arc, to_direction (f (line, 2)));
										
									elsif kw = keyword_width then
										expect_field_count (line, 2);
										symbol_arc.width := to_distance (f (line, 2));

									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;
						
					when SEC_CIRCLE =>
						case stack.parent is
							when SEC_DRAW =>
								declare
									use et_primitive_objects;
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_center then -- center x 1 y 2
										expect_field_count (line, 5);

										-- extract the start position starting at field 2
										set_center (symbol_circle, to_position (line,2));

									elsif kw = keyword_width then -- widht 0.2
										expect_field_count (line, 2);
										set_width (symbol_circle, to_distance (f (line, 2)));

									elsif kw = keyword_radius then -- radius 5
										expect_field_count (line, 2);
										set_radius (symbol_circle, to_radius (f (line, 2)));

									elsif kw = keyword_filled then -- filled yes/no
										expect_field_count (line, 2);
										symbol_circle.filled := to_circle_filled (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;
						
					when SEC_TEXT =>
						case stack.parent is
							when SEC_TEXTS =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_position then -- position x 1 y 2
										expect_field_count (line, 5);

										-- extract the text position starting at field 2
										symbol_text_position := to_position (line,2);

									elsif kw = keyword_content then -- content "dummy NAND gate"
										expect_field_count (line, 2);
										symbol_text_content := et_text.to_content (f (line, 2));

									elsif kw = keyword_size then -- size 5
										expect_field_count (line, 2);
										symbol_text_base.size := to_distance (f (line, 2));

									elsif kw = keyword_rotation then -- rotation 90.0
										expect_field_count (line, 2);
										symbol_text_base.rotation := pac_text_schematic.to_rotation_doc (f (line, 2));
										
-- 									elsif kw = keyword_style then -- style italic
-- 										expect_field_count (line, 2);
-- 										symbol_text_base.style := to_text_style (f (line, 2));

									elsif kw = keyword_alignment then -- alignment horizontal center vertical center
										expect_field_count (line, 5);
										symbol_text_base.alignment := to_alignment (line, 2);

									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;
						
					when SEC_PLACEHOLDER =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_position then -- position x 1 y 2
										expect_field_count (line, 5);

										-- extract the placeholder position starting at field 2
										symbol_text_position := to_position (line,2);

									elsif kw = keyword_meaning then -- meaning reference
										expect_field_count (line, 2);
										symbol_placeholder_meaning := to_meaning (f (line, 2));

									elsif kw = keyword_size then -- size 5
										expect_field_count (line, 2);
										symbol_text_base.size := to_distance (f (line, 2));

									elsif kw = keyword_rotation then -- rotation 90.0
										expect_field_count (line, 2);
										symbol_text_base.rotation := pac_text_schematic.to_rotation_doc (f (line, 2));

									elsif kw = keyword_alignment then -- alignment horizontal center vertical center
										expect_field_count (line, 5);
										symbol_text_base.alignment := to_alignment (line, 2);

									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;

					when SEC_PORT =>
						case stack.parent is
							when SEC_PORTS =>
								declare
									kw : string := f (line, 1);
									use et_primitive_objects;
									use et_port_visibility;
									use et_port_sensitivity;
									use et_port_strength;
									use et_logic;
									use et_power_sources;
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_position then -- position x 1 y 2
										expect_field_count (line, 5);

										-- extract the port position starting at field 2
										port.position := to_position (line,2);

									elsif kw = keyword_name then -- name I1A
										expect_field_count (line, 2);
										port_name := et_port_names.to_port_name (f (line, 2));

									elsif kw = keyword_length then -- length 5
										expect_field_count (line, 2);
										port.length := to_distance (f (line, 2));
										-- CS warning on zero length ?

									elsif kw = keyword_rotation then -- rotation 90.0
										expect_field_count (line, 2);
										port.rotation := to_rotation (f (line, 2));
										
									elsif kw = keyword_port_name_visible then -- port_name_visible yes/no
										expect_field_count (line, 2);
										port.port_name_visible := to_port_name_visible (f (line, 2));

									elsif kw = keyword_port_name_size then -- port_name_size 2.0
										expect_field_count (line, 2);
										port.port_name_size := to_distance (f (line, 2));

									elsif kw = keyword_terminal_name_visible then -- terminal_name_visible yes/no
										expect_field_count (line, 2);
										port.terminal_name_visible := to_terminal_name_visible (f (line, 2));

									elsif kw = keyword_terminal_name_size then -- terminal_name_size 2.0
										expect_field_count (line, 2);
										port.terminal_name_size := to_distance (f (line, 2));

									elsif kw = keyword_direction then -- direction BIDIR, PASSIVE, NOT_CONNECTED, ...
										expect_field_count (line, 2);
										port_direction := et_port_direction.to_port_direction (f (line, 2));

									elsif kw = keyword_sensitivity_edge then -- sensitivity_edge rising/falling/any
										expect_field_count (line, 2);
										port_sensitivity_edge := to_sensitivity_edge (f (line, 2));

									elsif kw = keyword_sensitivity_level then -- sensitivity_level high/low
										expect_field_count (line, 2);
										port_sensitivity_level := to_sensitivity_level (f (line, 2));

									elsif kw = keyword_inverted then -- inverted yes/no
										expect_field_count (line, 2);
										port_output_inverted := to_output_inverted (f (line, 2));

									elsif kw = keyword_tristate then -- tristate yes/no
										expect_field_count (line, 2);
										port_output_tristate := to_output_tristate (f (line, 2));

									elsif kw = keyword_level then -- level positive/negative/zero
										expect_field_count (line, 2);
										port_power_level := to_power_level (f (line, 2));

									elsif kw = keyword_weakness then -- weakness none/pull0/weak1 ...
										expect_field_count (line, 2);
										port_output_weakness := to_output_weakness (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;
						
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
		log (text => "reading symbol " & to_string (file_name) & " ...", level => log_threshold);
		log_indentation_up;
		
		-- test if container et_symbol_model.symbols already contains the symbol
		-- named "file_name". If so, there would be no need to read the file_name again.
		if pac_symbols.contains (symbol_library, file_name) then
			log (text => "already read -> skipped", level => log_threshold + 1);
		else
			
			-- open symbol file
			open (
				file => file_handle,
				mode => in_file, 
				name => et_directory_and_file_ops.expand (to_string (file_name)));

			set_input (file_handle);
			
			-- Init section stack.
			stack.init;
			stack.push (SEC_INIT);

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
					process_line;
				end if;
			end loop;

			-- As a safety measure the top section must be reached finally.
			if stack.depth > 1 then 
				log (WARNING, write_section_stack_not_empty);
			end if;

			set_input (previous_input);
			close (file_handle);

			-- Insert the symbol (accessed by pointer symbol) in et_symbol_model.symbols:
			pac_symbols.insert (
				container	=> symbol_library, 
				key			=> file_name, -- libraries/symbols/nand.sym
				position	=> symbol_cursor,
				inserted	=> symbol_inserted,
				new_item	=> symbol.all);
			
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
