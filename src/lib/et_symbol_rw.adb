------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        SYMBOL READ AND WRITE                             --
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

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	--use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.text_io;				use ada.text_io;

with ada.exceptions;

with ada.containers;            use ada.containers;
with ada.containers.ordered_maps;

with et_general;				use et_general;

with et_general_rw;				use et_general_rw;
with et_geometry;				use et_geometry;
with et_text;					--use et_text;

package body et_symbol_rw is

	function to_grid (
		line : in et_string_processing.type_fields_of_line; -- "default x 1 y 1"
		from : in positive)
		return type_grid is
		use et_string_processing;
		
		grid : type_grid; -- to be returned
		place : positive := from; -- the field being read from given line

	begin
		while place <= positive (field_count (line)) loop

			-- We expect after the x the corresponding value for x
			if f (line, place) = keyword_x then
				grid.x := to_distance (f (line, place + 1));

			-- We expect after the y the corresponding value for y
			elsif f (line, place) = keyword_y then
				grid.y := to_distance (f (line, place + 1));

			else
				invalid_keyword (f (line, place));
			end if;
					
			place := place + 2;
		end loop;
		
		return grid;
	end to_grid;

	function position (pos : in type_point) return string is
	-- Returns something like "x 12.34 y 45.0".

		function text return string is begin return 
			keyword_x & to_string (x (pos)) 
			& space & keyword_y & to_string (y (pos));
		end text;
		
	begin
		return text; -- a 2d point has just x and y
	end position;

	function to_position (
		line : in et_string_processing.type_fields_of_line; -- "keyword x 3 y 4" or "position x 44.5 y 53.5"
		from : in positive)
		return type_point is

		use et_string_processing;
		
		point : type_point; -- to be returned
		place : positive := from; -- the field being read from given line

		-- CS: flags to detect missing x or y
	begin
		while place <= positive (field_count (line)) loop

			-- We expect after the x the corresponding value for x
			if f (line, place) = keyword_x then
				--set_x (point, to_distance (f (line, place + 1)));
				set (X, to_distance (f (line, place + 1)), point);

			-- We expect after the y the corresponding value for y
			elsif f (line, place) = keyword_y then
				--set_y (point, to_distance (f (line, place + 1)));
				set (Y, to_distance (f (line, place + 1)),point);

			else
				invalid_keyword (f (line, place));
			end if;
				
			place := place + 2;
		end loop;
		
		return point;
	end to_position;
	
	procedure write_text_properties (t : in type_text_basic'class) is
		use et_symbols.pac_text;
		use et_text;
	begin
		write (keyword => keyword_size, parameters => to_string (t.size));
		write (keyword => keyword_rotation, parameters => pac_text.to_string (t.rotation));
-- 		write (keyword => keyword_style, parameters => to_string (t.style));
		write (keyword => keyword_alignment, parameters =>
				keyword_horizontal & space & to_string (t.alignment.horizontal) & space &
				keyword_vertical   & space & to_string (t.alignment.vertical)
				);
	end write_text_properties;

	procedure create_symbol (
	-- Creates a symbol and stores it in container et_symbols.symbols.
		symbol_name		: in type_symbol_model_file.bounded_string; -- libraries/symbols/nand.sym
		appearance		: in type_appearance;
		log_threshold	: in et_string_processing.type_log_level) is
		use et_string_processing;
		use type_symbols;
	begin
		log (text => "creating symbol " & to_string (symbol_name) & " ...", level => log_threshold);
		log_indentation_up;
		log (text => "appearance " & to_string (appearance) & " ...", level => log_threshold);
		
		-- Test if symbol already exists. If already exists, issue warning and exit.
		if contains (symbols, symbol_name) then
			log (WARNING, text => "symbol already exists -> skipped", level => log_threshold + 1);
		else
			case appearance is
				when PCB =>
					insert (
						container	=> symbols,
						key			=> symbol_name,
						new_item	=> (appearance => PCB, others => <>)
						);

				when VIRTUAL =>
					insert (
						container	=> symbols,
						key			=> symbol_name,
						new_item	=> (appearance => VIRTUAL, others => <>)
						);
			end case;					
		end if;

		log_indentation_down;
	end create_symbol;
	
	procedure write_symbol ( 
		symbol			: in type_symbol;
		log_threshold	: in et_string_processing.type_log_level) is

		use et_symbols.pac_shapes;
		use et_text;

		use type_lines;
		use type_arcs;
		use type_circles;
		use pac_texts;
		use type_ports;

		procedure write_line (cursor : in type_lines.cursor) is begin
			section_mark (section_line, HEADER);
			write (keyword => keyword_start, parameters => position (element (cursor).start_point));
			write (keyword => keyword_end  , parameters => position (element (cursor).end_point));
			write (keyword => keyword_width, parameters => to_string (element (cursor).width));
			section_mark (section_line, FOOTER);
		end write_line;

		procedure write_arc (cursor : in type_arcs.cursor) is begin
			section_mark (section_arc, HEADER);
			write (keyword => keyword_center, parameters => position (element (cursor).center));
			write (keyword => keyword_start , parameters => position (element (cursor).start_point));
			write (keyword => keyword_end   , parameters => position (element (cursor).end_point));
			write (keyword => et_geometry.keyword_direction, parameters => to_string (element (cursor).direction));
			write (keyword => keyword_radius, parameters => to_string (element (cursor).radius));
			write (keyword => keyword_width , parameters => to_string (element (cursor).width));
			section_mark (section_arc, FOOTER);
		end write_arc;

		procedure write_circle (cursor : in type_circles.cursor) is begin
			section_mark (section_circle, HEADER);
			write (keyword => keyword_center, parameters => position (element (cursor).center));
			write (keyword => keyword_radius, parameters => to_string (element (cursor).radius));
			write (keyword => keyword_width , parameters => to_string (element (cursor).width));
			write (keyword => keyword_filled, parameters => to_string (element (cursor).filled));
			section_mark (section_circle, FOOTER);
		end write_circle;

		procedure write_text (cursor : in pac_texts.cursor) is begin
			section_mark (section_text, HEADER);
			write (keyword => keyword_position, parameters => position (element (cursor).position));
			write (keyword => keyword_content , parameters => to_string (element (cursor).content));			
			write_text_properties (element (cursor));
			section_mark (section_text, FOOTER);
		end write_text;

		procedure write_placeholders is begin
			case symbol.appearance is
				when PCB =>

					section_mark (section_placeholders, HEADER);
					
					section_mark (section_placeholder, HEADER);
					write (keyword => keyword_meaning, parameters => to_string (symbol.name.meaning));
					write (keyword => keyword_position, parameters => position (symbol.name.position));
					write_text_properties (symbol.name);
					section_mark (section_placeholder, FOOTER);

					section_mark (section_placeholder, HEADER);
					write (keyword => keyword_meaning , parameters => to_string (symbol.value.meaning));
					write (keyword => keyword_position, parameters => position (symbol.value.position));
					write_text_properties (symbol.value);
					section_mark (section_placeholder, FOOTER);

					section_mark (section_placeholder, HEADER);
					write (keyword => keyword_meaning , parameters => to_string (symbol.purpose.meaning));
					write (keyword => keyword_position, parameters => position (symbol.purpose.position));
					write_text_properties (symbol.purpose);
					section_mark (section_placeholder, FOOTER);

					section_mark (section_placeholders, FOOTER);
					
				when others => null;
			end case;
		end write_placeholders;

		procedure write_port (cursor : in type_ports.cursor) is begin
			section_mark (section_port, HEADER);
			write (keyword => keyword_name, parameters => to_string (key (cursor)));
			write (keyword => keyword_position, parameters => position (element (cursor).position));
			write (keyword => et_symbols.keyword_direction, parameters => to_string (element (cursor).direction));
			
			case element (cursor).direction is
				when INPUT_DIGITAL =>
					write (keyword => keyword_sensitivity_edge, parameters => to_string (element (cursor).sensitivity_edge));
					write (keyword => keyword_sensitivity_level, parameters => to_string (element (cursor).sensitivity_level));

				when OUTPUT_ANALOG =>
					write (keyword => keyword_tristate, parameters => to_string (element (cursor).output_analog_tristate));
					write (keyword => keyword_weakness, parameters => to_string (element (cursor).output_analog_weakness));
					
				when OUTPUT_DIGITAL =>
					write (keyword => keyword_inverted, parameters => to_string (element (cursor).output_digital_inverted));
					write (keyword => keyword_tristate, parameters => to_string (element (cursor).output_digital_tristate));
					write (keyword => keyword_weakness, parameters => to_string (element (cursor).output_digital_weakness));
					
				when BIDIR_DIGITAL =>
					write (keyword => keyword_output_inverted, parameters => to_string (element (cursor).output_inverted));
					write (keyword => keyword_output_tristate, parameters => to_string (element (cursor).output_tristate));
					write (keyword => keyword_output_weakness, parameters => to_string (element (cursor).output_weakness));

					write (keyword => keyword_input_sensitivity_edge, parameters => to_string (element (cursor).input_sensitivity_edge));
					write (keyword => keyword_input_sensitivity_level, parameters => to_string (element (cursor).input_sensitivity_level));

				when POWER_OUT | POWER_IN =>
					write (keyword => keyword_level, parameters => to_string (element (cursor).level));
					
				when others => null; -- PASSIVE, INPUT_ANALOG, NOT_CONNECTED
			end case;
			
			write (keyword => keyword_length, parameters => to_string (element (cursor).length));
			write (keyword => keyword_rotation, parameters => to_string (element (cursor).rotation));
			write (keyword => keyword_port_name_visible, parameters => to_string (element (cursor).port_name_visible));
			write (keyword => keyword_port_name_size, parameters => to_string (element (cursor).port_name_size));
			write (keyword => keyword_terminal_name_visible, parameters => to_string (element (cursor).terminal_name_visible));
			write (keyword => keyword_terminal_name_size, parameters => to_string (element (cursor).terminal_name_size));
			section_mark (section_port, FOOTER);			
		end write_port;
		
	begin -- write_symbol
		-- appearance
		write (keyword => keyword_appearance, parameters => to_string (symbol.appearance));
		
		-- draw section begin
		section_mark (section_draw, HEADER);
		
		-- lines
		iterate (symbol.shapes.lines, write_line'access);

		-- arcs
		iterate (symbol.shapes.arcs, write_arc'access);
		
		-- circles
		iterate (symbol.shapes.circles, write_circle'access);

		-- draw section end
		section_mark (section_draw, FOOTER);
		
		
		-- TEXTS
		section_mark (section_texts, HEADER);
		iterate (symbol.texts, write_text'access);
		section_mark (section_texts, FOOTER);

		-- PLACEHOLDERS
		if symbol.appearance = PCB then
			write_placeholders;
		end if;

		-- PORTS
		section_mark (section_ports, HEADER);
		iterate (symbol.ports, write_port'access);
		section_mark (section_ports, FOOTER);
	end write_symbol;
	
	procedure save_symbol (
	-- Saves the given symbol model in a file specified by file_name.
		file_name		: in type_symbol_model_file.bounded_string; -- libraries/symbols/nand.sym
		symbol			: in type_symbol; -- the actual symbol model
		log_threshold	: in et_string_processing.type_log_level) is
		use et_string_processing;
		file_handle : ada.text_io.file_type;
	begin
		log (text => to_string (file_name), level => log_threshold);

		create (
			file 	=> file_handle,
			mode	=> out_file,
			name	=> to_string (file_name));

		set_output (file_handle);
		
		-- write a nice header
		put_line (comment_mark & " " & et_general.system_name & " symbol");
		put_line (comment_mark & " " & date);
		put_line (comment_mark & " " & row_separator_double);
		new_line;

		write_symbol (symbol, log_threshold + 1);

		-- write footer
		new_line;		
		put_line (comment_mark & " " & row_separator_double);
		put_line (comment_mark & " symbol file end");
		new_line;
		
		reset_tab_depth;		
		
		set_output (standard_output);
		close (file_handle);

		exception when event: others =>
			log (text => ada.exceptions.exception_message (event));
			if is_open (file_handle) then
				close (file_handle);
			end if;
			raise;
		
	end save_symbol;
	
	procedure read_symbol (
	-- Opens the symbol file and stores the symbol in container et_symbols.symbols.
		file_name 		: in type_symbol_model_file.bounded_string; -- libraries/symbols/nand.sym
		log_threshold	: in et_string_processing.type_log_level) is
		use et_string_processing;
		use et_text;
		
		file_handle : ada.text_io.file_type;

		line : et_string_processing.type_fields_of_line;

		-- This is the section stack of the symbol model. 
		-- Here we track the sections. On entering a section, its name is
		-- pushed onto the stack. When leaving a section the latest section name is popped.
		max_section_depth : constant positive := 3; -- incl. section init

		package stack is new et_general_rw.stack_lifo (
			item	=> type_section,
			max 	=> max_section_depth);

		function to_string (section : in type_section) return string is
		-- Converts a section like SEC_DRAW to a string "draw".
			len : positive := type_section'image (section)'length;
		begin
			return to_lower (type_section'image (section) (5..len));
		end to_string;
		
		-- VARIABLES FOR TEMPORARILY STORAGE AND ASSOCIATED HOUSEKEEPING SUBPROGRAMS:
		appearance			: type_appearance;
		symbol				: access type_symbol;
		symbol_line			: type_line;
		symbol_arc			: type_arc;
		symbol_circle		: type_circle;
		symbol_text_base	: type_text_basic;

		symbol_cursor		: type_symbols.cursor;
		symbol_inserted		: boolean;
		
		symbol_text_position		: type_point;
		symbol_text_content			: et_text.type_text_content.bounded_string;
		symbol_placeholder_meaning	: type_placeholder_meaning := placeholder_meaning_default;
		
		port					: type_port_base;
		port_name				: type_port_name.bounded_string;
		port_direction			: type_port_direction := port_direction_default;
		port_sensitivity_edge	: type_sensitivity_edge := sensitivity_edge_default;
		port_sensitivity_level	: type_sensitivity_level := sensitivity_level_default;
		port_output_inverted	: type_output_inverted := output_inverted_default;
		port_output_tristate	: type_output_tristate := output_tristate_default;
		port_output_weakness	: type_output_weakness := output_weakness_default;
		port_power_level		: type_power_level := port_power_level_default;

		procedure insert_port is 
			inserted	: boolean;
			cursor		: type_ports.cursor;
		begin
			case port_direction is
				when PASSIVE =>
					type_ports.insert (
						container	=> symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> PASSIVE)
						);

				when INPUT_ANALOG =>
					type_ports.insert (
						container	=> symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> INPUT_ANALOG)
						);

				when INPUT_DIGITAL =>
					type_ports.insert (
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
					type_ports.insert (
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
					type_ports.insert (
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
					type_ports.insert (
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
					type_ports.insert (
						container	=> symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> POWER_OUT,
							level					=> port_power_level)
						);

				when POWER_IN =>
					type_ports.insert (
						container	=> symbol.ports,
						key			=> port_name,
						inserted	=> inserted,
						position	=> cursor,
						new_item	=> (port with 
							direction				=> POWER_IN,
							level					=> port_power_level)
						);

				when NOT_CONNECTED =>
					type_ports.insert (
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
								type_lines.append (
									container	=> symbol.shapes.lines,
									new_item	=> symbol_line);

								-- clean up for next line
								symbol_line := (others => <>);
								
							when others => invalid_section;
						end case;

					when SEC_ARC =>
						case stack.parent is
							when SEC_DRAW =>

								-- append symbol_arc to unit_symbol
								type_arcs.append (
									container	=> symbol.shapes.arcs,
									new_item	=> symbol_arc);

								-- clean up for next arc
								symbol_arc := (others => <>);
								
							when others => invalid_section;
						end case;
						
					when SEC_CIRCLE =>
						case stack.parent is
							when SEC_DRAW =>

								-- append symbol_circle to unit_symbol
								type_circles.append (
									container	=> symbol.shapes.circles,
									new_item	=> symbol_circle);

								-- clean up for next circle
								symbol_circle := (others => <>);
								
							when others => invalid_section;
						end case;
						
					when SEC_TEXT =>
						case stack.parent is
							when SEC_TEXTS =>

								-- append symbol text to symbol
								pac_texts.append (
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
										symbol.name := (symbol_text_base with 
											position	=> symbol_text_position,
											meaning		=> symbol_placeholder_meaning);

									when VALUE =>
										symbol.value := (symbol_text_base with 
											position	=> symbol_text_position,
											meaning		=> symbol_placeholder_meaning);

									when PURPOSE =>
										symbol.purpose := (symbol_text_base with 
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
				section			: in type_section) -- SEC_DRAW
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
									when VIRTUAL =>
										symbol := new type_symbol' (
											appearance	=> VIRTUAL,
											others		=> <>);

									when PCB =>
										symbol := new type_symbol' (
											appearance	=> PCB,
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
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_start then -- start x 1 y 2
										expect_field_count (line, 5);

										-- extract the start position starting at field 2
										symbol_line.start_point := to_position (line,2);
										
									elsif kw = keyword_end then -- end x 0.00 y 0.00
										expect_field_count (line, 5);

										-- extract the end position starting at field 2
										symbol_line.end_point := to_position (line,2);

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
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_center then -- center x 1 y 2
										expect_field_count (line, 5);

										-- extract the start position starting at field 2
										symbol_arc.center := to_position (line,2);

									elsif kw = keyword_start then -- start x 1 y 2
										expect_field_count (line, 5);

										-- extract the start position starting at field 2
										symbol_arc.start_point := to_position (line,2);
										
									elsif kw = keyword_end then -- end x 0.00 y 0.00
										expect_field_count (line, 5);

										-- extract the end position starting at field 2
										symbol_arc.end_point := to_position (line,2);

									elsif kw = et_geometry.keyword_direction then -- direction ccw
										expect_field_count (line, 2);

										symbol_arc.direction := to_direction (f (line, 2));
										
									elsif kw = keyword_width then
										expect_field_count (line, 2);
										symbol_arc.width := to_distance (f (line, 2));

									elsif kw = keyword_radius then
										expect_field_count (line, 2);
										symbol_arc.radius := to_distance (f (line, 2));
										
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
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_center then -- center x 1 y 2
										expect_field_count (line, 5);

										-- extract the start position starting at field 2
										symbol_circle.center := to_position (line,2);

									elsif kw = keyword_width then -- widht 0.2
										expect_field_count (line, 2);
										symbol_circle.width := to_distance (f (line, 2));

									elsif kw = keyword_radius then -- radius 5
										expect_field_count (line, 2);
										symbol_circle.radius := to_distance (f (line, 2));

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

									elsif kw = et_text.keyword_size then -- size 5
										expect_field_count (line, 2);
										symbol_text_base.size := to_distance (f (line, 2));

									elsif kw = keyword_rotation then -- rotation 90.0
										expect_field_count (line, 2);
										symbol_text_base.rotation := pac_text.to_rotation_doc (f (line, 2));
										
-- 									elsif kw = keyword_style then -- style italic
-- 										expect_field_count (line, 2);
-- 										symbol_text_base.style := to_text_style (f (line, 2));

									elsif kw = et_text.keyword_alignment then -- alignment horizontal center vertical center
										expect_field_count (line, 5);
										symbol_text_base.alignment := et_text.to_alignment (line, 2);

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

									elsif kw = et_text.keyword_size then -- size 5
										expect_field_count (line, 2);
										symbol_text_base.size := to_distance (f (line, 2));

									elsif kw = keyword_rotation then -- rotation 90.0
										expect_field_count (line, 2);
										symbol_text_base.rotation := pac_text.to_rotation_doc (f (line, 2));

									elsif kw = et_text.keyword_alignment then -- alignment horizontal center vertical center
										expect_field_count (line, 5);
										symbol_text_base.alignment := et_text.to_alignment (line, 2);

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
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_position then -- position x 1 y 2
										expect_field_count (line, 5);

										-- extract the port position starting at field 2
										port.position := to_position (line,2);

									elsif kw = keyword_name then -- name I1A
										expect_field_count (line, 2);
										port_name := to_port_name (f (line, 2));

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

									elsif kw = et_symbols.keyword_direction then -- direction BIDIR, PASSIVE, NOT_CONNECTED, ...
										expect_field_count (line, 2);
										port_direction := to_port_direction (f (line, 2));

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
					 & affected_line (line) & to_string (line), console => true);
				raise;
			
		end process_line;

		previous_input : ada.text_io.file_type renames current_input;
		
	begin -- read_symbol
		log_indentation_up;
		log (text => "reading symbol " & to_string (file_name) & " ...", level => log_threshold);
		log_indentation_up;
		
		-- test if container et_symbols.symbols already contains the symbol
		-- named "file_name". If so, there would be no need to read the file_name again.
		if type_symbols.contains (symbols, file_name) then
			log (text => "already read -> skipped", level => log_threshold + 1);
		else
			
			-- open symbol file
			open (
				file => file_handle,
				mode => in_file, 
				name => expand (to_string (file_name)));

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
				log (WARNING, write_section_stack_not_empty);
			end if;

			set_input (previous_input);
			close (file_handle);

			-- Insert the symbol (accessed by pointer symbol) in et_symbols.symbols:
			type_symbols.insert (
				container	=> symbols, 
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

	
end et_symbol_rw;
