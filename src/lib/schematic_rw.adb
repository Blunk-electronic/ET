------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             SCHEMATIC_RW                                 --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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
with ada.tags;

with ada.exceptions;

with ada.containers;            use ada.containers;
with ada.containers.ordered_maps;

with et_libraries;
with et_general;				use et_general;

with et_coordinates;			use et_coordinates;
use et_coordinates.geometry;

with et_string_processing;
with general_rw;				use general_rw;
with et_geometry;				use et_geometry;
with et_text;					--use et_text;

package body schematic_rw is

	function to_grid (
		line : in et_string_processing.type_fields_of_line; -- "default x 1 y 1"
		from : in positive)
		return et_coordinates.geometry.type_grid is
		use et_string_processing;
		use et_coordinates.geometry;
		
		grid : et_coordinates.geometry.type_grid; -- to be returned

		place : positive := from; -- the field being read from given line

	begin
		while place <= positive (field_count (line)) loop

			-- We expect after the x the corresponding value for x
			if f (line, place) = keyword_pos_x then
				grid.x := to_distance (f (line, place + 1));

			-- We expect after the y the corresponding value for y
			elsif f (line, place) = keyword_pos_y then
				grid.y := to_distance (f (line, place + 1));

			else
				invalid_keyword (f (line, place));
			end if;
					
			place := place + 2;
		end loop;
		
		return grid;
	end to_grid;

	function position (pos : in et_coordinates.geometry.type_point'class) return string is
	-- Returns something like "x 12.34 y 45.0" or "sheet 3 x 12.34 y 45.0".
	-- This kind of output depends on the tag of the given object.
		use et_coordinates.geometry;
		use ada.tags;

		-- This function returns the basic text with x and y coordinates.
		function text return string is begin return 
			space & keyword_pos_x & to_string (x (pos)) 
			& space & keyword_pos_y & to_string (y (pos));
		end text;
		
	begin -- position
		if pos'tag = type_point'tag then
			return text; -- a 2d point has just x and y
		else
			-- A type_coordinates also has the sheet number:
			return space & keyword_sheet & to_sheet (sheet (et_coordinates.type_position (pos))) & text;
		end if;
	end position;

	function to_position (
		line : in et_string_processing.type_fields_of_line; -- "keyword x 3 y 4" or "position x 44.5 y 53.5"
		from : in positive)
		return et_coordinates.geometry.type_point is

		use et_string_processing;
		use geometry;
		
		point : et_coordinates.geometry.type_point; -- to be returned

		place : positive := from; -- the field being read from given line

		-- CS: flags to detect missing x or y
	begin
		while place <= positive (field_count (line)) loop

			-- We expect after the x the corresponding value for x
			if f (line, place) = keyword_pos_x then
				--set_x (point, to_distance (f (line, place + 1)));
				set (X, to_distance (f (line, place + 1)), point);

			-- We expect after the y the corresponding value for y
			elsif f (line, place) = keyword_pos_y then
				--set_y (point, to_distance (f (line, place + 1)));
				set (Y, to_distance (f (line, place + 1)),point);

			else
				invalid_keyword (f (line, place));
			end if;
				
			place := place + 2;
		end loop;
		
		return point;
	end to_position;
	
	procedure write_text_properties (t : in et_libraries.type_text_basic'class) is
		use et_libraries.pac_text;
		use et_text;
	begin
		write (keyword => keyword_size, parameters => et_libraries.pac_text.to_string (t.size));
		write (keyword => et_libraries.keyword_line_width, parameters => et_libraries.pac_text.to_string (t.line_width));
		write (keyword => keyword_rotation, parameters => geometry.to_string (t.rotation));
		write (keyword => keyword_style, parameters => et_libraries.to_string (t.style));
		write (keyword => keyword_alignment, parameters => space &
				keyword_horizontal & to_string (t.alignment.horizontal) & space &
				keyword_vertical   & to_string (t.alignment.vertical)
				);
		--write (keyword => keyword_hidden, parameters => et_libraries.to_string (text.visible)); -- CS: no need. probably useless
	end write_text_properties;

	
	procedure read_symbol (
	-- Opens the symbol file and stores the symbol in container et_libraries.symbols.
		file_name 		: in et_libraries.type_symbol_model_file.bounded_string; -- libraries/symbols/nand.sym
		log_threshold	: in et_string_processing.type_log_level) is
		use et_coordinates.geometry;
		use et_string_processing;
		use et_libraries;
		use et_text;
		
		file_handle : ada.text_io.file_type;

		line : et_string_processing.type_fields_of_line;

		-- This is the section stack of the symbol model. 
		-- Here we track the sections. On entering a section, its name is
		-- pushed onto the stack. When leaving a section the latest section name is popped.
		max_section_depth : constant positive := 3; -- incl. section init

		package stack is new general_rw.stack_lifo (
			item	=> type_section_name_symbol,
			max 	=> max_section_depth);

		function to_string (section : in type_section_name_symbol) return string is
		-- Converts a section like SEC_DRAW to a string "draw".
			len : positive := type_section_name_symbol'image (section)'length;
		begin
			return to_lower (type_section_name_symbol'image (section) (5..len));
		end to_string;
		
		-- VARIABLES FOR TEMPORARILY STORAGE AND ASSOCIATED HOUSEKEEPING SUBPROGRAMS:
		appearance			: type_device_appearance; -- sch, sch_pcb
		symbol				: access type_symbol;
		symbol_line			: et_libraries.type_line;
		symbol_arc			: et_libraries.type_arc;
		symbol_circle		: et_libraries.type_circle;
		symbol_text_base	: et_libraries.type_text_basic;
		symbol_text_position: et_coordinates.geometry.type_point;
		symbol_text_content	: et_text.type_text_content.bounded_string;
		symbol_placeholder_meaning : et_libraries.type_text_meaning := text_meaning_default;
		
		port					: et_libraries.type_port_base;
		port_name				: et_libraries.type_port_name.bounded_string;
		port_direction			: et_libraries.type_port_direction := port_direction_default;
		port_sensitivity_edge	: et_libraries.type_sensitivity_edge := sensitivity_edge_default;
		port_sensitivity_level	: et_libraries.type_sensitivity_level := sensitivity_level_default;
		port_output_inverted	: et_libraries.type_output_inverted := output_inverted_default;
		port_output_tristate	: et_libraries.type_output_tristate := output_tristate_default;
		port_output_weakness	: et_libraries.type_output_weakness := output_weakness_default;
		port_power_level		: et_libraries.type_power_level := port_power_level_default;

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
								et_libraries.type_lines.append (
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
								et_libraries.type_arcs.append (
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
								et_libraries.type_circles.append (
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
								type_symbol_texts.append (
									container	=> symbol.texts,
									new_item	=> (symbol_text_base with
											meaning		=> MISC,
											content		=> symbol_text_content,
											position	=> symbol_text_position));

								-- clean up for next symbol text
								symbol_text_base := (others => <>);
								symbol_text_content := to_content ("");
								symbol_text_position := geometry.origin;
								
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

									when et_libraries.VALUE =>
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
								symbol_text_position := geometry.origin;
								symbol_placeholder_meaning := text_meaning_default;
							
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
				section			: in type_section_name_symbol) -- SEC_DRAW
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
								appearance := et_libraries.to_appearance (f (line,2));
-- 								log (text => "appearance" & to_string (appearance), level => log_threshold + 1);								

								-- Create a new symbol where pointer "symbol" is pointing at.
								case appearance is
									when et_libraries.SCH =>
										symbol := new et_libraries.type_symbol' (
											appearance	=> et_libraries.SCH,
											others		=> <>);

									when et_libraries.SCH_PCB =>
										symbol := new et_libraries.type_symbol' (
											appearance	=> et_libraries.SCH_PCB,
											others		=> <>);

									when others => 
										raise constraint_error; -- CS

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
										symbol_circle.filled := et_libraries.to_circle_filled (f (line, 2));
										
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

									elsif kw = et_text.keyword_line_width then -- line_width 0.2
										expect_field_count (line, 2);
										symbol_text_base.line_width := to_distance (f (line, 2));

									elsif kw = keyword_rotation then -- rotation 90.0
										expect_field_count (line, 2);
										symbol_text_base.rotation := to_rotation (f (line, 2));
										
									elsif kw = keyword_style then -- style italic
										expect_field_count (line, 2);
										symbol_text_base.style := et_libraries.to_text_style (f (line, 2));

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
										symbol_placeholder_meaning := et_libraries.to_text_meaning (f (line, 2));

									elsif kw = et_text.keyword_size then -- size 5
										expect_field_count (line, 2);
										symbol_text_base.size := to_distance (f (line, 2));

									elsif kw = et_text.keyword_line_width then -- line_width 0.2
										expect_field_count (line, 2);
										symbol_text_base.line_width := to_distance (f (line, 2));

									elsif kw = keyword_rotation then -- rotation 90.0
										expect_field_count (line, 2);
										symbol_text_base.rotation := to_rotation (f (line, 2));
										
									elsif kw = keyword_style then -- style italic
										expect_field_count (line, 2);
										symbol_text_base.style := et_libraries.to_text_style (f (line, 2));

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
										port_name := et_libraries.to_port_name (f (line, 2));

									elsif kw = keyword_length then -- length 5
										expect_field_count (line, 2);
										port.length := to_distance (f (line, 2));

									elsif kw = keyword_rotation then -- rotation 90.0
										expect_field_count (line, 2);
										port.rotation := to_rotation (f (line, 2));
										
									elsif kw = keyword_port_name_visible then -- port_name_visible yes/no
										expect_field_count (line, 2);
										port.port_name_visible := et_libraries.to_port_name_visible (f (line, 2));

									elsif kw = keyword_port_name_size then -- port_name_size 2.0
										expect_field_count (line, 2);
										port.port_name_size := to_distance (f (line, 2));

									elsif kw = keyword_terminal_name_visible then -- terminal_name_visible yes/no
										expect_field_count (line, 2);
										port.terminal_name_visible := et_libraries.to_terminal_name_visible (f (line, 2));

									elsif kw = keyword_terminal_name_size then -- terminal_name_size 2.0
										expect_field_count (line, 2);
										port.terminal_name_size := to_distance (f (line, 2));

									elsif kw = keyword_direction then -- direction BIDIR, PASSIVE, NOT_CONNECTED, ...
										expect_field_count (line, 2);
										port_direction := et_libraries.to_port_direction (f (line, 2));

									elsif kw = keyword_sensitivity_edge then -- sensitivity_edge rising/falling/any
										expect_field_count (line, 2);
										port_sensitivity_edge := et_libraries.to_sensitivity_edge (f (line, 2));

									elsif kw = keyword_sensitivity_level then -- sensitivity_level high/low
										expect_field_count (line, 2);
										port_sensitivity_level := et_libraries.to_sensitivity_level (f (line, 2));

									elsif kw = keyword_inverted then -- inverted yes/no
										expect_field_count (line, 2);
										port_output_inverted := et_libraries.to_output_inverted (f (line, 2));

									elsif kw = keyword_tristate then -- tristate yes/no
										expect_field_count (line, 2);
										port_output_tristate := et_libraries.to_output_tristate (f (line, 2));

									elsif kw = keyword_level then -- level positive/negative/zero
										expect_field_count (line, 2);
										port_power_level := et_libraries.to_power_level (f (line, 2));

									elsif kw = keyword_weakness then -- weakness none/pull0/weak1 ...
										expect_field_count (line, 2);
										port_output_weakness := et_libraries.to_output_weakness (f (line, 2));
										
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
		
		-- test if container et_libraries.symbols already contains the symbol
		-- named "file_name". If so, there would be no need to read the file_name again.
		if et_libraries.type_symbols.contains (et_libraries.symbols, file_name) then
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

			-- Insert the symbol (accessed by pointer symbol) in et_libraries.symbols:
			et_libraries.type_symbols.insert (
				container	=> et_libraries.symbols, 
				key			=> file_name, -- libraries/symbols/nand.sym
				new_item	=> symbol.all);

		end if;

		-- CS Check integrity of symbol (style guides, conventions ...)
		-- use function "last" to fetch latest symbol

		log_indentation_down;
		log_indentation_down;		

		exception when event: others =>
			if is_open (file_handle) then 
				set_input (previous_input);
				close (file_handle); 
			end if;
			raise;

	end read_symbol;

	
end schematic_rw;
