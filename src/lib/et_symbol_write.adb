------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            SYMBOL WRITE                                  --
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


package body et_symbol_write is

	
	procedure write_text_properties (t : in type_text_basic'class) is
		use et_text;
	begin
		write (keyword => keyword_size, parameters => to_string (t.size));
		write (keyword => keyword_rotation, parameters => to_string (t.rotation));
-- 		write (keyword => keyword_style, parameters => to_string (t.style));
		write (keyword => keyword_alignment, parameters =>
				keyword_horizontal & space & to_string (t.alignment.horizontal) & space &
				keyword_vertical   & space & to_string (t.alignment.vertical)
				);
	end write_text_properties;



	
	procedure create_symbol (
		symbol_name		: in pac_symbol_model_file.bounded_string; -- libraries/symbols/nand.sym
		appearance		: in type_appearance;
		log_threshold	: in type_log_level) 
	is
		use et_string_processing;
		use pac_symbols;
	begin
		log (text => "creating symbol " & to_string (symbol_name) & " ...", level => log_threshold);
		log_indentation_up;
		log (text => "appearance " & to_string (appearance) & " ...", level => log_threshold);
		
		-- Test if symbol already exists. If already exists, issue warning and exit.
		if contains (symbol_library, symbol_name) then
			log (WARNING, text => "symbol already exists -> skipped", level => log_threshold + 1);
		else
			case appearance is
				when APPEARANCE_PCB =>
					insert (
						container	=> symbol_library,
						key			=> symbol_name,
						new_item	=> (appearance => APPEARANCE_PCB, others => <>)
						);

				when APPEARANCE_VIRTUAL =>
					insert (
						container	=> symbol_library,
						key			=> symbol_name,
						new_item	=> (appearance => APPEARANCE_VIRTUAL, others => <>)
						);
			end case;					
		end if;

		log_indentation_down;
	end create_symbol;

	

	
	procedure write_symbol ( 
		symbol			: in type_symbol;
		log_threshold	: in type_log_level)
	is
		use et_text;

		use pac_symbol_lines;
		use pac_symbol_arcs;
		use pac_symbol_circles;
		use pac_symbol_texts;	
		use pac_symbol_ports;
		
		
		procedure write_line (cursor : in pac_symbol_lines.cursor) is 
			use et_primitive_objects;
		begin
			section_mark (section_line, HEADER);
			write (keyword => keyword_start, parameters => to_string (get_A (cursor), FORMAT_2));
			write (keyword => keyword_end  , parameters => to_string (get_B (cursor), FORMAT_2));
			write (keyword => keyword_width, parameters => to_string (element (cursor).width));
			section_mark (section_line, FOOTER);
		end write_line;

		
		procedure write_arc (cursor : in pac_symbol_arcs.cursor) is 
			use et_primitive_objects;
		begin
			section_mark (section_arc, HEADER);
			write (keyword => keyword_center, parameters => to_string (get_center (cursor), FORMAT_2));
			write (keyword => keyword_start , parameters => to_string (get_A (cursor), FORMAT_2));
			write (keyword => keyword_end   , parameters => to_string (get_B (cursor), FORMAT_2));
			write (keyword => keyword_direction, parameters => to_string (get_direction (cursor)));
			write (keyword => keyword_width , parameters => to_string (element (cursor).width));
			section_mark (section_arc, FOOTER);
		end write_arc;

		
		procedure write_circle (cursor : in pac_symbol_circles.cursor) is 
			use et_primitive_objects;
			use et_schematic_geometry.pac_geometry_sch;
		begin
			section_mark (section_circle, HEADER);
			write (keyword => keyword_center, parameters => to_string (get_center (element (cursor)), FORMAT_2));
			write (keyword => keyword_radius, parameters => to_string (get_radius (element (cursor))));
			write (keyword => keyword_width , parameters => to_string (element (cursor).width));
			write (keyword => keyword_filled, parameters => to_string (element (cursor).filled));
			section_mark (section_circle, FOOTER);
		end write_circle;
		

		procedure write_text (cursor : in pac_symbol_texts.cursor) is begin
			section_mark (section_text, HEADER);
			write (keyword => keyword_position, parameters => to_string (element (cursor).position, FORMAT_2));
			write (keyword => keyword_content , parameters => to_string (element (cursor).content));			
			write_text_properties (element (cursor));
			section_mark (section_text, FOOTER);
		end write_text;
		

		procedure write_placeholders is begin
			case symbol.appearance is
				when APPEARANCE_PCB =>

					section_mark (section_placeholders, HEADER);
					
					section_mark (section_placeholder, HEADER);
					write (keyword => keyword_meaning, parameters => to_string (symbol.placeholders.name.meaning));
					write (keyword => keyword_position, parameters => to_string (symbol.placeholders.name.position, FORMAT_2));
					write_text_properties (symbol.placeholders.name);
					section_mark (section_placeholder, FOOTER);

					section_mark (section_placeholder, HEADER);
					write (keyword => keyword_meaning , parameters => to_string (symbol.placeholders.value.meaning));
					write (keyword => keyword_position, parameters => to_string (symbol.placeholders.value.position, FORMAT_2));
					write_text_properties (symbol.placeholders.value);
					section_mark (section_placeholder, FOOTER);

					section_mark (section_placeholder, HEADER);
					write (keyword => keyword_meaning , parameters => to_string (symbol.placeholders.purpose.meaning));
					write (keyword => keyword_position, parameters => to_string (symbol.placeholders.purpose.position, FORMAT_2));
					write_text_properties (symbol.placeholders.purpose);
					section_mark (section_placeholder, FOOTER);

					section_mark (section_placeholders, FOOTER);
					
				when others => null;
			end case;
		end write_placeholders;

		
		procedure write_port (cursor : in pac_symbol_ports.cursor) is 
			use et_port_names;
			use et_port_direction;
			use et_port_visibility;
			use et_port_sensitivity;
			use et_port_strength;
			use et_logic;
			use et_power_sources;
		begin
			section_mark (section_port, HEADER);
			write (keyword => keyword_name, parameters => to_string (key (cursor)));
			write (keyword => keyword_position, parameters => to_string (element (cursor).position, FORMAT_2));
			write (keyword => keyword_direction, parameters => to_string (element (cursor).direction));
			
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
		if symbol.appearance = APPEARANCE_PCB then
			write_placeholders;
		end if;

		-- PORTS
		section_mark (section_ports, HEADER);
		iterate (symbol.ports, write_port'access);
		section_mark (section_ports, FOOTER);
	end write_symbol;



	
	
	procedure save_symbol (
		file_name		: in pac_symbol_model_file.bounded_string; -- libraries/symbols/nand.sym
		symbol			: in type_symbol; -- the actual symbol model
		log_threshold	: in type_log_level)
	is
		use et_string_processing;
		use et_system_info;
		file_handle : ada.text_io.file_type;
	begin
		log (text => to_string (file_name), level => log_threshold);

		create (
			file 	=> file_handle,
			mode	=> out_file,
			name	=> to_string (file_name));

		set_output (file_handle);
		
		-- write a nice header
		put_line (comment_mark & " " & system_name & " symbol");
		put_line (comment_mark & " " & get_date);
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
	
	
end et_symbol_write;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
