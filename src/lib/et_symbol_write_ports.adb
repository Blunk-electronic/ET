------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         SYMBOL WRITE / PORTS                             --
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


with et_schematic_geometry;			use et_schematic_geometry;
with et_symbol_ports;				use et_symbol_ports;

with et_logic;
with et_power_sources;
with et_port_sensitivity;
with et_port_strength;
with et_port_visibility;
with et_port_direction;
with et_port_names;

with et_coordinates_formatting;		use et_coordinates_formatting;
with et_file_sections;				use et_file_sections;
with et_file_write;					use et_file_write;

with et_keywords;					use et_keywords;


package body et_symbol_write_ports is
	
	use pac_geometry_2;


	
	
	procedure write_ports ( 
		symbol			: in type_symbol_model;
		log_threshold	: in type_log_level)
	is
		use pac_symbol_ports;
		
		
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


		
	begin
		log (text => "write ports", level => log_threshold);
		log_indentation_up;

		section_mark (section_ports, HEADER);
		iterate (symbol.ports, write_port'access);
		section_mark (section_ports, FOOTER);
		
		log_indentation_down;
	end write_ports;

	
	
		
end et_symbol_write_ports;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
