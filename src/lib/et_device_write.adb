------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         DEVICE MODEL / WRITE                             --
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

with ada.characters;			use ada.characters;
with ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.text_io;				use ada.text_io;
-- with ada.tags;

with ada.directories;
with ada.exceptions;

with ada.containers;            use ada.containers;
with ada.containers.ordered_maps;

with et_directory_and_file_ops;
with et_primitive_objects;			use et_primitive_objects;
with et_coordinates_formatting;		use et_coordinates_formatting;
with et_schematic_geometry;			use et_schematic_geometry;
use et_schematic_geometry.pac_geometry_2;
with et_schematic_coordinates;		use et_schematic_coordinates;

with et_string_processing;
with et_time;
with et_file_write;					use et_file_write;
with et_package_write;
with et_package_read;
with et_conventions;
with et_text;
with et_alignment;					use et_alignment;
with et_power_sources;
with et_logic;
with et_port_strength;
with et_port_sensitivity;
with et_port_visibility;
with et_port_direction;
with et_port_names;

with et_package_name;				use et_package_name;
with et_device_placeholders;		use et_device_placeholders;
with et_device_partcode;			use et_device_partcode;
with et_pcb_stack;
with et_schematic_text;
with et_system_info;
with et_device_value;
with et_device_prefix;
with et_unit_name;

with et_package_library;
with et_package_variant_name;
with et_package_variant;
with et_package_variant_terminal_port_map;
with et_package_model_name;			use et_package_model_name;

with et_device_library;				use et_device_library;
with et_keywords;					use et_keywords;
with et_file_sections;				use et_file_sections;
with et_terminal_name;				use et_terminal_name;
with et_terminals;					use et_terminals;

with et_device_write_unit;			use et_device_write_unit;



package body et_device_write is

	

	procedure write_device (
		file_name		: in pac_device_model_file.bounded_string; -- ../lbr/logic_ttl/7400.dev
		device			: in type_device_model; -- the actual device model
		log_threshold	: in type_log_level)
	is
		use et_string_processing;
		use et_time;
		use et_system_info;
		use et_device_value;
		use et_device_prefix;

		use et_unit_name;
		use pac_unit_name;
		
		file_handle : ada.text_io.file_type;

		
		use et_package_variant;
		use pac_package_variants;
		
		use et_package_variant_name;
		use pac_package_variant_name;
		
		variant_cursor : pac_package_variants.cursor;

		
		
		procedure write_variant (
			packge	: in pac_package_variant_name.bounded_string;
			variant	: in type_package_variant) 
		is
			use et_package_library;
			use pac_package_variant_name;
			use et_port_names;

			use et_package_variant_terminal_port_map;
			use pac_terminal_port_map;	
			
			procedure write_terminal (terminal_cursor : in pac_terminal_port_map.cursor) is begin
				write (keyword => keyword_terminal, parameters => 
					space & to_string (key (terminal_cursor)) & space -- terminal name like G14 or 16
					& keyword_unit & space & to_string (element (terminal_cursor).unit) -- unit name like A,B or GPIO_BANK_1
					& space & keyword_port & space & to_string (element (terminal_cursor).name) 	-- port name like CE, WE, GND
					);
			end write_terminal;

			
		begin
			write (keyword => keyword_package_model, 
				   parameters => get_package_model_name (variant.model_cursor));
					-- CS path correct ??
			
			section_mark (section_terminal_port_map, HEADER);
			iterate (variant.terminal_port_map, write_terminal'access);
			section_mark (section_terminal_port_map, FOOTER);						
		end write_variant;


		
	begin -- save_device
		log (text => "save device model as " & to_string (file_name),
			 level => log_threshold);

		log_indentation_up;

		create (
			file 	=> file_handle,
			mode	=> out_file,
			name	=> to_string (file_name));

		set_output (file_handle);
		
		-- write a nice header
		put_line (comment_mark_default & " " & system_name & " device");
		put_line (comment_mark_default & " " & get_date);
		put_line (comment_mark_default & " " & row_separator_double);
		new_line;

		reset_tab_depth;

		-- prefix, appearance ...
		write (keyword => keyword_prefix, parameters => to_string (device.prefix));
		write (keyword => keyword_appearance, parameters => to_string (device.appearance));

		
		-- package variants
		case device.appearance is
			when APPEARANCE_PCB =>
				write (keyword => keyword_value, parameters => to_string (device.value));
				--write (keyword => keyword_partcode, parameters => to_string (device.partcode));

				section_mark (section_variants, HEADER);

				variant_cursor := device.variants.first;
				while variant_cursor /= pac_package_variants.no_element loop
					section_mark (section_variant, HEADER);
					write (keyword => keyword_name, parameters => to_string (key (variant_cursor)));

					query_element (
						position	=> variant_cursor,
						process		=> write_variant'access);

					section_mark (section_variant, FOOTER);					
					next (variant_cursor);
				end loop;

				section_mark (section_variants, FOOTER);
				
			when others => null;				
		end case;


		-- CS wrap in a procedure
		log (text => "units", level => log_threshold + 1);
		log_indentation_up;
		write_internal_units (device.units_internal, log_threshold + 2);
		write_external_units (device.units_external, log_threshold + 2);
		log_indentation_down;
		
		

		-- write footer
		new_line;		
		put_line (comment_mark_default & " " & row_separator_double);
		put_line (comment_mark_default & " device model file end");
		new_line;

		reset_tab_depth;
		
		set_output (standard_output);
		close (file_handle);

		log_indentation_down;
		

		exception when event: others =>
			log_indentation_down;
			log (text => ada.exceptions.exception_message (event));
			if is_open (file_handle) then
				close (file_handle);
			end if;
			raise;
		
	end write_device;



		
end et_device_write;
