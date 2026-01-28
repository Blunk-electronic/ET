------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE WRITE / ELECTRICAL DEVICES                     --
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
--                                                                          --
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
-- ToDo:
-- - clean up
--
--
--

with ada.text_io;					use ada.text_io;
with ada.strings;					use ada.strings;

with et_module;						use et_module;
with et_module_names;				use et_module_names;
with et_schematic_geometry;
with et_schematic_coordinates;

with et_keywords;					use et_keywords;

with et_pcb_sides;
with et_board_geometry;
with et_board_coordinates;

with et_coordinates_formatting;		use et_coordinates_formatting;
with et_device_name;				use et_device_name;

with et_device_model;
with et_device_appearance;
with et_device_purpose;
with et_device_model_names;

with et_device_value;
with et_device_partcode;

with et_package_variant;
with et_device_write;
with et_symbol_write;
with et_devices_electrical;			use et_devices_electrical;

with et_device_placeholders;
with et_device_placeholders.packages;
with et_device_placeholders.symbols;

with et_unit_name;
with et_units;
with et_mirroring;						use et_mirroring;
with et_alignment;						use et_alignment;

with et_file_write;						use et_file_write;
with et_file_sections;					use et_file_sections;



package body et_module_write_devices_electrical is

	use pac_generic_modules;


	
	procedure write_devices_electrical (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		use pac_devices_electrical;
		

		-- This procedure queries the units of
		-- the device candidate:
		procedure query_units (
			device_name	: in type_device_name;
			device		: in type_device_electrical) 
		is
			use et_schematic_coordinates;
			use et_units;
			use pac_units;
						
			unit_cursor : pac_units.cursor := device.units.first;

			use et_schematic_geometry.pac_geometry_2;
			use et_device_placeholders.symbols;
			
			
			procedure write_placeholder (
				ph : in type_text_placeholder) 
			is 
				use et_symbol_write;
				use et_device_placeholders;
			begin
				section_mark (section_placeholder, HEADER);
				write (keyword => keyword_meaning, parameters => to_string (ph.meaning));
				write (keyword => keyword_position, parameters => to_string (ph.position, FORMAT_2));
				write_text_properties (ph);
				section_mark (section_placeholder, FOOTER);
			end write_placeholder;

			
			use et_device_write;
			use et_device_appearance;
			use et_unit_name.pac_unit_name;

			
		begin -- query_units
			section_mark (section_units, HEADER);
			while unit_cursor /= pac_units.no_element loop
				section_mark (section_unit, HEADER);
				write (keyword => keyword_name, parameters => to_string (key (unit_cursor)));
				
				write (
					keyword => keyword_position, 
					parameters => to_string (element (unit_cursor).position, FORMAT_2)); -- position sheet 1 x 147.32 y 96.97 rotation 90.0
				
				write (keyword => keyword_mirrored, parameters => to_string (element (unit_cursor).mirror, verbose => false)); -- x_axis, y_axis, none

				if element (unit_cursor).appearance = APPEARANCE_PCB then
					section_mark (section_placeholders, HEADER);
					
					write_placeholder (element (unit_cursor).placeholders.name);
					write_placeholder (element (unit_cursor).placeholders.value);
					write_placeholder (element (unit_cursor).placeholders.purpose);
					--write_placeholder (element (unit_cursor).partcode);

					section_mark (section_placeholders, FOOTER);
				end if;
				
				section_mark (section_unit, FOOTER);
				next (unit_cursor);
			end loop;
			section_mark (section_units, FOOTER);
		end query_units;



		-- This procedure queries the placeholders of
		-- the package of the device candidate:
		procedure query_placeholders (
			device_name : in type_device_name;
			device 		: in type_device_electrical) 
		is
			use et_pcb_sides;
			use et_board_coordinates;
			use et_device_placeholders.packages;
			use pac_text_placeholders;

			face : type_face;
			layer : type_placeholder_layer;

			
			procedure write_placeholder (c : in pac_text_placeholders.cursor) is 
				ph : type_text_placeholder renames element (c);
				use et_device_placeholders;

				use et_board_geometry;
				use pac_geometry_2;
				position : type_package_position;
			begin
				section_mark (section_placeholder, HEADER);
				write (keyword => keyword_layer, parameters => to_string (layer));
				write (keyword => keyword_meaning, parameters => to_string (get_meaning (ph)));
				write (keyword => keyword_anchor, parameters => get_anchor_mode (ph));

				-------
				-- CS rework:
				
				-- Assemble the text position:
				set_position (position, get_position (ph));
				set_face (position, face);
							
				write (keyword => keyword_position, 
					parameters => to_string (position, FORMAT_2)); 
				-- position x 0.000 y 5.555 rotation 0.00 face top

				write (keyword => keyword_size, 
					parameters => to_string (ph.size)); -- size 1.000
				
				write (keyword => keyword_linewidth, 
					parameters => to_string (ph.line_width));
					
				write (keyword => keyword_alignment, 
					parameters =>
						keyword_horizontal & space & to_string (ph.alignment.horizontal) & space &
						keyword_vertical   & space & to_string (ph.alignment.vertical)
					);

				-- CS use et_alignment.to_string 
				--------
				
				section_mark (section_placeholder, FOOTER);
			end write_placeholder;
			
			
		begin -- query_placeholders
			section_mark (section_placeholders, HEADER);

			layer := SILKSCREEN;
			face := TOP;
			device.placeholders.silkscreen.top.iterate (write_placeholder'access);

			face := BOTTOM;				
			device.placeholders.silkscreen.bottom.iterate (write_placeholder'access);

			layer := ASSY_DOC;
			face := TOP;				
			device.placeholders.assy_doc.top.iterate (write_placeholder'access);

			face := BOTTOM;
			device.placeholders.assy_doc.bottom.iterate (write_placeholder'access);
			
			section_mark (section_placeholders, FOOTER);				
		end query_placeholders;



		
		procedure write (d : in pac_devices_electrical.cursor) is 
			device : type_device_electrical renames element (d);
			-- CS use "device" instead of "element (d)"
			use et_device_appearance;
			use et_device_model_names;
			use et_device_purpose;
			use et_device_value;
			use et_device_partcode;
			use et_package_variant;
			use pac_package_variant_name;
		begin
			log (text => get_device_name (d), level => log_threshold + 1);
			
			section_mark (section_device, HEADER);
			write (keyword => keyword_name, parameters => to_string (key (d)));
			write (keyword => keyword_appearance, parameters => to_string (element (d).appearance));
			
			write (keyword => keyword_model, 
				parameters => get_device_model_name (device));

			case element (d).appearance is
				when APPEARANCE_PCB =>
					-- write the value if a value exists for the device:
					if not is_empty (element (d).value) then
						write (keyword => keyword_value, parameters => to_string (element (d).value));
					end if;
					
					write (keyword => keyword_variant , parameters => to_string (element (d).variant));

					-- write the partcode if a partcode exists for the device;
					if not is_empty (element (d).partcode) then
						write (keyword => keyword_partcode, parameters => to_string (element (d).partcode));
					end if;

					-- write the purpose if a purpose exists for the device;
					if not is_empty (element (d).purpose) then
						write (keyword => keyword_purpose , parameters => to_string (element (d).purpose), wrap => true);
					end if;
					
					section_mark (section_package, HEADER);

					-- This is the position of the package in the layout, 
					write (keyword => keyword_position, parameters => -- position x 34.5 y 60.1 face top/bottom
						et_board_coordinates.to_string (get_position (device), FORMAT_2));
				
					query_element (d, query_placeholders'access);
					section_mark (section_package, FOOTER);
					
				when APPEARANCE_VIRTUAL => null;
			end case;

			query_element (d, query_units'access);
			
			section_mark (section_device, FOOTER);
			new_line;
		end write;
	

		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is begin
			section_mark (section_devices, HEADER);
			iterate (module.devices, write'access);
			section_mark (section_devices, FOOTER);
		end query_module;

		

	begin
		log (text => "module " & to_string (module_cursor)
			 & " write electrical devices",
			 level => log_threshold);

		log_indentation_up;
		query_element (module_cursor, query_module'access);
		log_indentation_down;
	end write_devices_electrical;


	
end et_module_write_devices_electrical;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
