------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   MODULE WRITE / NON-ELECTRICAL DEVICE                   --
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

with et_keywords;					use et_keywords;
with et_section_headers;			use et_section_headers;

with et_pcb_sides;					use et_pcb_sides;
with et_board_coordinates;			use et_board_coordinates;

with et_coordinates_formatting;		use et_coordinates_formatting;
with et_device_name;				use et_device_name;

with et_device_purpose;
with et_device_value;
with et_device_partcode;

with et_devices_non_electrical;			use et_devices_non_electrical;
with et_package_model_name;

with et_device_placeholders.packages;

with et_general_rw;						use et_general_rw;
with et_board_write;					use et_board_write;



package body et_module_write_device_non_electrical is

	use pac_generic_modules;
	

	
	procedure write_devices_non_electrical (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		use pac_devices_non_electrical;

		
		procedure query_devices_non_electric (
			c : in pac_devices_non_electrical.cursor) 
		is
			device : type_device_non_electrical renames element (c);
			-- CS use "device" instead of "element (c)"

			use et_package_model_name;

			
			procedure query_placeholders (
				device_name : in type_device_name;
				device 		: in type_device_non_electrical) 
			is
				use et_device_placeholders;
				use et_device_placeholders.packages;
				use et_device_placeholders.packages.pac_text_placeholders;

				face : type_face;
				layer : type_placeholder_layer;

				
				procedure write_placeholder (
					c : in et_device_placeholders.packages.pac_text_placeholders.cursor)
				is 
					ph : et_device_placeholders.packages.type_text_placeholder renames element (c);
				begin
					section_mark (section_placeholder, HEADER);
					write (keyword => keyword_layer, parameters => to_string (layer));
					write (keyword => keyword_meaning, parameters => to_string (element (c).meaning));
					write (keyword => keyword_anchor, parameters => to_string (get_anchor_mode (ph)));
					
					write_text_properties_with_face (ph, face);

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

			
		begin
			log (text => get_device_name (c), level => log_threshold + 1);
			
			section_mark (section_device, HEADER);

			write (keyword => keyword_name, parameters => to_string (key (c))); -- name FD1
			write (keyword => keyword_position, parameters =>
				to_string (get_position (device), FORMAT_2));
			
			write (keyword => keyword_model, parameters => to_string (element (c).package_model));

			query_element (c, query_placeholders'access);
			
			section_mark (section_device, FOOTER);
		end query_devices_non_electric;


		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is begin
			section_mark (section_devices_non_electric, HEADER);
			iterate (module.devices_non_electric, query_devices_non_electric'access);
			section_mark (section_devices_non_electric, FOOTER);
		end query_module;

		

	begin
		log (text => "module " & to_string (module_cursor)
			 & " write non-electrical devices",
			 level => log_threshold);

		log_indentation_up;
		query_element (module_cursor, query_module'access);
		log_indentation_down;
	end write_devices_non_electrical;


	
end et_module_write_device_non_electrical;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
