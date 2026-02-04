------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   DEVICE MODEL / WRITE PACKAGE VARIANT                   --
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
-- 
-- To Do:
-- - clean up
-- - use renames
--

with ada.text_io;					use ada.text_io;
with ada.strings; 					use ada.strings;
with ada.exceptions;

with et_package_library;
with et_package_variant_name;		use et_package_variant_name;
with et_package_variant;			use et_package_variant;
with et_package_variant_terminal_port_map;

with et_unit_name;
with et_port_names;
with et_terminal_name;
with et_package_variant_terminal_port_map;

with et_keywords;					use et_keywords;
with et_file_sections;				use et_file_sections;
with et_file_write;					use et_file_write;





package body et_device_write_package_variant is


	procedure write_package_variant (
		variants		: in pac_package_variants.map;
		log_threshold	: in type_log_level)
	is
		use pac_package_variant_name;
		
		use pac_package_variants;
		variant_cursor : pac_package_variants.cursor := variants.first;



		procedure write_variant (
			packge	: in pac_package_variant_name.bounded_string;
			variant	: in type_package_variant) 
		is
			use et_package_library;
			use et_port_names;

			use et_package_variant_terminal_port_map;
			use pac_terminal_port_map;	
			
			
			procedure write_terminal (
				terminal_cursor : in pac_terminal_port_map.cursor) 
			is 
				use et_unit_name;
				use pac_unit_name;
				use et_terminal_name;
			begin
				write (keyword => keyword_terminal, parameters => 
					space & to_string (key (terminal_cursor)) & space -- terminal name like G14 or 16
					-- CS use a function that returns the terminal name as string
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


		
	begin
		-- CS log messages
		
		section_mark (section_variants, HEADER);

		
		while variant_cursor /= pac_package_variants.no_element loop
			section_mark (section_variant, HEADER);
			
			write (keyword => keyword_name, 
				parameters => to_string (key (variant_cursor)));

			query_element (
				position	=> variant_cursor,
				process		=> write_variant'access);

			section_mark (section_variant, FOOTER);
			
			next (variant_cursor);
		end loop;

		
		section_mark (section_variants, FOOTER);
	end write_package_variant;

	
		
end et_device_write_package_variant;
