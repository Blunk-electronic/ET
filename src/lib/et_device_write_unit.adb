------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      DEVICE MODEL / WRITE UNIT                           --
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
--
--

with ada.text_io;				use ada.text_io;
with ada.strings; 				use ada.strings;

with ada.exceptions;

with et_coordinates_formatting;		use et_coordinates_formatting;
with et_schematic_geometry;			use et_schematic_geometry;
use et_schematic_geometry.pac_geometry_2;

with et_file_write;					use et_file_write;
with et_file_sections;				use et_file_sections;

with et_symbol_write;
with et_symbol_library;

with et_unit_name;					use et_unit_name;
with et_unit_swap_level;			use et_unit_swap_level;
with et_unit_add_level;				use et_unit_add_level;

with et_keywords;					use et_keywords;



package body et_device_write_unit is


	use pac_unit_name;
	

	
	procedure write_internal_units (
		units 			: in pac_units_internal.map;
		log_threshold	: in type_log_level)
	is
		use pac_units_internal;

		unit_internal_cursor : pac_units_internal.cursor := units.first;

		
		procedure query_internal_unit (
			name	: in pac_unit_name.bounded_string;
			unit	: in type_unit_internal) 
		is
			use et_symbol_write;
		begin
			write (keyword => keyword_name, parameters => to_string (name));
			write (keyword => keyword_position, parameters => to_string (unit.position, FORMAT_2));
			write (keyword => keyword_swap_level, parameters => to_string (unit.swap_level));
			write (keyword => keyword_add_level , parameters => to_string (unit.add_level));
			section_mark (section_symbol, HEADER);
			save_symbol_1 (unit.symbol, log_threshold + 1);
			section_mark (section_symbol, FOOTER);
		end query_internal_unit;
		
		
	begin
		section_mark (section_units_internal, HEADER);
		
		while unit_internal_cursor /= pac_units_internal.no_element loop
			section_mark (section_unit, HEADER);
			query_element (unit_internal_cursor, query_internal_unit'access);
			section_mark (section_unit, FOOTER);
			next (unit_internal_cursor);
		end loop;
		
		section_mark (section_units_internal, FOOTER);
	end write_internal_units;

	




	

	
		
	procedure write_external_units (
		units 			: in pac_units_external.map;
		log_threshold	: in type_log_level)
	is
		use pac_units_external;

		unit_external_cursor : pac_units_external.cursor := units.first;
		
		
		procedure query_external_unit (
			name	: in pac_unit_name.bounded_string;
			unit	: in type_unit_external) 
		is
			use et_symbol_library;
		begin
			write (keyword => keyword_name, parameters => to_string (name));
			write (keyword => keyword_position, parameters => to_string (unit.position, FORMAT_2));
			write (keyword => keyword_swap_level, parameters => to_string (unit.swap_level));
			write (keyword => keyword_add_level , parameters => to_string (unit.add_level));
			
			-- write (keyword => keyword_symbol_file, parameters => to_string (unit.model));

			write (keyword => keyword_symbol_file, 
				   parameters => get_symbol_model_name (unit.model_cursor));

			
		end query_external_unit;
		
		
	begin
		section_mark (section_units_external, HEADER);

		while unit_external_cursor /= pac_units_external.no_element loop
			section_mark (section_unit, HEADER);
			query_element (unit_external_cursor, query_external_unit'access);
			section_mark (section_unit, FOOTER);
			next (unit_external_cursor);
		end loop;
		
		section_mark (section_units_external, FOOTER);
	end write_external_units;

	

		
end et_device_write_unit;
