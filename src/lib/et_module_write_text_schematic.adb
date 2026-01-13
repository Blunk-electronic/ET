------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                     MODULE WRITE / TEXT IN SCHEMATIC                     --
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
with et_keywords;					use et_keywords;
with et_section_headers;			use et_section_headers;
with et_schematic_geometry;			use et_schematic_geometry;
with et_schematic_coordinates;		use et_schematic_coordinates;
with et_sheets;						use et_sheets;

with et_coordinates_formatting;		use et_coordinates_formatting;
with et_text_content;				use et_text_content;
with et_schematic_text;				use et_schematic_text;

with et_alignment;					use et_alignment;

with et_general_rw;					use et_general_rw;
with et_file_write;					use et_file_write;


package body et_module_write_text_schematic is

	use pac_generic_modules;
	use pac_geometry_2;

	
	
	
	procedure write_schematic_texts (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		use pac_text_schematic;
		use pac_texts;
		
		
		procedure write (text_cursor : in pac_texts.cursor) is 
		begin
			section_mark (section_text, HEADER);
			write
				(
				keyword		=> keyword_position,
				parameters	=> keyword_sheet & to_string (element (text_cursor).sheet) 
								& space & to_string (element (text_cursor).position, FORMAT_2)
				); -- position sheet 1 x 30 y 180
			
			write (keyword => keyword_rotation, 
					parameters => to_string (to_rotation (element (text_cursor).rotation)));
			
			write (keyword => keyword_content, wrap => true,
					parameters => to_string (element (text_cursor).content));
			
			write (keyword => keyword_size, parameters => to_string (element (text_cursor).size));
			write (keyword => keyword_alignment, parameters =>
				keyword_horizontal & space & to_string (element (text_cursor).alignment.horizontal)
				& space & keyword_vertical & space
				& to_string (element (text_cursor).alignment.vertical));

			-- CS font
			
			section_mark (section_text, FOOTER);
		end write;


		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is begin
			section_mark (section_texts, HEADER);
			iterate (module.texts, write'access);
			section_mark (section_texts, FOOTER);
		end query_module;

	
	
	begin
		log (text => "module " & to_string (module_cursor)
			& " write schematic texts",
			level => log_threshold);
			
		log_indentation_up;		
		query_element (module_cursor, query_module'access);						
		log_indentation_down;
	end write_schematic_texts;	
	

	
end et_module_write_text_schematic;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
