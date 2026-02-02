------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         SYMBOL WRITE / TEXT                              --
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

with ada.text_io;				use ada.text_io;
with ada.strings; 				use ada.strings;

with ada.exceptions;

with et_coordinates_formatting;		use et_coordinates_formatting;
with et_schematic_geometry;			use et_schematic_geometry;

with et_text_content;				use et_text_content;
with et_alignment;					use et_alignment;
with et_schematic_text;				use et_schematic_text;
with et_symbol_text;				use et_symbol_text;
with et_device_placeholders;		use et_device_placeholders;

with et_file_sections;				use et_file_sections;
with et_file_write;					use et_file_write;

with et_keywords;					use et_keywords;

with et_device_appearance;			use et_device_appearance;



package body et_symbol_write_text is

	use pac_text_schematic;
	use pac_geometry_2;


	
	procedure write_text_properties (t : in type_text_basic'class) is
	begin
		write (keyword => keyword_size, parameters => to_string (t.size));
		write (keyword => keyword_rotation, parameters => to_string (t.rotation));
-- 		write (keyword => keyword_style, parameters => to_string (t.style));
		write (keyword => keyword_alignment, parameters =>
				keyword_horizontal & space & to_string (t.alignment.horizontal) & space &
				keyword_vertical   & space & to_string (t.alignment.vertical)
				);
	end write_text_properties;


	
	

	procedure write_texts (
		symbol			: in type_symbol_model;
		log_threshold	: in type_log_level)
	is
		use pac_symbol_texts;

		procedure write_text (
			cursor : in pac_symbol_texts.cursor)
		is begin
			section_mark (section_text, HEADER);
			write (keyword => keyword_position, parameters => to_string (element (cursor).position, FORMAT_2));
			write (keyword => keyword_content , parameters => to_string (element (cursor).content));			
			write_text_properties (element (cursor));
			section_mark (section_text, FOOTER);
		end write_text;

		
	begin
		log (text => "write texts", level => log_threshold);
		log_indentation_up;

		section_mark (section_texts, HEADER);

		iterate (symbol.texts, write_text'access);

		section_mark (section_texts, FOOTER);
		
		log_indentation_down;
	end write_texts;
	
		

		
		


		
	procedure write_placeholders (
		symbol			: in type_symbol_model;
		log_threshold	: in type_log_level)
	is 
	
		procedure do_it is begin
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
		end do_it;
	
	
	begin
		case symbol.appearance is
			when APPEARANCE_PCB =>
				log (text => "write text placeholders", level => log_threshold);
				log_indentation_up;
				do_it;				
				log_indentation_down;
				
			when others => null;
		end case;		
	end write_placeholders;

		
		
	
end et_symbol_write_text;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
