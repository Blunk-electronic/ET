------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         SYMBOL READ / TEXT                               --
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
-- To Do:
-- - clean up
--
--


with ada.text_io;					use ada.text_io;
with ada.exceptions;

with et_schematic_geometry;			use et_schematic_geometry;

with et_text_content;				use et_text_content;
with et_alignment;					use et_alignment;
with et_schematic_text;				use et_schematic_text;
with et_symbol_text;				use et_symbol_text;
with et_device_placeholders;		use et_device_placeholders;

with et_keywords;					use et_keywords;


package body et_symbol_read_text is

	use pac_geometry_2;
	use pac_text_schematic;


	symbol_text_base			: type_text_basic;
	symbol_text_position		: type_vector_model;
	symbol_text_content			: pac_text_content.bounded_string;
	symbol_placeholder_meaning	: type_placeholder_meaning := placeholder_meaning_default;


	

	procedure read_text (
		line : in type_fields_of_line)
	is
		kw : string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_position then -- position x 1 y 2
			expect_field_count (line, 5);

			-- extract the text position starting at field 2
			symbol_text_position := to_vector_model (line,2);

		elsif kw = keyword_content then -- content "dummy NAND gate"
			expect_field_count (line, 2);
			symbol_text_content := to_content (f (line, 2));

		elsif kw = keyword_size then -- size 5
			expect_field_count (line, 2);
			symbol_text_base.size := to_distance (f (line, 2));

		elsif kw = keyword_rotation then -- rotation 90.0
			expect_field_count (line, 2);
			symbol_text_base.rotation := pac_text_schematic.to_rotation_doc (f (line, 2));
			
-- 		elsif kw = keyword_style then -- style italic
-- 			expect_field_count (line, 2);
-- 			symbol_text_base.style := to_text_style (f (line, 2));

		elsif kw = keyword_alignment then -- alignment horizontal center vertical center
			expect_field_count (line, 5);
			symbol_text_base.alignment := to_alignment (line, 2);

		else
			invalid_keyword (kw);
		end if;
	end read_text;
	
				
		
		
		
	procedure insert_text (
		symbol			: in type_symbol_model_access;
		log_threshold	: in type_log_level)
	is begin
		log (text => "insert text", level => log_threshold);
		log_indentation_up;


		-- append symbol text to symbol
		pac_symbol_texts.append (
			container	=> symbol.texts,
			new_item	=> (symbol_text_base with
				content		=> symbol_text_content,
				position	=> symbol_text_position));

		
		-- clean up for next symbol text
		symbol_text_base := (others => <>);
		symbol_text_content := to_content ("");
		symbol_text_position := origin;
		
		log_indentation_down;
	end insert_text;
	










	procedure read_placeholder (
		line : in type_fields_of_line)
	is
		kw : string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_position then -- position x 1 y 2
			expect_field_count (line, 5);

			-- extract the placeholder position starting at field 2
			symbol_text_position := to_vector_model (line, 2);

		elsif kw = keyword_meaning then -- meaning reference
			expect_field_count (line, 2);
			symbol_placeholder_meaning := to_meaning (f (line, 2));

		elsif kw = keyword_size then -- size 5
			expect_field_count (line, 2);
			symbol_text_base.size := to_distance (f (line, 2));

		elsif kw = keyword_rotation then -- rotation 90.0
			expect_field_count (line, 2);
			symbol_text_base.rotation := pac_text_schematic.to_rotation_doc (f (line, 2));

		elsif kw = keyword_alignment then -- alignment horizontal center vertical center
			expect_field_count (line, 5);
			symbol_text_base.alignment := to_alignment (line, 2);

		else
			invalid_keyword (kw);
		end if;
	end read_placeholder;


	
	

	procedure insert_placeholder (
		symbol			: in type_symbol_model_access;
		log_threshold	: in type_log_level)
	is begin
		log (text => "insert text placeholder", level => log_threshold);
		log_indentation_up;

		-- Assign symbol the text placeholder to the symbol.
		-- The meaning of the placeholder determines where
		-- the placeholder is to be assigned. 
		-- If meaning is not specified in section PLACEHOLDER,
		-- the default meaning is assumed which raise an error.

		-- CS: warn if placeholder exists multiple times. The latest
		-- placeholder would overwrite the previous one.

		case symbol_placeholder_meaning is
			when NAME =>
				symbol.placeholders.name := (symbol_text_base with 
					position	=> symbol_text_position,
					meaning		=> symbol_placeholder_meaning);

			when VALUE =>
				symbol.placeholders.value := (symbol_text_base with 
					position	=> symbol_text_position,
					meaning		=> symbol_placeholder_meaning);

			when PURPOSE =>
				symbol.placeholders.purpose := (symbol_text_base with 
					position	=> symbol_text_position,
					meaning		=> symbol_placeholder_meaning);

			-- Default meaning causes an error:
			when others => 
				log (ERROR, "meaning of placeholder not specified !");
				raise constraint_error;
		end case;

		
		-- clean up for next symbol text placeholder
		symbol_text_base := (others => <>);
		symbol_text_position := origin;
		symbol_placeholder_meaning := placeholder_meaning_default;
		
		log_indentation_down;
	end insert_placeholder;

	
	
end et_symbol_read_text;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
