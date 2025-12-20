------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      MODULE READ / TEXT IN SCHEMATIC                     --
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
with ada.characters;				use ada.characters;
with ada.strings;					use ada.strings;

with et_module_names;				use et_module_names;
with et_keywords;					use et_keywords;

with et_schematic_geometry;			use et_schematic_geometry;
with et_schematic_coordinates;		use et_schematic_coordinates;
with et_text;						use et_text;
with et_schematic_text;				use et_schematic_text;

with et_alignment;					use et_alignment;

with et_general_rw;					use et_general_rw;



package body et_module_read_text_schematic is

	use pac_generic_modules;
	use pac_geometry_2;


	schematic_text : et_schematic_text.type_text;
	
	
	
	procedure read_schematic_text (
		line : in type_fields_of_line)
	is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_position then -- position sheet 2 x 91.44 y 118.56
			expect_field_count (line, 7);

			declare
				-- extract position of schematic_text starting at field 2
				pos : constant type_object_position := 
					to_position (line, 2);
			begin
				schematic_text.position := pos.place;
				schematic_text.sheet := get_sheet (pos);
			end;

		elsif kw = keyword_content then -- content "DUMMY TEXT IN CORE MODULE"
			expect_field_count (line, 2); -- actual content in quotes !
			schematic_text.content := et_text.to_content (f (line, 2));

		elsif kw = keyword_size then -- size 1.4
			expect_field_count (line, 2);
			schematic_text.size := to_distance (f (line, 2));

		elsif kw = keyword_rotation then -- rotation 90
			expect_field_count (line, 2);
			schematic_text.rotation := pac_text_schematic.to_rotation_doc (f (line, 2));

-- 			elsif kw = keyword_style then -- style normal/italic
-- 				expect_field_count (line, 2);
			-- schematic_text.font := et_symbol_model.to_text_style (f (line, 2)); -- CS
			-- CS: currently font and style are ignored.

		elsif kw = keyword_alignment then -- alignment horizontal center vertical center
			expect_field_count (line, 5);

			-- extract alignment starting at field 2
			schematic_text.alignment := to_alignment (line, 2);
			
		else
			invalid_keyword (kw);
		end if;
	end read_schematic_text;

	
	
	
	
	
	
	procedure insert_schematic_text (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
	
	
		procedure insert_schematic_text (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			-- append schematic note to collection of notes
			pac_texts.append (module.texts, schematic_text);
		end insert_schematic_text;

	
	
	begin
		log (text => "module " & to_string (module_cursor)
			& " insert_schematic_text",
			level => log_threshold);
			
		log_indentation_up;		

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> insert_schematic_text'access);

			
		schematic_text := (others => <>);
						
		log_indentation_down;
	end insert_schematic_text;
	
	
	
				
end et_module_read_text_schematic;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
