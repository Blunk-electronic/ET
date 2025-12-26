------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE READ / DRAWING FRAMES                          --
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
with et_module_instance;
with et_keywords;					use et_keywords;

with et_drawing_frame;				use et_drawing_frame;
with et_drawing_frame.schematic;	use et_drawing_frame.schematic;
with et_drawing_frame_rw;			use et_drawing_frame_rw;

with et_text_content;				use et_text_content;

with et_schematic_coordinates;
with et_sheets;						use et_sheets;


with et_general_rw;					use et_general_rw;



package body et_module_read_frames is

	use pac_generic_modules;

	
	
	sheet_descriptions			: pac_schematic_descriptions.map;
	sheet_description_category	: type_schematic_sheet_category := 
		schematic_sheet_category_default; -- product/develpment/routing
	
	sheet_description_number	: type_sheet := type_sheet'first; -- 1, 2. 3, ...
	sheet_description_text		: pac_text_content.bounded_string;		-- "voltage regulator"

	-- CS frame_count_schematic		: et_schematic_coordinates.type_submodule_sheet_number := et_schematic_coordinates.type_submodule_sheet_number'first; -- 10 frames
	frame_template_schematic	: pac_template_name.bounded_string;	-- $ET_FRAMES/drawing_frame_version_1.frs
	frame_template_board		: pac_template_name.bounded_string;	-- $ET_FRAMES/drawing_frame_version_2.frb
	frame_board_position		: type_position; -- x 0 y 0

	
	

	procedure read_frame_template_schematic (
		line : in type_fields_of_line)
	is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_template then -- template $ET_FRAMES/drawing_frame_version_1.frs
			expect_field_count (line, 2);
			frame_template_schematic := to_template_name (f (line, 2));
		else
			invalid_keyword (kw);
		end if;
	end;

		

		
	procedure read_frame_template_board (
		line : in type_fields_of_line)
	is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_template then -- template $ET_FRAMES/drawing_frame_version_2.frb
			expect_field_count (line, 2);
			frame_template_board := to_template_name (f (line, 2));

		elsif kw = keyword_position then -- position x 40 y 60
			expect_field_count (line, 5);
			frame_board_position := et_drawing_frame_rw.to_position (line, 2);
		else
			invalid_keyword (kw);
		end if;
	end;

	
		
		

	procedure read_sheet_description (
		line : in type_fields_of_line)
	is
		use et_schematic_coordinates;	
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_sheet_number then -- number 2
			expect_field_count (line, 2);
			sheet_description_number := to_sheet (f (line, 2));

		elsif kw = keyword_sheet_category then -- category develompent/product/routing
			expect_field_count (line, 2);
			sheet_description_category := to_category (f (line, 2));

		elsif kw = keyword_sheet_description then -- text "voltage regulator"
			expect_field_count (line, 2);
			sheet_description_text := to_content (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;
	end read_sheet_description;

		

		
		
		
	procedure add_sheet_description is 
		use et_schematic_coordinates;	
		use pac_schematic_descriptions;
		inserted : boolean;
		position : pac_schematic_descriptions.cursor;
	begin
		insert (
			container	=> sheet_descriptions,
			key			=> sheet_description_number,
			inserted	=> inserted,
			position	=> position,
			new_item	=> (sheet_description_text, sheet_description_category)
			);

		-- clean up for next sheet description
		sheet_description_category := schematic_sheet_category_default;
		sheet_description_number := type_sheet'first;
		sheet_description_text := to_content("");
	end add_sheet_description;

				
				
		
		
		
		
		
	procedure set_frame_schematic (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
	

		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			log (text => "drawing frame schematic " & to_string (frame_template_schematic), 
				level => log_threshold + 1);

			-- set the frame template name
			module.frames.template := frame_template_schematic;

			-- assign the sheet descriptions:
			module.frames.descriptions := sheet_descriptions;

			-- Clean up sheet descriptions even if
			-- there should not be another section for sheet descriptions:
			pac_schematic_descriptions.clear (sheet_descriptions);
			
			-- read the frame template file
			module.frames.frame := read_frame_schematic (
				file_name		=> frame_template_schematic,
				log_threshold	=> log_threshold + 2);

		end do_it;
					

	begin
		-- set schematic frame template
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);
		
	end set_frame_schematic;

	
	
	
	
	
	
	
	
	procedure set_frame_board  (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			log (text => "drawing frame board " & to_string (frame_template_board), level => log_threshold + 1);

			-- set the frame template name
			module.board.frame.template := frame_template_board;

			-- read the frame template file
			module.board.frame.frame := read_frame_board (
				file_name		=> frame_template_board,
				log_threshold	=> log_threshold + 2);

			-- Set the frame position:
			module.board.frame.frame.position := frame_board_position;
		end do_it;

		
	begin
		-- set board/layout frame template
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

	end set_frame_board;


	
	
	
end et_module_read_frames;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
