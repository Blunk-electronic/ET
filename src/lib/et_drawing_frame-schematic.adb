------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      DRAWING FRAME SCHEMATIC                             --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--



package body et_drawing_frame.schematic is

	
	
	procedure apply_defaults_schematic (frame : in out type_frame_schematic) is

		-- LINES OF TITLE BLOCK
		type type_lines is array (positive range <>) of type_line;

		lines_sch : constant type_lines (1 .. 10) := (
			-- outer lines
			((  0,  0),(220,  0)),
			((220,  0),(220, 40)),
			((220, 40),(  0, 40)),
			((  0, 40),(  0,  0)),

			-- inner lines
			(( 89,  0),( 89, 40)), -- vertical
			((119, 20),(119,  0)), -- vertical
			((150, 20),(150,  0)), -- vertical
			(( 89, 15),(220, 15)), -- horizontal
			(( 89, 20),(220, 20)), -- horizontal
			(( 89, 25),(220, 25))  -- horizontal
			);
		

		-- Collects the lines of the given array and returns them as a list:
		function make_lines (lines : in type_lines) return pac_lines.list is
			use pac_lines;
			result : pac_lines.list;
		begin
			for i in lines'first .. lines'last loop
				result.append (lines (i));
			end loop;
			return result;
		end make_lines;
		----------------------------------

		-- TEXTS IN TITLE BLOCK
		type type_texts is array (positive range <>) of type_static_text;

		texts_sch : constant type_texts (1 .. 14) := (
			(position => (  2, 36), size => 3, content => to_content ("Company:")),
			(position => (  2, 31), size => 3, content => to_content ("Project:")),
			(position => (  2, 26), size => 3, content => to_content ("Module:")),
			(position => (  2, 21), size => 3, content => to_content ("Variant:")),
			(position => (  2, 16), size => 3, content => to_content ("Customer:")),
			(position => (  2, 11), size => 3, content => to_content ("Partcode:")),
			(position => (  2,  6), size => 3, content => to_content ("Drwg. No:")),
			(position => (  2,  1), size => 3, content => to_content ("Revision:")),

			--(position => (120, 16), size => 3, content => to_content ("date")),
			--(position => (152, 16), size => 3, content => to_content ("name")),
			(position => ( 90, 11), size => 3, content => to_content ("edited:")),
			(position => ( 90,  6), size => 3, content => to_content ("checked:")),
			(position => ( 90,  1), size => 3, content => to_content ("approved")),

			(position => ( 90, 35), size => 3, content => to_content ("SHEET DESCRIPTION:")),
			(position => (192, 21), size => 3, content => to_content ("SHEET")),
			(position => ( 90, 21), size => 3, content => to_content ("CAT:"))
			);

		
		-- Collects the texts of the given array and returns them as a list:
		function make_texts (texts : in type_texts) return pac_static_texts.list is
			use pac_static_texts;
			result : pac_static_texts.list;
		begin
			for i in texts'first .. texts'last loop
				result.append (texts (i));
			end loop;
			return result;
		end make_texts;
		
		
	begin -- apply_defaults_schematic
		-- type_title_bock (basic stuff):
		frame.title_block_schematic.position										:= ( 55,  6);
		frame.title_block_schematic.lines := make_lines (lines_sch);
		frame.title_block_schematic.placeholders_common.project_name.position 				:= ( 30, 31);
		frame.title_block_schematic.placeholders_common.module_file_name.position 			:= ( 30, 26);
		frame.title_block_schematic.placeholders_common.active_assembly_variant.position	:= ( 30, 21);
		frame.title_block_schematic.static_texts := make_texts (texts_sch);

		frame.title_block_schematic.placeholders_additional.company.position 		:= ( 30, 36);
		frame.title_block_schematic.placeholders_additional.customer.position 		:= ( 30, 16);
		frame.title_block_schematic.placeholders_additional.partcode.position 		:= ( 30, 11);
		frame.title_block_schematic.placeholders_additional.drawing_number.position	:= ( 30,  6);
		frame.title_block_schematic.placeholders_additional.revision.position 		:= ( 30,  1);

		frame.title_block_schematic.placeholders_additional.drawn_by.position 		:= (152, 11);
		frame.title_block_schematic.placeholders_additional.checked_by.position 	:= (152,  6);
		frame.title_block_schematic.placeholders_additional.approved_by.position 	:= (152,  1);

		frame.title_block_schematic.placeholders_additional.drawn_date.position 	:= (120, 11);
		frame.title_block_schematic.placeholders_additional.checked_date.position 	:= (120,  6);
		frame.title_block_schematic.placeholders_additional.approved_date.position 	:= (120,  1);
	
		frame.title_block_schematic.placeholders_additional.sheet_number.position 		:= (210, 21);
		frame.title_block_schematic.placeholders_additional.sheet_description.position 	:= ( 90, 30);
		frame.title_block_schematic.placeholders_additional.sheet_category.position 	:= (105, 21);

	end apply_defaults_schematic;

	





	function make_default_frame_schematic
		return type_frame_schematic
	is
		f : type_frame_schematic;
	begin
		apply_defaults_schematic (f);
		return f;
	end make_default_frame_schematic;


	
	
	function to_string (cat : in type_schematic_sheet_category) return string is begin
		return type_schematic_sheet_category'image (cat);
	end;


	
	function to_category (cat : in string) return type_schematic_sheet_category is begin
		return type_schematic_sheet_category'value (cat);
	end;


	
	
	function get_sheet_count (
		descriptions : in pac_schematic_descriptions.vector)
		return type_sheet
	is begin
		return type_sheet (descriptions.length);
	end;


	
	
	function sheet_exists (
		frames	: in type_frames_schematic;
		sheet	: in type_sheet)
		return boolean
	is
		use pac_schematic_descriptions;
		cursor : pac_schematic_descriptions.cursor;
	begin
		-- Locate the given sheet among the frames:
		cursor := frames.descriptions.to_cursor (sheet);

		return has_element (cursor);
	end;
	

	
	
	function get_sheet (
		sheet_cursor : in pac_schematic_descriptions.cursor)
		return string
	is 
		sheet : type_sheet;
		use pac_schematic_descriptions;
	begin
		sheet := to_index (sheet_cursor);
		return to_string (sheet);
	end;

	

	
	function get_category (
		sheet_cursor : in pac_schematic_descriptions.cursor)
		return string
	is
		use pac_schematic_descriptions;
		category : type_schematic_sheet_category;
	begin
		category := element (sheet_cursor).category;
		return to_string (category);
	end;
	
	

	
	function get_content (
		sheet_cursor : in pac_schematic_descriptions.cursor)
		return string
	is
		use pac_schematic_descriptions;
		content : pac_text_content.bounded_string;
	begin
		content := element (sheet_cursor).content;
		return to_string (content);
	end;

	
	
	
	
	
	function get_sheet_count (
		frames	: in type_frames_schematic)
		return type_sheet
	is
		use pac_schematic_descriptions;
	begin
		return type_sheet (frames.descriptions.length);
	end;
		
		
	
	
	
	procedure delete_sheet (
		frames	: in out type_frames_schematic;
		sheet	: in type_sheet) -- CS rename to target
	is 
		use pac_schematic_descriptions;
		cursor : pac_schematic_descriptions.cursor;		
		
		total : type_sheet := get_sheet_count (frames);
		
		
		-- procedure delete_last_sheet is 
		-- begin
		-- 	-- Locate the given sheet among the frames:
		-- 	cursor := frames.descriptions.find (sheet);
  -- 
		-- 	-- Delete the sheet:
		-- 	frames.descriptions.delete (cursor);		
		-- end;
		
		
-- 		procedure delete_other_sheet is
-- 			lower_end : pac_schematic_descriptions.vector;
-- 			upper_end : pac_schematic_descriptions.vector;
-- 			
-- 			
-- 			procedure get_lower_end is 
-- 				cursor : pac_schematic_descriptions.cursor;
-- 				place : type_sheet := 1;
-- 			begin
-- 				cursor := frames.descriptions.first;
-- 			
-- 				while has_element (cursor) loop
-- 					lower_end.insert (element (cursor));
-- 					place := place + 1;
-- 					if place = sheet - 1 then
-- 						exit;
-- 					end if;
-- 					next (cursor);
-- 				end loop;
-- 			end get_lower_end;
-- 			
-- 			
-- 			procedure get_upper_end is
-- 				cursor : pac_schematic_descriptions.cursor;
-- 				place : type_sheet := sheet + 1;
-- 			begin
-- 				cursor := frames.descriptions.find (sheet + 1);
-- 				
-- 				while has_element (cursor) loop
-- 					upper_end.insert (element (cursor));
-- 					next (cursor);
-- 				end loop;
-- 			end get_upper_end;
-- 						
-- 
-- 			procedure append is
-- 				cursor : pac_schematic_descriptions.cursor;
-- 			begin
-- 				null;
-- 			end append;
							
		-- begin
-- 			get_lower_end;
-- 			get_upper_end;
-- 			
-- 			append;
		-- end delete_other_sheet;
		
		
	begin
		if sheet > total then
			-- The given sheet number is greater
			-- than the total number of sheets:
			null; -- CS raise exception ? or just do nothing ?
			
		elsif sheet = total then
			-- The last sheet is to be deleted:
			-- delete_last_sheet;
			null;
		else
			-- Delete a sheet that is not 
			-- the last sheet:
			-- delete_other_sheet;
			null;
		end if;

	end delete_sheet;

	
	
		
	
	
	procedure set_category (
		frames	: in out type_frames_schematic;
		sheet	: in type_sheet;
		cat		: in type_schematic_sheet_category)
	is
		use pac_schematic_descriptions;

		
		-- Assigns the given category to the sheet:		
		procedure query_sheet (
			description	: in out type_schematic_description)
		is begin
			description.category := cat;
		end;
		
		
	begin
		frames.descriptions.update_element (
			sheet, query_sheet'access);
			
	end set_category;


	
	
end et_drawing_frame.schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
