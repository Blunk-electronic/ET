------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE WRITE / DRAWING FRAMES                         --
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

with et_drawing_frame;				use et_drawing_frame;
with et_drawing_frame.schematic;
with et_drawing_frame.board;
with et_text_content;				use et_text_content;
with et_sheets;						use et_sheets;

with et_general_rw;					use et_general_rw;
with et_drawing_frame_rw;			use et_drawing_frame_rw;



package body et_module_write_frames is

	use pac_generic_modules;

	
		
	
	procedure write_frames  (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is 

			-- This procedure writes the stuff related to the
			-- drawing frames of the schematic:
			procedure schematic is
				use et_drawing_frame.schematic;
				
			
				procedure write_sheet_descriptions is
					use pac_schematic_descriptions;

					
					procedure query_sheet (
						s : in pac_schematic_descriptions.cursor) 
					is
						use et_sheets;
					begin
						section_mark (section_sheet, HEADER);
						write (
							keyword		=> keyword_sheet_number,
							parameters	=> to_string (key (s)));

						write (
							keyword		=> keyword_sheet_category,
							parameters	=> to_string (element (s).category));

						write (
							keyword		=> keyword_sheet_description,
							wrap		=> true,
							parameters	=> to_string (element (s).content));
						
						section_mark (section_sheet, FOOTER);
					end query_sheet;

					
				begin
					section_mark (section_sheet_descriptions, HEADER);
					iterate (module.frames.descriptions, query_sheet'access);
					section_mark (section_sheet_descriptions, FOOTER);
				end write_sheet_descriptions;

				
			begin
				section_mark (section_schematic, HEADER);
				
				-- Write the schematic frame template 
				-- like "template ../frames/dummy.frs":
				write (
					keyword 	=> keyword_template, 
					parameters	=> to_string (module.frames.template));
				
				write_sheet_descriptions;

				section_mark (section_schematic, FOOTER);
			end schematic;
			

			

			-- This procedure writes the stuff related to the
			-- drawing frame of the board:
			procedure board is
				use et_drawing_frame.board;
				frame_pos : et_drawing_frame.type_position;
			begin
				section_mark (section_board, HEADER);
				
				-- Write the frame template like "template ../frames/dummy.frb":
				write (
					keyword		=> keyword_template, 
					parameters	=> to_string (module.board.frame.template));

				
				-- Write the frame position like "position x 40 y 60"
				frame_pos := get_position (module.board.frame.frame); 
				
				write (
					keyword		=> keyword_position,
					parameters	=> to_string (frame_pos, FORMAT_2));

				section_mark (section_board, FOOTER);
			end board;

			
		begin
			section_mark (section_drawing_frames, HEADER);
			schematic;
			board;
			section_mark (section_drawing_frames, FOOTER);
		end query_module;

		
		
	begin
		log (text => "module " & to_string (module_cursor),
			 level => log_threshold);

		log_indentation_up;

		query_element (module_cursor, query_module'access);

		log_indentation_down;
	end write_frames;


	
	
	
end et_module_write_frames;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
