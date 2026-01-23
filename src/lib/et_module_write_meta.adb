------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      MODULE WRITE / META DATA                             --
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

with et_module_names;				use et_module_names;
with et_file_sections;				use et_file_sections;
with et_keywords;					use et_keywords;

with et_meta_device_libraries_schematic;	use et_meta_device_libraries_schematic;
with et_meta_device_libraries_board;		use et_meta_device_libraries_board;

with et_meta;						use et_meta;

with et_time;						use et_time;

with et_file_write;					use et_file_write;


package body et_module_write_meta is

	use pac_generic_modules;



	procedure write_meta (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		meta : et_meta.type_meta := element (module_cursor).meta;

		
		procedure write_basic (basic : in type_meta_basic'class) is begin
			write (keyword => keyword_company, parameters => to_string (basic.company), wrap => true);
			write (keyword => keyword_customer, parameters => to_string (basic.customer), wrap => true);
			write (keyword => keyword_partcode, parameters => to_string (basic.partcode));
			write (keyword => keyword_drawing_number, parameters => to_string (basic.drawing_number));
			write (keyword => keyword_revision, parameters => to_string (basic.revision));
			
			write (keyword => keyword_drawn_by, parameters => to_string (basic.drawn_by), wrap => true);
			write (keyword => keyword_drawn_date, parameters => to_string_YMD (basic.drawn_date));
			
			write (keyword => keyword_checked_by, parameters => to_string (basic.checked_by), wrap => true);
			write (keyword => keyword_checked_date, parameters => to_string_YMD (basic.checked_date));

			write (keyword => keyword_approved_by, parameters => to_string (basic.approved_by), wrap => true);
			write (keyword => keyword_approved_date, parameters => to_string_YMD (basic.approved_date));
		end write_basic;


		
		procedure write_schematic (sch : in type_meta_schematic) is 
			use pac_library_paths_schematic;
			
			procedure query_lib (c : in pac_library_paths_schematic.cursor) is begin
				write (keyword => keyword_path, parameters => to_string (element (c)));
			end query_lib;
	
		begin -- write_schematic
			section_mark (section_preferred_libraries, HEADER);
			sch.preferred_libs.iterate (query_lib'access);
			section_mark (section_preferred_libraries, FOOTER);
		end write_schematic;


		
		procedure write_board (brd : in type_meta_board) is
			use pac_library_paths_board;
			
			procedure query_lib (c : in pac_library_paths_board.cursor) is begin
				write (keyword => keyword_path, parameters => to_string (element (c)));
			end query_lib;

		begin -- write_board
			section_mark (section_preferred_libraries, HEADER);
			brd.preferred_libs.iterate (query_lib'access);
			section_mark (section_preferred_libraries, FOOTER);
		end write_board;

		
		
	begin
		log_indentation_up;
		log (text => "meta data", level => log_threshold);
		
		section_mark (section_meta, HEADER);

		-- schematic related
		section_mark (section_schematic, HEADER);
		write_basic (meta.schematic);
		write_schematic (meta.schematic);
		
		section_mark (section_schematic, FOOTER);

		
		-- board related
		section_mark (section_board, HEADER);
		write_basic (meta.board);
		write_board (meta.board);
		section_mark (section_board, FOOTER);
		
		section_mark (section_meta, FOOTER);

		log_indentation_down;
	end write_meta;


		
end et_module_write_meta;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
