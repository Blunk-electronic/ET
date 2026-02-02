------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            SYMBOL WRITE                                  --
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

with ada.text_io;				use ada.text_io;
with ada.strings; 				use ada.strings;

with ada.exceptions;

with et_system_info;
with et_directory_and_file_ops;
with et_file_sections;				use et_file_sections;
with et_file_write;					use et_file_write;
with et_symbol_library;				use et_symbol_library;

with et_time;						use et_time;
with et_keywords;					use et_keywords;
with et_device_appearance;			use et_device_appearance;

with et_symbol_write_text;			use et_symbol_write_text;
with et_symbol_write_body;			use et_symbol_write_body;
with et_symbol_write_ports;			use et_symbol_write_ports;


package body et_symbol_write is


	
	
	procedure save_symbol (
		file_name		: in pac_symbol_model_name.bounded_string; -- libraries/symbols/nand.sym
		symbol			: in type_symbol_model; -- the actual symbol model
		log_threshold	: in type_log_level)
	is
		use et_system_info;
		file_handle : ada.text_io.file_type;


		procedure do_write is
		begin
			-- appearance
			write (keyword => keyword_appearance, 
				parameters => to_string (symbol.appearance));
			
			-- body section begin
			section_mark (section_draw, HEADER);

			-- CS sort the following actions by their importance			
			write_body_lines (symbol, log_threshold + 1);
			write_body_arcs (symbol, log_threshold + 1);
			write_body_circles (symbol, log_threshold + 1);

			-- body section end
			section_mark (section_draw, FOOTER);
			
			
			write_texts (symbol, log_threshold + 1);
			write_placeholders (symbol, log_threshold + 1);
			write_ports (symbol, log_threshold + 1);
		end do_write;

		
	begin
		log (text => to_string (file_name), level => log_threshold);

		create (
			file 	=> file_handle,
			mode	=> out_file,
			name	=> to_string (file_name));

		set_output (file_handle);
		
		-- write a nice header
		put_line (comment_mark_default & " " & system_name & " symbol");
		put_line (comment_mark_default & " " & get_date);
		put_line (comment_mark_default & " " & row_separator_double);
		new_line;

		-- CS: ? reset_tab_depth;
		
		do_write;

		-- write footer
		new_line;		
		put_line (comment_mark_default & " " & row_separator_double);
		put_line (comment_mark_default & " symbol file end");
		new_line;
		
		reset_tab_depth;		
		
		set_output (standard_output);
		close (file_handle);

		exception when event: others =>
			log (text => ada.exceptions.exception_message (event));
			if is_open (file_handle) then
				close (file_handle);
			end if;
			raise;
		
	end save_symbol;
	
	
end et_symbol_write;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
