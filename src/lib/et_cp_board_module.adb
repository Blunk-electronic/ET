------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                COMMAND PROCESSOR / BOARD / MODULE                        --
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
--
--
--

with ada.text_io;						use ada.text_io;
with ada.characters.handling;			use ada.characters.handling;
with ada.strings; 						use ada.strings;

with et_schematic_coordinates;

with et_sheets;
with et_module_names;					use et_module_names;
with et_module_write;					use et_module_write;

with et_canvas_schematic;
with et_canvas_board;



package body et_cp_board_module is

	

	procedure save_module (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		
		
	begin
		-- CS log message
		
		-- Since we are already in the project directory,
		-- we can call the write_module procedures right away.
		
		case cmd_field_count is
			when 4 =>
				-- Save the module with its own name:
				write_module (
					module_cursor	=> module,
					log_threshold	=> log_threshold + 1);

			when 5 =>
				-- Save the module with a different name:
				write_module (
					module_cursor	=> module,
					save_as_name	=> to_module_name (get_field (cmd, 5)), -- led_driver_test
					log_threshold	=> log_threshold + 1);
				
			when 6 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end save_module;







	procedure show_module (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		
		module_name : pac_module_name.bounded_string;

		use et_sheets;
		sheet : type_sheet := 1;

		
		-- Sets the active module and first sheet.
		procedure module_and_first_sheet is 
			use et_canvas_schematic;
			use et_schematic_coordinates;
		begin
			module_name := to_module_name (get_field (cmd, 5));
			set_module (module_name);
			active_sheet := sheet;
			
			et_canvas_schematic.update_schematic_editor;
			et_canvas_board.update_board_editor;
		end module_and_first_sheet;



		-- Sets the active module and sheet.
		procedure module_and_random_sheet is 
			use et_canvas_schematic;
			use et_schematic_coordinates;
		begin
			module_name := to_module_name (get_field (cmd, 5));
			set_module (module_name);

			log (text => "sheet " & to_string (sheet), 
				level => log_threshold + 1);
			
			sheet := to_sheet (get_field (cmd, 6));
			active_sheet := sheet;

			et_canvas_schematic.update_schematic_editor;
			et_canvas_board.update_board_editor;
		end module_and_random_sheet;
		
		
	begin		
		log (text => "show module (via board editor) " 
			& enclose_in_quotes (to_string (module)),
			level => log_threshold );

		
		case cmd_field_count is
			when 5 => module_and_first_sheet; -- show module LED-driver
			when 6 => module_and_random_sheet; -- show module LED-driver 2

			when 7 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
		
	end show_module;



		
end et_cp_board_module;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
