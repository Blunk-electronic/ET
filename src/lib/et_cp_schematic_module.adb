------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--              COMMAND PROCESSOR / SCHEMATIC / MODULE                      --
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
-- - rework
-- - propose arguments if command incomplete
--

with ada.text_io;						use ada.text_io;
with ada.characters.handling;			use ada.characters.handling;
with ada.strings; 						use ada.strings;
with ada.containers;

with et_module_write;					use et_module_write;
with et_module_ops;						use et_module_ops;
with et_module_names;					use et_module_names;
with et_sheets;
with et_schematic_coordinates;			use et_schematic_coordinates;

with et_canvas_schematic;				use et_canvas_schematic;
with et_canvas_board;


package body et_cp_schematic_module is

	use pac_generic_modules;



	procedure create_module (
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		


		procedure do_it (
			module_name : in pac_module_name.bounded_string) 
		is begin
			create_module (
				module_name		=> module_name, -- led_driver_test
				log_threshold	=> log_threshold + 1);

			-- Show the module in schematic and board editor:
			
			active_module := locate_module (module_name);
			active_sheet := 1;

			-- Update module name in the schematic window title bar:
			-- CS set_title_bar (active_module);
			
			-- CS update_sheet_number_display;
			
			-- Update the board window title bar:
			-- CS et_canvas_board.set_title_bar (active_module);
		end do_it;

		
		
	begin
		-- CS log message
		
		case cmd_field_count is
			when 5 => do_it (to_module_name (get_field (cmd, 5)));
			
			when 6 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
		
	end create_module;

	








	procedure show_module (
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		module : pac_module_name.bounded_string;

		use et_sheets;
		sheet : type_sheet := 1;

		
		-- Sets the active module and first sheet.
		procedure module_and_first_sheet is begin
			module := to_module_name (get_field (cmd, 5));
			set_module (module);
			active_sheet := sheet;
			
			update_schematic_editor;
			et_canvas_board.update_board_editor;
		end module_and_first_sheet;



		-- Sets the active module and sheet.
		procedure module_and_random_sheet is begin
			module := to_module_name (get_field (cmd, 5));
			set_module (module);

			log (text => "sheet " & to_string (sheet), 
				level => log_threshold + 1);
			
			sheet := to_sheet (get_field (cmd, 6));
			active_sheet := sheet;

			update_schematic_editor;
			et_canvas_board.update_board_editor;
		end module_and_random_sheet;
		
		
	begin
		log (text => "show module (via schematic editor) " 
			& enclose_in_quotes (to_string (module)),
			level => log_threshold);

		case cmd_field_count is
			when 5 => module_and_first_sheet; -- show module LED-driver
			
			when 6 => module_and_random_sheet; -- show module LED-driver 2
			
			when 7 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end show_module;






	

	procedure delete_module (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		


		use ada.containers;

		
		-- Delete the current module:
		procedure delete_active is begin
			delete_module (
				module_name		=> key (module),
				log_threshold	=> log_threshold + 1);

			-- As long as there are other modules, open the 
			-- first of the generic modules.
			-- If no modules available any more, close the schematic
			-- and board editor:

			-- CS Set the previous module active instead ?
			if length (generic_modules) > 0 then
			-- CS use function that returns the number of generic modules
				
				active_module := generic_modules.first;
				active_sheet := 1;

				log (text => "set module " 
					& enclose_in_quotes (get_active_module), 
					level => log_threshold + 1);

				-- Update module name in the schematic window title bar:
				set_title_bar (active_module);
				
				update_sheet_number_display;
				
				-- Update the board window title bar:
				et_canvas_board.set_title_bar (active_module);
			else
				-- CS
				null;
				-- terminate_main;
			end if;
		end delete_active;


		
		-- Deletes an explicitly given module:
		procedure delete_explicit (
			module_name : in pac_module_name.bounded_string) 
		is begin
			delete_module (
				module_name		=> module_name, -- pwr_supply
				log_threshold	=> log_threshold + 1);

			-- As long as there are other modules, open the 
			-- first of the generic modules.
			-- If no modules available any more, close the schematic
			-- and board editor:
			
			-- CS Set the previous module active instead ?
			if length (generic_modules) > 0 then
			-- CS use function that returns the number of generic modules
			
				active_module := generic_modules.first;
				active_sheet := 1;

				log (text => "set module " 
					& enclose_in_quotes (get_active_module), 
					level => log_threshold + 1);

				-- Update module name in the schematic window title bar:
				set_title_bar (active_module);
				
				update_sheet_number_display;
				
				-- Update the board window title bar:
				et_canvas_board.set_title_bar (active_module);
			else
				-- CS
				null;
				-- terminate_main;
			end if;
		end delete_explicit;

		
	begin
		-- CS log message
		
		case cmd_field_count is
			when 4 => delete_active;
			-- schematic demo delete module
			
			when 5 => delete_explicit (to_module_name (get_field (cmd, 5)));
			-- schematic demo delete module pwr_supply
			
			when 6 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end delete_module;





	



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
					save_as_name	=> to_module_name (get_field (cmd, 5)), -- pwr_supply
					log_threshold	=> log_threshold + 1);

				
			when 6 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;	
	end save_module;

	
		
end et_cp_schematic_module;


	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
