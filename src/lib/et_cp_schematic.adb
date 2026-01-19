------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                      COMMAND PROCESSOR / SCHEMATIC                       --
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
-- - command to set design rules
-- - command to set assembly variants
-- - command to add a text note

with ada.text_io;						use ada.text_io;
with ada.containers;
with ada.characters.handling;			use ada.characters.handling;
with ada.strings; 						use ada.strings;
with ada.exceptions;					use ada.exceptions;

with et_module_names;					use et_module_names;
with et_runmode;						use et_runmode;

with et_modes.schematic;

with et_schematic_geometry;
with et_schematic_coordinates;
with et_sheets;							use et_sheets;

with et_canvas_schematic;
with et_canvas_board;

with et_modes;							use et_modes;
with et_module_ops;						use et_module_ops;
with et_module_write;					use et_module_write;

with et_cp_schematic_canvas;			use et_cp_schematic_canvas;
with et_cp_schematic_display;			use et_cp_schematic_display;
with et_cp_schematic_assembly_variant;	use et_cp_schematic_assembly_variant;
with et_cp_schematic_submodule;			use et_cp_schematic_submodule;
with et_cp_schematic_script;			use et_cp_schematic_script;
with et_cp_schematic_device;			use et_cp_schematic_device;
with et_cp_schematic_unit;				use et_cp_schematic_unit;
with et_cp_schematic_nets;				use et_cp_schematic_nets;
with et_cp_schematic_netchanger;		use et_cp_schematic_netchanger;


package body et_cp_schematic is
	

	
	
	procedure evaluate_command_exit_code (
		cmd				: in type_single_cmd;
		log_threshold	: in type_log_level)
	is 
		code : constant type_exit_code_command := get_exit_code (cmd);
	begin
		case code is
			when 0 => null; -- no errors

			when 1 => -- command incomplete
				log (ERROR, "Command incomplete. Exit code" & to_string (code), 
					level => log_threshold);

			when 2 => -- command too long
				log (ERROR, "Command too long. Exit code" & to_string (code), 
					level => log_threshold);

			when others =>
				log (ERROR, "Other error. Exit code" & to_string (code), 
					level => log_threshold);
		end case;
	end evaluate_command_exit_code;
	



	
	
	procedure execute_schematic_command (
		module_cursor	: in pac_generic_modules.cursor;
		cmd				: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		use et_schematic_coordinates;
		use et_schematic_geometry;
		use pac_geometry_2;

		use et_canvas_schematic;
		use et_canvas_schematic.pac_canvas;
		use et_modes.schematic;


		

		-- This function is a shortcut to get a single field
		-- from the given command:
		function get_field (place : in type_field_count) 
			return string 
		is begin
			return get_field (cmd, place);
		end;

		
		-- This procedure sets the verb and the noun:
		procedure set_verb_and_noun is begin
			-- Set the verb.
			-- Read it from field 3:
			verb := to_verb (get_field (3));

			
			-- There are some very short commands which do not require a noun.
			-- For such commands we do not read the noun.
			case verb is
				when VERB_EXIT | VERB_QUIT => null; -- no noun
				
				-- Set the noun. Read it from field 4:		
				when others => noun := to_noun (get_field (4));
			end case;
		end set_verb_and_noun;



		
		-- Updates the verb-noun display depending on the 
		-- origin of the command and the runmode:
		procedure update_verb_noun_display is begin
			case get_origin (cmd) is
				when ORIGIN_CONSOLE => update_mode_display;

				when ORIGIN_SCRIPT =>
					-- put_line ("script");
				
					if runmode = MODE_MODULE then
						-- put_line ("module");
						-- log (text => "update verb-noun-display", level => log_threshold + 1);
						update_mode_display;
					end if;

			end case;
		end update_verb_noun_display;
		

		
		module	: pac_module_name.bounded_string; -- motor_driver (without extension *.mod)




		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);


		-- This procedure is a shortcut. Call it in case the given command is too long:
		procedure too_long is begin
			command_too_long (cmd, cmd_field_count - 1);
		end;


		-- This procedure is a shortcut. 
		-- Call it in case the given command is incomplete:
		procedure command_incomplete is begin
			command_incomplete (cmd);
		end;
		
		
	



		
		
		-- This procedure extracts from the command the
		-- sheet number and sets it active.
		-- It updates the sheet number display accordingly:
		procedure show_sheet is -- GUI related
			sheet : type_sheet;

			procedure show is
			begin
				sheet := to_sheet (get_field (5));

				-- CS test whether sheet exists
				
				active_sheet := sheet;
				update_sheet_number_display;
			end show;
				
		begin
			log (text => "set sheet" & to_string (sheet), level => log_threshold + 1); 
			
			case cmd_field_count is
				when 5 => show;
				when 6 .. type_field_count'last => too_long;
				when others => command_incomplete;
			end case;
			
		end show_sheet;




		

	-----------------------------------------------------------------------------------	

	-- MODULE OPERATIONS:
		
		
		procedure create_module is
			
			procedure do_it (
				module_name : in pac_module_name.bounded_string) 
			is
				use pac_generic_modules;
			begin
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
			case cmd_field_count is
				when 5 => do_it (to_module_name (get_field (5)));
				when 6 .. type_field_count'last => too_long;
				when others => command_incomplete;
			end case;
		end create_module;



		
		
		-- This procedure extracts from the command the
		-- name of the generic module and optionally the
		-- sheet number.
		-- It sets the given module and sheet as active
		-- and updates the editor window according
		-- to the activated module:
		procedure show_module is  -- GUI related

			module : pac_module_name.bounded_string;

			sheet : type_sheet := 1;

			
			-- Sets the active module and first sheet.
			procedure module_and_first_sheet is begin
				module := to_module_name (get_field (5));
				set_module (module);
				active_sheet := sheet;
				
				update_schematic_editor;
				et_canvas_board.update_board_editor;
			end module_and_first_sheet;



			-- Sets the active module and sheet.
			procedure module_and_random_sheet is begin
				module := to_module_name (get_field (5));
				set_module (module);

				log (text => "sheet " & to_string (sheet), 
					level => log_threshold + 1);
				
				sheet := to_sheet (get_field (6));
				active_sheet := sheet;

				update_schematic_editor;
				et_canvas_board.update_board_editor;
			end module_and_random_sheet;
			
			
		begin
			log (text => "show module (via schematic editor) " 
				& enclose_in_quotes (to_string (module)),
				level => log_threshold + 1);

			case cmd_field_count is
				when 5 => module_and_first_sheet; -- show module LED-driver
				when 6 => module_and_random_sheet; -- show module LED-driver 2
				when 7 .. type_field_count'last => too_long;
				when others => command_incomplete;
			end case;
			
		end show_module;

		


		

		procedure delete_module is

			use ada.containers;
			use pac_generic_modules;

			
			-- Delete the current active module:
			procedure delete_active is begin
				delete_module (
					module_name		=> key (module_cursor),
					log_threshold	=> log_threshold + 1);

				-- As long as there are other modules, open the 
				-- first of the generic modules.
				-- If no modules available any more, close the schematic
				-- and board editor:

				-- CS Set the previous module active instead ?
				if length (generic_modules) > 0 then
					
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


			
			procedure delete_explicit (
				module_name : in pac_module_name.bounded_string) 
			is begin
				delete_module (
					module_name		=> module_name, -- led_driver_test
					log_threshold	=> log_threshold + 1);

				-- As long as there are other modules, open the 
				-- first of the generic modules.
				-- If no modules available any more, close the schematic
				-- and board editor:
				
				-- CS Set the previous module active instead ?
				if length (generic_modules) > 0 then
				
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
			case cmd_field_count is
				when 4 => delete_active;								
				when 5 => delete_explicit (to_module_name (get_field (5)));
				when 6 .. type_field_count'last => too_long;								
				when others => command_incomplete;
			end case;
		end delete_module;



		
		
		-- Actions to save a module:
		procedure save_module is 
		begin
			-- Since we are already in the project directory,
			-- we can call the save_module procedures right away.
			
			case cmd_field_count is
				when 4 =>
					-- Save the module with its own name:
					write_module (
						module_cursor	=> active_module,
						log_threshold	=> log_threshold + 1);

				when 5 =>
					-- Save the module with a different name:
					write_module (
						module_cursor	=> active_module,
						save_as_name	=> to_module_name (get_field (5)), -- led_driver_test
						log_threshold	=> log_threshold + 1);
					
				when 6 .. type_field_count'last => too_long;
					
				when others => command_incomplete;
			end case;			

		end save_module;

		
		

	-----------------------------------------------------------------------------------


		

		
		-- Parses the given command and dispatches to
		-- further subroutines:
		procedure parse is 
		begin
			log (text => "parse", level => log_threshold + 1);
			log_indentation_up;
			

			-- Clear the status bar if we are in graphical mode:
			if runmode /= MODE_HEADLESS then
				status_clear;
			end if;

			
			case verb is
				when VERB_ADD =>
					case noun is
						when NOUN_DEVICE =>
							add_device (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_NETCHANGER =>
							add_netchanger (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_PORT =>
							add_port_to_submodule (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_SUBMODULE =>
							add_submodule (module_cursor, cmd, log_threshold + 1);
							
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_BUILD =>
					case noun is
						when NOUN_SUBMODULES_TREE =>
							build_submodules_tree (module_cursor, cmd, log_threshold + 1);

						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_CHECK =>
					case noun is
						when NOUN_INTEGRITY =>
							check_submodules_integrity (module_cursor, cmd, log_threshold + 1);
								
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_COPY =>
					case noun is
						when NOUN_DEVICE =>
							copy_device (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_SUBMODULE =>
							copy_submodule (module_cursor, cmd, log_threshold + 1);
							
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_CREATE =>
					case noun is
						when NOUN_VARIANT => 
							create_assembly_variant (module_cursor, cmd, log_threshold + 1);

						when NOUN_MODULE =>
							create_module;
							
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_DELETE =>
					case noun is
						when NOUN_DEVICE =>
							delete_device (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_NET_CONNECTOR =>
							delete_net_connector (module_cursor, cmd, log_threshold + 1);

						when NOUN_NET_LABEL =>
							delete_net_label (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_MODULE =>
							delete_module;
							
						when NOUN_NET =>
							delete_net (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_NETCHANGER =>
							delete_netchanger (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_PORT =>
							delete_port_of_submodule (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_SEGMENT =>
							delete_net_segment (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_STRAND =>
							delete_net_strand (module_cursor, cmd, log_threshold + 1);

						when NOUN_SUBMODULE =>
							delete_submodule (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_TEXT =>
							NULL; -- CS
							
						when NOUN_UNIT =>
							delete_unit (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_VARIANT => 
							delete_assembly_variant (module_cursor, cmd, log_threshold + 1);
							
						when others => invalid_noun (to_string (noun));
					end case;
					
					
				when VERB_DESCRIBE =>
					case noun is
						when NOUN_VARIANT => 
							describe_assembly_variant (module_cursor, cmd, log_threshold + 1);
							
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_DISPLAY =>
					case noun is
						when NOUN_PORTS		-- like "schematic led_driver display ports [on/off]"
							| NOUN_NETS		-- like "schematic led_driver display nets [on/off]"
							| NOUN_NAMES | NOUN_VALUES | NOUN_PURPOSES
							| NOUN_TEXTS
							=> display (cmd, log_threshold + 1);

						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_DRAG =>
					case noun is
						when NOUN_UNIT =>
							drag_unit (module_cursor, cmd, log_threshold + 1);
									
						when NOUN_NETCHANGER =>
							drag_netchanger (module_cursor, cmd, log_threshold + 1);

						when NOUN_PORT =>
							drag_port_of_submodule (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_SEGMENT =>
							drag_net_segment (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_SUBMODULE =>
							drag_submodule (module_cursor, cmd, log_threshold + 1);
							
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_DRAW =>
					case noun is
						when NOUN_NET => draw_net (module_cursor, cmd, log_threshold + 1);
							
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_EXECUTE =>
					case noun is
						when NOUN_SCRIPT =>
							execute_script (cmd, log_threshold);
								
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_EXIT | VERB_QUIT => 
					null;
					-- CS terminate_main;
					-- CS does not work via script (gtk error ...)

					
				when VERB_FETCH =>
					case noun is
						when NOUN_UNIT =>
							fetch_unit (module_cursor, cmd, log_threshold + 1);

						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_MOVE =>
					case noun is
						when NOUN_CURSOR =>
							move_cursor (cmd, log_threshold + 1);
							
						when NOUN_NAME | NOUN_VALUE | NOUN_PARTCODE | NOUN_PURPOSE =>
							move_unit_placeholder (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_PORT =>
							move_port_of_submodule (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_NETCHANGER =>
							move_netchanger (module_cursor, cmd, log_threshold + 1);

						when NOUN_NET_LABEL =>
							move_net_label (module_cursor, cmd, log_threshold + 1);
									
						when NOUN_TEXT =>
							NULL; -- CS
							
						when NOUN_SUBMODULE =>
							move_submodule (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_UNIT =>
							move_unit (module_cursor, cmd, log_threshold + 1);
									
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_MAKE =>
					case noun is
						when NOUN_NETLISTS => 
							export_netlist (module_cursor, cmd, log_threshold + 1);
							
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_MOUNT =>
					case noun is
						when NOUN_DEVICE => 
							mount_device (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_SUBMODULE =>
							mount_submodule (module_cursor, cmd, log_threshold + 1);
												
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_PLACE =>
					case noun is
						when NOUN_NET_CONNECTOR =>
							place_net_connector (module_cursor, cmd, log_threshold + 1);

						when NOUN_NET_LABEL =>
							place_net_label (module_cursor, cmd, log_threshold + 1);

						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_REMOVE =>
					case noun is
						when NOUN_DEVICE => 
							remove_device (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_SUBMODULE =>
							remove_submodule (module_cursor, cmd, log_threshold + 1);
							
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_RENAME =>
					case noun is
						when NOUN_DEVICE =>
							rename_device (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_SUBMODULE =>
							rename_submodule (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_NET =>
							rename_net (module_cursor, cmd, log_threshold + 1);
					
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_RENUMBER =>
					case noun is
						when NOUN_DEVICES =>
							renumber_devices (module_cursor, cmd, log_threshold + 1);
							
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_ROTATE =>
					case noun is
						when NOUN_TEXT =>
							NULL; -- CS

						when NOUN_UNIT =>
							rotate_unit (module_cursor, cmd, log_threshold + 1);
									
						when NOUN_NAME | NOUN_VALUE | NOUN_PURPOSE | NOUN_PARTCODE =>
							rotate_unit_placeholder (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_NETCHANGER =>
							rotate_netchanger (module_cursor, cmd, log_threshold + 1);
							
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_SAVE =>
					case noun is
						when NOUN_MODULE =>
							save_module;
							
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_SET =>
					case noun is
						when NOUN_CLASS =>
							set_net_class (module_cursor, cmd, log_threshold + 1);
												
						when NOUN_GRID =>
							set_grid (module_cursor, cmd, log_threshold + 1);

						when NOUN_CURSOR =>
							set_cursor (cmd, log_threshold + 1);
						
						when NOUN_ZOOM =>
							set_zoom (cmd, log_threshold + 1);
							
						when NOUN_SCALE =>
							set_scale (cmd, log_threshold + 1);
							
						when NOUN_PARTCODE =>
							set_device_partcode (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_PURPOSE =>
							set_device_purpose (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_SCOPE =>
							set_net_scope (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_SUBMODULE_FILE =>
							set_submodule_file (module_cursor, cmd, log_threshold + 1);							

						when NOUN_VALUE =>
							set_device_value (module_cursor, cmd, log_threshold + 1);					
							
						when NOUN_VARIANT =>
							set_device_package_variant (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_TEXT_SIZE =>
							NULL; -- CS
							
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_SHOW => -- GUI related
					case noun is
						when NOUN_DEVICE =>
							show_device (module_cursor, cmd, log_threshold + 1);

						when NOUN_MODULE =>
							show_module;
							
						when NOUN_NET =>
							show_net (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_SHEET =>
							show_sheet;
						
							
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_UNMOUNT =>
					case noun is
						when NOUN_DEVICE => 
							unmount_device (module_cursor, cmd, log_threshold + 1);
							
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_WRITE =>
					case noun is
						when NOUN_TEXT =>
							NULL; -- CS

						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_ZOOM =>
					zoom_all (cmd, log_threshold + 1);


				when others => null;
					
			end case;

			
			-- Update GUI if we are in graphical mode:
			if runmode /= MODE_HEADLESS then
				null;			
			end if;


			log_indentation_down;
			
			
			exception
				when event: others =>
					-- log (text => ada.exceptions.exception_information (event), console => true);
					log (text => ada.exceptions.exception_information (event));
			
		end parse;		


		
		
	begin
		log (text => "execute schematic command: " & enclose_in_quotes (get_all_fields (cmd)),
			level => log_threshold);

		log_indentation_up;
		
		log (text => "command origin: " & get_origin (cmd), level => log_threshold + 1);

		set_verb_and_noun;
		

		-- Once verb and noun are known, they must be shown
		-- in the verb-noun-display:
		update_verb_noun_display;
		
		
		-- Parse the command:	
		parse;


		evaluate_command_exit_code (cmd, log_threshold);

		
		-- After each command (regardless if it is complete or not)
		-- set the focus to the canvas:
		-- CS: remove ?
		-- if runmode /= MODE_HEADLESS then
		-- 	canvas.grab_focus; -- NOTE ! calls "cb_draw"
		-- end if;

		log_indentation_down;

		
		-- exception when event: others =>

				-- CS
				-- evaluate_exception (
				-- 	name	=> exception_name (event),
				-- 	message	=> exception_message (event));

				-- raise;
	end execute_schematic_command;
	
		
end et_cp_schematic;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
