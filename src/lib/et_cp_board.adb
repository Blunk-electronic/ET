------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       COMMAND PROCESSOR / BOARD                          --
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
--   ToDo:
--   - command to define a global cutout area
--   - command to set design rules


with ada.text_io;					use ada.text_io;
with ada.strings; 					use ada.strings;
with ada.characters.handling;		use ada.characters.handling;
with ada.exceptions;				use ada.exceptions;

with et_module_names;					use et_module_names;
with et_runmode;						use et_runmode;
with et_script_processor;

with et_module_instance;			use et_module_instance;

with et_schematic_coordinates;
with et_board_geometry;				use et_board_geometry;
with et_board_coordinates;			use et_board_coordinates;

with et_sheets;
with et_modes.board;
with et_canvas_board_devices;
with et_canvas_board_texts;
with et_canvas_board_vias;
with et_design_rules_board;			use et_design_rules_board;

with et_board_ops;

with et_net_names;					use et_net_names;
with et_device_name;

with et_keywords;					use et_keywords;

with et_canvas_schematic;
with et_canvas_board;

with et_modes;						use et_modes;
with et_module_ops;					use et_module_ops;

with et_canvas_board_preliminary_object;

with et_exceptions;					use et_exceptions;

with et_cp_board_canvas;			use et_cp_board_canvas;
with et_cp_board_display;			use et_cp_board_display;
with et_cp_board_outline;			use et_cp_board_outline;
with et_cp_board_keepout;			use et_cp_board_keepout;
with et_cp_board_silkscreen;		use et_cp_board_silkscreen;
with et_cp_board_assy_doc;			use et_cp_board_assy_doc;
with et_cp_board_restrict;			use et_cp_board_restrict;
with et_cp_board_stopmask;			use et_cp_board_stopmask;
with et_cp_board_stencil;			use et_cp_board_stencil;
with et_cp_board_text;				use et_cp_board_text;
with et_cp_board_signal_layer;		use et_cp_board_signal_layer;
with et_cp_board_via;				use et_cp_board_via;
with et_cp_board_device;			use et_cp_board_device;
with et_cp_board_conductors;		use et_cp_board_conductors;
with et_cp_board_frame;				use et_cp_board_frame;
with et_cp_board_module;			use et_cp_board_module;


-- to do:


package body et_cp_board is


	device_missing	: constant string := "Device name missing !";
	module_missing	: constant string := "Module name missing !";
	net_missing		: constant string := "Net name missing !";


	

	procedure parse_execute_script (
		cmd				: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is 
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);

	
		-- This procedure is to be called when the command is complete.
		-- It lauches the given script and sets the exit code of
		-- the command according to the outcome of the script execution:
		procedure command_complete is
			use et_script_processor;
			exit_code : type_exit_code_script;
		begin
			exit_code := execute_nested_script (
				file			=> get_field (cmd, 5),
				log_threshold	=> log_threshold + 1);

			case exit_code is
				when SUCCESSFUL =>
					set_exit_code (cmd, 0);

				when others =>
					set_exit_code (cmd, 3);
			end case;
		end command_complete;

		
	begin
		case get_origin (cmd) is
			when ORIGIN_CONSOLE =>
				
				case cmd_field_count is
					when 4 => 
						-- Command is incomplete like "execute script"
						command_incomplete (cmd);
						
					when 5 =>
						-- Command is complete like "execute script demo.scr"
						command_complete;
								
					when 6 .. type_field_count'last =>
						command_too_long (cmd, cmd_field_count - 1);


					when others => null;
						-- CS should never happen
						raise constraint_error;
						
				end case;

				
			when ORIGIN_SCRIPT =>
				case cmd_field_count is
					when 5 => 
						-- Command is complete like "execute script demo.scr"
						command_complete;
						
					when 6 .. type_field_count'last =>
						command_too_long (cmd, cmd_field_count - 1);

					when others =>
						command_incomplete (cmd);
						
				end case;
		end case;
	end parse_execute_script;



	
	

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
	

	
	


	procedure execute_board_command (
		module_cursor	: in pac_generic_modules.cursor;
		cmd				: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		use et_board_ops;
		use pac_geometry_2;

		use et_canvas_board;
		use et_canvas_board.pac_canvas;
		use et_modes.board;


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


		-- This procedure is a shortcut. 
		-- Call it in case the given command is too long:
		procedure too_long is begin
			command_too_long (cmd, cmd_field_count - 1);
		end;


		-- This procedure is a shortcut. 
		-- Call it in case the given command is incomplete:
		procedure command_incomplete is begin
			command_incomplete (cmd);
		end;





		



		-- This procedure extracts from the command the
		-- name of the generic module and optionally the
		-- sheet number.
		-- It sets the given module and sheet as active
		-- and updates the editor window according
		-- to the activated module:
		procedure show_module is  -- GUI related

			module : pac_module_name.bounded_string;

			use et_sheets;
			sheet : type_sheet := 1;

			
			-- Sets the active module and first sheet.
			procedure module_and_first_sheet is 
				use et_canvas_schematic;
				use et_schematic_coordinates;
			begin
				module := to_module_name (get_field (5));
				set_module (module);
				active_sheet := sheet;
				
				et_canvas_schematic.update_schematic_editor;
				et_canvas_board.update_board_editor;
			end module_and_first_sheet;



			-- Sets the active module and sheet.
			procedure module_and_random_sheet is 
				use et_canvas_schematic;
				use et_schematic_coordinates;
			begin
				module := to_module_name (get_field (5));
				set_module (module);

				log (text => "sheet " & to_string (sheet), 
					level => log_threshold + 1);
				
				sheet := to_sheet (get_field (6));
				active_sheet := sheet;

				et_canvas_schematic.update_schematic_editor;
				et_canvas_board.update_board_editor;
			end module_and_random_sheet;
			
			
		begin
			log (text => "show module (via board editor) " 
				& enclose_in_quotes (to_string (module)),
				level => log_threshold + 1);

			case cmd_field_count is
				when 5 => module_and_first_sheet; -- show module LED-driver
				when 6 => module_and_random_sheet; -- show module LED-driver 2
				when 7 .. type_field_count'last => too_long;
				when others => command_incomplete;
			end case;
			
		end show_module;



		



		
		
		-- Parses the given command and dispatches to
		-- further subroutines:
		procedure parse is begin

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
							add_non_electrical_device (module_cursor, cmd, log_threshold + 1);

						when NOUN_LAYER =>
							add_signal_layer (module_cursor, cmd, log_threshold + 1);

						when others => invalid_noun (to_string (noun));
					end case;


				when VERB_CLEAR =>
					case noun is
						when NOUN_ZONE =>
							clear_zones (module_cursor, cmd, log_threshold + 1);

						when others => invalid_noun (to_string (noun));
					end case;


				when VERB_COPY =>
					case noun is
						when NOUN_DEVICE =>
							copy_device (module_cursor, cmd, log_threshold + 1);

						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_DELETE =>
					case noun is
						when NOUN_DEVICE =>
							delete_device (module_cursor, cmd, log_threshold + 1);				

						when NOUN_LAYER =>
							delete_signal_layer (module_cursor, cmd, log_threshold + 1);

						when NOUN_HOLE =>
							delete_hole_segment (module_cursor, cmd, log_threshold + 1);

						when NOUN_OUTLINE =>
							delete_outline_segment (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_SILKSCREEN =>
							delete_silkscreen (module_cursor, cmd, log_threshold + 1);

						when NOUN_ASSY =>
							delete_assy_doc (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_KEEPOUT =>
							delete_keepout (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_STENCIL =>
							delete_stencil (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_STOPMASK =>
							delete_stopmask (module_cursor, cmd, log_threshold + 1);

						when NOUN_VIA =>
							delete_via (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_ROUTE_RESTRICT =>
							delete_route_restrict (module_cursor, cmd, log_threshold + 1);

						when NOUN_VIA_RESTRICT =>
							delete_via_restrict (module_cursor, cmd, log_threshold + 1);

						when NOUN_FREETRACK =>
							delete_freetrack_segment (module_cursor, cmd, log_threshold + 1);

						when NOUN_TRACK =>
							delete_net_segment (module_cursor, cmd, log_threshold + 1);
							
						when others => invalid_noun (to_string (noun));

					end case;

					
				when VERB_DISPLAY =>
					case noun is
						when NOUN_SILKSCREEN
							| NOUN_ASSY | NOUN_KEEPOUT | NOUN_STOPMASK | NOUN_STENCIL | NOUN_ORIGINS =>
							display_non_conductor (cmd, log_threshold + 1);
							
						when NOUN_CONDUCTORS =>
							display_conductor (cmd, log_threshold + 1);

						when NOUN_OUTLINE =>
							display_outline (cmd, log_threshold + 1);

						when NOUN_RATSNEST =>
							display_ratsnest (cmd, log_threshold + 1);
							
						when NOUN_RESTRICT =>
							display_restrict (cmd, log_threshold + 1);

						when NOUN_VIAS => 
							display_vias (cmd, log_threshold + 1);
							
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_DRAW =>
					case noun is
						when NOUN_HOLE =>
							draw_board_hole (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_OUTLINE =>
							draw_board_outline (module_cursor, cmd, log_threshold + 1);

						when NOUN_SILKSCREEN =>
							draw_silkscreen (module_cursor, cmd, log_threshold + 1);

						when NOUN_ASSY =>
							draw_assy_doc (module_cursor, cmd, log_threshold + 1);

						when NOUN_KEEPOUT =>
							draw_keepout_zone (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_ROUTE_RESTRICT =>
							draw_route_restrict (module_cursor, cmd, log_threshold + 1);

						when NOUN_STENCIL =>
							draw_stencil (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_STOPMASK =>
							draw_stopmask (module_cursor, cmd, log_threshold + 1);

						when NOUN_VIA_RESTRICT =>
							draw_via_restrict (module_cursor, cmd, log_threshold + 1);
							
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_EXECUTE =>
					case noun is
						when NOUN_SCRIPT =>
							parse_execute_script (cmd, log_threshold);
								
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_EXIT | VERB_QUIT => 
					null;
					-- CS terminate_main; 
					-- CS does not work via script (gtk error ...)

					
				when VERB_FILL =>
					case noun is
						when NOUN_ZONE =>
							fill_zones (module_cursor, cmd, log_threshold + 1);
							
						when others => 
							invalid_noun (to_string (noun));
					end case;

					
				when VERB_FLIP =>
					case noun is
						when NOUN_DEVICE =>
							flip_device (module_cursor, cmd, log_threshold + 1);

						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_MAKE =>
					case noun is
						when NOUN_PNP =>
							case cmd_field_count is
								when 4 =>
									make_pick_and_place 
										(
										module_name 	=> module,
										log_threshold	=> log_threshold + 1);

								when 5 .. type_field_count'last =>
									too_long;
									
								when others =>
									command_incomplete;
							end case;

						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_MOVE =>
					case noun is
						when NOUN_FRAME =>
							move_drawing_frame (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_CURSOR =>
							move_cursor (cmd, log_threshold + 1);
							
						when NOUN_DEVICE =>
							move_device (module_cursor, cmd, log_threshold + 1);

						when NOUN_NAME | NOUN_VALUE | NOUN_PARTCODE | NOUN_PURPOSE =>
							move_device_placeholder (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_SUBMODULE =>
							case cmd_field_count is
								when 8 =>
									move_submodule (
										module_name 	=> module,
										instance		=> to_instance_name (get_field (5)), -- OSC1
										coordinates		=> to_coordinates (get_field (6)),  -- relative/absolute
										point			=> type_vector_model (set (
															x => to_distance (dd => get_field (7)),
															y => to_distance (dd => get_field (8)))),
										log_threshold	=> log_threshold + 1
										);

								when 9 .. type_field_count'last =>
									too_long;
									
								when others =>
									command_incomplete;
							end case;

						when NOUN_VIA =>
							move_via (module_cursor, cmd, log_threshold + 1);
							
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_PLACE =>
					case noun is
						when NOUN_VIA => 
							place_via (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_TEXT =>
							place_text (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_PLACEHOLDER
							=> place_text_placeholder (module_cursor, cmd, log_threshold + 1);
							
						when others	=> invalid_noun (to_string (noun));
					end case;
					

				when VERB_RENAME =>
					case noun is
						when NOUN_DEVICE =>
							rename_device (module_cursor, cmd, log_threshold + 1);

						when others => invalid_noun (to_string (noun));
					end case;


				when VERB_RESTORE =>
					case noun is
						when NOUN_PLACEHOLDERs =>
							restore_device_placeholders (module_cursor, cmd, log_threshold + 1);
							
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_ROUTE =>
					case noun is
						when NOUN_FREETRACK =>
							route_freetrack (module_cursor, cmd, log_threshold + 1);

						when NOUN_NET =>
							route_net (module_cursor, cmd, log_threshold + 1);
							
						when others => invalid_noun (to_string (noun));
					end case;

				
				when VERB_ROTATE =>
					case noun is
						when NOUN_DEVICE =>
							rotate_device (module_cursor, cmd, log_threshold + 1);

						when NOUN_NAME | NOUN_VALUE | NOUN_PARTCODE | NOUN_PURPOSE =>
							rotate_device_placeholder (module_cursor, cmd, log_threshold + 1);
							
						when others => invalid_noun (to_string (noun));
					end case;


				when VERB_SAVE =>
					case noun is
						when NOUN_MODULE =>
							save_module (module_cursor, cmd, log_threshold + 1);
							
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_SET =>
					case noun is
						when NOUN_GRID =>
							set_grid (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_CURSOR =>
							set_cursor (cmd, log_threshold + 1);

						when NOUN_ZOOM =>
							set_zoom (cmd, log_threshold + 1);

						when NOUN_SCALE =>
							set_scale (cmd, log_threshold + 1);
							
						when NOUN_ZONE =>
							set_fill_zone_properties (module_cursor, cmd, log_threshold + 1);
							-- conductor layers related

						-- CS NOUN_VALUE, NOUN_PARTCODE, NOUN_PURPOSE ?
							
						when NOUN_VIA =>
							set_via_properties (module_cursor, cmd, log_threshold + 1);

						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_SHOW => -- GUI related
					case noun is
						when NOUN_MODULE =>
							show_module;

						when NOUN_DEVICE =>
							show_device (module_cursor, cmd, log_threshold + 1);
							
						when NOUN_NET =>
							show_net (module_cursor, cmd, log_threshold + 1);
		
					when others => invalid_noun (to_string (noun));
				end case;


				when VERB_UPDATE =>
					case noun is
						when NOUN_RATSNEST =>
							update_ratsnest (module_cursor, cmd, log_threshold + 1);

						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_ZOOM =>
					zoom_all (cmd, log_threshold + 1);


				when others =>
					null;
					
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


		
		

		-- This procedure proposes missing arguments:
		procedure propose_arguments is
			use et_canvas_board_devices;
			use et_canvas_board_texts;
			use et_canvas_board_vias;
			use et_canvas_board_preliminary_object;
			use et_device_name;
			device_name : type_device_name;

			
			procedure module_name_missing is begin
				set_status (incomplete & module_missing);
			end module_name_missing;

			
			procedure device_name_missing is begin
				set_status (incomplete & device_missing);
				-- No menu required and not reasonable.
				-- It might become very long if there were hundreds of devices.
			end device_name_missing;

			
			procedure device_not_found is begin
				set_status ("ERROR: Device " & to_string (device_name) & " not found !");
			end device_not_found;

			
			procedure net_name_missing is begin
				set_status (incomplete & net_missing);
			end net_name_missing;

			
		begin -- propose_arguments

			-- Missing arguments are to be proposed only if
			-- the command origin is the console.
			-- Otherwise nothing happens here.
			if not is_complete (cmd) and get_origin (cmd) = ORIGIN_CONSOLE then
			
				log_command_incomplete (cmd_field_count, log_threshold);

				case verb is
					when VERB_PLACE =>
						case noun is
							when NOUN_TEXT =>
								show_text_properties;
								set_finalization_pending (cmd);

							when NOUN_VIA =>
								case cmd_field_count is
									when 4 => -- place via
										show_via_properties;
										set_finalization_pending (cmd);

									when 5 => -- place via RESET_N
										-- Preset the net name so that it is visible
										-- in the via properties bar:
										object_net_name := to_net_name (get_field (5));

										show_via_properties;
										set_finalization_pending (cmd);

									when others => null;
								end case;
								
							when others => null; -- CS
						end case;

					when others => null; -- CS
				end case;
				
			end if;
		end propose_arguments;

		


		
	begin
		log (text => "execute board command: " & enclose_in_quotes (get_all_fields (cmd)),
			level => log_threshold);

		log_indentation_up;
		
		log (text => "command origin: " & get_origin (cmd), level => log_threshold);

		
		module := to_module_name (get_field (2)); -- motor_driver (without extension *.mod)
		-- CS: Becomes obsolete once all board ops use the
		-- given module_cursor.


		set_verb_and_noun;
		

		-- Once verb and noun are known, they must be shown
		-- in the verb-noun-display:
		update_verb_noun_display;
		
		
		-- parse the command:
		parse;


		-- If the command is incomplete and if it was entered
		-- via the console, then further arguments are proposed.
		-- Otherwise nothing happens here:
		propose_arguments;


		evaluate_command_exit_code (cmd, log_threshold);

		
		-- After every command (regardless if it is complete or not)
		-- set the focus to the canvas:
		-- CS: remove ?
		-- if runmode /= MODE_HEADLESS then
		-- 	canvas.grab_focus; -- NOTE ! calls "cb_draw"
		-- end if;

		log_indentation_down;
		

		-- exception when event: others =>

				-- null;
			-- CS
				-- evaluate_exception (
				-- 	name	=> exception_name (event),
				-- 	message	=> exception_message (event));

				-- raise;
	end execute_board_command;

	
end et_cp_board;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
