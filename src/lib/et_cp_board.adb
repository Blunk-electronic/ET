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
--   - command to move netchanger, to change the signal layer of a netchanger

with ada.text_io;					use ada.text_io;
with ada.strings; 					use ada.strings;
with ada.characters.handling;		use ada.characters.handling;
with ada.exceptions;				use ada.exceptions;

with et_runmode;					use et_runmode;

with et_modes;						use et_modes;
with et_modes.board;				use et_modes.board;

with et_canvas_schematic;
with et_canvas_board;

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
with et_cp_board_material_pnp;		use et_cp_board_material_pnp;
with et_cp_board_submodule;			use et_cp_board_submodule;
-- CS with et_cp_board_netchanger;		use et_cp_board_netchanger;
with et_cp_board_script;			use et_cp_board_script;


package body et_cp_board is


	

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
		use et_canvas_board;
		use et_canvas_board.pac_canvas;



		
		-- This procedure sets the verb and the noun:
		procedure set_verb_and_noun is begin
			-- Set the verb.
			-- Read it from field 3:
			verb := to_verb (get_field (cmd, 3));

			
			-- There are some very short commands which do not require a noun.
			-- For such commands we do not read the noun.
			case verb is
				when VERB_EXIT | VERB_QUIT => null; -- no noun
				
				-- Set the noun. Read it from field 4:		
				when others => noun := to_noun (get_field (cmd, 4));
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
							execute_script (cmd, log_threshold);
								
						when others => invalid_noun (to_string (noun));
					end case;

					
				when VERB_EXIT | VERB_QUIT => 
					null;
					-- CS terminate_main; 
					-- CS does not work via script (gtk error ...)
					-- CS ask to save the module ?

					
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
							export_pick_and_place  (module_cursor, cmd, log_threshold + 1);

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
							move_submodule (module_cursor, cmd, log_threshold + 1);

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

					
				when VERB_SHOW =>
					case noun is
						when NOUN_MODULE =>
							show_module (module_cursor, cmd, log_threshold + 1);

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

			
			log_indentation_down;

			
			exception
				when event: others =>
					-- log (text => ada.exceptions.exception_information (event), console => true);
					log (text => ada.exceptions.exception_information (event));
			
		end parse;


	
	begin
		log (text => "execute board command: " 
			& enclose_in_quotes (get_all_fields (cmd)),
			level => log_threshold);

		log_indentation_up;
		
		log (text => "command origin: " & get_origin (cmd), 
			 level => log_threshold);

	
		set_verb_and_noun;
		

		-- Once verb and noun are known, they must be shown
		-- in the verb-noun-display:
		update_verb_noun_display;
		
		
		-- parse the command:
		parse;



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
