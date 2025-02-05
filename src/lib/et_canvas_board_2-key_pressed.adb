------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           CANVAS FOR BOARD                               --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                -- 
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with et_canvas_tool;					use et_canvas_tool;

with et_board_ops.conductors;			use et_board_ops.conductors;
with et_board_ops.ratsnest;
with et_board_verb_noun_keys;			use et_board_verb_noun_keys;

with et_ratsnest;

with et_canvas_board_preliminary_object;

-- with et_canvas_board_assy_doc;			use et_canvas_board_assy_doc;
-- with et_canvas_board_silkscreen;		use et_canvas_board_silkscreen;


separate (et_canvas_board_2)


procedure key_pressed (
	key			: in gdk_key_type;
	key_shift	: in gdk_modifier_type)
is
	use gdk.types;
	use gdk.types.keysyms;

	use et_modes;
	use et_modes.board;

	-- use et_canvas_board_devices;
	
	
	point : type_vector_model renames get_cursor_position;
	


	procedure clear is begin
		case key is
			when key_noun_zone =>
				noun := NOUN_ZONE;
				clear_zones (active_module, log_threshold + 1);

				set_status ("conductor zones cleared");
				
			when others => status_noun_invalid;
		end case;
	end clear;

	
	
	procedure delete is begin
		case key is
			when key_noun_assy =>
				noun := NOUN_ASSY;
				set_status (et_canvas_board_assy_doc.status_delete_object);

			when key_noun_silkscreen =>
				noun := NOUN_SILKSCREEN;
				set_status (et_canvas_board_silkscreen.status_delete_object);

			
			when key_noun_non_electrical_device =>
				noun := NOUN_NON_ELECTRICAL_DEVICE;
				set_status (et_canvas_board_devices.status_delete_device);

			-- NOTE: Electrical devices can be deleted in
			-- schematic only !


			when key_noun_outline =>
				noun := NOUN_OUTLINE;
				set_status (et_canvas_board_outline.status_delete_object);
				

			when key_noun_via =>
				noun := NOUN_VIA;
				set_status (et_canvas_board_vias.status_delete_via);


			when key_noun_conductors =>
				noun := NOUN_CONDUCTORS;
				set_status (et_canvas_board_conductors.status_delete_object);
				
			when key_noun_track =>
				noun := NOUN_TRACK;
				set_status (et_canvas_board_tracks.status_delete_object);
				
				
			when key_noun_freetrack =>
				noun := NOUN_FREETRACK;
				set_status (et_canvas_board_conductors.status_delete_object);


				
			-- If "m" pressed, then a mode is being selected.
			when key_mode =>
				case noun is
					when NOUN_TRACK =>
						et_canvas_board_tracks.next_ripup_mode;
						
					when others => null;
				end case;

				
				
			-- If space pressed then the operator wishes to operate by keyboard:
			when key_space =>		
				null;
				-- CS
				case noun is
					when NOUN_CONDUCTORS =>
						et_canvas_board_conductors.delete_object (point);

					when NOUN_TRACK =>
						et_canvas_board_tracks.ripup (point);

					when NOUN_FREETRACK =>
						et_canvas_board_conductors.delete_object (point);

					when NOUN_OUTLINE =>
						et_canvas_board_outline.delete_object (point);
						
					when NOUN_ASSY =>
						et_canvas_board_assy_doc.delete_object (point);
-- 
-- 					when NOUN_SILKSCREEN =>
-- 						et_canvas_board_silkscreen.delete_object (point);
-- 
-- 
-- 					when NOUN_NON_ELECTRICAL_DEVICE =>
-- 						delete_non_electrical_device (KEYBOARD, point);
-- 
-- 					when NOUN_VIA =>
-- 						delete_via (KEYBOARD, point);
						
					when others => null;
				end case;		


			-- If page down pressed, then the operator is clarifying:
			when key_clarify =>

				-- CS
				case noun is
					when NOUN_ASSY =>
						if clarification_pending then
							et_canvas_board_assy_doc.clarify_object;
						end if;
-- 
-- 					when NOUN_SILKSCREEN =>
-- 						if clarification_pending then
-- 							et_canvas_board_silkscreen.select_object;
-- 						end if;
-- 					
-- 					when NOUN_NON_ELECTRICAL_DEVICE =>
-- 						if clarification_pending then
-- 							select_non_electrical_device;
-- 						end if;
-- 
-- 					when NOUN_VIA =>
-- 						if clarification_pending then
-- 							select_via;
-- 						end if;

					when NOUN_OUTLINE =>
						if clarification_pending then
							et_canvas_board_outline.select_segment;
						end if;

					when NOUN_CONDUCTORS =>
						if clarification_pending then
							et_canvas_board_conductors.clarify_object;
						end if;
						
					when NOUN_TRACK =>
						if clarification_pending then
							et_canvas_board_tracks.select_track;
						end if;

					when NOUN_FREETRACK =>
						if clarification_pending then
							et_canvas_board_conductors.clarify_object;
						end if;

						
					when others => null;							
				end case;

				
			when others => status_noun_invalid;
		end case;
	end delete;

	
	
	procedure fill is begin
		case key is
			when key_noun_zone =>
				noun := NOUN_ZONE;
				fill_zones (active_module, NORMAL, log_threshold + 1);

				set_status ("conductor zones filled");
				
			when others => status_noun_invalid;
		end case;
	end fill;


	
	procedure flip is 
		use et_canvas_board_devices;
	begin
		case key is
			when key_noun_device =>
				noun := NOUN_DEVICE;
				set_status (status_flip_device);

			when key_noun_non_electrical_device =>
				noun := NOUN_NON_ELECTRICAL_DEVICE;
				set_status (status_flip_device);

				
			-- If space pressed then the operator wishes to operate by keyboard:
			when key_space =>		
				case noun is
					when NOUN_DEVICE =>				
						flip_electrical_device (KEYBOARD, point);

					when NOUN_NON_ELECTRICAL_DEVICE =>
						flip_non_electrical_device (KEYBOARD, point);
						
					when others => null;
				end case;		


			-- If page down pressed, then the operator is clarifying:
			when key_clarify =>
				case noun is
					when NOUN_DEVICE =>
						if clarification_pending then
							select_electrical_device;
						end if;
    
					when NOUN_NON_ELECTRICAL_DEVICE =>
						if clarification_pending then
							select_non_electrical_device;
						end if;
    
					when others => null;							
				end case;
				
			when others => status_noun_invalid;
		end case;
	end flip;

	
	
	procedure move is begin
		case key is
			when key_noun_assy =>
				noun := NOUN_ASSY;
				set_status (et_canvas_board_assy_doc.status_move_object);

			when key_noun_silkscreen =>
				noun := NOUN_SILKSCREEN;
				set_status (et_canvas_board_silkscreen.status_move_object);

			when key_noun_track =>
				noun := NOUN_TRACK;
				set_status (et_canvas_board_tracks.status_move_track);

			when key_noun_conductors =>
				noun := NOUN_CONDUCTORS;
				set_status (et_canvas_board_conductors.status_move_object);

			when key_noun_freetrack =>
				noun := NOUN_FREETRACK;
				set_status (et_canvas_board_conductors.status_move_object);
				
			when key_noun_device =>
				noun := NOUN_DEVICE;
				set_status (et_canvas_board_devices.status_move_device);

			when key_noun_non_electrical_device =>
				noun := NOUN_NON_ELECTRICAL_DEVICE;
				set_status (et_canvas_board_devices.status_move_device);

			when key_noun_outline =>
				noun := NOUN_OUTLINE;
				set_status (et_canvas_board_outline.status_move_object);
				
			when key_noun_via =>
				noun := NOUN_VIA;
				set_status (et_canvas_board_vias.status_move_via);
		
			when key_noun_text =>
				noun := NOUN_TEXT;
				set_status (et_canvas_board_texts.status_move_text);


				
			-- If space pressed then the operator wishes to operate by keyboard:
			when key_space =>	
				case noun is
					when NOUN_ASSY =>
						et_canvas_board_assy_doc.move_object (KEYBOARD, point);

					when NOUN_SILKSCREEN =>
						et_canvas_board_silkscreen.move_object (KEYBOARD, point);

					when NOUN_CONDUCTORS =>
						et_canvas_board_conductors.move_object (KEYBOARD, point);
						
					when NOUN_TRACK =>
						et_canvas_board_tracks.move_track (KEYBOARD, point);

					when NOUN_FREETRACK =>
						et_canvas_board_conductors.move_object (KEYBOARD, point);

					when NOUN_DEVICE =>		
						et_canvas_board_devices.move_electrical_device (KEYBOARD, point);
						
					when NOUN_NON_ELECTRICAL_DEVICE =>
						et_canvas_board_devices.move_non_electrical_device (KEYBOARD, point);

					when NOUN_OUTLINE =>
						et_canvas_board_outline.move_object (KEYBOARD, point);
						
					when NOUN_TEXT =>
						et_canvas_board_texts.move_text (KEYBOARD, point);
						
					when NOUN_VIA =>
						et_canvas_board_vias.move_via (KEYBOARD, point);

					when others => null;
				end case;		


			-- If page down pressed, then the operator is clarifying:
			when key_clarify =>
				case noun is
					-- when NOUN_NAME => 
					-- 	if clarification_pending then
					-- 		et_canvas_board_devices.clarify_placeholder;
					-- 	end if;
     -- 
					-- when NOUN_PURPOSE => 
					-- 	if clarification_pending then
					-- 		clarify_placeholder;
					-- 	end if;

					when NOUN_ASSY =>
						if clarification_pending then
							et_canvas_board_assy_doc.clarify_object;
						end if;

					when NOUN_SILKSCREEN =>
						if clarification_pending then
							et_canvas_board_silkscreen.select_object;
						end if;

					when NOUN_CONDUCTORS =>
						if clarification_pending then
							et_canvas_board_conductors.clarify_object;
						end if;
						
					when NOUN_TRACK =>
						if clarification_pending then
							et_canvas_board_tracks.select_track;
						end if;

					when NOUN_FREETRACK =>
						if clarification_pending then
							et_canvas_board_conductors.clarify_object;
						end if;
						
					when NOUN_DEVICE =>
						if clarification_pending then
							et_canvas_board_devices.select_electrical_device;
						end if;

					when NOUN_NON_ELECTRICAL_DEVICE =>
						if clarification_pending then
							et_canvas_board_devices.select_non_electrical_device;
						end if;

					when NOUN_OUTLINE =>
						if clarification_pending then
							et_canvas_board_outline.select_segment;
						end if;
						
					when NOUN_TEXT =>
						if clarification_pending then
							et_canvas_board_texts.select_text;
						end if;
						
					when NOUN_VIA =>
						if clarification_pending then
							et_canvas_board_vias.select_via;
						end if;
						
					-- when NOUN_VALUE => 
					-- 	if clarification_pending then
					-- 		clarify_placeholder;
					-- 	end if;
						
					when others => null;							
				end case;
				
			when others => status_noun_invalid;
		end case;
	end move;


	
	procedure draw is 
		use pac_path_and_bend;
		use et_canvas_board_lines;
		use et_canvas_board_outline;
		use et_canvas_board_zone;
		use et_canvas_board_preliminary_object;
	begin
		case key is
			when key_noun_line =>
				noun := NOUN_LINE;
				reset_preliminary_object;				
				show_line_properties;
				set_status (status_draw_line);

			when key_noun_outline =>
				noun := NOUN_OUTLINE;
				reset_object;				
				set_status (status_draw_outline);

				
			when key_noun_zone =>
				noun := NOUN_ZONE;
				reset_preliminary_object;
				show_zone_properties;
				set_status (status_draw_zone);
				

			-- If space pressed, then the operator wishes to operate via keyboard:	
			when key_space =>
				case noun is
					when NOUN_LINE =>
						et_canvas_board_lines.make_path (KEYBOARD, point);

					when NOUN_OUTLINE =>
						et_canvas_board_outline.make_path (KEYBOARD, point);
						
					when NOUN_ZONE =>
						et_canvas_board_zone.make_path (KEYBOARD, point);

						
					when others => null;
				end case;

			-- If B pressed, then a bend style is being selected.
			-- this affects only certain modes and is ignored otherwise:
			when key_bend_style =>
				case noun is
					when NOUN_LINE =>
						next_bend_style (live_path);

					when NOUN_OUTLINE =>
						next_bend_style (live_path);
						
					when NOUN_ZONE =>
						next_bend_style (live_path);
						
					when others => null;
				end case;
				
			when others => status_noun_invalid;
		end case;
	end draw;
		

	
	procedure place is 
		use et_canvas_board_texts;
		use et_canvas_board_vias;
	begin
		case key is
			when key_noun_text =>
				noun := NOUN_TEXT;
				show_text_properties;
				set_status (status_place_text);

				
			when key_noun_via =>
				noun := NOUN_VIA;
				show_via_properties;				

				
			-- If space pressed, then the operator wishes to operate via keyboard:	
			when key_space =>
				case noun is
					when NOUN_TEXT =>
						place_text (point);

					when NOUN_VIA =>
						place_via (point);
						
					when others => null;
				end case;
				
			when others => status_noun_invalid;
		end case;
	end place;


	
	procedure rotate is begin
		case key is
			when key_noun_device =>
				noun := NOUN_DEVICE;
				-- CS set_status (status_rotate_device);

			when key_noun_non_electrical_device =>
				noun := NOUN_NON_ELECTRICAL_DEVICE;
				-- CS set_status (status_rotate_device);

				
			-- If space pressed then the operator wishes to operate
			-- by keyboard:
			when key_space =>		
				null;
				-- CS
-- 				case noun is
-- 					when NOUN_DEVICE =>
-- 						rotate_electrical_device (KEYBOARD, point);
-- 
-- 					when NOUN_NON_ELECTRICAL_DEVICE =>							
-- 						rotate_non_electrical_device (KEYBOARD, point);
-- 						
-- 					when others => null;
-- 				end case;		


			-- If page down pressed, then the operator is clarifying:
			when key_clarify =>
				null;
				-- CS
-- 				case noun is
-- 					when NOUN_DEVICE =>
-- 						if clarification_pending then
-- 							select_electrical_device;
-- 						end if;
-- 
-- 					when NOUN_NON_ELECTRICAL_DEVICE =>
-- 						if clarification_pending then
-- 							select_non_electrical_device;
-- 						end if;
-- 						
-- 					when others => null;							
-- 				end case;
				
			when others => status_noun_invalid;
		end case;
	end rotate;


	
	procedure route is 
		use pac_path_and_bend;
		use et_canvas_board_tracks;
	begin
		case key is
			when key_noun_net =>
				noun := NOUN_NET;

				show_track_properties;
				set_status (status_draw_track);


			-- If space pressed, then the operator wishes to operate via keyboard.
			when key_space =>
				case noun is
					when NOUN_NET =>
						et_canvas_board_tracks.make_path (KEYBOARD, point);
						
					when others => null;
				end case;

				
			-- If "m" pressed, then a snap mode is being selected.
			when key_mode =>
				case noun is
					when NOUN_NET =>
						null;
						-- CS next_snap_mode;
						
					when others => null;
				end case;

				
			-- If "b" pressed, then a bend style is being selected.
			when key_bend_style =>
				case noun is
					when NOUN_NET =>
						next_bend_style (live_path);
						
					when others => null;
				end case;

				
			-- If page down pressed, then the operator is clarifying:
			when key_clarify =>
				case noun is
					when NOUN_NET =>
						if clarification_pending then
							select_airwire;
						end if;
						
					when others => null;							
				end case;

				
			when others => status_noun_invalid;
		end case;
	end route;

	
	
	procedure update is 
		use et_board_ops.ratsnest;
		use et_ratsnest;
	begin
		case key is
			when key_noun_ratsnest =>
				noun := NOUN_RATSNEST;
				update_ratsnest (active_module, log_threshold + 1);

				-- CS set_status (status_ratsnest_updated);
				
			when others => status_noun_invalid;
		end case;
	end update;


	
begin -- key_pressed

	log (text => "key_pressed (board) ", -- CS which key ?
		 level => log_threshold);

	
	--put_line ("board: evaluating other key ...");
-- 		put_line (gdk_modifier_type'image (key_ctrl));

	case key is
			
		when others =>
			
			-- If the command is waiting for finalization, usually by pressing
			-- the space key, AND the primary tool is the keyboard, then
			-- we call the corresponding subprogram right away here:
			if single_cmd_status.finalization_pending and primary_tool = KEYBOARD then
				case verb is
					when VERB_PLACE		=> place;
					when others			=> null;
				end case;
		
			else
			-- Evaluate the verb and noun (as typed on the keyboard):
				
				case expect_entry is
					when EXP_VERB =>
						--put_line ("VERB entered");

						-- Next we expect an entry to select a noun.
						-- If the verb entry is invalid then expect_entry
						-- will be overwritten by EXP_VERB so that the
						-- operator is required to re-enter a valid verb.
						expect_entry := EXP_NOUN;

						-- As long as no valid noun has been entered
						-- display the default noun:
						noun := noun_default;
						
						case key is
							when key_verb_clear =>
								verb := VERB_CLEAR;
								status_enter_noun;

							when key_verb_delete =>
								verb := VERB_DELETE;
								status_enter_noun;

							when key_verb_draw =>
								verb := VERB_DRAW;
								status_enter_noun;

							when key_verb_fill =>
								verb := VERB_FILL;
								status_enter_noun;

							when key_verb_flip =>
								verb := VERB_FLIP;
								status_enter_noun;
								
							when key_verb_move =>
								verb := VERB_MOVE;
								status_enter_noun;
								
							when key_verb_place =>
								verb := VERB_PLACE;
								status_enter_noun;

							when key_verb_rotate =>
								verb := VERB_ROTATE;
								status_enter_noun;
								
							when key_verb_route =>
								verb := VERB_ROUTE;
								status_enter_noun;

							when key_verb_update =>
								verb := VERB_UPDATE;
								status_enter_noun;

								
							when others =>
								--put_line ("other key pressed " & gdk_key_type'image (key));

								-- If invalid verb entered, overwrite expect_entry by EXP_VERB
								-- and show error in status bar:
								expect_entry := EXP_VERB;
								status_verb_invalid;
						end case;

						---- Clean up: ???
						---- Some toolbars or property bars must be removed:
						--et_canvas_board_texts.remove_text_properties; -- after placing text

						
					when EXP_NOUN =>
						--put_line ("NOUN entered");

						case verb is
							when VERB_CLEAR		=> clear;
							when VERB_DELETE	=> delete;
							when VERB_DRAW		=> draw;
							when VERB_FILL		=> fill;
							when VERB_FLIP		=> flip;
							when VERB_MOVE		=> move;
							when VERB_PLACE		=> place;
							when VERB_ROTATE	=> rotate;
							when VERB_ROUTE		=> route;
							when VERB_UPDATE	=> update;
							when others => null; -- CS
						end case;
						
				end case;

			end if;
	end case;

	
	redraw;	
	-- CS use redraw_board if only board affected
	-- CS redraw after "enter" pressed

	
	update_mode_display;

	-- CS
	-- exception when event: others =>
		-- CS set_status (exception_message (event));
		--reset_selections;
		-- redraw;
		-- update_mode_display;
	
end key_pressed;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
