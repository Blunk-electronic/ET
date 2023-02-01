------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           CANVAS FOR BOARD                               --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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

separate (et_canvas_board)

procedure key_pressed (
	self	: not null access type_view;
	key		: in gdk_key_type) 
is
	use gdk.types;
	use gdk.types.keysyms;

	use et_modes;

	point : type_point renames cursor_main.position;
	

	procedure delete is begin
		case key is
			when GDK_LC_n =>
				noun := NOUN_NON_ELECTRICAL_DEVICE;
				set_status (status_delete);

			-- NOTE: Electrical devices can be deleted in
			-- schematic only !


			when GDK_LC_v =>
				noun := NOUN_VIA;
				set_status (status_delete_via);

				
				
			-- If space pressed then the operator wishes to operate by keyboard:
			when GDK_Space =>		
				case noun is
					when NOUN_NON_ELECTRICAL_DEVICE =>
						delete_non_electrical_device (KEYBOARD, point);

					when NOUN_VIA =>
						delete_via (KEYBOARD, point);
						
					when others => null;
				end case;		


			-- If page down pressed, then the operator is clarifying:
			when GDK_page_down =>
				case noun is
					when NOUN_NON_ELECTRICAL_DEVICE =>
						if clarification_pending then
							clarify_non_electrical_device;
						end if;

					when NOUN_VIA =>
						if clarification_pending then
							select_via;
						end if;

					when others => null;							
				end case;

				
			when others => status_noun_invalid;
		end case;
	end delete;

	
	procedure fill is begin
		case key is
			when GDK_LC_z =>
				noun := NOUN_ZONE;
				fill_fill_zones (current_active_module, NORMAL, log_threshold + 1);

				set_status ("zones filled");
				
			when others => status_noun_invalid;
		end case;
	end fill;


	procedure flip is begin
		case key is
			when GDK_LC_d =>
				noun := NOUN_DEVICE;
				set_status (status_flip);

			when GDK_LC_n =>
				noun := NOUN_NON_ELECTRICAL_DEVICE;
				set_status (status_flip);

				
			-- If space pressed then the operator wishes to operate by keyboard:
			when GDK_Space =>		
				case noun is
					when NOUN_DEVICE =>				
						flip_electrical_device (KEYBOARD, point);

					when NOUN_NON_ELECTRICAL_DEVICE =>
						flip_non_electrical_device (KEYBOARD, point);
						
					when others => null;
				end case;		


			-- If page down pressed, then the operator is clarifying:
			when GDK_page_down =>
				case noun is
					when NOUN_DEVICE =>
						if clarification_pending then
							clarify_electrical_device;
						end if;

					when NOUN_NON_ELECTRICAL_DEVICE =>
						if clarification_pending then
							clarify_non_electrical_device;
						end if;

					when others => null;							
				end case;
				
			when others => status_noun_invalid;
		end case;
	end flip;

	
	procedure move is begin
		case key is
			when GDK_LC_d =>
				noun := NOUN_DEVICE;
				set_status (status_move);

			when GDK_LC_n =>
				noun := NOUN_NON_ELECTRICAL_DEVICE;
				set_status (status_move);

			when GDK_LC_v =>
				noun := NOUN_VIA;
				set_status (status_move_via);
		
				
			-- If space pressed then the operator wishes to operate by keyboard:
			when GDK_Space =>		
				case noun is
					when NOUN_DEVICE =>		
						move_electrical_device (KEYBOARD, point);
						
					when NOUN_NON_ELECTRICAL_DEVICE =>
						move_non_electrical_device (KEYBOARD, point);


					when NOUN_VIA =>
						move_via (KEYBOARD, point);
						
					when others => null;
				end case;		


			-- If page down pressed, then the operator is clarifying:
			when GDK_page_down =>
				case noun is

					--when NOUN_NAME => 
						--if clarification_pending then
							--clarify_placeholder;
						--end if;

					--when NOUN_PURPOSE => 
						--if clarification_pending then
							--clarify_placeholder;
						--end if;

					when NOUN_DEVICE =>
						if clarification_pending then
							clarify_electrical_device;
						end if;

					when NOUN_NON_ELECTRICAL_DEVICE =>
						if clarification_pending then
							clarify_non_electrical_device;
						end if;

					when NOUN_VIA =>
						if clarification_pending then
							select_via;
						end if;

						
					--when NOUN_VALUE => 
						--if clarification_pending then
							--clarify_placeholder;
						--end if;
						
					when others => null;							
				end case;
				
			when others => status_noun_invalid;
		end case;
	end move;

	
	procedure place is begin
		case key is
			-- EVALUATE KEY FOR NOUN:
			when GDK_LC_t =>
				noun := NOUN_TEXT;
				show_text_properties;
				set_status (status_place_text);

			when GDK_LC_v =>
				noun := NOUN_VIA;
				show_via_properties;
				set_status (status_place_via);
				
			-- If space pressed, then the operator wishes to operate via keyboard:	
			when GDK_Space =>
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
			when GDK_LC_d =>
				noun := NOUN_DEVICE;
				set_status (status_rotate);

			when GDK_LC_n =>
				noun := NOUN_NON_ELECTRICAL_DEVICE;
				set_status (status_rotate);

				
			-- If space pressed then the operator wishes to operate
			-- by keyboard:
			when GDK_Space =>		
				case noun is
					when NOUN_DEVICE =>
						rotate_electrical_device (KEYBOARD, point);

					when NOUN_NON_ELECTRICAL_DEVICE =>							
						rotate_non_electrical_device (KEYBOARD, point);
						
					when others => null;
				end case;		


			-- If page down pressed, then the operator is clarifying:
			when GDK_page_down =>
				case noun is
					when NOUN_DEVICE =>
						if clarification_pending then
							clarify_electrical_device;
						end if;

					when NOUN_NON_ELECTRICAL_DEVICE =>
						if clarification_pending then
							clarify_non_electrical_device;
						end if;
						
					when others => null;							
				end case;
				
			when others => status_noun_invalid;
		end case;
	end rotate;

	
	procedure update is 
		use et_ratsnest;
	begin
		case key is
			when GDK_LC_r =>
				noun := NOUN_RATSNEST;
				update_ratsnest (current_active_module, log_threshold + 1);

				set_status (status_ratsnest_updated);
				
			when others => status_noun_invalid;
		end case;
	end update;

	
begin -- key_pressed
	
	--put_line ("board: evaluating other key ...");
-- 		put_line (gdk_modifier_type'image (key_ctrl));

	case key is
		when GDK_Escape =>
			expect_entry := expect_entry_default;
			
			-- Verb and noun emain as they are
			-- so that the mode is unchanged.
			
			reset_request_clarification;
			status_enter_verb;

			reset_text_place; -- after placing a text
			reset_preliminary_via; -- after placing a via
			reset_preliminary_electrical_device; -- after moving, rotating, flipping a device
			reset_preliminary_non_electrical_device;
			
		when GDK_F11 =>
			et_canvas_schematic.previous_module;

		when GDK_F12 =>
			et_canvas_schematic.next_module;
			
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
							when GDK_Delete =>
								verb := VERB_DELETE;
								status_enter_noun;

							when GDK_LC_d =>
								verb := VERB_DRAW;
								status_enter_noun;

							when GDK_LC_f =>
								verb := VERB_FILL;
								status_enter_noun;

							when GDK_LC_l =>
								verb := VERB_FLIP;
								status_enter_noun;
								
							when GDK_LC_m =>
								verb := VERB_MOVE;
								status_enter_noun;
								
							when GDK_LC_p =>
								verb := VERB_PLACE;
								status_enter_noun;

							when GDK_LC_r =>
								verb := VERB_ROTATE;
								status_enter_noun;
								
							when GDK_LC_t =>
								verb := VERB_ROUTE;
								status_enter_noun;

							when GDK_LC_u =>
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
							when VERB_DELETE	=> delete;
							when VERB_FILL		=> fill;
							when VERB_FLIP		=> flip;
							when VERB_MOVE		=> move;
							when VERB_PLACE		=> place;
							when VERB_ROTATE	=> rotate;
							when VERB_UPDATE	=> update;
							when others => null; -- CS
						end case;
						
				end case;

			end if;
	end case;

	
	redraw;	
	-- CS use redraw_board if only board affected
	-- CS redraw after "enter" pressed

	
	update_mode_display (canvas);

	
	exception when event: others =>
		set_status (exception_message (event));

		--reset_selections;
	
		redraw;
		update_mode_display (canvas);
	
end key_pressed;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
