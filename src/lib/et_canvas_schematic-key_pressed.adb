------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        CANVAS FOR SCHEMATIC                              --
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


separate (et_canvas_schematic)

procedure key_pressed (
	self	: not null access type_view;
	key		: in gdk_key_type) 
is
	use gdk.types;
	use gdk.types.keysyms;

	use et_modes;

	
	procedure delete is begin
		case key is
			-- EVALUATE KEY FOR NOUN:
			when GDK_LC_l =>
				noun := NOUN_LABEL;
				set_status (et_canvas_schematic_nets.status_delete_label);
			
			when GDK_LC_u =>
				noun := NOUN_UNIT;					
				set_status (et_canvas_schematic_units.status_delete);
				
			when GDK_LC_n =>
				noun := NOUN_NET;					
				set_status (et_canvas_schematic_nets.status_delete);

			-- If space pressed, then the operator wishes to operate via keyboard:	
			when GDK_Space =>
				case noun is
					when NOUN_LABEL =>
						if not clarification_pending then
							delete_label (cursor_main.position);
						else
							delete_selected_label;
						end if;
					
					when NOUN_NET => 
						if not clarification_pending then
							delete_net_segment (cursor_main.position);
						else
							delete_selected_net_segment;
						end if;

					when NOUN_UNIT =>
						if not clarification_pending then
							delete_unit (cursor_main.position);
						else
							delete_selected_unit;
						end if;
						
					when others => null;							
				end case;

			-- If page down pressed, then the operator is clarifying:
			when GDK_page_down =>
				case noun is
					when NOUN_LABEL => 
						if clarification_pending then
							clarify_label;
						end if;

					when NOUN_NET => 
						if clarification_pending then
							clarify_net_segment;
						end if;

					when NOUN_UNIT =>
						if clarification_pending then
							clarify_unit;
						end if;

					when others =>
						null;
						
				end case;
				
			when others => status_noun_invalid;
		end case;
	end delete;

	
	procedure drag is begin
		case key is
			-- EVALUATE KEY FOR NOUN:
			when GDK_LC_n =>
				noun := NOUN_NET;
				set_status (et_canvas_schematic_nets.status_drag);

				-- When dragging net segments, we enforce the default grid
				-- and snap the cursor position to the default grid:
				self.reset_grid_and_cursor;
				
			when GDK_LC_u =>
				noun := NOUN_UNIT;
				set_status (et_canvas_schematic_units.status_drag);

				-- When dragging units, we enforce the default grid
				-- and snap the cursor position to the default grid:
				self.reset_grid_and_cursor;

			-- If space pressed then the operator wishes to operate
			-- by keyboard:
			when GDK_Space =>
	
				case noun is
					when NOUN_NET =>
						-- When dragging net segments, we enforce the default grid
						-- and snap the cursor position to the default grid:
						self.reset_grid_and_cursor;
						drag_segment (KEYBOARD, cursor_main.position);						

					when NOUN_UNIT =>
						-- When dragging units, we enforce the default grid
						-- and snap the cursor position to the default grid:
						self.reset_grid_and_cursor;
						drag_unit (KEYBOARD, cursor_main.position);

					when others => null;
						
				end case;

			-- If page down pressed, then the operator is clarifying:
			when GDK_page_down =>
				case noun is
					when NOUN_UNIT =>
						if clarification_pending then
							clarify_unit;
						end if;

					when NOUN_NET => 
						if clarification_pending then
							clarify_net_segment;
						end if;

					when others => null;
						
				end case;
				
			when others => status_noun_invalid;
		end case;
	end drag;

	
	procedure draw is begin
		case key is
			-- EVALUATE KEY FOR NOUN:
			when GDK_LC_n =>
				noun := NOUN_NET;
				
				set_status (status_draw_net);

				-- we start a new route:
				reset_net_route;

				-- When drawing net segments, we enforce the default grid
				-- and snap the cursor position to the default grid:
				self.reset_grid_and_cursor;

				
			-- If space pressed, then the operator wishes to operate via keyboard:
			when GDK_Space =>
				case noun is
					when NOUN_NET =>
						-- When drawing net segments, we enforce the default grid
						-- and snap the cursor position to the default grid:
						self.reset_grid_and_cursor;

						make_net_route (KEYBOARD, cursor_main.position);	
						
					when others => null;
				end case;

				
			-- If B pressed, then a bend style is being selected.
			-- this affects only certain modes and is ignored otherwise:
			when GDK_LC_b =>
				case noun is
					when NOUN_NET =>
						next_bend_style (preliminary_segment.path);
						
					when others => null;
						
				end case;
				
			when others => status_noun_invalid;
		end case;
	end draw;

	
	procedure move is begin
		case key is
			-- EVALUATE KEY FOR NOUN:
-- 				when GDK_LC_n =>
-- 					noun := NOUN_NET;

				-- CS
				--set_status (et_canvas_schematic_nets.status_move);

			when GDK_LC_l =>
				noun := NOUN_LABEL;
				set_status (et_canvas_schematic_nets.status_move_label);

			when GDK_LC_n =>
				noun := NOUN_NAME;					
				set_status (et_canvas_schematic_units.status_move_placeholder);

			when GDK_LC_p =>
				noun := NOUN_PURPOSE;					
				set_status (et_canvas_schematic_units.status_move_placeholder);
				
			when GDK_LC_u =>
				noun := NOUN_UNIT;
				set_status (et_canvas_schematic_units.status_move);

				-- When moving units, we enforce the default grid
				-- and snap the cursor position to the default grid:
				self.reset_grid_and_cursor;
				
			when GDK_LC_v =>
				noun := NOUN_VALUE;					
				set_status (et_canvas_schematic_units.status_move_placeholder);

				
			-- If space pressed then the operator wishes to operate
			-- by keyboard:
			when GDK_Space =>
	
				case noun is
-- CS
-- 						when NOUN_NET =>
-- 							if not segment.being_moved then
-- 								
-- 								-- Set the tool being used for moving the segment:
-- 								segment.tool := KEYBOARD;
-- 								
-- 								if not clarification_pending then
-- 									find_segments (cursor_main.position);
-- 								else
-- 									segment.being_moved := true;
-- 									reset_request_clarification;
-- 								end if;
-- 								
-- 							else
-- 								-- Finally assign the cursor position to the
-- 								-- currently selected segment:
-- 								et_canvas_schematic_nets.finalize_move (
-- 									destination		=> cursor_main.position,
-- 									log_threshold	=> log_threshold + 1);
-- 
-- 							end if;

					when NOUN_LABEL =>
						move_label (KEYBOARD, cursor_main.position);

						
					when NOUN_NAME =>
						move_placeholder (KEYBOARD, cursor_main.position, NAME);
						
					when NOUN_PURPOSE =>
						move_placeholder (KEYBOARD, cursor_main.position, PURPOSE);

					when NOUN_VALUE =>
						move_placeholder (KEYBOARD, cursor_main.position, VALUE);
						
						
					when NOUN_UNIT =>
						-- When moving units, we enforce the default grid
						-- and snap the cursor position to the default grid:
						self.reset_grid_and_cursor;
						move_unit (KEYBOARD, cursor_main.position);

					when others => null;
						
				end case;

			-- If page down pressed, then the operator is clarifying:
			when GDK_page_down =>

				case noun is

					when NOUN_LABEL => 
						if clarification_pending then
							clarify_label;
						end if;
					
					when NOUN_NAME => 
						if clarification_pending then
							clarify_placeholder;
						end if;

					when NOUN_PURPOSE => 
						if clarification_pending then
							clarify_placeholder;
						end if;

					when NOUN_UNIT =>
						if clarification_pending then
							clarify_unit;
						end if;
						
					when NOUN_VALUE => 
						if clarification_pending then
							clarify_placeholder;
						end if;

						
					when others => null;
						
				end case;
				
			when others => status_noun_invalid;
		end case;
	end move;

	
	procedure place is 
		use et_schematic;
	begin
		case key is
			-- EVALUATE KEY FOR NOUN:
			when GDK_LC_l =>
				noun := NOUN_LABEL;
				label.appearance := SIMPLE;
				set_status (et_canvas_schematic_nets.status_place_label_simple);

				-- For placing simple net labels, the fine grid is required:
				self.set_grid (FINE);
				
			when GDK_L =>
				noun := NOUN_LABEL;
				label.appearance := TAG;
				set_status (et_canvas_schematic_nets.status_place_label_tag);

			-- If space pressed, then the operator wishes to operate via keyboard:	
			when GDK_Space =>
				case noun is
					when NOUN_LABEL =>
						place_label (KEYBOARD, cursor_main.position);
						
					when others => null;							
				end case;

			-- If page down pressed, then the operator is clarifying:
			when GDK_page_down =>
				case noun is

					when NOUN_LABEL => 
						if clarification_pending then
							clarify_net_segment;
						end if;

					when others => null;
						
				end case;

			when GDK_LC_r =>
				case noun is

					when NOUN_LABEL =>
						-- Rotate simple label:
						if label.ready then
							toggle_rotation (label.rotation_simple);
						end if;

					when others => null;
						
				end case;
				
			when others => status_noun_invalid;
		end case;
	end place;

	
	procedure rotate is begin
		case key is
			-- EVALUATE KEY FOR NOUN:
			when GDK_LC_n =>
				noun := NOUN_NAME;					
				set_status (et_canvas_schematic_units.status_rotate_placeholder);

			when GDK_LC_p =>
				noun := NOUN_PURPOSE;					
				set_status (et_canvas_schematic_units.status_rotate_placeholder);

				
			when GDK_LC_u =>
				noun := NOUN_UNIT;					
				set_status (et_canvas_schematic_units.status_rotate);

			when GDK_LC_v =>
				noun := NOUN_VALUE;					
				set_status (et_canvas_schematic_units.status_rotate_placeholder);


			-- If space pressed, then the operator wishes to operate via keyboard:	
			when GDK_Space =>
				case noun is
					when NOUN_NAME =>
						if not clarification_pending then
							rotate_placeholder (
								point		=> cursor_main.position,
								category	=> NAME);
						else
							rotate_selected_placeholder (NAME);
						end if;
						
					when NOUN_PURPOSE =>
						if not clarification_pending then
							rotate_placeholder (
								point		=> cursor_main.position,
								category	=> PURPOSE);
						else
							rotate_selected_placeholder (PURPOSE);
						end if;

					when NOUN_UNIT =>
						if not clarification_pending then
							rotate_unit (cursor_main.position);
						else
							rotate_selected_unit;
						end if;
						
					when NOUN_VALUE =>
						if not clarification_pending then
							rotate_placeholder (
								point		=> cursor_main.position,
								category	=> VALUE);
						else
							rotate_selected_placeholder (VALUE);
						end if;
						
					when others => null;
				end case;

			-- If page down pressed, then the operator is clarifying:
			when GDK_page_down =>
				case noun is
					when NOUN_NAME | NOUN_VALUE | NOUN_PURPOSE => 
						if clarification_pending then
							clarify_placeholder;
						end if;

					when NOUN_UNIT =>
						if clarification_pending then
							clarify_unit;
						end if;

					when others => null;
						
				end case;
				
			when others => status_noun_invalid;
		end case;
	end rotate;

	
	procedure add is 
		use pac_devices_lib;
	begin
		case key is
			-- EVALUATE KEY FOR NOUN:
			when GDK_LC_d =>
				noun := NOUN_DEVICE;					
				set_status (et_canvas_schematic_units.status_add);

				-- When adding units, we enforce the default grid
				-- and snap the cursor position to the default grid:
				self.reset_grid_and_cursor;
				
				-- open device model selection
				add_device; 
				
			-- If space pressed, then the operator wishes to operate via keyboard:	
			when GDK_Space =>
				case noun is

					when NOUN_DEVICE =>
						-- When adding units, we enforce the default grid
						-- and snap the cursor position to the default grid:
						self.reset_grid_and_cursor;

						-- If a unit has been selected already, then
						-- the number of "activate" actions must be counted.
						-- The "activate" action in this case is pressing the
						-- "space" key. After the first "activate" the tool
						-- for placing the unit is set. After the second "activate"
						-- the unit is placed at the current cursor position.
						-- If no unit has been selected yet, then the device
						-- model selection dialog opens.
						if unit_add.device /= pac_devices_lib.no_element then -- unit selected

							-- Set the tool being used for placing the unit:
							increment_activate_counter;
							
							case activate_counter is
								when 1 =>
									unit_add.tool := KEYBOARD;

								when 2 =>
									-- Finally place the unit at the current 
									-- cursor position:
									finalize_add_device (cursor_main.position);

								when others => null;
							end case;

						else -- no unit selected yet
							add_device; -- open device model selection
						end if;
						
					when others => null;
						
				end case;

				
			when others => null;
		end case;
	end add;

	
	procedure invoke is 
		use pac_devices_lib;
	begin
		case key is
			-- EVALUATE KEY FOR NOUN:
			when GDK_LC_u =>
				noun := NOUN_UNIT;					
				set_status (et_canvas_schematic_units.status_invoke);

			-- If space pressed, then the operator wishes to operate via keyboard:	
			when GDK_Space =>
				case noun is

					when NOUN_UNIT =>
						-- If no device has been selected already, then
						-- set the tool used for invoking.
						if unit_add.device = pac_devices_lib.no_element then

							unit_add.tool := KEYBOARD;

							if not clarification_pending then
								invoke_unit (cursor_main.position);
							else
								show_units;
							end if;

						else
							finalize_invoke (cursor_main.position, log_threshold + 1);
						end if;
						
					when others => null;
						
				end case;

			-- If page down pressed, then the operator is clarifying:
			when GDK_page_down =>
				case noun is

					when NOUN_UNIT => 
						if clarification_pending then
							clarify_unit;
						end if;
						
					when others => null;
						
				end case;
				
			when others => null;
		end case;
	end invoke;

	
	procedure set is begin
		case key is
			-- EVALUATE KEY FOR NOUN:
			when GDK_LC_p =>
				noun := NOUN_PARTCODE;
				set_status (et_canvas_schematic_units.status_set_partcode);

			when GDK_LC_u =>
				noun := NOUN_PURPOSE;
				set_status (et_canvas_schematic_units.status_set_purpose);
			
			when GDK_LC_v =>
				noun := NOUN_VALUE;					
				set_status (et_canvas_schematic_units.status_set_value);

			when GDK_LC_a =>
				noun := NOUN_VARIANT;
				set_status (et_canvas_schematic_units.status_set_variant);
				
			-- If space pressed, then the operator wishes to operate via keyboard:	
			when GDK_Space =>
				case noun is
					
					when NOUN_PARTCODE | NOUN_PURPOSE | NOUN_VALUE | NOUN_VARIANT =>
						if not clarification_pending then
							set_property (cursor_main.position);
						else
							set_property_selected_unit;
						end if;
						
					when others => null;
				end case;

			-- If page down pressed, then the operator is clarifying:
			when GDK_page_down =>
				case noun is
					when NOUN_PARTCODE | NOUN_PURPOSE | NOUN_VALUE | NOUN_VARIANT =>
						if clarification_pending then
							clarify_unit;
						end if;

					when others => null;							
				end case;
				
			when others => status_noun_invalid;
		end case;
		
	end set;

	
	procedure show is begin
		case key is
			-- EVALUATE KEY FOR NOUN:
			when GDK_LC_d =>
				noun := NOUN_DEVICE;
				set_status (et_canvas_schematic_units.status_show_device);

			when GDK_LC_n =>
				noun := NOUN_NET;
				set_status (et_canvas_schematic_nets.status_show_net);
				
			-- If space pressed, then the operator wishes to operate via keyboard:	
			when GDK_Space =>
				case noun is
					when NOUN_DEVICE =>
						if not clarification_pending then
							find_units_for_show (cursor_main.position);
						else
							show_properties_of_selected_device;
						end if;
						
					when NOUN_NET =>
						if not clarification_pending then
							find_segments (cursor_main.position);
						else
							show_properties_of_selected_net;
						end if;
						
					when others => null;
				end case;

			-- If page down pressed, then the operator is clarifying:
			when GDK_page_down =>
				case noun is
					when NOUN_DEVICE => 
						if clarification_pending then
							clarify_unit;
						end if;

					when NOUN_NET =>
						if clarification_pending then
							clarify_net_segment;
						end if;

					when others => null;
						
				end case;
				
			when others => status_noun_invalid;
		end case;
	end show;

	
	procedure rename is 
		use et_schematic_ops.nets;
	begin
		case key is
			-- EVALUATE KEY FOR NOUN:
			when GDK_LC_d =>
				noun := NOUN_DEVICE;
				set_status (et_canvas_schematic_units.status_rename);

			when GDK_LC_s => -- rename strand
				noun := NOUN_NET;
				net_rename.scope := STRAND;
				set_status (et_canvas_schematic_nets.status_rename_net_strand);
			
			when GDK_LC_n => -- rename all strands on current sheet
				noun := NOUN_NET;
				net_rename.scope := SHEET;
				set_status (et_canvas_schematic_nets.status_rename_net_sheet);

			when GDK_N => -- rename everywhere: all strands on all sheets
				noun := NOUN_NET;
				net_rename.scope := EVERYWHERE;
				set_status (et_canvas_schematic_nets.status_rename_net_everywhere);
				
			-- If space pressed, then the operator wishes to operate via keyboard:	
			when GDK_Space =>
				case noun is
					when NOUN_DEVICE =>
						if not clarification_pending then
							set_property (cursor_main.position);
						else
							set_property_selected_unit;
						end if;

					when NOUN_NET =>
						if not clarification_pending then
							find_segments (cursor_main.position);
						else
							et_canvas_schematic_nets.window_set_property;
						end if;
						
					when others => null;
				end case;

			-- If page down pressed, then the operator is clarifying:
			when GDK_page_down =>
				case noun is
					when NOUN_DEVICE =>
						if clarification_pending then
							clarify_unit;
						end if;

					when NOUN_NET =>
						if clarification_pending then
							clarify_net_segment;
						end if;
						
					when others => null;							
				end case;
				
			when others => status_noun_invalid;
		end case;

	end rename;

	
begin -- key_pressed
	
-- 		put_line ("schematic: evaluating other key ...");
-- 		put_line (gdk_modifier_type'image (key_ctrl));

	case key is
		when GDK_Escape =>
			expect_entry := expect_entry_default;
			reset_selections;
			status_enter_verb;			

		-- Advance to next sheet:
		when GDK_KP_Add =>
			current_active_sheet := current_active_sheet + 1;
			update_sheet_number_display;

		-- Advance to previous sheet:
		when GDK_KP_Subtract =>
			if current_active_sheet > sheet_default then
				current_active_sheet := current_active_sheet - 1;
				update_sheet_number_display;
			end if;

		when GDK_F11 =>
			previous_module;

		when GDK_F12 =>
			next_module;
			
		when others =>

			-- If an imcomplete command has been entered via console then it starts
			-- waiting for finalization. This can be done by pressing the SPACE key.
			-- Then we call the corresponding subprogram for the actual job right away here:
			
			--if single_cmd_status.finalization_pending and primary_tool = KEYBOARD then
			if single_cmd_status.finalization_pending then
			
				if key = GDK_Space then
						
					case verb is
						when VERB_DELETE	=> delete;
						when VERB_DRAG		=> drag;
						when VERB_DRAW		=> draw;
						when VERB_INVOKE	=> invoke;
						when VERB_MOVE		=> move;
						when VERB_PLACE		=> place;							
						when others			=> null;
					end case;

				end if;
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

						-- EVALUATE KEY FOR VERB:
						case key is
							when GDK_Delete =>
								verb := VERB_DELETE;
								status_enter_noun;

							when GDK_LC_a =>
								verb := VERB_ADD;
								status_enter_noun;
								
							when GDK_LC_g =>
								verb := VERB_DRAG;
								status_enter_noun;

							when GDK_LC_d =>
								verb := VERB_DRAW;
								status_enter_noun;

							when GDK_LC_h =>
								verb := VERB_SHOW;
								status_enter_noun;
								
							when GDK_LC_i =>
								verb := VERB_INVOKE;
								status_enter_noun;
								
							when GDK_LC_m =>
								verb := VERB_MOVE;
								status_enter_noun;

							when GDK_LC_n =>
								verb := VERB_RENAME;
								status_enter_noun;
								
							when GDK_LC_p =>
								verb := VERB_PLACE;
								status_enter_noun;
								
							when GDK_LC_r =>
								verb := VERB_ROTATE;
								status_enter_noun;

							when GDK_LC_s =>
								verb := VERB_SET;
								status_enter_noun;
								
							when others =>
								--put_line ("other key pressed " & gdk_key_type'image (key));
								
								-- If invalid verb entered, overwrite expect_entry by EXP_VERB
								-- and show error in status bar:
								expect_entry := EXP_VERB;
								status_verb_invalid;
						end case;


					when EXP_NOUN =>
						--put_line ("NOUN entered");

						case verb is
							when VERB_ADD		=> add;
							when VERB_DELETE	=> delete;
							when VERB_DRAG		=> drag;
							when VERB_DRAW		=> draw;
							when VERB_INVOKE	=> invoke;
							when VERB_MOVE		=> move;
							when VERB_PLACE		=> place;
							when VERB_RENAME	=> rename;
							when VERB_ROTATE	=> rotate;
							when VERB_SET		=> set;
							when VERB_SHOW		=> show;
							when others => null; -- CS
						end case;
						
				end case;

			end if;		
	end case;

	redraw;
	-- CS use redraw_schematic if only schematic affected
	-- CS redraw after "enter" pressed
	
	update_mode_display (canvas);

	
	exception when event: others =>
		set_status (exception_message (event));

		reset_selections;
	
		redraw;
		update_mode_display (canvas);
	
end key_pressed;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
