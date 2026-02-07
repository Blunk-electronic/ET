------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        CANVAS FOR SCHEMATIC                              --
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
-- <http://www.gnu.org/licenses/>.   
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

with et_device_placeholders;			use et_device_placeholders;
with et_schematic_verb_noun_keys;		use et_schematic_verb_noun_keys;
with et_schematic_ops_nets;


separate (et_canvas_schematic)

procedure key_pressed (
	key			: in gdk_key_type;
	key_shift	: in gdk_modifier_type)
is
	use gdk.types;
	use gdk.types.keysyms;

	use et_modes;
	use et_modes.schematic;
	use et_canvas_schematic_nets;
	use et_canvas_schematic_units;


	point : type_vector_model renames get_cursor_position;

	-- CS global variable for the tool KEYBOARD
	
	
	procedure delete is begin
		case key is
			-- EVALUATE KEY FOR NOUN:
			when key_noun_device =>
				noun := NOUN_DEVICE;					
				set_status (et_canvas_schematic_units.status_delete_device);

			when key_noun_connector =>
				noun := NOUN_NET_CONNECTOR;
				set_status (et_canvas_schematic_nets.status_delete_connector);

			when key_noun_label =>
				noun := NOUN_NET_LABEL;
				set_status (et_canvas_schematic_nets.status_delete_label);
				
			when key_noun_unit =>
				noun := NOUN_UNIT;					
				set_status (et_canvas_schematic_units.status_delete_unit);
				
			when key_noun_net_all_sheets =>
				noun := NOUN_NET;
				et_schematic_ops_nets.modify_net_on_all_sheets := true;
				set_status (et_canvas_schematic_nets.status_delete);

			when key_noun_net =>
				noun := NOUN_NET;					
				et_schematic_ops_nets.modify_net_on_all_sheets := false;
				set_status (et_canvas_schematic_nets.status_delete);
				
			when key_noun_strand =>
				noun := NOUN_STRAND;					
				set_status (et_canvas_schematic_nets.status_delete);
				
			when key_noun_segment =>
				noun := NOUN_SEGMENT;				
				set_status (et_canvas_schematic_nets.status_delete);


				
				
			-- If space pressed, then the operator wishes to operate via keyboard:	
			when key_space =>
				case noun is
					when NOUN_NET_CONNECTOR | NOUN_NET_LABEL | NOUN_NET | NOUN_STRAND | NOUN_SEGMENT => 
						et_canvas_schematic_nets.delete_object (point);
						
					when NOUN_DEVICE | NOUN_UNIT =>
						et_canvas_schematic_units.delete_object (point);
						
					when others => null;							
				end case;

				
			-- If page down pressed, then the operator is clarifying:
			when key_clarify =>
				case noun is
					when NOUN_NET_CONNECTOR | NOUN_NET_LABEL | NOUN_NET | NOUN_STRAND | NOUN_SEGMENT => 
						if clarification_pending then
							et_canvas_schematic_nets.clarify_object;
						end if;

					when NOUN_DEVICE | NOUN_UNIT =>
						if clarification_pending then
							et_canvas_schematic_units.clarify_object;
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
			when key_noun_segment =>
				noun := NOUN_SEGMENT;
				set_status (et_canvas_schematic_nets.status_drag);

				-- When dragging net segments, we enforce the default grid
				-- and snap the cursor position to the default grid:
				reset_grid_and_cursor;

				
			when key_noun_unit =>
				noun := NOUN_UNIT;
				set_status (et_canvas_schematic_units.status_drag);

				-- When dragging units, we enforce the default grid
				-- and snap the cursor position to the default grid:
				reset_grid_and_cursor;

				
			-- If space pressed then the operator wishes to operate
			-- by keyboard:
			when key_space =>	
				case noun is
					when NOUN_SEGMENT =>
						-- When dragging net segments, we enforce the default grid
						-- and snap the cursor position to the default grid:
						reset_grid_and_cursor;
						et_canvas_schematic_nets.drag_object (KEYBOARD, get_cursor_position);						

					when NOUN_UNIT =>
						-- When dragging units, we enforce the default grid
						-- and snap the cursor position to the default grid:
						reset_grid_and_cursor;
						et_canvas_schematic_units.drag_object (KEYBOARD, get_cursor_position);

					when others => null;						
				end case;

				
			-- If page down pressed, then the operator is clarifying:
			when key_clarify =>
				case noun is
					when NOUN_UNIT =>
						if clarification_pending then
							et_canvas_schematic_units.clarify_object;
						end if;

					when NOUN_SEGMENT =>
						if clarification_pending then
							et_canvas_schematic_nets.clarify_object;
						end if;

					when others => null;						
				end case;

				
			when others => status_noun_invalid;
		end case;
	end drag;


	
	
	procedure draw is 
		use pac_path_and_bend;
	begin
		case key is
			-- EVALUATE KEY FOR NOUN:
			when key_noun_net =>
				noun := NOUN_NET;
				
				set_status (status_draw_net);

				-- we start a new route:
				reset_preliminary_segment;

				-- When drawing net segments, we enforce the default grid
				-- and snap the cursor position to the default grid:
				reset_grid_and_cursor;

				
			-- If space pressed, then the operator wishes to operate via keyboard:
			when key_space =>
				case noun is
					when NOUN_NET =>
						-- When drawing net segments, we enforce the default grid
						-- and snap the cursor position to the default grid:
						reset_grid_and_cursor;

						make_path (KEYBOARD, get_cursor_position);	
						
					when others => null;
				end case;

				
			-- If B pressed, then a bend style is being selected.
			-- this affects only certain modes and is ignored otherwise:
			when key_bend_style =>
				case noun is
					when NOUN_NET =>
						next_bend_style (live_path);
						
					when others => null;
						
				end case;
				
			when others => status_noun_invalid;
		end case;
	end draw;




	

	procedure mirror is begin
		case key is
			-- EVALUATE KEY FOR NOUN:
			when key_noun_unit =>
				noun := NOUN_UNIT;
				-- CS
				null;
				-- set_status (et_canvas_schematic_units.status_move_placeholder);

				
			-- If space pressed then the operator wishes to operate
			-- by keyboard:
			when key_space =>	
				case noun is
					when NOUN_UNIT =>
						null;
						-- CS
						-- et_canvas_schematic_units.move_object (KEYBOARD, get_cursor_position);

					when others => null;
				end case;

				
			-- If page down pressed, then the operator is clarifying:
			when key_clarify =>
				case noun is
					when NOUN_UNIT =>
						if clarification_pending then
							-- CS
							null;
							-- et_canvas_schematic_units.clarify_object;
						end if;
						
					when others => null;
				end case;
				
			when others => status_noun_invalid;
		end case;
	end mirror;



	
	
	procedure move is begin
		case key is
			-- EVALUATE KEY FOR NOUN:
			when key_noun_label =>
				noun := NOUN_NET_LABEL;
				set_status (et_canvas_schematic_nets.status_move_label);

			when key_noun_placeholder =>
				noun := NOUN_PLACEHOLDER;					
				set_status (et_canvas_schematic_units.status_move_placeholder);

			when key_noun_unit =>
				noun := NOUN_UNIT;
				set_status (et_canvas_schematic_units.status_move_placeholder);

				-- When moving units, we enforce the default grid
				-- and snap the cursor position to the default grid:
				reset_grid_and_cursor;

				
			-- If space pressed then the operator wishes to operate
			-- by keyboard:
			when key_space =>	
				case noun is
					when NOUN_NET_LABEL =>
						et_canvas_schematic_nets.move_object (KEYBOARD, get_cursor_position);
						
					when NOUN_PLACEHOLDER =>
						et_canvas_schematic_units.move_object (KEYBOARD, get_cursor_position);
						
					when NOUN_UNIT =>
						-- When moving units, we enforce the default grid
						-- and snap the cursor position to the default grid:
						reset_grid_and_cursor;
						et_canvas_schematic_units.move_object (KEYBOARD, get_cursor_position);

					when others => null;
				end case;

				
			-- If page down pressed, then the operator is clarifying:
			when key_clarify =>
				case noun is
					when NOUN_NET_LABEL => 
						if clarification_pending then
							et_canvas_schematic_nets.clarify_object;
						end if;
					
					when NOUN_PLACEHOLDER => 
						if clarification_pending then
							et_canvas_schematic_units.clarify_object;
						end if;

					when NOUN_UNIT =>
						if clarification_pending then
							et_canvas_schematic_units.clarify_object;
						end if;
						
					when others => null;
				end case;
				
			when others => status_noun_invalid;
		end case;
	end move;


	
	procedure place is begin
		case key is
			-- EVALUATE KEY FOR NOUN:
			when key_noun_label =>
				noun := NOUN_NET_LABEL;
				set_status (et_canvas_schematic_nets.status_place_label);

				-- For placing simple net labels, the fine grid is required:
				-- CS self.set_grid (FINE);
				
			when key_noun_connector =>
				noun := NOUN_NET_CONNECTOR;
				set_status (et_canvas_schematic_nets.status_place_connector);

			-- If space pressed, then the operator wishes to operate via keyboard:	
			when key_space =>
				case noun is
					when NOUN_NET_LABEL =>
						place_net_label (KEYBOARD, get_cursor_position);

					when NOUN_NET_CONNECTOR =>
						place_net_connector (KEYBOARD, get_cursor_position);
						
					when others => null;							
				end case;

			-- If page down pressed, then the operator is clarifying:
			when key_clarify =>
				case noun is

					when NOUN_NET_LABEL | NOUN_NET_CONNECTOR => 
						if clarification_pending then
							et_canvas_schematic_nets.clarify_object;
						end if;

					when others => null;
						
				end case;

			when GDK_LC_r =>
				case noun is

					when NOUN_NET_LABEL =>
						-- Rotate simple label:
						null; -- CS
						-- if label.ready then
						-- 	toggle_rotation (label.rotation_simple);
						-- end if;

					when others => null;
						
				end case;
				
			when others => status_noun_invalid;
		end case;
	end place;


	
	procedure rotate is begin
		case key is
			-- EVALUATE KEY FOR NOUN:
			when key_noun_placeholder =>
				noun := NOUN_PLACEHOLDER;					
				set_status (et_canvas_schematic_units.status_rotate_placeholder);

			when key_noun_unit =>
				noun := NOUN_UNIT;					
				set_status (et_canvas_schematic_units.status_rotate);


			-- If space pressed, then the operator wishes to operate via keyboard:	
			when key_space =>
				case noun is
					when NOUN_PURPOSE => -- CS remove
						et_canvas_schematic_units.rotate_object (point);

					when NOUN_UNIT =>
						et_canvas_schematic_units.rotate_object (point);
						
					when others => null;
				end case;

				
			-- If page down pressed, then the operator is clarifying:
			when key_clarify =>
				case noun is
					when NOUN_PLACEHOLDER => 
						if clarification_pending then
							et_canvas_schematic_units.clarify_object;
						end if;

					when NOUN_UNIT =>
						if clarification_pending then
							et_canvas_schematic_units.clarify_object;
						end if;

					when others => null;
						
				end case;
				
			when others => status_noun_invalid;
		end case;
	end rotate;



	
	procedure add is begin
		case key is
			-- EVALUATE KEY FOR NOUN:
			when key_noun_device =>
				noun := NOUN_DEVICE;					
				set_status (et_canvas_schematic_units.status_add);

				-- When adding units, we enforce the default grid
				-- and snap the cursor position to the default grid:
				reset_grid_and_cursor;
				
				-- open device model selection
				show_device_model_selection; 

				
			-- If space pressed, then the operator wishes to operate via keyboard:	
			when key_space =>
				case noun is

					when NOUN_DEVICE =>
						if unit_add.valid then
							-- When adding units, we enforce the default grid
							-- and snap the cursor position to the default grid:
							reset_grid_and_cursor;

							-- If a device model has been selected, then
							-- an unit will be dropped at the current 
							-- cursor position. The properties of the new device
							-- are taken from the preliminary unit_add:
							add_electrical_device (get_cursor_position);
						end if;
						
					when others => null;						
				end case;
				

			-- If the operator wants to rotate the unit
			-- being added, then add 90 degrees to the
			-- temporaily unit:
			when key_verb_rotate =>
				case noun is
					when NOUN_DEVICE =>
						put_line ("rotate");
						rotate_unit_add;

					when others => null;						
				end case;

				
			when others => null;
		end case;
	end add;



	procedure copy is begin
		case key is
			-- EVALUATE KEY FOR NOUN:
			when key_noun_device =>
				noun := NOUN_DEVICE;					
				set_status (et_canvas_schematic_units.status_copy);

				-- When copying units, we enforce the default grid
				-- and snap the cursor position to the default grid:
				reset_grid_and_cursor;
				
				
			-- If space pressed, then the operator wishes to operate via keyboard:	
			when key_space =>
				case noun is
					when NOUN_DEVICE =>
						et_canvas_schematic_units.copy_object (KEYBOARD, point);
						
					when others => null;						
				end case;


			-- If page down pressed, then the operator is clarifying:
			when key_clarify =>
				case noun is
					when NOUN_DEVICE =>
						if clarification_pending then
							et_canvas_schematic_units.clarify_object;
						end if;

					when others =>
						null;
						
				end case;

			-- If the operator wants to rotate the unit
			-- being added, then add 90 degrees to the
			-- temporily unit:
			when key_verb_rotate =>
				case noun is
					when NOUN_DEVICE =>
						rotate_unit_add;

					when others => null;						
				end case;
				
			when others => null;
		end case;
	end copy;


	
	
	procedure fetch is begin
		case key is
			-- EVALUATE KEY FOR NOUN:
			when key_noun_unit =>
				noun := NOUN_UNIT;					
				set_status (et_canvas_schematic_units.status_fetch);
				

			-- If space pressed, then the operator wishes to operate via keyboard:	
			when key_space =>
				case noun is
					when NOUN_UNIT =>
						et_canvas_schematic_units.fetch_unit (KEYBOARD, point);
						
					when others => null;						
				end case;
				

			-- If page down pressed, then the operator is clarifying:
			when key_clarify =>
				case noun is
					when NOUN_UNIT => 
						if clarification_pending then
							et_canvas_schematic_units.clarify_object;
						end if;

					when others => null;						
				end case;

				
			-- If the operator wants to rotate the unit
			-- being fetched, then add 90 degrees to the
			-- temporily unit:
			when key_verb_rotate =>
				case noun is
					when NOUN_UNIT =>
						rotate_unit_fetch;

					when others => null;						
				end case;

						
			when others => null;
		end case;
	end fetch;


	
	
	procedure set is begin
		case key is
			-- EVALUATE KEY FOR NOUN:
			when key_noun_partcode =>
				noun := NOUN_PARTCODE;
				set_status (et_canvas_schematic_units.status_set_partcode);

			when key_noun_purpose =>
				noun := NOUN_PURPOSE;
				set_status (et_canvas_schematic_units.status_set_purpose);
			
			when key_noun_value =>
				noun := NOUN_VALUE;					
				set_status (et_canvas_schematic_units.status_set_value);

			when key_noun_package_variant =>
				noun := NOUN_VARIANT;
				set_status (et_canvas_schematic_units.status_set_variant);
				
			-- If space pressed, then the operator wishes to operate via keyboard:	
			when key_space =>
				case noun is
					when NOUN_VALUE =>
						et_canvas_schematic_units.set_value (point);

					when NOUN_PURPOSE =>
						et_canvas_schematic_units.set_purpose (point);

					when NOUN_PARTCODE =>
						et_canvas_schematic_units.set_partcode (point);
						
					when NOUN_VARIANT =>
						et_canvas_schematic_units.set_package_variant (point);
						
					when others => null;
				end case;

			-- If page down pressed, then the operator is clarifying:
			when key_clarify =>
				case noun is
					when NOUN_PARTCODE | NOUN_PURPOSE | NOUN_VALUE | NOUN_VARIANT =>
						if clarification_pending then
							et_canvas_schematic_units.clarify_object;
						end if;

					when others => null;							
				end case;
				
			when others => status_noun_invalid;
		end case;
		
	end set;


	
	procedure show is begin
		case key is
			-- EVALUATE KEY FOR NOUN:
			when key_noun_device =>
				noun := NOUN_DEVICE;
				set_status (et_canvas_schematic_units.status_show_device);
				
			when key_noun_net =>
				noun := NOUN_NET;
				set_status (et_canvas_schematic_nets.status_show_net);

			when key_noun_label =>
				noun := NOUN_NET_LABEL;
				-- CS set_status (et_canvas_schematic_nets.status_show_label);

				
			-- If space pressed, then the operator wishes to operate via keyboard:	
			when key_space =>
				case noun is
					when NOUN_DEVICE =>
						et_canvas_schematic_units.show_object (get_cursor_position);
						
					when NOUN_NET_CONNECTOR | NOUN_NET_LABEL | NOUN_NET =>
						et_canvas_schematic_nets.show_object (get_cursor_position);
						
					when others => null;
				end case;

				
			-- If page down pressed, then the operator is clarifying:
			when key_clarify =>
				case noun is
					when NOUN_DEVICE => 
						if clarification_pending then
							et_canvas_schematic_units.clarify_object;
						end if;

					when NOUN_NET_CONNECTOR | NOUN_NET_LABEL | NOUN_NET =>
						if clarification_pending then
							et_canvas_schematic_nets.clarify_object;
						end if;

					when others => null;
						
				end case;
				
			when others => status_noun_invalid;
		end case;
	end show;


	
	
	procedure rename is 
		use et_schematic_ops_nets;
	begin
		case key is
			-- EVALUATE KEY FOR NOUN:
			when key_noun_device =>
				noun := NOUN_DEVICE;
				set_status (et_canvas_schematic_units.status_rename_device);
				
			when key_noun_strand => -- rename strand
				noun := NOUN_STRAND;
				set_status (et_canvas_schematic_nets.status_rename_net_strand);
				
			when key_noun_net => -- rename all strands on current sheet
				noun := NOUN_NET;
				et_schematic_ops_nets.modify_net_on_all_sheets := false;
				set_status (et_canvas_schematic_nets.status_rename_net_sheet);
				
			when key_noun_net_all_sheets => -- rename everywhere: all strands on all sheets
				noun := NOUN_NET;
				et_schematic_ops_nets.modify_net_on_all_sheets := true;
				set_status (et_canvas_schematic_nets.status_rename_net_everywhere);

				
			-- If space pressed, then the operator wishes to operate via keyboard:	
			when key_space =>
				case noun is
					when NOUN_DEVICE =>
						et_canvas_schematic_units.rename_object (point);

					when NOUN_STRAND | NOUN_NET =>
						et_canvas_schematic_nets.rename_object (point);
						
					when others => null;
				end case;

				
			-- If page down pressed, then the operator is clarifying:
			when key_clarify =>
				case noun is
					when NOUN_DEVICE =>
						if clarification_pending then
							et_canvas_schematic_units.clarify_object;
						end if;

					when NOUN_STRAND | NOUN_NET =>
						if clarification_pending then
							et_canvas_schematic_nets.clarify_object;
						end if;
						
					when others => null;							
				end case;
				
			when others => status_noun_invalid;
		end case;

	end rename;

	
	
begin -- key_pressed
	log (text => "key pressed (schematic): " & to_string (key),
		 level => log_threshold);

-- 		put_line ("schematic: evaluating other key ...");
		-- put_line (gdk_key_type'image (key));

	case key is
			
		when others =>

			-- CS: The following block seems not relevant any more and 
			-- thus has been put in comments for the time being:
			
			-- If an imcomplete command has been entered via console then it starts
			-- waiting for finalization. This can be done by pressing the SPACE key.
			-- Then we call the corresponding subprogram for the actual job right away here:
			
			--if single_cmd.finalization_pending and primary_tool = KEYBOARD then
-- 			if finalization_is_pending (cmd) then
-- 			
-- 				if key = key_space then
-- 						
-- 					case verb is
-- 						when VERB_DELETE	=> delete;
-- 						when VERB_DRAG		=> drag;
-- 						when VERB_DRAW		=> draw;
-- 						when VERB_FETCH		=> fetch;
-- 						when VERB_MOVE		=> move;
-- 						when VERB_PLACE		=> place;							
-- 						when others			=> null;
-- 					end case;
-- 
-- 				end if;
-- 			else
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
							when key_verb_delete =>
								verb := VERB_DELETE;
								status_enter_noun;

							when key_verb_add =>
								verb := VERB_ADD;
								status_enter_noun;

							when key_verb_copy =>
								verb := VERB_COPY;
								status_enter_noun;
								
							when key_verb_drag =>
								verb := VERB_DRAG;
								status_enter_noun;

							when key_verb_draw =>
								verb := VERB_DRAW;
								status_enter_noun;

							when key_verb_show =>
								verb := VERB_SHOW;
								status_enter_noun;
								
							when key_verb_fetch =>
								verb := VERB_FETCH;
								status_enter_noun;

							when key_verb_mirror =>
								verb := VERB_MIRROR;
								status_enter_noun;
								
							when key_verb_move =>
								verb := VERB_MOVE;
								status_enter_noun;

							when key_verb_rename =>
								verb := VERB_RENAME;
								status_enter_noun;
								
							when key_verb_place =>
								verb := VERB_PLACE;
								status_enter_noun;
								
							when key_verb_rotate =>
								verb := VERB_ROTATE;
								status_enter_noun;

							when key_verb_set =>
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
							when VERB_COPY		=> copy;
							when VERB_DELETE	=> delete;
							when VERB_DRAG		=> drag;
							when VERB_DRAW		=> draw;
							when VERB_FETCH		=> fetch;
							when VERB_MIRROR	=> mirror;
							when VERB_MOVE		=> move;
							when VERB_PLACE		=> place;
							when VERB_RENAME	=> rename;
							when VERB_ROTATE	=> rotate;
							when VERB_SET		=> set;
							when VERB_SHOW		=> show;
							when others => null; -- CS
						end case;
						
				end case;

			-- end if;		
	end case;

	redraw;
	-- CS use redraw_schematic if only schematic affected
	-- CS redraw after "enter" pressed
	
	update_mode_display;


	-- CS
	-- exception when event: others =>
	-- 	set_status (exception_message (event));
	-- 	reset_selections;
	-- 	redraw;
	-- 	update_mode_display;
	
end key_pressed;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
