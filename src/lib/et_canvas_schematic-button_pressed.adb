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

procedure button_pressed (
	self	: not null access type_view;
	button	: in type_mouse_button;
	point	: in type_point) 
is
	snap_point : constant type_point := snap_to_grid (self, point);

	procedure left_button is 
		use pac_devices_lib;
	begin
		-- A left click always moves the cursor:
		self.move_cursor (ABSOLUTE, cursor_main, point);
		self.update_coordinates_display;

		case verb is
			when VERB_ADD =>
				case noun is
					when NOUN_DEVICE =>

						-- When adding units, we enforce the default grid
						-- and snap the cursor position to the default grid:
						self.reset_grid_and_cursor;
						
						-- If a unit has been selected already, then
						-- the number of "activate" actions must be counted.
						-- The "activate" action in this case is a left click.
						-- After the first "activate" the tool
						-- for placing the unit is set. After the second "activate"
						-- the unit is placed at the current mouse position.
						-- If no unit has been selected yet, then the device
						-- model selection dialog opens.
						if unit_add.device /= pac_devices_lib.no_element then

							increment_activate_counter;
							
							case activate_counter is
								when 1 =>
									unit_add.tool := MOUSE;

								when 2 =>
									-- Finally place the unit at the current 
									-- mouse position:
									finalize_add_device (snap_to_grid (self, point));

								when others => null;
							end case;

						else -- no unit selected yet
							add_device; -- open device model selection
						end if;
							
					when others => null;
							
				end case;
			
			when VERB_DELETE =>
				case noun is
					when NOUN_LABEL =>
						if not clarification_pending then
							delete_label (point);
						else
							delete_selected_label;
						end if;
						
					when NOUN_NET => 
						if not clarification_pending then
							delete_net_segment (point);
						else
							delete_selected_net_segment;
						end if;

					when NOUN_UNIT =>
						if not clarification_pending then
							delete_unit (point);
						else
							delete_selected_unit;
						end if;
						
					when others => null;
				end case;

			when VERB_DRAG =>
				case noun is
					when NOUN_UNIT =>
						-- When dragging units, we enforce the default grid
						-- and snap the cursor position to the default grid:
						self.reset_grid_and_cursor;
						drag_unit (MOUSE, snap_point);
						
					when NOUN_NET => 
						-- When dragging net segments, we enforce the default grid
						-- and snap the cursor position to the default grid:
						self.reset_grid_and_cursor;
						drag_segment (MOUSE, snap_point);
		
					when others => null;
				end case;
				
			when VERB_DRAW =>
				case noun is
					when NOUN_NET =>
						-- When drawing net segments, we enforce the default grid
						-- and snap the cursor position to the default grid:
						self.reset_grid_and_cursor;

						self.make_net_route (MOUSE, snap_to_grid (self, point));
						
					when others => null;							
				end case;

			when VERB_INVOKE =>
				case noun is

					when NOUN_UNIT =>
						-- If no device has been selected already, then
						-- set the tool used for invoking.
						if unit_add.device = pac_devices_lib.no_element then

							unit_add.tool := MOUSE;

							if not clarification_pending then
								invoke_unit (snap_to_grid (self, point));
							else
								show_units;
							end if;

						else
							finalize_invoke (snap_to_grid (self, point), log_threshold + 1);
						end if;
						
					when others => null;
						
				end case;
				
			when VERB_MOVE =>
				case noun is
					when NOUN_LABEL =>
						move_label (MOUSE, point);

						
					when NOUN_NAME =>
						move_placeholder (MOUSE, snap_point, NAME);

					when NOUN_PURPOSE =>
						move_placeholder (MOUSE, snap_point, PURPOSE);

					when NOUN_VALUE =>
						move_placeholder (MOUSE, snap_point, VALUE);

						
					when NOUN_UNIT =>
						-- When moving units, we enforce the default grid
						-- and snap the cursor position to the default grid:
						self.reset_grid_and_cursor;
						move_unit (MOUSE, snap_point);							
						
					when others => null;							
				end case;

			when VERB_PLACE =>
				case noun is
					when NOUN_LABEL =>
						place_label (MOUSE, snap_point);
						
					when others => null;
				end case;

			when VERB_RENAME =>
				case noun is
					when NOUN_DEVICE =>
						if not clarification_pending then
							set_property (point);
						else
							set_property_selected_unit;
						end if;

					when NOUN_NET =>
						if not clarification_pending then
							find_segments (point);
						else
							et_canvas_schematic_nets.window_set_property;
						end if;
						
					when others => null;
				end case;
				
			when VERB_ROTATE =>
				case noun is
					when NOUN_NAME =>
						if not clarification_pending then
							rotate_placeholder (
								point		=> point,
								category	=> NAME);
						else
							rotate_selected_placeholder (NAME);
						end if;
						
					when NOUN_PURPOSE =>
						if not clarification_pending then
							rotate_placeholder (
								point		=> point,
								category	=> PURPOSE);
						else
							rotate_selected_placeholder (PURPOSE);
						end if;
					
					when NOUN_UNIT =>
						if not clarification_pending then
							rotate_unit (point);
						else
							rotate_selected_unit;
						end if;

					when NOUN_VALUE =>
						if not clarification_pending then
							rotate_placeholder (
								point		=> point,
								category	=> VALUE);
						else
							rotate_selected_placeholder (VALUE);
						end if;

						
					when others => null;
				end case;

			when VERB_SET =>
				case noun is
					when NOUN_PARTCODE | NOUN_PURPOSE | NOUN_VALUE | NOUN_VARIANT =>
						if not clarification_pending then
							set_property (point);
						else
							set_property_selected_unit;
						end if;
						
					when others => null;
				end case;

			when VERB_SHOW =>
				case noun is
					when NOUN_DEVICE =>
						if not clarification_pending then
							find_units_for_show (point);
						else
							show_properties_of_selected_device;
						end if;
						
					when NOUN_NET =>
						if not clarification_pending then
							find_segments (point);
						else
							show_properties_of_selected_net;
						end if;
						
					when others => null;
				end case;
				
			when others => null; -- CS
		end case;
		
	end left_button;

	
	-- If right button clicked, then the operator is clarifying:
	procedure right_button is begin
		case verb is
			when VERB_DELETE =>
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
						
					when others => null;							
				end case;

			when VERB_DRAG =>
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
				
			when VERB_DRAW =>
				case noun is
					when NOUN_NET =>
						next_bend_style (route.path);
						
					when others => null;							
				end case;

			when VERB_INVOKE =>
				case noun is

					when NOUN_UNIT => 
						if clarification_pending then
							clarify_unit;
						end if;
						
					when others => null;
						
				end case;
				
			when VERB_MOVE =>
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

			when VERB_PLACE =>
				case noun is
					
					when NOUN_LABEL => 
						if clarification_pending then
							clarify_net_segment;
						end if;

						-- Rotate simple label:
						if label.being_moved then
							toggle_rotation (label.rotation_simple);
						end if;
						
					when others => null;
				end case;

			when VERB_RENAME =>
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
						
			when VERB_ROTATE =>
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

			when VERB_SET =>
				case noun is
					when NOUN_PARTCODE | NOUN_PURPOSE | NOUN_VALUE =>
						if clarification_pending then
							clarify_unit;
						end if;
						
					when others => null;							
				end case;

			when VERB_SHOW =>
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
				
			when others => null; -- CS
		end case;

	end right_button;

	
begin -- button_pressed
	--log (text => to_string (button) & " at" & to_string (point), level => log_threshold);
	
	case button is
		when 1 => left_button;
		when 3 => right_button;
		when others => null;
	end case;

	redraw;
	-- CS use redraw_schematic if only schematic affected

	
	exception when event: others =>
		set_status (exception_message (event));

		reset_selections;
		redraw;
	
end button_pressed;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
