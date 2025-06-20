------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        CANVAS FOR SCHEMATIC                              --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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


with et_text;

with et_device_model;
with et_device_library;			use et_device_library;
with et_device_placeholders;


separate (et_canvas_schematic_2)

procedure button_pressed (
	event : in type_mouse_event) 
is
	use et_modes.schematic;
	
	snap_point : constant type_vector_model := snap_to_grid (event.point);
	-- CS rename to point

	
	procedure left_button is 
		use et_device_model;
		use pac_devices_lib;
		
		use et_canvas_schematic_nets;
		use et_canvas_schematic_units;
		use et_device_placeholders;


		procedure add_device is
		begin
			-- When adding units, we enforce the default grid
			-- and snap the cursor position to the default grid:
			reset_grid_and_cursor;
			
			-- If no unit has been selected yet, then the device
			-- model selection dialog opens.
			if unit_add.device /= pac_devices_lib.no_element then

				-- If a unit has already been selected, then
				-- it will be dropped at the current mouse position:
				drop_unit (snap_point);

				-- Open the device model selection for 
				-- the next unit:
				show_model_selection;

			else -- no unit selected yet
				-- Open the device model selection window
				-- to allow selection of the unit:
				show_model_selection;
			end if;
		end add_device;


		
		procedure fetch_unit is
		begin
			-- If no device has been selected already, then
			-- fetch a unit:
			if unit_add.device = pac_devices_lib.no_element then

				if not clarification_pending then
					fetch_unit (snap_point);
				else
					show_units;
				end if;

			else
				finalize_fetch (snap_point, log_threshold + 1);
			end if;
		end fetch_unit;

		
		
	begin
		case verb is
			when VERB_ADD =>
				case noun is
					when NOUN_DEVICE =>
						add_device;
							
					when others => null;							
				end case;

				
			when VERB_DELETE =>
				case noun is
					when NOUN_LABEL | NOUN_NET | NOUN_STRAND | NOUN_SEGMENT => 
						et_canvas_schematic_nets.delete_object (snap_point);
						
					when NOUN_UNIT =>
						et_canvas_schematic_units.delete_object (snap_point);
						
					when others => null;
				end case;

				
			when VERB_DRAG =>
				case noun is
					when NOUN_UNIT =>
						-- When dragging units, we enforce the default grid
						-- and snap the cursor position to the default grid:
						reset_grid_and_cursor;
						et_canvas_schematic_units.drag_object (MOUSE, snap_point);
						
					when NOUN_SEGMENT => 
						-- When dragging net segments, we enforce the default grid
						-- and snap the cursor position to the default grid:
						reset_grid_and_cursor;
						et_canvas_schematic_nets.drag_object (MOUSE, snap_point);
		
					when others => null;
				end case;

				
				
			when VERB_DRAW =>
				case noun is
					when NOUN_NET =>
						-- When drawing net segments, we enforce the default grid
						-- and snap the cursor position to the default grid:
						reset_grid_and_cursor;

						make_path (MOUSE, snap_point);
						
					when others => null;							
				end case;

				
			when VERB_FETCH =>
				case noun is
					when NOUN_UNIT =>
						fetch_unit;
						
					when others => null;
				end case;

				
			when VERB_MOVE =>
				case noun is
					when NOUN_LABEL =>
						et_canvas_schematic_nets.move_object (MOUSE, snap_point);
						
					when NOUN_NAME | NOUN_PURPOSE | NOUN_VALUE =>
						et_canvas_schematic_units.move_object (MOUSE, snap_point);
				
					when NOUN_UNIT =>
						-- When moving units, we enforce the default grid
						-- and snap the cursor position to the default grid:
						reset_grid_and_cursor;
						et_canvas_schematic_units.move_object (MOUSE, snap_point);
						
					when others => null;							
				end case;

				
			when VERB_PLACE =>
				case noun is
					when NOUN_LABEL =>
						--place_label (MOUSE, snap_point);
						null; -- CS
						
					when others => null;
				end case;

				
			when VERB_RENAME =>
				case noun is
					when NOUN_DEVICE =>
						if not clarification_pending then
							set_property (event.point);
						else
							set_property_selected_unit;
						end if;

					when NOUN_NET =>
						if not clarification_pending then
							-- CS
							null;
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
								point		=> event.point,
								category	=> NAME);
						else
							rotate_selected_placeholder (NAME);
						end if;
						
					when NOUN_PURPOSE =>
						if not clarification_pending then
							rotate_placeholder (
								point		=> event.point,
								category	=> PURPOSE);
						else
							rotate_selected_placeholder (PURPOSE);
						end if;
					
					when NOUN_UNIT =>
						-- if not clarification_pending then
						-- 	rotate_unit (event.point);
						-- else
						-- 	rotate_selected_unit;
						-- end if;
						et_canvas_schematic_units.rotate_object (snap_point);

					when NOUN_VALUE =>
						if not clarification_pending then
							rotate_placeholder (
								point		=> event.point,
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
							set_property (event.point);
						else
							set_property_selected_unit;
						end if;
						
					when others => null;
				end case;

				
			when VERB_SHOW =>
				case noun is
					when NOUN_DEVICE =>
						if not clarification_pending then
							find_units_for_show (event.point);
						else
							show_properties_of_selected_device;
						end if;

						
					when NOUN_NET | NOUN_LABEL =>
						et_canvas_schematic_nets.show_object (snap_point);
						
					when others => null;
				end case;
				
			when others => null; -- CS
		end case;		
	end left_button;

	

	
	-- If right button clicked, then the operator is clarifying:
	procedure right_button is 
		use et_canvas_schematic_nets;
		use et_canvas_schematic_units;

		use pac_path_and_bend;
		use et_text;
	begin
		case verb is
			when VERB_DELETE =>
				case noun is
					when NOUN_LABEL | NOUN_NET | NOUN_STRAND | NOUN_SEGMENT => 
						if clarification_pending then
							et_canvas_schematic_nets.clarify_object;
						end if;

					when NOUN_UNIT =>
						if clarification_pending then
							et_canvas_schematic_units.clarify_object;
						end if;
						
					when others => null;							
				end case;

				
			when VERB_DRAG =>
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

				
			when VERB_DRAW =>
				case noun is
					when NOUN_NET =>
						next_bend_style (live_path);
						
					when others => null;							
				end case;

				
			when VERB_FETCH =>
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
							et_canvas_schematic_nets.clarify_object;
						end if;
					
					when NOUN_NAME | NOUN_PURPOSE | NOUN_VALUE => 
						if clarification_pending then
							et_canvas_schematic_units.clarify_object;
						end if;

					when NOUN_UNIT =>
						if clarification_pending then
							et_canvas_schematic_units.clarify_object;
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
						if label.ready then
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
							et_canvas_schematic_units.clarify_object;
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

					when NOUN_NET | NOUN_LABEL =>
						if clarification_pending then
							et_canvas_schematic_nets.clarify_object;
						end if;
						
					when others => null;							
				end case;

				
			when others => null; -- CS
		end case;

	end right_button;

	
	
begin -- button_pressed
	log (text => "button_pressed (schematic) " 
		 & to_string (event), level => log_threshold);

	
	case event.button is
		when 1 => left_button;
		when 3 => right_button;
		when others => null;
	end case;

	-- redraw;
	-- CS use redraw_schematic if only schematic affected


	-- CS
	-- exception when event: others =>
	-- 	set_status (exception_message (event));
	--  reset_selections;
	--  redraw;
	
end button_pressed;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
