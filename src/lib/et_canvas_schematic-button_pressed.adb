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


-- with et_text;

with et_device_placeholders;


separate (et_canvas_schematic)

procedure button_pressed (
	event : in type_mouse_event) 
is
	use et_modes.schematic;
	
	snap_point : constant type_vector_model := snap_to_grid (event.point);
	-- CS rename to point

	-- CS global variable for the tool MOUSE
	
	procedure left_button is 
		use et_canvas_schematic_nets;
		use et_canvas_schematic_units;
		use et_device_placeholders;


		procedure add_device is 
			use et_canvas_schematic_units;
		begin
			-- If the operator is done with the device model
			-- selection, then the unit can be dropped:
			if unit_add.valid then
				-- When adding units, we enforce the default grid
				-- and snap the cursor position to the default grid:
				reset_grid_and_cursor;

				-- If a device model has been selected, then
				-- an unit will be dropped at the current 
				-- cursor position. The properties of the new device
				-- are taken from the preliminary unit_add:
				add_device (snap_point);
			end if;
		end add_device;

		
	begin
		case verb is
			when VERB_ADD =>
				case noun is
					when NOUN_DEVICE =>
						add_device;
							
					when others => null;							
				end case;


			when VERB_COPY =>
				case noun is
					when NOUN_DEVICE =>
						et_canvas_schematic_units.copy_object (MOUSE, snap_point);
							
					when others => null;							
				end case;

				
			when VERB_DELETE =>
				case noun is
					when NOUN_NET_CONNECTOR | NOUN_NET_LABEL | NOUN_NET | NOUN_STRAND | NOUN_SEGMENT => 
						et_canvas_schematic_nets.delete_object (snap_point);
						
					when NOUN_DEVICE | NOUN_UNIT =>
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
						fetch_unit (MOUSE, snap_point);
						
					when others => null;
				end case;

				
			when VERB_MOVE =>
				case noun is
					when NOUN_NET_LABEL =>
						et_canvas_schematic_nets.move_object (MOUSE, snap_point);
						
					when NOUN_PLACEHOLDER =>
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
					when NOUN_NET_CONNECTOR =>
						place_net_connector (MOUSE, snap_point);

					when NOUN_NET_LABEL =>
						place_net_label (MOUSE, snap_point);

						
					when others => null;
				end case;

				
			when VERB_RENAME =>
				case noun is
					when NOUN_DEVICE =>
						et_canvas_schematic_units.rename_object (snap_point);
							
					when NOUN_STRAND | NOUN_NET =>
						et_canvas_schematic_nets.rename_object (snap_point);
						
					when others => null;
				end case;

				
			when VERB_ROTATE =>
				case noun is
					when NOUN_PLACEHOLDER =>
						et_canvas_schematic_units.rotate_object (snap_point);
						
					when NOUN_UNIT =>
						et_canvas_schematic_units.rotate_object (snap_point);
						
					when others => null;
				end case;

				
			when VERB_SET =>
				case noun is
					when NOUN_VALUE =>
						et_canvas_schematic_units.set_value (snap_point);

					when NOUN_PURPOSE =>
						et_canvas_schematic_units.set_purpose (snap_point);

					when NOUN_PARTCODE =>
						et_canvas_schematic_units.set_partcode (snap_point);
						
					when NOUN_VARIANT =>
						et_canvas_schematic_units.set_package_variant (snap_point);

						
					when others => null;
				end case;

				
			when VERB_SHOW =>
				case noun is
					when NOUN_DEVICE =>
						null;
						-- CS
						-- if not clarification_pending then
						-- 	find_units_for_show (event.point);
						-- else
						-- 	show_properties_of_selected_device;
						-- end if;

						
					when NOUN_NET | NOUN_NET_LABEL =>
						et_canvas_schematic_nets.show_object (snap_point);
						
					when others => null;
				end case;
				
			when others => null; -- CS
		end case;		
	end left_button;

	

	
	-- If right button clicked, then the operator is clarifying:
	-- CS: Rotate objects (while adding, copying, fetching, ...)
	procedure right_button is 
		use et_canvas_schematic_nets;
		use et_canvas_schematic_units;

		use pac_path_and_bend;
	begin
		case verb is
			when VERB_COPY =>
				case noun is
					when NOUN_DEVICE =>
						if clarification_pending then
							et_canvas_schematic_units.clarify_object;
						end if;
						
					when others => null;							
				end case;

				
			when VERB_DELETE =>
				case noun is
					when NOUN_NET_CONNECTOR | NOUN_NET_LABEL | NOUN_NET | NOUN_STRAND | NOUN_SEGMENT => 
						if clarification_pending then
							et_canvas_schematic_nets.clarify_object;
						end if;

					when NOUN_DEVICE | NOUN_UNIT =>
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
							et_canvas_schematic_units.clarify_object;
						end if;
						
					when others => null;						
				end case;

				
			when VERB_MOVE =>
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

				
			when VERB_PLACE =>
				case noun is					
					when NOUN_NET_CONNECTOR | NOUN_NET_LABEL => 
						if clarification_pending then
							et_canvas_schematic_nets.clarify_object;
						end if;
						
					when others => null;
				end case;

				
			when VERB_RENAME =>
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

				
			when VERB_ROTATE =>
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

				
			when VERB_SET =>
				case noun is
					when NOUN_PARTCODE | NOUN_PURPOSE | NOUN_VALUE | NOUN_VARIANT =>
						if clarification_pending then
							et_canvas_schematic_units.clarify_object;
						end if;
						
					when others => null;							
				end case;

				
			when VERB_SHOW =>
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

				
			when others => null; -- CS
		end case;

	end right_button;

	
	
begin -- button_pressed
	log (text => "button_pressed (schematic) "  -- CS which button ?
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
