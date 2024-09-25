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


with et_canvas_tool;				use et_canvas_tool;

with et_board_ops.conductors;


separate (et_canvas_board_2)

procedure button_pressed (
	event : in type_mouse_event)
is
	use et_modes.board;
	
	snap_point : constant type_vector_model := snap_to_grid (event.point);

	
	procedure left_button is 
	begin
		
		case verb is				
			when VERB_FLIP =>
				case noun is
					when NOUN_DEVICE =>
						et_canvas_board_devices.flip_electrical_device (MOUSE, event.point);
						
					when NOUN_NON_ELECTRICAL_DEVICE =>
						et_canvas_board_devices.flip_non_electrical_device (MOUSE, event.point);
						
					when others => null;
				end case;

				
			when VERB_MOVE =>
				case noun is
					when NOUN_ASSY =>
						et_canvas_board_assy_doc.move_object (MOUSE, snap_point);

					when NOUN_SILKSCREEN =>
						et_canvas_board_silkscreen.move_object (MOUSE, snap_point);

					when NOUN_TRACK =>
						et_canvas_board_tracks.move_track (MOUSE, snap_point);
						
					when NOUN_DEVICE =>
						et_canvas_board_devices.move_electrical_device (MOUSE, snap_point);

					when NOUN_NON_ELECTRICAL_DEVICE =>
						et_canvas_board_devices.move_non_electrical_device (MOUSE, snap_point);

					when NOUN_TEXT =>
						et_canvas_board_texts.move_text (MOUSE, snap_point);
						
					when NOUN_VIA =>
						et_canvas_board_vias.move_via (MOUSE, snap_point);
						
					when others => null;
				end case;


			when VERB_DRAW =>
				case noun is
					when NOUN_LINE =>
						-- put_line ("draw line");
						et_canvas_board_lines.make_path (MOUSE, snap_point);

					when others => null;						
				end case;

				
			when VERB_PLACE =>
				case noun is
					when NOUN_TEXT =>
						et_canvas_board_texts.place_text (snap_point);

					when NOUN_VIA =>
						et_canvas_board_vias.place_via (snap_point);
						
					when others => null;
				end case;
				

			when VERB_ROTATE =>
				case noun is
					when NOUN_DEVICE =>
						et_canvas_board_devices.rotate_electrical_device (MOUSE, event.point);

					when NOUN_NON_ELECTRICAL_DEVICE =>
						et_canvas_board_devices.rotate_non_electrical_device (MOUSE, event.point);
						
					when others => null;
				end case;


			when VERB_ROUTE =>
				case noun is
					when NOUN_NET =>
						et_canvas_board_tracks.make_path (MOUSE, snap_point);

					when others => null;
				end case;
								

			when VERB_RIPUP =>
				case noun is
					when NOUN_NET =>
						et_canvas_board_tracks.ripup (event.point);

					when others => null;
				end case;

				
			when VERB_DELETE =>
				case noun is
					when NOUN_ASSY =>
						et_canvas_board_assy_doc.delete_object (event.point);

					when NOUN_SILKSCREEN =>
						et_canvas_board_silkscreen.delete_object (event.point);
					
					when NOUN_NON_ELECTRICAL_DEVICE =>
						et_canvas_board_devices.delete_non_electrical_device (MOUSE, event.point);

					when NOUN_VIA =>
						et_canvas_board_vias.delete_via (MOUSE, event.point);
						
					when others => null;
				end case;
				
			when others => null;

		end case;			
	end left_button;

	
	
	procedure right_button is begin
		case verb is
			when VERB_MOVE | VERB_ROTATE =>
				case noun is
					
					--when NOUN_NAME => 
						--if clarification_pending then
							--clarify_placeholder;
						--end if;

					--when NOUN_PURPOSE => 
						--if clarification_pending then
							--clarify_placeholder;
						--end if;

					when NOUN_ASSY =>
						if clarification_pending then
							et_canvas_board_assy_doc.select_object;
						end if;

					when NOUN_SILKSCREEN =>
						if clarification_pending then
							et_canvas_board_silkscreen.select_object;
						end if;

					when NOUN_TRACK =>
						if clarification_pending then
							et_canvas_board_tracks.select_track;
						end if;
						
					when NOUN_DEVICE =>
						if clarification_pending then
							et_canvas_board_devices.select_electrical_device;
						end if;

					when NOUN_NON_ELECTRICAL_DEVICE =>
						if clarification_pending then
							et_canvas_board_devices.select_non_electrical_device;
						end if;

					when NOUN_TEXT =>
						if clarification_pending then
							et_canvas_board_texts.select_text;
						end if;
						
					when NOUN_VIA =>
						if clarification_pending then
							et_canvas_board_vias.select_via;
						end if;
						
					--when NOUN_VALUE => 
						--if clarification_pending then
							--clarify_placeholder;
						--end if;
						
					when others => null;							
				end case;

				
			when VERB_DRAW =>
				case noun is
					when NOUN_LINE =>
						pac_path_and_bend.next_bend_style (
							et_canvas_board_lines.preliminary_line.path);
						
					when others => null;							
				end case;

				
			when VERB_FLIP =>
				case noun is
					
					when NOUN_DEVICE =>
						if clarification_pending then
							et_canvas_board_devices.select_electrical_device;
						end if;

					when NOUN_NON_ELECTRICAL_DEVICE =>
						if clarification_pending then
							et_canvas_board_devices.select_non_electrical_device;
						end if;
						
					when others => null;							
				end case;


			when VERB_DELETE =>
				case noun is
					when NOUN_ASSY =>
						if clarification_pending then
							et_canvas_board_assy_doc.select_object;
						end if;

					when NOUN_SILKSCREEN =>
						if clarification_pending then
							et_canvas_board_silkscreen.select_object;
						end if;
					
					when NOUN_NON_ELECTRICAL_DEVICE =>
						if clarification_pending then
							et_canvas_board_devices.select_non_electrical_device;
						end if;

					when NOUN_VIA =>
						if clarification_pending then
							et_canvas_board_vias.select_via;
						end if;
						
					when others => null;							
				end case;


			when VERB_ROUTE =>
				case noun is
					when NOUN_NET =>
						-- As long as a clarification of the 
						-- airwires is pending, a right click
						-- advances to the next airwire.
						-- If no clarification is requested, then
						-- a right click changes the bend style:
						if clarification_pending then
							et_canvas_board_tracks.select_airwire;
						else
							pac_path_and_bend.next_bend_style (
								et_canvas_board_tracks.preliminary_track.path);
						end if;
						
					when others => null;							
				end case;


			when VERB_RIPUP =>
				case noun is
					when NOUN_NET =>
						-- As long as a clarification of the 
						-- segment is pending, a right click
						-- advances to the next segment.
						-- If no clarification is requested, then
						-- a right click changes the ripup mode:
						if clarification_pending then
							et_canvas_board_tracks.select_track;
						else
							-- select ripup mode
							et_canvas_board_tracks.next_ripup_mode;
						end if;
						
					when others => null;							
				end case;

				
			when others => null;
		end case;
	end right_button;

	
begin -- button_pressed
	log (text => "button_pressed (board) " 
		 & to_string (event), level => log_threshold);
	
	case event.button is
		when 1 => left_button;
		when 3 => right_button;
		when others => null;
	end case;
 
	-- redraw;
	-- CS use redraw_board if only board affected

	
	exception when event: others =>
		set_status (exception_message (event));

		-- CS reset_selections;
		-- redraw;
	
end button_pressed;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
