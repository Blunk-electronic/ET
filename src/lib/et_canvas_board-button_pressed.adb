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

with et_canvas_board_tracks;


separate (et_canvas_board)

procedure button_pressed (
	self	: not null access type_view;
	button	: in type_mouse_button;
	point	: in type_point) 
is
	snap_point : constant type_point := snap_to_grid (point);

	
	procedure left_button is begin
		-- A left click always moves the cursor:
		self.move_cursor (ABSOLUTE, cursor_main, point);
		self.update_coordinates_display;
		
		case verb is				
			when VERB_FLIP =>
				case noun is
					when NOUN_DEVICE =>
						flip_electrical_device (MOUSE, point);
						
					when NOUN_NON_ELECTRICAL_DEVICE =>
						flip_non_electrical_device (MOUSE, point);
						
					when others => null;
				end case;

				
			when VERB_MOVE =>
				case noun is
					when NOUN_ASSY =>
						et_canvas_board_assy_doc.move_object (MOUSE, point);

					when NOUN_SILKSCREEN =>
						et_canvas_board_silkscreen.move_object (MOUSE, point);

					when NOUN_TRACK =>
						et_canvas_board_tracks.move_track (MOUSE, point);
						
					when NOUN_DEVICE =>
						move_electrical_device (MOUSE, point);

					when NOUN_NON_ELECTRICAL_DEVICE =>
						move_non_electrical_device (MOUSE, point);

					when NOUN_TEXT =>
						move_text (MOUSE, point);
						
					when NOUN_VIA =>
						move_via (MOUSE, snap_point);
						
					when others => null;
				end case;


			when VERB_DRAW =>
				case noun is
					when NOUN_LINE =>
						make_path (MOUSE, snap_point);

					when others => null;						
				end case;

				
			when VERB_PLACE =>
				case noun is
					when NOUN_TEXT =>
						place_text (snap_point);

					when NOUN_VIA =>
						place_via (snap_point);
						
					when others => null;
				end case;
				

			when VERB_ROTATE =>
				case noun is
					when NOUN_DEVICE =>
						rotate_electrical_device (MOUSE, point);

					when NOUN_NON_ELECTRICAL_DEVICE =>
						rotate_non_electrical_device (MOUSE, point);
						
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
						et_canvas_board_tracks.ripup (point);

					when others => null;
				end case;

				
			when VERB_DELETE =>
				case noun is
					when NOUN_ASSY =>
						et_canvas_board_assy_doc.delete_object (point);

					when NOUN_SILKSCREEN =>
						et_canvas_board_silkscreen.delete_object (point);
					
					when NOUN_NON_ELECTRICAL_DEVICE =>
						delete_non_electrical_device (MOUSE, point);

					when NOUN_VIA =>
						delete_via (MOUSE, point);
						
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
							select_electrical_device;
						end if;

					when NOUN_NON_ELECTRICAL_DEVICE =>
						if clarification_pending then
							select_non_electrical_device;
						end if;

					when NOUN_TEXT =>
						if clarification_pending then
							select_text;
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

				
			when VERB_DRAW =>
				case noun is
					when NOUN_LINE =>
						next_bend_style (preliminary_line.path);
						
					when others => null;							
				end case;

				
			when VERB_FLIP =>
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
							select_non_electrical_device;
						end if;

					when NOUN_VIA =>
						if clarification_pending then
							select_via;
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
							next_bend_style (et_canvas_board_tracks.preliminary_track.path);
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
	--log (text => to_string (button) & " at" & to_string (point), level => log_threshold);
	
	case button is
		when 1 => left_button;
		when 3 => right_button;
		when others => null;
	end case;

	redraw;
	-- CS use redraw_board if only board affected

	
	exception when event: others =>
		set_status (exception_message (event));

		-- CS reset_selections;
		redraw;
	
end button_pressed;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
