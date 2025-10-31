------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           CANVAS FOR BOARD                               --
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


with et_canvas_tool;						use et_canvas_tool;



separate (et_canvas_board)

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
						et_canvas_board_devices.flip_object (snap_point);
						
						
					when others => null;
				end case;

				
			when VERB_MOVE =>
				case noun is
					when NOUN_ASSY =>
						et_canvas_board_assy_doc.move_object (MOUSE, snap_point);

					when NOUN_SILKSCREEN =>
						et_canvas_board_silkscreen.move_object (MOUSE, snap_point);

					when NOUN_STOPMASK =>
						et_canvas_board_stopmask.move_object (MOUSE, snap_point);

					when NOUN_STENCIL =>
						et_canvas_board_stencil.move_object (MOUSE, snap_point);

					when NOUN_KEEPOUT =>
						et_canvas_board_keepout.move_object (MOUSE, snap_point);
						
					when NOUN_CONDUCTORS =>
						et_canvas_board_conductors.move_object (MOUSE, snap_point);
						
					-- when NOUN_TRACK =>
					-- 	et_canvas_board_tracks.move_track (MOUSE, snap_point);

					when NOUN_FREETRACK =>
						et_canvas_board_conductors.move_object (MOUSE, snap_point);
						
					when NOUN_DEVICE =>
						et_canvas_board_devices.move_object (MOUSE, snap_point);


					when NOUN_OUTLINE =>
						et_canvas_board_outline.move_object (MOUSE, snap_point);

					-- when NOUN_TEXT =>
					-- 	et_canvas_board_texts.move_text (MOUSE, snap_point);
						
					when NOUN_VIA =>
						et_canvas_board_vias.move_object (MOUSE, snap_point);
						
					when others => null;
				end case;


			when VERB_DRAW =>
				case noun is
					when NOUN_LINE =>
						-- put_line ("draw line");
						make_path (MOUSE, snap_point,
							et_canvas_board_lines.add_by_category'access);
						
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
						et_canvas_board_devices.rotate_object (event.point);

					when others => null;
				end case;


			when VERB_ROUTE =>
				case noun is
					when NOUN_NET =>
						et_canvas_board_tracks.make_path (MOUSE, snap_point);

					when others => null;
				end case;
								
				
			when VERB_DELETE =>
				case noun is
					when NOUN_ASSY =>
						et_canvas_board_assy_doc.delete_object (event.point);

					when NOUN_SILKSCREEN =>
						et_canvas_board_silkscreen.delete_object (event.point);
					
					when NOUN_STOPMASK =>
						et_canvas_board_stopmask.delete_object (event.point);

					when NOUN_STENCIL =>
						et_canvas_board_stencil.delete_object (event.point);

					when NOUN_KEEPOUT =>
						et_canvas_board_keepout.delete_object (event.point);
						
					when NOUN_DEVICE =>
						et_canvas_board_devices.delete_object (event.point);

					when NOUN_OUTLINE =>
						et_canvas_board_outline.delete_object (event.point);
						
					when NOUN_VIA =>
						et_canvas_board_vias.delete_via (MOUSE, event.point);

					when NOUN_CONDUCTORS =>
						et_canvas_board_conductors.delete_object (event.point);
						
					-- when NOUN_TRACK =>
					-- 	et_canvas_board_tracks.ripup (event.point);

					when NOUN_FREETRACK =>
						et_canvas_board_conductors.delete_object (event.point);


						
					when others => null;
				end case;


			when VERB_SHOW =>
				case noun is
					when NOUN_DEVICE =>
						et_canvas_board_devices.show_object (event.point);
						
					when NOUN_NET =>
						null;
						-- et_canvas_board_tracks.show_object (event.point);
						
					when others => null;
				end case;

				
			when others => null;

		end case;			
	end left_button;


	
	
	procedure right_button is 
		use et_ripup;
	begin
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
							et_canvas_board_assy_doc.clarify_object;
						end if;

					when NOUN_SILKSCREEN =>
						if clarification_pending then
							et_canvas_board_silkscreen.clarify_object;
						end if;

					when NOUN_STOPMASK =>
						if clarification_pending then
							et_canvas_board_stopmask.clarify_object;
						end if;

					when NOUN_STENCIL =>
						if clarification_pending then
							et_canvas_board_stencil.clarify_object;
						end if;

					when NOUN_KEEPOUT =>
						if clarification_pending then
							et_canvas_board_keepout.clarify_object;
						end if;
						
					when NOUN_CONDUCTORS =>
						if clarification_pending then
							et_canvas_board_conductors.clarify_object;
						end if;
						
					-- when NOUN_TRACK =>
					-- 	if clarification_pending then
					-- 		et_canvas_board_tracks.select_track;
					-- 	end if;

					when NOUN_FREETRACK =>
						if clarification_pending then
							et_canvas_board_conductors.clarify_object;
						end if;
						
					when NOUN_DEVICE =>
						if clarification_pending then
							et_canvas_board_devices.clarify_object;
						end if;


					when NOUN_OUTLINE =>
						if clarification_pending then
							et_canvas_board_outline.clarify_object;
						end if;
						
					-- when NOUN_TEXT =>
					-- 	if clarification_pending then
					-- 		et_canvas_board_texts.select_text;
					-- 	end if;
						
					when NOUN_VIA =>
						if clarification_pending then
							et_canvas_board_vias.clarify_object;
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
							live_path);
						
					when NOUN_ZONE =>
						pac_path_and_bend.next_bend_style (
							live_path);
						
					when others => null;							
				end case;

				
			when VERB_FLIP =>
				case noun is					
					when NOUN_DEVICE =>
						if clarification_pending then
							et_canvas_board_devices.clarify_object;
						end if;
						
					when others => null;							
				end case;


			when VERB_DELETE =>
				case noun is
					when NOUN_ASSY =>
						if clarification_pending then
							et_canvas_board_assy_doc.clarify_object;
						end if;

					when NOUN_SILKSCREEN =>
						if clarification_pending then
							et_canvas_board_silkscreen.clarify_object;
						end if;

					when NOUN_STOPMASK =>
						if clarification_pending then
							et_canvas_board_stopmask.clarify_object;
						end if;

					when NOUN_STENCIL =>
						if clarification_pending then
							et_canvas_board_stencil.clarify_object;
						end if;

					when NOUN_KEEPOUT =>
						if clarification_pending then
							et_canvas_board_keepout.clarify_object;
						end if;
						
					when NOUN_DEVICE =>
						if clarification_pending then
							et_canvas_board_devices.clarify_object;
						end if;

					when NOUN_OUTLINE =>
						if clarification_pending then
							et_canvas_board_outline.clarify_object;
						end if;
						
					when NOUN_VIA =>
						if clarification_pending then
							et_canvas_board_vias.clarify_object;
						end if;

					when NOUN_CONDUCTORS =>
						-- As long as a clarification of the 
						-- segment is pending, a right click
						-- advances to the next segment.
						-- If no clarification is requested, then
						-- a right click changes the ripup mode:
						if clarification_pending then
							et_canvas_board_conductors.clarify_object;
						else
							-- select ripup mode
							next_ripup_mode;
							set_status (to_string (ripup_mode));
						end if;

						
					-- when NOUN_TRACK =>
					-- 	-- As long as a clarification of the 
					-- 	-- segment is pending, a right click
					-- 	-- advances to the next segment.
					-- 	-- If no clarification is requested, then
					-- 	-- a right click changes the ripup mode:
					-- 	if clarification_pending then
					-- 		et_canvas_board_tracks.select_track;
					-- 	else
					-- 		-- select ripup mode
					-- 		next_ripup_mode;
					-- 		set_status (to_string (ripup_mode));
					-- 	end if;

					when NOUN_FREETRACK =>
						if clarification_pending then
							et_canvas_board_conductors.clarify_object;
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
							et_canvas_board_tracks.clarify_airwire;
						else
							pac_path_and_bend.next_bend_style (live_path);
						end if;
						
					when others => null;							
				end case;


			when VERB_SHOW =>
				case noun is
					when NOUN_DEVICE =>
						if clarification_pending then
							et_canvas_board_devices.clarify_object;
						end if;

					when NOUN_NET =>
						if clarification_pending then
							null;
							-- et_canvas_schematic_nets.clarify_object;
						end if;
						
					when others => null;							
				end case;

				
			when others => null;
		end case;
	end right_button;

	
begin -- button_pressed
	log (text => "button_pressed (board) "   -- CS which button ?
		 & to_string (event), level => log_threshold);
	
	case event.button is
		when 1 => left_button;
		when 3 => right_button;
		when others => null;
	end case;
 
	-- redraw;
	-- CS use redraw_board if only board affected


	-- CS
	-- exception when event: others =>
	-- 	set_status (exception_message (event));
	-- CS reset_selections;
	-- redraw;
	
end button_pressed;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
