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
with et_canvas_board_freetracks;
with et_canvas_board_preliminary_object;		use et_canvas_board_preliminary_object;


separate (et_canvas_board_2)

procedure mouse_moved (
	point	: in type_vector_model) 
is 
	use et_modes.board;

begin
	case verb is
		when VERB_DRAW =>
			case noun is
				when NOUN_LINE =>
					if preliminary_object.ready then
						redraw_board;
					end if;

				when NOUN_OUTLINE =>
					if preliminary_object.ready then
						redraw_board;
					end if;
					
				when NOUN_ZONE =>
					if preliminary_object.ready then
						redraw_board;
					end if;
					
				when others => null;
			end case;
			
			
		when VERB_MOVE =>
			case noun is
				when NOUN_ASSY =>
					if preliminary_object.ready then
						redraw_board;
					end if;

				when NOUN_SILKSCREEN =>
					if preliminary_object.ready then
						redraw_board;
					end if;

				when NOUN_TRACK =>
					if et_canvas_board_tracks.preliminary_segment.ready then
						redraw_board;
					end if;

				when NOUN_FREETRACK =>
					if preliminary_object.ready then
						redraw_board;
					end if;
					
				when NOUN_DEVICE =>
					if et_canvas_board_devices.preliminary_electrical_device.ready then
						redraw_board;
					end if;

				when NOUN_NON_ELECTRICAL_DEVICE =>
					if et_canvas_board_devices.preliminary_non_electrical_device.ready then
						redraw_board;
					end if;

				when NOUN_TEXT =>
					if et_canvas_board_texts.preliminary_text.ready then
						redraw_board;
					end if;
					
				when NOUN_VIA =>
					if et_canvas_board_vias.preliminary_via.ready then
						redraw_board;
					end if;
					
				when others => null;
			end case;
			

		when VERB_PLACE =>
			case noun is
				when NOUN_TEXT =>
					if et_canvas_board_texts.preliminary_text.ready then
						redraw_board;
					end if;

				when NOUN_VIA =>
					if et_canvas_board_vias.preliminary_via.ready then
						redraw_board;
					end if;
					
				when others => null;
			end case;


		when VERB_ROUTE =>
			case noun is
				when NOUN_NET =>
					if et_canvas_board_tracks.preliminary_track.ready then
						redraw_board;
					end if;

				when others => null;
			end case;
			
			
		when others => null;
	end case;
	
end mouse_moved;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
