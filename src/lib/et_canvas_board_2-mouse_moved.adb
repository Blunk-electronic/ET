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
					if edit_process_running then
						redraw_board;
					end if;

				when NOUN_OUTLINE =>
					if edit_process_running then
						redraw_board;
					end if;
					
				when NOUN_ZONE =>
					if edit_process_running then
						redraw_board;
					end if;
					
				when others => null;
			end case;
			
			
		when VERB_MOVE =>
			case noun is
				when NOUN_ASSY =>
					if edit_process_running then
						redraw_board;
					end if;

				when NOUN_SILKSCREEN =>
					if edit_process_running then
						redraw_board;
					end if;

				when NOUN_STOPMASK | NOUN_STENCIL | NOUN_KEEPOUT =>
					if edit_process_running then
						redraw_board;
					end if;
					
				when NOUN_CONDUCTORS =>
					if edit_process_running then
						redraw_board;
					end if;

				when NOUN_TRACK =>
					if edit_process_running then
						redraw_board;
					end if;
					
				when NOUN_FREETRACK =>
					if edit_process_running then
						redraw_board;
					end if;
					
				when NOUN_DEVICE =>
					if edit_process_running then
						redraw_board;
					end if;

				-- when NOUN_NON_ELECTRICAL_DEVICE =>
				-- 	if edit_process_running then
				-- 		redraw_board;
				-- 	end if;

				when NOUN_OUTLINE =>
					if edit_process_running then
						redraw_board;
					end if;
					
				when NOUN_TEXT =>
					if edit_process_running then
						redraw_board;
					end if;
					
				when NOUN_VIA =>
					if edit_process_running then
						redraw_board;
					end if;
					
				when others => null;
			end case;
			

		when VERB_PLACE =>
			case noun is
				when NOUN_TEXT =>
					redraw_board;

				when NOUN_VIA =>
					redraw_board;
					
				when others => null;
			end case;


		when VERB_ROUTE =>
			case noun is
				when NOUN_NET =>
					redraw_board;

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
