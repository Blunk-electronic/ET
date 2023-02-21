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

procedure mouse_moved (
	self	: not null access type_view;
	point	: in type_point) 
is
	use pac_devices_lib;
begin
	case verb is
		when VERB_ADD =>
			case noun is
				when NOUN_DEVICE =>
					if unit_add.device /= pac_devices_lib.no_element then
						redraw;
					end if;

				when others => null;
			end case;
		
		when VERB_DRAW =>
			case noun is
				when NOUN_NET =>
					if route.path.being_drawn then
						redraw;
					end if;


				when others => null;
			end case;
			
		when VERB_DRAG | VERB_MOVE | VERB_PLACE =>
			case noun is
				when NOUN_LABEL =>
					if label.ready then
						redraw_schematic;
					end if;
					
				when NOUN_NAME | NOUN_PURPOSE | NOUN_VALUE => 
					if placeholder_move.being_moved then
						redraw_schematic;
					end if;

				when NOUN_NET =>
					if preliminary_segment.ready then
						redraw_schematic;
					end if;

				when NOUN_UNIT =>
					if unit_move.being_moved then
						redraw_schematic;
					end if;

				when others => null;
			end case;

		when VERB_INVOKE =>
			case noun is
				when NOUN_UNIT =>
					if unit_add.device /= pac_devices_lib.no_element then
						redraw;
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
