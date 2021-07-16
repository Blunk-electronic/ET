------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              TESTBENCH                                   --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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
-- DESCRIPTION:
-- 

with ada.text_io;				use ada.text_io;
with ada.strings.unbounded;

--with ada.numerics.generic_elementary_functions;

with et_geometry;				use et_geometry;
with et_pcb_coordinates;		use et_pcb_coordinates;
with et_board_shapes_and_text;

use et_pcb_coordinates.pac_geometry_brd;
use et_board_shapes_and_text.pac_shapes;


procedure testbench is

	use functions_float;

	
	C : type_point := type_point (set (x => 3.1, y => 4.1));

	type type_segment_area is new type_polygon_base with null record;
	P : type_segment_area;

		
	use pac_polygon_segments;
	s : type_polygon_segments := (circular => false, others => <>);

	L : type_line;
	A : type_arc;
	
begin
	L.start_point := type_point (set (x =>  3.0000, y =>  4.1000));
	L.end_point   := type_point (set (x =>  8.0000, y =>  4.1000));
	append (s.segments, (LINE, L));

	A.start_point := type_point (set (x =>  8.0000, y =>  4.1000));
	A.end_point   := type_point (set (x =>  8.0000, y =>  3.9000));
	A.center      := type_point (set (x =>  8.0000, y =>  4.0000));
	A.direction   := CW;
	append (s.segments, (ARC, A));

	L.start_point := type_point (set (x =>  8.0000, y =>  3.9000));
	L.end_point   := type_point (set (x =>  3.0000, y =>  3.9000));
	append (s.segments, (LINE, L));

	A.start_point := type_point (set (x =>  3.0000, y =>  3.9000));
	A.end_point   := type_point (set (x =>  3.0000, y =>  4.1000));
	A.center      := type_point (set (x =>  3.0000, y =>  4.0000));
	A.direction   := CW;
	append (s.segments, (ARC, A));
	
	P.contours := s;


	new_line;

	if is_closed (P).closed then

		put_line ("point" & to_string (C));
		put_line (to_string (P));
		new_line;

		case in_polygon_status (P, C).status is
			when INSIDE =>
				null;
				
			when OUTSIDE =>
				null;
		end case;

	else
		put_line ("not closed");
	end if;
	
end testbench;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
