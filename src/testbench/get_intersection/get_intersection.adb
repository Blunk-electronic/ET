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

with et_packages;				use et_packages;
with et_routing;				use et_routing;

procedure get_intersection is

	use functions_float;

	S : type_point := type_point (set (0.7118000000, 25.0000000000));
	D : type_point := type_point (set (1.0, 0.0));

	L : type_line_vector := (
		v_start => to_vector (S),
		v_direction => to_vector (D));

	
	C : type_circle := (
		center => type_point (set (10.8025000000, 24.9250000000)),
		radius => 0.075);

	I : type_intersection_of_line_and_circle := get_intersection (L, C);
	
begin

	put_line (type_intersection_status_of_line_and_circle'image (I.status));
	--cl := (S, E, 0.15);
	--put_line (to_string (cl));
	--new_line;


	
	--segment := to_line_segment (cl);

		
	----put_line ("segment " & to_string (segment));
	--new_line;
	
	--distance := get_shortest_distance (P,segment);

	--put_line ("distance" & to_string (distance));
	
	--P := type_point (round (P));
	--L := type_line (round (L));
	--put_line (to_string (L));

	--for i in 1 .. 1 loop
		--new_line;
		--P := type_point (move (P, 180.0, 0.2));
		--put_line ("P:" & to_string (P));
		
		--d := get_intersection (P, L, WITH_END_POINTS);
		--put_line ("D:" & to_string (get_intersection (d)));
		--put_line ("out of range: " & boolean'image (out_of_range (d)));
	--end loop;

	null;
	
end get_intersection;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
