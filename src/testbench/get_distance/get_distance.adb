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

procedure get_distance is

	use functions_float;


	--P : type_point := type_point (set (24.8887514, 29.7050000));
	--S : type_point := type_point (set (24.9999996, 32.2500000));
	--E : type_point := type_point (set (24.9999999, 30.7950000));
	-- D: 0.3112487274
	-- out of range: TRU
	
	--P : type_point := type_point (set (160.9, 0.0));
	--S : type_point := type_point (set (160.25, -19.705));
	--E : type_point := type_point (set (159.25, 0.2950));
	--D: 1.4334592930
	--out of range: FALSE

	
	--P : type_point := type_point (set (10.7999999, 0.0));
	--S : type_point := type_point (set (10.2499989, 0.3949991));
	--E : type_point := type_point (set (10.2499999, 1.8500000));
	-- D: 0.3500012535
	-- out of range: TRUE
	
	P : type_point := type_point (set (210.9, 0.0));
	S : type_point := type_point (set (210.25, -13.535));
	E : type_point := type_point (set (190.25, 93.265));
	--D: 2.9336481243
	--out of range: FALSE

	
	L : type_line := (S, E);
	
	d : type_distance_point_line;
	
begin
	--P := type_point (round (P));
	--put_line (to_string (P));
	--L := type_line (round (L));
	--put_line (to_string (L));

	for i in 1 .. 1 loop
		new_line;
		P := type_point (move (P, 180.0, 0.2));
		put_line ("P:" & to_string (P));
		
		d := get_distance (P, L, WITH_END_POINTS);
		put_line ("D:" & to_string (get_distance (d)));
		put_line ("out of range: " & boolean'image (out_of_range (d)));
	end loop;


end get_distance;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
