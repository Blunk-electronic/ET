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

procedure testbench is

	use functions_float;


 --circle: C: (x/y)  18.0233/ 0.0000 R: 0.3500
--D C-A: 0.4208 -86.363
 --D: 0.0708

	
	A : type_arc;

--	P0 : type_point := type_point (set (18.4, 0.03));
	P : type_point := type_point (set (18.20, 0.03));
	--P2 : type_point := type_point (set (18.0233, 0.0));
	--P3 : type_point := type_point (set (18.0, 0.0));

	--P : type_point;
	
	x : type_distance;

	dp : type_distance_polar;
begin
	new_line;

	A.start_point := type_point (set (x =>  18.05, y =>  0.48));
	A.end_point   := type_point (set (x =>  18.05, y => -0.42));
	A.center      := type_point (set (x =>  18.05, y =>  0.03));
	A.direction   := CCW;

	if is_valid (A) then
		--put_line ("valid");
		
		--put_line (to_string (A));

		x := get_break (
			init		=> 18.05,
			place		=> AFTER,
			obstacle	=> (ARC, A),
			clearance	=> 0.35,
			lth			=> 1);

		put_line (to_string (x));


		--put_line ("P:" & to_string (P));
		
		--for i in 1 .. 20 loop

			--put_line ("P:" & to_string (P));
			----put_line (to_string (A));

			--dp := get_shortest_distance (P , A);
			--put_line (to_string (dp));
			--new_line;

			--P := type_point (move (P, 180.0, 0.05));
		--end loop;

		
	else
		put_line ("invalid");
	end if;
	

	
end testbench;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
