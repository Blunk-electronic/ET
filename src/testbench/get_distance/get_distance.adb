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
-- This program computes the distance of a point P to a conductor segment.
-- The conductor segment is a line of width W. It has a start point S and
-- an end point E.



with ada.text_io;				use ada.text_io;

with et_geometry;				use et_geometry;
with et_pcb_coordinates;		use et_pcb_coordinates;
with et_board_shapes_and_text;

use et_pcb_coordinates.pac_geometry_brd;
use et_board_shapes_and_text.pac_shapes;

with et_design_rules;			use et_design_rules;
with et_packages;				use et_packages;
with et_routing;				use et_routing;

procedure get_distance is

	use pac_functions_distance;

	P, p_init : type_point := type_point (set ( 10.000000, -1.0000000));
	--P, p_init : type_point := type_point (set ( 10.00000000, -0.996));
	S, s_init : type_point := type_point (set (  0.000000, 0.00000000));
	E, e_init : type_point := type_point (set (  5.000000, 5.00000000));
	--E, e_init : type_point := type_point (set (  5.000000, 0.00000000));

	
	L : type_line := (S, E);
	
	d : type_rotation;

	-- the center line of the conductor segment:
	cl, cl_init : et_packages.type_conductor_line;

	-- the width of the conductor segment:
	W : type_track_width := 0.15;
	
	segment : type_conductor_line_segment;
	distance : type_distance;

	test_no : natural := 0;
	errors : natural := 0;

	procedure next_test is begin
		test_no := test_no + 1;
		new_line;
		put_line ("TEST No:" & natural'image (test_no));
	end next_test;
		
	
	function inside_segment (d : in type_distance)
		return boolean
	is begin
		if d <= zero then
			put_line ("ERROR: P" & to_string (P) & " is inside segment !");
			errors := errors + 1;
			return true;
		else
			return false;
		end if;
	end inside_segment;
		
begin

	cl_init := (S, E, W);

	
	put_line ("line width" & to_string (W));

	
	--put_line ("segment " & to_string (segment));
	
	--distance := get_shortest_distance (P,segment);
	--new_line;
	
	--put_line ("P:" & to_string (P) & " / D:" & to_string (distance));

	
	next_test;
	put_line ("P MOVES UPWARDS");
	P := p_init;
	cl := cl_init;
	segment := to_line_segment (cl);
	put_line (to_string (segment));
	for i in 1 .. 7_000 loop
		P := type_point (move (P, 90.0, 0.001));
		
		distance := get_shortest_distance (P,segment);
		put_line (positive'image (i) & ": P:" & to_string (P) & " / D:" & to_string (distance));
		--put_line ("P:" & to_string (P) & " / D:" & to_string (distance));

		if inside_segment (distance) then
			exit;
		end if;
	end loop;
	-- 6000: P: (x/y)  10.00000000/ 5.00000000 / D: 4.92500000
	
	next_test;
	d := 125.0;
	put_line ("P MOVES IN DIRECTION " & to_string (d) & " DEG");
	P := p_init;
	cl := cl_init;
	put_line ("CL: " & to_string (cl));
	segment := to_line_segment (cl);
	for i in 1 .. 10_000 loop
		P := type_point (move (P, d, 0.001));
		
		distance := get_shortest_distance (P,segment);
		put_line (positive'image (i) & ": P:" & to_string (P) & " / D:" & to_string (distance));

		if inside_segment (distance) then
			exit;
		end if;
	end loop;
	-- 7783: P: (x/y)  5.53582686/ 5.37544445 / D: 0.57926979

	
	next_test;
	put_line ("P ROTATES ABOUT THE ORIGIN");
	P := type_point (set ( 10.000000, -0.0000000));
	cl := cl_init;
	segment := to_line_segment (cl);	
	for i in 1 .. 360 loop
		rotate_by (P, 1.0);
		
		distance := get_shortest_distance (P,segment);

		put_line (positive'image (i) & ": P:" & to_string (P) 
		--& " A:" & to_string (get_angle (get_distance (origin, P)))
			& " A:" & to_string (get_rotation (P)) 
			& " / D:" & to_string (distance));

		if distance <= zero then
			put_line ("ERROR");
			exit;
		end if;
	end loop;
	-- 45: P: (x/y)  7.07106780/ 7.07106780 A: 45.0000 / D: 2.85393217
	-- 225: P: (x/y)  -7.07106779/ -7.07106779 A: -135.0000 / D: 9.92499997
	
	-- The segment rotates about the origin.
	-- The start point is fixed to the origin.
	-- The end point rotates.
	next_test;
	put_line ("END POINT OF SEGMENT ROTATES ABOUT THE ORIGIN");
	P := type_point (set ( 10.000000, -0.0000000));
	S := s_init;
	E := e_init;
	for i in 1 .. 360 loop

		rotate_by (E, 1.0);

		cl := (S, E, W);
		segment := to_line_segment (cl);
		
		distance := get_shortest_distance (P,segment);

		put_line (positive'image (i) & ": CL: " & to_string (cl)
			& " / D:" & to_string (distance));

		if distance <= zero then
			put_line ("ERROR");
			exit;
		end if;
	end loop;
	-- 135: CL: line: S: (x/y)  0.00000000/ 0.00000000 / E: (x/y)  -7.07106774/ 0.00000000 / D: 9.92000000

	new_line;
	if errors = 0 then
		put_line ("PASS");
	else
		put_line ("FAIL ! errors:" & natural'image (errors));
	end if;
	
end get_distance;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
