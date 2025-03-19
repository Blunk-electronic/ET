------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              TESTBENCH                                   --
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
with ada.strings;				use ada.strings;

with et_geometry_1;
with et_geometry_2a;

with et_primitive_objects;		use et_primitive_objects;


procedure test is

	distance_digits_left  : constant :=  5;
	--distance_digits_right : constant := 10; -- 0.1pm
	distance_digits_right : constant := 4; -- 1um
	
	distance_smallest : constant := 1.0 / (10 ** distance_digits_right);
	
	type type_distance_model is delta distance_smallest 
		digits distance_digits_left + distance_digits_right
		range - 0.1 * (10 ** distance_digits_left) .. 
			  + 0.1 * (10 ** distance_digits_left);

	
		
	-- Angle or rotation is in mathematical sense, means:
	-- positive rotation -> counter clock wise
	-- negative rotation -> clock wise

	rotation_digits_left  : constant := 3;
	rotation_digits_right : constant := 7;

	rotation_smallest : constant := 1.0 / (10 ** rotation_digits_right);
	type type_rotation_model is delta rotation_smallest 
		digits rotation_digits_left + rotation_digits_right
		range -360.0 + rotation_smallest .. 360.0 - rotation_smallest;

		

	
	type type_float is digits 12;
	
	package pac_geometry_brd is new et_geometry_1 (
		type_float	=> type_float,

		-- For assumed greatest numbers of 999.999..
		-- we have 3 digits left and 9 digits right of comma.
		-- This leads to an accuracy of:											  
		accuracy	=> 1.0E-14
		-- CS: For numbers greater 999.9 this accuracy is useless.
		);
	
	use pac_geometry_brd;



	-- instantiation of the geometry_2 package:
	package pac_geometry_2 is new et_geometry_2a (
		pac_geometry_1			=> pac_geometry_brd,
		type_distance			=> type_distance_model,
		axis_max				=> +1_000.0,
		axis_min				=> -1_000.0,
		type_rotation			=> type_rotation_model
		);
	
	use pac_geometry_2;



	A : type_arc;
	
	-- A, B : type_arc_angles;

	--	P : type_vector_model := set (65.0, 45.0);
	P : type_vector_model := set (51.0, 25.0);

	D : type_distance_polar;
begin

	-- test 1a:
	A.center := 		set (50.0, 25.0);
	A.start_point :=	set (50.0,  0.0);
	A.end_point :=	set (50.0, 50.0);
	A.direction := CCW;

-- goto start_test;	
	
<<start_test>>
	
	put_line (to_string (A));
	put_line ("P: " & to_string (P));

	D := get_shortest_distance (A, P);
	
	-- segments := split_arc (arc, 3);

	-- output_segments (segments);

	-- Z := get_zone (to_arc_coarse (arc), P);

	-- put_line ("Z " & to_string (Z));
	
end test;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
