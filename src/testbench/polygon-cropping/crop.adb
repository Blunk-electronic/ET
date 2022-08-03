------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              TESTBENCH                                   --
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
-- DESCRIPTION:
-- 

with ada.text_io;				use ada.text_io;
with ada.strings.unbounded;		use ada.strings.unbounded;

with et_geometry;				use et_geometry;
with et_pcb_coordinates;		use et_pcb_coordinates;
with et_board_shapes_and_text;	use et_board_shapes_and_text;
with et_contour_to_polygon;		use et_contour_to_polygon;
with et_string_processing;		use et_string_processing;

procedure crop is

	--test_count : constant positive := 26;
	test_count : constant positive := 26;

	use pac_geometry_brd;
	use pac_geometry_2;

	use pac_contours;
	use pac_polygons;
	use pac_polygon_cropping;


	use pac_cropped;
	EXP_list : pac_cropped.list;

	-- A: cropping
	-- B: to be cropped

	type type_test is record
		A, B	: unbounded_string;
		result_expected : type_crop;
		result_actual : type_crop;
	end record;

	
	type type_test_array is array (1 .. test_count) of type_test;
	set : type_test_array;

	subtype type_index is natural range 0 .. type_test_array'last;
	idx : type_index := 0;

	procedure next_index is begin
		idx := idx + 1;
	end next_index;


	errors : natural := 0;

	procedure count_error is begin
		errors := errors + 1;
	end count_error;
		



	
	B_default : constant string := "line 0 0 line 100 0 line 100 100 line 0 100";
	B_default_vertices : constant string := "0 0  100 0  100 100  0 100";
	
	B_default_cw : constant string := "line 0 0 line 0 100 line 100 100 line 100 0";
	B_default_cw_vertices : constant string := "0 0  0 100  100 100  100 0";
	

	procedure init_test is begin
		EXP_list.clear;
	end init_test;
		

	procedure add_to_expect (
		s : in string)
	is
		P : type_polygon;
		W : type_direction_of_rotation;
	begin
		P := to_polygon (s);
		-- CS set winding ?
		
		W := get_winding (P);
		--put_line ("winding: " & to_string (W));
		
		EXP_list.append (P);
	end add_to_expect;
	
	
	procedure make_set (
		A, B	: in string; -- contours
		expect	: in type_crop)
	is begin
		next_index;
		set (idx).A := to_unbounded_string (A);
		set (idx).B := to_unbounded_string (B);
		set (idx).result_expected := expect;
	end;
		
	
	procedure make_test is 
		C : type_contour;
		A, B: type_polygon;

		procedure query_polygon (p : in pac_cropped.cursor) is begin
			put_line (to_string (element (p)));
		end query_polygon;

	begin
		for i in set'first .. idx loop
			new_line;
			put_line ("TEST:" & natural'image (i));
			put_line ("-------------");
			
			C := type_contour (to_contour (to_string (set(i).A)));
			-- CS set winding ?
			A := to_polygon (C, fab_tolerance);
			--put_line ("A: " & to_string (A));
			--put_line ("A shortest edge: " & to_string (get_shortest_edge (A)));

			C := type_contour (to_contour (to_string (set(i).B)));
			-- CS set winding ?
			B := to_polygon (C, fab_tolerance);
			--put_line ("B: " & to_string (B));
			--put_line ("B shortest edge: " & to_string (get_shortest_edge (B)));

			
			set (i).result_actual := crop (A, B);

			-- Use this statement if more debug messages required:
			--set (i).result_actual := crop (A, B, true);

			
			-- On error show details:
			if set (i).result_actual /= set (i).result_expected then
				new_line;
				put_line ("ERROR ! Test No.:" & type_index'image (i));
				new_line;
				put_line ("A: " & to_string (A));
				new_line;
				put_line ("B: " & to_string (B));
				new_line;
				
				put_line ("EXPECTED:");
				if set (i).result_expected.exists then
					if set(i).result_expected.fragments.is_empty then
						put_line ("no area");
					else
						set(i).result_expected.fragments.iterate (query_polygon'access);
					end if;
				else
					put_line ("no cropping possible");
				end if;
				new_line;
				
				put_line ("FOUND:");
				if set (i).result_actual.exists then
					if set (i).result_actual.fragments.is_empty then
						put_line ("no area");
					else
						set (i).result_actual.fragments.iterate (query_polygon'access);
					end if;
				else
					put_line ("no cropping possible");
				end if;
				
				
				-- repeat test in debug mode so that details are shown:
				new_line;
				put_line ("DEBUG DETAILS:");
				put_line ("--------------");
				set (i).result_actual := crop (A, B, true);

				count_error;
			end if;
		
		end loop;

		
		--exception
			--when others => null;

		
	end make_test;

	
begin

	-- TEST 1:
	init_test;
	add_to_expect ("50 0  50 50  100 50  100 100  0 100  0 0");
	
	make_set (
		A => "line 50 0 line 100 0 line 100 50 line 50 50",
		B => B_default,
		expect => (exists => true, status => A_OVERLAPS_B, fragments => EXP_list, count => 1));
	-- go

--goto skip;
	
	
	-- TEST 2:
	init_test;
	add_to_expect ("50 0  50 50  100 50  100 100  0 100  0 0");

	make_set (
		A => "line 50 0 line 101 0 line 101 50 line 50 50",
		B => B_default,
		expect => (exists => true, status => A_OVERLAPS_B, fragments => EXP_list, count => 1));
	-- go

--goto skip;
	
	
	-- TEST 3:
	init_test;
	add_to_expect ("100 10  80 10  80 20  100 20  100 100  0 100  0 0  100 0");
	
	make_set (
		A => "line 80 10 line 150 10 line 150 20 line 80 20",
		B => B_default,
		expect => (exists => true, status => A_OVERLAPS_B, fragments => EXP_list, count => 1));
	-- go


	-- TEST 4:
	init_test;
	add_to_expect ("0.5 1  1 1  1 0.5  1.5 0.5  1.5 1.5  0.5 1.5");
	
	make_set (
		A => "line 0 0 line 1 0 line 1 1 line 0 1",
		B => "line 0.5 0.5 line 1.5 0.5 line 1.5 1.5 line 0.5 1.5",
		expect => (exists => true, status => A_OVERLAPS_B, fragments => EXP_list, count => 1));
	-- go

	

	-- TEST 5:
	init_test;
	add_to_expect ("40 0  40 50  60 50  60 0  80 0  80 50  100 50  100 100  0 100  0 0");
	
	make_set (
		A => "line 40 -10 line 120 -10 line 120 50 line 80 50 line 80 -5 line 60 -5 line 60 50 line 40 50",
		B => B_default,
		expect => (exists => true, status => A_OVERLAPS_B, fragments => EXP_list, count => 1));
	-- go


	-- TEST 6:
	init_test;
	add_to_expect ("2.08333333333333333E+01 0  25 50  2.08333333333333333E+01 100  0 100  0 0");
	add_to_expect ("100 42.5  4.33333333333333333E+01 0  100 0");
	add_to_expect ("4.33333333333333333E+01 100  100 57.5  100 100");

	
	make_set (
		A => "line 20 -10 line 30 -10 line 110 50 line 30 110 line 20 110 line 25 50",
		B => B_default,
		expect => (exists => true, status => A_OVERLAPS_B, fragments => EXP_list, count => 3));
	-- go

	

	-- TEST 7:
	init_test;
	add_to_expect ("40 0  40 100  0 100  0 0");
	add_to_expect ("50 100  50 0  100 0  100 100");

	
	make_set (
		A => "line 40 -10 line 50 -10 line 50 110 line 40 110",
		B => B_default,
		expect => (exists => true, status => A_OVERLAPS_B, fragments => EXP_list, count => 2));
	-- go

--goto skip;
	
	-- TEST 8:
	init_test;
	add_to_expect ("0 50  50 50  50 0  100 0  100 100  0 100");
	
	make_set (
		A => "line 0 0 line 50 0 line 50 50 line 0 50",
		B => B_default,
		expect => (exists => true, status => A_OVERLAPS_B, fragments => EXP_list, count => 1));
	-- go


	-- TEST 9:
	init_test;
	add_to_expect ("30 0  30 50  50 50  50 0  100 0  100 100  0 100  0 0");
	
	make_set (
		A => "line 30 0 line 50 0 line 50 50 line 30 50",
		B => B_default,
		expect => (exists => true, status => A_OVERLAPS_B, fragments => EXP_list, count => 1));
	-- go



	-- TEST 10:
	init_test;
	add_to_expect ("50 0  50 50  100 50  100 100  0 100  0 0");
	
	make_set (
		A => "line 50 0 line 100 0 line 101 50 line 50 50",
		B => B_default,
		expect => (exists => true, status => A_OVERLAPS_B, fragments => EXP_list, count => 1));
	-- go


	-- TEST 11:
	init_test;
	add_to_expect ("50 0  50 60  100 60  100 100  0 100  0 0");
	add_to_expect ("100 40  80 40  80 20  100 0");

	
	make_set (
		A => "line 50 0 line 100 0 line 80 20 line 80 40 line 110 40 line 110 60 line 50 60",
		B => B_default,
		expect => (exists => true, status => A_OVERLAPS_B, fragments => EXP_list, count => 2));
	-- go


	-- TEST 12:
	init_test;
	add_to_expect ("50 0  50 50  0 100  0 0");
	add_to_expect ("100 20  60 20  60 0  100 0");
	add_to_expect ("100 100  60 60  100 60");

	
	make_set (
		A => "line 50 -10 line 60 -10 line 60 20 line 120 20 line 120 60 line 60 60 line 105 105 line -5 105 line 50 50",
		B => B_default,
		expect => (exists => true, status => A_OVERLAPS_B, fragments => EXP_list, count => 3));
	-- go


	-- TEST 13:
	init_test;
	add_to_expect ("0 0  100 100  0 100");
	
	make_set (
		A => "line -5 -5 line 105 -5 line 105 105",
		B => B_default,
		expect => (exists => true, status => A_OVERLAPS_B, fragments => EXP_list, count => 1));
	

	-- TEST 14:
	init_test;
	-- Cropping not possible since A is completly inside B
	make_set (
		A => "line 50 0 line 80 50 line 70 60 line 40 10",
		B => B_default,
		expect => (exists => false, status => A_INSIDE_B));

--goto skip;
	

	-- TEST 15:
	init_test;
	add_to_expect (B_default_vertices);
	-- B is not cropped at all because A is outside B.
	
	make_set (
		A => "line 50 0 line 80 -50 line 70 -60 line 40 -10",
		B => B_default,
		expect => (exists => true, status => A_DOES_NOT_OVERLAP_B, fragments => EXP_list, count => 1));


	
	-- TEST 16:
	init_test;
	add_to_expect (B_default_vertices);
	-- B is not cropped at all because A is outside B.
	
	make_set (
		A => "line 200 10 line 250 10 line 250 50",
		B => B_default,
		expect => (exists => true, status => A_DOES_NOT_OVERLAP_B, fragments => EXP_list, count => 1));



	-- TEST 17:
	init_test;
	add_to_expect ("50 0  100 50  100 100  0 100  0 0");
	
	make_set (
		A => "line 50 0 line 110 -20 line 120 0 line 110 60",
		B => B_default,
		expect => (exists => true, status => A_OVERLAPS_B, fragments => EXP_list, count => 1));


	
	-- TEST 18:
	init_test;
	add_to_expect ("0 0  25 50  50 50  50 0  100 0  100 100  0 100");
	
	make_set (
		A => "line 0 0 line 25 -50 line 50 -50 line 50 50 line 25 50",
		B => B_default,
		expect => (exists => true, status => A_OVERLAPS_B, fragments => EXP_list, count => 1));


	
	-- TEST 19:
	init_test;
	add_to_expect ("10 0  20 10  80 10  90 0  100 0  100 100  0 100  0 0");
	
	make_set (
		A => "line 10 0 line 10 -10 line 90 -10 line 90 0 line 80 10 line 20 10",
		B => B_default,
		expect => (exists => true, status => A_OVERLAPS_B, fragments => EXP_list, count => 1));


	-- TEST 20:
	init_test;
	-- We expect B to disappear since it is completely inside A.
	
	make_set (
		A => "line 0 0 line 100 0 line 100 100 line 0 100",
		B => "line 10 10 line 90 10 line 90 90 line 10 90",
		expect => (exists => true, status => B_INSIDE_A, fragments => EXP_list, count => 0));



	-- TEST 21:
	init_test;
	-- We expect B to disappear since it is congruent with A.
	
	make_set (
		B => B_default,
		A => B_default,
		expect => (exists => true, status => CONGRUENT, fragments => EXP_list, count => 0));
	-- go

--goto skip;
	

	-- TEST 22:
	init_test;
	add_to_expect ("10 0  10 10  20 10  20 0  100 0  100 10 "
		& " 90 10  90 80  40 80  40 100  0 100  0 0");
	
	add_to_expect ("60 100  60 90  90 90  90 100");
	
	make_set (
		A => "line 10 10 line 10 -10 line 150 -10 line 150 110 line 90 110 line 90 90 "
			& "line 60 90 line 60 110 line 40 110 line 40 80 line 90 80 line 90 10 line 110 10 "
			& "line 110 -5 line 20 -5 line 20 10",
		B => B_default,
		expect => (exists => true, status => A_OVERLAPS_B, fragments => EXP_list, count => 2));
	-- go


	-- TEST 23:
	init_test;
	add_to_expect (B_default_vertices);
	-- The right edge of A overlaps the left edge of B. Both edges have the same length.
	-- A overlaps B here. But B remains unchanged. B will not be cropped.
	
	make_set (
		A => "line 100 0 line 200 0 line 200 100 line 100 100",
		B => B_default,
		expect => (exists => true, status => A_OVERLAPS_B, fragments => EXP_list, count => 1));



	-- TEST 24:
	init_test;
	add_to_expect ("0 0  100 0  100 10  100 90  100 100  0 100");
	-- The right edge of A overlaps the left edge of B partly.
	-- A overlaps B here. But B remains unchanged. B will not be cropped.
	
	make_set (
		A => "line 100 10 line 200 10 line 200 90 line 100 90",
		B => B_default,
		expect => (exists => true, status => A_OVERLAPS_B, fragments => EXP_list, count => 1));



	-- TEST 25:
	init_test;
	add_to_expect ("0 0  100 0  100 10  100 100  0 100");
	-- The right edge of A overlaps the left edge of B partly.
	-- A overlaps B here. But B remains unchanged. B will not be cropped.
	
	make_set (
		A => "line 100 10 line 200 10 line 200 110 line 100 110",
		B => B_default,
		expect => (exists => true, status => A_OVERLAPS_B, fragments => EXP_list, count => 1));




	-- TEST 26:
	init_test;
	add_to_expect ("0 0  0 100  100 100  100 0  110 0  110 110  -10 110  -10 0");
	-- B wraps around A. Edges do overlap.
	-- B remains unchanged. B will not be cropped.
	
	make_set (
		A => B_default, -- cropping
		B =>  "line 0 0 line 0 100 line 100 100 line 100 0 line 110 0 line 110 110 line -10 110 line -10 0",
		expect => (exists => true, status => A_OVERLAPS_B, fragments => EXP_list, count => 1));

	
	-- TEST 23 (wie Test 3 aber mit polygon B in CW orientiert):
	--init_test;
	--add_to_expect ("100 10  80 10  80 20  100 20  100 100  0 100  0 0  100 0");
	
	--make_set (
		--A => "line 80 10 line 150 10 line 150 20 line 80 20",
		--B => B_default_cw,
		--expect => (exists => true, status => A_OVERLAPS_B, fragments => EXP_list));
	-- nogo
	
<<skip>>
	
	---------------------	
	make_test;

	new_line;
	put_line ("--------------");
	put_line ("ERRORS total:" & natural'image (errors));

	
end crop;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
