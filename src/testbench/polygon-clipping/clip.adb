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
with et_string_processing;		use et_string_processing;

procedure clip is

	use pac_geometry_2;
	use pac_polygons;
	use pac_polygon_clipping;


	use pac_clipped;
	EXP : pac_clipped.list;
	

	-- A: to be clipped
	-- B: clipping

	type type_test is record
		A, B	: unbounded_string;
		result_expected : pac_clipped.list;
		result_actual : pac_clipped.list;
	end record;

	type type_test_array is array (1..17) of type_test;
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
	

	procedure init_test is begin
		EXP.clear;
	end init_test;
		

	procedure add_to_expect (
		clipped : in out pac_clipped.list;
		s		: in string)
	is
		F : type_fields_of_line;
	begin
		F := read_line (line => s, comment_mark => "#");
		clipped.append (type_polygon (to_polygon (F)));
	end add_to_expect;
	
	
	procedure make_set (
		A, B	: in string;
		expect	: in pac_clipped.list)
	is begin
		next_index;
		set (idx).A := to_unbounded_string (A);
		set (idx).B := to_unbounded_string (B);
		set (idx).result_expected := expect;
	end;
		
	
	procedure make_test is 
		F	: type_fields_of_line;
		A, B: type_polygon;

		procedure query_polygon (p : in pac_clipped.cursor) is begin
			put_line (to_string (element (p)));
		end query_polygon;

	begin
		for i in set'first .. idx loop
			new_line;
			put_line ("TEST:" & natural'image (i));
			put_line ("-------------");
			
			F := read_line (line => to_string (set(i).A), comment_mark => "#");
			A := type_polygon (to_polygon (F));
			--put_line ("A: " & to_string (A));

			F := read_line (line => to_string (set(i).B), comment_mark => "#");
			B := type_polygon (to_polygon (F));
			--put_line ("B: " & to_string (B));

			set (i).result_actual := clip (A, B);

			if set (i).result_actual /= set (i).result_expected then
				new_line;
				put_line ("ERROR ! Test No.:" & type_index'image (i));
				new_line;
				put_line ("A: " & to_string (A));
				new_line;
				put_line ("B: " & to_string (B));
				new_line;
				
				put_line ("EXPECTED:");
				set(i).result_expected.iterate (query_polygon'access);
				new_line;
				
				put_line ("FOUND:");
				set (i).result_actual.iterate (query_polygon'access);

				-- repeat test in debug mode so that details are shown:
				new_line;
				put_line ("DEBUG DETAILS:");
				put_line ("--------------");
				set (i).result_actual := clip (A, B, true);

				count_error;
			end if;
		end loop;
			
	end make_test;

	
begin

	-- TEST 1:
	init_test;
	add_to_expect (EXP, "line 100 50 line 50 50 line 50 0 line 100 0");
	
	make_set (
		A => "line 50 0 line 100 0 line 100 50 line 50 50",
		B => B_default,
		expect => EXP);
	-- go


	-- TEST 2:
	init_test;
	add_to_expect (EXP, "line 100 50 line 50 50 line 50 0 line 100 0");

	make_set (
		A => "line 50 0 line 101 0 line 101 50 line 50 50",
		B => B_default,
		expect => EXP);
	-- go

	
	-- TEST 3:
	init_test;
	add_to_expect (EXP, "line 100 20 line 80 20 line 80 10 line 100 10");
	
	make_set (
		A => "line 80 10 line 150 10 line 150 20 line 80 20",
		B => B_default,
		expect => EXP);
	-- go


	-- TEST 4:
	init_test;
	add_to_expect (EXP, "line 1 0.5 line 1 1 line 0.5 1 line 0.5 0.5");
	
	make_set (
		A => "line 0 0 line 1 0 line 1 1 line 0 1",
		B => "line 0.5 0.5 line 1.5 0.5 line 1.5 1.5 line 0.5 1.5",
		expect => EXP);
	-- go

	

	-- TEST 5:
	init_test;
	add_to_expect (EXP, "line 100 50 line 80 50 line 80 0 line 100 0");
	add_to_expect (EXP, "line 60 0 line 60 50 line 40 50 line 40 0");
	
	make_set (
		A => "line 40 -10 line 120 -10 line 120 50 line 80 50 line 80 -5 line 60 -5 line 60 50 line 40 50",
		B => B_default,
		expect => EXP);
	-- go


	-- TEST 6:
	init_test;
	add_to_expect (EXP, "line 43.3333333333 0 line 100 42.5 line 100 57.5 " 
				   & "line 43.3333333333 100 line 20.8333333333 100 line 25 50 line 20.8333333333 0");
	
	make_set (
		A => "line 20 -10 line 30 -10 line 110 50 line 30 110 line 20 110 line 25 50",
		B => B_default,
		expect => EXP);
	-- go

	

	-- TEST 7:
	init_test;
	add_to_expect (EXP, "line 50 0 line 50 100 line 40 100 line 40 0");
	
	make_set (
		A => "line 40 -10 line 50 -10 line 50 110 line 40 110",
		B => B_default,
		expect => EXP);
	-- go


	
	-- TEST 8:
	init_test;
	add_to_expect (EXP, "line 50 0 line 50 50 line 0 50 line 0 0");
	
	make_set (
		A => "line 0 0 line 50 0 line 50 50 line 0 50",
		B => B_default,
		expect => EXP);
	-- go


	-- TEST 9:
	init_test;
	add_to_expect (EXP, "line 50 0 line 50 50 line 30 50 line 30 0");
	
	make_set (
		A => "line 30 0 line 50 0 line 50 50 line 30 50",
		B => B_default,
		expect => EXP);
	-- go



	-- TEST 10:
	init_test;
	add_to_expect (EXP, "line 100 50 line 50 50 line 50 0 line 100 0");
	
	make_set (
		A => "line 50 0 line 100 0 line 101 50 line 50 50",
		B => B_default,
		expect => EXP);
	-- go


	-- TEST 11:
	init_test;
	add_to_expect (EXP, "line 100 0 line 80 20 line 80 40 line 100 40 line 100 60 line 50 60 line 50 0");
	
	make_set (
		A => "line 50 0 line 100 0 line 80 20 line 80 40 line 110 40 line 110 60 line 50 60",
		B => B_default,
		expect => EXP);
	-- go


	-- TEST 12:
	init_test;
	add_to_expect (EXP, "line 60 0 line 60 20 line 100 20 line 100 60 line 60 60 line 100 100 line 0 100 line 50 50 line 50 0");
	
	make_set (
		A => "line 50 -10 line 60 -10 line 60 20 line 120 20 line 120 60 line 60 60 line 105 105 line -5 105 line 50 50",
		B => B_default,
		expect => EXP);
	-- go


	-- TEST 13:
	init_test;
	add_to_expect (EXP, "line 100 100 line 0 0 line 100 0");
	
	make_set (
		A => "line -5 -5 line 105 -5 line 105 105",
		B => B_default,
		expect => EXP);
	

	-- TEST 14:
	init_test;
	add_to_expect (EXP, "line 50 0 line 80 50 line 70 60 line 40 10");
	
	make_set (
		A => "line 50 0 line 80 50 line 70 60 line 40 10",
		B => B_default,
		expect => EXP);


	-- TEST 15:
	init_test;
	-- we expect nothing as the polygons do not overlap
	
	make_set (
		A => "line 50 0 line 80 -50 line 70 -60 line 40 -10",
		B => B_default,
		expect => EXP);


	-- TEST 16:
	init_test;
	-- we expect nothing as the polygons do not overlap
	
	make_set (
		A => "line 200 10 line 250 10 line 250 50",
		B => B_default,
		expect => EXP);



	-- TEST 17:
	init_test;
	add_to_expect (EXP, "line 50 0 line 80 50 line 70 60 line 40 10");
	
	make_set (
		A => "line 70 0 line 90 -20 line 120 50 line 100 70",
		B => B_default,
		expect => EXP);

	
	---------------------	
	make_test;

	new_line;
	put_line ("--------------");
	put_line ("ERRORS total:" & natural'image (errors));

	
end clip;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
