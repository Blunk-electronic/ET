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
with ada.strings;				use ada.strings;
with ada.strings.fixed;			use ada.strings.fixed;
with ada.strings.unbounded;		use ada.strings.unbounded;

with et_geometry;				use et_geometry;
with et_pcb_coordinates;		use et_pcb_coordinates;
with et_board_shapes_and_text;	use et_board_shapes_and_text;
with et_string_processing;		use et_string_processing;

procedure union is

	use pac_geometry_2;
	use pac_polygons;
	use pac_polygon_union;


	

	subtype type_index is natural range 0 .. 100;
	idx : type_index := 0;

	procedure next_index is begin
		idx := idx + 1;
	end next_index;


	errors : natural := 0;

	procedure count_error is begin
		errors := errors + 1;
	end count_error;
		



	
	B_default : constant string := "line 0 0 line 100 0 line 100 100 line 0 100";
	

	--procedure init_test is begin
		--EXP := (others => <>);
	--end init_test;
		

	
	
	procedure do_test (
		A, B, E	: in string)
	is 
		F : type_fields_of_line;
		PA, PB: type_polygon;

		PE : type_polygon;
		union_exists : boolean := false;
		
		ACT : type_union;


		procedure show_error is begin			
			new_line;
			put_line ("ERROR ! Test No.:" & type_index'image (idx));
			new_line;
			put_line ("A: " & to_string (PA));
			new_line;
			put_line ("B: " & to_string (PB));
			new_line;
				
			put_line ("EXPECTED:");
			if union_exists then
				put_line ("Union: " & to_string (PE));
			else
				put_line ("Union: none");
			end if;
			new_line;
				
			put_line ("FOUND:");
			if ACT.exists then
				put_line ("Union: " & to_string (ACT.union));
			else
				put_line ("Union: none");
			end if;
			new_line;
			
			-- repeat test in debug mode so that details are shown:
			new_line;
			put_line ("DEBUG DETAILS:");
			put_line ("--------------");
			ACT := union (PA, PB, true);

			count_error;
		end show_error;

		
	begin
		next_index;

		
		F := read_line (line => A, comment_mark => "#");
		PA := type_polygon (to_polygon (F));
		--put_line ("A: " & to_string (A));

		F := read_line (line => B, comment_mark => "#");
		PB := type_polygon (to_polygon (F));
		--put_line ("B: " & to_string (B));

		ACT := union (PA, PB); -- debug messages off
		--ACT := union (PA, PB, true); -- debug messages on

		
		if E'length > 0 then

			F := read_line (line => E, comment_mark => "#");
			PE := type_polygon (to_polygon (F));
			--put_line ("EXP: " & to_string (PE));
			union_exists := true;			
			
		else -- given "expected" was empty
			union_exists := false;
		end if;


		case union_exists is
			when TRUE =>
				if ACT.exists = union_exists then				
					if ACT.union = PE then
						null; -- actual same as expected -> ok
					else
						show_error;					
					end if;
				else
					show_error;
				end if;
				
			when FALSE =>
				if ACT.exists = union_exists then
					null; -- actual same as expected -> ok
				else
					show_error;
				end if;
		end case;
			
	end;
		



	
begin
	--goto test;
	
	
	-- TEST 1:
	do_test (
		A => "line 50 0 line 100 0 line 100 50 line 50 50",
		B => B_default,
		--E => B_default);
		E => "line 50 0 line 100 0 line 100 50 line 100 100 line 0 100 line 0 0");
	-- go

	--goto end_test;
	
	-- TEST 2:
	do_test (
		A => "line 50 0 line 101 0 line 101 50 line 50 50",
		B => B_default,
		E => "line 101 0 line 101 50 line 100 50 line 100 100 line 0 100 line 0 0 line 50 0 line 100 0");
	-- go

	
	-- TEST 3:
	do_test (
		A => "line 80 10 line 150 10 line 150 20 line 80 20",
		B => B_default,
		E => "line 150 10 line 150 20 line 100 20 line 100 100 line 0 100 line 0 0 line 100 0 line 100 10 ");
	-- go


	-- TEST 4:
	do_test (
		A => "line 0 0 line 1 0 line 1 1 line 0 1",
		B => "line 0.5 0.5 line 1.5 0.5 line 1.5 1.5 line 0.5 1.5",
		E => "line 0 0 line 1 0 line 1 0.5 line 1.5 0.5 line 1.5 1.5 line 0.5 1.5 line 0.5 1.0 line 0 1");
	-- go

	

	-- TEST 5:
	do_test (
		A => "line 40 -10 line 120 -10 line 120 50 line 80 50 line 80 -5 line 60 -5 line 60 50 line 40 50",
		B => B_default,
		E => "line 40 -10 line 120 -10 line 120 50 line 100 50 line 100 100 line 0 100 line 0 0 line 40 0");
	-- go


	-- TEST 6:
	do_test (
		A => "line 20 -10 line 30 -10 line 110 50 line 30 110 line 20 110 line 25 50",
		B => B_default,
		E =>  "line 20 -10 line 30 -10 line 43.3333333333 0 line 100 0 "
			& "line 100 42.5 line 110 50 line 100 57.5 "
			& "line 100 100 line 43.3333333333 100 line 30 110 line 20 110 line 20.8333333333 100 line 0 100 line 0 0 "
			& "line 20.8333333333 0 ");
	--go
	

	-- TEST 7:
	do_test (
		A => "line 40 -10 line 50 -10 line 50 110 line 40 110",
		B => B_default,
		E => "line 40 -10 line 50 -10 line 50 0 line 100 0 line 100 100 "
		   & "line 50 100 line 50 110 line 40 110 line 40 100 line 0 100 line 0 0 line 40 0");
	-- go


	
	-- TEST 8:
	do_test (
		A => "line 0 0 line 50 0 line 50 50 line 0 50",
		B => B_default,
		--E => B_default);
		E => "line 0 50 line 0 0 line 50 0 line 100 0 line 100 100 line 0 100");
	-- go


	-- TEST 9:
	do_test (
		A => "line 30 0 line 50 0 line 50 50 line 30 50",
		B => B_default,
		--E => B_default);
		E => "line 30 0 line 50 0 line 100 0 line 100 100 line 0 100 line 0 0");
	-- go


	-- TEST 10:
	do_test (
		A => "line 50 0 line 100 0 line 101 50 line 50 50",
		B => B_default,
		E => "line 101 50 line 100 50 line 100 100 line 0 100 line 0 0 line 50 0 line 100 0");
	-- go


	-- TEST 11:
	do_test (
		A => "line 50 0 line 100 0 line 80 20 line 80 40 line 110 40 line 110 60 line 50 60",
		B => B_default,
		E => "line 110 40 line 110 60 line 100 60 line 100 100 line 0 100 "
		   & "line 0 0 line 50 0 line 100 0 line 100 40");
	-- go


	-- TEST 12:
	do_test (
		A => "line 50 -10 line 60 -10 line 60 20 line 120 20 line 120 60 line 60 60 line 105 105 line -5 105 line 50 50",
		B => B_default,
		E => "line 50 -10 line 60 -10 line 60 0 line 100 0 line 100 20 line 120 20 "
		   & "line 120 60 line 100 60 line 100 100 line 105 105 line -5 105 "
		   & "line 0 100 line 0 0 line 50 0");	
	-- go


	-- TEST 13:
	do_test (
		A => "line -5 -5 line 105 -5 line 105 105",
		B => B_default,
		E => "line -5 -5 line 105 -5 line 105 105 line 100 100 line 0 100 line 0 0");
	

	-- TEST 14:
	do_test (
		A => "line 50 0 line 80 50 line 70 60 line 40 10",
		B => B_default,
		E => B_default);


	-- TEST 15:
	do_test (
		A => "line 50 0 line 80 -50 line 70 -60 line 40 -10",
		B => B_default,
		E => ""); -- we expect nothing because the polygons do not overlap


	-- TEST 16:
	do_test (
		A => "line 200 10 line 250 10 line 250 50",
		B => B_default,
		E => ""); -- we expect nothing because the polygons do not overlap


	-- TEST 17:
	do_test (
		A => "line 50 0 line 110 -20 line 120 0 line 110 60",
		B => B_default,
		E => "line 110 -20 line 120 0 line 110 60 line 100 50 line 100 100 line 0 100 line 0 0 line 50 0");

	
	-- TEST 18:
	do_test (
		A => "line 0 0 line 25 -50 line 50 -50 line 50 50 line 25 50",
		B => B_default,
		E => "line 25 -50 line 50 -50 lie 50 0 line 100 0 line 100 100 line 0 100 line 0 0");

	
	-- TEST 19:
	do_test (
		A => "line 10 0 line 10 -10 line 90 -10 line 90 0 line 80 10 line 20 10",
		B => B_default,
		E => "line 10 -10 line 90 -10 line 90 0 line 100 0 line 100 100 line 0 100 line 0 0 line 10 0");


	-- TEST 20:
	do_test (
		A => "line 10 10 line 90 10 line 90 90 line 10 90",
		B => "line 0 0 line 100 0 line 100 20 line 50 20 line 50 80 line 100 80 "
			& "line 100 100 line 0 100",
		E => "line 90 20 line 90 80 line 100 80 "
			& "line 100 100 line 0 100 line 0 0 line 100 0 line 100 20");


<<test>>
	
	-- TEST 21, wie test 5. A hat anderen Startpunkt:
	do_test (
		A => "line 60 -5 line 60 50 line 40 50 line 40 -10 line 120 -10 line 120 50 line 80 50 line 80 -5",
		B => B_default,
		E => "line 40 -10 line 120 -10 line 120 50 line 100 50 line 100 100 line 0 100 line 0 0 line 40 0");
	-- go

	
	-- TEST 22, wie test 5. A hat anderen Startpunkt:
	do_test (
		A => "line 80 50 line 80 -5 line 60 -5 line 60 50 line 40 50 line 40 -10 line 120 -10 line 120 50",
		B => B_default,
		E => "line 40 -10 line 120 -10 line 120 50 line 100 50 line 100 100 line 0 100 line 0 0 line 40 0");
	-- go
	
	
	-----------------------	

<<end_test>>

	new_line;
	put_line ("--------------");
	put_line ("TESTS  total:" & natural'image (idx));
	put_line ("ERRORS total:" & natural'image (errors));

	
end union;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16