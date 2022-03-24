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

procedure winding is

	--test_count : constant positive := 23;

	use pac_geometry_brd;
	use pac_geometry_2;
	use pac_polygons;

	
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
	B_default_cw : constant string := "line 0 0 line 0 100 line 100 100 line 100 0";
	


	procedure do_test (
		s 			: in string;
		w_expect	: in type_direction_of_rotation)
	is 
		F : type_fields_of_line;
		P : type_polygon;	
		w_actual : type_direction_of_rotation;
	begin
		next_index;

		F := read_line (line => s, comment_mark => "#");
		P := type_polygon (to_polygon (F));
		w_actual := get_winding (P);


		-- USED TO TEST SET_WINDING:
		
		--new_line;
		--put_line (to_string (P));
		
		--if w_actual = CCW then
			--put_line ("winding is CCW. converting to CW ...");
			--set_winding (P, CW);
		--else
			--put_line ("winding is CW. converting to CCW ...");
			--set_winding (P, CCW);
		--end if;
		
		--put_line ("new " & to_string (P));

		
		if w_actual /= w_expect then
			count_error;
			new_line;
			put_line ("ERROR. Test" & type_index'image (idx) & ":");
			put_line ("expected: " & to_string (w_expect));
			put_line ("found   : " & to_string (w_actual));
			put_line (to_string (P));
		end if;
	end do_test;

	
begin

	-- TEST 1:
	do_test (
		s => B_default,
		w_expect => CCW);

	
	-- TEST 2:
	do_test (
		s => B_default_cw,
		w_expect => CW);


	-- TEST 3:
	do_test (
		s => "line 20 -10 line 30 -10 line 110 50 line 30 110 line 20 110 line 25 50",
		w_expect => CCW);

	
	-- TEST 4:
	do_test (
		s => "line 40 -10 line 120 -10 line 120 50 line 80 50 line 80 -5 line 60 -5 line 60 50 line 40 50",
		w_expect => CCW);
	

	-- TEST 5:
	do_test (
		s => "line 10 10 line 10 -10 line 150 -10 line 150 110 line 90 110 line 90 90 "
			& "line 60 90 line 60 110 line 40 110 line 40 80 line 90 80 line 90 10 line 110 10 "
			& "line 110 -5 line 20 -5 line 20 10",
		w_expect => CCW);
	
	---------------------	

	new_line;
	put_line ("--------------");
	put_line ("ERRORS total:" & natural'image (errors));

	
end winding;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
