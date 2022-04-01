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
with ada.strings.unbounded;
with ada.numerics;				use ada.numerics;

with et_geometry;				use et_geometry;
with et_pcb_coordinates;		use et_pcb_coordinates;
with et_board_shapes_and_text;	use et_board_shapes_and_text;
with et_string_processing;		use et_string_processing;

with et_contour_to_polygon;		use et_contour_to_polygon;


procedure to_polygon is

	use pac_geometry_brd;
	use pac_functions_distance;
	use pac_geometry_2;
	use pac_polygons;
	use pac_contours;


	subtype type_index is natural range 0 .. 100;	
	idx : type_index := 0;

	procedure next_index is begin
		idx := idx + 1;
	end next_index;


	errors : natural := 0;

	procedure count_error is begin
		errors := errors + 1;
	end count_error;
		


	procedure do_test (
		contour_segments	: in string;
		tolerance			: in type_distance_positive;				  
		polygon_expect		: in string)
	is 
		C_in : type_contour := type_contour (to_contour (contour_segments));
		
		P_exp : type_polygon := type_polygon (to_polygon (polygon_expect));
		P_actual : type_polygon;
	begin
		next_index;

		
		--put_line (to_string (P_in));
		--new_line;
		--put_line (to_string (P_exp));
		
		P_actual := to_polygon (C_in, tolerance, true); -- debug messages on

		
		--if P_actual /= P_exp then
		if not are_congruent (P_actual, P_exp) then
			count_error;
			new_line;
			put_line ("ERROR. Test" & type_index'image (idx) & ":");
			put_line ("expected: " & to_string (P_exp));
			new_line;
			put_line ("found   : " & to_string (P_actual));
		end if;
	end do_test;


begin


	do_test (
		contour_segments => "line 0 100 line 0 0 line 100 0 line 100 100",
		tolerance => 20.0, --fab_tolerance,
		polygon_expect => "0 0  100 0  100 100  0 100");


	do_test (
		contour_segments => "arc 50 0  0 0  cw " -- center 50/0 start 0/0 end 100/0
			& "line 100 0 line 100 100 line 0 100",

		tolerance => 20.0, --fab_tolerance,
			
		polygon_expect =>
			  "0 0 "
			& "14.6446609407 35.3553390593 "
			& "50 50 "
			& "85.3553390593 35.3553390593 "
			& "100 0 "
			& "100 100 " 
			& "0 100");

	
	---------------------	

	new_line;
	put_line ("--------------");
	put_line ("ERRORS total:" & natural'image (errors));

	
end to_polygon;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
