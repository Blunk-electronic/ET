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

with et_geometry;				use et_geometry;
with et_pcb_coordinates;		use et_pcb_coordinates;
with et_board_shapes_and_text;	use et_board_shapes_and_text;
with et_string_processing;		use et_string_processing;

procedure to_contour is

	use pac_geometry_2;
	use pac_polygons;
	use pac_contours;
	use pac_contour_to_polygon;


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
		polygon_vertices	: in string;
		contour_expect		: in string)
	is 
		P_in : type_polygon := to_polygon (polygon_vertices);
		
		C_exp : type_contour := type_contour (to_contour (contour_expect));
		C_actual : type_contour;
	begin
		next_index;

		
		--put_line (to_string (P_in));
		--new_line;
		--put_line (to_string (C_exp));
		
		C_actual := to_contour (P_in);

		
		if C_actual /= C_exp then
			count_error;
			new_line;
			put_line ("ERROR. Test" & type_index'image (idx) & ":");
			put_line ("expected: " & to_string (C_exp));
			put_line ("found   : " & to_string (C_actual));
		end if;
	end do_test;


begin

	do_test (
		polygon_vertices => "0 0  100 0  100 100  0 100",
		contour_expect => "line 0 0 line 100 0 line 100 100 line 0 100");
	
end to_contour;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
