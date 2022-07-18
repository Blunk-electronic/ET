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

procedure crop2 is

	use pac_geometry_brd;
	use pac_geometry_2;

	use pac_contours;
	use pac_polygons;
	use pac_polygon_cropping;


	use pac_cropped;

	-- A: cropping
	-- B: to be cropped


	
	contour_A_string : constant string := "circle 0 0 5";
	contour_A : type_contour;
	polygon_A : type_polygon;
	
	--contour_A_string : constant string := "line 0 -5  line 0 -10 line 10 -10 line 10 0"
		--& " arc 0 0   5 0  cw";				  

	contour_B_string : constant string := "line -10 -4  line 10 -4  line 10 4  line -10 4";				  
	contour_B : type_contour;
	polygon_B : type_polygon;

	crop_result : type_crop;
	polygon_C : type_polygon;

	fab_tol : type_distance := 0.4;
	--fab_tol : type_distance := 0.001;
	--fab_tol : type_distance := 0.0001;
begin

	contour_A := type_contour (to_contour (contour_A_string));
	polygon_A := to_polygon (contour_A, fab_tol);
	put_line ("polygon_A " & to_string (polygon_A));
	new_line;
	
	contour_B := type_contour (to_contour (contour_B_string));
	polygon_B := to_polygon (contour_B, fab_tol);
	put_line ("polygon_B " & to_string (polygon_B));
	new_line;
	
	crop_result := crop (polygon_A, polygon_B, true); -- debug messages on

	put_line ("crop result 1 :" & to_string (crop_result));
	new_line;

	crop_result := crop (polygon_A, crop_result.fragments.last_element, true);
	put_line ("crop result 2 :" & to_string (crop_result));
end crop2;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
