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
with et_contour_to_polygon;		use et_contour_to_polygon;
with et_string_processing;		use et_string_processing;

procedure offset is

	use pac_geometry_2;
	use pac_geometry_brd;
	
	use pac_contours;
	use pac_polygons;
	use pac_polygon_offsetting;

	C : type_contour;
	P : type_polygon;

	--S : string := "line 0 0 line 100 0 line 100 100 line 0 100";
	--S : string := "line 0 1 line 100 1 line 100 100 line 0 100";
	S : string := "line 0 0 arc 20 0 10 0 cw line 30 0 line 50 0 line 50 50 line 0 50";

	tolerance : type_distance_positive := fab_tolerance;

begin

	
	C := type_contour (to_contour (S));
	--put_line ("contour: " & to_string (C));
	
	-- expand polygon:
	--P := to_polygon (C, tolerance);
	--offset_polygon (P, +1.0); 
	--put_line ("expanded: " & to_string (P));

	--new_line;

	--tolerance := 0.1;
	tolerance := 2.0;
	
	-- shrink polygon:
	P := to_polygon (C, tolerance);
	put_line ("original: " & to_string (P));

	new_line;
	
	--offset_polygon (P, -0.4);
	offset_polygon (P, -1.0);
	--offset_polygon (P, -1.0);
	put_line ("shrank  : " & to_string (P));
	
end offset;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
