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
with ada.containers;			use ada.containers;
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
	P_original, P_scratch, P_offset : type_polygon;
	E_CT : count_type;

	--S : string := "line 0 0 line 100 0 line 100 100 line 0 100";
	--S : string := "line 0 1 line 100 1 line 100 100 line 0 100";
	S : string := "line 0 0 arc 20 0 10 0 cw line 30 0 line 50 0 line 50 50 line 0 50";

	tolerance : type_distance_positive := fab_tolerance;

	shrank_polygon_1 : constant type_polygon := to_polygon (
		  --"1.00000000000000000E+00 1.00000000000000000E+00 "
          "9.58578643762690495E+00 1.00000000000000000E+00 "
		& "2.00000000000000000E+01 1.14142135623730950E+01 "
		& "3.04142135623730950E+01 1.00000000000000000E+00 "
		& "4.90000000000000000E+01 1.00000000000000000E+00 "
		& "4.90000000000000000E+01 4.90000000000000000E+01 "
		& "1.00000000000000000E+00 4.90000000000000000E+01 "
		& "1.00000000000000000E+00 1.00000000000000000E+00");
	
begin
	--put_line ("rounding error:" & pac_geometry_brd.to_string (type_float_internal'small));
	
	C := type_contour (to_contour (S));
	--put_line ("contour: " & to_string (C));
	
	--tolerance := 0.01;
	
	P_original := to_polygon (C, tolerance);
	put_line ("original: " & to_string (P_original));
	
	offset_polygon (P_original, -0.4, true);
	--offset_polygon (P_original, -1.0, true); 
	put_line ("result  : " & to_string (P_original));

	--new_line;

	goto skip_1;

	
	--tolerance := 0.1;
	tolerance := 10.0;
	
	-- convert contour to polygon:
	P_original := to_polygon (C, tolerance);
	P_scratch := P_original;
	put_line ("original: " & to_string (P_original));

	--new_line;

	E_CT := get_edges_total (P_original);
	--E_CT := 2;

	for i in 1 .. E_CT loop
		P_offset := P_scratch;

		--offset_polygon (P_offset, -1.0); -- debug messages off
		offset_polygon (P_offset, -1.0, true); -- debug messages on

		new_line;
		put_line ("offset  : " & to_string (P_offset));
		
		--if not are_congruent (P_offset, shrank_polygon_1, true) then -- debug messages on
		if not are_congruent (P_offset, shrank_polygon_1) then -- debug messages off
			put_line ("ERROR");
			put_line ("expected: " & to_string (shrank_polygon_1));
			put_line ("found   : " & to_string (P_offset));
		end if;
		
		rotate (P_scratch);
		new_line;
		put_line ("rotation #" & count_type'image (i)
			& " / P_scratch: " & to_string (P_scratch));

		if not are_congruent (P_original, P_scratch) then -- debug messages off
			put_line ("ERROR");
			--put_line ("expected: " & to_string (P_original));
			--put_line ("found   : " & to_string (P_scratch));
		end if;

	end loop;

	<<skip_1>>
end offset;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
	
