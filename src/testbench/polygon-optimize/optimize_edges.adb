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

procedure optimize_edges is

	--test_count : constant positive := 26;
	test_count : constant positive := 29;

	use pac_geometry_brd;
	use pac_geometry_2;

	use pac_contours;
	use pac_polygons;


	
	B_default_vertices : constant string := "0 0  100 0  100 100  0 100";
	B_5_vertices : constant string := "0 0  50 0  100 0  100 100  0 100";
	
	initial, reference, optimized : type_polygon;

	
begin
	-- The initial polygon:
	--initial := to_polygon (B_default_vertices);
	initial := to_polygon (B_5_vertices);
	put_line ("initial polygon:");
	put_line (to_string (initial));

	
	-- Optimize the initial polygon and keep the result:
	reference := initial;
	optimize_edges (polygon => reference, debug => false);
	put_line ("optimized reference:");
	put_line (to_string (reference));
	
	
	for i in 1 .. 5 loop
		put_line ("step" & natural'image (i));
		rotate (initial);
		
		--put_line ("given:");
		--put_line (to_string (initial));

		optimized := initial;

		--if i = 2 then
			--optimize_edges (polygon => optimized, debug => true);
		--else
			optimize_edges (polygon => optimized, debug => false);
		--end if;

		--put_line ("optimized:");
		--put_line (to_string (optimized));
		--new_line;

		
		if not are_congruent (reference, optimized) then
			put_line ("ERROR !");
			put_line ("given:");
			put_line (to_string (initial));

			put_line ("optimized:");
			put_line (to_string (optimized));
			new_line;

			exit;
		end if;

		--put_line ("optimized:");
		--put_line (to_string (Q));
		--new_line;
	end loop;
	
	
end optimize_edges;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
