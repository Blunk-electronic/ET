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

procedure clip is

	use pac_geometry_2;
	use pac_polygons;
	use pac_polygon_clipping;

	A, B: type_polygon;

	use pac_clipped;
	C : pac_clipped.list;
	

	-- A: to be clipped
	-- B: clipping
	
	--SA : string := "line 80 10 line 150 10 line 150 20 line 80 20";
	--SB : string := "line 0 0 line 100 0 line 100 100 line 0 100";
	-- ok
	
	--SA : string := "line 0 0 line 1 0 line 1 1 line 0 1";
	--SB : string := "line 0.5 0.5 line 1.5 0.5 line 1.5 1.5 line 0.5 1.5";
	-- ok
	
	--SA : string := "line 40 -10 line 120 -10 line 120 50 line 80 50 line 80 -5 line 60 -5 line 60 50 line 40 50";
	--SB : string := "line 0 0 line 100 0 line 100 100 line 0 100";
	-- ok

	--SA : string := "line 20 -10 line 30 -10 line 110 50 line 30 110 line 20 110 line 25 50";
	--SB : string := "line 0 0 line 100 0 line 100 100 line 0 100";
	-- ok

	--SA : string := "line 30 0 line 50 0 line 50 50 line 30 50";
	--SB : string := "line 0 0 line 100 0 line 100 100 line 0 100";
	-- ok

	SA : string := "line 0 0 line 50 0 line 50 50 line 0 50";
	SB : string := "line 0 0 line 100 0 line 100 100 line 0 100";
	-- nogo

	--SA : string := "line 40 -10 line 50 -10 line 50 110 line 40 110";
	--SB : string := "line 0 0 line 100 0 line 100 100 line 0 100";
	-- ok


	
	--SA : string := "line 0 0 line 1 0 line 1 1 line 0 1";
	--SB : string := "line 0.5 0.5 line 1.5 0.5 line 1.5 1.5 line 0.5 1.5";

	
	F : type_fields_of_line;

	procedure query_polygon (p : in pac_clipped.cursor) is begin
		put_line ("C: " & to_string (element (p)));
	end query_polygon;
		
		
begin

	F := read_line (
		line			=> SA, 
		comment_mark	=> "#");
	
	A := type_polygon (to_polygon (F));
	--put_line ("A: " & to_string (A));

	F := read_line (
		line			=> SB, 
		comment_mark	=> "#");
	
	B := type_polygon (to_polygon (F));
	--put_line ("B: " & to_string (B));


	C := clip (A, B);
	--C := clip (B, A);
	C.iterate (query_polygon'access);
	
end clip;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
