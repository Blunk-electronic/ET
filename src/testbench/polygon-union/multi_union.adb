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

procedure multi_union is


	use pac_polygons;
	use pac_polygon_union;


	

	--subtype type_index is natural range 0 .. 100;
	--idx : type_index := 0;

	--procedure next_index is begin
		--idx := idx + 1;
	--end next_index;


	--errors : natural := 0;

	--procedure count_error is begin
		--errors := errors + 1;
	--end count_error;
		

	use pac_polygon_list;
	P_list_in : pac_polygon_list.list;
	
	P_1	: constant string := "0 0  1 0  1 1  0 1";
	P_2 : constant string := "0.5 0.5  0.5 -1.5  1.5 -1.5  1.5 0.5";
	P_3 : constant string := "2 1  3 1  3 3  2 3";
	P_4 : constant string := "2.5 3  2.8 3  2.8 4  2.5 4";
	
	

	procedure query_polygon (c : in pac_polygon_list.cursor) is begin
		put_line (to_string (element (c)));
	end query_polygon;

	
begin


	P_list_in.append (to_polygon (P_3));
	P_list_in.append (to_polygon (P_1));
	P_list_in.append (to_polygon (P_2));
	P_list_in.append (to_polygon (P_4));
	
	multi_union (P_list_in);

	
	P_list_in.iterate (query_polygon'access);
	
end multi_union;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
