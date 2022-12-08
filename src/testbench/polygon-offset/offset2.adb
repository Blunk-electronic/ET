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

procedure offset2 is

	use pac_geometry_2;
	use pac_geometry_brd;
	
	use pac_polygons;
	use pac_polygon_offsetting;


	P : type_polygon;

	--E_CT : count_type;

	mode : type_approximation_mode := SHRINK;

	debug : boolean := true;
	
begin
	--P := to_polygon ("0.0 0.0  1.0 0.0  1.0 1.0  0.0 1.0");
	P := to_polygon ("0.0 0.0  0.5 0.0  0.2 -1.0  0.6 -1.1  1.5 -0.3  1.0 0.0  1.0 1.0  0.0 1.0");
	put_line ("original: " & to_string (P));

	--offset_polygon (P, 1.0, debug);
	offset_polygon (P, -0.4, debug);
	put_line ("result  : " & to_string (P));

end offset2;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
	
