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




procedure get_intersection_edges is

	use pac_geometry_2;
	use pac_geometry_brd;

	use pac_polygons;
	

	E1 : type_edge := (
		start_point	=> (3.44773780171480198E+01, 6.44913871817609994E+00, 0.0),
		end_point	=> (3.44675173477225672E+01, 6.55904941371864518E+00, 0.0));

	E2 : type_edge := (
		start_point	=> (3.44598438925330826E+01, 6.66913443486875019E+00, 0.0),
		end_point	=> (3.44675173477225672E+01, 6.55904941371864518E+00, 0.0));


	I : type_intersection_of_two_lines (EXISTS);
begin

	I := get_intersection (E1, E2);

	put_line (type_intersection_status_of_two_lines'image (I.status));
	null;
	
end get_intersection_edges;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
