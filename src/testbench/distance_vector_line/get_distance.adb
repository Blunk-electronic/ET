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
with et_pcb_coordinates;		--use et_pcb_coordinates;
-- with et_board_shapes_and_text;	use et_board_shapes_and_text;


procedure get_distance is

	use et_pcb_coordinates.pac_geometry_brd;
	-- use et_pcb_coordinates.pac_geometry_2;

	-- use pac_contours;
	-- use pac_polygons;

	
	
	--v : type_vector := set (8.0, 6.0, 0.0);
	--v : type_vector := set (1.16000000000E+02, 1.24000000000E+02, 0.0);
	v : type_vector := set (1.16000000000E+02, 1.23999999999E+02, 0.0);
	--e : type_edge; -- := (start_point => set (0.0, 0.0), end_point => set (11.0. 0.0));
	l : type_line_fine := ((
		start_point	=> set (6.60000000000E+01, 1.24000000000E+02),
		end_point	=> set (6.60000000000E+01, 1.30000000000E+02)));

	d : type_distance_point_line;

	--offset : type_offset := (0.0, type_float_positive'small);
	offset : type_offset := (0.0, 1.0E-17);
begin
	-- e.start_point := (0.0, 0.0, 0.0);
	-- e.end_point := (11.0, 0.0, 0.0);

	for i in 1 .. 1_000_000_000 loop

		--put_line ("v: " & to_string (v) & " | " & to_string (l));
		-- put_line ("v: " & to_string (v));
		
		d := get_distance (
			vector	=> v,
			line	=> l,
			line_range	=> WITH_END_POINTS);

		--put_line ("distance" & to_string (get_distance (d)));
		--new_line;

		if d.distance = 0.0 then
			put_line ("v: " & to_string (v));
			put_line ("distance" & to_string (get_distance (d)));
			exit;
		end if;
		
		move_by (v, offset);
	end loop;
	
end get_distance;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
