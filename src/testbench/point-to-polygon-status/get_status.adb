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

procedure get_status is

	use pac_geometry_brd;
	use pac_geometry_2;
	use pac_contours;
	use pac_polygons;
	use pac_polygon_clipping;

	P : type_polygon;
	T : type_point;
	V : type_vector;
	

	tolerance : type_distance_positive := fab_tolerance;
	
	
	-- Builds the polygon P:
	procedure make_polygon (
		s : in string)
	is
		C : type_contour;
	begin
		C := type_contour (to_contour (s));
		P := to_polygon (C, tolerance);
	end;
	
	
	c_square : constant string := "line 0 0 line 100 0 line 100 100 line 0 100";
	c_m_shaped : constant string := "line 0 0 line 50 50 line 100 0 line 100 100 line 0 100";
	c_rhombus : constant string := "line 50 0  line 100 50  line 50 100  line 0 50";
	--P1 : constant string := "line 0 0 line 100 0 line 100 100 line 50 10 line 0 100";
	--P_u_shaped : constant string := "line 0 0 line 100 0 line 100 100 line 90 100 line 90 10 line 10 10 line 10 100 line 0 100";


	
	procedure print_status (PPS : in type_point_to_polygon_status) is
		use pac_probe_line_intersections_polygon;
		use pac_edges;
		
		procedure query_intersection (i : in pac_probe_line_intersections_polygon.cursor) is begin
			put_line ("x-pos : " & type_float_internal'image (element (i)));
		end;
	
	begin
		put_line ("STATUS:");
		put_line ("probe line start: " & to_string (PPS.start));
		put_line ("point is: " & type_location'image (PPS.location));
		put_line ("intersections:");
		if PPS.intersections.is_empty then
			put_line (" none");
		else
			PPS.intersections.iterate (query_intersection'access);
		end if;

		case PPS.location is
			when INSIDE | OUTSIDE =>
				put_line ("distance to polygon: " & to_string (PPS.distance));

			when ON_EDGE =>
				put_line (to_string (element (PPS.edge)));
				
			when ON_VERTEX =>
				put_line ("edge 1: " & to_string (element (PPS.edges.edge_1)));
				put_line ("edge 2: " & to_string (element (PPS.edges.edge_2)));
				
		end case;
	end print_status;


	procedure do_test is begin
		put_line ("-----------");
		--put_line ("point:" & to_string (T));
		put_line ("vector:" & to_string (V));
		put_line (to_string (P));

		declare
			--S : type_point_to_polygon_status := get_point_to_polygon_status (P, T);
			S : type_point_to_polygon_status := get_point_to_polygon_status (P, V);
		begin
			print_status (S);
		end;
	end do_test;
	
begin

	--make_polygon (P_u_shaped);
	
	--make_polygon (c_square);
	--V := set (0.0, 20.0000000000); -- go
	--V := set (0.0, 0.0); -- go
	
	--make_polygon (c_rhombus);
	--V := set (0.0, 50.0); -- go
	--V := set (25.0, 25.0); -- go

	make_polygon (c_m_shaped);
	--V := set (50.0, 50.0); -- go
	V := set (10.0, 50.0); -- go


	
	-- T := type_point (set (-10.0, 99.0)); -- go
	--T := type_point (set (0.0, 99.0)); -- go
	--T := type_point (set (0.0, 100.0)); -- go
	--T := type_point (set (0.0, 0.0)); -- go
	--T := type_point (set (1.0, 1.0)); -- go
	--T := type_point (set (9.9999999999, 10.0000000000)); -- go
	
	--V := set (-10.0, 10.0000000000); -- go
	--V := set (0.0, 0.0); -- go
	--V := set (0.00000000001, 20.0000000000); -- go
	--V := set (1.0E-12, 20.0000000000); -- go
	--V := set (1.0E-16, 20.0000000000); -- go

	
	do_test;
	
	
end get_status;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
