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
with ada.containers.doubly_linked_lists;

with et_geometry;				use et_geometry;
with et_pcb_coordinates;		use et_pcb_coordinates;
with et_board_shapes_and_text;	use et_board_shapes_and_text;
with et_string_processing;		use et_string_processing;

procedure get_status is

	use pac_geometry_2;
	use pac_geometry_brd;
	use pac_polygons;
	use pac_polygon_clipping;

	-- The polygon under test:
	P : type_polygon;

	-- The line under test:
	L : type_line;

	-- The actual test result:
	S_actual : type_line_to_polygon_status;


	-- The expected test result:
	I : type_intersection_line_edge;
	use pac_line_edge_intersections;
	I_list : pac_line_edge_intersections.list;
	edge : pac_polygon_segments.cursor;
	S_expect : type_line_to_polygon_status;
		

	-- Builds the polygon P:
	procedure make_polygon (
		s : in string)
	is
		F : type_fields_of_line;
	begin
		F := read_line (line => s, comment_mark => "#");
		P := type_polygon (to_polygon (F));
	end;
	
	


	default_polygon : constant string := "line 0 0 line 100 0 line 100 100 line 0 100";
	
	P_v_shaped : constant string := "line 0 0 line 100 0 line 100 100 line 50 10 line 0 100";
	
	P_u_shaped : constant string := "line 0 0 line 100 0 line 100 100 line 90 100 "
									& "line 90 10 line 10 10 line 10 100 line 0 100";
	
	P_1_shaped : constant string := "line 50 0 line 100 0 line 100 100 line 0 100 line 0 50 line 50 50";
	
	P_staircase_inside : constant string := "line 90 0 line 100 0 line 100 100 line 0 100 "
											& "line 0 20 line 80 20 line 80 10 line 90 10";
	
	procedure print_status (status : in type_line_to_polygon_status) is
		use pac_line_edge_intersections;
		
		procedure query_intersection (i : in pac_line_edge_intersections.cursor) is 
			use pac_polygon_segments;
			EC : pac_polygon_segments.cursor := element (i).edge;
		begin
			put_line ("place : " & to_string (element (i).place));
			put_line ("edge  : " & to_string (element (EC).segment_line));
			put_line ("drctn : " & type_intersection_direction'image (element (i).direction));
			new_line;
		end;
	
	begin
		--put_line ("STATUS:");
		put_line ("line start point is: " & type_line_end'image (status.start_point));
		put_line ("intersections:");
		if status.intersections.is_empty then
			put_line (" none");
			put_line (" line center is : " & type_line_center'image (status.center_point));
		else
			status.intersections.iterate (query_intersection'access);
		end if;
		put_line ("line end point is: " & type_line_end'image (status.end_point));
		new_line;
	end print_status;


	
		


	procedure append_expected_intersection (
		x, y		: in type_distance;
		direction	: in type_intersection_direction;
		edge		: in pac_polygon_segments.cursor)
	is 
		p : type_point := type_point (set (x, y));
	begin
		I_list.append ((place => p, direction => direction, edge => edge));
	end append_expected_intersection;
	

	procedure set_expect (
		start_point, end_point	: in type_line_end;
		center_point			: in type_line_center := OUTSIDE;
		intersections			: in pac_line_edge_intersections.list)
	is begin
		S_expect.start_point := start_point;
		S_expect.end_point := end_point;
		S_expect.center_point := center_point;
		S_expect.intersections := intersections;
	end set_expect;


	procedure do_test is begin
		put_line ("-----------");
		put_line (to_string (L));
		new_line;
		
		S_actual := get_line_to_polygon_status (P, L);

		--if S_actual.start_point = S_expect.start_point
		--and S_actual.end_point = S_expect.end_point
		--and S_actual.center_point = S_expect.center_point
		--and S_actual.intersections = S_expect.intersections
		--then
			--null;
		--else

		if S_actual /= S_expect then
			put_line ("ERROR !!!");
			put_line ("expected:");
			print_status (S_expect);
			put_line ("found:");
			print_status (S_actual);
		end if;

		-- clean up for next test:
		I_list.clear;
	end do_test;

	
begin

	make_polygon (default_polygon);
	
	L := type_line (make_line (0.0, 0.0, 110.0, 110.0));
	edge := get_segment_edge (P, type_line (make_line (100.0, 0.0, 100.0, 100.0)));
	append_expected_intersection (100.0, 100.0, LEAVING, edge);
	set_expect (
		start_point		=> IS_VERTEX, 
		end_point		=> OUTSIDE, 
		intersections	=> I_list);

	do_test;
	

	L := type_line (make_line (0.0, 0.0, 200.0, 0.0));
	edge := get_segment_edge (P, type_line (make_line (100.0, 0.0, 100.0, 100.0)));
	append_expected_intersection (100.0, 0.0, LEAVING, edge);
	set_expect (
		start_point		=> IS_VERTEX, 
		end_point		=> OUTSIDE, 
		intersections	=> I_list);

	do_test;


	L := type_line (make_line (0.0, 0.0, 100.0, 100.0));
	edge := get_segment_edge (P, type_line (make_line (100.0, 0.0, 100.0, 100.0)));
	set_expect (
		start_point		=> IS_VERTEX, 
		end_point		=> IS_VERTEX, 
		center_point	=> INSIDE,
		intersections	=> I_list); -- empty

	do_test;
	

	L := type_line (make_line (0.0, 0.0, 50.0, 50.0));
	set_expect (
		start_point		=> IS_VERTEX, 
		end_point		=> INSIDE, 
		center_point	=> INSIDE,
		intersections	=> I_list); -- empty

	do_test;

	
	L := type_line (make_line (0.0, 0.0, 100.0, 0.0));
	set_expect (
		start_point		=> IS_VERTEX, 
		end_point		=> IS_VERTEX, 
		center_point	=> ON_EDGE,
		intersections	=> I_list); -- empty

	do_test;

	
	L := type_line (make_line (-10.0, -10.0, 110.0, 110.0));
	edge := get_segment_edge (P, type_line (make_line (0.0, 0.0, 100.0, 0.0)));
	append_expected_intersection (0.0, 0.0, ENTERING, edge);
	edge := get_segment_edge (P, type_line (make_line (100.0, 0.0, 100.0, 100.0)));
	append_expected_intersection (100.0, 100.0, LEAVING, edge);
	set_expect (
		start_point		=> OUTSIDE, 
		end_point		=> OUTSIDE, 
		--center_point	=> INSIDE,
		intersections	=> I_list);

	do_test;


	
	L := type_line (make_line (0.0, 0.0, 200.0, 1.0));
	edge := get_segment_edge (P, type_line (make_line (100.0, 0.0, 100.0, 100.0)));
	append_expected_intersection (100.0, 0.5, LEAVING, edge);
	set_expect (
		start_point		=> IS_VERTEX, 
		end_point		=> OUTSIDE, 
		--center_point	=> INSIDE,
		intersections	=> I_list);

	do_test;


	
	L := type_line (make_line (-10.0, 50.0, 110.0, 50.0));
	edge := get_segment_edge (P, type_line (make_line (0.0, 100.0, 0.0, 0.0)));
	append_expected_intersection (0.0, 50.0, ENTERING, edge);
	edge := get_segment_edge (P, type_line (make_line (100.0, 0.0, 100.0, 100.0)));
	append_expected_intersection (100.0, 50.0, LEAVING, edge);
	set_expect (
		start_point		=> OUTSIDE, 
		end_point		=> OUTSIDE, 
		--center_point	=> INSIDE,
		intersections	=> I_list);

	do_test;

	
	L := type_line (make_line (10.0, 50.0, 110.0, 50.0));
	edge := get_segment_edge (P, type_line (make_line (100.0, 0.0, 100.0, 100.0)));
	append_expected_intersection (100.0, 50.0, LEAVING, edge);
	set_expect (
		start_point		=> INSIDE, 
		end_point		=> OUTSIDE, 
		--center_point	=> INSIDE,
		intersections	=> I_list);

	do_test;


	L := type_line (make_line (-10.0, 10.0, 10.0, -10.0));
	set_expect (
		start_point		=> OUTSIDE, 
		end_point		=> OUTSIDE, 
		--center_point	=> ON_EDGE,
		intersections	=> I_list); -- empty

	do_test;


-----------	
	make_polygon (P_1_shaped);

	L := type_line (make_line (-10.0, 60.0, 60.0, -10.0));
	set_expect (
		start_point		=> OUTSIDE, 
		end_point		=> OUTSIDE, 
		--center_point	=> ON_EDGE,
		intersections	=> I_list); -- empty

	do_test;


	L := type_line (make_line (40.0, 60.0, 60.0, 40.0));
	set_expect (
		start_point		=> INSIDE, 
		end_point		=> INSIDE, 
		--center_point	=> ON_EDGE,
		intersections	=> I_list); -- empty

	do_test;

	

	L := type_line (make_line (40.0, 60.0, 61.0, 40.0));
	set_expect (
		start_point		=> INSIDE, 
		end_point		=> INSIDE, 
		center_point	=> INSIDE,
		intersections	=> I_list); -- empty

	
	do_test;

	
	
	make_polygon (P_staircase_inside);
	L := type_line (make_line (70.0, 30.0, 95.0, 5.0));
	set_expect (
		start_point		=> INSIDE, 
		end_point		=> INSIDE, 
		--center_point	=> INSIDE,
		intersections	=> I_list); -- empty

	do_test;


	make_polygon (P_staircase_inside);
	L := type_line (make_line (70.0, 30.0, 100.0, 0.0));
	set_expect (
		start_point		=> INSIDE, 
		end_point		=> IS_VERTEX, 
		--center_point	=> INSIDE,
		intersections	=> I_list); -- empty

	do_test;


	--P_u_shaped : constant string := "line 0 0 line 100 0 line 100 100 line 90 100 "
									--& "line 90 10 line 10 10 line 10 100 line 0 100";


	
	make_polygon (P_u_shaped);
	
	L := type_line (make_line (10.0, 50.0, 110.0, 50.0));

	edge := get_segment_edge (P, type_line (make_line (90.0, 100.0, 90.0, 10.0)));
	append_expected_intersection (90.0, 50.0, ENTERING, edge);

	edge := get_segment_edge (P, type_line (make_line (100.0, 0.0, 100.0, 100.0)));
	append_expected_intersection (100.0, 50.0, LEAVING, edge);

	set_expect (
		start_point		=> ON_EDGE, 
		end_point		=> OUTSIDE, 
		--center_point	=> INSIDE,
		intersections	=> I_list); -- empty

	do_test;


	

	L := type_line (make_line (10.0, 50.0, -10.0, 50.0));

	edge := get_segment_edge (P, type_line (make_line (0.0, 100.0, 0.0, 0.0)));
	append_expected_intersection (0.0, 50.0, LEAVING, edge);

	set_expect (
		start_point		=> ON_EDGE, 
		end_point		=> OUTSIDE, 
		--center_point	=> INSIDE,
		intersections	=> I_list); -- empty

	do_test;




	
	L := type_line (make_line (10.0, 10.0, -10.0, 10.0));

	edge := get_segment_edge (P, type_line (make_line (0.0, 100.0, 0.0, 0.0)));
	append_expected_intersection (0.0, 10.0, LEAVING, edge);

	set_expect (
		start_point		=> IS_VERTEX, 
		end_point		=> OUTSIDE, 
		--center_point	=> INSIDE,
		intersections	=> I_list); -- empty

	do_test;






	L := type_line (make_line (10.0, 10.0, 110.0, 90.0));

	edge := get_segment_edge (P, type_line (make_line (90.0, 100.0, 90.0, 10.0)));
	append_expected_intersection (90.0, 74.0, ENTERING, edge);

	edge := get_segment_edge (P, type_line (make_line (100.0, 0.0, 100.0, 100.0)));
	append_expected_intersection (100.0, 82.0, LEAVING, edge);

	
	set_expect (
		start_point		=> IS_VERTEX, 
		end_point		=> OUTSIDE, 
		--center_point	=> INSIDE,
		intersections	=> I_list); -- empty

	do_test;

	
	--L := type_line (make_line (0.0, 100.0, 100.0, 100.0)); -- go
	--L := type_line (make_line (5.0, 100.0, 100.0, 100.0)); -- go
	--L := type_line (make_line (5.0, 100.0, 95.0, 100.0)); -- go
	--L := type_line (make_line (-10.0, 50.0, 110.0, 50.0)); -- go
	--L := type_line (make_line (-10.0, 110.0, 10.0, 10.0)); -- go
	--do_test;

	

	
end get_status;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16