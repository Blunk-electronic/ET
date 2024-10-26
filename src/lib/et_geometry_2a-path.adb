------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             PATH AND BEND                                --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                -- 
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
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
-- <http://www.gnu.org/licenses/>.   
------------------------------------------------------------------------------

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

package body et_geometry_2a.path is
	
	function to_path (
		start_point, end_point	: in type_vector_model;
		style					: in type_bend_style)
		return type_path
	is
		-- The area required for the path is a rectangle.
		-- We will need to figure out whether it is wider than tall:
		dx : constant type_distance := get_distance (start_point, end_point, AXIS_X);
		dy : constant type_distance := get_distance (start_point, end_point, AXIS_Y);

		sup_start, sup_end : type_vector_model; -- support points near given start and end point

		-- distance of support points from given start or end point:
		ds : constant type_distance_positive := 1.0;

		bended : type_bended := YES;
		bend_point : type_vector_model;

		-- CS this procedure should be made public as "intersection" or similar
		-- CS use function get_intersection with S1, R1, S2, R2 as input
		-- to compute intersection I.
		procedure compute_bend_point is 
			first_line	: constant type_line := (start_point, sup_start, others => <>);
			second_line	: constant type_line := (end_point, sup_end, others => <>);

			-- first line start vector:
			S1 : constant type_vector := get_start_vector (first_line);

			-- first line direction vector:
			R1 : constant type_vector := get_direction_vector (first_line);

			-- second line start vector:
			S2 : constant type_vector := get_start_vector (second_line);

			-- second line direction vector
			R2 : constant type_vector := get_direction_vector (second_line);

			-- scratch variables:
			a, b, c, d, e, f, g : type_float;
			lambda_1, lambda_2 : type_float;

			-- location vector of intersection
			I : type_vector;
		begin
			-- The direction vector of the first line can be zero in x (R1.x).
			-- In order to avoid division by zero we must switch between
			-- two ways to find the intersection:
			if get_x (R1) /= 0.0 then
				a := get_y (S1);
				b := get_x (S2) * get_y (R1) / get_x (R1);
				c := get_x (S1) * get_y (R1) / get_x (R1);
				d := get_y (S2);
				e := get_y (R2);
				f := get_x (R2) * get_y (R1) / get_x (R1);
				g := 1.0 / (e - f);

				lambda_2 := (a + b - c - d) * g;

				I := add (S2, scale (R2, lambda_2));
			else
				a := get_y (S2);
				b := get_x (S1) * get_y (R2) / get_x (R2);
				c := get_x (S2) * get_y (R2) / get_x (R2);
				d := get_y (S1);
				e := get_y (R1);
				f := get_x (R1) * get_y (R2) / get_x (R2);
				g := 1.0 / (e - f);

				lambda_1 := (a + b - c - d) * g;

				I := add (S1, scale (R1, lambda_1));
			end if;
			
			bend_point := to_point (I);
		end compute_bend_point;
		
	begin -- to_path
		
		-- If start and end point are equally then do nothing
		-- and return given start and end point as they are:
		if start_point = end_point then
			bended := NO;
		else
		
			-- If start and end point have same x or y position, then we
			-- have a straight direct line between them.
			if dx = zero or dy = zero then
				bended := NO;
			else

				case style is
					when STRAIGTH_THEN_ANGLED =>
						if abs (dx) = abs (dy) then -- diagonal line from start to end
							bended := NO;
						else
							
							-- compute support point near start point:
							-- The first line must run straight from start point:
							--if wider_than_tall then
							if abs (dx) > abs (dy) then -- wider than tall
								sup_start := set (get_x (start_point) + ds, get_y (start_point));
							else -- taller than wide
								sup_start := set (get_x (start_point), get_y (start_point) + ds);
							end if;

							-- compute support point near end point:
							-- The second line must run angled from end point:
							if dx > zero then -- to the right
								if dy > zero then -- upwards
									sup_end := set (get_x (end_point) + ds, get_y (end_point) + ds);
									--  45 degree
								else
									sup_end := set (get_x (end_point) + ds, get_y (end_point) - ds);
									-- -45 degree
								end if;
							else -- to the left
								if dy > zero then -- upwards
									sup_end := set (get_x (end_point) - ds, get_y (end_point) + ds);
									-- 135 degree
								else
									sup_end := set (get_x (end_point) - ds, get_y (end_point) - ds);
									-- 225 degree
								end if;
							end if;

							compute_bend_point;

						end if;
						
					when DIRECT => bended := NO;

					when ANGLED_THEN_STRAIGHT =>
						if abs (dx) = abs (dy) then -- diagonal line from start to end
							bended := NO;
						else
							
							-- Compute support point near start point:
							-- The first line must run angled from start point:
							if dx > zero then -- to the right
								if dy > zero then -- upwards
									sup_start := set (get_x (start_point) + ds, get_y (start_point) + ds);
									--  45 degree
								else -- downwards
									sup_start := set (get_x (start_point) + ds, get_y (start_point) - ds);
									-- -45 degree
								end if;
							else -- to the left
								if dy > zero then -- upwards
									sup_start := set (get_x (start_point) - ds, get_y (start_point) + ds);
									-- 135 degree
								else -- downwards
									sup_start := set (get_x (start_point) - ds, get_y (start_point) - ds);
									-- 225 degree
								end if;
							end if;

							-- compute support point near end point:
							-- The second line must run straight from end point:
							if abs (dx) > abs (dy) then -- wider than tall
								sup_end := set (get_x (end_point) + ds, get_y (end_point));
								-- horizontally
							else -- taller than wide
								sup_end := set (get_x (end_point), get_y (end_point) + ds);
								-- vertically
							end if;

							compute_bend_point;
						end if;
						
					when VERTICAL_THEN_HORIZONTAL =>
						-- Compute support point near start point:
						-- The first line must run vertically from start point:
						sup_start := set (get_x (start_point), get_y (start_point) + ds);
						-- vertically

						-- The second line must run horizontally from end point:
						sup_end := set (get_x (end_point) + ds, get_y (end_point));
						-- horizontally

						compute_bend_point;
						
					when HORIZONTAL_THEN_VERTICAL =>
						-- Compute support point near start point:
						-- The first line must run horizontal from start point:
						sup_start := set (get_x (start_point) + ds, get_y (start_point));
						-- horizontally

						-- compute support point near end point:
						-- The second line must run vertically from end point:
						sup_end := set (get_x (end_point), get_y (end_point) + ds);
						-- vertically

						compute_bend_point;
						
				end case;
			end if;

		end if;
			
		if bended = NO then
			return (NO, start_point, end_point);
		else
			return (YES, start_point, end_point, bend_point);
		end if;

	end to_path;


	
	
	procedure next_bend_style (
		path : in out type_path_live) 
	is
		i : constant natural := type_bend_style'pos (path.bend_style);
		-- i points now to the current bend style

		-- get the index of the last available bend style:
		max : constant natural := type_bend_style'pos (type_bend_style'last);
	begin
		if i < max then
			-- jump to next bend style
			path.bend_style := type_bend_style'succ (type_bend_style'val (i));
		else 
			-- After the last bend style, jump back to the first bend style:
			path.bend_style := type_bend_style'first;
		end if;
	end next_bend_style;
	
	
end et_geometry_2a.path;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
