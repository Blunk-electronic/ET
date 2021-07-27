------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               ROUTING                                    --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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

with ada.numerics.generic_elementary_functions;



package body et_routing is
	
	use et_pcb_coordinates.pac_geometry_brd;
	use et_board_shapes_and_text.pac_shapes;

	package functions_float is new ada.numerics.generic_elementary_functions (float);
	use functions_float;

	use pac_generic_modules;
	
	
	function compute_clearance (
		status			: in type_point_status; -- transition to inside/outside area
		y_position		: in type_position_axis; -- the y-position of the fill line
		intersection	: in type_probe_line_intersection; -- provides curvature, x-value, angle, ...
		line_width		: in type_track_width; -- the width of the fill line
		extra_clearance	: in boolean := false;
		clearance		: in type_distance_positive := zero;
		log_threshold	: in type_log_level)
		return type_distance_positive
	is
		result : type_distance_positive;

		-- Since the cap of the fill line is round, the minimal distance to observe
		-- from the center of the cap to the polygon/board edge is:
		clearance_min : constant float := float (clearance + line_width * 0.5);

		type type_line is new et_board_shapes_and_text.pac_shapes.type_line with null record;
			
	
		function compute_straight return type_distance_positive is
			-- If the probe line intersects with a straight segment of the polygon/board
			-- edge then we are dealing with a rectangular triangle.
			
			-- The total of inner angles of a rectangular triangle is 180 degrees.
			-- Two angles are known. Hence:

			-- The distance from center of the line cap to board edge
			-- along the probe line (in x-direction).
			-- (It is the hypothenusis of the triangle.):	
			side_c : float; -- to be returned

			-- The distance from track end point to polygon/board edge.
			-- A line perpendicular to the polygon/board edge.
			side_a : constant float := clearance_min;

			-- The angle between side_a and side_c:
			angle_b : constant float := float (90.0 - intersection.angle);
		begin
			--put_line (" clearance" & to_string (clearance));
			--put_line (" width" & to_string (width));
			--put_line (" side_a" & float'image (side_a) & " angle_b" & float'image (angle_b));

			side_c := side_a / cos (angle_b, float (units_per_cycle)); 
			--put_line (" side_c " & float'image (side_c));

			return type_distance_positive (side_c);
		end compute_straight;


		-- Computes the clearance from center of cap to convex board edge.
		-- - If status is OUTSIDE then we are approaching the convex edge from inside
		--   the board area to the outside.
		-- - If status is INSIDE then we are leaving the concave edge behind and are entering
		--   the board area from outside to inside. However, if you look back to the board edge, then
		--   it appears convex. We have to compute the distance from board edge to the center
		--   of the cap.
		--
		-- CS: The numerical algorithm implemented here is probably not the best.
		-- It requires optimization or replacement by a direct, non-numerical method.
		function compute_convex return type_track_clearance is

			-- The point where the probe line intersects the arc:
			PI : type_point;
			
			CI_pre : type_line;			
			CI : type_line_vector; -- the line from center of arc to intersection point PI

			probe_line_start : type_point;
			probe_line_end : type_point;
			probe_line_pre : type_line;
			probe_line : type_line_vector;

			-- The angle between probe line and CI is greater 0 and
			-- less than 180 degrees:
			subtype type_angle is float range (0.0 + float'small) .. (180.0 - float'small);
			angle_gamma : type_angle;

			side_a, side_b, side_c, side_d : float;

			-- Since this is a numeric method we limit the number of iterations to a
			-- reasonable maximum. This prevents the the algorithm from indefinite looping.
			-- CS: Testing requried. Adjust if necessary.
			subtype type_iteration is natural range 0 .. 100;
			i : type_iteration := 0;

			error : float;
			min_error : constant float := float (type_distance_positive'small);
			
		begin -- compute_convex
			log (text => "computing convex. targeted clearance" 
				 & to_string (type_distance_positive (clearance_min)),
				 level => log_threshold + 2);

			log_indentation_up;
			
			-- First me must initialize the point of intersection PI:
			case status is
				when OUTSIDE =>
					-- If we approach the board edge from inside the board area, then
					-- the x_position of the intersection is BEFORE the the x-position of the 
					-- center of the arc:
					PI := type_point (set (intersection.x_position, y_position));

				when INSIDE =>
					-- If we approach the board edge from outside the board area, then
					-- the x_position of the intersection is BEHIND the the x-position of the 
					-- center of the arc. So the x_position must be mirrored so that
					-- it comes before the x-position of the center of the arc:
					declare
						center_x : type_position_axis := get_x (intersection.center);
						delta_x : type_position_axis := intersection.x_position - center_x;
						x_pos_mirrored : type_position_axis := center_x - delta_x;
					begin
						PI := type_point (set (x_pos_mirrored, y_position));
					end;					
			end case;

			-- Build a line from the center of the arc to the point of intersection:
			CI_pre := (
				start_point	=> intersection.center,
				end_point	=> PI);

			CI := to_line_vector (CI_pre);

			-- The probe line runs from the point of intersection in 0 degrees to the right.
			-- To compose this probe line, it does not matter where it starts or where it
			-- ends. Only the direction matters:
			probe_line_start := PI;
			probe_line_end := type_point (set (intersection.x_position + 1.0, y_position));
			probe_line_pre := (probe_line_start, probe_line_end);
			probe_line := to_line_vector (probe_line_pre);
			
			--log (text => "");
			angle_gamma := type_angle (get_angle_of_itersection (probe_line, CI));
			--log (text => "gamma " & float'image (angle_gamma));

			-- Now we build a triangle composed of:
			-- - sida_a - from center of arc to intersection with board edge
			-- - side_b - from center of cap (P) to intersection with board edge
			-- - side_c - from center of arc to center of cap (P)
			-- - angle_gamma - between sida_b and side_a
			
			side_a := float (intersection.radius);
			--log (text => "side a" & float'image (side_a));

			-- As initial value for side_b we assume the greatest possible distance, which
			-- is from center of arc + the minimal clearance (defined by line width
			-- and DRU settings):
			side_b := clearance_min + side_a;

			-- Here is the numerical algorithm. It computes side_d (this is what we want),
			-- the distance from the center of the cap to the edge of the board:
			loop
				-- Count the number of iterations. Constraint error arises if maximum exceeded:				
				i := i + 1;
				
				--log (text => "");
				log (text => "iteration" & natural'image (i), level => log_threshold + 2);
				
				--log (text => " side b" & float'image (side_b));

				-- Compute side_c using the law of cosines (German: Kosinussatz):
				side_c := sqrt (
							side_a ** 2.0 + side_b ** 2.0 
							- 2.0 * side_a * side_b 
							* cos (angle_gamma, float (units_per_cycle)));
				
				--log (text => " side c" & float'image (side_c));
				
				-- Compute the resulting side_d and the deviation from the targeted clearance:
				side_d := side_c - side_a;

				--log (text => " clearance" & float'image (side_d),
					 --level => log_threshold + 2);

				error := abs (side_d - clearance_min);

				log (text => " error" & to_string (type_distance_positive (error)), level => log_threshold + 2);

				-- If the deviation is less than the minimal allowed error then exit
				-- the algorithm. The computed side_d is then the result of this function.
				if error < min_error then
					exit;
				else

					-- If the deviation is to large, then side_b must be become shorter or longer
					-- and another iteration be started.
					
					if side_d > clearance_min then -- too much clearance, reduce side_b
						--log (text => " too far");
						--side_b := side_b - side_b / 2.0; -- CS improve
						side_b := side_b - error;
						
					else -- too less clearance, increase side_b
						--log (text => " too close");
						--side_b := side_b + side_b / 2.0; -- CS improve
						side_b := side_b + error;

					end if;
					-- NOTE: The case when side_b equals clearance_min has been covered already above.

				end if;
			end loop;

			--log (text => "iterations " & natural'image (i));

			log_indentation_down;
			
			return type_track_clearance (side_b);

			--exception
				--when others =>
					--log (text => "iteration limit exceeded: " & natural'image (i + 1));
					--raise;
					
		end compute_convex;

		
		-- Computes the clearance from center of cap to concave board edge.
		-- - If status is OUTSIDE then we are approaching the concave edge from inside
		--   the board area to the outside.
		-- - If status is INSIDE then we are leaving the convex edge behind and are entering
		--   the board area from outside to inside. However, if you look back to the board edge, then
		--   it appears concave. We have to compute the distance from board edge to the center
		--   of the cap.
		--
		-- CS: The numerical algorithm implemented here is probably not the best.
		-- It requires optimization or replacement by a direct, non-numerical method.
		function compute_concave return type_track_clearance is

			error : type_distance_positive;
			min_error : constant type_distance_positive := type_distance_positive'small;
			
			-- The initial position of the cap is at maximum distance away from the point
			-- of intersection. It is right below the center of the arc:
			P : type_point := type_point (set (
				x => get_x (intersection.center), -- the x-value of the center of the arc
				y => y_position));

			type type_direction is (RIGHT, LEFT);
			
			procedure shift_cap (direction : in type_direction) is
				offset : type_distance_relative;

				-- The amount by which the cap will be shifted right or left:
				dx : type_distance_positive := zero;

			begin
				log (text => " shift cap " & type_direction'image (direction), 
					level => log_threshold + 2);

				case direction is
					when RIGHT =>

						dx := error;

						-- The cap wil be moved right. So dx must be positive:
						offset := to_distance_relative (set (x => + dx, y => zero));
						
					when LEFT =>

						dx := error;
						
						-- The cap wil be moved left. So dx must be negative:
						offset := to_distance_relative (set (x => - dx, y => zero));
				end case;
				
				-- Move the cap by the offset:
				move_by (P, offset);

				log (text => " new cap position" & to_string (P), 
					level => log_threshold + 3);
				
			end shift_cap;
			
			-- Since this is a numerical method we limit the number of iterations to a
			-- reasonable maximum. This prevents the the algorithm from indefinite looping.
			-- CS: Testing requried. Adjust if necessary.
			subtype type_iteration is natural range 0 .. 100;
			i : type_iteration := 0;

			-- Take a copy of clearance_min and convert it to type_distance:
			clearance_min_concave : constant type_distance_positive := type_distance_positive (clearance_min);
			
			clearance : type_distance_positive;

			result : type_distance_positive;
			
		begin -- compute_concave
			log (text => "computing concave. targeted clearance" 
				 & to_string (clearance_min_concave),
				 level => log_threshold + 2);

			log_indentation_up;

			--log (text => "intersection x" & to_string (intersection.x_position));

			clearance := intersection.radius - get_distance_total (intersection.center, P);

			if clearance < clearance_min_concave then
				-- If the initial clearance from cap to board edge is already less than the minimum
				-- clearance then the result is:
				result := abs (intersection.x_position - get_x (P));

			else
				-- Here is the numerical algorithm. It computes result (what we want), the distance 
				-- from the center of the cap to the edge of the board:
				loop
					-- Count the number of iterations. Constraint error arises if maximum exceeded:				
					i := i + 1;
					
					--log (text => "");
					log (text => "iteration" & natural'image (i), level => log_threshold + 2);
					
					clearance := intersection.radius - get_distance_total (intersection.center, P);

					error := abs (clearance - clearance_min_concave);

					log (text => " error" & to_string (error), level => log_threshold + 2);

					
					-- If the deviation is less than the minimal allowed error then exit
					-- the algorithm. The computed side_d is then the result of this function.
					if error < min_error then
						exit;
					else

						if clearance > clearance_min_concave then -- too much clearance
							--log (text => " too far");

							case status is
								-- If the probe line is leaving the board area at
								-- a concave edge, then the cap must be moved right
								-- TOWARDS the board edge:
								when OUTSIDE => shift_cap (RIGHT);

								-- If the probe line is entering the board area at
								-- a convex edge and if you look back to the edge, then it
								-- appears concave. Now the cap must be moved left
								-- TOWARDS the board edge:
								when INSIDE => shift_cap (LEFT);
							end case;
							
						else -- too less clearance
							--log (text => " too close");
							case status is
								-- If the probe line is leaving the board area at
								-- a concave edge, then the cap must be moved left
								-- AWAY from the board edge:
								when OUTSIDE => shift_cap (LEFT);

								-- If the probe line is entering the board area at
								-- a convex edge and if you look back to the edge, then it
								-- appears concave. Now the cap must be moved right
								-- AWAY from the board edge:
								when INSIDE => shift_cap (RIGHT);
							end case;
									
						end if;
						-- NOTE: The case when clearance equals clearance_min_concave has been covered already above.

					end if;

				end loop;

				result := abs (intersection.x_position - get_x (P));
			end if;

			log_indentation_down;
			
			return result;

			--exception
				--when others =>
					--log (text => "iteration limit exceeded: " & natural'image (i + 1));
					--raise;

		end compute_concave;
		
	begin -- compute_clearance
		
		log (text => "computing clearance fill line to edge ...", level => log_threshold);
		log_indentation_up;

		log (text => "probe line y" & to_string (y_position)
				& " width" & to_string (line_width)
				& " intersection x" & to_string (intersection.x_position)
				& " angle" & to_string (intersection.angle) 
				& " curvature " & to_string (intersection.curvature)
				& " status " & to_string (status),
			level => log_threshold + 1);

		
		if intersection.angle = 90.0 then

			-- The probe line approaches the board edge perpendicular:
			log (text => " line approaches edge perpendicular. nothing to do.",
				 level => log_threshold + 2);

			result := type_distance_positive (clearance_min);
			
		else
			
			case intersection.curvature is
				
				when STRAIGHT => result := compute_straight;

				when CONVEX =>

					case status is
						when OUTSIDE	=> result := compute_convex;
						when INSIDE		=> result := compute_concave;
					end case;
					
				when CONCAVE =>
					case status is
						when OUTSIDE	=> result := compute_concave;
						when INSIDE		=> result := compute_convex;
					end case;
					
			end case;
			
		end if;

		
		log (text => " clearance" & to_string (result),
			 level => log_threshold + 1);
		
		log_indentation_down;
		
		return result; 
		
	end compute_clearance;


		
			


	function get_distance_to_edge (
		module_cursor	: in pac_generic_modules.cursor;
		point			: in type_point;
		lth				: in type_log_level)
		return type_distance_polar
	is
		result : type_distance_polar := to_polar (type_distance_positive'last, zero_rotation);

		procedure update (d : in type_distance_polar) is begin
			--log (text => " dh" & to_string (get_absolute (d)), level => lth + 1);
			if get_absolute (d) < get_absolute (result) then
				result := d;
			end if;
		end update;

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in et_schematic.type_module) 
		is
			use et_packages;
			use pac_pcb_cutouts;
			procedure query_hole (c : pac_pcb_cutouts.cursor) is begin
				update (get_shortest_distance (element (c), point));
			end query_hole;
			
		begin
			-- test board outline:
			log (text => "probing outline ...", level => lth + 1);
			result := get_shortest_distance (module.board.contours.outline, point);

			log (text => " distance to outline" & to_string (get_absolute (result)),
				 level => lth + 1);
			
			-- test holes in board (if there are any):
			if not is_empty (module.board.contours.holes) then
				log (text => "probing holes...", level => lth + 1);			
				iterate (module.board.contours.holes, query_hole'access);

				log (text => " distance to hole" & to_string (get_absolute (result)),
					 level => lth + 1);
			end if;
		end query_module;

	begin
		log (text => "computing distance of point" & to_string (point) 
			 & " to board edge ...", level => lth);

		log_indentation_up;
		query_element (module_cursor, query_module'access);	
		log_indentation_down;
		return result;
	end get_distance_to_edge;
	
	
	function on_board (
		module_cursor	: in pac_generic_modules.cursor;
		point			: in type_point;
		lth				: in type_log_level)
		return boolean
	is
		result : boolean := true;
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in et_schematic.type_module) 
		is
			procedure query_outline is begin
				if in_polygon_status (module.board.contours.outline, point).status = OUTSIDE then
					--log (text => "outside", level => lth + 1);
					result := false;
				end if;
			end query_outline;
			
			
			procedure query_holes is
				use et_packages;
				use pac_pcb_cutouts;
				c : pac_pcb_cutouts.cursor := module.board.contours.holes.first;
			begin
				while c /= pac_pcb_cutouts.no_element loop
					if in_polygon_status (element (c), point).status = INSIDE then

						log (text => "point is in a hole", level => lth + 1);
						result := false;
						
						exit; -- no need to test other holes
					end if;
					
					next (c);
				end loop;
			end query_holes;
			
		begin -- query_module
			log (text => "probing outline ...", level => lth + 1);
			query_outline;

			if result = true then -- point is inside board outlines
				log (text => "point is inside board outlines. probing holes ...", level => lth + 1);
				query_holes;
			end if;
		end query_module;

	begin -- on_board
		log (text => "probing whether point" & to_string (point) 
			 & " is on board ...", level => lth);

		log_indentation_up;
		query_element (module_cursor, query_module'access);

		if result = true then
			log (text => "point is on board", level => lth);
		else
			log (text => "point is not on board", level => lth);
		end if;
		
		log_indentation_down;
		return result;
	end on_board;

						  
	function clear_for_track (
		module_cursor	: in pac_generic_modules.cursor;
		start_point		: in type_point;
		net_cursor		: in et_schematic.pac_nets.cursor;
		fill_zone		: in type_fill_zone;
		layer			: in type_signal_layer;
		width			: in type_track_width;
		ignore_same_net	: in boolean;
		lth				: in type_log_level)		
		return boolean is separate;


	function get_total_width (
		track	: in type_track)
		return type_distance_positive
	is begin
		return track.width + 2.0 * track.clearance;
	end get_total_width;


	function get_dimensions (
		track : in type_track)
		return type_track_dimensions
	is
		result : type_track_dimensions;

		-- here the given track starts:
		track_start : constant type_point := to_point (track.center.v_start);

		-- the total track width (incl. clearance) is:
		width : constant type_track_width := get_total_width (track);

		-- the start and end points of the upper and lower edge:
		upper_edge_start, upper_edge_end,
		lower_edge_start, lower_edge_end : type_point;
		
	begin
		-- the given track travels in this direction:
		result.direction := get_angle (track.center);

		-- the offset of the track relative to the origin is:
		result.offset := to_distance_relative (track_start);
		
		-- build the line in the center of the track:
		result.center_line := (
				start_point	=> origin,
				end_point	=> type_point (set (
								x => far_right - width * 0.5,
								y => zero)));

		
		-- build the horizontally running track:
		
		-- build the boundaries of the track
		result.boundaries := get_boundaries (result.center_line, width);

		
		-- build start and end points of upper and lower edge
		upper_edge_start := type_point (
			set (result.boundaries.smallest_x, result.boundaries.greatest_y));

		upper_edge_end := type_point (
			set (result.boundaries.greatest_x, result.boundaries.greatest_y));

		--
		lower_edge_start := type_point (
			set (result.boundaries.smallest_x, result.boundaries.smallest_y));

		lower_edge_end := type_point (
			set (result.boundaries.greatest_x, result.boundaries.smallest_y));

		-- build upper and lower edge:
		result.upper_edge := (upper_edge_start, upper_edge_end);
		result.lower_edge := (lower_edge_start, lower_edge_end);

		return result;
	end get_dimensions;

	
	function get_break (
		init		: in type_distance;
		place		: in type_place;
		obstacle	: in type_obstacle;
		clearance	: in type_distance_positive;
		lth			: in type_log_level) 
		return type_distance
	is
		-- Build a circle that models the cap of the track.
		-- The circle covers the clearance required for the track:
		c : type_circle := (radius => clearance, others => <>);
		
		-- the distance between center of cap and obstacle:
		d_cap_to_obstacle : type_distance;
		d_cap_to_obstacle_abs : type_distance_positive;

		step : type_distance_positive;
		
		-- There is a maximum of iterations. If maximum reached
		-- a constraint_error is raised.
		max_iterations : constant positive := 1000; -- CS increase if necessary
		--max_iterations : constant positive := 20; -- CS increase if necessary

	begin		
		log (text => "starting numerical search ...", level => lth);
		log_indentation_up;

		-- Set the inital position of the cap:
		case place is
			when BEFORE =>
				c.center := type_point (set (init - c.radius, zero));

			when AFTER =>
				c.center := type_point (set (init + c.radius, zero));
		end case;

		
		for i in 1 .. max_iterations loop
			
			-- Calculate the distance between the cap (incl. clearance) and the obstacle:
			case obstacle.shape is
				when LINE =>
					d_cap_to_obstacle := get_distance (c, obstacle.line);
				when ARC =>
					d_cap_to_obstacle := get_distance (c, obstacle.arc);
				when CIRCLE =>
					d_cap_to_obstacle := get_distance (c, obstacle.circle);
			end case;
					
			log (text => " distance" & to_string (d_cap_to_obstacle), level => lth + 1);
			--put_line (" D:" & to_string (d_cap_to_obstacle));-- & " C:" & to_string (c.center));
			
			d_cap_to_obstacle_abs := abs (d_cap_to_obstacle);

			
			-- Cancel this loop once the distance is sufficiently small.
			-- Otherwise take half of the distance and move cap to new position:
			if d_cap_to_obstacle_abs <= type_distance'small then
				log (text => " break point found after" & positive'image (i) & " iterations",
					level => lth + 2);
				exit;
			else
				step := d_cap_to_obstacle_abs * 0.5;
				
				case place is
					when BEFORE =>
						if d_cap_to_obstacle > zero then
							-- move cap right towards the obstacle:
							c.center := type_point (move (c.center, 0.0, step));
						else
							-- move cap left away from the obstacle:
							c.center := type_point (move (c.center, 180.0, step));
						end if;
						
					when AFTER =>
						if d_cap_to_obstacle > zero then
							-- move cap left towards the obstacle:
							c.center := type_point (move (c.center, 180.0, step));
						else
							-- move cap right away from the obstacle:
							c.center := type_point (move (c.center, 0.0, step));
						end if;
				end case;
			end if;

			
			-- Once the maximum of iterations has been reached, raise exception:
			if i = max_iterations then
				raise constraint_error with "ERROR: Max. interations of " & positive'image (i) &
				" reached !";
			end if;
		end loop;

		log_indentation_down;
		
		return get_x (c.center);
	end get_break;

	
	function get_break_by_line (
		track	: in type_track;
		line	: in type_line;
		place	: in type_place;
		lth		: in type_log_level)
		return type_break
	is
		track_dimensions : constant type_track_dimensions := get_dimensions (track);

		-- the clearance between center of cap and line:
		clearance : constant type_distance_positive := track.width * 0.5 + track.clearance;
		
		function move_and_rotate_line (line: in type_line) return type_line is
			l : type_line := line;
		begin
			move_by (l, invert (track_dimensions.offset));
			rotate_by (l, - track_dimensions.direction);
			return l;
		end move_and_rotate_line;

		-- The line we will work with from now on:
		line_tmp : constant type_line := move_and_rotate_line (line);

		-- the boundaries of the rotated line
		line_boundaries : constant type_boundaries := get_boundaries (line_tmp, zero);
		-- (The line has zero line width.)
		
		
		-- the intersections of the upper and lower edge of the track 
		-- with the rotated line:
		i_upper : constant type_intersection_of_two_lines :=
			get_intersection (to_line_vector (track_dimensions.upper_edge), line_tmp);
	
		i_lower : constant type_intersection_of_two_lines :=
			get_intersection (to_line_vector (track_dimensions.lower_edge), line_tmp);

		-- the area where track and line boundaries intersect:
		bi : constant type_boundaries_intersection :=
			  get_intersection (track_dimensions.boundaries, line_boundaries);

		-- the place along the x-axis where the search for the break is to begin:
		start_point : type_distance;
		
		bp : type_point;
		break_exists : boolean := false;

		
		procedure full_intersection is 
			-- The intersection of the center_line of the track with the candidate line:
			i_center : constant type_intersection_of_two_lines :=
				get_intersection (to_line_vector (track_dimensions.center_line), line_tmp);

			-- We are dealing with a rectangular triangle.			
			-- The distance from the center of the cap of the track to the 
			-- candidate line (in x-direction).
			-- (It is the hypothenusis of the triangle.):	
			spacing : type_distance_positive;

			-- The angle between clearance and spacing:
			angle : constant float := float (90.0 - i_center.intersection.angle);

		begin
			log_indentation_up;

			--log (text => "line " & to_string (line_tmp) 
				 --& " intersects center of track at" & to_string (i_center.intersection.point)
				 --& " angle" & to_string (i_center.intersection.angle),
				 --level => lth + 2);
			
			-- clearance is the distance from center of the cap perpendicular to the line.
			spacing := type_distance_positive (
					float (clearance) / cos (angle, float (units_per_cycle)));

			--spacing := 0.0;
			log (text => "required spacing" & to_string (spacing), level => lth + 2);
			
			-- Depending on the given place, the break point must be moved 
			-- left or right by the spacing:
			case place is
				when BEFORE =>
					bp := type_point (set (get_x (i_center.intersection.point) - spacing, zero));
					
				when AFTER =>
					bp := type_point (set (get_x (i_center.intersection.point) + spacing, zero));
			end case;
			
			log_indentation_down;
		end full_intersection;
			
	begin -- get_break_by_line
		if bi.exists then -- line and track boundaries do intersect in some way

			log (text => "break by" & to_string (line), level => lth);
			log_indentation_up;
			
			-- If we search for a break before the line, then it makes sense
			-- only if the area of overlap begins after the start of the track.
			-- This condition test should avoid useless searching for a break. 
			-- CS: not verified ! Remove this test if assumption is wrong.
			if (place = BEFORE and bi.intersection.smallest_x >= zero) 

			-- CS: A similar optimization when place is AFTER ?				
			or place = AFTER 
			then
				
				if (i_upper.status = EXISTS and i_lower.status = EXISTS) then
					-- The candidate line intersects the upper and lower edge of the track.

					log (text => "line intersects track upper and lower edge", level => lth + 1);
			
					full_intersection;
					-- bp is now set
					
				else
					-- The candidate line intersects only one edge or none at all.
					log (text => "line intersects track partially", level => lth + 1);
					log_indentation_up;
					
					case place is
						when BEFORE =>
							-- Use the LEFT border of the overlap area as start point for the
							-- search operation:
							start_point := bi.intersection.smallest_x;

						when AFTER =>
							-- Use the RIGHT border of the overlap area as start point for the
							-- search operation:
							start_point := bi.intersection.greatest_x;
					end case;
					
					bp := type_point (set (get_break (
							init		=> start_point,
							place		=> place,
							obstacle	=> (et_geometry.LINE, line_tmp),
							clearance	=> clearance,
							lth			=> lth + 1),
							zero));

					log_indentation_down;
				end if;

			
				-- The computed break point must be after the start of the track.
				-- If it is before the start of the track, then it is discarded.
				if get_x (bp) > zero then

					-- Rotate and move the break point back according to
					-- the track direction and offset:
					rotate_to (bp, track_dimensions.direction);
					move_by (bp, track_dimensions.offset);

					break_exists := true;

					log (text => "break point " & type_place'image (place) & " line:" & to_string (bp),
						level => lth + 2);
				end if;

			end if;

			log_indentation_down;
		end if;

		
		if break_exists then
			return (exists => true, point => bp);
		else
			return (exists => false);
		end if;
			
	end get_break_by_line;
	

	-- This function sorts the given intersections (of line and circle)
	-- and returns them in a simple list of distances along the x-axis:
	function get_x_values (
		i_upper, i_lower : in type_intersection_of_line_and_circle)
		return pac_distances.list
	is
		result : pac_distances.list;

		use pac_distances_sorting;
		
		procedure read_x_position (
			i : in type_intersection_of_line_and_circle) 
		is begin
			case i.status is
				when ONE_EXISTS =>
					if i.tangent_status = SECANT then
						result.append (get_x (i.intersection.point));
					end if;

				when TWO_EXIST =>
					result.append (get_x (i.intersection_1.point));
					result.append (get_x (i.intersection_2.point));
					
				when NONE_EXIST => null;
			end case;
		end read_x_position;
		
	begin
		read_x_position (i_upper);
		read_x_position (i_lower);

		sort (result);
		
		return result;
	end get_x_values;

	
	function get_break_by_arc (
		track	: in type_track;
		arc		: in type_arc;
		place	: in type_place;
		lth		: in type_log_level)
		return type_break_double
	is
		track_dimensions : constant type_track_dimensions := get_dimensions (track);

		-- the clearance between center of cap and arc:
		clearance : constant type_distance_positive := track.width * 0.5 + track.clearance;
		
		function move_and_rotate_arc (arc : in type_arc) return type_arc is
			a : type_arc := arc;
		begin
			move_by (a, invert (track_dimensions.offset));
			rotate_by (a, - track_dimensions.direction);
			return a;
		end move_and_rotate_arc;

		-- The arc we will work with from now on.
		arc_tmp : constant type_arc := move_and_rotate_arc (arc);

		-- the boundaries of the rotated arc:
		arc_boundaries : constant type_boundaries := get_boundaries (arc_tmp, zero);
		-- (The arc has zero line width.)

		
		-- the intersections of the upper and lower edge of the track
		-- with the rotated arc:
		i_upper : constant type_intersection_of_line_and_circle :=
			get_intersection (to_line_vector (track_dimensions.upper_edge), arc_tmp);
	
		i_lower : constant type_intersection_of_line_and_circle :=
			get_intersection (to_line_vector (track_dimensions.lower_edge), arc_tmp);

		-- the area where track and arc boundaries intersect:
		bi : constant type_boundaries_intersection := 
			get_intersection (track_dimensions.boundaries, arc_boundaries);

		-- the place along the x-axis where the search for the break is to begin:
		start_point : type_distance;
		
		-- the possible break points and the number of break points:
		break_count : type_break_count := 0;
		bp1, bp2 : type_point;

		-- Get all the intersections of the track with the arc.
		-- The arc may intersect the upper and lower edge of the track.
		use pac_distances;
		x_values_pre : pac_distances.list := get_x_values (i_upper, i_lower);
		-- x_values_pre now contains the sorted x-positions from left to right

		-- Splits the arc in 3 or 4 segments. Searches for each segment
		-- the break before/after the intersection with the track.
		procedure set_break_points (arc : in type_arc) is
			-- this is the split operation:
			arcs : constant type_arcs := split_arc (arc);
			-- arcs is now a collection of arc segments

			arc_boundaries : type_boundaries;

			x_pre : type_distance;
			x_values : pac_distances.list;
			x_cursor : pac_distances.cursor;

			use pac_distances_sorting;
		begin
			-- Loop in collection of arc segments (it is an array of arcs):
			for i in arcs'first .. arcs'last loop

				-- Get the boundaries of the candidate arc:
				arc_boundaries := get_boundaries (arcs (i), zero); -- arc has zero width

				declare
					-- Get the overlap area of the track and arc boundaries:
					bi : constant type_boundaries_intersection := 
						get_intersection (track_dimensions.boundaries, arc_boundaries);
				begin
					if bi.exists then
						case place is
							when BEFORE =>
								-- Use the LEFT border of the overlap area as start point for the
								-- search operation:
								start_point := bi.intersection.smallest_x;

							when AFTER =>
								-- Use the RIGHT border of the overlap area as start point for the
								-- search operation:
								start_point := bi.intersection.greatest_x;
						end case;

						x_pre := get_break (
							init		=> start_point,
							place		=> place,
							obstacle	=> (et_geometry.ARC, arcs (i)),
							clearance	=> clearance,
							lth			=> lth + 2);
						
						-- The break must be after the start of the track.
						-- Otherwise the break is ignored.
						-- Collect the x-position of the break in container x_values.
						if x_pre > zero then
							x_values.append (x_pre);
						end if;
					end if;
				end;
			end loop;

			-- sort the x-positions (increasing order)
			sort (x_values);

			-- derive the break count from the number of x-positions
			break_count := type_break_count (length (x_values));

			case break_count is
				when 0 => null;
				when 1 => 
					x_cursor := x_values.first;
					bp1 := type_point (set (element (x_cursor), zero));
					
				when 2 =>
					x_cursor := x_values.first;					
					bp1 := type_point (set (element (x_cursor), zero));
					next (x_cursor);
					bp2 := type_point (set (element (x_cursor), zero));
			end case;
		end set_break_points;

		
		-- Searches the break point (before or after) by means 
		-- of the boundaries of the whole overlapping area.
		procedure use_overlap_area is begin
			log (text => "using boundaries of whole overlapping area", level => lth + 1);
			log_indentation_up;
			
			case place is
				when BEFORE =>
					-- The start point of the search is the LEFT border of the
					-- overlapping area:
					start_point := bi.intersection.smallest_x;
														
				when AFTER =>
					-- The start point of the search is the RIGHT border of the
					-- overlapping area:
					start_point := bi.intersection.greatest_x;
			end case;

			bp1 := type_point (set (get_break (
				init		=> start_point,
				place		=> place,
				obstacle	=> (et_geometry.ARC, arc_tmp),
				clearance	=> clearance,
				lth			=> lth + 2),
				zero));

			-- The computed break point must be after the start of the track.
			-- If it is before the start of the track, then it is discarded.
			if get_x (bp1) > zero then -- CS really necessary ?

				-- Rotate and move the break point back according to
				-- the track direction and offset:
				rotate_to (bp1, track_dimensions.direction);
				move_by (bp1, track_dimensions.offset);

				break_count := 1;

				log (text => "break point " & type_place'image (place) & " arc:" & to_string (bp1),
					level => lth + 2);
			end if;

			log_indentation_down;
		end use_overlap_area;
		
		
	begin -- get_break_by_arc
		if bi.exists then -- arc and track do intersect in some way

			log (text => "break by" & to_string (arc), level => lth);
			log_indentation_up;

			-- The number of intersections with the arc determines
			-- how to proceed:
			case length (x_values_pre) is

				when 0 =>
					-- The arc is embedded in the track and does NOT intersect the track.
					-- If x_values_pre is empty then the arc is embedded in the track
					-- without touching the upper or lower edge of the track.
					-- If we search for a break before the arc, then it makes sense
					-- only if the area of overlap begins after the start of the track.
					if (place = BEFORE and bi.intersection.smallest_x >= zero)

					-- CS: A similar optimization when place is AFTER ?				
					or place = AFTER 
					then
						use_overlap_area;
					else
						log (text => "boundaries of arc are before start of track -> arc skipped",
							 level => lth);
					end if;

				when 1..2 =>
					-- The arc intersects the track in up to two points.
					-- If we search for a break before the arc, then it makes sense
					-- only if the last intersection comss after the start of the track.
					if (place = BEFORE and x_values_pre.last_element >= zero)

					-- CS: A similar optimization when place is AFTER ?				
					or place = AFTER 
					then
						use_overlap_area;
					else
						log (text => "all intersections are before start of track -> arc skipped",
							level => lth);
					end if;
				
				when 3..4 =>					
					-- The arc intersects the track in 3 or 4 points.
					-- So the arc must be split in 2 or 3 segments. Each of them
					-- will then be treated separately.
					log (text => "splitting of arc required", level => lth + 1);
					log_indentation_up;
					
					set_break_points (arc_tmp);

					case break_count is
						when 0 => null;
						when 1 =>

							-- Rotate and move the break point back according to
							-- the track direction and offset:
							rotate_to (bp1, track_dimensions.direction);
							move_by (bp1, track_dimensions.offset);

							log (text => "break point 1 " & type_place'image (place) & " arc:" & to_string (bp1),
									level => lth + 2);

						when 2 =>
							rotate_to (bp1, track_dimensions.direction);
							move_by (bp1, track_dimensions.offset);

							log (text => "break point 1 " & type_place'image (place) & " arc:" & to_string (bp1),
									level => lth + 2);

							rotate_to (bp2, track_dimensions.direction);
							move_by (bp2, track_dimensions.offset);
							
							log (text => "break point 2 " & type_place'image (place) & " arc:" & to_string (bp2),
								level => lth + 2);

					end case;

					log_indentation_down;

				when others => raise constraint_error; -- CS should never happen
			end case;
			
			log_indentation_down;
		end if;


		case break_count is
			when 0 => return (count => 0);
			when 1 => return (1, bp1);
			when 2 => return (2, bp1, bp2);
		end case;
	end get_break_by_arc;


	function get_break_by_circle (
		track	: in type_track;
		circle	: in type_circle;
		place	: in type_place;
		lth		: in type_log_level)
		return type_break_double
	is
		track_dimensions : constant type_track_dimensions := get_dimensions (track);

		-- the clearance between center of cap and arc:
		clearance : constant type_distance_positive := track.width * 0.5 + track.clearance;
		
		function move_circle (circle : in type_circle) return type_circle is
			c : type_circle := circle;
		begin
			move_by (c, invert (track_dimensions.offset));
			return c;
		end move_circle;

		-- The circle we will work with from now on.
		circle_tmp : constant type_circle := move_circle (circle);

		-- the boundaries of the rotated arc:
		circle_boundaries : constant type_boundaries := get_boundaries (circle_tmp, zero);
		-- (The circle has zero line width.)

		
		-- the intersections of the upper and lower edge of the track
		-- with the circle:
		i_upper : constant type_intersection_of_line_and_circle :=
			get_intersection (to_line_vector (track_dimensions.upper_edge), circle_tmp);
	
		i_lower : constant type_intersection_of_line_and_circle :=
			get_intersection (to_line_vector (track_dimensions.lower_edge), circle_tmp);

		-- the area where track and circle boundaries intersect:
		bi : constant type_boundaries_intersection := 
			get_intersection (track_dimensions.boundaries, circle_boundaries);

		-- the place along the x-axis where the search for the break is to begin:
		start_point : type_distance;
		
		-- the possible break points and the number of break points:
		break_count : type_break_count := 0;
		bp1, bp2 : type_point;

		-- Get all the intersections of the track with the arc.
		-- The arc may intersect the upper and lower edge of the track.
		use pac_distances;
		x_values_pre : pac_distances.list := get_x_values (i_upper, i_lower);
		-- x_values_pre now contains the sorted x-positions from left to right

		-- Splits the circle in 2 arcs. Searches for each arc segment
		-- the break before/after the intersection with the track.
		procedure set_break_points (circle : in type_circle) is
			-- this is the split operation:
			arcs : constant type_arcs := split_circle (circle);
			-- arcs is now a collection of two arc segments

			arc_boundaries : type_boundaries;

			x_pre : type_distance;
			x_values : pac_distances.list;
			x_cursor : pac_distances.cursor;

			use pac_distances_sorting;
		begin
			-- Loop in collection of arc segments (it is an array of arcs):
			for i in arcs'first .. arcs'last loop

				-- log the candidate arc:
				log (text => to_string (arcs (i)), level => lth + 2);
				
				-- Get the boundaries of the candidate arc:
				arc_boundaries := get_boundaries (arcs (i), zero); -- arc has zero line width
				
				declare
					-- Get the overlap area of the track and arc boundaries:
					bi : constant type_boundaries_intersection := 
						get_intersection (track_dimensions.boundaries, arc_boundaries);
				begin
					if bi.exists then
						
						case place is
							when BEFORE =>
								-- Use the LEFT border of the overlap area as start point for the
								-- search operation:
								start_point := bi.intersection.smallest_x;

							when AFTER =>
								-- Use the RIGHT border of the overlap area as start point for the
								-- search operation:
								start_point := bi.intersection.greatest_x;
						end case;

						--log (text => "place" & type_place'image (place) 
							--& "start point: " & to_string (start_point) 
							--& " clearance" & to_string (clearance));

						--log (text => "arc" & to_string (arcs (i)));
						
						x_pre := get_break (
							init		=> start_point,
							place		=> place,
							obstacle	=> (et_geometry.ARC, arcs (i)),
							clearance	=> clearance,
							lth			=> lth + 2);
						
						-- The break must be after the start of the track.
						-- Otherwise the break is ignored.
						-- Collect the x-position of the break in container x_values.
						if x_pre > zero then
							x_values.append (x_pre);
						end if;
						
					end if;
				end;
			end loop;

			-- sort the x-positions (increasing order)
			sort (x_values);

			-- derive the break count from the number of x-positions
			break_count := type_break_count (length (x_values));

			case break_count is
				when 0 => null;
				when 1 => 
					x_cursor := x_values.first;
					bp1 := type_point (set (element (x_cursor), zero));
					
				when 2 =>
					x_cursor := x_values.first;					
					bp1 := type_point (set (element (x_cursor), zero));
					next (x_cursor);
					bp2 := type_point (set (element (x_cursor), zero));
			end case;
		end set_break_points;
		
	begin -- get_break_by_circle
		if bi.exists then -- circle and track do intersect in some way

			log (text => "break by" & to_string (circle), level => lth);
			log_indentation_up;

			-- The number of intersections with the arc determines
			-- how to proceed:
			-- CS: see get_break_by_arc for possible improvements here.
			case length (x_values_pre) is
				when 0 .. 2 =>
					-- when 0: 
					-- The circle is embedded in the track
					-- without touching the upper or lower edge of the track.
					
					-- when 1: 
					-- The circle is embedded in the track
					-- but touches the upper or lower edge of the track.
					-- One edge is a tangent to the circle.

					-- when 2:
					-- The circle intersects the track twice with the upper edge
					-- OR twice with the lower edge.

					-- If we search for a break before the circle, then it makes sense
					-- only if the area of overlap begins after the start of the track.
					if (place = BEFORE and bi.intersection.smallest_x >= zero) -- CS ?

					-- CS: A similar optimization when place is AFTER ?				
					or place = AFTER 
					then
						-- The search for the break point (before or after) can be 
						-- done by means of the boundaries of the whole overlapping area.
						log (text => "using boundaries of whole overlapping area",
							 level => lth + 1);
						
						log_indentation_up;
						
						case place is
							when BEFORE =>
								-- The start point of the search is the LEFT border of the
								-- overlapping area:
								start_point := bi.intersection.smallest_x;
							
							when AFTER =>
								-- The start point of the search is the RIGHT border of the
								-- overlapping area:
								start_point := bi.intersection.greatest_x;
						end case;

						bp1 := type_point (set (get_break (
							init		=> start_point,
							place		=> place,
							obstacle	=> (et_geometry.CIRCLE, circle_tmp),
							clearance	=> clearance,
							lth			=> lth + 2),
							zero));


						-- The computed break point must be after the start of the track.
						-- If it is before the start of the track, then it is discarded.
						if get_x (bp1) > zero then -- CS really necessary ?

							-- Rotate and move the break point back according to
							-- the track direction and offset:
							rotate_to (bp1, track_dimensions.direction);
							move_by (bp1, track_dimensions.offset);

							break_count := 1;

							log (text => "break point " & type_place'image (place) & " circle:" & to_string (bp1),
								level => lth + 2);
						end if;

						log_indentation_down;
					else
						log (text => "boundaries of circle before start of track -> circle skipped",
							 level => lth);
					end if;

				when 4 =>					
					-- The circle intersects the track in 4 points.
					-- So the circle must be split in 2 arcs. Each arc
					-- will then be treated separately.
					log (text => "splitting of circle required", level => lth + 1);
					log_indentation_up;
					
					set_break_points (circle_tmp);

					case break_count is
						when 0 => null;
						when 1 =>

							-- Rotate and move the break point back according to
							-- the track direction and offset:
							rotate_to (bp1, track_dimensions.direction);
							move_by (bp1, track_dimensions.offset);

							log (text => "break point 1 " & type_place'image (place) & " arc:" & to_string (bp1),
									level => lth + 1);

						when 2 =>
							rotate_to (bp1, track_dimensions.direction);
							move_by (bp1, track_dimensions.offset);

							log (text => "break point 1 " & type_place'image (place) & " arc:" & to_string (bp1),
									level => lth + 1);

							rotate_to (bp2, track_dimensions.direction);
							move_by (bp2, track_dimensions.offset);
							
							log (text => "break point 2 " & type_place'image (place) & " arc:" & to_string (bp2),
								level => lth + 1);

					end case;

					log_indentation_down;
					
				when others =>
					raise constraint_error; -- CS useful message
			end case;
			
			log_indentation_down;
		end if;

		
		case break_count is
			when 0 => return (count => 0);
			when 1 => return (1, bp1);
			when 2 => return (2, bp1, bp2);
		end case;
	end get_break_by_circle;

	

	function after_start_of_track (
		track	: in type_track;
		point	: in type_point)
		return boolean
	is
		track_start : constant type_point := to_point (track.center.v_start);
		track_direction : constant type_rotation := get_angle (track.center);
		point_direction : constant type_rotation := get_angle (get_distance (track_start, point));
	begin
		if point_direction = track_direction then
			return true;
			
		elsif point_direction = - track_direction then
			return false; -- point is before start point of track
			
		else
			raise constraint_error with
				"ERROR: Point" & to_string (point) & " is not on track !";
			-- CS output track properties (start, width, clearance);
			
			--return false;
		end if;
	end after_start_of_track;

	
	function get_distance (
		module_cursor	: in pac_generic_modules.cursor;
		start_point		: in type_point;
		place			: in type_place := BEFORE;
		direction		: in type_rotation;
		net_cursor		: in et_schematic.pac_nets.cursor := et_schematic.pac_nets.no_element;
		fill_zone		: in type_fill_zone;
		layer			: in type_signal_layer;
		width			: in type_track_width;
		ignore_same_net	: in boolean;
		lth				: in type_log_level)
		return type_route_distance is separate;


	
end et_routing;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
