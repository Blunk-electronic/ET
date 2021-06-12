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
with ada.tags;					use ada.tags;

with ada.numerics.generic_elementary_functions;

with et_vias;					use et_vias;
with et_terminals;				use et_terminals;
with et_packages;				use et_packages;
with et_pcb;					use et_pcb;
with et_pcb_stack;				use et_pcb_stack;
with et_pcb_coordinates;		use et_pcb_coordinates;
with et_board_shapes_and_text;



package body et_routing is
	
	use et_pcb_coordinates.pac_geometry_brd;
	use et_board_shapes_and_text.pac_shapes;

	package functions_float is new ada.numerics.generic_elementary_functions (float);
	use functions_float;

	use pac_generic_modules;
	
	
	function compute_clearance (
		status			: in type_point_status; -- transition to inside/outside area
		y_position		: in type_distance; -- the y-position of the fill line
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
			min_error : constant float := float (type_distance'small);
			
		begin -- compute_convex
			log (text => "computing convex. targeted clearance" 
				 & to_string (type_distance (clearance_min)),
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
						center_x : type_distance := get_x (intersection.center);
						delta_x : type_distance_positive := intersection.x_position - center_x;
						x_pos_mirrored : type_distance := center_x - delta_x;
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

				log (text => " error" & to_string (type_distance (error)), level => log_threshold + 2);

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
			min_error : constant type_distance_positive := type_distance'small;
			
			-- The initial position of the cap is at maximum distance away from the point
			-- of intersection. It is right below the center of the arc:
			P : type_point := type_point (set (
				x => get_x (intersection.center), -- the x-value of the center of the arc
				y => y_position));

			type type_direction is (RIGHT, LEFT);
			
			procedure shift_cap (direction : in type_direction) is
				offset : type_point;

				-- The amount by which the cap will be shifted right or left:
				dx : type_distance_positive := zero;

			begin
				log (text => " shift cap " & type_direction'image (direction), 
					level => log_threshold + 2);

				case direction is
					when RIGHT =>

						dx := error;

						-- The cap wil be moved right. So dx must be positive:
						offset := type_point (set (x => + dx, y => zero));
						
					when LEFT =>

						dx := error;
						
						-- The cap wil be moved left. So dx must be negative:
						offset := type_point (set (x => - dx, y => zero));
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
			clearance_min_concave : constant type_distance_positive := type_distance (clearance_min);
			
			clearance : type_distance_positive;

			result : type_distance_positive;
			
		begin -- compute_concave
			log (text => "computing concave. targeted clearance" 
				 & to_string (clearance_min_concave),
				 level => log_threshold + 2);

			log_indentation_up;

			--log (text => "intersection x" & to_string (intersection.x_position));

			clearance := intersection.radius - distance_total (intersection.center, P);

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
					
					clearance := intersection.radius - distance_total (intersection.center, P);

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
		point			: in type_point)
		return type_distance_polar
	is
		result : type_distance_polar := to_polar (type_distance_positive'last, zero_rotation);

		procedure update (d : in type_distance_polar) is begin
			if get_absolute (d) < get_absolute (result) then
				result := d;
			end if;
		end update;

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in et_schematic.type_module) 
		is
			use pac_pcb_cutouts;
			procedure query_hole (c : pac_pcb_cutouts.cursor) is begin
				update (get_shortest_distance (element (c), point));
			end query_hole;
			
		begin
			-- test board outline:
			result := get_shortest_distance (module.board.contours.outline, point);
			
			-- test holes in board:
			iterate (module.board.contours.holes, query_hole'access);
		end query_module;

	begin
		query_element (module_cursor, query_module'access);		
		return result;
	end get_distance_to_edge;
	
	
	function on_board (
		module_cursor	: in pac_generic_modules.cursor;
		point			: in type_point)
		return boolean
	is
		result : boolean := true;
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in et_schematic.type_module) 
		is
			procedure query_outline is begin
				if in_polygon_status (module.board.contours.outline, point).status = OUTSIDE then
					result := false;
				end if;
			end query_outline;
			
			
			procedure query_holes is
				use pac_pcb_cutouts;
				c : pac_pcb_cutouts.cursor := module.board.contours.holes.first;
			begin
				while c /= pac_pcb_cutouts.no_element loop

					if in_polygon_status (element (c), point).status = INSIDE then
						-- point is inside a hole
						result := false;
						exit; -- no need to test other holes
					end if;
					
					next (c);
				end loop;
			end query_holes;
			
		begin
			-- test board outline:
			query_outline;

			if result = true then -- point is within board outlines
				
				-- test holes in board:
				query_holes;
			end if;
		end query_module;

	begin
		query_element (module_cursor, query_module'access);

		return result;
	end on_board;

						  
	function clear_for_track (
		module_cursor	: in pac_generic_modules.cursor;
		start_point		: in type_point;
		net				: in et_schematic.pac_nets.cursor;
		layer			: in type_signal_layer;
		width			: in type_track_width)
		return boolean
	is
		result : boolean := false;

		design_rules : constant type_design_rules := get_pcb_design_rules (module_cursor);
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in et_schematic.type_module) 
		is
		begin -- query_module

			null;
			
			-- if fill_zone.observe then query 
			-- - fill_zone.outline
			-- - global cutout areas
			-- - net specific cutout areas
			
			-- CS abort if status is invalid.
			
			-- query tracks, texts, pads, ...
		end query_module;

		distance_to_edge : type_distance_positive;
		
	begin
		if on_board (module_cursor, start_point) then

			distance_to_edge := get_absolute (get_distance_to_edge (module_cursor, start_point));

			--log (text => "d to edge:" & to_string (distance_to_edge));
			--distance_to_edge := distance_to_edge - 0.5 * width;
			
			--if distance_to_edge >= design_rules.clearances.conductor_to_board_edge then
			if distance_to_edge >= design_rules.clearances.conductor_to_board_edge + 0.5 * width then
				result := true;
			end if;
				
			--query_element (module_cursor, query_module'access);
		end if;

		return result;
	end clear_for_track;


	function get_total_width (
		track	: in type_track)
		return type_track_clearance
	is begin
		return track.width + track.clearance;
	end get_total_width;

	
	function get_overlap (
		track	: in type_track;
		line	: in type_line)
		return type_overlap
	is
		-- here the given track starts:
		track_start : constant type_point := to_point (track.center.v_start);

		-- the given track travels in this direction:
		track_direction : constant type_rotation := get_angle (track.center);

		-- the total track width (incl. clearance) is:
		track_width_total : constant type_track_width := get_total_width (track);

		-- the offset of the track relative to the origin is:
		offset : constant type_point := track_start;

		-- build a horizontally traveling track that starts at the origin
		-- and runs to the far right:
		track_line : constant type_line := (
					--start_point	=> track_start,
					start_point	=> origin,
					end_point	=> type_point (set (
									x => far_right - track_width_total * 0.5,
									y => zero)));

		-- build the boundaries of the track:
		track_boundaries : constant type_boundaries := 
			get_boundaries (track_line, track_width_total);

		line_tmp : type_line := line;
		line_boundaries : type_boundaries;

		bi : type_boundaries_intersection;
		ol_start, ol_end : type_point;
		
	begin
		-- Move the given line by the offset towards the origin,
		-- rotate the line by the track direction and 
		-- build the boundaries of the line:
		move_by (line_tmp, type_point (invert (offset)));
		rotate_by (line_tmp, - track_direction);
		line_boundaries := get_boundaries (line_tmp, zero);

		-- Get the intersection of track and line boundaries:
		bi := get_intersection (track_boundaries, line_boundaries);

		if bi.exists then
			-- Compute the points of overlap begin and end,
			-- rotate them by the track direction and
			-- move them by the offset:
			ol_start := type_point (set (bi.intersection.smallest_x, zero));
			ol_end   := type_point (set (bi.intersection.greatest_x, zero));

			-- CS numerical approch that moves ol_start to the right
			-- and ol_end to the left until the cap of the track
			-- barely touches the line.
			-- CS if we investigate the intersections with the upper
			-- and lower edge of the track, then the analytical solution could be better.
			-- See get_overlap for arc below.

			rotate_to (ol_start, track_direction);
			move_by (ol_start, offset);
			
			rotate_to (ol_end, track_direction);
			move_by (ol_end, offset);
			
			return (true, ol_start, ol_end);
			
		else
			return (exists => false);
		end if;
	end get_overlap;


	function get_overlap (
		track	: in type_track;
		arc		: in type_arc)
		return type_overlap
	is
		-- here the given track starts:
		track_start : constant type_point := to_point (track.center.v_start);

		-- the given track travels in this direction:
		track_direction : constant type_rotation := get_angle (track.center);

		-- the total track width (incl. clearance) is:
		track_width_total : constant type_track_width := get_total_width (track);

		-- the offset of the track relative to the origin is:		
		offset : constant type_point := track_start;

		-- build a horizontally traveling track that starts at the origin
		-- and runs to the far right:
		track_line : constant type_line := (
					start_point	=> origin,
					end_point	=> type_point (set (
									x => far_right - track_width_total * 0.5,
									y => zero)));
		
		-- build the boundaries of the track:
		track_boundaries : constant type_boundaries :=
			get_boundaries (track_line, track_width_total);

		--
		track_upper_edge_start : constant type_point := type_point (
				set (track_boundaries.smallest_x, track_boundaries.greatest_y));

		track_upper_edge_end : constant type_point := type_point (
				set (track_boundaries.greatest_x, track_boundaries.greatest_y));

		--
		track_lower_edge_start : constant type_point := type_point (
				set (track_boundaries.smallest_x, track_boundaries.smallest_y));

		track_lower_edge_end : constant type_point := type_point (
				set (track_boundaries.greatest_x, track_boundaries.smallest_y));

		--
		track_upper_edge : constant type_line := (track_upper_edge_start, track_upper_edge_end);
		track_lower_edge : constant type_line := (track_lower_edge_start, track_lower_edge_end);

		--
		function move_and_rotate_arc (arc : in type_arc) return type_arc is
			a : type_arc := arc;
		begin
			move_by (a, type_point (invert (offset)));
			rotate_by (a, - track_direction);
			return a;
		end move_and_rotate_arc;

		-- The arc we will work with from now on.
		-- (It is now moved and rotated in the same way as the track_line):
		arc_tmp : constant type_arc := move_and_rotate_arc (arc);
		arc_boundaries : constant type_boundaries := get_boundaries (arc_tmp, zero);
		-- (The arc has zero line width.)

		
		-- the intersections of the upper and lower edge of the track
		-- with the arc:
		i_upper : constant type_intersection_of_line_and_circle :=
			get_intersection (to_line_vector (track_upper_edge), arc);

		i_lower : constant type_intersection_of_line_and_circle :=
			get_intersection (to_line_vector (track_lower_edge), arc);

		-- the area where track and arc boundaries intersect:
		bi : constant type_boundaries_intersection := 
			get_intersection (track_boundaries, arc_boundaries);
		
		ol_start, ol_end : type_point;

		procedure rotate_and_move_back is begin
			rotate_to (ol_start, track_direction);
			move_by (ol_start, offset);
			
			rotate_to (ol_end, track_direction);
			move_by (ol_end, offset);
		end rotate_and_move_back;
		
	begin -- get_overlap

		if bi.exists then -- arc and track do intersect in some way
			
			if 
			 (i_upper.status = ONE_EXISTS and i_lower.status = ONE_EXISTS) 
			or 
			 (i_upper.status = ONE_EXISTS and i_lower.status = NONE_EXIST)
			or
			 (i_upper.status = NONE_EXIST and i_lower.status = ONE_EXISTS)
			or
			 (i_upper.status = NONE_EXIST and i_lower.status = NONE_EXIST)
			then
				
				ol_start := type_point (set (bi.intersection.smallest_x, zero));
				ol_end   := type_point (set (bi.intersection.greatest_x, zero));

				-- CS numerical approch that moves ol_start to the right
				-- and ol_end to the left until the cap of the track
				-- barely touches the arc.
				
				rotate_and_move_back;				
			
				return (true, ol_start, ol_end);
			end if;
			
		else
			-- If no boundaries exist, then there is no overlap:
			return (exists => false);
		end if;

		return (exists => false);
	end get_overlap;
	

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

	
	function get_break (
		track	: in type_track;
		line	: in type_line;
		place	: in type_place)
		return type_break
	is
		--track_direction : constant type_rotation := get_angle (track.center);
		--track_start : constant type_point := to_point (track.center.v_start);
		ol : constant type_overlap := get_overlap (track, line);
		
		line_vector : constant type_line_vector := to_line_vector (line);

		break_point : type_point;
	begin
		if ol.exists then
		
			case place is
				when BEFORE =>

					break_point := ol.start_point;

					-- The break must be after the start of the track:
					if after_start_of_track (track, break_point) then
					
						move_by (break_point, type_point (set (- type_distance'small, zero)));
						-- CS
							
						-- The begin of the break must be after the start of the track:
						if after_start_of_track (track, break_point) then
							return (exists => true, point => break_point);
						else
							return (exists => false);
						end if;

					else
						return (exists => false);
					end if;

					
				when AFTER =>
					break_point := ol.end_point;

					-- The break must be after the start of the track:
					if after_start_of_track (track, break_point) then
					
						move_by (break_point, type_point (set (+ type_distance'small, zero)));
						-- CS

						-- The end of the break must be after the start of the track:
						if after_start_of_track (track, break_point) then
							return (exists => true, point => break_point);
						else
							return (exists => false);
						end if;

					else
						return (exists => false);
					end if;

					
			end case;

		else
			return (exists => false);
		end if;
	end get_break;

	
	function get_break (
		track	: in type_track;
		arc		: in type_arc;
		place	: in type_place)
		return type_break
	is
		--track_start : constant type_point := to_point (track.center.v_start);
		
		--i : constant type_intersection_of_line_and_circle := get_intersection (track.center, arc);
		--oi : type_ordered_line_circle_intersections;
		
		result : type_break (exists => false);
	begin
		--case i.status is
			--when NONE_EXIST => return (exists => false);
			--when ONE_EXISTS => 
				--if i.tangent_status = SECANT then
					--return (exists => true, point => to_point (i.intersection.point));
				--else
					--return (exists => false);
				--end if;
				
			--when TWO_EXIST => 
				---- first intersection with circle after track start point only !
				----get_first
				----oi := order_intersections (track_start, i);
				
				--return (exists => false);

		--end case;
		
		return result;
	end get_break;


	function get_break (
		track	: in type_track;
		circle	: in type_circle;
		place	: in type_place)
		return type_break
	is
		result : type_break (exists => false);
	begin
		-- first intersection with circle after track start point only !
		return result;
	end get_break;

	
	function get_distance (
		module_cursor	: in pac_generic_modules.cursor;
		start_point		: in type_point;
		place			: in type_place := BEFORE;
		direction		: in type_rotation;
		net				: in et_schematic.pac_nets.cursor := et_schematic.pac_nets.no_element;
		fill_zone		: in type_fill_zone;
		layer			: in type_signal_layer;
		width			: in type_track_width;
		log_threshold	: in type_log_level)
		return type_route_distance
	is
		probe_ray : constant type_ray := (start_point, direction);
		probe_line : constant type_line_vector := to_line_vector (probe_ray);

		design_rules : constant type_design_rules := get_pcb_design_rules (module_cursor);
		
		track : type_track := (
			center		=> probe_line,
			width		=> width,
			others		=> <>);

		distance_to_obstacle : type_distance_positive := type_distance'last;
		distance_after_obstacle : type_distance_positive := type_distance'last;
		status : type_valid := VALID;

		package pac_points_after_obstacles is new doubly_linked_lists (type_point);
		points_after_obstacles : pac_points_after_obstacles.list;
		use pac_points_after_obstacles;
		package pac_sorting is new pac_points_after_obstacles.generic_sorting;
		
		procedure process_break (break : in type_point) is
			d : type_distance_positive;
		begin
			case place is
				when BEFORE =>
					d := distance_total (start_point, break);
					
					if d < distance_to_obstacle then
						distance_to_obstacle := d;
					end if;

				when AFTER =>
					append (points_after_obstacles, break);
					
			end case;
		end process_break;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in et_schematic.type_module) 
		is
			use pac_polygon_segments;

			procedure query_segment (c : in pac_polygon_segments.cursor) is
				
				procedure test_line is 
					b : constant type_break := get_break (track, element (c).segment_line, place);
				begin
					if b.exists then
						process_break (b.point);
					end if;
				end test_line;

				procedure test_arc is
					b : constant type_break := get_break (track, element (c).segment_arc, place);
				begin
					if b.exists then
						process_break (b.point);
					end if;						
				end test_arc;
				
			begin -- query_segment
				case element (c).shape is
					when LINE	=> test_line;
					when ARC	=> test_arc;
				end case;
			end query_segment;

			
			procedure query_circle (c : in type_circle) is 
				b : constant type_break := get_break (track, c, place);
			begin
				if b.exists then
					process_break (b.point);
				end if;
			end query_circle;

			
			procedure query_outline is begin
				if module.board.contours.outline.contours.circular then
					query_circle (module.board.contours.outline.contours.circle);
				else
					iterate (module.board.contours.outline.contours.segments, query_segment'access);
				end if;
			end query_outline;

			procedure query_holes is
				use pac_pcb_cutouts;

				procedure query_hole (c : in pac_pcb_cutouts.cursor) is begin
					if element (c).contours.circular then
						query_circle (element (c).contours.circle);
					else
						iterate (element (c).contours.segments, query_segment'access);
					end if;
				end query_hole;
				
			begin
				iterate (module.board.contours.holes, query_hole'access);
			end query_holes;
			
		begin -- query_module

			track.clearance	:= design_rules.clearances.conductor_to_board_edge;

			-- board contours:
			query_outline;
			query_holes;

			-- if fill_zone.observe then query 
			-- - fill_zone.outline
			-- - global cutout areas
			-- - net specific cutout areas
			
			-- CS abort if status is invalid.
			
			-- query tracks, texts, pads, ...
		end query_module;


		procedure find_valid_point_after_obstacles is
			c : pac_points_after_obstacles.cursor;
		begin
			query_element (module_cursor, query_module'access);
			pac_sorting.sort (points_after_obstacles);

			c := points_after_obstacles.first;
			while c /= pac_points_after_obstacles.no_element loop

				if clear_for_track (module_cursor, element (c), net, layer, width) then
					distance_after_obstacle := distance_total (start_point, element (c));
					exit;
				end if;
				
				next (c);
			end loop;

			if c = pac_points_after_obstacles.no_element then
				status := INVALID;
			else
				status := VALID;
			end if;
		end find_valid_point_after_obstacles;

		
	begin -- get_distance
		case place is
			when BEFORE =>
				log (text => "distance to obstacle from point" 
					 & to_string (start_point)
					 & " direction" & to_string (direction),
					 level => log_threshold);

				log_indentation_up;
				
				-- test whether start_point is suitable to start a track
				if clear_for_track (module_cursor, start_point, net, layer, width) then
					query_element (module_cursor, query_module'access);

					--if distance_to_obstacle = type_distance'last then
						--raise constraint_error;
					--end if;

					log (text => to_string (distance_to_obstacle),
						level => log_threshold);

					log_indentation_down;
					
					return (VALID, distance_to_obstacle);
				else

					--log (text => "no obstacle found",
					log (text => "track not allowed here",
						level => log_threshold);

					log_indentation_down;
					
					return (status => INVALID);
				end if;


				
			when AFTER =>
				log (text => "distance after obstacles from point" 
					 & to_string (start_point)
					 & " direction" & to_string (direction),
					 level => log_threshold);

				log_indentation_up;
				
				find_valid_point_after_obstacles;
				
				case status is
					when VALID =>
						log (text => to_string (distance_after_obstacle),
							level => log_threshold);

						log_indentation_down;
						
						return (VALID, distance_after_obstacle);

						
					when INVALID => 
						log (text => "no obstacle found",
							level => log_threshold);

						log_indentation_down;
						
						return (status => INVALID);
				end case;

				
		end case;

	end get_distance;


	
end et_routing;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
