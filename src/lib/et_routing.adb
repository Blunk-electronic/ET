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

	function "<" (left, right : in type_proximity_point) return boolean is begin
		if left.x < right.x then
			return true;
		else
			return false;
		end if;
	end "<";
		
	function to_string (prox_points : in pac_proximity_points.set)
		return string
	is
		use pac_proximity_points;
		use ada.strings.unbounded;
		
		text : unbounded_string := to_unbounded_string ("proximity points (x/status):");
		
		procedure query_point (c : in pac_proximity_points.cursor) is begin
			text := text & to_string (element (c).x) 
				& "/" & type_stop_go'image (element (c).status);
		end query_point;
		
	begin
		if not is_empty (prox_points) then
			iterate (prox_points, query_point'access);
			return to_string (text);
		else
			return "no proximity points";
		end if;
	end to_string;


	
	function get_polygon_proximity_points (
		polygon			: in type_polygon_conductor;
		length			: in type_distance_positive;
		start			: in type_point;
		line_width		: in type_track_width;  -- the width of the fill line
		log_threshold	: in type_log_level)
		return pac_proximity_points.set
	is
		points_preliminary	: pac_proximity_points.set;
		points_final		: pac_proximity_points.set; -- to be returned
		
		half_width : constant type_distance := line_width * 0.5;
		lower_edge : constant type_distance := Y (start) - half_width;
		upper_edge : constant type_distance := Y (start) + half_width;

		-- Build a probe line that runs along the lower edge of the fill line:
		pl_l : constant type_line_vector := (
			v_start		=> to_vector (type_point (set (X (start), lower_edge))),
			v_direction	=> to_vector (type_point (set (1.0, 0.0))));

		-- Build a probe line that runs along the middle of the fill line:
		pl_c : constant type_line_vector := (
			v_start		=> to_vector (start),
			v_direction	=> to_vector (type_point (set (1.0, 0.0))));

		-- Build a probe line that runs along the upper edge of the fill line:
		pl_h : constant type_line_vector := (
			v_start		=> to_vector (type_point (set (X (start), upper_edge))),
			v_direction	=> to_vector (type_point (set (1.0, 0.0))));

			
		boundaries_probe_line : type_boundaries;
			
		segments : constant type_polygon_segments := get_segments (polygon);

		str_bound_int	: constant string := "boundaries intersect";
		str_lower_edge	: constant string := "intersects lower edge at";
		str_upper_edge	: constant string := "intersects upper edge at";
		str_between		: constant string := "between lower and upper edge";

		type type_edge is (UPPER, LOWER);
		
		function compute_for_slope (
			center_line	: in type_line_vector;
			edge_line	: in type_line_vector;
			intersection_tangent : in type_line_vector;
			intersection: in type_intersection;
			edge		: in type_edge;
			direction	: in type_line_direction)
			return type_distance
		is
			--clv : constant type_line_vector := to_line_vector (candidate_line);
			m1, m2 : type_intersection_of_two_lines (status => EXISTS);

			lv2 : type_line_vector;
			ray : type_ray;

		begin
			m1 := get_intersection (edge_line, intersection_tangent);
			
			ray.start_point := to_point (m1.intersection.point);

			-- The given intersection.angle is of little use since it ranges between 0 and 180
			-- degrees. Combined with the given direction we get the exact angle
			-- of intersection.
			
			case edge is
				when UPPER =>
					case direction is
						when RISING =>
							if intersection.angle > 90.0 then
								-- ray.direction := (180.0 - intersection.angle) * 0.5;
								-- the simplified version:
								ray.direction := 90.0 - intersection.angle * 0.5;
								
							elsif intersection.angle < 90.0 then
								ray.direction := intersection.angle * 0.5;
								
							else
								raise constraint_error; -- CS should never happen
							end if;
								
							--log (text => "rising direction" & to_string (ray.direction), level => log_threshold + 3);
							
						when FALLING =>
							if intersection.angle > 90.0 then
								-- ray.direction := 180.0 - (180.0 - intersection.angle) * 0.5;
								-- the simplified version:
								ray.direction := 90.0 + intersection.angle * 0.5;
								
							elsif intersection.angle < 90.0 then
								ray.direction := 180.0 - intersection.angle * 0.5;
								
							else
								raise constraint_error; -- CS should never happen
							end if;
							
							--log (text => "falling direction" & to_string (ray.direction), level => log_threshold + 3);
							
						when others => raise constraint_error; -- CS
					end case;

				when LOWER =>
					case direction is
						when RISING =>
							if intersection.angle > 90.0 then
								-- ray.direction := 180.0 + (180.0 - intersection.angle) * 0.5;
								-- the simplified version:
								ray.direction := 270.0 - intersection.angle * 0.5;
								
							elsif intersection.angle < 90.0 then
								ray.direction := 180.0 + intersection.angle * 0.5;
								
							else
								raise constraint_error; -- CS should never happen
							end if;

						when FALLING =>
							if intersection.angle > 90.0 then
								-- ray.direction := - (180.0 - intersection.angle) * 0.5;
								-- the simplified version:
								ray.direction := - 90.0 + intersection.angle * 0.5;
								
							elsif intersection.angle < 90.0 then
								ray.direction := - (intersection.angle) * 0.5;
								
							else
								raise constraint_error; -- CS should never happen
							end if;

						when others => raise constraint_error; -- CS
					end case;
			end case;
					
			lv2 := to_line_vector (ray);
			m2 := get_intersection (center_line, lv2);
			
			return get_x (m2.intersection.point);
		end compute_for_slope;

		
		use pac_polygon_lines;
		use pac_polygon_arcs;
		use pac_polygon_circles;
		
		procedure query_line (c : in pac_polygon_lines.cursor) is
			use pac_proximity_points;
			
			candidate_line : constant type_polygon_line := element (c);

			candidate_line_direction : constant type_line_direction := get_direction (candidate_line);
			
			-- Compute the boundaries of the candidate line.
			-- NOTE: The candidate line is an edge of the polygon and thus has no width.
			boundaries : constant type_boundaries := get_boundaries (
				line	=> candidate_line,
				width	=> zero);

			-- The place in x-direction where the fill line has to start or to stop:
			x_stop_go : type_distance := zero;
			
			procedure test_intersection is
				i_h : constant type_intersection_of_two_lines := get_intersection (pl_h, candidate_line);
				i_c : constant type_intersection_of_two_lines := get_intersection (pl_c, candidate_line);
				i_l : constant type_intersection_of_two_lines := get_intersection (pl_l, candidate_line);

				function get_intersections (tp : in type_point) return type_ordered_line_circle_intersections is
					-- build a circle about the given touch point:
					type tc is new type_circle with null record;
					c : constant tc := (center => tp, radius => half_width);

					ilc : type_intersection_of_line_and_circle (status => TWO_EXIST);
					-- NOTE: There must be two intersections of the probe line (center line)
					-- and the circle !
				begin
					ilc := get_intersection (
							line	=> pl_c,
							circle	=> c);

					-- Return the intersections ordered from left to right (relative to the given
					-- start point):
					return order_intersections (start, ilc);
				end get_intersections;

				oi_center_line : type_ordered_line_circle_intersections;
				
			begin -- test_intersection
				
				if i_c.status = EXISTS then
					-- Ignore an intersection with the center of the probe line.
					-- This is not a proximity point.
					null;
				else					
					case i_l.status is
						when EXISTS =>
							x_stop_go := X (to_point (i_l.intersection.point));

							log (text => str_lower_edge & to_string (x_stop_go), level => log_threshold + 3);

							case candidate_line_direction is
								when RISING =>
									x_stop_go := compute_for_slope (
														pl_c, pl_h, 
														to_line_vector (candidate_line),
														i_l.intersection, LOWER, candidate_line_direction);
									
									insert (points_preliminary, (status => STOP, x => x_stop_go));

								when FALLING =>
									x_stop_go := compute_for_slope (
														pl_c, pl_h,
														to_line_vector (candidate_line),
														i_l.intersection, LOWER, candidate_line_direction);
							
									insert (points_preliminary, (status => GO, x => x_stop_go));
									
								when VERTICAL => null; -- ignored. CS test required
									
								when HORIZONTAL => raise constraint_error;
									-- CS should never happen. Already covered on overlap of probe
									-- line with candidate line.
							end case;
									
						when OVERLAP => null;
							log (text => "overlaps lower edge", level => log_threshold + 3);
						
						when NOT_EXISTENT =>
							-- Candidate line of polygon line starts and ends somewhere 
							-- above the lower edge of the fill line:
							
							case i_h.status is
								when EXISTS => 
									x_stop_go := X (to_point (i_h.intersection.point));
									
									log (text => str_upper_edge & to_string (x_stop_go), level => log_threshold + 3);

									case candidate_line_direction is
										when RISING =>
											x_stop_go := compute_for_slope (
															pl_c, pl_l, 
															to_line_vector (candidate_line),
															i_h.intersection, UPPER, candidate_line_direction);

											insert (points_preliminary, (status => GO, x => x_stop_go));
											
										when FALLING =>
											x_stop_go := compute_for_slope (
															pl_c, pl_l,
															to_line_vector (candidate_line),
															i_h.intersection, UPPER, candidate_line_direction);
											
											insert (points_preliminary, (status => STOP, x => x_stop_go));
											
										when VERTICAL => null; -- ignored. CS test required
										
										when HORIZONTAL => raise constraint_error;
											-- CS should never happen. Already covered on overlap of probe
											-- line with candidate line.
									end case;

								
								when OVERLAP =>
									log (text => "overlaps upper edge", level => log_threshold + 3);
									
								when NOT_EXISTENT => 
									
									-- Candidate line of polygon line starts and ends somewhere below
									-- the upper edge of the fill line:

									log (text => str_between, level => log_threshold + 3);

									-- The border of the fill line cap touches the candidate line
									-- at some point. But the ends of the candidate line have a
									-- y position different from the probe line. So we imagine a circle
									-- with the touch point as its center. The circle then intersects
									-- the probe line (center line of fill line) on two points.

									-- compute the STOP mark:
									oi_center_line := get_intersections (get_left_end (candidate_line, boundaries));
									
									x_stop_go := get_x (oi_center_line.entry_point.point);

									-- CS: test number of intersections of candidate line with cap.
									-- If there are two intersections then x_stop_go must be reduced
									-- via a loop until there is only a tangent left.
									
									insert (points_preliminary, (status => STOP, x => x_stop_go));

									
									
									-- compute the GO mark:
									oi_center_line := get_intersections (get_right_end (candidate_line, boundaries));
									
									x_stop_go := get_x (oi_center_line.exit_point.point);

									-- CS: test number of intersections of candidate line with cap.
									-- If there are two intersections then x_stop_go must be increased
									-- via a loop until there is only a tangent left.
									
									insert (points_preliminary, (status => GO, x => x_stop_go));
									
							end case;

					end case;
				end if;
			end test_intersection;
				
		begin -- query_line
			log (text => to_string (candidate_line) 
				 & to_string (candidate_line_direction) 
				 & " " & to_string (boundaries),
				 level => log_threshold + 2);

			log_indentation_up;

			if intersect (boundaries_probe_line, boundaries) then
				log (text => str_bound_int, level => log_threshold + 2);
				log_indentation_up;
				
				test_intersection;
				
				log_indentation_down;
			end if;

			log_indentation_down;
		end query_line;


		-- Generates a line vector of the given intersection.
		-- The line can be regarded as a tangent to the polygon edge
		-- on the point of intersection.
		function to_line (i : in type_intersection)
			return type_line_vector
		is
			v : type_line_vector;
			r : type_ray;
		begin
			r.start_point := to_point (i.point);
			r.direction := 180.0 - i.angle;

			return to_line_vector (r);
		end to_line;
		
		
		procedure query_arc (c : in pac_polygon_arcs.cursor) is

			use pac_proximity_points;
			
			candidate_arc : constant type_polygon_arc := element (c);

			-- Compute the boundaries of the candidate arc.
			-- NOTE: The candidate arc is an edge of the polygon and thus has no width.
			boundaries : constant type_boundaries := get_boundaries (
				arc			=> candidate_arc,
				line_width	=> zero);

			-- The place in x-direction where the fill line has to start or to stop:
			x_stop_go : type_distance := zero;

			tangent_direction : type_line_direction;
			
			procedure test_intersection is
				i_h : constant type_intersection_of_line_and_circle := get_intersection (pl_h, candidate_arc);
				i_c : constant type_intersection_of_line_and_circle := get_intersection (pl_c, candidate_arc);
				i_l : constant type_intersection_of_line_and_circle := get_intersection (pl_l, candidate_arc);

				-- If we have two intersections, then the one that is on the left, will 
				-- be the STOP mark. Because here the arc causes the fill line to stop.
				-- The intersection on the right will be the GO mark, because here a fill
				-- line has to start:
				procedure order_intersections (
					i : in type_intersection_of_line_and_circle;
					edge : in type_edge)
				is 
					x1 : constant type_distance := get_x (i.intersection_1.point);
					x2 : constant type_distance := get_x (i.intersection_2.point);
				begin
					if x1 < x2 then
						-- x1 comes before x2 in x-direction. Means x1 provides the STOP mark
						-- and x2 provides the GO mark:
						case edge is
							when UPPER =>
								log (text => str_upper_edge & to_string (x1) & " and" & to_string (x2),
									level => log_threshold +3);

								tangent_direction := get_tangent_direction (i.intersection_1.angle);

								x_stop_go := compute_for_slope (
										pl_c, pl_l, 
										to_line (i.intersection_1),
										i.intersection_1, edge, tangent_direction);

								insert (points_preliminary, (status => STOP, x => x_stop_go));


								
								tangent_direction := get_tangent_direction (i.intersection_2.angle);

								x_stop_go := compute_for_slope (
										pl_c, pl_l,
										to_line (i.intersection_2),
										i.intersection_2, edge, tangent_direction);

								insert (points_preliminary, (status => GO, x => x_stop_go));
								
								
								
							when LOWER =>
								log (text => str_lower_edge & to_string (x1) & " and" & to_string (x2),
									level => log_threshold +3); 

								tangent_direction := get_tangent_direction (i.intersection_1.angle);

								x_stop_go := compute_for_slope (
										pl_c, pl_h, 
										to_line (i.intersection_1),
										i.intersection_1, edge, tangent_direction);

								insert (points_preliminary, (status => STOP, x => x_stop_go));
								

								
								tangent_direction := get_tangent_direction (i.intersection_2.angle);

								x_stop_go := compute_for_slope (
										pl_c, pl_h,
										to_line (i.intersection_2),
										i.intersection_2, edge, tangent_direction);

								insert (points_preliminary, (status => GO, x => x_stop_go));
						end case;

						
						
					elsif x1 > x2 then
						-- x2 comes before x1 in x-direction. Means x2 provides the STOP mark
						-- and x1 provides the GO mark:

						case edge is
							when UPPER =>
								log (text => str_upper_edge & to_string (x2) & " and" & to_string (x1),
									level => log_threshold +3); 


								tangent_direction := get_tangent_direction (i.intersection_2.angle);

								x_stop_go := compute_for_slope (
										pl_c, pl_l, 
										to_line (i.intersection_2),
										i.intersection_2, edge, tangent_direction);

								insert (points_preliminary, (status => STOP, x => x_stop_go));


								
								tangent_direction := get_tangent_direction (i.intersection_1.angle);

								x_stop_go := compute_for_slope (
										pl_c, pl_l,
										to_line (i.intersection_1),
										i.intersection_1, edge, tangent_direction);

								insert (points_preliminary, (status => GO, x => x_stop_go));


								
							when LOWER =>
								log (text => str_lower_edge & to_string (x2) & " and" & to_string (x1),
									level => log_threshold +3); 

								tangent_direction := get_tangent_direction (i.intersection_2.angle);

								x_stop_go := compute_for_slope (
										pl_c, pl_h, 
										to_line (i.intersection_2),
										i.intersection_2, edge, tangent_direction);

								insert (points_preliminary, (status => STOP, x => x_stop_go));


								
								tangent_direction := get_tangent_direction (i.intersection_1.angle);

								x_stop_go := compute_for_slope (
										pl_c, pl_h,
										to_line (i.intersection_1),
										i.intersection_1, edge, tangent_direction);

								insert (points_preliminary, (status => GO, x => x_stop_go));

						end case;
						
					else 
						-- x1 and x2 are equal. CS should never happen
						raise constraint_error; 
					end if;
																 
				end order_intersections;
				
			begin -- test_intersection
				if i_c.status /= NONE_EXIST then
					-- Ignore an intersection with the center of the probe line.
					-- Because this is not a proximity point but an intersection point.
					null;
				else					
					case i_l.status is
						when ONE_EXISTS =>
							-- The intersection of lower edge and the arc must have the
							-- nature of a secant. If it is a tangent, then the arc is
							-- not entering the area of the fill line, hence this would 
							-- not be an obstacle.
							if i_l.tangent_status = SECANT then
								
								x_stop_go := X (to_point (i_l.intersection.point));

								log (text => str_lower_edge & to_string (x_stop_go)
									& " angle" & to_string (i_l.intersection.angle),
									level => log_threshold + 3);

								tangent_direction := get_tangent_direction (i_l.intersection.angle);
								
								case tangent_direction is
									when RISING =>
										
										x_stop_go := compute_for_slope (
														pl_c, pl_h, 
														to_line (i_l.intersection),
														i_l.intersection, LOWER, tangent_direction);
										
										insert (points_preliminary, (status => STOP, x => x_stop_go));

									when FALLING =>

										x_stop_go := compute_for_slope (
														pl_c, pl_h, 
														to_line (i_l.intersection),
														i_l.intersection, LOWER, tangent_direction);

										insert (points_preliminary, (status => GO, x => x_stop_go));
										
									when VERTICAL => null; -- ignored. CS test required
										
									when HORIZONTAL => raise constraint_error;
										-- This should never happen. Because the edge is not
										-- a tangent to the candidate arc.
								end case;
							end if;
							
						when TWO_EXIST =>
							-- Candidate arc intersects the lower edge twice. It comes from below
							-- crosses the lower edge, enters the area of the fill line (but does 
							-- not cross the center line), goes down, crosses the lower edge again
							-- and travels further down:
							order_intersections (i_l, LOWER);
						
						when NONE_EXIST =>
							-- Candidate arc of polygon intersects upper edge somewhere.
							
							case i_h.status is
								when ONE_EXISTS =>
									-- The intersection of upper edge and the arc must have the
									-- nature of a secant. If it is a tangent, then the arc is
									-- not entering the area of the fill line, hence this would 
									-- not be an obstacle.
									if i_h.tangent_status = SECANT then
									
										x_stop_go := X (to_point (i_h.intersection.point));
										
										log (text => str_upper_edge & to_string (x_stop_go)
											& " angle" & to_string (i_h.intersection.angle),
											level => log_threshold + 3);

										tangent_direction := get_tangent_direction (i_h.intersection.angle);

										case tangent_direction is
											when RISING =>
												x_stop_go := compute_for_slope (
																pl_c, pl_l,
																to_line (i_h.intersection),
																i_h.intersection, UPPER, tangent_direction);
												
												insert (points_preliminary, (status => GO, x => x_stop_go));
																							
											when FALLING =>
												x_stop_go := compute_for_slope (
																pl_c, pl_l,
																to_line (i_h.intersection),
																i_h.intersection, UPPER, tangent_direction);

												insert (points_preliminary, (status => STOP, x => x_stop_go));
												
											when VERTICAL => null; -- ignored. CS test required
											
											when HORIZONTAL => raise constraint_error;
											-- This should never happen. Because the edge is not
											-- a tangent to the candidate arc.

										end case;										
									end if;
									
								when TWO_EXIST =>
									-- Candidate arc intersects the upper edge twice. It comes from above
									-- crosses the upper edge, enters the area of the fill line (but does 
									-- not cross the center line), goes up, crosses the upper edge again
									-- and travels further up:
									order_intersections (i_h, UPPER);
									
								when NONE_EXIST => 									
									-- Candidate arc starts and ends somewhere between upper and lower edge
									-- (but does not cross the center line):

									log (text => str_between, level => log_threshold + 3);
									
									-- compute the STOP mark:
									
									-- The smallest x value of the candidate arc is where
									-- the fill line has to stop.
									x_stop_go := boundaries.smallest_x;

									-- Since the fill line has a width, the STOP mark is
									-- at some distance left of the just computed position:
									insert (points_preliminary, (status => STOP, x => x_stop_go - half_width));

									
									
									-- compute the GO mark:
									
									-- The greatest x value of the arc is where
									-- the fill line has to start:
									x_stop_go := boundaries.greatest_x;

									-- Since the fill line has a width, the GO mark is
									-- at some distance right of the just computed position:
									insert (points_preliminary, (status => GO, x => x_stop_go + half_width));
									
							end case;

					end case;
				end if;

			end test_intersection;
			
		begin -- query_arc
			log (text => to_string (candidate_arc) & " " & to_string (boundaries),
				 level => log_threshold + 2);

			log_indentation_up;

			if intersect (boundaries_probe_line, boundaries) then
				log (text => str_bound_int, level => log_threshold + 2);
				log_indentation_up;
				
				test_intersection;
				
				log_indentation_down;
			end if;

			log_indentation_down;
		end query_arc;

			
		-- The list "points_preliminary" may contain successive STOP or GO marks. 
		-- From a row of STOP marks the FIRST must be extracted.
		-- From a row of GO marks the LAST must be extracted.
		-- This procedure removes those excessive stop/go marks and stores the result
		-- in variable "points_final":
		procedure reduce is
			use pac_proximity_points;

			-- Since the probe line has started outside the polygon,
			-- there is no obstacle, hence we are initially not in a stop region.
			-- This flag changes to TRUE when a STOP region begins.
			-- It changes to FALSE when a GO region begins:
			stop_region : boolean := false;

			procedure query_point (c : in pac_proximity_points.cursor) is begin
	
				case element (c).status is
					when STOP =>
						if not stop_region then
							-- We are leaving a GO region and are about
							-- to enter a STOP region.
							stop_region := true;

							-- The the first STOP mark:
							points_final.insert (element (c));
						end if;
						
					when GO =>
						if stop_region then
							-- We are leaving a STOP region and are about
							-- to enter a GO region.
							stop_region := false;
						end if;

						-- Since we have to find the last GO mark in a row of
						-- successive GO marks we must look ahead to the next
						-- mark.
						-- If this is the last proximity point then take it.
						-- If this is NOT the last point, then take it if
						-- the next point is a stop mark.
						if next (c) = pac_proximity_points.no_element then
							-- last point
							points_final.insert (element (c));
						else
							-- not the last point
							if element (next (c)).status = STOP then
								-- next point is a stop mark
								points_final.insert (element (c));
							end if;
						end if;

				end case;
			end query_point;
			
		begin -- reduce
			log (text => "removing excessive stop/go marks ...", level => log_threshold + 3);
			log (text => " old marks: " & to_string (points_preliminary), level => log_threshold + 4);
			
			iterate (points_preliminary, query_point'access);				
		end reduce;

		-- The resulting list of proximity points needs a well defined first point.
		-- The x-position of the point is taken from the given start point.
		-- RULE 0:	If the resulting list points_final is empty (becaus no proximity
		--			points have been detected) then we insert a GO mark.
		-- RULE 1:	If points_final starts with a GO mark, then we prepend a STOP mark.
		-- RULE 2:	If points_final starts with a STOP mark, then we prepend a GO mark.

		-- RULE 3:	If there is only one GO mark at the end of the proximity points,
		-- 			then it is to be replaced by a GO mark that sits at the begin of
		--			the proximity points:
		procedure insert_start_point is
			use pac_proximity_points;
			sp_go	: constant type_proximity_point := (x => X (start), status => GO);
			sp_stop	: constant type_proximity_point := (x => X (start), status => STOP);
		begin
			if is_empty (points_final) then
				-- apply RULE 0:
				insert (points_final, sp_go);
				
			else
				if points_final.length = 1 and element (points_final.first).status = GO then
					-- apply RULE 3:
					points_final.clear;
					insert (points_final, sp_go);
					
				else
				
					case element (points_final.first).status is
						when GO =>
							-- apply RULE 1:
							if not contains (points_final, sp_stop) then
								insert (points_final, sp_stop);
							end if;

						when STOP =>
							-- apply RULE 2:
							if not contains (points_final, sp_go) then
								insert (points_final, sp_go);
							end if;
					end case;
					
				end if;
			end if;
			
		end insert_start_point;
		
	begin -- get_polygon_proximity_points
		log (text => "computing proximity points ...", level => log_threshold);
		log_indentation_up;

		log (text => "polygon length (x)" & to_string (length)
			 & " start" & to_string (start)
			 & " line width" & to_string (line_width),
			 level => log_threshold + 1);
		
		-- Compute the boundaries of the probe line.
		-- The probe line starts at start and ends at the far right
		-- end of the polygon.
		-- NOTE: The start point can have a negative x-value. For this reason
		-- the absolute must be used.
		boundaries_probe_line := get_boundaries (
			point_one	=> start,
			point_two	=> type_point (set (
								x => abs (X (start)) + length,
								y => Y (start))),
			width		=> line_width);


		log (text => "probe line " & to_string (boundaries_probe_line), level => log_threshold + 1);

		-- Probe the polygon contours for proximities with the probe line:		
		log (text => "probing polygon lines ...", level => log_threshold + 2);
		iterate (segments.lines, query_line'access);

		log (text => "probing polygon arcs ...", level => log_threshold + 2);
		iterate (segments.arcs, query_arc'access);

		-- NOTE: The circles will NOT be probed.
		-- They are outer edges of the polygon ! They are not cutout zones !
		-- The circles cause no proximity points.		

		-- Discard excessive stop/go marks.
		reduce;
		
		insert_start_point;
		
		log (text => to_string (points_final), level => log_threshold + 2);
		
		log_indentation_down;
		
		return points_final;
	end get_polygon_proximity_points;


	
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
						center_x : type_distance := X (intersection.center);
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

			-- Here is the numerical algorithm. It computes side_d (this is what we wnat),
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
				x => X (intersection.center), -- the x-value of the center of the arc
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
				result := abs (intersection.x_position - X (P));

			else
				-- Here is the numerical algorithm. It computes result (what we wnat), the distance 
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

				result := abs (intersection.x_position - X (P));
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



	function compute_fill_lines (
		module_cursor		: in pac_generic_modules.cursor;
		design_rules		: in type_design_rules;
		board_domain		: in type_inside_polygon_query_result;
		polygon_domain		: in type_inside_polygon_query_result;
		polygon_proximities	: in pac_proximity_points.set;
		width				: in type_track_width; -- the width of a fill line
		clearance			: in type_track_clearance;
		isolation 			: in type_track_clearance; 
		easing				: in type_easing;
		log_threshold		: in type_log_level)
		return pac_fill_lines.list
	is

		-- We will return a bunch of fill lines which are in a single row:
		result : pac_fill_lines.list;

		-- This is the y position of all fill lines. All fill lines
		-- are in the same row:
		y_position : constant type_distance := Y (board_domain.start); 

		-- A single fill line to be inserted in the result:
		fill_line : type_fill_line; -- G====S

		
		-- For each domain we have an individual flag:
		type type_domain_status is record
			board, polygon, -- CS rename to board_intersections, polygon_intersections ?
			polygon_proximities : type_stop_go := STOP;
		end record;

		domain_status : type_domain_status;
		
		-- After ANDing the status of the domains this the the final
		-- signal for start and stop of a fill line:
		final_line_status : type_stop_go := STOP;

		-- At the start point of the domains (All start at the same x-position),
		-- the individual line flags must be initialized:
		procedure init_line_statuses is 
			use pac_proximity_points;
		begin
			case board_domain.status is
				when INSIDE		=> domain_status.board := GO;
				when OUTSIDE	=> domain_status.board := STOP;
			end case;

			case polygon_domain.status is
				when INSIDE		=> domain_status.polygon := GO;
				when OUTSIDE	=> domain_status.polygon := STOP;
			end case;

			-- The probe line of the proximity points has started outside
			-- the polygon. So we there is no obstacle at this point:
			if is_empty (polygon_proximities) then
				domain_status.polygon_proximities := GO;
			else
				domain_status.polygon_proximities := element (polygon_proximities.first).status;
			end if;
			
			--log (text => "status board line: " & type_stop_go'image (domain_status.board),
				--level => log_threshold + 1);

			--log (text => "status polygon line: " & type_stop_go'image (domain_status.polygon),
				--level => log_threshold + 1);
			
		end init_line_statuses;

		procedure toggle_status_board_line is begin
			case domain_status.board is
				when STOP	=> domain_status.board := GO;
				when GO		=> domain_status.board := STOP;
			end case;
		end toggle_status_board_line;

		procedure toggle_status_polygon_line is begin
			case domain_status.polygon is
				when STOP	=> domain_status.polygon := GO;
				when GO		=> domain_status.polygon := STOP;
			end case;
		end toggle_status_polygon_line;
		
		procedure toggle_status_polygon_proximities_line is begin
			case domain_status.polygon_proximities is
				when STOP	=> domain_status.polygon_proximities := GO;
				when GO		=> domain_status.polygon_proximities := STOP;
			end case;
		end toggle_status_polygon_proximities_line;

		
		-- This flag indicates that the status for the final
		-- fill line has changed from GO to STOP or from STOP to GO:
		final_line_status_has_changed : boolean := false;

		
		
		-- The switches are just a collection of x-positions.
		-- For each domain we have a list of switches. 
		-- A switch point is the place where the STOP/GO status changes.
		use pac_distances;		

		type type_switches is record
			board, polygon, polygon_proximities : pac_distances.list;
		end record;

		switches : type_switches;
		

		
		use pac_probe_line_intersections;

		-- The result of an "inside-polyon-query" is a list of intersections where a probe
		-- line intersects a polygon, board contours, tracks, pads or vector text.
		-- This function takes the result of an "inside-polyon-query" and computes
		-- the exact positions where the domain issues a STOP/GO signal. The exact positions
		-- to be returned are the so called switches (see declarations above).
		-- Optionally, an extra clearance may be taken into account. Extra clearance
		-- is required for example when the given points are intersections with the board
		-- contours.
		function make_switches (
			points			: in type_inside_polygon_query_result;
			extra_clearance	: in boolean := false;					   
			clearance		: in type_track_clearance := type_track_clearance'first)					   
			return pac_distances.list
		is 
			switches : pac_distances.list; -- to be returned

			status_inside_outside : type_point_status := points.status;

			procedure query_intersection (c : in pac_probe_line_intersections.cursor) is

				-- The space to be computed between intersection and switch.
				spacing : type_track_clearance;

				-- The position of the switch:
				new_x : type_distance;

			begin
				-- At each intersection we have a transition from inside to outside
				-- or the other way around. The initial status depends on the status of
				-- the given list of intersection points. See declaration above.
				toggle_status (status_inside_outside);

				if extra_clearance then
					
					spacing := compute_clearance (
						status			=> status_inside_outside, -- transition to inside/outside area
						y_position		=> y_position,
						intersection	=> element (c),
						line_width		=> width,
						extra_clearance	=> true,
						clearance		=> clearance,
						log_threshold	=> log_threshold + 1);

				else
					
					spacing := compute_clearance (
						status			=> status_inside_outside, -- transition to inside/outside area
						y_position		=> y_position,
						intersection	=> element (c),
						line_width		=> width,
						log_threshold	=> log_threshold + 1);

				end if;

				
				case status_inside_outside is
					when INSIDE => -- A change from outside to inside occured.
						-- Fill area ENTERED.
						-- Create a new virtual intersection after the original
						-- intersection. The original intersection is omitted.
						new_x := element (c).x_position + spacing;

						append (switches, new_x);
						
					when OUTSIDE => -- A change from inside to outside occured.
						-- Fill area LEFT.
						-- Create a new virtual intersection before the original
						-- intersection. The original intersection is omitted.
						new_x := element (c).x_position - spacing;

						-- If the change to the outside comes before the
						-- change into the inside then both points must be discarded.
						-- Otherwise we would later get a fill line that has zero length or
						-- runs from right to the left (Fill lines always run from left to right.).
						if is_empty (switches) then
							append (switches, new_x);
						else
							if last_element (switches) < new_x then
								append (switches, new_x);
							else
								switches.delete_last;
							end if;
						end if;
						
				end case;
				
				log (text => "switch at" & to_string (new_x), level => log_threshold + 1);
				
			end query_intersection;
				
		begin -- make_switches
			log_indentation_up;
			
			iterate (points.intersections, query_intersection'access);

			log_indentation_down;
			
			return switches;
		end make_switches;




		-- A milestone is a point in x-direction where the fill line
		-- starts or where it comes to an end.
		-- When searching a milestone after a certain point in x-direction
		-- no further milestone could exist. For this reason astatus flag
		-- is required:

		type type_milestone_status is (
			VALID,		-- there is a milestone after a certain point in x-direction
			INVALID);	-- there is no milestone after a certain point in x-direction

		type type_milestone is record
			status	: type_milestone_status := INVALID;

			-- The absolute x-position of the milestone 
			-- after a certain point in x-direction:
			x		: type_distance := zero;
		end record;

		milestone : type_milestone;


		

		-- Computes the final_line_status by ANDing the individual
		-- domain statuses. 
		-- ALL domains must give their go for the final fill line.
		-- Sets the flag final_line_status_has_changed accordingly.
		procedure update_final_line_status is begin
			if  domain_status.board = GO
			and domain_status.polygon = GO 
			and domain_status.polygon_proximities = GO	
			then

				-- Detect a change of the status:
				if final_line_status = STOP then
					final_line_status_has_changed := true;
				else
					final_line_status_has_changed := false;
				end if;

				-- Set the line status:
				final_line_status := GO;
			else

				-- Detect a change of the status:
				if final_line_status = GO then
					final_line_status_has_changed := true;
				else
					final_line_status_has_changed := false;
				end if;

				-- Set the line status:
				final_line_status := STOP;
			end if;

			--if final_line_status_has_changed then
				--log (text => "line status " & type_stop_go'image (final_line_status),
					--level => log_threshold);
			--end if;
				
		end update_final_line_status;


		
		-- Sets the start or end point of a fill line if the current milestone
		-- is valid.
		-- The fill line always starts at the
		-- current milestone where the final_line_status changes to GO.
		-- The fill line ends when the final_line_status changes to STOP.
		procedure make_fill_line is
			use pac_fill_lines;
		begin
			if milestone.status = VALID then
				
				case final_line_status is 
					when GO =>
						fill_line.start_point := type_point (set (milestone.x, y_position));
						
					when STOP =>
						--if milestone.x_value > x_position then

						-- Its end point is assigned when the final_line_status has just CHANGED
						-- into a STOP mark.
						
							if final_line_status_has_changed then
								fill_line.end_point := type_point (set (milestone.x, y_position));
								
								append (result, fill_line);
							end if;
							
						--end if;
				end case;
				
			end if;			
		end make_fill_line;

		-- Locates the next milestone in x-direction forward the given 
		-- x-value "forward".
		-- If a valid milestone was found, updates the final line status by
		-- calling procedure update_final_line_status.
		function get_next_milestone (forward : in type_distance)
			return type_milestone
		is
			-- the milestone being build and to be returned:
			ms : type_milestone;

			-- These are the smallest distances found between "forward"
			-- and the points in the domains:
			type type_smallest_differences is array (1..3) of type_distance;
			sdx : type_smallest_differences := (others => type_distance'last);

			procedure query_board_switch (c : in pac_distances.cursor) is
				x : type_distance := element (c);
			begin
				if x > forward then
					ms.status := VALID;

					if x < sdx(1) then
						sdx(1) := x;
					end if;
				end if;
			end query_board_switch;

			procedure query_polygon_switch (c : in pac_distances.cursor) is
				x : type_distance := element (c);
			begin
				if x > forward then
					ms.status := VALID;

					if x < sdx(2) then
						sdx(2) := x;
					end if;
				end if;
			end query_polygon_switch;

			use pac_proximity_points;
			
			procedure query_polygon_proximities_switch (c : in pac_proximity_points.cursor) is
				x : type_distance := element (c).x;
			begin
				if x > forward then
					ms.status := VALID;

					if x < sdx(3) then
						sdx(3) := x;
					end if;
				end if;
			end query_polygon_proximities_switch;

			
			-- Finds the smallest difference among the domains and sets the
			-- absolute position of the milestone to be returned.
			-- Toggles the status of the individual lines.
			procedure set_x is
				dx : type_distance := type_distance'last;
			begin
				-- Find the nearest milestone among the domains
				-- by finding the smallest difference stored in array sdx:
				
				--log (text => "sdx 1" & to_string (sdx(1)), level => log_threshold + 1);
				--log (text => "sdx 2" & to_string (sdx(2)), level => log_threshold + 1);

				for i in type_smallest_differences'first .. type_smallest_differences'last loop

					if sdx(i) < dx then
						dx := sdx(i);
					end if;

				end loop;

				--log (text => "nearest" & to_string (dx), level => log_threshold + 1);
				
				-- Find the domains that have the current x_value.
				-- There can be more than one domain having the current x_value
				-- (This happens for example when board contour and polygon edge overlap.):
				for i in type_smallest_differences'first .. type_smallest_differences'last loop

					if sdx(i) = dx then
						case i is
							when 1 =>
								toggle_status_board_line;
							
							when 2 =>
								toggle_status_polygon_line;

							when 3 =>
								toggle_status_polygon_proximities_line;

							-- CS toggle others
						end case;
					end if;

				end loop;				

				-- The absolute position in x of the switch point is:
				ms.x := dx;
			end set_x;
			
		begin -- get_next_milestone
			
			log (text => "seaching next milestone after" & to_string (forward),
				level => log_threshold + 1);

			log_indentation_up;

			-- Find the board switch that is nearest to the given forward-position:
			iterate (switches.board, query_board_switch'access);

			-- Find the polygon switch that is nearest to the given forward-position:
			iterate (switches.polygon, query_polygon_switch'access);

			-- Find the polygon proximity switch that is nearest to the given forward-position:
			iterate (polygon_proximities, query_polygon_proximities_switch'access);

			
			case ms.status is
				when VALID => 
					-- If a valid milestone was found, then the milestone nearest to
					-- the given "forward" is to be found:
					-- Set the absolute x-position of the milestone.
					set_x;

					--log (text => "",
						--level => log_threshold);
					
					--log (text => "switch at" & to_string (ms.x_value),
						--level => log_threshold);

					log (text => "milestone is at" & to_string (ms.x), level => log_threshold + 1);

					
					update_final_line_status;
					
				when INVALID =>					
					-- If no valid milestone was found, then the milestone to be returned
					-- is INVALID.
					null;

					log (text => "none found", level => log_threshold + 1);

			end case;

			log_indentation_down;
			
			return ms;
		end get_next_milestone;
		
	begin -- compute_fill_lines
		
		log (text => "evaluating milestones after" & to_string (board_domain.start),
			 level => log_threshold);

		log_indentation_up;
		
		log (text => "make board switches", level => log_threshold);

		-- The calculation of the board switches requires to observe
		-- the DRU settings for clearance conductor to board edge. For this reason
		-- we pass extra clearance:
		switches.board := make_switches (
			points			=> board_domain,
			extra_clearance	=> true,
			clearance		=> design_rules.clearances.conductor_to_board_edge);

		log (text => "make polyon switches", level => log_threshold);
		
		switches.polygon := make_switches (
			points			=> polygon_domain);

		init_line_statuses;

		-- Set the initial x-position of the milestone.
		-- So the first milestone is where the board domain begins:
		milestone.x := X (board_domain.start);
		milestone.status := VALID; -- the first milestone is always valid
		
		update_final_line_status;

		-- Assign start to the fill_line if the milestone is valid
		-- and if the final_line_status is GO.
		-- Otherwise nothing happens here:
		make_fill_line;

		
		-- Get the 2nd milestone. If there is a 2nd milestone, then
		-- milestone.x_value assumes the x-position of the 2nd milestone.
		-- If there is no 2nd milestone, then the status of milestone will be set INVALID.
		-- In this case no further milestones will be searched for.
		milestone := get_next_milestone (milestone.x);

		-- Assign start or end point to the fill_line if the milestone is valid:
		-- Otherwise nothing happens here:
		make_fill_line;
		
		-- Look for more milestones:
		while milestone.status = VALID loop

			milestone := get_next_milestone (milestone.x);

			-- Assign start or end point to the fill_line if the milestone is valid:
			-- Otherwise nothing happens here:
			make_fill_line;
			
		end loop;

		log_indentation_down;
		
		return result;
	end compute_fill_lines;
		
	
	function get_distance_to_obstacle (
		module_cursor	: in pac_generic_modules.cursor;
		start_point		: in type_point;
		direction		: in type_rotation;
		net_name		: in pac_net_name.bounded_string := no_name;
		layer			: in type_signal_layer;
		width			: in type_track_width;
		clearance		: in type_track_observe_clearance := track_observe_clearance_default;
		log_threshold	: in type_log_level)
		return type_distance
	is
		distance : type_distance := zero;
	begin
		null;

		return distance;
	end get_distance_to_obstacle;
	
	function get_distance_to_obstacle_in_polygon (
		module_cursor	: in pac_generic_modules.cursor;
		polygon			: in type_polygon_conductor'class;
		start_point		: in type_point;
		net_name		: in pac_net_name.bounded_string := no_name;
		clearance		: in type_track_observe_clearance := track_observe_clearance_default;
		log_threshold	: in type_log_level)
		return type_distance_positive
	is
		use et_conductor_polygons;
		
		distance : type_distance_positive := zero;

		layer : type_signal_layer;
		width : type_track_width := polygon.width_min;

		procedure floating_solid is
			p : type_polygon_conductor_solid_floating := type_polygon_conductor_solid_floating (polygon);
		begin
			layer := p.properties.layer;
		end floating_solid;
		
		procedure floating_hatched is
			p : type_polygon_conductor_hatched_floating := type_polygon_conductor_hatched_floating (polygon);
		begin
			layer := p.properties.layer;
		end floating_hatched;
			
		procedure route_solid is
			p : type_polygon_conductor_route_solid := type_polygon_conductor_route_solid (polygon);
		begin
			layer := p.properties.layer;
		end route_solid;

		procedure route_hatched is
			p : type_polygon_conductor_route_hatched := type_polygon_conductor_route_hatched (polygon);
		begin
			layer := p.properties.layer;
		end route_hatched;

		
	begin -- get_distance_to_obstacle_in_polygon
		
		if polygon'tag = type_polygon_conductor_solid_floating'tag then
			floating_solid;

		elsif polygon'tag = type_polygon_conductor_hatched_floating'tag then
			floating_hatched;
			
		elsif polygon'tag = type_polygon_conductor_route_solid'tag then
			route_solid;

		elsif polygon'tag = type_polygon_conductor_route_hatched'tag then
			route_hatched;

		end if;

		distance := 3.0;
		--null;

		return distance;
	end get_distance_to_obstacle_in_polygon;

	
	function get_start_point_beyond_obstacle (
		module_cursor	: in pac_generic_modules.cursor;
		end_point		: in type_point;
		direction		: in type_rotation;
		--net_name		: in pac_net_name.bounded_string := no_name;
		layer			: in type_signal_layer;
		width			: in type_track_width;
		clearance		: in type_track_observe_clearance := track_observe_clearance_default;
		log_threshold	: in type_log_level)
		return type_point
	is
		point : type_point;
	begin
		null;

		return point;
	end get_start_point_beyond_obstacle;

	
end et_routing;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
