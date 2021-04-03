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
	
	function compute_clearance_track_to_board_edge (
		status			: in type_point_status; -- transition to board inside/outside area
		y_position		: in type_distance; -- the y-position of the fill line
		intersection	: in type_probe_line_intersection; -- provides curvature, x-value, angle, ...
		line_width		: in type_track_width; -- the width of the fill line
		clearance_dru	: in type_track_clearance;  -- the clearance as given by DRU
		log_threshold	: in type_log_level)
		return type_track_clearance
	is
		result : type_track_clearance;

		-- Since the cap of the fill line is round, the minimal distance to observe
		-- from the center of the cap to the board edge is:
		clearance_min : constant float := float (clearance_dru + line_width * 0.5);

		type type_line is new et_board_shapes_and_text.pac_shapes.type_line with null record;
			
	
		function compute_straight return type_track_clearance is
			-- If the probe line intersects with a straight segment of the board
			-- edge then we are dealing with a rectangular triangle.
			
			-- The total of inner angles of a rectangular triangle is 180 degrees.
			-- Two angles are known. Hence:

			-- The distance from center of line cap to board edge along the probe line:
			side_c : float; -- to be returned

			-- The distance from track end point to board edge.
			-- A line perpendicular to the board edge:
			side_a : constant float := clearance_min;

			-- The angle between side_a and side_c:
			angle_b : constant float := float (90.0 - intersection.angle);
		begin
			--put_line (" clearance" & to_string (clearance));
			--put_line (" width" & to_string (width));
			--put_line (" side_a" & float'image (side_a) & " angle_b" & float'image (angle_b));

			side_c := side_a / cos (angle_b, float (units_per_cycle)); 
			--put_line (" side_c " & float'image (side_c));

			return type_track_clearance (side_c);
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

			result : type_track_clearance;
			
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
		
	begin -- compute_clearance_track_to_board_edge
		log (text => "computing clearance fill line to board edge ...",
			 level => log_threshold);
		
		log_indentation_up;
		
		--put_line ("angle" & type_rotation'image (angle));

		log (text => "probe line y" & to_string (y_position)
				& " width" & to_string (line_width)
				& " intersection x" & to_string (intersection.x_position)
				& " angle" & to_string (intersection.angle) 
				& " curvature " & to_string (intersection.curvature)
				& " status " & to_string (status),
			level => log_threshold + 1);

		
		if intersection.angle = 90.0 then

			-- The probe line approaches the board edge perpendicular:
			--log (text => "line approaches edge perpendicular. nothing to do.",
				 --level => log_threshold + 1);
			
			--log_indentation_down;
			
			--return type_track_clearance (clearance_min);
			result := type_track_clearance (clearance_min);
			
		else
			--log (text => "probe line y" & to_string (y_position)
				 --& " intersection x" & to_string (intersection.x_position)
				 --& " angle" & to_string (intersection.angle) 
				 --& " curvature " & to_string (intersection.curvature)
				 --& " status " & to_string (status),
				--level => log_threshold + 1);
			
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

		
		log (text => " clearance in x direction" & to_string (result),
			 level => log_threshold + 1);
		
		log_indentation_down;
		
		return result; 
		
	end compute_clearance_track_to_board_edge;

	
	function compute_fill_lines (
		module_cursor	: in pac_generic_modules.cursor;
		design_rules	: in type_design_rules;
		board_domain	: in type_inside_polygon_query_result;
		polygon_domain	: in type_inside_polygon_query_result;
		width			: in type_track_width; -- the width of a fill line
		clearance		: in type_track_clearance;
		isolation 		: in type_track_clearance; 
		easing			: in type_easing;
		log_threshold	: in type_log_level
		)
		return pac_fill_lines.list
	is
		-- EXAMPLE 1:
		--
		-- The points where the probe line intersects the
		-- board contours are provided in argument board_domain
		-- and shall be imagined this way:
		--
		-- P---G----------S---G-S-----> x-direction
		--
		-- Likewise the points of the polygon_domain:
		--
		-- P------G--------------S----> x-direction
		--
		-- computed fill lines (marked with "=") are where ALL domains
		-- give a go for a fill line:
		--
		-- -------G=======S---G=S-----> x-direction
		--
		-- NOTE: All domains have equal start points P.


		-- EXAMPLE 2:
		--
		-- The points where the probe line intersects the
		-- board contours are provided in argument board_domain
		-- and shall be imagined this way:
		--
		-- P---------G----S-----------> x-direction
		--
		-- Likewise the points of the polygon_domain:
		--
		-- P------G--------------S----> x-direction
		--
		-- computed fill lines (marked with "=") are where ALL domains
		-- give a go for a fill line:
		--
		-- ----------G====S-----------> x-direction
		--
		-- NOTE: All domains have equal start points P.

		-- The points marked with S or G are milestones where a domain
		-- gives a GO or STOP for a fill line.
		-- The milestones for the final fill line are a
		-- result of ANDing the GO status of the individual domains.
		
		type type_stop_go is (
			STOP,	-- the domain stops the line
			GO);	-- the domain gives a go for the line

		-- The status of an individual domain:
		board_line_status, polygon_line_status : type_stop_go := STOP;

		-- After ANDing the status of the domains this the the final
		-- signal for start and stop of a fill line:
		final_line_status : type_stop_go := STOP;

		-- This flag indicates that the status for the final
		-- fill line has changed from GO to STOP or from STOP to GO:
		final_line_status_has_changed : boolean := false;

		
		
		-- A single fill line to be inserted in the list of fill lines.
		-- which will be returned by this function:
		fill_line : type_fill_line; -- G====S

		-- The list of fill lines to be returned.
		-- NOTE: All the lines contained here are in the same row,
		-- means they all have the same y-value for start and end point:
		result : pac_fill_lines.list;

		
		-- This is the y position of all fill lines. All fill lines
		-- are in the same row:
		y_position : type_distance := Y (board_domain.start); 

		
		procedure init_domain_statuses is begin
			case board_domain.status is
				when INSIDE		=> board_line_status := GO;
				when OUTSIDE	=> board_line_status := STOP;
			end case;

			case polygon_domain.status is
				when INSIDE		=> polygon_line_status := GO;
				when OUTSIDE	=> polygon_line_status := STOP;
			end case;
		end init_domain_statuses;
		
		procedure toggle_status_board_line is begin
			case board_line_status is
				when STOP	=> board_line_status := GO;
				when GO		=> board_line_status := STOP;
			end case;
		end toggle_status_board_line;

		procedure toggle_status_polygon_line is begin
			case polygon_line_status is
				when STOP	=> polygon_line_status := GO;
				when GO		=> polygon_line_status := STOP;
			end case;
		end toggle_status_polygon_line;
		
		type type_milestone_status is (
			VALID,		-- there is a milestone after a certain point in x-direction
			INVALID);	-- there is no milestone after a certain point

		type type_milestone is record
			status		: type_milestone_status := INVALID;

			-- The absolute x-position of the milestone 
			-- after a certain point in x-direction:
			x_value		: type_distance := zero;
		end record;

		milestone : type_milestone;


		-- Computes the final_line_status by ANDing the individual
		-- domain statuses. 
		-- ALL domains must give their go for the final fill line.
		-- Sets the flag final_line_status_has_changed accordingly.
		procedure update_final_line_status is begin
			if board_line_status = GO and polygon_line_status = GO then

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
						fill_line.start_point := type_point (set (milestone.x_value, y_position));
						
					when STOP =>
						--if milestone.x_value > x_position then

						-- Its end point is assigned when the final_line_status has just CHANGED
						-- into a STOP mark.
						
							if final_line_status_has_changed then
								fill_line.end_point := type_point (set (milestone.x_value, y_position));
								
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
			use pac_probe_line_intersections;

			-- The basic tasks of this function are:
			-- 1. Find in each domain the point of intersection that comes right
			--    after "forward". The difference of point.x and "forward"
			--    is used to find the point. The smallest difference indicates
			--    the point nearest to "forward".
			-- 2. Store the smallest difference of point.x and "forward" in array sdx.
			--    Each domain has an element in sdx.
			-- 3. Find the smallest difference among the differences stored in sdx.
			--    intersections of multiple domains may be at the same x-position.
			--    For each intersection of a domain the status (INSIDE/OUTSIDE)
			--    is toggled to and fro.
			
			-- the milestone being build and to be returned:
			ms : type_milestone;

			-- These are the smallest distances found between "forward"
			-- and the points in the domains:
			type type_smallest_differences is array (1..2) of type_distance;
			sdx : type_smallest_differences := (others => type_distance'last);

			-- While searching among the intersections with the board contours
			-- this flag tells whether we are inside or outside the board area.
			-- Initially that status is taken from the given board domain.
			board_point_status : type_point_status := board_domain.status;
			
			procedure query_board_point (c : in pac_probe_line_intersections.cursor) is
				dx : type_distance;

				spacing : type_track_clearance;
				
				-- By adding or subtracting spacing we get a fill line that
				-- starts slightly after entering the board area and ends slightly
				-- before leaving the board area.
			begin
				--log (text => "board point " & to_string (element (c).x_position));
				
				-- Each intersection with the board contour causes a change
				-- of the flag board_point_status:
				toggle_status (board_point_status);

				-- In addition to the intersection with the board contour,
				-- the clearance between conductor and board edge must be
				-- respected. Since the line ends are round caps, the line
				-- width must also be taken into account.

				spacing := compute_clearance_track_to_board_edge (
					status			=> board_point_status, -- transition to board inside/outside area
					y_position		=> y_position,
					intersection	=> element (c),
					line_width		=> width,
					clearance_dru	=> design_rules.clearances.conductor_to_board_edge,
					log_threshold	=> log_threshold + 1);

				--put_line ("spacing " & to_string (spacing));
				
				case board_point_status is
					when INSIDE => -- A change from outside to inside occured.
						-- Board area entered.
						-- Create a new virtual intersection after the original
						-- intersection. The original intersection is omitted.
						dx := (element (c).x_position + spacing) - forward;

					when OUTSIDE => -- A change from inside to outside occured.
						-- Board area left.
						-- Create a new virtual intersection before the original
						-- intersection. The original intersection is omitted.
						dx := (element (c).x_position - spacing) - forward;
						
				end case;
				
				-- The point must be to the right of "forward":
				if dx > zero then
					ms.status := VALID;

					if dx < sdx(1) then
						sdx(1) := dx;
					end if;
				end if;				
			end query_board_point;


			-- While searching among the intersections with the polygon
			-- this flag tells whether we are inside or outside the polygon.
			-- Initially that status is taken from the given polygon domain.
			polygon_point_status : type_point_status := polygon_domain.status;
			
			procedure query_polygon_point (c : in pac_probe_line_intersections.cursor) is
				dx : type_distance;
				
				-- Since the line ends are round caps, the line
				-- width must be taken into account.
				spacing : constant type_distance_positive :=
					width * 0.5;

				-- By adding or subtracting spacing we get a fill line that
				-- starts slightly after entering the polygon and ends slightly
				-- before the leaving the polygon.
			begin
				-- Each intersection with the polygon causes a change
				-- of the flag polygon_point_status:
				toggle_status (polygon_point_status);

				case polygon_point_status is
					when INSIDE => -- A change from outside to inside occured.
						-- Create a new virtual intersection after the original
						-- intersection. The original intersection is omitted.
						dx := (element (c).x_position + spacing) - forward;

					when OUTSIDE => -- A change from inside to outside occured.
						-- Create a new virtual intersection before the original
						-- intersection. The original intersection is omitted.
						dx := (element (c).x_position - spacing) - forward;
						
				end case;
				
				-- The point must be to the right of "forward":
				if dx > zero then
					ms.status := VALID;

					if dx < sdx(2) then
						sdx(2) := dx;
					end if;					
				end if;				
			end query_polygon_point;

			-- Finds the smalles difference among the domains and sets the
			-- absolute position of the milestone to be returned.
			-- Toggles the status of the individual lines.
			procedure set_x is
				dx : type_distance := type_distance'last;
			begin
				-- Find the nearest milestone among the domains
				-- by finding the smallest difference stored in array sdx:
				for i in type_smallest_differences'first .. type_smallest_differences'last loop

					if sdx(i) < dx then
						dx := sdx(i);
					end if;

				end loop;

				--log (text => "dx" & to_string (dx) & " after" & to_string (forward),
					--level => log_threshold);
				
				-- Find the domains that have the current x_value.
				-- There can be more than one domain having the current x_value:
				for i in type_smallest_differences'first .. type_smallest_differences'last loop

					if sdx(i) = dx then
						case i is
							when 1 =>
								toggle_status_board_line;
							
							when 2 =>
								toggle_status_polygon_line;

							-- CS toggle others
						end case;
					end if;

				end loop;				

				-- The absolute position in x of the switch point is:
				ms.x_value := forward + dx;
			end set_x;
			
		begin -- get_next_milestone

			-- Find the x-value that is nearest to the given forward-value:
			iterate (board_domain.intersections, query_board_point'access);

			--log (text => "next board point after" & to_string (sdx(1)),
				--level => log_threshold);

			-- Find the x-value that is nearest to the given forward-value:			
			iterate (polygon_domain.intersections, query_polygon_point'access);

			--log (text => "next polygon point after" & to_string (sdx(2)),
				--level => log_threshold);

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

					update_final_line_status;
					
				when INVALID =>					
					-- If no valid milestone was found, then the milestone to be returned
					-- in INVALID.
					null;
			end case;
			
			return ms;
		end get_next_milestone;
		
	begin -- compute_fill_lines
		log (text => "evaluating milestones after" & to_string (board_domain.start),
			 level => log_threshold);

		init_domain_statuses;
	
		-- Set the initial x-position of the milestone.
		-- So the first milestone is where the board domain begins:
		milestone.x_value := X (board_domain.start);
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
		milestone := get_next_milestone (milestone.x_value);

		-- Assign start or end point to the fill_line if the milestone is valid:
		-- Otherwise nothing happens here:
		make_fill_line;
		
		-- Look for more milestones:
		while milestone.status = VALID loop

			milestone := get_next_milestone (milestone.x_value);

			-- Assign start or end point to the fill_line if the milestone is valid:
			-- Otherwise nothing happens here:
			make_fill_line;
			
		end loop;
		
		return result;
	end compute_fill_lines;


	function compute_fill_lines_2 (
		module_cursor	: in pac_generic_modules.cursor;
		design_rules	: in type_design_rules;
		board_domain	: in type_inside_polygon_query_result;
		polygon_domain	: in type_inside_polygon_query_result;
		width			: in type_track_width; -- the width of a fill line
		clearance		: in type_track_clearance;
		isolation 		: in type_track_clearance; 
		easing			: in type_easing;
		log_threshold	: in type_log_level
		)
		return pac_fill_lines.list
	is

		-- We will return a bunch of fill lines which are in a single row:
		result : pac_fill_lines.list;

		-- This is the y position of all fill lines. All fill lines
		-- are in the same row:
		y_position : type_distance := Y (board_domain.start); 

		-- A single fill line to be inserted in the list of fill lines.
		-- which will be returned by this function:
		fill_line : type_fill_line; -- G====S

		
		type type_stop_go is (
			STOP,	-- the domain stops the line
			GO);	-- the domain gives a go for the line

		-- The status of an individual domain:
		board_line_status, polygon_line_status : type_stop_go := STOP;

		-- After ANDing the status of the domains this the the final
		-- signal for start and stop of a fill line:
		final_line_status : type_stop_go := STOP;

		procedure init_line_statuses is begin
			case board_domain.status is
				when INSIDE		=> board_line_status := GO;
				when OUTSIDE	=> board_line_status := STOP;
			end case;

			case polygon_domain.status is
				when INSIDE		=> polygon_line_status := GO;
				when OUTSIDE	=> polygon_line_status := STOP;
			end case;

			--log (text => "status board line: " & type_stop_go'image (board_line_status),
				--level => log_threshold + 1);

			--log (text => "status polygon line: " & type_stop_go'image (polygon_line_status),
				--level => log_threshold + 1);
			
		end init_line_statuses;

		
		-- This flag indicates that the status for the final
		-- fill line has changed from GO to STOP or from STOP to GO:
		final_line_status_has_changed : boolean := false;



		procedure toggle_status_board_line is begin
			case board_line_status is
				when STOP	=> board_line_status := GO;
				when GO		=> board_line_status := STOP;
			end case;
		end toggle_status_board_line;

		procedure toggle_status_polygon_line is begin
			case polygon_line_status is
				when STOP	=> polygon_line_status := GO;
				when GO		=> polygon_line_status := STOP;
			end case;
		end toggle_status_polygon_line;
		
		

		
		type type_switch is record
			status		: type_stop_go := STOP; -- CS no need
			position	: type_distance := zero;
		end record;

		package pac_switches is new doubly_linked_lists (type_switch);
		use pac_switches;
		-- CS use pac_distances instead
		
		board_switches, polygon_switches : pac_switches.list;


		use pac_probe_line_intersections;

		function make_switches (
			points			: in type_inside_polygon_query_result;
			extra_clearance	: in boolean := false;					   
			clearance		: in type_track_clearance := type_track_clearance'first)					   
			return pac_switches.list
		is 
			switches : pac_switches.list;

			status_inside_outside : type_point_status := points.status;

			procedure query_intersection (c : in pac_probe_line_intersections.cursor) is
				new_x : type_distance;

				spacing : type_track_clearance;

			begin
				toggle_status (status_inside_outside);

				if extra_clearance then
					spacing := compute_clearance_track_to_board_edge (
						status			=> status_inside_outside, -- transition to board inside/outside area
						y_position		=> y_position,
						intersection	=> element (c),
						line_width		=> width,
						clearance_dru	=> clearance,
						log_threshold	=> log_threshold + 1);

				else
					null;
					-- Since the line ends are round caps, the line
					-- width must be taken into account.
					spacing := width * 0.5; -- CS
					
					--spacing := compute_clearance_track_to_board_edge (
						--status			=> status_inside_outside, -- transition to board inside/outside area
						--y_position		=> y_position,
						--intersection	=> element (c),
						--line_width		=> width,
						--clearance_dru	=> clearance,
						--log_threshold	=> log_threshold + 1);

				end if;

				
				case status_inside_outside is
					when INSIDE => -- A change from outside to inside occured.
						-- Board area entered.
						-- Create a new virtual intersection after the original
						-- intersection. The original intersection is omitted.
						new_x := element (c).x_position + spacing;

						append (switches, (GO, new_x));
						
					when OUTSIDE => -- A change from inside to outside occured.
						-- Board area left.
						-- Create a new virtual intersection before the original
						-- intersection. The original intersection is omitted.
						new_x := element (c).x_position - spacing;

						append (switches, (STOP, new_x));
				end case;
				
				log (text => "switch at" & to_string (new_x), level => log_threshold + 1);
				
			end query_intersection;
				
		begin	
			log_indentation_up;
			
			iterate (points.intersections, query_intersection'access);

			log_indentation_down;
			
			return switches;
		end make_switches;



		type type_milestone_status is (
			VALID,		-- there is a milestone after a certain point in x-direction
			INVALID);	-- there is no milestone after a certain point

		
		type type_milestone is record
			status		: type_milestone_status := INVALID;

			-- The absolute x-position of the milestone 
			-- after a certain point in x-direction:
			x_value		: type_distance := zero;
		end record;

		milestone : type_milestone;


		-- Computes the final_line_status by ANDing the individual
		-- domain statuses. 
		-- ALL domains must give their go for the final fill line.
		-- Sets the flag final_line_status_has_changed accordingly.
		procedure update_final_line_status is begin
			if board_line_status = GO and polygon_line_status = GO then

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
						fill_line.start_point := type_point (set (milestone.x_value, y_position));
						
					when STOP =>
						--if milestone.x_value > x_position then

						-- Its end point is assigned when the final_line_status has just CHANGED
						-- into a STOP mark.
						
							if final_line_status_has_changed then
								fill_line.end_point := type_point (set (milestone.x_value, y_position));
								
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
			type type_smallest_differences is array (1..2) of type_distance;
			sdx : type_smallest_differences := (others => type_distance'last);

			procedure query_board_switch (c : in pac_switches.cursor) is
				x : type_distance := element (c).position;
			begin
				if x > forward then
					ms.status := VALID;

					if x < sdx(1) then
						sdx(1) := x;
					end if;
				end if;
			end query_board_switch;

			procedure query_polygon_switch (c : in pac_switches.cursor) is
				x : type_distance := element (c).position;
			begin
				if x > forward then
					ms.status := VALID;

					if x < sdx(2) then
						sdx(2) := x;
					end if;
				end if;
			end query_polygon_switch;

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
				-- There can be more than one domain having the current x_value:
				for i in type_smallest_differences'first .. type_smallest_differences'last loop

					if sdx(i) = dx then
						case i is
							when 1 =>
								toggle_status_board_line;
							
							when 2 =>
								toggle_status_polygon_line;

							-- CS toggle others
						end case;
					end if;

				end loop;				

				-- The absolute position in x of the switch point is:
				ms.x_value := dx;
			end set_x;
			
		begin -- get_next_milestone
			
			log (text => "seaching milestone forward" & to_string (forward),
				level => log_threshold + 1);

			log_indentation_up;

			-- Find the board switch that is nearest to the given forward-position:
			iterate (board_switches, query_board_switch'access);

			-- Find the polygon switch that is nearest to the given forward-position:
			iterate (polygon_switches, query_polygon_switch'access);

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

					log (text => "milestone is at" & to_string (ms.x_value),
						level => log_threshold + 1);

					
					update_final_line_status;
					
				when INVALID =>					
					-- If no valid milestone was found, then the milestone to be returned
					-- is INVALID.
					null;

					log (text => "none found",
						level => log_threshold + 1);

			end case;

			log_indentation_down;
			
			return ms;
		end get_next_milestone;
		
	begin -- compute_fill_lines
		
		log (text => "evaluating milestones after" & to_string (board_domain.start),
			 level => log_threshold);

		log_indentation_up;
		
		--log (text => "make board switches", level => log_threshold);

		board_switches := make_switches (
			points			=> board_domain,
			extra_clearance	=> true,
			clearance		=> design_rules.clearances.conductor_to_board_edge);

		--log (text => "make polyon switches", level => log_threshold);
		
		polygon_switches := make_switches (
			points			=> polygon_domain);

		init_line_statuses;

		-- Set the initial x-position of the milestone.
		-- So the first milestone is where the board domain begins:
		milestone.x_value := X (board_domain.start);
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
		milestone := get_next_milestone (milestone.x_value);

		-- Assign start or end point to the fill_line if the milestone is valid:
		-- Otherwise nothing happens here:
		make_fill_line;
		
		-- Look for more milestones:
		while milestone.status = VALID loop

			milestone := get_next_milestone (milestone.x_value);

			-- Assign start or end point to the fill_line if the milestone is valid:
			-- Otherwise nothing happens here:
			make_fill_line;
			
		end loop;

		log_indentation_down;
		
		return result;
	end compute_fill_lines_2;
		
	
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
