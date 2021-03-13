------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                                PCB                                       --
--                                                                          --
--                              B o d y                                     --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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

with ada.strings.unbounded;
with ada.exceptions;
with ada.tags;

with et_text;					use et_text;

package body et_pcb is

	use pac_shapes;
	


	-- NET CLASSES
	function to_string (net_class_name : in pac_net_class_name.bounded_string) return string is
	begin
		return pac_net_class_name.to_string (net_class_name);
	end to_string;

	function to_net_class_name (net_class_name : in string) return pac_net_class_name.bounded_string is
	begin
		return pac_net_class_name.to_bounded_string (net_class_name);
	end to_net_class_name;
	
	function to_string (class_description : in pac_net_class_description.bounded_string) return string is
	begin
		return pac_net_class_description.to_string (class_description);
	end to_string;

	function to_net_class_description (class_description : in string) return pac_net_class_description.bounded_string is
	begin
		return pac_net_class_description.to_bounded_string (class_description);
	end to_net_class_description;
	
	function text_properties (text : in et_packages.type_text) return string is
	-- Returns the properties of the given text in a long single string.
	begin
		return to_string (text.position) & latin_1.space
			& "size" 
			& to_string (text.size)
			& " line width" & to_string (text.line_width)
			& " rotation" & to_string (rot (text.position))
			& et_text.to_string (text.alignment);
	end text_properties;
	
	function to_string (meaning : in type_text_meaning_conductor) return string is begin
		return to_lower (type_text_meaning_conductor'image (meaning));
	end to_string;

	function to_meaning (meaning : in string) return type_text_meaning_conductor is begin
		return type_text_meaning_conductor'value (meaning);
	end to_meaning;


	function get_dimensions (
		contours		: in type_pcb_contours)
		return type_dimensions
	is
		use et_geometry;
		
		result : type_dimensions;

		procedure update_greatest_x (p : in type_point) is 
			d : type_distance := X (p);
		begin
			if d > X (result.greatest) then
				--put_line ("X" & to_string (d));
				set (axis => X, value => d, point => result.greatest);
			end if;
		end update_greatest_x;
			
		procedure update_greatest_y (p : in type_point) is 
			d : type_distance := Y (p);
		begin
			if d > Y (result.greatest) then
				set (axis => Y, value => d, point => result.greatest);
			end if;
		end update_greatest_y;

		procedure update_smallest_x (p : in type_point) is 
			d : type_distance := X (p);
		begin
			if d < X (result.smallest) then
				set (axis => X, value => d, point => result.smallest);
			end if;
		end update_smallest_x;
			
		procedure update_smallest_y (p : in type_point) is 
			d : type_distance := Y (p);
		begin
			if d < Y (result.smallest) then
				set (axis => Y, value => d, point => result.smallest);
			end if;
		end update_smallest_y;

		
		use pac_pcb_contour_lines;
		use pac_pcb_contour_arcs;
		--use pac_pcb_contour_circles;

		procedure query_line (c : in pac_pcb_contour_lines.cursor) is 
		begin
			update_greatest_x (element (c).start_point);
			update_greatest_y (element (c).start_point);
			update_smallest_x (element (c).start_point);
			update_smallest_y (element (c).start_point);

			update_greatest_x (element (c).end_point);
			update_greatest_y (element (c).end_point);
			update_smallest_x (element (c).end_point);
			update_smallest_y (element (c).end_point);
		end query_line;
		
	begin
		iterate (contours.lines, query_line'access);
		-- CS arcs, circles
		
		return result;
	end get_dimensions;
	
	function on_board (
		point			: in type_point;
		contours		: in type_pcb_contours;
		log_threshold 	: in type_log_level)
		return type_inside_polygon_query_result
	is 
		-- This function bases on the algorithm published at
		-- <http://www.alienryderflex.com/polygon//>
		-- The algorithm has further been extended to detect intersections
		-- with arcs and even circles.
		
		-- The approach to detect whether the given point lies inside or outside the
		-- board area is as follows:
		-- 1. Build a probe line (starting at point) that runs at zero degrees
		--    to the right. The probe line divides the area in two: an upper half and a
		--    lower half. Special situations arise if objects start or end exactly at
		--    the probe line.
		-- 2. The number of intersections then tells us:
		--    - odd -> point is inside board area
		--    - zero or even -> point is outside board area

		result : type_inside_polygon_query_result := (start => point, others => <>);

		line : constant type_probe_line := (
				start_point	=> point,
				end_point	=> type_point (set (X (point) + 1.0, Y (point))));
		
		probe_line : constant type_line_vector := to_line_vector (line);

		-- For segments that end or start exactly on the Y value of the probe line
		-- we define a threshold:
		y_threshold : constant type_distance := Y (point);

		-- This is the variable for the number of intersections detected.
		-- From this number we will later deduce the position of the given point,
		-- means whether it is inside or outside the board area:
		it : count_type := 0;
		
		-- This procedure iterates lines, arcs and circles of the given
		-- contours and counts the intersections of the probe line
		-- with each of them:
		procedure find_intersections is 
			use pac_probe_line_intersections;

			-- This procedure collects the intersection in the return value.
			-- It extracts the x-value and the angle of intersection.
			-- NOTE: The angle of intersection with the board contour can be 
			-- greater 90 degrees, which is
			-- an obtuse angle (German: "stumpfer Winkel"). 
			-- For computing the clearance between objects (tracks. fill lines, vias, ...)
			-- and the board contours
			-- we are interested in the acute portion (german: "spitzer Winkel") of the 
			-- intersection. So we must subtract 90 degrees in case it is greater 90 degrees.
			-- See: <https://www.splashlearn.com/math-vocabulary/geometry/acute-angle/> for
			-- terminology:
			procedure collect_intersection (i : in type_intersection) is 
				angle : type_rotation := subtract_90_if_greater_90 (i.angle);
			begin
				log (text => " intersects line at"
					& to_string (to_point (i.point)) 
					& " angle" & to_string (angle),
					level => log_threshold + 2);
				
				append (result.intersections, (
					x_position	=> X (to_point (i.point)),
					angle		=> angle));
				
			end collect_intersection;
	
			use pac_pcb_contour_lines;
			use pac_pcb_contour_arcs;
			use pac_pcb_contour_circles;
			
			procedure query_line (c : in pac_pcb_contour_lines.cursor) is
				-- Find out whether there is an intersection of the probe line
				-- and the candidate line of the contour.
				i : constant type_intersection_of_two_lines := 
					get_intersection (probe_line, element (c));

				function crosses_threshold return boolean is begin
					-- If the start/end point of the candidate line is ABOVE-OR-ON the 
					-- threshold AND if the end/start point of the candidate line is BELOW the
					-- threshold then we consider the contour line to be threshold-crossing.
					if	
						Y (element (c).start_point) >= y_threshold and 
						Y (element (c).end_point)   <  y_threshold then
						return true;
						
					elsif
						Y (element (c).end_point)   >= y_threshold and 
						Y (element (c).start_point) <  y_threshold then
						return true;
						
					else
						return false;
					end if;
				end crosses_threshold;
			
			begin -- query_line				
				log (text => "probing" & to_string (element (c)), level => log_threshold + 2);
				
				if i.status = EXISTS then

					-- If the candidate line segment crosses the y_threshold then 
					-- count the intersection:
					if crosses_threshold then
						
						--log (text => " intersects line"
							--& " at" & to_string (i.intersection),
							--level => log_threshold + 2);

						-- Add the intersection to the result:
						collect_intersection (i.intersection);
						
					end if;
				end if;
			end query_line;

			procedure query_arc (c : in pac_pcb_contour_arcs.cursor) is

				-- the candidate arc:
				arc : constant type_pcb_contour_arc := element (c);
				
				-- Find out whether there is an intersection of the probe line
				-- and the candidate arc of the contour.
				i : constant type_intersection_of_line_and_circle := 
					get_intersection (probe_line, arc);

				function crosses_threshold return boolean is begin
					-- If start/end point of the candidate arc is ABOVE-OR-ON the 
					-- threshold AND if the end/start point of the candidate arc is BELOW the
					-- threshold then we consider the contour arc to be threshold-crossing.
					if	
						Y (arc.start_point) >= y_threshold and 
						Y (arc.end_point)   <  y_threshold then
						return true;
						
					elsif
						Y (arc.end_point)   >= y_threshold and 
						Y (arc.start_point) <  y_threshold then
						return true;
						
					else
						return false;
					end if;

				end crosses_threshold;

				procedure count_one is begin
					log (text => " intersects arc" --& to_string (arc)
							& " at" & to_string (i.intersection),
						level => log_threshold + 2);

					-- Add the intersection to the result:
					collect_intersection (i.intersection);
				end count_one;
				
				procedure count_two is begin
					log (text => " intersects arc" --& to_string (arc)
							& " at" & to_string (i.intersection_1)
							& " and" & to_string (i.intersection_2),
						level => log_threshold + 2);

					-- Add the intersections to the result:
					collect_intersection (i.intersection_1);
					collect_intersection (i.intersection_2);
				end count_two;
				
			begin -- query_arc		
				log (text => "probing " & to_string (arc), level => log_threshold + 2);
				
				case i.status is
					when NONE_EXIST => null;
					
					when ONE_EXISTS =>
						case i.tangent_status is
							when TANGENT => null; -- not counted
							
							when SECANT =>
								if crosses_threshold then
									-- The line intersects the arc at one point.
									-- Start and end point of the arc are opposide 
									-- of each other with the probe line betweeen them:
									count_one;
								end if;
						end case;

					when TWO_EXIST =>
						if Y (arc.start_point) /= y_threshold then
							-- Since we have TWO intersections, the end point of the arc
							-- must be in the same half as the start point of the arc:
							count_two;
						else
							-- Special case: Start or end point of arc lies exactly
							-- at the probe line.
							
							-- If start and end point of the candidate arc is ABOVE-OR-ON the 
							-- threshold then we consider the arc to be threshold-crossing.
							-- The remaining question is: How often does the arc intersect
							-- the probe line ?

							-- If start point at probe line:
							if Y (arc.start_point) = y_threshold then

								-- If the arc starts at the probe line and ends below
								-- the probe line, then it runs first upwards through the upper half
								-- and ends somewhere there:
								if Y (arc.end_point) > y_threshold then
									count_one;
									
								-- If the arc starts at the probe line and ends BELOW
								-- the probe line, then it runs first upwards through the upper half
								-- and ends somewhere in the lower half:
								elsif Y (arc.end_point) < y_threshold then
									count_two;
								end if;

								
							-- If end point at probe line:
							elsif Y (arc.end_point) = y_threshold then
								
								-- If the arc ends at the probe line and starts somewhere in the
								-- upper half, then it eventually comes down to the end point:
								if Y (arc.start_point) > y_threshold then
									count_one;

								-- If the arc ends at the probe line and starts below
								-- the probe line, then it first runs upwards into
								-- the upper half and eventually comes down to the end point:
								elsif Y (arc.start_point) < y_threshold then
									count_two;
								end if;
								
							end if;
						end if;
					
				end case;
			end query_arc;

			procedure query_circle (c : in pac_pcb_contour_circles.cursor) is
				-- Find out whether there is an intersection of the probe line
				-- and the candidate circle of the contour.
				i : constant type_intersection_of_line_and_circle := 
					get_intersection (probe_line, element (c));

			begin				
				log (text => "probing" & to_string (element (c)), level => log_threshold + 2);
				
				case i.status is
					when NONE_EXIST | ONE_EXISTS => null;
						-- NOTE: If the probe line is a tangent to the
						-- circle, then we threat this NOT as intersection.
					
					when TWO_EXIST =>
						-- The probe line intersects the circle at two points:
					
						log (text => " intersects circle" -- & to_string (element (c))
							 & " at" & to_string (i.intersection_1)
							 & " and" & to_string (i.intersection_2),
							level => log_threshold + 2);

						-- Add the intersections to the result:
						collect_intersection (i.intersection_1);
						collect_intersection (i.intersection_2);
				end case;
			end query_circle;
			
		begin -- find_intersections			
			log (text => "lines ...", level => log_threshold + 2);
			log_indentation_up;
			iterate (contours.lines, query_line'access);
			log_indentation_down;

			log (text => "arcs ...", level => log_threshold + 2);
			log_indentation_up;
			iterate (contours.arcs, query_arc'access);
			log_indentation_down;

			log (text => "circles ...", level => log_threshold + 2);
			log_indentation_up;
			iterate (contours.circles, query_circle'access);
			log_indentation_down;

		end find_intersections;

		-- This procedure logs the x-intersections if the current
		-- log level exceedes the given log level.
		procedure collect_intersection is 
			use ada.strings.unbounded;
			use pac_probe_line_intersections;

			x_intersections : unbounded_string := to_unbounded_string ("x/angle:");
			
			procedure query_intersection (
				c : pac_probe_line_intersections.cursor) 
			is begin
				x_intersections := x_intersections & to_string (element (c).x_position)
					& "/" & trim (to_string (element (c).angle), left);
			end query_intersection;
						
		begin
			if log_level > log_threshold + 1 then
				iterate (result.intersections, query_intersection'access);

				log (text => to_string (x_intersections));
			end if;
			
		end collect_intersection;

		procedure sort_intersections is
			package pac_sort_intersections is new pac_probe_line_intersections.generic_sorting;
			use pac_sort_intersections;
		begin
			sort (result.intersections);
		end sort_intersections;
		
	begin -- on_board		
		log (text => "determining position of point" & to_string (point)
			 & " relative to board outline ...", level => log_threshold);

		log_indentation_up;

		log (text => "using a probe line: start" 
			 & to_string (to_point (probe_line.v_start))
			 & " direction 0 degrees ...", 
			 level => log_threshold + 1);
			
		log (text => "counting intersections of probe line with board outline ...",
			 level => log_threshold + 1);

		log_indentation_up;
		
		-- Find the intersections of the probe line with the
		-- segments of the board countour:
		find_intersections;

		-- The intersections are not sorted yet. We need them sorted with the
		-- smallest x first:
		sort_intersections;
		
		log_indentation_down;

		-- get the total number of intersections
		it := pac_probe_line_intersections.length (result.intersections);
		
		log (text => "intersections total:" & count_type'image (it), level => log_threshold + 1);

		-- Log intersections where the probe line intersects the board contours
		-- to the right of the given point:
		if it > 0 then
			collect_intersection;
		end if;
		
		log_indentation_down;

		-- If the total number of intersections is an odd number, then the given point
		-- is on the board.
		-- If the total is even, then the point is outside the board area.
		if (it rem 2) = 1 then
			log (text => "point IS on board", level => log_threshold);
			result.status := INSIDE; -- point is in usable board area 
		else 
			log (text => "point is NOT on board", level => log_threshold);
			result.status := OUTSIDE; -- point is outside board area
		end if;
		
		return result;
	end on_board;

	
	function on_segment (
		point			: in type_point; -- x/y
		layer			: in type_signal_layer;
		line			: in pac_conductor_lines.cursor;
		accuracy		: in type_catch_zone)
		return boolean 
	is
		result : boolean := false; -- to be returned
		use pac_conductor_lines;
	begin
		if element (line).layer = layer then
			if on_line (point, element (line), accuracy) then
				result := true;
			else
				result := false;
			end if;
		else
			result := false;
		end if;
		
		return result;
	end on_segment;

	function on_segment (
		point			: in type_point; -- x/y
		layer			: in type_signal_layer;
		arc				: in pac_conductor_arcs.cursor;
		accuracy		: in type_distance)
		return boolean 
	is
		result : boolean := false; -- to be returned
		use pac_conductor_arcs;
	begin
		if element (arc).layer = layer then
			result := true; -- CS
		else
			result := false;
		end if;

		return result;
	end on_segment;


	function to_string (locked : in type_locked) return string is begin
		return to_lower (type_locked'image (locked));
	end;

	function to_lock_status (locked : in string) return type_locked is begin
		return type_locked'value (locked);
	end;

	

	function package_position (position : in type_package_position) return string is
	-- Returns the coordinates of a package (in a board) as string.
	begin
		return (" position" & to_string (type_point (position))
			& " angle" & to_string (rot (position))
			& " face" & to_string (get_face (position)));
	end package_position;

	
	function to_string (flipped : in type_flipped) return string is begin
		return to_lower (type_flipped'image (flipped));
	end;

	function to_flipped (flipped : in string) return type_flipped is begin
		return type_flipped'value (flipped);
	end;

	

	procedure text_conductor_properties (
		cursor			: in pac_conductor_texts.cursor;
		log_threshold 	: in et_string_processing.type_log_level) 
	is
		use pac_conductor_texts;
		text : type_conductor_text;
	begin
		text := element (cursor);
		log (text => "conductor text signal layer" & to_string (text.layer) & latin_1.space
			& "content '" & et_text.to_string (text.content) & "'", level => log_threshold
			);

		log_indentation_up;
		log (text => text_properties (type_text (text)), level => log_threshold + 1);
		log_indentation_down;
	end text_conductor_properties;

	


-- PROPERTIES OF ELECTRIC OBJECTS IN SIGNAL LAYERS
	procedure route_line_properties (
		cursor			: in pac_conductor_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level)
	is
		use pac_conductor_lines;
		line : type_conductor_line;
	begin
		line := element (cursor);
		log (text => "segment " & to_string (type_line (line)) &
			 " width" & to_string (line.width) &
			 " layer" & to_string (line.layer)
			 -- CS locked
			 , level => log_threshold);
	end route_line_properties;
	
	procedure route_via_properties (
		cursor			: in pac_vias.cursor;
		log_threshold 	: in et_string_processing.type_log_level) 
	is
		use pac_vias;
		
		procedure do_it (via : type_via) 
		is begin
			case via.category is
				when THROUGH =>
					log (text => "via" 
						& " category " & to_string (via.category) & space
						& to_string (type_drill (via)) 
						& " restring outer" & to_string (via.restring_outer) -- outer layers
						& " restring inner" & to_string (via.restring_inner), -- inner layers
						level => log_threshold);

				when others => null;
				-- CS log properties of other via categories
				
			end case;
						--& " layer_start" & to_string (via.layers.l_start) &
			 --" layer_end" & to_string (via.layers.l_end)
			 ---- CS locked
		end do_it;
	
	begin
		do_it (element (cursor));
	
	end route_via_properties;

		

-- PROPERTIES OF OBJECTS IN BOARD CONTOUR / OUTLINE / EDGE CUTS
	procedure line_pcb_contour_properties (
	-- Logs the properties of the given line of pcb contour
		cursor			: in pac_pcb_contour_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use pac_pcb_contour_lines;
		line : type_pcb_contour_line;
	begin
		line := element (cursor);
		log (text => "PCB contour (edge cuts / outline) line" & latin_1.space
			 & to_string (type_line (line)), level => log_threshold);
			-- CS lock status
	end line_pcb_contour_properties;

	procedure arc_pcb_contour_properties (
	-- Logs the properties of the given arc of pcb contour
		cursor			: in pac_pcb_contour_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use pac_pcb_contour_arcs;
		arc : type_pcb_contour_arc;
	begin
		arc := element (cursor);
		log (text => "PCB contour (edge cuts / outline) arc" & latin_1.space 
			 & to_string (type_arc (arc)), level => log_threshold);
			-- CS lock status
	end arc_pcb_contour_properties;

	procedure circle_pcb_contour_properties (
	-- Logs the properties of the given circle of pcb contour
		cursor			: in pac_pcb_contour_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use pac_pcb_contour_circles;
		circle : type_pcb_contour_circle;
	begin
		circle := element (cursor);
		log (text => "PCB contour (edge cuts / outline) circle" & latin_1.space 
			 & to_string (type_circle (circle)), level => log_threshold);
			-- CS lock status
	end circle_pcb_contour_properties;


	
end et_pcb;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
