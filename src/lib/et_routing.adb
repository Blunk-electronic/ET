------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               ROUTING                                    --
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


package body et_routing is
	
	use pac_generic_modules;
	

	function get_clearance (
		module	: in pac_generic_modules.cursor;
		device	: in et_schematic.pac_devices_sch.cursor;
		terminal: in pac_terminals.cursor)
		return type_get_terminal_clearance_result
	is 
		use pac_net_name;

		-- Get a cursor to the net connected with the terminal.
		-- If there is no net connected then we return a not-connected-status:
		net : constant et_schematic.pac_nets.cursor := 
			et_schematic_ops.get_net (module, device, pac_terminals.key (terminal));
		
		net_class	: type_net_class;
		status		: type_get_terminal_clearance_result;

		use et_schematic.pac_nets;
	begin

		if net = et_schematic.pac_nets.no_element then
			status := (connected => false);
		else
			--log (text => "net " & enclose_in_quotes (to_string (et_schematic.pac_nets.key (net))));

			-- Get the net class of the net:
			net_class := get_net_class (module, net);
			status := (connected => true, clearance => net_class.clearance);
		end if;

		return status;
	end get_clearance;

			
			


	function get_distance_to_edge (
		module_cursor	: in pac_generic_modules.cursor;
		point			: in type_point;
		log_category	: in type_log_category;
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
			if log_category >= HIGH then
				log (text => "probing outline ...", level => lth + 1);
			end if;
			
			result := get_shortest_distance (module.board.contours.outline, point);

			if log_category >= HIGH then
				log (text => " distance to outline" & to_string (get_absolute (result)),
					 level => lth + 1);
			end if;
			
			-- test holes in board (if there are any):
			if not is_empty (module.board.contours.holes) then

				if log_category >= HIGH then
					log (text => "probing holes...", level => lth + 1);			
				end if;
				
				iterate (module.board.contours.holes, query_hole'access);

				if log_category >= HIGH then
					log (text => " distance to hole" & to_string (get_absolute (result)),
						 level => lth + 1);
				end if;
			end if;
		end query_module;

	begin
		if log_category >= HIGH then
			log (text => "computing distance of point" & to_string (point) 
				& " to board edge ...", level => lth);

			log_indentation_up;
		end if;
		
		query_element (module_cursor, query_module'access);	

		if log_category >= HIGH then
			log_indentation_down;
		end if;
		
		return result;
	end get_distance_to_edge;
	
	
	--function on_board (
		--module_cursor	: in pac_generic_modules.cursor;
		--point			: in type_point;
		--log_category	: in type_log_category;
		--lth				: in type_log_level)		
		--return boolean
	--is
		--result : boolean := true;
		
		--procedure query_module (
			--module_name	: in pac_module_name.bounded_string;
			--module		: in et_schematic.type_module) 
		--is
			--procedure query_outline is begin
				--if get_point_to_polygon_status (
					--module.board.contours.outline, to_vector (point)).location = OUTSIDE 
				--then
					----log (text => "outside", level => lth + 1);
					--result := false;
				--end if;
			--end query_outline;
			
			
			--procedure query_holes is
				--use et_packages;
				--use pac_pcb_cutouts;
				--c : pac_pcb_cutouts.cursor := module.board.contours.holes.first;
			--begin
				--while c /= pac_pcb_cutouts.no_element loop
					--if get_point_to_polygon_status (
						--element (c), to_vector (point)).location = INSIDE 
					--then
						--if log_category >= HIGH then
							--log (text => "point is in a hole", level => lth + 1);
						--end if;
						
						--result := false;
						
						--exit; -- no need to test other holes
					--end if;
					
					--next (c);
				--end loop;
			--end query_holes;

			
		--begin -- query_module
			--if log_category >= HIGH then
				--log (text => "probing outline ...", level => lth + 1);
			--end if;
			
			--query_outline;

			--if result = true then -- point is inside board outlines
				--if log_category >= HIGH then
					--log (text => "point is inside board outlines. probing holes ...", level => lth + 1);
				--end if;
				
				--query_holes;
			--end if;
		--end query_module;

		
	--begin -- on_board
		--if log_category >= HIGH then
			--log (text => "probing whether point" & to_string (point) 
			 --& " is on board ...", level => lth);

			--log_indentation_up;
		--end if;
		
		--query_element (module_cursor, query_module'access);

		--if log_category >= HIGH then
			--if result = true then
				--log (text => "point is on board", level => lth);
			--else
				--log (text => "point is not on board", level => lth);
			--end if;
			
			--log_indentation_down;
		--end if;
		--return result;
	--end on_board;

						  


	function to_string (place : in type_place) return string is begin
		return "place: " & type_place'image (place);
	end to_string;

	
	function get_dimensions (
		track : in type_track)
		return type_track_dimensions
	is
		result : type_track_dimensions;

		-- here the given track starts:
		track_start : constant type_point := to_point (track.center.v_start);

		-- the total track width (incl. clearance) is:
		width : constant type_track_width := track.width + 2.0 * track.clearance;

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
		init			: in type_distance;
		place			: in type_place;
		obstacle		: in type_obstacle;
		clearance		: in type_distance_positive;
		log_category	: in type_log_category;
		lth				: in type_log_level) 
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
		max_iterations : constant positive := 2000; -- CS increase if necessary

	begin		
		if log_category = INSANE then
			log (text => "starting numerical search ...", level => lth);
			log_indentation_up;

			case obstacle.shape is
				when LINE =>
					log (text => to_string (place) & " obstacle " & to_string (obstacle.line), level => lth + 1);
				when ARC =>
					log (text => to_string (place) & " obstacle " & to_string (obstacle.arc), level => lth + 1);
				when CIRCLE =>
					log (text => to_string (place) & " obstacle " & to_string (obstacle.circle), level => lth + 1);
			end case;

			log (text => "clearance: " & to_string (clearance), level => lth + 1);
			log (text => "init     : " & to_string (init), level => lth + 1);
		end if;
		
		-- Set the inital position of the cap:
		case place is
			when BEFORE =>
				c.center := type_point (set (init - c.radius, zero));

			when AFTER =>
				c.center := type_point (set (init + c.radius, zero));
		end case;

		if log_category = INSANE then
			log (text => "cap start: " & to_string (c), level => lth + 1);
		end if;

		
		for i in 1 .. max_iterations loop
			
			-- Calculate the distance between the cap (incl. clearance) and the obstacle:
			--log (text => "cap      : " & to_string (c), level => lth + 1);
			
			case obstacle.shape is
				when LINE =>
					d_cap_to_obstacle := get_distance (c, obstacle.line);
				when ARC =>
					d_cap_to_obstacle := get_distance (c, obstacle.arc);
				when CIRCLE =>
					d_cap_to_obstacle := get_distance (c, obstacle.circle);
			end case;
					
			if log_category = INSANE then
				log (text => " distance" & to_string (d_cap_to_obstacle), level => lth + 1);
			end if;
			
			d_cap_to_obstacle_abs := abs (d_cap_to_obstacle);

			
			-- Cancel this loop once the distance is sufficiently small.
			-- Otherwise take half of the distance and move cap to new position:
			if d_cap_to_obstacle_abs < fab_tolerance then
				if log_category = INSANE then
					log (text => " break point found after" & positive'image (i) & " iterations",
						level => lth + 1);
				end if;
				
				exit;
			else
				step := d_cap_to_obstacle_abs * 0.5;
				--step := d_cap_to_obstacle_abs;
				
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


		if log_category = INSANE then
			log_indentation_down;
		end if;
		
		return get_x (c.center);
	end get_break;

	
	function get_break_by_line (
		track				: in type_track;
		track_dimensions	: in type_track_dimensions;
		line				: in type_line;
		place				: in type_place;
		log_category		: in type_log_category;
		lth					: in type_log_level)
		return type_break
	is
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
			angle : constant type_float_internal := type_float_internal (90.0 - i_center.intersection.angle);

			use pac_functions_distance;
		begin
			if log_category = INSANE then
				log_indentation_up;

			--log (text => "line " & to_string (line_tmp) 
				 --& " intersects center of track at" & to_string (i_center.intersection.point)
				 --& " angle" & to_string (i_center.intersection.angle),
				 --level => lth + 2);

			end if;
			
			-- clearance is the distance from center of the cap perpendicular to the line.
			--spacing := type_distance (round (
				--d_fine	=> type_distance_positive (float (clearance) / cos (angle, float (units_per_cycle))),
				--mode	=> UP));

			spacing := type_distance (round (
				d_fine	=> to_distance (type_float_internal (clearance) / cos (angle, units_per_cycle)),
				mode	=> UP));
			
			--spacing := to_distance (type_float_internal (clearance) / cos (angle, units_per_cycle));
			
			--log (text => "spacing float " & float'image (float (clearance) / cos (angle, float (units_per_cycle))));
			--log (text => "spacing       " & to_string (spacing));
			--log (text => "required spacing" & to_string (spacing), level => lth + 2);
			
			-- Depending on the given place, the break point must be moved 
			-- left or right by the spacing:
			case place is
				when BEFORE =>
					bp := type_point (set (to_distance (get_x (i_center.intersection.vector)) - spacing, zero));
					
				when AFTER =>
					bp := type_point (set (to_distance (get_x (i_center.intersection.vector)) + spacing, zero));
			end case;

			if log_category = INSANE then
				log_indentation_down;
			end if;
		end full_intersection;

		
	begin -- get_break_by_line
		if bi.exists then -- line and track boundaries do intersect in some way

			if log_category = INSANE then
				log (text => "break by " & to_string (line), level => lth);
				log_indentation_up;
			end if;

			--log (text => "track " & to_string (track_dimensions.boundaries));
			--log (text => "line  " & to_string (line_boundaries));
			
			if (i_upper.status = EXISTS and i_lower.status = EXISTS) then
				-- The candidate line intersects the upper and lower edge of the track.

				if log_category = INSANE then
					log (text => "line intersects track upper and lower edge", level => lth + 1);
				end if;
				
				full_intersection;
				-- bp is now set

			else
				-- We have a partial intersection.			
				-- The candidate line intersects only one edge or none at all.

				if log_category = INSANE then
					log (text => "line intersects track partially", level => lth + 1);
					log_indentation_up;
				end if;
				
				case place is
					when BEFORE =>
						-- Use the LEFT border of the line boundaries 
						-- as start point for the search operation:
						start_point := line_boundaries.smallest_x;

					when AFTER =>
						-- Use the RIGHT border of the line boundaries 
						-- as start point for the search operation:
						start_point := line_boundaries.greatest_x;
				end case;

				-- start the numerical search for the break point:
				bp := type_point (set (get_break (
					init			=> start_point,
					place			=> place,
					obstacle		=> (et_geometry.LINE, line_tmp),
					clearance		=> clearance,
					log_category	=> log_category,
					lth				=> lth + 1),
					zero));

				if log_category = INSANE then
					log_indentation_down;
				end if;
			end if;

			if log_category = INSANE then
				log_indentation_down;
			end if;
		end if;

		--bp := type_point (round (bp));

		
		-- The computed break point must be after the start of the track.
		-- If it is before the start of the track, then it is discarded.
		if get_x (bp) > zero then

			-- Rotate and move the break point back according to
			-- the track direction and offset:
			rotate_to (bp, track_dimensions.direction);
			move_by (bp, track_dimensions.offset);
			--bp := type_point (round (bp));
			
			break_exists := true;

			if log_category = INSANE then
				log (text => " break point " & type_place'image (place) & " line:" & to_string (bp),
					 level => lth + 2);
			end if;
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
						result.append (to_distance (get_x (i.intersection.vector)));
					end if;

				when TWO_EXIST =>
					result.append (to_distance (get_x (i.intersection_1.vector)));
					result.append (to_distance (get_x (i.intersection_2.vector)));
					
				when NONE_EXIST => null;
			end case;
		end read_x_position;
		
	begin
		read_x_position (i_upper);
		read_x_position (i_lower);

		sort (result);
		
		return result;
	end get_x_values;


	-- Searches for each arc fragment the break before/after 
	-- the intersection with the track.
	procedure set_break_points (
		track_dimensions	: in type_track_dimensions;
		clearance			: in type_distance_positive;						   
		place				: in type_place;
		arcs				: in type_arcs; -- arcs is an array of arc segments
		bp1					: in out type_point;
		bp2					: in out type_point;
		break_count			: in out type_break_count;
		log_category		: in type_log_category;
		lth					: in type_log_level)
	is
		arc_boundaries : type_boundaries;

		use pac_distances;
		x_values : pac_distances.list;
		x_cursor : pac_distances.cursor;
	begin
		-- Loop in collection of arc segments (it is an array of arcs):
		for i in arcs'first .. arcs'last loop

			if log_category = INSANE then
				-- log the candidate arc:
				log (text => "fragment" & natural'image (i) & ": "
					& to_string (arcs (i)), level => lth);
			end if;
			
			-- Get the boundaries of the candidate arc:
			arc_boundaries := get_boundaries (arcs (i), zero); -- arc has zero width

			declare
				-- Get the overlap area of the track and arc boundaries:
				bi : constant type_boundaries_intersection := 
					get_intersection (track_dimensions.boundaries, arc_boundaries);

				-- the place along the x-axis where the search for the break is to begin:
				start_point : type_distance;

				x_pre : type_distance;
			begin
				if bi.exists then
					case place is
						when BEFORE =>
							-- Use the LEFT border of the arc boundaries
							-- as start point for the search operation:
							start_point := arc_boundaries.smallest_x;

						when AFTER =>
							-- Use the RIGHT border of the arc boundaries
							-- as start point for the search operation:
							start_point := arc_boundaries.greatest_x;

					end case;

					x_pre := get_break (
						init			=> start_point,
						place			=> place,
						obstacle		=> (et_geometry.ARC, arcs (i)),
						clearance		=> clearance,
						log_category	=> log_category,
						lth				=> lth + 1);
					
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
		pac_distances_sorting.sort (x_values);

		-- derive the break count from the number of x-positions
		break_count := type_break_count (length (x_values));

		case break_count is
			when 0 => null;
			when 1 => 
				x_cursor := x_values.first;
				bp1 := type_point (set (element (x_cursor), zero));

				-- Rotate and move the break point back according to
				-- the track direction and offset:
				rotate_to (bp1, track_dimensions.direction);
				move_by (bp1, track_dimensions.offset);

				if log_category = INSANE then
					log (text => "break point 1 " & type_place'image (place) & " arc:" & to_string (bp1),
						level => lth + 2);
				end if;
				
			when 2 =>
				x_cursor := x_values.first;					
				bp1 := type_point (set (element (x_cursor), zero));
				next (x_cursor);
				bp2 := type_point (set (element (x_cursor), zero));

				-- Rotate and move the break points back according to
				-- the track direction and offset:
				rotate_to (bp1, track_dimensions.direction);
				move_by (bp1, track_dimensions.offset);

				if log_category = INSANE then
					log (text => "break point 1 " & type_place'image (place) & " arc:" & to_string (bp1),
						 level => lth + 2);
				end if;

				rotate_to (bp2, track_dimensions.direction);
				move_by (bp2, track_dimensions.offset);

				if log_category = INSANE then
					log (text => "break point 2 " & type_place'image (place) & " arc:" & to_string (bp2),
						 level => lth + 2);
				end if;

		end case;
	end set_break_points;

	
	function get_break_by_arc (
		track				: in type_track;
		track_dimensions	: in type_track_dimensions;
		arc					: in type_arc;
		place				: in type_place;
		log_category		: in type_log_category;
		lth					: in type_log_level)
		return type_break_double
	is
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

		
		-- the possible break points and the number of break points:
		break_count : type_break_count := 0;
		bp1, bp2 : type_point;

		-- Get all the intersections of the track with the arc.
		-- The arc may intersect the upper and lower edge of the track.
		use pac_distances;
		x_values_pre : pac_distances.list := get_x_values (i_upper, i_lower);
		-- x_values_pre now contains the sorted x-positions from left to right


		-- The arc must be split in 2 or 3 smaller arcs. Each of them
		-- will then be treated separately.
		procedure split is begin
			if log_category = INSANE then
				log (text => "splitting of arc required", level => lth + 1);
				log_indentation_up;
			end if;
			
			set_break_points (
				track_dimensions, clearance, place, 
				split_arc (arc_tmp), -- the arc is split here
				bp1, bp2, break_count, log_category, lth + 2);

			if log_category = INSANE then
				log_indentation_down;
			end if;
		end split;
		
		
		-- Searches the break point (before or after) by means 
		-- of the boundaries of the whole overlapping area.
		procedure use_overlap_area is 
			-- the place along the x-axis where the search for the break is to begin:
			start_point : type_distance;
		begin
			if log_category = INSANE then
				log (text => "using boundaries of whole overlapping area", level => lth + 1);
				log_indentation_up;
			end if;
			
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
				init			=> start_point,
				place			=> place,
				obstacle		=> (et_geometry.ARC, arc_tmp),
				clearance		=> clearance,
				log_category	=> log_category,
				lth				=> lth + 2),
				zero));

			-- The computed break point must be after the start of the track.
			-- If it is before the start of the track, then it is discarded.
			if get_x (bp1) > zero then -- CS really necessary ?

				-- Rotate and move the break point back according to
				-- the track direction and offset:
				rotate_to (bp1, track_dimensions.direction);
				move_by (bp1, track_dimensions.offset);

				break_count := 1;
				
				if log_category = INSANE then
					log (text => "break point " & type_place'image (place) & " arc:" & to_string (bp1),
						 level => lth + 2);
				end if;
			end if;

			if log_category = INSANE then
				log_indentation_down;
			end if;
		end use_overlap_area;
		
		
	begin -- get_break_by_arc
		if bi.exists then -- arc and track do intersect in some way

			if log_category = INSANE then
				log (text => "break by " & to_string (arc), level => lth);
				log_indentation_up;
			end if;
			
			-- The number of intersections with the arc determines
			-- how to proceed:
			case length (x_values_pre) is

				when 0 =>
					-- The arc is embedded in the track and does NOT intersect 
					-- the edges of the track.
					-- If x_values_pre is empty then the arc is embedded in the track
					-- without touching the upper or lower edge of the track.
					-- If we search for a break before the arc, then it makes sense
					-- only if the area of overlap begins after the start of the track.
					if (place = BEFORE and bi.intersection.smallest_x >= zero)

					-- CS: A similar optimization when place is AFTER ?				
					or place = AFTER 
					--or (place = AFTER and bi.intersection.greatest_x >= zero)
					then
						use_overlap_area;
					else
						if log_category = INSANE then
							log (text => "boundaries of arc are before start of track -> arc skipped",
								 level => lth);
						end if;
					end if;

				when 1..2 =>
					-- The arc intersects the track in up to two points.
					-- If we search for a break before the arc, then it makes sense
					-- only if the last intersection comes after the start of the track.
					if (place = BEFORE and x_values_pre.last_element >= zero)

					-- CS: A similar optimization when place is AFTER ?				
					or place = AFTER 
					then
						if i_upper.status = TWO_EXIST xor i_lower.status = TWO_EXIST then
						-- The arc intersects either the upper or the lower edge of
						-- the track twice:
							split;
						else
						-- The arc intersects the upper and lower edge of the track once.
						-- OR: The arc intersects any edge only once:
							use_overlap_area;
						end if;
					else
						if log_category = INSANE then
							log (text => "all intersections are before start of track -> arc skipped",
								 level => lth);
						end if;
					end if;
				
				when 3..4 =>
					-- The arc intersects the track in 3 or 4 points.
					split;

				when others => raise constraint_error; -- CS should never happen
			end case;

			if log_category = INSANE then
				log_indentation_down;
			end if;
		end if;


		case break_count is
			when 0 => return (count => 0);
			when 1 => return (1, bp1);
			when 2 => return (2, bp1, bp2);
		end case;
	end get_break_by_arc;


	function get_break_by_circle (
		track				: in type_track;
		track_dimensions	: in type_track_dimensions;
		circle				: in type_circle;
		place				: in type_place;
		log_category		: in type_log_category;
		lth					: in type_log_level)
		return type_break_double
	is
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
		
	begin -- get_break_by_circle
		if bi.exists then -- circle and track do intersect in some way

			if log_category = INSANE then
				log (text => "break by " & to_string (circle), level => lth);
				log_indentation_up;
			end if;
			
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

						if log_category = INSANE then
							log (text => "using boundaries of whole overlapping area",
								level => lth + 1);
						
							log_indentation_up;
						end if;
						
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
							init			=> start_point,
							place			=> place,
							obstacle		=> (et_geometry.CIRCLE, circle_tmp),
							clearance		=> clearance,
							log_category	=> log_category,
							lth				=> lth + 2),
							zero));

						-- The computed break point must be after the start of the track.
						-- If it is before the start of the track, then it is discarded.
						if get_x (bp1) > zero then

							-- Rotate and move the break point back according to
							-- the track direction and offset:
							rotate_to (bp1, track_dimensions.direction);
							move_by (bp1, track_dimensions.offset);

							break_count := 1;

							if log_category = INSANE then
								log (text => "break point " & type_place'image (place) & " circle:" & to_string (bp1),
									 level => lth + 2);
							end if;
						end if;

						if log_category = INSANE then
							log_indentation_down;
						end if;
					else
						if log_category = INSANE then
							log (text => "boundaries of circle before start of track -> circle skipped",
								 level => lth);
						end if;
					end if;

					
				when 4 =>					
					-- The circle intersects the track in 4 points.
					-- So the circle must be split in 2 arcs. Each arc
					-- will then be treated separately.
					if log_category = INSANE then
						log (text => "splitting of circle required", level => lth + 1);
						log_indentation_up;
					end if;
					
					-- split the circle in 2 arcs:
					set_break_points (
						track_dimensions, clearance, place, 
						split_circle (circle_tmp), -- the circle is split here
						bp1, bp2, break_count, log_category, lth + 2);

					if log_category = INSANE then
						log_indentation_down;
					end if;
					
				when others =>
					raise constraint_error; -- CS useful message
			end case;

			if log_category = INSANE then
				log_indentation_down;
			end if;
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
		design_rules	: in type_design_rules;
		bottom_layer	: in type_signal_layer;		
		start_point		: in type_point;
		place			: in type_place := BEFORE;
		direction		: in type_rotation;
		net_cursor		: in et_schematic.pac_nets.cursor := et_schematic.pac_nets.no_element;
		net_class		: in type_net_class;
		fill_zone		: in type_fill_zone;
		layer			: in type_signal_layer;
		width			: in type_track_width;
		ignore_same_net	: in boolean;
		log_category	: in type_log_category := log_category_default;
		lth				: in type_log_level)
		return type_route_distance is separate;

	
	function clear_for_track (
		module_cursor	: in pac_generic_modules.cursor;
		design_rules	: in type_design_rules;
		bottom_layer	: in type_signal_layer;
		start_point		: in type_point;
		net_cursor		: in et_schematic.pac_nets.cursor;
		net_class		: in type_net_class;
		fill_zone		: in type_fill_zone;
		layer			: in type_signal_layer;
		width			: in type_track_width;
		ignore_same_net	: in boolean;
		log_category	: in type_log_category := log_category_default;
		lth				: in type_log_level)		
		return boolean is separate;

	
end et_routing;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
