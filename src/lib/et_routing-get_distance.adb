------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         ROUTING.GET_DISTANCE                             --
--                                                                          --
--                               B o d y                                    --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with et_text;

separate (et_routing)

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
	return type_route_distance
is
	probe_ray : constant type_ray := (to_vector (start_point), direction);
	probe_line : constant type_line_vector := to_line_vector (probe_ray);

	-- get the design rules of the module:
	design_rules : constant type_design_rules := get_pcb_design_rules (module_cursor);
	
	-- The top conductor layer 1 is always there:
	top_layer		: constant type_signal_layer := type_signal_layer'first;

	-- The deepest conductor layer towards bottom is defined by the layer stack:
	bottom_layer	: constant type_signal_layer := deepest_conductor_layer (module_cursor);
	
	function is_inner_layer (layer : in type_signal_layer) return boolean is begin
		if layer > top_layer and layer < bottom_layer then
			return true;
		else
			return false;
		end if;
	end is_inner_layer;		


	use et_pcb;
	
	-- Get the net class settings of the given net.
	-- If no net was given (freetrack), then we get the settings of class "default":
	class_given_net : constant type_net_class := get_net_class (module_cursor, net_cursor);

	
	track : type_track := (
		center		=> probe_line,
		width		=> width,
		others		=> <>);

	distance_to_obstacle : type_distance_positive := type_distance_positive'last;
	distance_after_obstacle : type_distance_positive := type_distance_positive'last;
	status : type_valid := VALID;

	package pac_points_after_obstacles is new doubly_linked_lists (type_point);
	points_after_obstacles : pac_points_after_obstacles.list;
	use pac_points_after_obstacles;
	package pac_sorting is new pac_points_after_obstacles.generic_sorting;

	-- If given place is BEFORE then this procedure updates distance_to_obstacle
	-- so that the smallest distance from start_point to the break points is kept.
	-- If place is AFTER then the procedure collects the given break point
	-- in container "points_after_obstacles":
	procedure process_break (break : in type_point) is
		d : type_distance_positive;
	begin
		case place is
			when BEFORE =>
				d := get_distance_total (start_point, break);
				
				if d < distance_to_obstacle then
					distance_to_obstacle := d;
				end if;

			when AFTER =>
				append (points_after_obstacles, break);
				
		end case;
	end process_break;

	-- Test whether the given line causes a break in the track.
	-- Parameter "place" determines whether we are interested in the
	-- start or the end of the break.
	-- If there is a break then its position is sent to procedure
	-- process_break for further processing.
	procedure test_line (l : in type_line) is 
		b : constant type_break := get_break_by_line (track, l, place, lth + 2);
	begin
		--log (text => "test line");
		
		if b.exists then
			process_break (b.point);
		end if;
	end test_line;

	-- See procedure test_line for details.
	procedure test_arc (a : in type_arc) is
		b : constant type_break_double := get_break_by_arc (track, a, place, lth + 2);
	begin
		--log (text => "test arc");

		case b.count is
			when 0 => null;
				--log (text => "test arc 0");
				 
			when 1 => 
				--log (text => "test arc 1");
				process_break (b.point);
				
			when 2 => 
				--log (text => "test arc 2");
				process_break (b.point_1); process_break (b.point_2);
		end case;
	end test_arc;

	-- See procedure test_line for details.
	procedure test_circle (c : in type_circle) is 
		b : constant type_break_double := get_break_by_circle (track, c, place, lth + 2);
	begin
		--log (text => "test circle");
				
		case b.count is
			when 0 => null;
			when 1 => process_break (b.point);
			when 2 => process_break (b.point_1); process_break (b.point_2);
		end case;
	end test_circle;

	
	procedure query_obstacles (
		module_name	: in pac_module_name.bounded_string;
		module		: in et_schematic.type_module) 
	is
		use pac_polygon_segments;

		procedure query_segment (c : in pac_polygon_segments.cursor) is begin
			--log (text => "track start: " & to_string (track.center.v_start));
			case element (c).shape is
				when LINE => 
					--log (text => " line" & to_string (element (c).segment_line));
					test_line (element (c).segment_line);
					
				when ARC =>
					--log (text => " arc" & to_string (element (c).segment_arc));
					test_arc (element (c).segment_arc);
					
			end case;

			--log (text => "test end");				
		end query_segment;

		-- BOARD OUTLINE
		procedure query_outline is begin
			log (text => "probing outline ...", level => lth + 1);
			log_indentation_up;
			
			if module.board.contours.outline.contours.circular then
				test_circle (module.board.contours.outline.contours.circle);
			else
				iterate (module.board.contours.outline.contours.segments, query_segment'access);
			end if;

			log_indentation_down;
		end query_outline;

		-- holes
		procedure query_holes is
			use et_packages;			
			use pac_pcb_cutouts;

			procedure query_hole (c : in pac_pcb_cutouts.cursor) is begin
				log_indentation_up;
				
				if element (c).contours.circular then
					--log (text => "circular hole");
					test_circle (element (c).contours.circle);
				else		
					--log (text => "n-shaped hole");
					iterate (element (c).contours.segments, query_segment'access);
				end if;

				log_indentation_down;
			end query_hole;
			
		begin -- query_holes
			log (text => "probing holes ...", level => lth + 1);				
			iterate (module.board.contours.holes, query_hole'access);
		end query_holes;

		
		-- CONDUCTOR FILL ZONES
		procedure query_fill_zone is begin
			log (text => "probing fill zone ...", level => lth + 1);
			log_indentation_up;
			
			if fill_zone.outline.contours.circular then
				test_circle (fill_zone.outline.contours.circle);
			else
				iterate (fill_zone.outline.contours.segments, query_segment'access);
			end if;

			log_indentation_down;
		end query_fill_zone;


		-- GLOBAL CUTOUT AREAS
		procedure query_global_cutouts is 
			use et_conductor_polygons.pac_conductor_cutouts;
			
			procedure query_cutout (c : in et_conductor_polygons.pac_conductor_cutouts.cursor) is begin
				if element (c).layer = layer then
					log_indentation_up;
						
					if element (c).contours.circular then
						test_circle (element (c).contours.circle);
					else		
						iterate (element (c).contours.segments, query_segment'access);
					end if;

					log_indentation_down;
				end if;
			end query_cutout;
		
		begin -- query_global_cutouts
			log (text => "probing global cutout areas ...", level => lth + 1);
			iterate (module.board.conductors.cutouts, query_cutout'access);
		end query_global_cutouts;


		-- TRACKS
		procedure query_tracks is
			use et_schematic;
			use pac_nets;

			-- Queries the conductor segments of foregin nets.
			procedure query_net (nf : in pac_nets.cursor) is
				use et_nets.pac_net_name;
				use et_pcb.pac_conductor_lines;
				use et_pcb.pac_conductor_arcs;

				class_foregin_net : constant type_net_class := get_net_class (module_cursor, nf);

				use pac_distances_sorting;
				clearances : pac_distances_positive.list;

				use et_packages;				
				
				procedure query_line (c : in et_pcb.pac_conductor_lines.cursor) is
					segment : type_conductor_line_segment;
				begin
					if element (c).layer = layer then
						log (text => "segment " & to_string (element (c)), level => lth + 3);
						log_indentation_up;
						
						segment := to_line_segment (element (c));

						test_line (get_left_edge (segment));
						test_line (get_right_edge (segment));
						test_arc (get_start_cap (segment));
						test_arc (get_end_cap (segment));

						-- CS procedure test_segment_line (segment)
						log_indentation_down;
					end if;
				end query_line;

				procedure query_arc (c : in et_pcb.pac_conductor_arcs.cursor) is
					segment : type_conductor_arc_segment;
				begin
					if element (c).layer = layer then
						log (text => "segment " & to_string (element (c)), level => lth + 3);
						log_indentation_up;
						
						segment := to_arc_segment (element (c));

						test_arc (get_outer_edge (segment));
						test_arc (get_end_cap (segment));
						test_arc (get_inner_edge (segment));
						test_arc (get_end_cap (segment));

						log_indentation_down;
					end if;
				end query_arc;

				
				-- VIAS
				use et_vias;
				use pac_vias;

				procedure query_via (v : in pac_vias.cursor) is 

					function to_circle (restring : in type_restring_width) return type_circle is begin
						return (
							center => element (v).position,
							radius	=> element (v).diameter * 0.5 + restring);
					end to_circle;
					
				begin
					case element (v).category is
						when THROUGH =>
							log (text => to_string (element (v)), level => lth + 3);

							if is_inner_layer (layer) then
								test_circle (to_circle (element (v).restring_inner));

							else -- top or bottom layer
								test_circle (to_circle (element (v).restring_outer));
							end if;

						when BURIED =>
							if buried_via_uses_layer (element (v), layer) then
								test_circle (to_circle (element (v).restring_inner));
							end if;
							
						when BLIND_DRILLED_FROM_TOP =>
							if layer = type_signal_layer'first then
								test_circle (to_circle (element (v).restring_top));

							elsif blind_via_uses_layer (element (v), layer) then
								test_circle (to_circle (element (v).restring_inner));
							end if;

						when BLIND_DRILLED_FROM_BOTTOM =>
							if layer = bottom_layer then
								test_circle (to_circle (element (v).restring_bottom));

							elsif blind_via_uses_layer (element (v), layer, bottom_layer) then
								test_circle (to_circle (element (v).restring_inner));
							end if;

					end case;
				end query_via;

				procedure query_segments_and_vias is begin
					-- The clearance to foregin nets is the greatest of several different distances.
					-- The greatest of them will later be applied to the track clearance.
					clearances.append (class_given_net.clearance);
					clearances.append (class_foregin_net.clearance);

					if fill_zone.observe then 
						clearances.append (fill_zone.outline.isolation);
					end if;

					track.clearance	:= get_greatest (clearances);
					
					log_indentation_up;
					iterate (element (nf).route.lines, query_line'access);
					iterate (element (nf).route.arcs, query_arc'access);
					iterate (element (nf).route.vias, query_via'access);
					
					-- CS other objects ... see et_pcb.type_route
					log_indentation_down;
				end query_segments_and_vias;
				
			begin -- query_net
				log (text => "net " & to_string (key (nf)), level => lth + 2);

				if ignore_same_net then
					if net_cursor /= nf then
						query_segments_and_vias;
					end if;
				else
					query_segments_and_vias;
				end if;
			end query_net;
			
		begin -- query_tracks
			log (text => "probing tracks ...", level => lth + 1);
			log_indentation_up;
			iterate (module.nets, query_net'access);
			log_indentation_down;
		end query_tracks;

		
		procedure query_texts is
			use et_packages;
			use pac_conductor_texts;
			use et_text;
				
			procedure query_text (c : in pac_conductor_texts.cursor) is
				use et_board_shapes_and_text.pac_text_fab;
				use pac_vector_text_lines;

				procedure query_line (l : in pac_vector_text_lines.cursor) is
					-- Convert the line of the vector text to a 
					-- conductor line.
					cl : constant et_packages.type_conductor_line := 
						(type_line (element (l)) with element (c).line_width);
					
					-- Now we treat the line of the vector text like a regular
					-- line of conductor material:
					segment : constant type_conductor_line_segment := to_line_segment (cl);
				begin
					log_indentation_up;
					log (text => "text " & to_string (element (l)), level => lth + 3);

					test_line (get_left_edge (segment));
					test_line (get_right_edge (segment));

					test_arc (get_start_cap (segment));
					test_arc (get_end_cap (segment));
					
					-- CS procedure test_segment_line (segment)

					log_indentation_down;
				end query_line;

			begin -- query_text
				log (text => "text:" 
					 & " P:" & to_string (element (c).position)
					 & " / L: " & to_string (element (c).layer)
					 & " / C: " & enclose_in_quotes (to_string (element (c).content)),
					 level => lth + 2);

				if element (c).layer = layer then
					element (c).vectors.iterate (query_line'access);
				end if;
			end query_text;

			use pac_distances_sorting;
			clearances : pac_distances_positive.list;
			
		begin
			log (text => "probing vector texts ...", level => lth + 1);
			log_indentation_up;

			-- The clearance to the text is the greatest of 
			-- either the polygon isolation or the clearance of the given net.
			-- The greatest of them will be applied to the track clearance.
			clearances.append (class_given_net.clearance);

			if fill_zone.observe then 
				clearances.append (fill_zone.outline.isolation);
			end if;

			track.clearance	:= get_greatest (clearances);

			
			iterate (module.board.conductors.texts, query_text'access);
			log_indentation_down;
		end query_texts;

			
	begin -- query_obstacles

		track.clearance	:= design_rules.clearances.conductor_to_board_edge;

		-- board contours:
		query_outline;
		query_holes;

		
		-- next step is querying fill zones. There the clearance to the
		-- zone borders is zero. The track is to approach the border as
		-- close as possible (from inside the area):
		track.clearance := zero;
		
		if fill_zone.observe then 
			query_fill_zone;
		end if;

		-- For global cutout areas the track must approach the border
		-- of the area as close as possible (from outside the area):
		query_global_cutouts;

		query_tracks; -- The clearance is set for each net individually:

		-- CS query freetracks
		
		-- - net specific cutout areas
		
		-- CS abort if status is invalid ??? obsolete ??

		query_texts; -- The clearance is set according to net class and polygon isolation.
		
		-- query pads, ...

		-- query route restrict (ignore-parameter ?)

		-- CS: submodules ?
	end query_obstacles;


	-- Collects after start_point all the points where obstacles end.
	-- Detects the nearest point where it is allowed to start a track.
	-- If a suitable point was found then status changes to VALID.
	procedure find_valid_point_after_obstacles is
		c : pac_points_after_obstacles.cursor;
	begin
		query_element (module_cursor, query_obstacles'access);
		pac_sorting.sort (points_after_obstacles);

		c := points_after_obstacles.first;
		while c /= pac_points_after_obstacles.no_element loop

			if clear_for_track (
				module_cursor, element (c), net_cursor,
				fill_zone, layer, width, ignore_same_net, lth + 1) 
			then
				distance_after_obstacle := get_distance_total (start_point, element (c));
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
			log (text => "computing distance to obstacle from point" 
					& to_string (start_point)
					& " direction" & to_string (direction) & " ...",
					level => lth);

			log_indentation_up;
			
			-- Test whether start_point is suitable to start a track.
			-- At the given start_point or in its vicinity could be an obstacle already.
			if clear_for_track (module_cursor, start_point, 
				net_cursor, fill_zone, layer, width, ignore_same_net, lth + 1) 
			then
				-- start_point qualifies to start a track

				-- Probe everything on the board that could be an obstacle
				-- for the track in the given direction, net, layer. The distance to 
				-- the nearest obstacle (to the right of start_point) will finally be
				-- stored in variable distance_to_obstacle:
				query_element (module_cursor, query_obstacles'access);
				
				log (text => "distance to obstacle:" & to_string (distance_to_obstacle),
						level => lth);
				
				log_indentation_down;
				
				return (VALID, distance_to_obstacle);
				
			else 
				-- start_point does NOT qualify to start a track
				
				log (text => "track not allowed here",
					level => lth);

				log_indentation_down;
				
				return (status => INVALID);
			end if;


			
		when AFTER =>
			log (text => "computing distance until after obstacles from point" 
					& to_string (start_point)
					& " direction" & to_string (direction) & " ...",
					level => lth);

			log_indentation_up;

			-- There can be more than one obstacle. They may overlap in some way.
			-- So a point must be found after a cluster of obstacles where 
			-- it is allowed to start a track:
			find_valid_point_after_obstacles;

			-- Now the variable "status" indicates whether a valid point has 
			-- been found or not.
			
			case status is
				when VALID => -- suitable point found

					log (text => "distance until after obstacle:" & to_string (distance_after_obstacle),
						level => lth);
					
					log_indentation_down;

					return (VALID, distance_after_obstacle);

					
				when INVALID => -- NO suitable point found 
					
					log (text => "no obstacle found",
						level => lth);
					
					log_indentation_down;

					return (status => INVALID);
			end case;

			
	end case;

end get_distance;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
