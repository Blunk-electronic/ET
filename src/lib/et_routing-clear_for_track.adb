------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         ROUTING.CLEAR_FOR_TRACK                          --
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


separate (et_routing)

function clear_for_track (
	module_cursor	: in pac_generic_modules.cursor;
	start_point		: in type_point;
	net_cursor		: in et_schematic.pac_nets.cursor;
	fill_zone		: in type_fill_zone;
	layer			: in type_signal_layer;
	width			: in type_track_width;
	ignore_same_net	: in boolean;
	lth				: in type_log_level)		
	return boolean
is
	result : boolean := false;

	-- For some preselections (to improve performance) we will test if boundaries of
	-- objects overlap with the boundaries of the given start point. The start point
	-- is the center of a circle that has half the width of the inquired track.
	-- This is the inital circle around the start point.
	circle_around_start_point_init : constant type_circle := (start_point, width * 0.5);
	-- In the course of this function copies are taken of this circle and their radius
	-- modified by the particular clearance being effective.
	
	-- The boundaries of the circle around the start point:
	start_point_boundaries : type_boundaries;


	
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

	
	procedure query_module (
		module_name	: in pac_module_name.bounded_string;
		module		: in et_schematic.type_module) 
	is
		-- FILL ZONE
		procedure query_fill_zone is 
			use et_packages;
			distance_to_border : type_distance; -- CS rename to distance_to_border
		begin
			log (text => "probing fill zone ...", level => lth + 1);
			log_indentation_up;

			if in_polygon_status (fill_zone.outline, start_point).status = INSIDE then
				log (text => "point is in fill zone", level => lth + 1);

				-- the distance of the point to the border of the fill zone:
				distance_to_border := get_absolute (get_shortest_distance (fill_zone.outline, start_point));

				log (text => "distance to border:" & to_string (distance_to_border),
					level => lth + 1);
				
				-- the distance of the start point to the border:
				distance_to_border := distance_to_border - 0.5 * width;

				if distance_to_border >= zero then
					log (text => "point is in safe distance to border", level => lth + 1);
					result := true;
				else
					log (text => "point is too close to border", level => lth + 1);
					result := false;
				end if;
				
			else
				log (text => "point is outside fill zone", level => lth + 1);
				result := false;
			end if;

			log_indentation_down;
		end query_fill_zone;

		
		-- GLOBAL CUTOUTS IN CONDUCTOR POLYGONS
		procedure query_global_cutouts is 
			use et_conductor_polygons.boards;
			use pac_conductor_cutouts;

			procedure query_cutout (c : in pac_conductor_cutouts.cursor) is 
				distance_to_border : type_distance;
			begin
				if element (c).layer = layer then
					log_indentation_up;
						
					if in_polygon_status (element (c), start_point).status = OUTSIDE then
						log (text => "point is outside global cutout area", level => lth + 1);

						-- the distance of the point to the border of the cutout area:
						distance_to_border := get_absolute (get_shortest_distance (element (c), start_point));

						log (text => " distance to border:" & to_string (distance_to_border),
							level => lth + 1);
						
						-- the distance of the start point line to the border:
						distance_to_border := distance_to_border - 0.5 * width;

						if distance_to_border >= zero then
							log (text => " point is in safe distance to border", level => lth + 1);
						else
							log (text => " point is too close to border", level => lth + 1);
							result := false;
						end if;
						
					else
						log (text => " point is in global cutout area", level => lth + 1);
						result := false;
					end if;
					
					log_indentation_down;
				end if;
			end query_cutout;

		begin
			log (text => "probing global cutout areas ...", level => lth + 1);
			iterate (module.board.conductors.cutouts, query_cutout'access);
			-- CS use a loop instead of iterate. if result goes false, there is no need
			-- to probe other cutouts.
		end query_global_cutouts;

		
		-- TRACKS
		procedure query_tracks is
			use et_nets.pac_net_name;
			use et_schematic;
			use pac_nets;
			
			-- the cursor to the foregin net
			nf : pac_nets.cursor := module.nets.first;

			procedure query_net (
				name : in pac_net_name.bounded_string;
				net  : in type_net) 
			is
				class_foregin_net : constant type_net_class := get_net_class (module_cursor, nf);
						
				procedure query_segments_and_vias is 
					distance : type_distance;
					clearances : pac_distances_positive.list;

					-- clears the "result" flag if variable "distance" is:
					-- - negative or
					-- - the requested track is too close to the foregin segment or via
					procedure test_distance is begin
						log_indentation_up;
						
						if distance <= zero then 
							-- start_point is inside segment/via or on the edge of the segment/via
							log (text => "point is inside", level => lth + 4);
							result := false;
						else
							-- start_point is outside the segment/via
							log (text => "point is outside", level => lth + 4);
							
							-- the distance of the start point to the border of the segment/via:
							distance := distance - width * 0.5;

							if distance >= get_greatest (clearances) then
								log (text => "point is in safe distance", level => lth + 4);
							else
								log (text => "point is too close", level => lth + 4);
								result := false;
							end if;							
						end if;

						log_indentation_down;
					end test_distance;
					
					procedure query_lines is 
						use et_pcb.pac_conductor_lines;
						l : et_pcb.pac_conductor_lines.cursor := net.route.lines.first;
						segment_line : et_conductor_segment.type_conductor_line_segment;
						use et_packages;
					begin
						while l /= et_pcb.pac_conductor_lines.no_element and result = true loop
							segment_line := to_line_segment (element (l));
							log (text => to_string (segment_line), level => lth + 3);
							distance := get_shortest_distance (start_point, segment_line);
							test_distance;
							next (l);
						end loop;
					end query_lines;
					
					procedure query_arcs is 
						use et_pcb.pac_conductor_arcs;
						a : et_pcb.pac_conductor_arcs.cursor := net.route.arcs.first;
						segment_arc : et_conductor_segment.type_conductor_arc_segment;
						use et_packages;
					begin
						while a /= et_pcb.pac_conductor_arcs.no_element and result = true loop
							segment_arc := to_arc_segment (element (a));
							log (text => to_string (segment_arc), level => lth + 3);
							distance := get_shortest_distance (start_point, segment_arc);
							test_distance;
							next (a);
						end loop;
					end query_arcs;

					procedure query_vias is
						use et_vias;
						use pac_vias;
						v : pac_vias.cursor := net.route.vias.first;
						c : type_circle;

						procedure set_radius (restring : in type_restring_width) is begin
							c.radius := element (v).diameter * 0.5 + restring;

							if get_point_to_circle_status (start_point, c) = OUTSIDE then
								distance := get_absolute (get_shortest_distance (start_point, c));
								test_distance;
							else
								-- the start_point is inside the via
								result := false;
								log (text => " point is inside the via", level => lth + 4);
							end if;							
						end set_radius;
						
					begin -- query_vias
						while v /= pac_vias.no_element and result = true loop

							c.center := element (v).position;
							log (text => to_string (element (v)), level => lth + 3);
							
							case element (v).category is
								when THROUGH =>
									if is_inner_layer (layer) then
										set_radius (element (v).restring_inner);
									else
										set_radius (element (v).restring_outer);
									end if;
									
								when BURIED =>
									if buried_via_uses_layer (element (v), layer) then
										set_radius (element (v).restring_inner);
									end if;
									
								when BLIND_DRILLED_FROM_TOP =>
									if layer = type_signal_layer'first then
										set_radius (element (v).restring_top);

									elsif blind_via_uses_layer (element (v), layer) then
										set_radius (element (v).restring_inner);
									end if;

								when BLIND_DRILLED_FROM_BOTTOM =>
									if layer = bottom_layer then
										set_radius (element (v).restring_bottom);

									elsif blind_via_uses_layer (element (v), layer, bottom_layer) then
										set_radius (element (v).restring_inner);
									end if;
							end case;

							next (v);
						end loop;
					end query_vias;

					
				begin -- query_segments_and_vias
					log_indentation_up;
					
					clearances.append (class_given_net.clearance);
					clearances.append (class_foregin_net.clearance);

					if fill_zone.observe then 
						clearances.append (fill_zone.outline.isolation);
					end if;

					query_lines;
					query_arcs;
					query_vias;
				
					log_indentation_down;
				end query_segments_and_vias;
				
			begin -- query_net
				log (text => "net " & to_string (name), level => lth + 2);

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
			
			while nf /= pac_nets.no_element and result = true loop
				query_element (nf, query_net'access);
				next (nf);
			end loop;
			
			log_indentation_down;
		end query_tracks;
		

		-- TEXTS
		procedure query_texts is
			use pac_conductor_texts;
			t : pac_conductor_texts.cursor := module.board.conductors.texts.first;

			-- There can be many clearances which must be taken into account.
			-- The greatest among them will be relevant:
			clearances : pac_distances_positive.list;
			greatest_clearance : type_distance_positive;
			
			distance : type_distance;
			
			-- clears the "result" flag if variable "distance" is:
			-- - negative or
			-- - the requested track is too close to the segment of the text
			procedure test_distance is begin
				log_indentation_up;
				
				if distance <= zero then 
					-- start_point is inside text segment or on the edge of the segment
					log (text => "point is inside. distance to border" 
						 & to_string (distance), level => lth + 4);
					
					result := false;
				else
					-- start_point is outside the segment
					log (text => "point is outside", level => lth + 4);
					
					-- the distance of the start point to the border of the segment:
					distance := distance - width * 0.5;

					log (text => "distance:" & to_string (distance), level => lth + 5);
					
					if distance >= greatest_clearance then
						log (text => "point is in safe distance", level => lth + 4);
					else
						log (text => "point is too close", level => lth + 4);
						result := false;
					end if;							
				end if;

				log_indentation_down;
			end test_distance;

			
			procedure query_vectors (text : in type_conductor_text) is
				use et_board_shapes_and_text.pac_text_fab;
				use pac_vector_text_lines;

				vl : pac_vector_text_lines.cursor := pac_text_fab.first (text.vectors);
				cl : et_conductor_segment.type_conductor_line;
				segment : type_conductor_line_segment;
			begin
				while vl /= pac_vector_text_lines.no_element and result = true loop
					
					log (text => to_string (element (vl)) 
						 & " width" & to_string (element (t).line_width),
						 level => lth + 2);
					
					log_indentation_up;
					
					-- Convert the line of the vector text to a conductor line.
					cl := (type_line (element (vl)) with element (t).line_width);
					
					-- Now we treat the line of the vector text like a regular
					-- line of conductor material:
					segment := to_line_segment (cl);

					log (text => to_string (segment), level => lth + 3);
					distance := get_shortest_distance (start_point, segment);
					test_distance;

					log_indentation_down;
					
					next (vl);
				end loop;
			end query_vectors;

			-- Take a copy of the initial circle_around_start_point_init:
			circle_around_start_point : type_circle := circle_around_start_point_init;
			
			text_boundaries : type_boundaries;
			
		begin -- query_texts
			log (text => "probing texts ...", level => lth + 1);
			log_indentation_up;

			-- COLLECT CLEARANCES
			-- net specific:
			clearances.append (class_given_net.clearance);

			-- fill zone specific:
			if fill_zone.observe then 
				clearances.append (fill_zone.outline.isolation);
			end if;

			-- choose the greatest clearance:
			greatest_clearance := get_greatest (clearances);

			-- Extend the radius of the circle_around_start_point by the clearance
			-- and compute the boundaries of the circle:
			circle_around_start_point.radius := circle_around_start_point.radius + greatest_clearance;
			start_point_boundaries := get_boundaries (circle_around_start_point, zero);


			-- Iterate texts. Abort if result changes to "false":
			while t /= pac_conductor_texts.no_element and result = true loop
				
				-- CS log text properties
				
				if element (t).layer = layer then

					-- Preselection to improve performance:
					-- We are interested in texts whose boundaries enclose
					-- the boundaries of the given start point. If there is 
					-- no overlap then the text can be skipped:					
					text_boundaries := pac_text_fab.get_boundaries (element (t).vectors);
					
					if intersect (start_point_boundaries, text_boundaries) then
						
						query_element (
							position	=> t,
							process		=> query_vectors'access);

					end if;
				end if;
				
				next (t);
			end loop;
			
			log_indentation_down;
		end query_texts;

		
	begin -- query_module
		result := true;
		
		if fill_zone.observe then 
			query_fill_zone;
		end if;

		if result = true then
			query_global_cutouts;
		end if;
		
		-- - net specific cutout areas
		
		-- CS abort if status is invalid.
		
		-- cs pads, ...

		if result = true then
			query_tracks;
		end if;

		if result = true then
			query_texts;
		end if;

		
		-- CS query freetracks, route restrict
		
	end query_module;

	distance_to_edge : type_distance;
	
begin -- clear_for_track
	log (text => "----CLEAR FOR TRACK QUERY BEGIN-----", level => lth);
	log (text => "probing whether point" & to_string (start_point) 
			& " qualifies to start a track of width" & to_string (width)
			& " in layer " & to_string (layer),
			level => lth);

	-- CS write the net name or "freetrack" in log message
	
	log_indentation_up;

	-- The first an basic test is to figure out whether the point is on
	-- board and not inside a hole. If both conditions are true, then
	-- other objects will be probed:
	if on_board (module_cursor, start_point, lth + 1) then

		-- the distance of the point to the board edge (incl. holes):
		distance_to_edge := get_absolute (
			get_distance_to_edge (module_cursor, start_point, lth + 1));

		log (text => " distance point to board edge:" & to_string (distance_to_edge),
			level => lth + 1);
		
		-- the distance of the conductor to the board edge:
		distance_to_edge := distance_to_edge - 0.5 * width;

		log (text => " distance conductor to board edge:" & to_string (distance_to_edge),
			level => lth + 1);

		if distance_to_edge >= design_rules.clearances.conductor_to_board_edge then
			log (text => " point is in safe distance to board edge", level => lth + 1);

			-- probe other objects:
			query_element (module_cursor, query_module'access);
		else
			log (text => " point is too close to board edge", level => lth + 1);			
		end if;
			
	else
		log (text => " point is not in board area", level => lth + 1);			
	end if;

	
	if result = TRUE then
		log (text => "point accepted", level => lth);
	else
		log (text => "point not accepted", level => lth);
	end if;
	
	log_indentation_down;

	log (text => "----CLEAR FOR TRACK QUERY END-----", level => lth);
	
	return result;
end clear_for_track;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
