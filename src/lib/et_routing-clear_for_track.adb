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
			use et_conductor_polygons.pac_conductor_cutouts;

			procedure query_cutout (c : in et_conductor_polygons.pac_conductor_cutouts.cursor) is 
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
						segment_line : et_packages.type_conductor_line_segment;
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
						segment_arc : et_packages.type_conductor_arc_segment;
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
			use et_packages;
			use pac_conductor_texts;
			t : pac_conductor_texts.cursor := module.board.conductors.texts.first;

			distance : type_distance;
			clearances : pac_distances_positive.list;

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
					
					if distance >= get_greatest (clearances) then
						log (text => "point is in safe distance", level => lth + 4);
					else
						log (text => "point is too close", level => lth + 4);
						result := false;
					end if;							
				end if;

				log_indentation_down;
			end test_distance;

			use et_board_shapes_and_text.pac_text_fab;
			use pac_vector_text_lines;
			vector_text : pac_vector_text_lines.list;

			procedure query_lines is
				vl : pac_vector_text_lines.cursor := vector_text.first;
				cl : et_packages.type_conductor_line;
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
			end query_lines;
			
		begin -- query_texts
			log (text => "probing texts ...", level => lth + 1);
			log_indentation_up;

			clearances.append (class_given_net.clearance);

			if fill_zone.observe then 
				clearances.append (fill_zone.outline.isolation);
			end if;

			
			while t /= pac_conductor_texts.no_element and result = true loop
				
				-- CS log text properties
				
				if element (t).layer = layer then
					--log_indentation_up;

					-- Vectorize the text:
					vector_text := vectorize_text (
						content		=> element (t).content,
						size		=> element (t).size,
						rotation	=> rot (element (t).position),
						position	=> type_point (element (t).position),

						-- Mirror the text only if it is in the bottom layer:
						mirror		=> signal_layer_to_mirror (element (t).layer, bottom_layer),
						
						line_width	=> element (t).line_width,
						alignment	=> element (t).alignment -- right, bottom
						);

					query_lines;

					--log_indentation_down;
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