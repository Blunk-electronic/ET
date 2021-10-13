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

with et_text;

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
	result : aliased boolean := false;

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


	greatest_clearance : type_distance_positive;
	
	
	-- Clears the "result" flag if distance is:
	-- - negative or
	-- - less than the currently greatest_clearance
	procedure test_distance (
		distance	: in type_distance;
		lth			: in type_log_level)
	is 
		d : type_distance := distance;
	begin
		--log_indentation_up;
		
		if distance <= zero then 
			-- start_point is inside segment/via or on the edge of the segment/via
			log (text => " point is inside", level => lth + 1);
			result := false;
		else
			-- start_point is outside the segment/via
			log (text => " point is outside", level => lth + 1);
			
			-- the distance of the start point to the border of the segment/via:
			d := distance - width * 0.5;

			if d >= greatest_clearance then
				log (text => " point is in safe distance", level => lth + 1);
			else
				log (text => " point is too close", level => lth + 1);
				result := false;
			end if;							
		end if;

		--log_indentation_down;
	end test_distance;


	
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

					use pac_conductor_lines;
					
					procedure query_line (c : in pac_conductor_lines.cursor) is
						segment_line : et_conductor_segment.type_conductor_line_segment;
					begin
						if element (c).layer = layer then
							segment_line := to_line_segment (element (c));
							log (text => et_conductor_segment.to_string (segment_line), level => lth + 3);
							distance := et_conductor_segment.get_shortest_distance (start_point, segment_line);
							test_distance (distance, lth + 4);
						end if;
					end query_line;

					
					use pac_conductor_arcs;					

					procedure query_arc (c : pac_conductor_arcs.cursor) is 
						segment_arc : et_conductor_segment.type_conductor_arc_segment;
					begin
						if element (c).layer = layer then
							segment_arc := to_arc_segment (element (c));
							log (text => et_conductor_segment.to_string (segment_arc), level => lth + 3);
							distance := et_conductor_segment.get_shortest_distance (start_point, segment_arc);
							test_distance (distance, lth + 4);
						end if;
					end query_arc;


					use et_vias;
					use pac_vias;
					
					procedure query_via (v : in pac_vias.cursor) is
						c : type_circle;

						procedure set_radius (restring : in type_restring_width) is begin
							c.radius := element (v).diameter * 0.5 + restring;

							if get_point_to_circle_status (start_point, c) = OUTSIDE then
								distance := get_absolute (get_shortest_distance (start_point, c));
								test_distance (distance, lth + 4);
							else
								-- the start_point is inside the via
								result := false;
								log (text => " point is inside the via", level => lth + 4);
							end if;							
						end set_radius;
						
					begin -- query_via
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
					end query_via;

					
				begin -- query_segments_and_vias
					log_indentation_up;
					
					clearances.append (class_given_net.clearance);
					clearances.append (class_foregin_net.clearance);

					if fill_zone.observe then 
						clearances.append (fill_zone.outline.isolation);
					end if;

					greatest_clearance := get_greatest (clearances);
					
					iterate (
						lines	=> net.route.lines,
						process	=> query_line'access,
						proceed	=> result'access);

					iterate (
						arcs	=> net.route.arcs,
						process	=> query_arc'access,
						proceed	=> result'access);
					
					iterate (
						vias	=> net.route.vias,
						process	=> query_via'access,
						proceed	=> result'access);
				
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
			use et_conductor_text.boards;
			use pac_conductor_texts;

			-- There can be many clearances which must be taken into account.
			-- The greatest among them will be relevant:
			clearances : pac_distances_positive.list;
			

			procedure query_segment (
				c : in pac_conductor_line_segments.cursor)
			is 
				use pac_conductor_line_segments;
				distance : type_distance;
			begin
				log (text => et_conductor_segment.to_string (element (c)), level => lth + 3);
				
				-- Now we treat the line of the text like a regular
				-- line of conductor material:
				distance := et_conductor_segment.get_shortest_distance (start_point, element (c));
				test_distance (distance, lth + 4);
			end query_segment;
		

			procedure query_text (c : in pac_conductor_texts.cursor) is
				text_boundaries : type_boundaries;
				use et_text;
			begin
				if element (c).layer = layer then

					-- Preselection to improve performance:
					-- We are interested in texts whose boundaries enclose
					-- the boundaries of the given start point. If there is 
					-- no overlap then the text can be skipped:					
					text_boundaries := pac_text_fab.get_boundaries (element (c).vectors);
					
					if intersect (start_point_boundaries, text_boundaries) then

						log (text => "overlaps boundaries of text " 
								& enclose_in_quotes (to_string (element (c).content)) 
								& " at" & to_string (element (c).position),
								level => lth + 2);

						log_indentation_up;
						
						-- Probe the segments one by one.
						-- Abort once the result-flag goes false.
						iterate (
							segments	=> element (c).segments, 
							process		=> query_segment'access,
							proceed		=> result'access);

						log_indentation_down;
					end if;
				end if;
			end query_text;


			-- Take a copy of the initial circle_around_start_point_init:
			circle_around_start_point : type_circle := circle_around_start_point_init;

			
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
			iterate (
				texts	=> module.board.conductors.texts,
				process	=> query_text'access,
				proceed	=> result'access);
			
			log_indentation_down;
		end query_texts;


		procedure query_devices is
			use et_devices;
			use et_schematic;
			use pac_devices_sch;

			use et_packages;
			use pac_packages_lib;
			model : pac_package_model_file_name.bounded_string;
			package_cursor		: pac_packages_lib.cursor;
			package_position	: type_package_position; -- incl. rotation and face
			package_flipped		: type_flipped;

			procedure query_package (
				observe_foreign_nets : in type_observe_foreign_nets := FALSE) 
			is
				procedure query_texts is 
					use et_conductor_text;
					
					query_face : type_face;
					clearances : pac_distances_positive.list;
					
					procedure query_text (c : in packages.pac_conductor_texts.cursor) is
						t : packages.type_conductor_text := packages.pac_conductor_texts.element (c);

						use et_text;
						mirror_status : type_vector_text_mirrored := NO;
						
						procedure query_segments is 
							use pac_text_fab;
							v_text : type_vector_text;

							use et_conductor_text.boards;
							segments: pac_conductor_line_segments.list;

							
							procedure query_segment (
								c : in pac_conductor_line_segments.cursor)
							is 
								use pac_conductor_line_segments;
								distance : type_distance;
							begin
								log (text => et_conductor_segment.to_string (element (c)), level => lth + 4);
								
								-- Now we treat the line of the text like a regular
								-- line of conductor material:
								distance := et_conductor_segment.get_shortest_distance (start_point, element (c));
								test_distance (distance, lth + 5);
							end query_segment;

							
						begin
							-- Rotate the position of the text by the rotation of the package.
							-- NOTE: This does not affect the rotation of the text itself.
							rotate_by (t.position, rot (package_position));

							if package_flipped = YES then mirror (t.position, Y); end if;

							-- Move the text by the package position to 
							-- its final position:
							move_by (t.position, to_distance_relative (package_position));

							-- Vectorize the content of the text on the fly:
							v_text := pac_text_fab.vectorize_text (
								content		=> t.content,
								size		=> t.size,
								rotation	=> add (rot (t.position), rot (package_position)),
								position	=> type_point (t.position),
								mirror		=> mirror_status,
								line_width	=> t.line_width,
								alignment	=> t.alignment -- right, bottom
								);

							-- If boundaries of start point and text overlap,
							-- compute the conductor segments and probe each of them
							-- for its distance to the start point: 
							if intersect (start_point_boundaries, get_boundaries (v_text)) then
								log (text => "overlaps boundaries of text " 
										& enclose_in_quotes (to_string (t.content)) 
										& " at" & to_string (t.position),
									 level => lth + 3);
								
								segments := make_segments (v_text, t.line_width);

								log_indentation_up;

								-- Probe the segments one by one.
								-- Abort once the result-flag goes false.
								iterate (
									segments	=> segments, 
									process		=> query_segment'access,
									proceed		=> result'access);

								log_indentation_down;
							end if;

						end query_segments;
					
					begin -- query_text
						case query_face is
							when TOP =>
								if package_flipped = NO then 
									if layer = top_layer then
										query_segments;
									end if;

								else
									if layer = bottom_layer then
										mirror_status := YES;
										query_segments;
									end if;
								end if;
								
							when BOTTOM =>
								if package_flipped = NO then
									if layer = bottom_layer then
										mirror_status := YES;
										query_segments;
									end if;

								else
									if layer = top_layer then
										query_segments;
									end if;
								end if;

						end case;								

					end query_text;

					-- Take a copy of the initial circle_around_start_point_init:
					circle_around_start_point : type_circle := circle_around_start_point_init;
					
				begin -- query_texts
					if not is_inner_layer (layer) then
						log_indentation_up;
						
						-- COLLECT CLEARANCES
						-- net specific:
						clearances.append (class_given_net.clearance);

						-- fill zone specific:
						if fill_zone.observe then 
							clearances.append (fill_zone.outline.isolation);
						end if;

						greatest_clearance := get_greatest (clearances);
						
						-- Extend the radius of the circle_around_start_point by the
						-- greatest clearance and compute the boundaries of the circle:
						circle_around_start_point.radius := circle_around_start_point.radius + greatest_clearance;
						start_point_boundaries := get_boundaries (circle_around_start_point, zero);

						
						query_face := TOP;

						packages.iterate (
							texts	=> element (package_cursor).conductors.top.texts,
							process	=> query_text'access,
							proceed	=> result'access);
								 
						query_face := BOTTOM;
						
						packages.iterate (
							texts	=> element (package_cursor).conductors.bottom.texts,
							process	=> query_text'access,
							proceed	=> result'access);

						log_indentation_down;
					end if;
				end query_texts;

			begin
				query_texts;			
				-- CS terminals + net

				-- CS conductors
				-- CS route/via restrict
				-- CS holes
			end query_package;

			
			procedure query_device (c : in pac_devices_sch.cursor) is begin
				if is_real (c) then
					log (text => "device " & to_string (key (c)), level => lth + 2);
					log_indentation_up;

					model := get_package_model (c);
					log (text => "model " & to_string (model), level => lth + 3);
					
					-- locate the package model in the package library:
					package_cursor := locate_package_model (model);

					package_position := element (c).position;
					package_flipped := element (c).flipped;
					
					query_package (observe_foreign_nets => true);
					
					log_indentation_down;					
				end if;
			end query_device;

			
			use pac_devices_non_electric;
			
			procedure query_device (c : in pac_devices_non_electric.cursor) is begin
				log (text => "device " & to_string (key (c)), level => lth + 2);
				log_indentation_up;

				model := element (c).package_model;
				log (text => "model " & to_string (model), level => lth + 3);

				-- locate the package model in the package library:
				package_cursor := locate_package_model (model);

				package_position := element (c).position;
				package_flipped := element (c).flipped;
				
				query_package (observe_foreign_nets => false);
				
				log_indentation_down;
			end query_device;

			
		begin
			log (text => "probing devices ...", level => lth + 1);
			log_indentation_up;

			-- probe electrical devices. abort when result is false:
			iterate (
				devices	=> module.devices,
				process => query_device'access,
				proceed => result'access);

			-- probe non-electric devices
			iterate (
				devices => module.devices_non_electric,
				process => query_device'access,
				proceed	=> result'access);
			
			log_indentation_down;
		end query_devices;


		
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

		if result = true then
			query_devices;
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
