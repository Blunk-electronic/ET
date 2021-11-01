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
with et_schematic.device_query_ops;	use et_schematic.device_query_ops;

separate (et_routing)

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
	return boolean
is
	result : aliased boolean := false;


	-- The top conductor layer 1 is always there:
	top_layer : constant type_signal_layer := type_signal_layer'first;

	
	function is_inner_layer (layer : in type_signal_layer) return boolean is begin
		if layer > top_layer and layer < bottom_layer then
			return true;
		else
			return false;
		end if;
	end is_inner_layer;		

	
	
	-- For some preselections (to improve performance) we will test if boundaries of
	-- objects overlap with the boundaries of the given start point. The start point
	-- is the center of a circle that has half the width of the inquired track.
	-- This is the inital circle around the start point.
	circle_around_start_point_init : constant type_circle := (start_point, width * 0.5);

	-- In the course of this function copies of circle_around_start_point_init
	-- are taken here their radius extended by the particular clearance being 
	-- effective temporarily:
	circle_around_start_point : type_circle;

	
	-- The boundaries of the circle around the start point:
	start_point_boundaries : type_boundaries;

	
	-- The basic set of clearances contains
	-- the polygon isolation and the clearance of the given net.
	-- Some procedure may extend this set by other clearances (in their own local sets).
	-- The greatest clearance them will be applied to the track clearance.
	clearances_basic : pac_distances_positive.list;
	

	greatest_clearance : type_distance_positive;

	
	-- Extends the radius of the circle_around_start_point by the
	-- greatest clearance and updates the boundaries of the circle:
	procedure extend_circle is begin
		circle_around_start_point.radius := circle_around_start_point.radius + greatest_clearance;
		start_point_boundaries := get_boundaries (circle_around_start_point, zero);
	end extend_circle;


	
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

				clearances : pac_distances_positive.list := clearances_basic;

				
				procedure query_segments_and_vias is 
					distance : type_distance;
				
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

				-- Append the clearance of the foregin net and
				-- select the greatest among the list of clearances:
				clearances.append (class_foregin_net.clearance);
				greatest_clearance := get_greatest (clearances);

				
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

			
		begin -- query_texts
			log (text => "probing texts ...", level => lth + 1);
			log_indentation_up;

			-- Take a copy of the initial circle_around_start_point_init:
			circle_around_start_point := circle_around_start_point_init;

			
			-- choose the greatest clearance:
			greatest_clearance := get_greatest (clearances_basic);

			extend_circle;

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
			
			device_cursor : pac_devices_sch.cursor; -- used for querying electrical devices
			
			procedure query_package (
				observe_foreign_nets : in type_observe_foreign_nets) 
			is
				procedure query_texts is 
					use et_conductor_text;
					
					query_face : type_face;
					
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
						-- CS log text content and position
						
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

					
				begin -- query_texts
					if not is_inner_layer (layer) then
						log_indentation_up;

						-- Take a copy of the initial circle_around_start_point_init:
						circle_around_start_point := circle_around_start_point_init;
						
						greatest_clearance := get_greatest (clearances_basic);

						extend_circle;
						
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


				procedure query_terminals is
					use et_terminals;

					procedure query_terminal (c : in pac_terminals.cursor) is
						use pac_terminals;

						
						procedure move_outline_smt is 
							position : type_position := element (c).position;
							oln : type_polygon;
							distance : type_distance;
						begin
							oln := element (c).pad_shape_smt;
							
							move_contours (
								term_pos	=> position,
								outline		=> oln,
								flipped		=> package_flipped,
								package_pos	=> package_position);
															
							if in_polygon_status (oln, start_point).status = OUTSIDE then
								distance := get_absolute (get_shortest_distance (oln, start_point));
								test_distance (distance, lth + 4);
							else
								result := false;
							end if;

						end move_outline_smt;


						procedure move_outline_tht is 
							position : type_position := element (c).position;
							oln : type_polygon;
							distance : type_distance;

							procedure inner_layer is begin
								case element (c).tht_hole is
									when DRILLED =>											
										declare
											s : type_polygon_segments := (circular => true, others => <>);
										begin
											s.circle.radius := element (c).drill_size * 0.5 + element (c).width_inner_layers;
											oln.contours := s;
										end;

									when MILLED =>
										declare
											om : type_polygon := type_polygon (element (c).millings);
										begin
											offset_polygon (
												polygon		=> om, 
												offset		=> (style => BY_DISTANCE, distance => element (c).width_inner_layers));
											
											oln := om;
										end;
								end case;
							end inner_layer;
							
							
						begin -- move_outline_tht
							if package_flipped = NO then
								if layer = top_layer then
									oln := element (c).pad_shape_tht.top;
								elsif layer = bottom_layer then
									oln := element (c).pad_shape_tht.bottom;
								else
									inner_layer;
								end if;

							else -- package has been flipped by operator
								if layer = top_layer then
									oln := element (c).pad_shape_tht.bottom;
								elsif layer = bottom_layer then
									oln := element (c).pad_shape_tht.top;
								else
									inner_layer;
								end if;

							end if;
								
							move_contours (
								term_pos	=> position,
								outline		=> oln,
								flipped		=> package_flipped,
								package_pos	=> package_position);

							if in_polygon_status (oln, start_point).status = OUTSIDE then
								distance := get_absolute (get_shortest_distance (oln, start_point));
								test_distance (distance, lth + 4);
							else
								result := false;
							end if;

						end move_outline_tht;


						status : type_get_terminal_clearance_result;
						
					begin -- query_terminal
						log (text => "terminal " & to_string (key (c)), level => lth + 4);

						if observe_foreign_nets then
							
							-- Get the clearance of the connected foreign net
							-- and append it to clearances:
							status := get_clearance (module_cursor, device_cursor, c);

							if status.connected then
								declare
									clearances : pac_distances_positive.list := clearances_basic;
								begin
									clearances.append (status.clearance);
									greatest_clearance := get_greatest (clearances);

									extend_circle;
								end;
							else
								greatest_clearance := get_greatest (clearances_basic);
								extend_circle;
							end if;
						end if;

						case element (c).technology is
							when THT =>
								move_outline_tht;

							when SMT =>
								if package_flipped = NO then
									case element (c).face is
										when TOP =>
											if layer = top_layer then
												move_outline_smt;
											end if;

										when BOTTOM =>
											if layer = bottom_layer then
												move_outline_smt;
											end if;
									end case;
								else
									case element (c).face is
										when TOP =>
											if layer = bottom_layer then
												move_outline_smt;
											end if;

										when BOTTOM =>
											if layer = top_layer then
												move_outline_smt;
											end if;
									end case;
								end if;
						end case;
						
					end query_terminal;


				begin -- query_terminals

					-- Take a copy of the initial circle_around_start_point_init:
					circle_around_start_point := circle_around_start_point_init;
					
					greatest_clearance := get_greatest (clearances_basic);

					extend_circle;

					iterate (
						terminals	=> element (package_cursor).terminals,
						process		=> query_terminal'access,
						proceed		=> result'access);
					
				end query_terminals;
				
			begin
				query_terminals;
				query_texts;

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

					log_indentation_up;
					device_cursor := c;
					query_package (observe_foreign_nets => true);
					log_indentation_down;
					
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

				log_indentation_up;
				query_package (observe_foreign_nets => false);
				log_indentation_down;
				
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


	-- Setup the list of basic clearances:
	clearances_basic.append (net_class.clearance);

	if fill_zone.observe then 
		clearances_basic.append (fill_zone.outline.isolation);
	end if;


	

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
