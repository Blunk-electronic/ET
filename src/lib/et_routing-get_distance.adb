------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         ROUTING.GET_DISTANCE                             --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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
	return type_route_distance
is
	probe_ray : constant type_ray := (to_vector (start_point), direction);
	probe_line : constant type_line_vector := to_line_vector (probe_ray);

	-- The top conductor layer 1 is always there:
	top_layer : constant type_signal_layer := type_signal_layer'first;

	
	function is_inner_layer (layer : in type_signal_layer) return boolean is begin
		if layer > top_layer and layer < bottom_layer then
			return true;
		else
			return false;
		end if;
	end is_inner_layer;		



	-- The basic set of clearances contains
	-- the polygon isolation and the clearance of the given net.
	-- Some procedure may extend this set by other clearances (in their own local sets).
	-- The greatest clearance them will be applied to the track clearance.
	clearances_basic : pac_distances_positive.list;

	
	
	track : type_track := (
		center		=> probe_line,
		width		=> width,
		others		=> <>); -- means clearance to other objects

	track_dimensions : type_track_dimensions;
	-- NOTE: Requires to be updated after changing the clearance of the track.


	-- Applies from the given clearances the greatest clearance to the track:
	procedure apply_greatest_clearance_to_track (
		clearances : in pac_distances_positive.list)
	is begin
		track.clearance	:= get_greatest (clearances);
		track_dimensions := get_dimensions (track);
	end;

	
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
	procedure test_line (l : in type_line) is -- CS pass log threshhold
		b : constant type_break := 
			get_break_by_line (track, track_dimensions, l, place, log_category, lth + 2);
	begin
		--log (text => "test line");
		
		if b.exists then
			process_break (b.point);
		end if;
	end test_line;

	
	-- See procedure test_line for details.
	procedure test_arc (a : in type_arc) is -- CS pass log threshhold
		b : constant type_break_double := 
			get_break_by_arc (track, track_dimensions, a, place, log_category, lth + 2);
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
	procedure test_circle (c : in type_circle) is  -- CS pass log threshhold
		b : constant type_break_double := 
			get_break_by_circle (track, track_dimensions, c, place, log_category, lth + 2);
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
		use pac_distances_sorting;
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
			if log_category >= HIGH then
				log (text => "probing outline ...", level => lth + 1);
				log_indentation_up;
			end if;
			
			if module.board.contours.outline.contours.circular then
				test_circle (module.board.contours.outline.contours.circle);
			else
				iterate (module.board.contours.outline.contours.segments, query_segment'access);
			end if;

			if log_category >= HIGH then
				log_indentation_down;
			end if;
		end query_outline;

		
		-- holes
		procedure query_holes is
			use et_packages;			
			use pac_pcb_cutouts;

			procedure query_hole (c : in pac_pcb_cutouts.cursor) is begin

				if log_category >= HIGH then
					log_indentation_up;
				end if;
				
				if element (c).contours.circular then
					--log (text => "circular hole");
					test_circle (element (c).contours.circle);
				else		
					--log (text => "n-shaped hole");
					iterate (element (c).contours.segments, query_segment'access);
				end if;

				if log_category >= HIGH then
					log_indentation_down;
				end if;
			end query_hole;
			
		begin -- query_holes
			if log_category >= HIGH then
				log (text => "probing holes ...", level => lth + 1);
			end if;
			
			iterate (module.board.contours.holes, query_hole'access);
		end query_holes;

		
		-- CONDUCTOR FILL ZONES
		procedure query_fill_zone is begin

			if log_category >= HIGH then
				log (text => "probing fill zone ...", level => lth + 1);
				log_indentation_up;
			end if;
			
			if fill_zone.outline.contours.circular then
				test_circle (fill_zone.outline.contours.circle);
			else
				iterate (fill_zone.outline.contours.segments, query_segment'access);
			end if;

			if log_category >= HIGH then
				log_indentation_down;
			end if;
		end query_fill_zone;


		-- GLOBAL CUTOUT AREAS
		procedure query_global_cutouts is 
			use boards;
			use pac_conductor_cutouts;
			
			procedure query_cutout (c : in pac_conductor_cutouts.cursor) is begin
				if element (c).layer = layer then

					if log_category >= HIGH then
						log_indentation_up;
					end if;
						
					if element (c).contours.circular then
						test_circle (element (c).contours.circle);
					else		
						iterate (element (c).contours.segments, query_segment'access);
					end if;

					if log_category >= HIGH then
						log_indentation_down;
					end if;
				end if;
			end query_cutout;
		
		begin -- query_global_cutouts
			if log_category >= HIGH then
				log (text => "probing global cutout areas ...", level => lth + 1);
			end if;
			
			iterate (module.board.conductors.cutouts, query_cutout'access);
		end query_global_cutouts;


		-- TRACKS
		procedure query_tracks is
			use et_schematic;
			use pac_nets;

			-- Queries the conductor segments of foregin nets.
			procedure query_net (nf : in pac_nets.cursor) is
				use pac_net_name;
				use pac_conductor_lines;
				use pac_conductor_arcs;

				class_foregin_net : constant type_net_class := get_net_class (module_cursor, nf);

				-- The clearance to foregin nets is the greatest of several different distances.
				-- The greatest of them will later be applied to the track clearance.
				-- We start with the set of basic clearances in this local set:
				clearances : pac_distances_positive.list := clearances_basic;

				procedure query_line (c : in pac_conductor_lines.cursor) is
					segment : et_conductor_segment.type_conductor_line_segment;
					use et_conductor_segment;
				begin
					if element (c).layer = layer then
						
						if log_category >= HIGH then
							log (text => "segment " & to_string (element (c)), level => lth + 3);
							log_indentation_up;
						end if;
						
						segment := to_line_segment (element (c));

						test_line (get_left_edge (segment));
						test_line (get_right_edge (segment));
						test_arc (get_start_cap (segment));
						test_arc (get_end_cap (segment));

						-- CS procedure test_segment_line (segment)

						if log_category >= HIGH then
							log_indentation_down;
						end if;
					end if;
				end query_line;

				
				procedure query_arc (c : in pac_conductor_arcs.cursor) is
					segment : et_conductor_segment.type_conductor_arc_segment;
					use et_conductor_segment;
				begin
					if element (c).layer = layer then
						
						if log_category >= HIGH then
							log (text => "segment " & to_string (element (c)), level => lth + 3);
							log_indentation_up;
						end if;
						
						segment := to_arc_segment (element (c));

						test_arc (get_outer_edge (segment));
						test_arc (get_end_cap (segment));
						test_arc (get_inner_edge (segment));
						test_arc (get_end_cap (segment));

						if log_category >= HIGH then
							log_indentation_down;
						end if;
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
							if log_category >= HIGH then
								log (text => to_string (element (v)), level => lth + 3);
							end if;

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

					-- Add the clearance of the foregin net:
					clearances.append (class_foregin_net.clearance);

					apply_greatest_clearance_to_track (clearances);

					if log_category >= HIGH then
						log_indentation_up;
					end if;
					
					iterate (element (nf).route.lines, query_line'access);
					iterate (element (nf).route.arcs, query_arc'access);
					iterate (element (nf).route.vias, query_via'access);
					
					-- CS other objects ... see et_pcb.type_route

					if log_category >= HIGH then
						log_indentation_down;
					end if;
				end query_segments_and_vias;

				
			begin -- query_net
				if log_category >= HIGH then
					log (text => "net " & to_string (key (nf)), level => lth + 2);
				end if;

				if ignore_same_net then
					if net_cursor /= nf then
						query_segments_and_vias;
					end if;
				else
					query_segments_and_vias;
				end if;
			end query_net;

			
		begin -- query_tracks
			if log_category >= HIGH then
				log (text => "probing tracks ...", level => lth + 1);
				log_indentation_up;
			end if;
			
			iterate (module.nets, query_net'access);

			if log_category >= HIGH then
				log_indentation_down;
			end if;
		end query_tracks;


		procedure query_segment (
			s : in et_conductor_text.boards.pac_conductor_line_segments.cursor) 
		is 
			use et_conductor_text.boards.pac_conductor_line_segments;
			use et_conductor_segment;
		begin
			--log_indentation_up;
			--log (text => "text " & to_string (element (l)), level => lth + 3);

			-- Now we treat the line of the vector text like a regular
			-- line of conductor material:
			test_line (get_left_edge (element (s)));
			test_line (get_right_edge (element (s)));

			test_arc (get_start_cap (element (s)));
			test_arc (get_end_cap (element (s)));
			
			-- CS procedure test_segment_line (segment)

			--log_indentation_down;
		end query_segment;

		
		procedure query_texts is
			use et_conductor_text.boards;
			use pac_conductor_texts;
			use et_text;

			boundaries_track : type_boundaries;
			
			procedure query_text (c : in pac_conductor_texts.cursor) is
				use pac_conductor_line_segments;

				-- The boundaries of the candidate text:
				boundaries_text : constant type_boundaries := 
					pac_text_fab.get_boundaries (element (c).vectors);
				
			begin -- query_text
				if log_category >= HIGH then
					log (text => "text:" 
						& " P:" & to_string (element (c).position)
						& " / L: " & to_string (element (c).layer)
						& " / C: " & enclose_in_quotes (to_string (element (c).content)),
						level => lth + 2);
				end if;
				
				if element (c).layer = layer then

					-- Preselection to improve performance:
					-- We are interested in texts whose boundaries overlap
					-- those of the track. If there is no overlap then
					-- the text can be skipped:
					if intersect (boundaries_track, boundaries_text) then
						iterate (element (c).segments, query_segment'access);
					end if;
				end if;
			end query_text;

		begin
			if log_category >= HIGH then
				log (text => "probing vector texts ...", level => lth + 1);
				log_indentation_up;
			end if;

			apply_greatest_clearance_to_track (clearances_basic);

			boundaries_track := track_dimensions.boundaries;
			move_by (boundaries_track, track_dimensions.offset, true);
			
			iterate (module.board.conductors.texts, query_text'access);

			if log_category >= HIGH then
				log_indentation_down;
			end if;
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

			boundaries_track : type_boundaries;

			device_cursor : pac_devices_sch.cursor;

			
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
							
							if intersect (boundaries_track, get_boundaries (v_text)) then
								segments := make_segments (v_text, t.line_width);

								pac_conductor_line_segments.iterate (segments, query_segment'access);
							end if;
						end query_segments;
			
					begin -- query_text
						-- CS log text content and position
						--log (text => "text " & enclose_in_quotes (to_string (element (c).content)), level => lth + 4);
						
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
	
					
				begin
					if not is_inner_layer (layer) then
						apply_greatest_clearance_to_track (clearances_basic);

						boundaries_track := track_dimensions.boundaries;
						move_by (boundaries_track, track_dimensions.offset, true);

						query_face := TOP;
						element (package_cursor).conductors.top.texts.iterate (query_text'access);
						query_face := BOTTOM;
						element (package_cursor).conductors.bottom.texts.iterate (query_text'access);
					end if;
				end query_texts;


				procedure query_terminals is
					use pac_terminals;
					use pac_polygon_offsetting;

					procedure query_terminal (c : in pac_terminals.cursor) is
						use et_schematic_ops;

						procedure move_outline_smt is 
							position : type_position := element (c).position;
							oln : type_polygon;
						begin
							oln := element (c).pad_shape_smt;
							
							move_contours (
								term_pos	=> position,
								outline		=> oln,
								flipped		=> package_flipped,
								package_pos	=> package_position);
							
								
							if oln.contours.circular then
								test_circle (oln.contours.circle);
							else
								iterate (oln.contours.segments, query_segment'access);
							end if;
						end move_outline_smt;


						procedure move_outline_tht is 
							position : type_position := element (c).position;
							oln : type_polygon;

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
												offset		=> element (c).width_inner_layers);
											
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
															
							if oln.contours.circular then
								test_circle (oln.contours.circle);
							else
								iterate (oln.contours.segments, query_segment'access);
							end if;
						end move_outline_tht;

						status : type_get_terminal_clearance_result;
						
					begin -- query_terminal
						if log_category >= HIGH then
							log (text => "terminal " & to_string (key (c)), level => lth + 4);
						end if;
						
						if observe_foreign_nets then
							
							-- Get the clearance of the connected foreign net
							-- and append it to clearances:
							status := get_clearance (module_cursor, device_cursor, c);

							if status.connected then
								declare
									clearances : pac_distances_positive.list := clearances_basic;
								begin								
									if log_category >= HIGH then
										log (text => "clearance foregin net " 
												& to_string (status.clearance),
											level => lth + 5);
									end if;
									
									clearances.append (status.clearance);
									apply_greatest_clearance_to_track (clearances);
								end;
							else
								if log_category >= HIGH then
									log (text => "not connected", level => lth + 5);
								end if;
								
								apply_greatest_clearance_to_track (clearances_basic);
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
					
				begin
					apply_greatest_clearance_to_track (clearances_basic);
					
					-- Iterate through the terminals of the package:
					iterate (element (package_cursor).terminals, query_terminal'access);
				end query_terminals;

				
			begin
				query_terminals;
				query_texts;	
				
				-- CS conductors
				-- CS route/via restrict
				-- CS holes
			end query_package;

			
			procedure query_device (c : in pac_devices_sch.cursor) is begin
				if log_category >= HIGH then
					log (text => "device " & to_string (key (c)), level => lth + 2);
					log_indentation_up;
				end if;

				if is_real (c) then
					model := get_package_model (c);

					if log_category >= HIGH then
						log (text => "model " & to_string (model), level => lth + 3);
					end if;
					
					-- locate the package model in the package library:
					package_cursor := locate_package_model (model);

					package_position := element (c).position;
					package_flipped := element (c).flipped;

					if log_category >= HIGH then
						log_indentation_up;
					end if;
					
					device_cursor := c;
					query_package (observe_foreign_nets => true);

					if log_category >= HIGH then
						log_indentation_down;
					end if;
				end if;

				if log_category >= HIGH then
					log_indentation_down;
				end if;
			end query_device;

			
			use pac_devices_non_electric;
			
			procedure query_device (c : in pac_devices_non_electric.cursor) is begin

				if log_category >= HIGH then
					log (text => "device " & to_string (key (c)), level => lth + 2);
					log_indentation_up;
				end if;

				model := element (c).package_model;

				if log_category >= HIGH then
					log (text => "model " & to_string (model), level => lth + 3);
				end if;
				
				-- locate the package model in the package library:
				package_cursor := locate_package_model (model);

				package_position := element (c).position;
				package_flipped := element (c).flipped;

				if log_category >= HIGH then
					log_indentation_up;
				end if;
				
				query_package (observe_foreign_nets => false);

				if log_category >= HIGH then
					log_indentation_down;
					log_indentation_down;
				end if;
			end query_device;

			
		begin
			if log_category >= HIGH then
				log (text => "probing devices ...", level => lth + 1);
				log_indentation_up;
			end if;
			
			-- probe electrical devices:
			iterate (module.devices, query_device'access);

			-- probe non-electric devices
			iterate (module.devices_non_electric, query_device'access);

			if log_category >= HIGH then
				log_indentation_down;
			end if;
		end query_devices;
		
			
	begin -- query_obstacles

		track.clearance	:= design_rules.clearances.conductor_to_board_edge;
		track_dimensions := get_dimensions (track);
		
		-- board contours:
		query_outline;
		query_holes;

		
		-- next step is querying fill zones. There the clearance to the
		-- zone borders is zero. The track is to approach the border as
		-- close as possible (from inside the area):
		track.clearance := zero;
		track_dimensions := get_dimensions (track);
		
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
		
		query_devices;
		
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
				module_cursor, design_rules, bottom_layer, element (c), net_cursor, 
				net_class, fill_zone, layer, width, ignore_same_net, log_category, lth + 1) 
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

	
	-- Setup the list of basic clearances:
	clearances_basic.append (net_class.clearance);

	if fill_zone.observe then 
		clearances_basic.append (fill_zone.outline.isolation);
	end if;


	
	
	case place is
		when BEFORE =>
			if log_category >= HIGH then
				log (text => "computing distance to obstacle from point" 
						& to_string (start_point)
						& " direction" & to_string (direction) & " ...",
						level => lth);
			
				log_indentation_up;
			end if;
			
			-- Test whether start_point is suitable to start a track.
			-- At the given start_point or in its vicinity could be an obstacle already.
			if clear_for_track (module_cursor, design_rules, bottom_layer, start_point, 
				net_cursor, net_class, fill_zone, layer, width, ignore_same_net, log_category, lth + 1) 
			then
				-- start_point qualifies to start a track

				-- Probe everything on the board that could be an obstacle
				-- for the track in the given direction, net, layer. The distance to 
				-- the nearest obstacle (to the right of start_point) will finally be
				-- stored in variable distance_to_obstacle:
				query_element (module_cursor, query_obstacles'access);

				if log_category >= HIGH then
					log (text => "distance to obstacle:" & to_string (distance_to_obstacle),
						level => lth);
				
					log_indentation_down;
				end if;
				
				return (VALID, distance_to_obstacle);
				
			else 
				-- start_point does NOT qualify to start a track

				if log_category >= HIGH then
					log (text => "track not allowed here",
						level => lth);

					log_indentation_down;
				end if;
				
				return (status => INVALID);
			end if;


			
		when AFTER =>
			if log_category >= HIGH then
				log (text => "computing distance until after obstacles from point" 
					& to_string (start_point)
					& " direction" & to_string (direction) & " ...",
					level => lth);

				log_indentation_up;
			end if;

			-- There can be more than one obstacle. They may overlap in some way.
			-- So a point must be found after a cluster of obstacles where 
			-- it is allowed to start a track:
			find_valid_point_after_obstacles;

			-- Now the variable "status" indicates whether a valid point has 
			-- been found or not.
			
			case status is
				when VALID => -- suitable point found

					if log_category >= HIGH then
						log (text => "distance until after obstacle:" & to_string (distance_after_obstacle),
							level => lth);
					
						log_indentation_down;
					end if;

					return (VALID, distance_after_obstacle);

					
				when INVALID => -- NO suitable point found 

					if log_category >= HIGH then
						log (text => "no obstacle found", level => lth);
					
						log_indentation_down;
					end if;

					return (status => INVALID);
			end case;

			
	end case;

end get_distance;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
