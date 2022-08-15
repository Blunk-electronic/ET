------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                  BOARD OPERATIONS / FILLING FILL ZONES                   --
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

with ada.strings;					use ada.strings;
with ada.exceptions;
with et_exceptions;					use et_exceptions;

with et_contour_to_polygon;			use et_contour_to_polygon;
with et_routing;					use et_routing;

separate (et_board_ops)

procedure fill_fill_zones (
	module_cursor	: in pac_generic_modules.cursor;
	log_category	: in type_log_category;
	log_threshold	: in type_log_level;
	nets 			: in pac_net_names.list := no_net_names)
is 
	use pac_geometry_brd;
	use pac_polygons;
	use pac_polygon_clipping;
	use pac_polygon_cropping;
	use pac_polygon_offsetting;
	
	use pac_net_names;

	all_zones : boolean;

	
	-- Get the design rules:
	design_rules : constant type_design_rules := get_pcb_design_rules (module_cursor);

	-- The deepest conductor layer towards bottom is defined by the layer stack:
	bottom_layer	: constant type_signal_layer := deepest_conductor_layer (module_cursor);


	use pac_islands;
	--use pac_cropped;
	--use pac_clipped;
	
	
	-- The outer contour of the board. After shrinking by the
	-- conductor-to-edge clearance this serves as master for
	-- filling zones of nets. Each net may have an individual setting for 
	-- the with of the fill lines.
	board_outer_contour_master : type_polygon;

	-- The holses inside the PCB area. After expanding by the
	-- conductor-to-edge clearance this serves as master for
	-- filling zones of nets. Each net may have an individual setting for 
	-- the with of the fill lines.
	use pac_polygon_list;
	board_holes_master : pac_polygon_list.list;


	offset_scratch : type_distance;
	
	
	procedure log_lower_left_corner (
		corner	: in type_point;
		lth		: in type_log_level) 
	is begin
		log (text => "lower left corner:" & to_string (corner),
			level => lth);
	end log_lower_left_corner;



	function to_polygons (
		net_cursor	: in pac_nets.cursor;
		layer 		: in type_signal_layer)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;
	begin

		return result;
	end to_polygons;
	
	
	
	-- Computes the horizontal fill lines required after given start point.
	-- Creates fill lines from the left to the right.
	-- Appends the fill lines to the polygon indicated by
	-- polygon cursor p:
	--function make_rows (
		--net_cursor		: in pac_nets.cursor;
		--net_class		: in type_net_class;
		--fill_zone		: in et_routing.type_fill_zone;
		--layer			: in type_signal_layer;
		--width			: in type_track_width; -- width of a fill line
		--height			: in type_distance_positive; -- of the polygon
		--start_point_in	: in type_point;
		--lth				: in type_log_level) 
		--return pac_rows.list
	--is

		---- Take a copy of the given start point because start point will
		---- change its position in the course of this procedure:
		--start_point : type_point := start_point_in;
		
		---- The fill lines to be returned. 
		---- Ordered from bottom to top and the left to the right:
		--use pac_rows;
		--result : pac_rows.list;

		
		---- a single row for temporarily use:
		--row : type_row;

		
		
		---- The number of rows in a rational number (like 45.7):
		--rows_rational : type_distance_positive;

		---- The minimal number of rows in a natural number (like 45):
		--rows_min : natural;

		---- If an extra row is required, this flag goes true;
		--extra_row : boolean := false;

		
		----The effective line width of a fill line is smaller than line_width
		----because the fill lines must overlap slightly:
		--effective_line_width : type_distance_positive;

		
		
		--procedure fill_row is
			--use pac_h_lines;

			--fill_line : type_line;
			
			--point : type_point := start_point;
			--row_y : type_position_axis := get_y (point);
			--status : type_valid;
			--distance : type_distance_positive;

			---- Queries the distance after a given point to the next
			---- obstacle. Updates variables "status" and "distance".
			---- If it sets status to INVALID then the given point is
			---- NOT allowed to start a fill line:
			--procedure get_distance_to_obstacle (start : in type_point) is 
				--d : constant type_route_distance := get_distance (
				--module_cursor	=> module_cursor,
				--design_rules	=> design_rules,
				--bottom_layer	=> bottom_layer,
				--start_point		=> start,
				--place			=> BEFORE,
				--direction		=> 0.0,
				--net_cursor		=> net_cursor,
				--net_class		=> net_class,
				--fill_zone		=> fill_zone,
				--layer			=> layer,
				--width			=> width,
				--ignore_same_net	=> true,
				--log_category	=> log_category,
				--lth				=> log_threshold + 3);
			--begin
				--status := d.status;

				--if d.status = VALID then
					--distance := d.distance;
				--end if;
			--end get_distance_to_obstacle;

			---- Queries the distance after a given point to the next place
			---- where it is allowed to start a fill line.
			---- Updates variables "status" and "distance":
			---- If it sets status to INVALID then NO place after the given point
			---- has been found to start a fill line:
			--procedure get_distance_after_obstacle (start : in type_point) is 
				--d : constant type_route_distance := get_distance (
				--module_cursor	=> module_cursor,
				--design_rules	=> design_rules,
				--bottom_layer	=> bottom_layer,
				--start_point		=> start,
				--place			=> AFTER,
				--direction		=> 0.0,
				--net_cursor		=> net_cursor,
				--net_class		=> net_class,
				--fill_zone		=> fill_zone,
				--layer			=> layer,
				--width			=> width,
				--log_category	=> log_category,
				--ignore_same_net	=> true,
				--lth				=> log_threshold + 3);
			--begin
				--status := d.status;

				--if d.status = VALID then
					--distance := d.distance;
				--end if;
			--end get_distance_after_obstacle;

			---- Safety measure to prevent infinite looping.
			---- CS: Increase maximum to reasonable value:
			--subtype type_line_count is positive range 1 .. 1000;
			
		--begin -- fill_row
			---- clean up the single temporarily row
			--row.lines.clear;
			
			--log_indentation_up;

			---- For the current row we compute fill line per fill line
			---- from the left to the right. This loop counts the
			---- fill lines per row. 
			---- Each iteration computes a single fill line.
			--for lc in 1.. type_line_count'last loop
				
				--log (text => "fill line" & positive'image (lc), level => log_threshold + 2);
				--log_indentation_up;
				
				--get_distance_after_obstacle (point);
				
				--if status = VALID then 
					---- there is a place to start another fill line

					---- move point to the place where the obstacle ends:
					--point := type_point (set (
						--x => get_x (point) + distance,
						--y => row_y));

					---- the fill line starts here:
					--fill_line.start_point := point;
					
					--get_distance_to_obstacle (point);

					--if distance = type_distance_positive'last then
						--raise constraint_error with 
						--"ERROR: No end point for fill line found !";
					--end if;

					
					--if status = VALID then -- point is allowed to start a line

						--point := type_point (set (
							--x => get_x (point) + distance,
							--y => row_y));

						---- the fill line ends here:
						--fill_line.end_point := point;
						
						---- append the just computed fill line to the row:
						--append (row.lines, fill_line);
					--end if;
					
				--else
					---- no place to start another fill line.
					---- Abort this row:
					--log_indentation_down;
					--exit;
				--end if;

				--log_indentation_down;
			--end loop;

			---- Row finished. Add it to the list of rows:
			--result.append (row);
			
			--log_indentation_down;
		--end fill_row;
		

		---- This is the offset required for the lower left corner:
		---- half of the minimal line widht to the right and up.
		---- This measure is required in order to let the fill lines start inside
		---- the polygon and not at the polygon edge:
		--offset : type_distance_relative;

		
	--begin -- make_rows

		---- Since the fill lines overlap slightly the effective
		---- line width is smaller than line_width. The effective_line_width
		---- is used to compute the number of fill lines:
		--effective_line_width := width * fill_line_overlap_factor;

		---- Compute the number of fill lines in a rational number (like 6.3).
		--rows_rational := type_distance_positive (height / effective_line_width);

		---- Compute the minimal number of fill lines in a natural number (like 6)
		--rows_min := natural (float'floor (float (rows_rational)));
		
		--log (text => "height:" & to_string (height) 
			--& " / line width:" & to_string (width)
			--& " / rows min:" & natural'image (rows_min),
			--level => log_threshold);

		----if rows_rational > type_distance_positive (rows_min) then
			----log (text => "extra row required", level => log_threshold + 2);
			----extra_row := true;
		----end if;


		
		---- The fill line runs horizontally from the left to the right
		---- edge of the polygon.
		---- - Move the start point so that the line starts at a
		----   virtual point outside (on the left) of the polygon.
		----   This way the line enters the polygon in any case with its full width.
		----   (The fill line has round caps at start and end point !).
		---- - Further-on move the start point up so that the lower edge
		----   of the fill line lies on the lower edge of the polygon:
		--offset := to_distance_relative (set (
				--x => - width * 0.5, -- to the left
				--y => + width * 0.5)); -- up

		--move_by (start_point, offset);

		---- Make the fill lines (bottom - up):
		--for r in 1 .. rows_min loop

			---- For the lowest fill line, the start point has already been
			---- computed. For each of the follwing lines the start point
			---- moves up by the effective line width (lines must overlap):
			--if r > 1 then
				--offset := to_distance_relative (set (
						--x => zero, -- no change
						--y => effective_line_width)); -- up
				
				--move_by (start_point, offset);
			--end if;

			--log (text => "row" & natural'image (r) 
					--& ": start" & to_string (start_point),
					--level => log_threshold + 1);
			
			--fill_row;					
		--end loop;

		---- If an extra row is required, then compute its start point starting
		---- with the upper left corner of the polygon.
		----if extra_row then

			----start_point := type_point (set (boundaries.smallest_x, boundaries.greatest_y));

			------ - Move the start point so that the line starts at a
			------   virtual point outside (on the left) of the polygon.
			------   This way the line enters the polygon in any case with its full width.
			------   (The fill line has round caps at start and end point !).
			------ - Further-on move the start point down so that the upper edge
			------   of the fill line lies on the upper edge of the polygon:
			----offset := to_distance_relative (set (
					----x => - line_width * 0.5, -- to the left
					----y => - line_width * 0.5)); -- down

			----move_by (start_point, offset);

			----log (text => "extra row: start" & to_string (start_point),
					----level => log_threshold + 3);
			
			----fill_row;
		----end if;

		--return result;
	--end make_rows;


	
	procedure floating_polygons is
		use pac_floating_solid;
		
		--procedure query_polygon (c : in pac_floating_solid.cursor) is begin
			--lower_left_corner := get_lower_left_corner (element (c));
			--log_lower_left_corner (log_threshold + 2);
		--end query_polygon;
	
		use pac_floating_hatched;
		
		--procedure query_polygon (c : in pac_floating_hatched.cursor) is begin
			--lower_left_corner := get_lower_left_corner (element (c));
			--log_lower_left_corner (log_threshold + 2);
		--end query_polygon;

		procedure floating_solid (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			c : pac_floating_solid.cursor := module.board.conductors.fill_zones.solid.first;
		begin
			while c /= pac_floating_solid.no_element loop

				--lower_left_corner := get_lower_left_corner (element (c));
				--log_lower_left_corner (log_threshold + 2);
				
				next (c);
			end loop;
		end floating_solid;

		
		procedure floating_hatched (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			c : pac_floating_hatched.cursor := module.board.conductors.fill_zones.hatched.first;
		begin
			while c /= pac_floating_hatched.no_element loop

				--lower_left_corner := get_lower_left_corner (element (c));
				--log_lower_left_corner (log_threshold + 2);
				
				next (c);
			end loop;
		end floating_hatched;

		
	begin -- floating_polygons
		log (text => "floating polygons ...", level => log_threshold + 1);

		--polygon_cursor := element (
		--iterate (element (module_cursor).board.conductors.polygons.solid, query_polygon'access);
		--iterate (element (module_cursor).board.conductors.polygons.hatched, query_polygon'access);

		update_element (generic_modules, module_cursor, floating_solid'access);
		update_element (generic_modules, module_cursor, floating_hatched'access);
	end floating_polygons;


	
	-- Fills polygons that are connected with a net:
	procedure signal_contours is
		use et_nets;
		
		use pac_nets;
		use pac_route_solid;
		use pac_route_hatched;

		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			net_cursor : pac_nets.cursor;
			net_class : type_net_class;
		
			
			procedure route_solid (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is 
				-- The cursor that points to the zone being filled:
				use pac_route_solid;
				zone_cursor : pac_route_solid.cursor := net.route.fill_zones.solid.first;

				-- The candidate fill zone must be converted to a polygon:
				zone : type_polygon;
				
				-- The boundaries of the zone (greatest/smallest x/y):
				boundaries : type_boundaries;

				-- We fill the zones with lines from left to right.
				lower_left_corner : type_point;


				-- The width of border and fill lines:
				line_width : type_track_width;

				
				-- Deletes the complete fill of the zone:
				procedure clear_fill (
					zone	: in out type_route_solid)
				is begin
					zone.islands := no_islands;
				end clear_fill;



				-- Crops the fill zone by the outer board edges and the
				-- inner holes.
				procedure process_board_contours is
				
					outer_contour : type_polygon := board_outer_contour_master;
					holes : pac_polygon_list.list := board_holes_master;

					conductors : pac_polygon_list.list;
					
					-- After clipping the outer board contour by
					-- the holes we get a list of islands with board area:
					islands : pac_polygon_list.list;


					procedure set_islands (
						zone : in out type_route_solid)
					is
						procedure query_island (i : in pac_polygon_list.cursor) is begin
							zone.islands.append ((
								border	=> type_outer_border (element (i)),
								others	=> <>));					 
						end query_island;
						
					begin
						islands.iterate (query_island'access);
					end set_islands;
					
					
				begin
					log (text => "processing board contours ...", level => log_threshold + 4);
					log_indentation_up;
					
					-- Shrink the outer contour (of the board) by half the line 
					-- width of the fill lines:
					log (text => "outer contour ...", level => log_threshold + 4);
					offset_polygon (outer_contour, - type_float_internal_positive (line_width) * 0.5);

					-- Clip the fill zone by the outer contour of the board.
					-- The result is a list of islands:
					islands := clip (zone, outer_contour);

			
					log (text => "holes (" & count_type'image (holes.length) & ") ...", level => log_threshold + 4);
					
					-- Expand the holes by half the line width of the fill lines:
					offset_holes (holes, line_width * 0.5);

					-- Now we crop the islands by the holes:
					islands := multi_crop_2 (
						polygon_B_list	=> islands,
						polygon_A_list	=> holes,
						debug			=> false);
						--debug			=> true);

					-- CS: crop islands by vias, tracks, restrict areas, ... 
					-- CS fill zone cutouts ?

					conductors := to_polygons (
							net_cursor	=> net_cursor,
							layer		=> element (zone_cursor).properties.layer);
					
					-- CS: make cutout areas inside islands 
					-- (caused by holes, vias, tracks, restrict areas ...)

					-- Assign the islands to the candidate fill zone:
					net.route.fill_zones.solid.update_element (
						zone_cursor, set_islands'access);
					
					log_indentation_down;
				end process_board_contours;
			
				
			begin -- route_solid
				
				while zone_cursor /= pac_route_solid.no_element loop

					log (text => "zone with corner nearest to origin:" 
						 & to_string (get_corner_nearest_to_origin (element (zone_cursor))),
						level => log_threshold + 3);

					log_indentation_up;

					
					-- clear the complete fill:
					update_element (net.route.fill_zones.solid, zone_cursor, clear_fill'access);

					-- Get the width of the border and the fill lines.
					-- NOTE: Each fill zone may have a special line width:
					line_width := element (zone_cursor).width_min;

					
					-- Convert the contour of the candidate fill zone to a polygon.
					-- Shrink the zone by half the line width so that the border of the zone
					-- does not extend further than the user defined contour:
					zone := to_polygon (element (zone_cursor), fab_tolerance);

					-- CS log lowest left vertex
					
					offset_polygon (zone, - type_float_internal_positive (line_width) * 0.5);

					process_board_contours;



					

					-- Get the boundaries of the zone. From the boundaries we will
					-- later derive the total height and the lower left corner:
					boundaries := get_boundaries (zone, type_float_internal_positive (zero));
					--log (text => to_string (boundaries), level => log_threshold + 2);
					
					-- obtain the lower left corner of the zone from the boundaries:
					--lower_left_corner := type_point (set (boundaries.smallest_x, boundaries.smallest_y));
					--log_lower_left_corner (lower_left_corner, log_threshold + 2);

					
					--log_indentation_up;

					---- compute the rows:
					--rows := make_rows (
						--net_cursor		=> net_cursor,
						--net_class		=> net_class,
						--fill_zone		=> (observe => true, outline => et_fill_zones.type_zone (element (polygon_cursor))),
						--layer			=> element (polygon_cursor).properties.layer,
						--width			=> element (polygon_cursor).width_min,
						--height			=> get_height (boundaries),
						--start_point_in	=> lower_left_corner,
						--lth				=> log_threshold + 3);

					--update_element (net.route.fill_zones.solid, polygon_cursor, add_rows'access);
					
					--log_indentation_down;
					log_indentation_down;
					
					next (zone_cursor);
				end loop;
			end route_solid;

			
			procedure route_hatched (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is 
				zone_cursor : pac_route_hatched.cursor := net.route.fill_zones.hatched.first;

				-- CS: delete all existing fill lines:
				
				-- The width of a fill line:
				line_width : type_track_width;

				-- The boundaries of the polygon (greatest/smallest x/y):
				boundaries : type_boundaries;

				-- The total height of the polygon:
				height : type_float_internal_positive;
				
			begin
				while zone_cursor /= pac_route_hatched.no_element loop

					-- Get the boundaries of the polygon. From the boundaries we will
					-- later derive the total height and the lower left corner:
					boundaries := get_boundaries (element (zone_cursor), zero);
					--log (text => to_string (boundaries), level => log_threshold + 2);

					-- Get the total height of the polygon:
					height := get_height (boundaries);

					-- Get the width of the fill lines:
					line_width := element (zone_cursor).width_min;

					
					--log_net_name;
					--lower_left_corner := get_lower_left_corner (element (p));
					--log_lower_left_corner (log_threshold + 3);

					--log_indentation_down;
					next (zone_cursor);
				end loop;
			end route_hatched;


			procedure query_net is begin
				log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 2);
				
				log_indentation_up;

				net_class := get_net_class (module_cursor, net_cursor);
				
				update_element (module.nets, net_cursor, route_solid'access);
				update_element (module.nets, net_cursor, route_hatched'access);

				log_indentation_down;
			end query_net;
			

			procedure query_given_net (gn : pac_net_names.cursor) is begin
				-- Locate the given net in the module.
				-- If if does not exist, issue a warning.
				net_cursor := find (module.nets, element (gn));

				if net_cursor /= pac_nets.no_element then
					query_net;
				else
					log (
						importance => WARNING, 
						text => "Net " & enclose_in_quotes (to_string (element (gn))) 
							& " does not exist !", 
						level => log_threshold + 2);
				end if;
			end query_given_net;
			
			
		begin -- query_nets
			if all_zones then

				-- we must query all nets:
				net_cursor := module.nets.first;

				-- Iterate through all nets of the module:
				while net_cursor /= pac_nets.no_element loop
					query_net;
					next (net_cursor);
				end loop;

			else
				-- we query only the nets given by argument "nets":
				nets.iterate (query_given_net'access);

			end if;
		end query_nets;

		
	begin 
		log (text => "signal contours ...", level => log_threshold + 1);
		log_indentation_up;
		update_element (generic_modules, module_cursor, query_nets'access);
		log_indentation_down;
	end signal_contours;

	
begin -- fill_fill_zones

	log (text => "module " 
		& enclose_in_quotes (to_string (key (module_cursor)))
		& " filling zones. Log category " 
		& to_string (log_category) & " ...",
		level => log_threshold);

	log_indentation_up;

	log (text => "converting outer board contour to polygon ...", level => log_threshold + 1);
	
	board_outer_contour_master := to_polygon (
		contour		=> get_outline (module_cursor),
		tolerance	=> fab_tolerance);
	
	-- Shrink the outer board edge by the conductor-to-edge clearance
	-- as given by the design rules:

	offset_scratch := - design_rules.clearances.conductor_to_board_edge;
	
	log (text => "offsetting by DRU parameter " -- CS use predefined string
		& enclose_in_quotes (dru_parameter_clearance_conductor_to_board_edge) 
		& to_string (offset_scratch),
		level => log_threshold + 1);
	
	offset_polygon (board_outer_contour_master, type_float_internal (offset_scratch));
	-- for debuggin use:
	--offset_polygon (board_outer_contour_master, offset_scratch, true);
	-- CS consider half the line width !


	
	log (text => "converting holes to polygons ...", level => log_threshold + 1);
	
	board_holes_master := to_polygons (
		holes		=> get_holes (module_cursor),
		tolerance	=> fab_tolerance);

	-- Expand the holes by the conductor-to-edge clearance
	-- as given by the design rules:

	offset_scratch := - offset_scratch;
	
	log (text => "offsetting by DRU parameter " -- CS use predefined string 
		& enclose_in_quotes (dru_parameter_clearance_conductor_to_board_edge) 
		& to_string (offset_scratch),
		level => log_threshold + 1);

	offset_holes (board_holes_master, offset_scratch);
	-- for debugging use:
	--offset_holes (board_holes, offset_scratch, true);
	-- CS consider half the line width !

	
	if is_empty (nets) then
		
		-- Fill all zones if no explicit net names given:
		
		log (text => "filling all zones ...", level => log_threshold + 1);

		all_zones := true;
		
		log_indentation_up;
		signal_contours;

		-- CS floating_zones;
		-- use class settings of class "default":

		log_indentation_down;
					
	else
		log (text => "filling zones of dedicated nets ...", level => log_threshold + 1);

		all_zones := false;
		
		log_indentation_up;
		signal_contours;
		log_indentation_down;
		
	end if;

	log_indentation_down;
	
end fill_fill_zones;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
