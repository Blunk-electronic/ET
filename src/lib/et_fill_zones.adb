------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             FILL ZONES                                   --
--                                                                          --
--                              B o d y                                     --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                -- 
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
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

with et_exceptions;				use et_exceptions;


package body et_fill_zones is


	
	procedure iterate (
		lakes	: in pac_lakes.list;
		process	: not null access procedure (position : in pac_lakes.cursor);
		proceed	: not null access boolean)
	is
		c : pac_lakes.cursor := lakes.first;
	begin
		while c /= pac_lakes.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;

	

	procedure iterate (
		islands	: in pac_islands.list;
		process	: not null access procedure (position : in pac_islands.cursor);
		proceed	: not null access boolean)
	is
		c : pac_islands.cursor := islands.first;
	begin
		while c /= pac_islands.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;


	
	
	procedure make_stripes (
		island	: in out type_island;
		style	: in type_style)
	is		
		-- The boundaries of the island (greatest/smallest x/y):
		-- boundaries : constant type_boundaries := get_boundaries (island.shore.centerline);
		boundaries : constant type_boundaries := get_boundaries (island.shore);

		height : constant type_float_positive := get_height (boundaries);
		bottom : constant pac_geometry_brd.type_float := get_bottom (boundaries);

		--effective_width : type_float_positive;

		-- The factor that causes the fill stripes to overlap slightly.
		-- It is required in order to avoid a possible small gap between them
		-- that could occur during manufacturing.
		-- The lower the factor the more overlap. 1.0 means no overlap.
		--overlap_factor : constant type_float_positive := 0.99;

		
		stripe_count_rational : type_float_positive;
		stripe_count_natural : natural;
		stripe_spacing : type_float_positive;

		-- The point at which a stripe starts. We will fill the island from bottom to top.
		-- The lowest left place to start from is outside the island:
		A : type_vector := set (
			 x => get_left (boundaries) - 1.0, -- left of the island
			 y => bottom); -- just a default. will be changed while the A is moving upward

		-- The status of the A relative to the island:
		status : type_point_status (OUTSIDE); -- will/must always be outside

		-- The main collection of x-values where a stripe enters or leaves an outer or inner border:
		x_main : pac_float_numbers.list;
		
		use pac_float_numbers;
		use pac_float_numbers_sorting;


		-- This flag decides whether a stripe starts or ends:
		stripe_start : boolean := true;

		-- This procedure queries an x-value an builds a stripe:
		procedure query_x_intersection (i : in pac_float_numbers.cursor) is begin
			if i /= x_main.last then

				if stripe_start then
					island.stripes.append ((
						A	=> set (element (i), A.y),
						B	=> set (element (next (i)), A.y),
						status		=> <>)); -- default status

					stripe_start := false;					
				else
					stripe_start := true;
				end if;

			end if;
		end query_x_intersection;

		
		-- This procedure queries a lake and appends
		-- the x-values of the candidate border to the main collection of
		-- x-values:
		--procedure query_lake (l : in pac_lakes.cursor) is
		procedure query_lake (l : in pac_polygon_list.cursor) is
			--lake : type_lake renames element (l);
			lake : type_polygon renames element (l);
		begin			
			--status := get_point_status (lake.centerline, A);
			status := get_point_status (lake, A);

			splice (
				target	=> x_main,
				source	=> status.x_intersections, 
				before	=> pac_float_numbers.no_element);

			exception 
				when others =>
					--put_line ("bottom: " & to_string (bottom));
					--put_line ("height: " & to_string (height));
					put_line ("status : " & to_string (
						-- get_point_status (lake.centerlin	 A, true)));
						get_point_status (lake, A, true)));
					
					raise;
		end query_lake;

		
	begin
		--new_line;
		--put_line ("bottom: " & to_string (bottom));
		--put_line ("height: " & to_string (height));
		--put_line ("left: " & to_string (A.x));
		
		case style.style is
			when SOLID =>
				-- Since the stripes must overlap slightly the effective
				-- linewidth is smaller than style.linewidth. The effective_width
				-- is used to compute the number of stripes:
				--effective_width := type_float_positive (style.linewidth) / overlap_factor;

				-- Compute the number of stripes in a rational number (like 6.3).
				--stripe_count_rational := height / effective_width;
				stripe_count_rational := height / type_float_positive (style.linewidth);

				-- Round up the number of stripes to the next natural number (like 7)
				stripe_count_natural := natural (type_float_positive'ceiling (
						type_float_positive (stripe_count_rational)));
				

				stripe_spacing := height / type_float_positive (stripe_count_natural);


				-- Compute the rows of stripes from bottom to top:
				for row in 1 .. stripe_count_natural loop
				
					A.y := bottom + type_float_positive (row) * stripe_spacing;
					
					--put_line (to_string (get_point_status (island.outer_border, set (x_start, y))));
					-- status := get_point_status (island.shore.centerline, A, false); -- debug off
					status := get_point_status (island.shore, A, false); -- debug off
					x_main := status.x_intersections;

					-- Compute the intersections with the lakes:
					island.lakes.iterate (query_lake'access);

					-- Sort the main collection from left to right (ascending order):
					sort (x_main);

					-- Build the stripes for the current row:
					stripe_start := true;
					x_main.iterate (query_x_intersection'access);					
				end loop;

				
			when HATCHED =>
				null;
				-- CS
		end case;

		exception 
			when others =>
				--put_line ("bottom: " & to_string (bottom));
				--put_line ("height: " & to_string (height));
				put_line ("boundaries : " & to_string (boundaries));
				put_line ("A: " & to_string (A));

				raise;
		
	end make_stripes;

	
	
	procedure fill_island (
		islands		: in out pac_islands.list;
		position	: in pac_islands.cursor;
		style		: in type_style;
		process		: not null access procedure (
						island	: in out type_island;
						style	: in type_style))
	is
		island : type_island := element (position);
	begin
		process (island, style);
		islands.replace_element (position, island);
	end fill_island;



-- EASING
	function to_easing_style (easing : in string) return type_easing_style is begin
		return type_easing_style'value (easing);
	end;

	function to_string (easing : in type_easing_style) return string is begin
		return to_lower (type_easing_style'image (easing));
	end;





	
	function get_half_linewidth (
		zone	: in type_zone)
		return type_float_positive
	is begin
		return type_float_positive (zone.linewidth) * 0.5;		
	end get_half_linewidth;

	





	procedure make_islands_and_lakes (
		zone			: in out type_zone;
		linewidth		: in type_track_width;								 
		islands 		: in pac_polygon_list.list;
		lakes			: in pac_polygon_list.list;
		fill			: in boolean;
		log_threshold	: in type_log_level)
	is
		debug : boolean := false;


		-- This procedure assigns the given islands to the given zone:
		procedure set_islands (islands : in pac_polygon_list.list) is
			
			procedure query_island (i : in pac_polygon_list.cursor) is 
				island : type_polygon renames element (i);
			begin
				zone.islands.append ((
					shore		 => island,
					others		 => <>)); 
			end query_island;

		begin
			islands.iterate (query_island'access);
		end set_islands;




		-- Iterates the islands, detects polygons that
		-- form the lakes (inside the islands). Lakes are caused by objects
		-- that are completely inside a particular island.
		procedure set_lakes (lakes : in pac_polygon_list.list) is
			lakes_tmp : pac_polygon_list.list := lakes;

			use pac_islands;
			island_cursor : pac_islands.cursor := zone.islands.first;


			-- This procedure queries an island and assigns
			-- to it all lakes that are on the island:
			procedure query_island (
				island : in out type_island)
			is 
				use pac_overlap_status;
				use pac_polygon_union;

				-- These are the lakes that lie
				-- on the candidate island:
				lakes_on_island : pac_polygon_list.list;
				
				-- This procedure queries a lake and appends
				-- it to the lakes that are on the island:
				procedure query_lake (c : in pac_polygon_list.cursor) is
					lake : type_polygon renames element (c);
				begin
					island.lakes.append (lake);
				end query_lake;
				

			begin
				-- Get lakes on the candidate island:
				lakes_on_island := get_polygons (
					area		=> island.shore,  
					polygons	=> lakes_tmp,
					status		=> to_set (B_INSIDE_A));

				multi_union (lakes_on_island);

				-- Iterate the lakes:
				lakes_on_island.iterate (query_lake'access);
			end query_island;					
			
	
		begin
			while has_element (island_cursor) loop
				-- CS optimize so that only those islands are queried
				-- that have been assigned. The given list of lakes
				-- applies only to those islands.
				-- So it is a waste of time to query all islands of the zone.

				zone.islands.update_element (island_cursor, query_island'access);
				next (island_cursor);
			end loop;					
		end set_lakes;





		-- Fills the islands according to the fill style
		-- of the given zone:
		procedure fill_islands is 
			use pac_islands;
			island_cursor : pac_islands.cursor := zone.islands.first;
		begin
			case zone.fill_style is
				when SOLID =>
					declare
						style : constant type_style := (
							style		=> SOLID,
	   						linewidth	=> linewidth);
					begin
						while island_cursor /= pac_islands.no_element loop
							fill_island (
								islands		=> zone.islands, 
								position	=> island_cursor,
								style		=> style,
								process		=> make_stripes'access);

							next (island_cursor);
						end loop;					

					end;
					
				when HATCHED =>
					declare
						style : constant type_style := (
							style		=> HATCHED,
							linewidth	=> linewidth,						   
							spacing		=> zone.spacing);
					begin
						while island_cursor /= pac_islands.no_element loop
							fill_island (
								islands		=> zone.islands, 
								position	=> island_cursor,
								style		=> style,
								process		=> make_stripes'access);

							next (island_cursor);
						end loop;					

					end;					
			end case;			
		end fill_islands;



	begin
		log (text => "make_islands_and_lakes", level => log_threshold);
		-- CS log arguments, number of islands and lakes

		set_islands (islands);
		set_lakes (lakes);

		if fill then
			fill_islands;
		end if;
	end make_islands_and_lakes;







	function between_islands (
		zone	: in type_zone;
		point	: in type_vector;
		debug	: in boolean := false)
		return boolean
	is
		proceed : aliased boolean := true;

		
		procedure query_island (c : in pac_islands.cursor) is
			island : type_island renames element (c);

			-- Take the real conducting area of the island into account:
			status : constant type_point_status :=
				--get_point_status (island.shore.outer_edge, point, debug);
				get_point_status (island.shore, point, debug);
			
		begin
			case status.location is
				when INSIDE | ON_VERTEX | ON_EDGE => proceed := false;
				when others => null;
			end case;
		end query_island;
		
	begin
		iterate (zone.islands, query_island'access, proceed'access);
		return proceed;
	end between_islands;



	function get_lake (
		zone	: in type_zone;
		point	: in type_vector;
		debug	: in boolean := false)
		--return type_lake
		return type_polygon
	is
		--result : type_lake;
		result : type_polygon;
		proceed : aliased boolean := true;

		
		procedure query_island (i : in pac_islands.cursor) is
			island : type_island renames element (i);

			
			--procedure query_lake (l : in pac_lakes.cursor) is
			procedure query_lake (l : in pac_polygon_list.cursor) is
				--lake : type_lake renames element (l);
				lake : type_polygon renames element (l);

				-- This takes the real conducting area of the surrounding 
				-- island is taken into account:
				lake_status : constant type_point_status :=
					--get_point_status (lake.inner_edge, point);
					get_point_status (lake, point);
				
			begin
				if debug then
					put_line (" lake");
				end if;

				case lake_status.location is
					when INSIDE =>
						proceed := false;
						result := lake;

						if debug then
							put_line ("  inside");
							--put_line ("  *0 " & to_string (element (result)));
						end if;
						
					when others => null; -- ignore this inner border
				end case;
			end query_lake;

			
		begin
			if debug then
				put_line (" island");
			end if;

			iterate (island.lakes, query_lake'access, proceed'access);
		end query_island;

		
	begin
		if debug then
			put_line ("get lake");
		end if;
		
		iterate (zone.islands, query_island'access, proceed'access);

		if debug then
			-- put_line (" centerline " & to_string (result.centerline));
			-- put_line (" inner edge " & to_string (result.inner_edge));
			put_line (" centerline " & to_string (result));
		end if;

		--if debug then
			--put_line ("--------------");
		--end if;

		if proceed then
			raise semantic_error_1 with "Point is not in a lake !"; 
			-- CS output something more helpful
		end if;
		
		return result;
	end get_lake;
	

	
	
	function get_location (
		zone	: in type_zone;
		point	: in type_vector;
		debug	: in boolean := false)
		return type_location
	is
		location : type_location := NON_CONDUCTING_AREA;
		proceed_island : aliased boolean := true;

		
		procedure query_island (i : in pac_islands.cursor) is
			island : type_island renames element (i);

			-- Take the real conducting area of the island into account:
			island_status : constant type_point_status :=
				--get_point_status (island.shore.outer_edge, point);
				get_point_status (island.shore, point);

			proceed_lake : aliased boolean := true;

			
			--procedure query_lake (l : in pac_lakes.cursor) is
			procedure query_lake (l : in pac_polygon_list.cursor) is
				--lake : type_lake renames element (l);
				lake : type_polygon renames element (l);

				-- This takes the real conducting area of the surrounding island into account.
				lake_status : constant type_point_status :=
					--get_point_status (lake.inner_edge, point);
					get_point_status (lake, point);

			begin
				if debug then
					put_line ("lake");
				end if;

				case lake_status.location is
					when OUTSIDE | ON_VERTEX | ON_EDGE =>
						if debug then
							put_line (" outside");
						end if;
						
					when INSIDE => null; -- ignore this inner border
						location := NON_CONDUCTING_AREA;
						proceed_lake := false;
						if debug then
							put_line (" inside");
						end if;
					
				end case;
			end query_lake;

			
		begin -- query_island
			if debug then
				put_line ("island");
			end if;
			
			case island_status.location is
				when INSIDE | ON_VERTEX | ON_EDGE =>
					location := CONDUCTING_AREA;
					proceed_island := false;
					
					if debug then
						put_line (" on island");
					end if;
					
					iterate (island.lakes, query_lake'access, proceed_lake'access);
										
				when OUTSIDE => null; -- ignore this island
			end case;
		end query_island;
		
	begin
		-- location default is NON_CONDUCTING_AREA !
		
		if debug then
			put_line ("get location of point" & to_string (point));
		end if;

		iterate (zone.islands, query_island'access, proceed_island'access);
		return location;
	end get_location;




	
	
	function get_distance_to_nearest_island (
		zone		: in type_zone;
		start_point	: in type_vector;
		direction	: in type_angle;
		debug		: in boolean := false)
		return type_distance_to_conducting_area
	is
		result_edge_exists : boolean := true;
		result_distance_to_edge : type_float_positive := 0.0;
		
		result_centerline_exists : boolean := true;
		result_distance_to_centerline : type_float_positive := 0.0;
		
		ray : constant type_ray := (start_point, direction);

		half_linewidth : constant type_float_positive := get_half_linewidth (zone);
		
		use pac_vectors;
		-- intersections_with_edges : pac_vectors.list;
		intersections_with_centerlines : pac_vectors.list;

		
		procedure query_island (i : in pac_islands.cursor) is
			island : type_island renames element (i);

			procedure query_centerline (e : in pac_edges.cursor) is
				use pac_edges;
				I : constant type_line_vector_intersection := 
					get_intersection (ray, element (e));
			begin
				case I.status is
					when EXISTS =>
						if debug then
							put_line (" intersection at " & to_string (I.intersection));
						end if;

						intersections_with_centerlines.append (I.intersection);

					when others => null;
				end case;
			end query_centerline;

			
			-- procedure query_edge (e : in pac_edges.cursor) is
			-- 	use pac_edges;
			-- 	I : constant type_line_vector_intersection := 
			-- 		get_intersection (ray, element (e));
			-- begin
			-- 	case I.status is
			-- 		when EXISTS =>
			-- 			if debug then
			-- 				put_line (" intersection at " & to_string (I.intersection));
			-- 			end if;
   -- 
			-- 			intersections_with_edges.append (I.intersection);
   -- 
			-- 		when others => null;
			-- 	end case;
			-- end query_edge;

			
		begin
			if debug then
				put_line (" island");
			end if;

			--island.shore.centerline.edges.iterate (query_centerline'access);
			island.shore.edges.iterate (query_centerline'access);
			-- island.shore.outer_edge.edges.iterate (query_edge'access);
		end query_island;

		
	begin
		if debug then
			put_line ("ray " & to_string (start_point) & " direction " & to_string (direction));
		end if;
		
		-- Collect the intersections of the ray with the islands
		-- in container "intersections":
		zone.islands.iterate (query_island'access);

		-- if is_empty (intersections_with_edges) then
		-- 	result_edge_exists := false; -- no island found in given direction
		-- else
		-- 	-- Extract from intersections the one that is closest to start_point:
		-- 	remove_redundant_vectors (intersections_with_edges);
		-- 	sort_by_distance (intersections_with_edges, start_point);
  -- 
		-- 	result_distance_to_edge := 
		-- 		get_distance_total (start_point, intersections_with_edges.first_element);
		-- end if;


		if is_empty (intersections_with_centerlines) then
			result_centerline_exists := false; -- no centerline found in given direction
		else
			-- Extract from intersections the one that is closest to start_point:
			remove_redundant_vectors (intersections_with_centerlines);
			sort_by_distance (intersections_with_centerlines, start_point);

			result_distance_to_centerline := 
				get_distance_total (start_point, intersections_with_centerlines.first_element);
		end if;

		
		-- case result_edge_exists is
		-- 	when TRUE =>

				case result_centerline_exists is
					when TRUE =>
						return (
							edge_exists => true,
							distance_to_edge =>	result_distance_to_edge,
							centerline_exists => true,
							distance_to_centerline => result_distance_to_centerline
							);

						
					when FALSE =>
						return (
							edge_exists => true,
							distance_to_edge =>	result_distance_to_edge,
							centerline_exists => false);
						
				end case;

		-- 	when FALSE =>
		-- 		return (
		-- 			edge_exists => false,
		-- 			centerline_exists => false);
		-- end case;
		
	end get_distance_to_nearest_island;




	
	

	procedure get_distance_to_island (
		zone			: in type_zone;
		point			: in type_vector;
		direction		: in type_angle;
		island_exists	: out boolean;
		distance		: out type_float_positive;
		log_threshold	: in type_log_level)
	is
		-- Build a ray that starts at the given point
		-- and travels into the given direction:
		ray : constant type_ray := (point, direction);

		
		use pac_vectors;
		intersections : pac_vectors.list;



		procedure query_island (i : in pac_islands.cursor) is
			island : type_island renames element (i);

			
			procedure query_centerline (e : in pac_edges.cursor) is
				use pac_edges;
				I : constant type_line_vector_intersection := 
					get_intersection (ray, element (e));
			begin
				case I.status is
					when EXISTS =>
						intersections.append (I.intersection);

					when others => null;
				end case;
			end query_centerline;

			
		begin
			island.shore.edges.iterate (query_centerline'access);
		end query_island;


		
	begin
		log (text => "get_distance_to_island"
			 & " point: " & to_string (point)
			 & " direction: " & to_string (direction),
			 level => log_threshold);

		log_indentation_up;

		-- Collect the intersections of the ray with the islands
		-- in container "intersections":
		zone.islands.iterate (query_island'access);


		if is_empty (intersections) then
			-- The ray does not intersect any edge of any island.
			-- So there is no island in the given direction:
			island_exists := false;
			distance := type_float_positive'last;
		else
			-- Extract from intersections the one that is 
			-- closest to the given start point:
			remove_redundant_vectors (intersections);
			sort_by_distance (intersections, point);

			island_exists := true;
			distance := get_distance_total (point, intersections.first_element);
		end if;
		
		
		log_indentation_down;
	end get_distance_to_island;

	
	


	
	
	
	function get_distance_to_conducting_area (
		zone			: in type_zone;
		linewidth		: in type_track_width;
		start_point		: in type_vector;
		direction		: in type_angle;
		location_known	: in type_location_known := false;
		location		: in type_location := CONDUCTING_AREA;
		log_threshold	: in type_log_level)
		return type_distance_to_conducting_area
	is
		result_edge_exists : boolean := true;
		result_distance_to_edge : type_float_positive := 0.0;
		
		result_centerline_exists : boolean := true;
		result_distance_to_centerline : type_float_positive := 0.0;

		location_computed : type_location;

		lake : type_polygon;


		-- This procedure determines whether the given start point
		-- is somewhere inside the conducting area (island) or in 
		-- a non-conducting area (lake).
		-- If the location of the start point is already known, then
		-- the given location is used for further steps:		
		procedure compute_location is begin
			if location_known then
				location_computed := location;

				log (text => "location of point already known",
					 level => log_threshold + 1);

			else
				-- Compute the location of the start point:
				log (text => "compute location of start point",
					 level => log_threshold + 1);

				location_computed := get_location (zone, start_point);
			end if;
		end compute_location;


	begin
		log (text => "get_distance_to_conducting_area",
			 level => log_threshold);

		log_indentation_up;
		
		compute_location;

		-- CS rework the follwing code:
		
		case location_computed is
			when CONDUCTING_AREA => 
				-- Start point is already in conducting area.
				log (text => "in conducting area", level => log_threshold + 1);

				log_indentation_down;
				
				-- Since the start point is inside the conducting
				-- area, the distance to the conducting area is zero:
				return in_conducting_area;
				

				
			when NON_CONDUCTING_AREA =>
				-- Start point is either between islands or inside inner borders:
				log (text => "in non-conducting area", level => log_threshold + 1);

				
				if between_islands (zone, start_point) then
					log (text => "between islands", level => log_threshold + 1);

					log_indentation_down;
					
					-- Point is between islands.
					--return get_distance_to_nearest_island (zone, start_point, direction, debug);
					return get_distance_to_nearest_island (zone, start_point, direction);

				else
					log (text => "inside lake", level => log_threshold + 1);
				
					-- Point is in a lake. Get the lake concerned:					
					--lake := get_lake (zone, start_point, true);
					lake := get_lake (zone, start_point);

					--if debug then
						--put_line ("lake: " & to_string (lake));
					--end if;

					-- The lake has a shore with a certain linewidth.
					-- So the shore has a centerline.
					-- Get the distance from the start point to
					-- the centerline:
					result_distance_to_centerline := 
						get_distance_to_border (lake, start_point, direction);

					--if debug then
						--put_line ("distance to centerline of shore" 
						-- & to_string (result_distance_to_centerline));
					--end if;

					-- In order to get the inner edge of the shore
					-- the centerline of the lake must be shrinked by
					-- half the linewidth of the zone:
					offset_polygon (lake, - type_float_positive (linewidth * 0.5),
													log_threshold + 2);

					-- Get the distance to the inner edge of the shore:
					result_distance_to_edge := 
						get_distance_to_border (lake, start_point, direction);

					log_indentation_down;
					
					return (
						edge_exists				=> true,
						distance_to_edge		=> result_distance_to_edge,
						centerline_exists		=> true,
						distance_to_centerline	=> result_distance_to_centerline);
					
				end if;
		end case;
	end get_distance_to_conducting_area;

	


	



	procedure get_distance_to_border (
		zone			: in type_zone;
		point			: in type_vector;
		direction		: in type_angle;
		border_exists	: out boolean;
		distance		: out type_float_positive;
		log_threshold	: in type_log_level)
	is

		-- procedure between_islands is
		-- 	d : type_distance_to_conducting_area :=
		-- 		get_distance_to_nearest_island (zone, point, direction);			
		-- begin			
		-- 	if d.centerline_exists then
		-- 		border_exists := true;
		-- 		distance := d.distance_to_centerline;
		-- 	else
		-- 		border_exists := false;
		-- 		distance := type_float_positive'last;
		-- 	end if;		
		-- end between_islands;


		
		procedure between_islands is begin
			get_distance_to_island (
				zone			=> zone,
				point			=> point,
				direction		=> direction,
				island_exists	=> border_exists,
				distance		=> distance,
				log_threshold	=> log_threshold + 2);
		end;

		

		procedure on_island is
			lake : type_polygon;
		begin
			border_exists := true;

			-- CS: For the moment we assume the point
			-- is in a lake:
			lake := get_lake (zone, point);

			-- The lake has a shore with a certain linewidth.
			-- So the shore has a centerline.
			-- Get the distance from the start point to
			-- the centerline:
			distance := get_distance_to_border (lake, point, direction);			
		end on_island;
		

		
	begin
		log (text => "get_distance_to_border"
			 & " start point: " & to_string (point)
			 & " direction: " & to_string (direction),
			 level => log_threshold);
		log_indentation_up;


		if between_islands (zone, point) then
			log (text => "point lies between islands", level => log_threshold + 1);
			between_islands;			
		else
			log (text => "point lies on an island", level => log_threshold + 1);
			on_island;
		end if;
		
		log_indentation_down;
	end get_distance_to_border;


	

	
	
-- 	procedure route_fill_zone_properties (
-- 	-- Logs the properties of the given fill_zone of a route
-- 		cursor			: in pac_conductor_fill_zones_signal.cursor;
-- 		log_threshold 	: in et_string_processing.type_log_level) is
-- 		use pac_conductor_fill_zones_signal;
-- 		use type_fill_zone_points;
-- 		points : type_fill_zone_points.set;
-- 		point_cursor : type_fill_zone_points.cursor;
-- 	begin
-- 		-- general stuff
-- 		log (text => "fill_zone" & 
-- 			 " " & text_fill_zone_signal_layer & to_string (element (cursor).layer) &
-- 			 " " & text_fill_zone_width_min & to_string (element (cursor).width_min) &
-- 			 " " & text_fill_zone_pad_connection & to_string (element (cursor).pad_connection) &
-- 			 " " & text_fill_zone_priority_level & to_string (element (cursor).priority_level) &
-- 			 " " & text_fill_zone_isolation_gap & to_string (element (cursor).isolation_gap) &
-- 			 " " & text_fill_zone_corner_easing & to_string (element (cursor).corner_easing) &
-- 			 " " & text_fill_zone_easing_radius & to_string (element (cursor).easing_radius),
-- 			 level => log_threshold);
-- 
-- 		log_indentation_up;
-- 		
-- 		-- type depended stuff
-- 		case element (cursor).pad_connection is
-- 			when THERMAL =>
-- 				log (text => text_fill_zone_pad_technology & to_string (element (cursor).thermal_technology) &
-- 					" " & text_fill_zone_thermal_width & to_string (element (cursor).thermal_width) &
-- 					" " & text_fill_zone_thermal_gap & to_string (element (cursor).thermal_gap),
-- 					level => log_threshold);
-- 
-- 			when SOLID =>
-- 				log (text => text_fill_zone_pad_technology & to_string (element (cursor).solid_technology),
-- 					level => log_threshold);
-- 				
-- 			when NONE =>
-- 				null;
-- 		end case;
-- 
-- 		-- corner points
-- 		log (text => text_fill_zone_corner_points, level => log_threshold);
-- 		points := element (cursor).corners;
-- 		point_cursor := points.first;
-- 		while point_cursor /= type_fill_zone_points.no_element loop
-- 			log (text => to_string (element (point_cursor)), level => log_threshold);
-- 			next (point_cursor);
-- 		end loop;
-- 		
-- 		log_indentation_down;
-- 	end route_fill_zone_properties;

		
	
end et_fill_zones;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
