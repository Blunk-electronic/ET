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
with ada.containers; 				use ada.containers;
with ada.containers.doubly_linked_lists;


with et_exceptions;					use et_exceptions;

with et_contour_to_polygon;			use et_contour_to_polygon;
with et_routing;					use et_routing;
with et_board_ops.devices;
with et_thermal_relief;				use et_thermal_relief;


separate (et_board_ops)

procedure fill_fill_zones (
	module_cursor	: in pac_generic_modules.cursor;
	log_category	: in type_log_category;
	log_threshold	: in type_log_level;
	nets 			: in pac_net_names.list := no_net_names)
is 
	-- CS: Most of this stuff can be moved to et_fill_zones.boards
	-- so that solid and hatched zones inherit from primitive operations
	-- defined for type_zone. 
	
	use pac_geometry_brd;
	use pac_polygons;
	use pac_polygon_clipping;
	use pac_polygon_cropping;
	use pac_polygon_offsetting;
	use pac_polygon_union;
	
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

	-- The holes inside the board area. After expanding by the
	-- conductor-to-edge clearance this serves as master for
	-- filling zones of nets. Each net may have an individual setting for 
	-- the with of the fill lines.
	use pac_polygon_list;
	board_holes_master : pac_polygon_list.list;

	-- The holes inside the board area after expanding 
	-- the board_holes_master by half the
	-- line width of a particular fill zone:
	holes : pac_polygon_list.list;


	-- The default set to test overlap statuses contains B_INSIDE_A and A_OVERLAPS_B.
	-- This function creates such a set:
	function make_overlap_status_set return pac_overlap_status.set is
		use pac_overlap_status;
		S : pac_overlap_status.set := to_set (B_INSIDE_A);
	begin
		S.insert (A_OVERLAPS_B);
		return S;
	end make_overlap_status_set;
	
	
	overlap_status_set : constant pac_overlap_status.set := make_overlap_status_set;

	
	
	-- Expands the board_holes_master by half the given linewidth.
	-- Extracts the holes that are in the given zone and those with 
	-- overlap the zone.
	-- Loads variable "holes" with the result:
	procedure expand_holes (
		zone		: in type_polygon;
		linewidth	: in type_track_width)
	is begin
		holes := board_holes_master;
		
		log (text => "expanding holes (" & count_type'image (holes.length) & ") ...", 
			 level => log_threshold + 4);
				
		-- Expand the holes by half the line width of the fill lines:
		offset_holes (holes, linewidth * 0.5);

		-- Extract those holes that are inside the zone and those
		-- with overlap the zone:
		holes := get_polygons (zone, holes, overlap_status_set);
				
		multi_union (holes);
	end expand_holes;


	
	-- Clips the given zone by the outer board contours.
	-- The result - a list of islands - will be cropped by the holes.
	-- Assumes that variable "holes" has been updated by procedure expand_holes.
	-- The result is a list of polygons because the given zone may disintegrate
	-- into smaller fragments:
	function zone_to_polygons (
		zone		: in type_polygon;
		line_width	: in type_track_width)
		return pac_polygon_list.list
	is		
		outer_contour : type_polygon := board_outer_contour_master;

		-- After clipping the zone by the outer board contours
		-- we get a list of islands:
		islands : pac_polygon_list.list;				
	begin
		log (text => "processing board contours ...", level => log_threshold + 4);
		log_indentation_up;
		
		-- Shrink the outer contour (of the board) by half the line 
		-- width of the zone border:
		log (text => "outer contour ...", level => log_threshold + 4);
		offset_polygon (outer_contour, - type_float_positive (line_width) * 0.5);

		-- Clip the fill zone by the outer contour of the board.
		-- The zone may shrink or disintegrate into smaller islands:
		islands := clip (zone, outer_contour);

		-- Now we crop the islands by the holes. The outcome are even more islands.
		-- This adresses holes that
		-- - cause islands to shrink
		-- - cause a fragmentation of islands
		-- Holes that are completely inside islands are ignored here.
		islands := multi_crop_2 (
			polygon_B_list	=> islands,
			polygon_A_list	=> holes,
			debug			=> false);
			--debug			=> true);

		log_indentation_down;

		return islands;
	end zone_to_polygons;


	-- Temporarily storage for properties of zones connected with a net:
	relief_properties	: type_relief_properties;
	terminal_reliefes	: pac_reliefes.list;
	terminal_connection	: type_pad_connection := pad_connection_default;
	terminal_technology	: type_pad_technology := pad_technology_default;
	native_tracks_embedded : type_native_tracks_embedded := false;


	type type_conductor_to_polygons_result is record
		polygons				: pac_polygon_list.list;
		terminals_with_relief	: pac_terminals_with_relief.list;
	end record;
	
	
	-- Returns a list of polygons caused by conductor
	-- objects (tracks, terminals, vias, texts, fiducials) in the given signal layer.
	-- The polygons are expanded by the zone_clearance or by
	-- the clearance of a particular net (the greater value of them is applied).
	-- Returns only those polygons which are inside the given zone.
	-- As a byproduct, the return also contains a list of terminals that require
	-- thermal reliefes. If the zone is not connected with the given parent_net, then the
	-- list "terminals_with_relief" is empty:
	function conductors_to_polygons (
		zone_polygon	: in type_polygon;
		zone_clearance	: in type_track_clearance;
		linewidth		: in type_track_width;								
		layer 			: in type_signal_layer;
		parent_net		: in pac_nets.cursor := pac_nets.no_element)
		return type_conductor_to_polygons_result
	is
		result : type_conductor_to_polygons_result;

		layer_category : type_signal_layer_category;

		
		half_linewidth_float : constant type_float_positive := 
			type_float_positive (linewidth * 0.5);

		zone_clearance_float : constant type_float_positive :=
			type_float_positive (zone_clearance);

		-- Most offset operations here use a default value:
		default_offset : constant type_float_positive :=
			half_linewidth_float + zone_clearance_float;
		
		
		-- NETS -------------------------------------------------------------

		-- If a parent net was given (via argument parent_net) then
		-- this will hold the actual net name like "GND".
		-- Otherwise it will be left empty:
		parent_net_name : pac_net_name.bounded_string;

		procedure set_parent_net_name is begin
			if parent_net /= pac_nets.no_element then
				parent_net_name := key (parent_net);
			end if;
		end set_parent_net_name;
	

		collect_terminals_with_relief : boolean := false;
			

		
		-- Converts the terminals of the given net to a list of polygons.
		-- If the flag "collect_terminals_with_relief" is true, then the information required
		-- to compute the thermal reliefes is collected in 
		-- list "terminals_with_thermal_relief":
		function get_terminal_polygons (net_cursor : in pac_nets.cursor)
			return pac_polygon_list.list
		is
			use et_nets;
			
			result : pac_polygon_list.list; -- a list of polygons to be returned
			ports : et_nets.type_ports;

			use pac_device_ports;

			
			-- Converts the terminal, that is linked to the given device port,
			-- to a polygon and appends it to the result:
			procedure query_device_port (d : in pac_device_ports.cursor) is

				port : type_device_port renames element (d);
				-- Now port contains the device name, unit name and port name.
				
				-- Get the cursor to the device in the schematic:
				device_cursor : constant pac_devices_sch.cursor := 
					locate_device (module_cursor, port.device_name);

				-- Get the cursor to the physical terminal (in the package model)
				-- that is linked with the port:
				terminal_cursor : constant pac_terminals.cursor := 
					get_terminal (device_cursor, port.unit_name, port.port_name);

				-- Convert the terminal outline to a polygon:
				use et_board_ops.devices;
				terminal_polygon : constant type_terminal_polygon := to_polygon (
					module_cursor, device_cursor, terminal_cursor, layer_category, fill_tolerance);

				terminal_zone_overlap : type_overlap_status;
				
			begin -- query_device_port
				-- If the terminal does not affect the current signal layer,
				-- then nothing happens here. Otherwise the outline of the terminal
				-- will be appended to the result:
				if terminal_polygon.exists then
					result.append (terminal_polygon.polygon);
				end if;

				-- If the terminals of this net require thermal reliefes, then
				-- collect the necessary information:
				if collect_terminals_with_relief then

					-- Do a preselection of those terminals that are overlapping
					-- the given zone or are inside the given zone:
					terminal_zone_overlap := get_overlap_status (
						polygon_A => terminal_polygon.polygon,
						polygon_B => zone_polygon);

					case terminal_zone_overlap is
						when A_INSIDE_B | A_OVERLAPS_B =>
							conductors_to_polygons.result.terminals_with_relief.append ((
								position => terminal_polygon.position, -- in the board
								outline	 => terminal_polygon.polygon, -- in the board
								terminal => terminal_cursor));  -- in the package model

						when others => null;
					end case;
				end if;
			end query_device_port;

			
		begin
			-- Get the ports of all devices connected with the given net.
			-- Therefore we do not pass a specific assembly variant here.
			ports := get_ports (net_cursor); 

			-- In variable "ports" we are interested in selector "devices" exclusively.
			-- Submodule ports and netchangers are just virtual devices
			-- that connect two conductor tracks. They can therefore be ignored:
			ports.devices.iterate (query_device_port'access);
	
			return result;
		end get_terminal_polygons;

		
		-- Extracts all conductor objects connected with the given net
		-- offsets them by half_linewidth_float + a special clearance
		-- and appends them to the result:
		procedure extract_conductor_objects (net_cursor : in pac_nets.cursor) is

			-- The clearance between net and zone is either the given zone_clearance
			-- or the clearance of the net itself. However, the greater value is applied:
			net_class : constant type_net_class := get_net_class (module_cursor, net_cursor);
			
			clearance : constant type_track_clearance := 
				get_greatest (zone_clearance, net_class.clearance);

			-- The polygons of the candidate net are collected here
			-- (later they will be appended to the result):
			polygons : pac_polygon_list.list;

			
			route : et_pcb.type_route renames element (net_cursor).route;

			procedure query_line (l : in pac_conductor_lines.cursor) is
				use pac_conductor_lines;
				line : type_conductor_line renames element (l);
			begin
				if line.layer = layer then
					polygons.append (to_polygon (line, fill_tolerance));
				end if;
			end query_line;

			
			procedure query_arc (a : in pac_conductor_arcs.cursor) is
				use pac_conductor_arcs;
				arc : type_conductor_arc renames element (a);
			begin
				if arc.layer = layer then
					polygons.append (to_polygon (arc, fill_tolerance));
				end if;
			end query_arc;

			

			procedure query_via (v : in pac_vias.cursor) is
				use pac_vias;
				via : type_via renames element (v);
			begin
				case via.category is
					when THROUGH =>
						if layer_category = OUTER_TOP or layer_category = OUTER_BOTTOM then
							polygons.append (to_polygon (via.position, via.restring_outer, via.diameter, fill_tolerance));
						else
							polygons.append (to_polygon (via.position, via.restring_inner, via.diameter, fill_tolerance));
						end if;

					when BLIND_DRILLED_FROM_TOP =>
						if layer_category = OUTER_TOP then
							polygons.append (to_polygon (via.position, via.restring_top, via.diameter, fill_tolerance));
						elsif blind_via_uses_layer (via, layer, bottom_layer) then
							polygons.append (to_polygon (via.position, via.restring_inner, via.diameter, fill_tolerance));
						end if;

					when BLIND_DRILLED_FROM_BOTTOM =>
						if layer_category = OUTER_BOTTOM then
							polygons.append (to_polygon (via.position, via.restring_bottom, via.diameter, fill_tolerance));
						elsif blind_via_uses_layer (via, layer, bottom_layer) then
							polygons.append (to_polygon (via.position, via.restring_inner, via.diameter, fill_tolerance));
						end if;
						
					when BURIED =>
						if layer_category = INNER and then
						   buried_via_uses_layer (via, layer) then
							polygons.append (to_polygon (via.position, via.restring_inner, via.diameter, fill_tolerance));
						end if;
				end case;
			end query_via;


			-- Converts tracks, vias and terminals to polygons,
			-- expands them by the clearance and appends them to
			-- the result:
			procedure convert_conductor_objects_to_polygons is 
				terminals : pac_polygon_list.list;
			begin
				-- Query track segments:
				route.lines.iterate (query_line'access);
				route.arcs.iterate (query_arc'access);
				
				-- Query vias:
				route.vias.iterate (query_via'access);
				
				-- CS evaluate native_tracks_embedded ?

				
				-- CS fill zones, ... see et_pcb.type_route

				-- terminals of packages
				terminals := get_terminal_polygons (net_cursor);
				polygons.splice (before => pac_polygon_list.no_element, source => terminals);

				-- expand polygons by clearance
				offset_polygons (polygons, half_linewidth_float + type_float_positive (clearance));

				result.polygons.splice (before => pac_polygon_list.no_element, source => polygons);
			end convert_conductor_objects_to_polygons;
			

			-- If we are processing a zone that is connected with a net,
			-- and the candidate is the parent net of the zone, then this
			-- flag is set to true:
			in_parent_net : boolean := false;

			
		begin -- extract_conductor_objects
			
			-- If no parent net was given, then the given zone is 
			-- assumed to be a floating zone. If a parent net was given,
			-- then the zone is connected with a net:
			if parent_net = pac_nets.no_element then -- floating zone
				collect_terminals_with_relief := false;
				convert_conductor_objects_to_polygons;
			else
				-- Zone is connected with a net:
				-- NOTE: This is relevant exclusively for zones connected with a net !

				if key (net_cursor) = parent_net_name then
					in_parent_net := true;
				end if;

				
				case terminal_connection is
					when SOLID =>
						-- Skip the parent net and process all other nets.
						-- Thus all conductor objects of the parent net will be completely
						-- embedded in the fill zone:
						if not in_parent_net then
							collect_terminals_with_relief := false;
							convert_conductor_objects_to_polygons;
						end if;

					when THERMAL =>
						if in_parent_net then
							collect_terminals_with_relief := true;
						else
							collect_terminals_with_relief := false;
						end if;
						convert_conductor_objects_to_polygons;
				end case;
			end if;

		end extract_conductor_objects;

		
		-- This procedure takes a cursor to a device in the schematic,
		-- converts the outlines of its unconnected terminals to polygons,
		-- offsets them by the zone_clearance + half_linewidth_float and
		-- finally appends them to the result:
		procedure extract_unconnected_terminals (
			device_cursor : in pac_devices_sch.cursor) 
		is
			use et_board_ops.devices;
			
			terminals : constant pac_terminals.map := 
				get_unconnected_terminals (module_cursor, device_cursor);

			
			procedure query_terminal (terminal_cursor : in pac_terminals.cursor) is
				-- Convert the terminal outline to a polygon:
				terminal_polygon : type_terminal_polygon := to_polygon (
					module_cursor, device_cursor, terminal_cursor, 
					layer_category, fill_tolerance);
			begin
				--put_line ("nc " & to_string (pac_terminals.key (terminal_cursor)));
				
				if terminal_polygon.exists then
					offset_polygon (terminal_polygon.polygon, default_offset);
					result.polygons.append (terminal_polygon.polygon);
				end if;
			end query_terminal;

			
		begin
			--put_line ("dev " & to_string (key (device_cursor)));
			terminals.iterate (query_terminal'access);
		end extract_unconnected_terminals;

		
		
		-- TEXTS ---------------------------------------------------------------
		use et_conductor_text.boards.pac_conductor_texts;
		
		procedure query_text (t : in pac_conductor_texts.cursor) is 
			text : type_conductor_text renames element (t);
			borders : pac_polygon_list.list;
		begin
			if text.layer = layer then

				borders := get_borders (text.vectors);

				offset_polygons (borders, default_offset);
				
				-- NOTE: The borders of the characters of the text should not overlap.
				-- Therefore there is no need for unioning the characters at this time.
				
				result.polygons.splice (
					before => pac_polygon_list.no_element,
					source => borders);

			end if;
		end query_text;
		
		
	begin -- conductors_to_polygons
		
		log (text => "processing conductor objects ...", level => log_threshold + 4);
		log_indentation_up;
		
		-- Set the layer category:
		if layer = 1 then
			layer_category := OUTER_TOP;
		elsif layer = bottom_layer then
			layer_category := OUTER_BOTTOM;
		else
			layer_category := INNER;
		end if;
		

		-- Assigns to parent_net_name the actual name of the
		-- parent net. Does nothing if no parent net given
		-- by argument parent_net:
		set_parent_net_name;

		-- Extract conductor objects of nets. 
		-- Exempt the parent net (if specified by argument parent_net):
		element (module_cursor).nets.iterate (extract_conductor_objects'access);

		-- Extract unconnected terminals of devices:
		element (module_cursor).devices.iterate (extract_unconnected_terminals'access);
			
		-- board texts:
		element (module_cursor).board.conductors.texts.iterate (query_text'access);
		
		-- CS non electrical conductor stuff (foreign floating fill zones, package text, fiducials, ...)

		-- Now the polygons held in variable "result"
		-- - inside the given zone or
		-- - overlapping the given zone
		-- must be extracted. 
		result.polygons := get_polygons (zone_polygon, result.polygons, overlap_status_set);
		
		log_indentation_down;
		
		return result;
	end conductors_to_polygons;
	


	-- Returns a list of polygons caused by route restrict
	-- objects in the given signal layer.
	function restrict_to_polygons (
		layer : in type_signal_layer)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;
	begin
		log (text => "processing restrict area ...", level => log_threshold + 4);
		log_indentation_up;

		-- CS iterate global restrict areas
		-- CS iterate net specific cutouts ?

		log_indentation_down;
		return result;
	end restrict_to_polygons;


	-- Returns a list of polygons caused by cutout areas
	-- in the given signal layer.
	function cutouts_to_polygons (
		layer : in type_signal_layer)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;
	begin
		log (text => "processing cutout area ...", level => log_threshold + 4);
		log_indentation_up;
		
		-- CS iterate global cutout areas

		log_indentation_down;
		return result;
	end cutouts_to_polygons;


	

	-- Clears the given basket:
	procedure empty_basket (
		basket : in out pac_polygon_list.list)
	is begin
		basket.clear;
	end empty_basket;
	

	
	-- Appends a list of polygons to the given basket.
	-- NOTE: The given list will be emptied.
	procedure put_into_basket (
		basket		: in out pac_polygon_list.list;
		polygons	: in out pac_polygon_list.list)
	is begin
		basket.splice (before => pac_polygon_list.no_element, source => polygons);

		-- Merge overlapping polygons:
		--multi_union (basket);
	end put_into_basket;


	-- Fills the given zone that is in the given layer
	-- with the given linewidth and clearance to foreign conductor
	-- objects. If a certain conductor object requires a greater
	-- clearance, then that clearance will take precedence.
	-- If a parent net is given - via cursor - then the conductor objects of this
	-- net will be treated in a special way. Then the given zone must be a 
	-- type_route_hatched or a type_route_solid.
	-- - terminals may be embedded in the zone or may get connected via thermal relieves.
	-- - tracks may be embedded in the zone or the zone will be filled around them.
	-- - see specification of type_route_solid and type_route_hatched.
	-- If something goes wrong, an exception is raised.
	procedure fill_zone (
		zone		: in out type_zone'class;
		linewidth	: in type_track_width;
		layer 		: in et_pcb_stack.type_signal_layer;
		clearance	: in type_track_clearance;
		parent_net	: in pac_nets.cursor := pac_nets.no_element;
		debug		: in boolean := false)
	is
		zone_polygon : type_polygon;
		
		islands : pac_polygon_list.list;
		restrict, cutouts : pac_polygon_list.list;
		conductors_to_polygons_result : type_conductor_to_polygons_result;
		cropping_basket : pac_polygon_list.list;

		half_linewidth_float : constant type_float_positive :=
			0.5 * type_float_positive (linewidth);

		
		-- Iterates the islands and assigns them to the given zone.
		-- Computes the outer edges of the islands according to the
		-- border-width of the candidate zone:
		procedure set_islands is
			
			procedure query_island (i : in pac_polygon_list.cursor) is 
				island_centerline : type_polygon renames element (i);
			begin
				zone.islands.append ((
					shore		 => (
						centerline => island_centerline,
						outer_edge => offset_polygon (island_centerline, half_linewidth_float)),
					others		 => <>)); 

				-- CS: Currently, the outer edge is just an expansion of the centerline.
				-- The border is drawn with lines that have round caps. These caps cause
				-- the outer edge to have rounded corners. The computation of the outer edge
				-- should take this into account.
				
				--put_line ("islands total" & count_type'image (zone.islands.length));

				exception when constraint_error =>
					log (
						importance => WARNING,
						text => "Offsetting centerline of island failed !",
						level => log_threshold + 3);
					-- CS write centerline edges in logfile ?
					raise;			

			end query_island;
			
		begin
			islands.iterate (query_island'access);
		end set_islands;


		-- Iterates the islands, detects polygons that
		-- form the lakes (inside the islands). Lakes are caused by objects
		-- that are completely inside a particular island.
		procedure set_lakes is 
			island_cursor : pac_islands.cursor := zone.islands.first;

			procedure make_lakes (
				island : in out type_island)
			is 
				use pac_overlap_status;
				centerlines : pac_polygon_list.list;
				
				procedure query_centerline (cl : in pac_polygon_list.cursor) is
					centerline : type_polygon renames element (cl);
				begin
					if debug then
						put_line ("   centerline");
					end if;

					-- The inner edges are obtained by shrinking the centerline
					-- by half the fill linewidth:
					island.lakes.append ((
						centerline	=> centerline,
						inner_edge	=> offset_polygon (centerline, - half_linewidth_float)));
					--inner_edge	=> offset_polygon (centerline, - half_linewidth_float, true)));

					exception when constraint_error =>
						log (
							importance => WARNING,
							text => "Offsetting centerline of lake failed !",
							level => log_threshold + 3);
						-- CS write centerline edges in logfile ?
						raise;			

				end query_centerline;
				
			begin
				-- Get lakes inside the candidate island.
				-- These are the centerlines of the lakes:
				centerlines := get_polygons (
					area		=> island.shore.centerline, 
					polygons	=> cropping_basket,
					status		=> to_set (B_INSIDE_A));

				-- Iterate the centerlines and compute the inner edges.
				centerlines.iterate (query_centerline'access);
			end make_lakes;					
				
		begin
			while island_cursor /= pac_islands.no_element loop

				if debug then
					put_line ("  island");
				end if;

				zone.islands.update_element (island_cursor, make_lakes'access);
				next (island_cursor);
			end loop;					
		end set_lakes;

		
		-- Fills the islands according to the fill style
		-- of the given zone:
		procedure fill_islands is 
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


		procedure make_thermal_reliefes is
		begin
			log (text => "making thermal reliefes", level => log_threshold + 4);

			-- Delete all reliefes left over from the previous zone:
			terminal_reliefes.clear;
			
			terminal_reliefes := make_reliefes (
				zone				=> zone,
				relief_properties	=> relief_properties,								   
				terminals			=> conductors_to_polygons_result.terminals_with_relief,
				zone_clearance		=> clearance,
				zone_linewidth		=> linewidth);

		end make_thermal_reliefes;

		
	begin -- fill_zone
		log (text => "zone with corner nearest to origin:" 
			 & to_string (get_corner_nearest_to_origin (zone)),
			level => log_threshold + 3);


		if debug then
			put_line ("fill zone");
		end if;
		
		log_indentation_up;

		--if parent_net /= pac_nets.no_element then
			--log (text => "parent net " 
				--& enclose_in_quotes (to_string (key (parent_net))),
				 --level => log_threshold + 3);
		--end if;

		
		-- Remove the old fill (incl. islands, lakes and thermal reliefes):
		zone.islands := no_islands;

		
		-- Convert the contour of the candidate fill zone to a polygon:
		if debug then
			put_line (" convert contour to polygon");
		end if;
		
		zone_polygon := to_polygon (zone, fill_tolerance, SHRINK); 
		-- NOTE: The SHRINK argument applies to the approximation mode of 
		-- arcs and circles. Has nothing to do with the actual shrinking of the zone
		-- by the follwing statement.


		log (text => "shrinking zone", level => log_threshold + 4);

		-- The border of the zone is drawn with the given linewidth.
		-- The zone must be shrinked by half the linewidth so that the 
		-- outline of the border is congruent to the zone drawn by the operator:
		offset_polygon (zone_polygon, - type_float_positive (linewidth) * 0.5);

		-- CS log lowest left vertex

		-- The holes inside the board area and inside the zone
		-- will crop the zone later. Since the zone border has a linewidth,
		-- the holes must be made greater than they acutually are:
		expand_holes (zone_polygon, linewidth); -- updates variable "holes"
		
		-- Crop the zone by the outer board edges and the holes (stored in
		-- variable "holes").
		-- As a result, the zone disintegrates. It gets fragmented into islands:
		islands := zone_to_polygons (
			zone		=> zone_polygon,
			line_width	=> linewidth);

		
		-- Now we start collecting contours of objects inside the zone.
		-- They will be put in the cropping basket.
		-- Later everything in the basket will be used to crop the islands (of the zone)
		-- and to create inner borders inside the islands:
		empty_basket (cropping_basket);
		
		-- Collect holes in basket:
		put_into_basket (cropping_basket, holes);

		
		-- Get the contours of all conductor objects in the affected layer
		-- and in the zone_polygon.
		-- This is about tracks, terminals, vias, texts and fiducials.
		-- The clearance of these objects to the zone is determined by
		-- the zone isolation or the net clearance. The greater value is applied:
		conductors_to_polygons_result := conductors_to_polygons (
			zone_polygon	=> zone_polygon,
			zone_clearance	=> clearance,
			linewidth		=> linewidth,									 
			layer			=> layer,
			parent_net		=> parent_net);

		put_into_basket (cropping_basket, conductors_to_polygons_result.polygons);


		
		-- Crop the islands by all cutout areas in the affected layer.
		cutouts := cutouts_to_polygons (layer);
		put_into_basket (cropping_basket, cutouts);
		--put_line ("A4");
		
		
		-- Crop the islands by all route restrict objects in the affected layer.
		restrict := restrict_to_polygons (layer);
		put_into_basket (cropping_basket, restrict);
		--put_line ("A5");
		

		-- Union the content of the cropping basket as much as possible:
		if debug then
			put_line (" union cropping basket");
		end if;
		
		--multi_union (cropping_basket, debug);
		multi_union (cropping_basket);
		
		-- Now the basket is ready to crop its content with the islands.
		-- The result are even more islands:

		--put_line ("B");


		-- CS: experimental
		--multi_union_2 (cropping_basket, debug); -- debug messages on
		--multi_union (cropping_basket);

		if debug then
			put_line (" crop islands with cropping basket");
		end if;
		
		islands := multi_crop_2 (
			polygon_B_list	=> islands,
			polygon_A_list	=> cropping_basket,
			debug			=> false);


		
		-- Assign the islands to the zone:
		if debug then
			put_line (" set islands");
		end if;

		set_islands;


		
		-- Assign lakes to the islands of the zone:
		if debug then
			put_line (" set lakes");
		end if;

		set_lakes;



		
		if parent_net /= pac_nets.no_element then

			if debug then
				put_line (" make thermal reliefes");
			end if;
			
			make_thermal_reliefes; 
			-- bases on the inner borders that we just computed. see statement above
		end if;



		
		-- Fill the islands with stripes:
		if debug then
			put_line (" fill islands");
		end if;

		fill_islands;


		
		log_indentation_down;
		
	end fill_zone;

	
	

	
	procedure floating_zones is
		use pac_floating_solid;
		use pac_floating_hatched;
		

		procedure floating_solid (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			zone_cursor : pac_floating_solid.cursor := module.board.conductors.fill_zones.solid.first;

			procedure do_it (
				zone : in out type_floating_solid)
			is begin
				fill_zone (
					zone		=> zone,
					linewidth	=> element (zone_cursor).linewidth,
					layer		=> zone.properties.layer,
					clearance	=> zone.isolation);
			end do_it;

		begin
			while zone_cursor /= pac_floating_solid.no_element loop
				module.board.conductors.fill_zones.solid.update_element (zone_cursor, do_it'access);
				next (zone_cursor);
			end loop;
		end floating_solid;

		
		procedure floating_hatched (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			zone_cursor : pac_floating_hatched.cursor := module.board.conductors.fill_zones.hatched.first;

			procedure do_it (
				zone : in out type_floating_hatched)
			is begin
				fill_zone (
					zone		=> zone,
					linewidth	=> element (zone_cursor).linewidth,
					layer		=> zone.properties.layer,
					clearance	=> zone.isolation);
			end do_it;
			
		begin
			while zone_cursor /= pac_floating_hatched.no_element loop
				module.board.conductors.fill_zones.hatched.update_element (zone_cursor, do_it'access);
				next (zone_cursor);
			end loop;
		end floating_hatched;

		
	begin -- floating_zones
		log (text => "floating zones ...", level => log_threshold + 1);
		update_element (generic_modules, module_cursor, floating_solid'access);
		update_element (generic_modules, module_cursor, floating_hatched'access);
	end floating_zones;


	
	-- Fills polygons that are connected with a net:
	procedure connected_zones is
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


				procedure do_it (
					zone : in out type_route_solid)
				is 
					-- Backup the zone in case something went wrong:
					zone_bakup : constant type_route_solid := zone; 
				begin
					-- load temporarily variables of zone properties:
					relief_properties := zone.relief_properties;
					terminal_connection	:= zone.connection;
					native_tracks_embedded := zone.native_tracks_embedded;

					if zone.connection = SOLID then
						terminal_technology	:= zone.technology;
					end if;

					
					fill_zone (
						zone		=> zone,
						linewidth	=> element (zone_cursor).linewidth,
						layer		=> zone.properties.layer,
						clearance	=> get_greatest (zone.isolation, net_class.clearance),
						parent_net	=> net_cursor,
						debug		=> false
						--debug		=> true
						);

					zone.reliefes := terminal_reliefes;

					-- If something went wrong, output some
					-- helpful information and restore the zone:
					exception when event:
						others =>
						log (
							importance => WARNING,
							text => exception_information (event));
					
						log (
							importance => WARNING,
							text => "Zone at"
								& to_string (get_corner_nearest_to_origin (zone))
								& " has NOT been filled !");
							--level => log_threshold + 3);

						-- CS log zone properties ?
						-- CS write warning in GUI status bar
						
						zone := zone_bakup;			
				end do_it;
				
				
			begin -- route_solid
				while zone_cursor /= pac_route_solid.no_element loop

					-- do the filling
					net.route.fill_zones.solid.update_element (zone_cursor, do_it'access);
				
					next (zone_cursor);
				end loop;
			end route_solid;

			
			procedure route_hatched (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is 
				use pac_route_hatched;
				zone_cursor : pac_route_hatched.cursor := net.route.fill_zones.hatched.first;


				procedure do_it (
					zone : in out type_route_hatched)
				is begin
					-- load temporarily variables of zone properties:
					relief_properties := zone.relief_properties;
					terminal_connection	:= zone.connection;
					native_tracks_embedded := zone.native_tracks_embedded;

					if zone.connection = SOLID then
						terminal_technology	:= zone.technology;
					end if;

					
					fill_zone (
						zone		=> zone,
						linewidth	=> element (zone_cursor).linewidth,
						layer		=> zone.properties.layer,
						clearance	=> get_greatest (zone.isolation, net_class.clearance),
						parent_net	=> net_cursor
						);
				end do_it;
					
				
			begin -- route_hatched				
				while zone_cursor /= pac_route_hatched.no_element loop

					-- do the filling
					net.route.fill_zones.hatched.update_element (zone_cursor, do_it'access);

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
		log (text => "connected zones ...", level => log_threshold + 1);
		log_indentation_up;
		update_element (generic_modules, module_cursor, query_nets'access);
		log_indentation_down;
	end connected_zones;


	offset_scratch : type_distance;
	
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
		mode		=> SHRINK,										 
		tolerance	=> fill_tolerance);
	
	-- Shrink the outer board edge by the conductor-to-edge clearance
	-- as given by the design rules:

	offset_scratch := - design_rules.clearances.conductor_to_board_edge;
	
	log (text => "offsetting by DRU parameter " -- CS use predefined string
		& enclose_in_quotes (dru_parameter_clearance_conductor_to_board_edge) 
		& to_string (offset_scratch),
		level => log_threshold + 1);
	
	offset_polygon (board_outer_contour_master, type_float (offset_scratch));
	-- for debuggin use:
	--offset_polygon (board_outer_contour_master, offset_scratch, true);
	-- CS consider half the line width !


	
	log (text => "converting holes to polygons ...", level => log_threshold + 1);
	
	board_holes_master := to_polygons (
		holes		=> get_holes (module_cursor),
		tolerance	=> fill_tolerance);

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
		connected_zones;

		floating_zones;

		log_indentation_down;
					
	else
		log (text => "filling zones of dedicated nets ...", level => log_threshold + 1);

		all_zones := false;
		
		log_indentation_up;
		connected_zones;
		log_indentation_down;
		
	end if;

	log_indentation_down;
	
end fill_fill_zones;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
