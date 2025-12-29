------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE READ / BOARD ZONES ROUTE                       --
--                                                                          --
--                               B o d y                                    --
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
--                                                                          --
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
-- ToDo:
-- - clean up
--
--
--

with ada.text_io;					use ada.text_io;
with ada.characters;				use ada.characters;
with ada.strings;					use ada.strings;

with et_module_names;				use et_module_names;
with et_keywords;					use et_keywords;
with et_fill_zones;					use et_fill_zones;
with et_fill_zones.boards;			use et_fill_zones.boards;
with et_thermal_relief;				use et_thermal_relief;
with et_pcb;						use et_pcb;
with et_pcb_signal_layers;			use et_pcb_signal_layers;
with et_design_rules_board;			use et_design_rules_board;
with et_board_geometry;				use et_board_geometry;
with et_primitive_objects;			use et_primitive_objects;

with et_module_read_board_contour;	use et_module_read_board_contour;

with et_general_rw;					use et_general_rw;

with et_module_read_nets;



package body et_module_read_board_zones_route is

	use pac_generic_modules;
	use pac_geometry_2;
	use pac_contours;
	use pac_signal_layers;

	
	
	fill_spacing : type_track_clearance := type_track_clearance'first;
	-- CS rename to zone_fill_spacing
	
	board_fill_style : type_fill_style := fill_style_default;	
	-- CS rename to zone_fill_style
	
	board_easing : type_easing;
	-- CS rename to zone_easing
	
	signal_layer : type_signal_layer;
	-- CS rename to zone_signal_layer
	
	contour_priority : type_priority := type_priority'first;
	-- CS rename to zone_priority
	
	polygon_width_min : type_track_width := type_track_width'first;
	-- CS rename to zone_width_min
	
	polygon_isolation : type_track_clearance := type_track_clearance'first; 
	-- CS rename to zone_isolation
	-- applies to conductor zones only
		
	signal_layers : pac_signal_layers.set;
	-- CS rename to zone_signal_layers
	
	relief_properties : type_relief_properties;

	pad_connection : type_pad_connection := type_pad_connection'first;	

	

	
	

	procedure reset_scratch is begin
		fill_spacing		:= type_track_clearance'first;
		board_fill_style	:= fill_style_default;
		--board_hatching		:= (others => <>);
		board_easing 		:= (others => <>);
		relief_properties	:= (others => <>);
		pad_connection 		:= type_pad_connection'first;	
		
		contour_priority		:= type_priority'first;  -- board relevant only
		polygon_isolation		:= type_track_clearance'first;
		polygon_width_min		:= type_track_width'first;

		signal_layer			:= type_signal_layer'first;  -- board relevant only
		clear (signal_layers);

		contour := (others => <>);
		-- CS use procedure reset_contour
	end;


	


	
	
	procedure read_cutout_route (
		line : in type_fields_of_line)
	is
		kw : constant  string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_easing_style then -- easing_style none/chamfer/fillet
			expect_field_count (line, 2);
			board_easing.style := to_easing_style (f (line, 2));

		elsif kw = keyword_easing_radius then -- easing_radius 0.3
			expect_field_count (line, 2);
			board_easing.radius := to_distance (f (line, 2));

		elsif kw = keyword_layer then -- layer 2
			expect_field_count (line, 2);
			signal_layer := to_signal_layer (f (line, 2));
			-- CS validate_signal_layer;

		else
			invalid_keyword (kw);
		end if;
	end read_cutout_route;






	procedure read_fill_zone_route (
		line : in type_fields_of_line)
	is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_priority then -- priority 2
			expect_field_count (line, 2);
			contour_priority := to_priority (f (line, 2));

		elsif kw = keyword_isolation then -- isolation 0.5
			expect_field_count (line, 2);
			polygon_isolation := to_distance (f (line, 2));
			
		elsif kw = keyword_easing_style then -- easing_style none/chamfer/fillet
			expect_field_count (line, 2);
			board_easing.style := to_easing_style (f (line, 2));

		elsif kw = keyword_easing_radius then -- easing_radius 0.3
			expect_field_count (line, 2);
			board_easing.radius := to_distance (f (line, 2));

		elsif kw = keyword_fill_style then -- fill_style solid,hatched
			expect_field_count (line, 2);
			board_fill_style := to_fill_style (f (line, 2));

		elsif kw = keyword_spacing then -- spacing 1
			expect_field_count (line, 2);
			fill_spacing := to_distance (f (line, 2));

		elsif kw = keyword_layer then -- layer 2
			expect_field_count (line, 2);
			signal_layer := to_signal_layer (f (line, 2));
			-- CS validate_signal_layer;
			
		elsif kw = keyword_width then -- width 0.3
			expect_field_count (line, 2);
			polygon_width_min := to_distance (f (line, 2));

		elsif kw = keyword_pad_technology then -- pad_technology smt_only/tht_only/smt_and_tht
			expect_field_count (line, 2);
			relief_properties.technology := to_pad_technology (f (line, 2));

		elsif kw = keyword_connection then -- connection thermal/solid
			expect_field_count (line, 2);
			pad_connection := to_pad_connection (f (line, 2));
			
		elsif kw = keyword_relief_width_min then -- relief_width_min 0.3
			expect_field_count (line, 2);
			relief_properties.width_min := to_distance (f (line, 2));

		elsif kw = keyword_relief_gap_max then -- relief_gap_max 0.7
			expect_field_count (line, 2);
			relief_properties.gap_max := to_distance (f (line, 2));

		else
			invalid_keyword (kw);
		end if;
	end read_fill_zone_route;




	



	procedure build_route_polygon (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)									  
	is
		use et_thermal_relief;
		use et_module_read_nets;
		
		
		procedure solid_polygon is
			use pac_route_solid;

			procedure connection_thermal is
				p : type_route_solid (connection => THERMAL);
			begin
				load_segments (p, get_segments (contour));
				
				p.easing := board_easing;
				
				p.linewidth	:= polygon_width_min;
				p.isolation	:= polygon_isolation;
				
				p.properties.layer			:= signal_layer;
				p.properties.priority_level	:= contour_priority;
				p.relief_properties			:= relief_properties;

				pac_route_solid.append (
					container	=> route.zones.solid,
					new_item	=> p);
			end;

			
			procedure connection_solid is
				p : type_route_solid (connection => SOLID);
			begin
				load_segments (p, get_segments (contour));
				
				p.easing := board_easing;
				
				p.linewidth	:= polygon_width_min;
				p.isolation	:= polygon_isolation;
				
				p.properties.layer			:= signal_layer;
				p.properties.priority_level	:= contour_priority;
				p.technology				:= relief_properties.technology;

				pac_route_solid.append (
					container	=> route.zones.solid,
					new_item	=> p);
			end;

			
		begin -- solid_polygon
			case pad_connection is
				when THERMAL	=> connection_thermal;
				when SOLID		=> connection_solid;
			end case;
		end solid_polygon;


		
		
		procedure hatched_polygon is
			use pac_route_hatched;


			procedure connection_thermal is
				p : type_route_hatched (connection => THERMAL);
			begin
				load_segments (p, get_segments (contour));
				
				p.easing := board_easing;
				
				p.linewidth	:= polygon_width_min;
				p.isolation	:= polygon_isolation;
				
				p.properties.layer			:= signal_layer;
				p.properties.priority_level	:= contour_priority;
				p.relief_properties			:= relief_properties;
				
				pac_route_hatched.append (
					container	=> route.zones.hatched,
					new_item	=> p);
			end;

			
			procedure connection_solid is
				p : type_route_hatched (connection => SOLID);
			begin
				load_segments (p, get_segments (contour));
				
				p.easing := board_easing;
				
				p.linewidth	:= polygon_width_min;
				p.isolation	:= polygon_isolation;
				
				p.properties.layer			:= signal_layer;
				p.properties.priority_level	:= contour_priority;
				
				p.technology := relief_properties.technology;
				
				pac_route_hatched.append (
					container	=> route.zones.hatched,
					new_item	=> p);
			end;

			
		begin -- hatched_polygon
			case pad_connection is
				when THERMAL	=> connection_thermal;
				when SOLID		=> connection_solid;
			end case;
		end hatched_polygon;

		
	begin -- build_route_polygon
		-- CS log messages
		
		case board_fill_style is
			when SOLID		=> solid_polygon;
			when HATCHED	=> hatched_polygon;
		end case;

		reset_scratch; -- clean up for next polygon
	end build_route_polygon;


	


end et_module_read_board_zones_route;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
