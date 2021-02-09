------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                      POLYGONS IN CONDUCTOR LAYERS                        --
--                                                                          --
--                               S p e c                                    --
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
--   to do:


with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_general;
with et_string_processing;		use et_string_processing;

with et_pcb_coordinates;		use et_pcb_coordinates;
with et_geometry;
with et_board_shapes_and_text;	use et_board_shapes_and_text;
with et_nets;					use et_nets;
with et_drills;					use et_drills;
with et_vias;					use et_vias;
with et_packages;				use et_packages;
with et_pcb_stack;				use et_pcb_stack;
--with et_frames;
with et_design_rules;			use et_design_rules;

package et_conductor_polygons is
	
	use et_pcb_coordinates.pac_geometry_brd;
	use et_board_shapes_and_text.pac_shapes;

	

	
	-- Cutout-polygons in conductor layers:
	type type_conductor_cutout is new et_packages.type_cutout_zone with record
		layer 	: type_signal_layer := type_signal_layer'first;
	end record;

	package pac_conductor_cutouts is new doubly_linked_lists (type_conductor_cutout);


	
	-- Polgon priority: 0 is weakest, 100 ist strongest.
	keyword_priority : constant string := "priority";
	
	polygon_priority_max : constant natural := 100;
	subtype type_polygon_priority is natural range natural'first .. polygon_priority_max;
	function to_string (priority_level : in type_polygon_priority) return string;
	function to_polygon_priority (priority_level : in string) return type_polygon_priority;



	-- After filling a polygon the polygon may be reduced to a single
	-- smaller filled area or it may break into several separated filled
	-- areas. These areas are always computed automatically
	-- on filling a conductor polygon.
		
	type type_filled_area is new type_polygon_base with null record;
	package pac_filled_areas is new doubly_linked_lists (type_filled_area);

	--type type_cutout_area is new type_polygon_base with null record;
	--package pac_cutout_areas is new doubly_linked_lists (type_cutout_area);

	--type type_filled_area_2 is record
		--contours	: pac_filled_areas.list;
		--cutouts		: pac_cutout_areas.list;
	--end record;

	no_filled_areas : constant pac_filled_areas.list := pac_filled_areas.empty_list;

	--no_filled_areas : constant type_filled_area_2 := (
			--contours	=> pac_filled_areas.empty_list,	
			--cutouts		=> pac_cutout_areas.empty_list);
														 
	
	-- All fill zones in conductor layers have these common
	-- properties:
	type type_conductor_polygon_properties is record
		layer 			: type_signal_layer := type_signal_layer'first;
		priority_level	: type_polygon_priority := type_polygon_priority'first;
		filled_areas	: pac_filled_areas.list;
		--filled_areas	: type_filled_area_2 := no_filled_areas;
	end record;

	
	
	-- A floating conductor polygon is not connected to any net:
	type type_polygon_conductor_solid_floating is new 
		type_polygon_conductor (fill_style => SOLID)
	with record
		properties	: type_conductor_polygon_properties;
	end record;

	package pac_conductor_polygons_floating_solid is new 
		indefinite_doubly_linked_lists (type_polygon_conductor_solid_floating);

		
	type type_polygon_conductor_hatched_floating is new 
		type_polygon_conductor (fill_style => HATCHED) 
	with record
		properties	: type_conductor_polygon_properties;
	end record;

	package pac_conductor_polygons_floating_hatched is new
		indefinite_doubly_linked_lists (type_polygon_conductor_hatched_floating);
	
	type type_conductor_polygons_floating is record
		solid	: pac_conductor_polygons_floating_solid.list;
		hatched	: pac_conductor_polygons_floating_hatched.list;
	end record;

	
-- Types for ELECTRIC !! conductor objects:


	-- A polygon in a signal layer is usually connected with 
	-- THT or SMD pads (or both) via thermals, solid (or not at all).
	-- For this reason we define a controlled type here because some
	-- properties may exist (or may not exists) depending
	-- on the kinde of pad_connection:

-- THERMALS
	keyword_thermal_width : constant string := "thermal_width";		
	keyword_thermal_gap : constant string := "thermal_gap";
	
	polygon_thermal_width_min : constant type_track_width := type_track_width'first;
	polygon_thermal_width_max : constant type_track_width := 3.0; -- CS: adjust if nessecariy
	subtype type_polygon_thermal_width is pac_geometry_brd.type_distance_positive
		range polygon_thermal_width_min .. polygon_thermal_width_max;

	-- If a terminal is connected/associated with a polyon,
	-- this is the space between pad and polygon:
	polygon_thermal_gap_min : constant type_track_clearance := type_track_clearance'first;
	polygon_thermal_gap_max : constant type_track_clearance := 3.0; -- CS: adjust if nessecariy
	subtype type_polygon_thermal_gap is type_track_clearance range polygon_thermal_gap_min .. polygon_thermal_gap_max;


	-- Polygons which are connected with a net
	-- can be connected with pads by thermals or solid:
	keyword_pad_connection : constant string := "pad_connection";
	type type_polygon_pad_connection is (THERMAL, SOLID);
	polygon_pad_connection_default : constant type_polygon_pad_connection := THERMAL;

	function to_string (polygon_pad_connection : in type_polygon_pad_connection) return string;
	function to_pad_connection (connection : in string) return type_polygon_pad_connection;

	
	-- Polygons may be connected with SMT, THT or all pad technologies
	-- CS: Is that a reasonable idea ????? it was inherited from kicad.
	keyword_pad_technology : constant string := "pad_technology";
	
	type type_polygon_pad_technology is (
		SMT_ONLY,
		THT_ONLY,
		SMT_AND_THT);

	polygon_pad_technology_default : constant type_polygon_pad_technology := SMT_AND_THT;
	
	function to_string (polygon_pad_technology : in type_polygon_pad_technology) return string;
	function to_pad_technology (technology : in string) return type_polygon_pad_technology;

	type type_thermal is record
		-- whether SMT, THT or both kinds of pads connect with the polygon
		technology	: type_polygon_pad_technology := polygon_pad_technology_default;

		-- the width of the thermal spokes
		width		: type_polygon_thermal_width := type_polygon_thermal_width'first;

		-- the space between pad and polygon -- CS: rename to thermal_length ?
		gap			: type_polygon_thermal_gap := type_polygon_thermal_gap'first;
	end record;
	
	
	type type_polygon_conductor_route_solid (connection : type_polygon_pad_connection) is new
		et_packages.type_polygon_conductor_solid
	with record
		properties	: type_conductor_polygon_properties;

		case connection is
			when THERMAL =>
				thermal : type_thermal;

			when SOLID =>
				-- whether SMT, THT or both kinds of pads connect with the polygon
				technology	: type_polygon_pad_technology;
				-- no need for any kind of thermal parameters
		end case;				
	end record;

	type type_polygon_conductor_route_hatched (connection : type_polygon_pad_connection) is new
		et_packages.type_polygon_conductor_hatched 
	with record
		properties	: type_conductor_polygon_properties;
				
		case connection is
			when THERMAL =>
				thermal : type_thermal;

			when SOLID =>
				-- whether SMT, THT or both kinds of pads connect with the polygon
				technology	: type_polygon_pad_technology;
				-- no need for any kind of thermal parameters
		end case;
	end record;


	
	package pac_signal_polygons_solid is new
		indefinite_doubly_linked_lists (type_polygon_conductor_route_solid);
	
	package pac_signal_polygons_hatched is new
		indefinite_doubly_linked_lists (type_polygon_conductor_route_hatched);	


		
	type type_signal_polygons is record
		solid	: pac_signal_polygons_solid.list;
		hatched	: pac_signal_polygons_hatched.list;
	end record;

	
-- LOGGING PROPERTIES OF OBJECTS

	function conductor_polygon_properties_to_string (
		polygon			: in type_polygon_conductor'class;
		properties		: in type_conductor_polygon_properties;

		-- Net name is relevant if polygon is part of a route.
		-- The type of the given polygon is the cirteria:
		net_name		: in pac_net_name.bounded_string := no_name)
		return string;
	
	text_polygon_thermal_width : constant string := "thermal_width";	
	text_polygon_thermal_gap : constant string := "thermal_gap";	
	text_polygon_pad_connection : constant string := "pad_connection";	
	text_polygon_pad_technology : constant string := "connected_with";	
	text_polygon_width_min : constant string := "minimum_width";	
	text_polygon_signal_layer : constant string := "signal_layer";
	

	type type_user_settings_polygons_conductor is record

		-- relevant if polygon is connected with a net:
		connection		: type_polygon_pad_connection := polygon_pad_connection_default;

		-- relevant if connection is thermal
		thermal			: type_thermal;
		
		fill_style		: type_fill_style := fill_style_default;

		-- relevant if fill style is HATCHED:
		hatching		: type_conductor_hatching;

		min_width		: type_track_width := type_track_width'first;
		isolation		: type_track_clearance := type_track_clearance'first;
		priority_level	: type_polygon_priority := type_polygon_priority'first;
		easing			: type_easing;
	end record;

	
end et_conductor_polygons;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
