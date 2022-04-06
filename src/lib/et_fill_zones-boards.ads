------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       FILL ZONES IN BOARDS                               --
--                                                                          --
--                              S p e c                                     --
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

with et_net_names;				use et_net_names;
with et_pcb_stack;				use et_pcb_stack;


package et_fill_zones.boards is

	-- priority: 0 is weakest, 100 is strongest.
	keyword_priority : constant string := "priority";
	
	fill_zone_priority_max : constant natural := 100;

	subtype type_fill_zone_priority is natural range natural'first .. fill_zone_priority_max;

	function to_string (priority_level : in type_fill_zone_priority) return string;
	function to_fill_zone_priority (priority_level : in string) return type_fill_zone_priority;

	

	-- A fill_zone in a signal layer is usually connected with 
	-- THT or SMD pads (or both) via thermals, solid (or not at all).
	-- For this reason we define a controlled type here because some
	-- properties may exist (or may not exists) depending
	-- on the kinde of pad_connection:

	
-- THERMALS
	keyword_thermal_width : constant string := "thermal_width";		
	keyword_thermal_gap : constant string := "thermal_gap";
	
	fill_zone_thermal_width_min : constant type_track_width := type_track_width'first;
	fill_zone_thermal_width_max : constant type_track_width := 3.0; -- CS: adjust if nessecariy
	subtype type_fill_zone_thermal_width is pac_geometry_brd.type_distance_positive
		range fill_zone_thermal_width_min .. fill_zone_thermal_width_max;

	-- If a terminal is connected/associated with a polyon,
	-- this is the space between pad and fill_zone:
	fill_zone_thermal_gap_min : constant type_track_clearance := type_track_clearance'first;
	fill_zone_thermal_gap_max : constant type_track_clearance := 3.0; -- CS: adjust if nessecariy
	subtype type_fill_zone_thermal_gap is type_track_clearance range fill_zone_thermal_gap_min .. fill_zone_thermal_gap_max;


	-- Polygons which are connected with a net
	-- can be connected with pads by thermals or solid:
	keyword_pad_connection : constant string := "pad_connection";
	type type_fill_zone_pad_connection is (THERMAL, SOLID);
	fill_zone_pad_connection_default : constant type_fill_zone_pad_connection := THERMAL;

	function to_string (fill_zone_pad_connection : in type_fill_zone_pad_connection) return string;
	function to_pad_connection (connection : in string) return type_fill_zone_pad_connection;

	
	-- Polygons may be connected with SMT, THT or all pad technologies
	-- CS: Is that a reasonable idea ????? it was inherited from kicad.
	keyword_pad_technology : constant string := "pad_technology";
	
	type type_fill_zone_pad_technology is (
		SMT_ONLY,
		THT_ONLY,
		SMT_AND_THT);

	fill_zone_pad_technology_default : constant type_fill_zone_pad_technology := SMT_AND_THT;
	
	function to_string (fill_zone_pad_technology : in type_fill_zone_pad_technology) return string;
	function to_pad_technology (technology : in string) return type_fill_zone_pad_technology;

	type type_thermal is record -- CS rename to type_thermal_relief ?
		-- whether SMT, THT or both kinds of pads connect with the fill_zone
		technology	: type_fill_zone_pad_technology := fill_zone_pad_technology_default;

		-- the width of the thermal relief spokes
		width		: type_fill_zone_thermal_width := type_fill_zone_thermal_width'first;

		-- the space between pad and fill_zone -- CS: rename to thermal_length ?
		gap			: type_fill_zone_thermal_gap := type_fill_zone_thermal_gap'first;
	end record;
	
	
	
	
-- LOGGING PROPERTIES OF OBJECTS

	
	text_fill_zone_thermal_width : constant string := "thermal_width";	
	text_fill_zone_thermal_gap : constant string := "thermal_gap";	
	text_fill_zone_pad_connection : constant string := "pad_connection";	
	text_fill_zone_pad_technology : constant string := "connected_with";	
	text_fill_zone_width_min : constant string := "minimum_width";	
	text_fill_zone_signal_layer : constant string := "signal_layer";
	

	type type_user_settings is record

		-- relevant if fill_zone is connected with a net:
		connection		: type_fill_zone_pad_connection := fill_zone_pad_connection_default;

		-- relevant if connection is thermal
		thermal			: type_thermal;
		
		fill_style		: type_fill_style := fill_style_default;

		-- relevant if fill style is HATCHED:
		hatching		: type_conductor_hatching;

		min_width		: type_track_width := type_track_width'first;
		isolation		: type_track_clearance := type_track_clearance'first;
		priority_level	: type_fill_zone_priority := type_fill_zone_priority'first;
		easing			: type_easing;
	end record;



	
	package pac_thermals is new doubly_linked_lists (type_line);

	type type_fill is new et_fill_zones.type_fill with record
		thermals	: pac_thermals.list;
	end record;

	
	
	-- All fill zones in conductor layers have these common properties:
	type type_properties is record
		layer 			: type_signal_layer := type_signal_layer'first;
		priority_level	: type_fill_zone_priority := type_fill_zone_priority'first;
		fill			: type_fill;
	end record;

	
	function conductor_fill_zone_properties_to_string (
		fill_zone		: in type_zone'class;
		properties		: in type_properties;

		-- Net name is relevant if fill_zone is part of a route.
		-- The type of the given fill_zone is the cirteria:
		net_name		: in pac_net_name.bounded_string := no_name)
		return string;

	
	
	-- A floating conductor fill_zone is not connected to any net:
	type type_solid_floating is new 
		type_zone (fill_style => SOLID)
	with record
		properties	: type_properties;
	end record;

	package pac_floating_solid is new 
		indefinite_doubly_linked_lists (type_solid_floating);

		
		
	type type_hatched_floating is new 
		type_zone (fill_style => HATCHED) 
	with record
		properties	: type_properties;
	end record;

	package pac_floating_hatched is new
		indefinite_doubly_linked_lists (type_hatched_floating);

		
	type type_floating is record
		solid	: pac_floating_solid.list;
		hatched	: pac_floating_hatched.list;
	end record;



	type type_solid_route (connection : type_fill_zone_pad_connection) 
		is new type_fill_zone_solid
	with record
		properties	: type_properties;

		case connection is
			when THERMAL =>
				thermal : type_thermal;

			when SOLID =>
				-- whether SMT, THT or both kinds of pads connect with the fill_zone
				technology	: type_fill_zone_pad_technology;
				-- no need for any kind of thermal parameters
		end case;				
	end record;

	
	type type_hatched_route (connection : type_fill_zone_pad_connection) 
		is new type_fill_zone_hatched 
	with record
		properties	: type_properties;
				
		case connection is
			when THERMAL =>
				thermal : type_thermal;

			when SOLID =>
				-- whether SMT, THT or both kinds of pads connect with the fill_zone
				technology	: type_fill_zone_pad_technology;
				-- no need for any kind of thermal parameters
		end case;
	end record;


	
	package pac_solid_route is new
		indefinite_doubly_linked_lists (type_solid_route);
	
	package pac_hatched_route is new
		indefinite_doubly_linked_lists (type_hatched_route);	


		
	type type_route is record
		solid	: pac_solid_route.list;
		hatched	: pac_hatched_route.list;
	end record;




	
	
	type type_conductor_cutout 
		is new type_contour with
	record
		layer 	: type_signal_layer := type_signal_layer'first;
	end record;

	package pac_conductor_cutouts is new doubly_linked_lists (type_conductor_cutout);

	
end et_fill_zones.boards;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
