------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       FILL ZONES IN BOARDS                               --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                -- 
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
--   to do:

with et_net_names;				use et_net_names;
with et_pcb_stack;				use et_pcb_stack;
with et_thermal_relief;			use et_thermal_relief;


package et_fill_zones.boards is
	
	
	-- priority: 0 is weakest
	zone_priority_max : constant natural := 100;

	subtype type_priority is natural range natural'first .. zone_priority_max;

	function to_string (priority_level : in type_priority) return string;
	function to_priority (priority_level : in string) return type_priority;

	
	

	type type_user_settings is record

		-- relevant if fill_zone is connected with a net:
		connection		: type_pad_connection := pad_connection_default;

		-- relevant if connection is thermal
		thermal			: type_relief_properties;
		
		fill_style		: type_fill_style := fill_style_default;

		-- relevant if fill style is HATCHED:
		spacing			: type_track_clearance := type_track_clearance'first;

		-- linewidth of borders and fill stripes:
		linewidth		: type_track_width := type_track_width'first;
		
		isolation		: type_track_clearance := type_track_clearance'first;
		priority_level	: type_priority := type_priority'first;
		easing			: type_easing;
	end record;



	

	
	
	-- All fill zones in conductor layers have these common properties:
	type type_properties is record
		layer 			: type_signal_layer := type_signal_layer'first;
		priority_level	: type_priority := type_priority'first;
	end record;

	
	function to_string (
		fill_zone		: in type_zone'class;
		properties		: in type_properties;

		-- Net name is relevant if fill_zone is part of a route.
		-- The type of the given fill_zone is the cirteria:
		net_name		: in pac_net_name.bounded_string := no_name)
		return string;



	-- CS: For the future. Primitive operations for sold and hatched zones
	-- should be defined here like:
	--procedure fill_zone (
		--zone		: in out type_zone;
		--linewidth	: in type_track_width;
		--layer 		: in et_pcb_stack.type_signal_layer;
		--clearance	: in type_track_clearance;
		--parent_net	: in pac_nets.cursor := pac_nets.no_element;
		--debug		: in boolean := false)
	--is null;
	
	
-- FLOATING FILL ZONES (not connected to any net):
	
	type type_floating_solid is new 
		type_zone (fill_style => SOLID)
	with record
		properties	: type_properties;
	end record;

	package pac_floating_solid is new 
		indefinite_doubly_linked_lists (type_floating_solid);

		
		
	type type_floating_hatched is new 
		type_zone (fill_style => HATCHED) 
	with record
		properties	: type_properties;
	end record;

	package pac_floating_hatched is new
		indefinite_doubly_linked_lists (type_floating_hatched);

		
	type type_floating is record -- CS rename to type_fill_zone_floating ?
		solid	: pac_floating_solid.list;
		hatched	: pac_floating_hatched.list;
	end record;


	


	
-- FILL ZONES CONNECTED WITH A NET (part of a route)

	-- Tracks that are not connected with a fill zone cause 
	-- cutout areas inside the zone. These tracks are foreign to the
	-- zone.
	-- In contrast, a track of a native net - connected with the zone - usually
	-- does not cause a cutout area. So the track gets embedded in the zone.
	-- After filling the zone, the track gets overpainted and becomes 
	-- invisible. It becomes visible again if the fill of the zone is cleared.
	-- However, if the user insists on not-embedding then we need
	-- a simple boolean type to express this requirement:
	type type_native_tracks_embedded is new boolean;
	
	
	type type_route_solid (connection : type_pad_connection) 
		is new type_zone_solid
	with record
		properties	: type_properties;

		-- By default, native tracks are embedded in the zone.
		-- If the connection is SOLID, then this property is ignored,
		-- means: Everything will be embedded in the zone:
		native_tracks_embedded : type_native_tracks_embedded := false;

		case connection is
			when THERMAL =>
				relief_properties	: type_relief_properties;

				-- Storage place for reliefes.
				-- Reliefes are always generated automatically
				-- depending on the terminal geometry, user specific settings, etc ...
				reliefes			: pac_reliefes.list;
				

			when SOLID =>
				-- whether SMT, THT or both kinds of pads connect with the fill_zone
				technology	: type_pad_technology;
				-- no need for any kind of thermal parameters
		end case;				
	end record;


	
	type type_route_hatched (connection : type_pad_connection) 
		is new type_zone_hatched 
	with record
		properties	: type_properties;

		-- By default, native tracks are embedded in the zone.
		-- If the connection is SOLID, then this property is ignored,
		-- means: Everything will be embedded in the zone:
		native_tracks_embedded : type_native_tracks_embedded := false;
	
		case connection is
			when THERMAL =>
				relief_properties	: type_relief_properties;

				-- Storage place for reliefes.
				-- Reliefes are always generated automatically
				-- depending on the terminal geometry, user specific settings, etc ...
				reliefes			: pac_reliefes.list;

				
			when SOLID =>
				-- whether SMT, THT or both kinds of pads connect with the fill_zone
				technology	: type_pad_technology;
				-- no need for any kind of thermal parameters
		end case;
	end record;


	
	package pac_route_solid is new
		indefinite_doubly_linked_lists (type_route_solid);
	
	package pac_route_hatched is new
		indefinite_doubly_linked_lists (type_route_hatched);	


		
	type type_route is record -- CS rename to type_fill_zone_tied ?
		solid	: pac_route_solid.list;
		hatched	: pac_route_hatched.list;
	end record;





	

-- CUTOUT ZONES (drawn by the user. areas where a zone is not to be filled).
-- These zones apply to all fill zones. 	
	
	type type_cutout is new type_contour with record
		layer 	: type_signal_layer := type_signal_layer'first;
	end record;

	package pac_cutouts is new doubly_linked_lists (type_cutout);
	
	
end et_fill_zones.boards;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
