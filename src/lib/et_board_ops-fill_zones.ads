------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     BOARD OPERATIONS / FILL ZONES                        --
--                                                                          --
--                               S p e c                                    --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
-- DESCRIPTION:
--
-- This package contains subprograms which are required in
-- order to handle conductor zones. Other CAE tool refer to them
-- as "polygons" or "pour area".
--
--   ToDo: 
--


with et_devices_electrical;				use et_devices_electrical;
with et_thermal_relief;					use et_thermal_relief;
with et_fill_zones;						use et_fill_zones;


package et_board_ops.fill_zones is

	use pac_geometry_brd;
	use pac_polygons;

	

	-- This controlled type is used by the functon to_polygon below:
	type type_terminal_polygon (exists : boolean) is record
		case exists is
			when TRUE	=> 
				polygon		: type_polygon;
				position	: type_terminal_position_fine;
				
			when FALSE	=> null;
		end case;
	end record;

	
	-- Returns the position of a terminal and its contour as a polygon.
	-- If the terminal does not affect the given layer category,
	-- then nothing happens here -> returns just a "false".
	-- See specification of type_terminal_polygon above.
	function get_terminal_polygon (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in pac_devices_electrical.cursor;
		terminal_cursor	: in pac_terminals.cursor;
		layer_category	: in type_signal_layer_category;
		tolerance		: in type_distance_positive;
		log_threshold	: in type_log_level)
		return type_terminal_polygon;

	


	-- This procedure searches for terminals of packages that are 
	-- connected with the given net and appends them to the output "polygons".
	-- If the flag "with_relief" is true, then the information required
	-- to compute thermal reliefes is collected and output in
	-- list "terminals_with_relief":
	procedure get_polygons_of_connected_terminals (
		module_cursor			: in pac_generic_modules.cursor;
		
		-- This is specifies whether the affected
		-- conductor layer is a top, bottom or inner signal layer:
		layer_category 			: in type_signal_layer_category;
		
		-- This is the zone that is to be filled.
		-- We will be searching for therminals of device packages
		-- that overlap the zone or are inside the zone:
		zone					: in pac_polygons.type_polygon;

		-- This is the offset by which the polygons must be exapnded:
		offset					: in type_float_positive;
		
		-- This is the net for which terminals are searched for:
		net_cursor 				: in pac_nets.cursor;
		
		-- This is the outcome of the procedure, a list of polygons:
		polygons				: in out pac_polygon_list.list;
		
		-- This flag specifies whether additional information
		-- for thermal reliefes is also to be collected:
		with_reliefes			: in boolean;
		
		-- The output providing information about thermal reliefes:
		terminals_with_relief	: out pac_terminals_with_relief.list;
		
		log_threshold			: in type_log_level);
	

	
	
	
	
	-- Extracts polygons of nets (routed tracks, terminals, vias).
	-- The polygons are expanded by the zone_clearance or by
	-- the clearance of a particular net (the greater value of them is applied)
	-- and appended to the argument "polygons".
	-- Appends only those polygons which are inside the given zone.
	-- As a byproduct, the et_thermal_relief also contains a list of 
	-- terminals that require thermal reliefes. If the zone is not 
	-- connected with the given parent_net, then no thermal reliefes
	-- are generated (terminals_with_relief is empty):
	procedure get_polygons_of_nets (
		module_cursor			: in pac_generic_modules.cursor;
		
		-- This specifies whether the affected
		-- conductor layer is a top, bottom or inner signal layer:
		layer_category 			: in type_signal_layer_category;
		
		-- This is the zone inside which objects are searched for:
		zone					: in type_polygon;

		-- This is the linewidth used for the zone contour
		-- and the fill lines:
		linewidth				: in type_track_width;

		-- The targeted signal layer:		
		layer 					: in type_signal_layer;

		-- The clearance of the zone to foreign objects:
		zone_clearance			: in type_track_clearance;

		-- The deepest conductor layer of the board:
		bottom_layer			: in type_signal_layer;

		-- The net that the zone is connected with.
		-- If no_element, then the zone is assumed to be floating:
		parent_net				: in pac_nets.cursor;
		
		-- This is the outcome of the procedure.
		-- The polygons found by the procedure are appended here:
		polygons				: in out pac_polygon_list.list;
		
		terminal_connection		: in type_pad_connection;
		
		-- A list of terminals that require thermal reliefes:
		terminals_with_relief	: out pac_terminals_with_relief.list;
		
		log_threshold			: in type_log_level);

	
	



	-- IMPORTANT: This procedure addresses only electrical devices.
	-- It collects terminals of packages that are not
	-- connected with any net, converts them to polygons
	-- and appends them to "polygons":
	procedure get_polygons_of_unconnected_terminals (
		module_cursor			: in pac_generic_modules.cursor;
		
		-- This is specifies whether the affected
		-- conductor layer is a top, bottom or inner signal layer:
		layer_category 			: in type_signal_layer_category;
		
		-- This is the zone inside which therminals
		-- are searched for:
		zone					: in pac_polygons.type_polygon;

		-- The clearance of the zone to foreign objects:
		zone_clearance			: in type_track_clearance;

		-- This is the linewidth used for the zone contour
		-- and the fill lines:
		linewidth				: in type_track_width;
		
		-- This is the outcome of the procedure, a list of polygons:
		polygons				: in out pac_polygons.pac_polygon_list.list;
		
		log_threshold			: in type_log_level);
	
	

	
	-- IMPORTANT: This procedure addresses non-electrical devices only.
	-- It extracts the contours of all conducting objects (terminals, lines,
	-- arcs, circles, route restrict and holes),
	-- offsets each of then and appends them to the result "polygons":
	procedure get_polygons_of_non_electrical_devices (
		module_cursor			: in pac_generic_modules.cursor;

		-- This is specifies whether the affected
		-- conductor layer is a top or, bottom or inner signal layer:
		layer_category 			: in type_signal_layer_category;
		
		-- This is the zone inside which texts are searched for:
		zone					: in pac_polygons.type_polygon;
		
		-- The clearance of the zone to foreign objects:
		zone_clearance			: in type_track_clearance;

		-- This is the linewidth used for the zone contour
		-- and the fill lines:
		linewidth				: in type_track_width;

		-- The clearance between conductor and board edge:
		clearance_to_edge		: in type_distance_positive;
		
		-- This is the outcome of the procedure, a list of polygons:
		polygons				: in out pac_polygons.pac_polygon_list.list;
		
		log_threshold			: in type_log_level);





	-- IMPORTANT: This procedure addresses electrical devices only.
	-- It extracts the contours of all conducting objects (lines,
	-- arcs, circles, route restrict and holes),
	-- offsets each of then and appends them to the result "polygons".
	-- NOTE: Terminals are not processed here (See procedure
	-- get_polygons_of_unconnected_terminals):
	procedure get_polygons_of_electrical_devices (
		module_cursor			: in pac_generic_modules.cursor;

		-- This is specifies whether the affected
		-- conductor layer is a top or, bottom or inner signal layer:
		layer_category 			: in type_signal_layer_category;
		
		-- This is the zone inside which texts are searched for:
		zone					: in pac_polygons.type_polygon;
		
		-- The clearance of the zone to foreign objects:
		zone_clearance			: in type_track_clearance;

		-- This is the linewidth used for the zone contour
		-- and the fill lines:
		linewidth				: in type_track_width;

		-- The clearance between conductor and board edge:
		clearance_to_edge		: in type_distance_positive;
		
		-- This is the outcome of the procedure, a list of polygons:
		polygons				: in out pac_polygons.pac_polygon_list.list;
		
		log_threshold			: in type_log_level);

	

	

	-- This procedure collects board texts, converts them to polygons
	-- and appends them to "polygons":
	procedure get_polygons_of_board_texts (
		module_cursor			: in pac_generic_modules.cursor;
		
		-- This is the zone inside which texts are searched for:
		zone					: in pac_polygons.type_polygon;
		
		-- The clearance of the zone to foreign objects:
		zone_clearance			: in type_track_clearance;

		-- This is the linewidth used for the zone contour
		-- and the fill lines:
		linewidth				: in type_track_width;
		
		-- The targeted signal layer:		
		layer 					: in type_signal_layer;
		
		-- This is the outcome of the procedure, a list of polygons:
		polygons				: in out pac_polygons.pac_polygon_list.list;
		
		log_threshold			: in type_log_level);
	




	

	-- type type_conductor_to_polygons_result is record
	-- 	polygons				: pac_polygon_list.list;
	-- 	terminals_with_relief	: pac_terminals_with_relief.list;
	-- end record;
	
	
	-- Outputs a list of polygons caused by conductor
	-- objects (tracks, terminals, vias, texts, fiducials),
	-- holes, restrict areas, cutouts and foreign fill zones
	-- which touch the given zone.
	-- As a byproduct, outputs also a list of terminals
	-- that require thermal reliefes. If the zone is not connected 
	-- with the given parent_net, then the list "terminals_with_relief" 
	-- is empty:
	procedure get_touching_polygons (
		module_cursor		: in pac_generic_modules.cursor;
		zone_polygon		: in type_polygon; -- CS rename to zone
		zone_clearance		: in type_track_clearance;
		linewidth			: in type_track_width;
		layer 				: in type_signal_layer;
		parent_net			: in pac_nets.cursor := pac_nets.no_element;
		terminal_connection	: in type_pad_connection;
		clearance_to_edge 	: in type_distance_positive;

		polygons				: out pac_polygon_list.list;
		terminals_with_relief	: out pac_terminals_with_relief.list;
		
		log_threshold		: in type_log_level);



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
		module_cursor		: in pac_generic_modules.cursor;

		-- This is the zone to be filled. It has been defined by the
		-- operator. It will be clipped by the outer contour:
		zone				: in out type_zone'class;

		-- The is the outer contour that restricts/clips the given zone:
		outer_contour		: in type_polygon;
		
		linewidth			: in type_track_width;
		layer 				: in et_pcb_stack.type_signal_layer;
		clearance			: in type_track_clearance; -- CS rename to isolation
		clearance_to_edge 	: in type_distance_positive;
		parent_net			: in pac_nets.cursor := pac_nets.no_element;
		terminal_connection	: in type_pad_connection;
		relief_properties	: in type_relief_properties;
		reliefes			: out pac_reliefes.list;
		log_threshold		: in type_log_level);




	-- Fills fill zones. If nets is empty, then all
	-- zones will be filled (even those who are floating).
	-- If nets contains net names then only the zones of these
	-- nets will be filled:
	procedure fill_zones (
		module_cursor	: in pac_generic_modules.cursor;	
		log_category	: in type_log_category;
		log_threshold	: in type_log_level;
		nets 			: in pac_net_names.list := no_net_names); -- GND, GNDA, P3V3, ...



	-- This procedure adds a fill zone to the board:
	procedure add_zone (
		module_cursor	: in pac_generic_modules.cursor;
		zone			: in type_zone'class;
		log_threshold	: in type_log_level;

		-- Net name is relevant if filil zone is part of a route.
		-- The type of the given fill zone is the cirteria:
		net_name		: in pac_net_name.bounded_string := et_net_names.no_name);



	-- Clears fill zones. If nets is empty, then all
	-- zones will be cleared (even those who are floating).
	-- If nets contains net names then only the zones of these
	-- nets will be filled:
	procedure clear_zones (
		module_cursor	: in pac_generic_modules.cursor;	
		log_threshold	: in type_log_level;
		nets 			: in pac_net_names.list := no_net_names); -- GND, GNDA, P3V3, ...

	
end et_board_ops.fill_zones;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
