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
--   ToDo: 

with et_text;
with et_conductor_segment.boards;		use et_conductor_segment.boards;
with et_fill_zones;						use et_fill_zones;
with et_fill_zones.boards;				use et_fill_zones.boards;
with et_conductor_text.boards;			use et_conductor_text.boards;
with et_pcb_placeholders;				use et_pcb_placeholders;
with et_device_name;					use et_device_name;
with et_ripup;							use et_ripup;

with et_board_geometry;
-- use et_board_geometry.pac_polygons;

with et_thermal_relief;					use et_thermal_relief;


package et_board_ops.fill_zones is

	-- CS rework procedures so that a module cursor
	-- is used instead the module_name.

	use et_board_text;
	use pac_text_board;
	use pac_net_name;
	use pac_grid;





	-- This procedure collects terminals of packages that are connected with
	-- the given net and appends them to the terminal_polygons.
	-- If the flag "with_relief" is true, then the information required
	-- to compute thermal reliefes is collected in 
	-- list "terminals_with_relief":
	procedure get_terminal_polygons ( -- CS rename to get_polygons_of_connected_terminals
		module_cursor			: in pac_generic_modules.cursor;
		
		-- This is specifies whether the affected
		-- conductor layer is a top, bottom or inner signal layer:
		layer_category 			: in type_signal_layer_category;
		
		-- This is the zone inside which therminals
		-- are searched for:
		zone_polygon			: in pac_polygons.type_polygon;
		
		-- This is the net for which terminals are searched for:
		net_cursor 				: in pac_nets.cursor;
		
		-- This is the outcome of the procedure, a list of polygons:
		terminal_polygons		: out pac_polygons.pac_polygon_list.list;
		-- CS rename to polygons, should be in out
		
		-- This flag specifies whether additional information
		-- for thermal reliefes is also to be collected:
		with_reliefes			: in boolean;
		
		-- The output providing information about thermal reliefes:
		terminals_with_relief	: out pac_terminals_with_relief.list;
		
		log_threshold			: in type_log_level);
	

	
	
	
	
	-- Extracts polygons of nets (routed tracks, terminals, vias).
	-- The polygons are expanded by the zone_clearance or by
	-- the clearance of a particular net (the greater value of them is applied).
	-- Returns only those polygons which are inside the given zone.
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
		zone					: in et_board_geometry.pac_polygons.type_polygon;

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
		-- The polygons found by the procedure are appended
		-- the given list of polygons:
		result					: in out et_board_geometry.pac_polygons.pac_polygon_list.list;
		-- CS rename to polygons ?
		
		terminal_connection		: in type_pad_connection;
		
		-- A list of terminals that require thermal reliefes:
		terminals_with_relief	: out pac_terminals_with_relief.list;
		
		log_threshold			: in type_log_level);

	
	

	
end et_board_ops.fill_zones;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
