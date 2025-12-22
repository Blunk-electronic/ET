------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                              ROUTE                                       --
--                                                                          --
--                             S p e c                                      --
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
--   to do:
--

with et_pcb_signal_layers;				use et_pcb_signal_layers;
with et_vias;							use et_vias;

with et_fill_zones;						use et_fill_zones;
with et_fill_zones.boards;				use et_fill_zones.boards;
with et_route_restrict.boards;			use et_route_restrict.boards;
with et_conductor_segment.boards;		use et_conductor_segment.boards;

with et_board_geometry;					use et_board_geometry;

with et_ratsnest;


package et_route is

	use pac_polygons;	
	
		
	-- A complete net may consist of these conductor objects.
	-- Everything that is a conducting object is called a "route":
	
	type type_net_route is record
		airwires	: et_ratsnest.type_airwires;
		
		lines 		: pac_conductor_lines.list;
		arcs		: pac_conductor_arcs.list;
		-- CS: circles ?
		vias		: pac_vias.list;

		-- fill zones:
		zones		: boards.type_route;

		-- user defined restrictions. currently not supported. CS
		restrict	: et_route_restrict.boards.type_route_restrict;
	end record;


	
	-- Iterates the track segments and vias of the
	-- given route and converts them to polygons:
	function get_polygons (
		route 			: in type_net_route;
		layer_category 	: in type_signal_layer_category;
		layer			: in type_signal_layer;
		bottom_layer	: in type_signal_layer)
		return pac_polygon_list.list;
	

	
end et_route;



-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
