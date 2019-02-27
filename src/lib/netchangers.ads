------------------------------------------------------------------------------
--                                                                          --
--                          SYSTEM ET NETCHANGERS                           --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo: 


-- with ada.text_io;				use ada.text_io;
-- with ada.strings.maps;			use ada.strings.maps;
-- with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers;            use ada.containers;
-- with ada.containers.vectors;
with ada.containers.doubly_linked_lists;
-- with ada.containers.indefinite_doubly_linked_lists;
-- with ada.containers.ordered_maps;
-- with ada.containers.indefinite_ordered_maps;
-- with ada.containers.ordered_sets;

with et_general;
with et_coordinates;		use et_coordinates;
with et_libraries;			use et_libraries;
with et_pcb;
with et_pcb_coordinates;

package netchangers is

	procedure dummy;

	type type_port is record
		position	: type_2d_point;
		length		: type_port_length; 
		rotation	: et_coordinates.type_angle;
	end record;
	
	type type_netchanger_symbol is record
		-- the port on the right
		master_port	: type_port := (
						position	=> type_2d_point (set_point (x => 10.0, y => 0.0)),
						length		=> 5.0,
						rotation	=> 0.0);

		-- the port on the left
		slave_port	: type_port := (
						position	=> type_2d_point (set_point (x => -10.0, y => 0.0)),
						length		=> 5.0,
						rotation	=> 180.0);

		-- the arc that connects the ports
		arc	: et_libraries.type_arc := (
						center		=> type_2d_point (set_point (x => 0.0, y => 0.0)),
						radius		=> 5.0,
						start_point	=> type_2d_point (set_point (x => -5.0, y => 0.0)),
						end_point	=> type_2d_point (set_point (x =>  5.0, y => 0.0)),
						width		=> line_width_port_default);
	end record;

	type type_netchanger is record
		position_sch	: et_coordinates.type_coordinates; -- x,y,sheet
		symbol			: type_netchanger_symbol;
		
		position_brd	: et_pcb_coordinates.type_point_2d; -- x,y
		signal_layer	: et_pcb.type_signal_layer := et_pcb.type_signal_layer'first;
	end record;

	-- A module connector connects the parent module with the submodule.
	-- The connector consists of an external port and and internal port.
	-- The external port connects with the parent module. The internal port connects
	-- with the submodule.
-- 	type type_module_connector is record
-- 		external	: type_port := (
-- 						
-- 						-- the position is relative to the module center:
-- 						position	=> type_2d_point (set_point (x => 0.0, y => 0.0)),
-- 						length		=> 5.0,
-- 						rotation	=> 0.0);
-- 
-- 		internal	: type_port := (
-- 						
-- 						-- the position is somewhere in the submodule:
-- 						position	=> et_coordinates.type_coordinates, -- x,y,sheet
-- 						length		=> 5.0,
-- 						rotation	=> 0.0);
-- 		
-- 		-- CS symbol			: type_netchanger_symbol;
-- 		
-- 		position_brd	: et_pcb_coordinates.type_point_2d; -- x,y
-- 		signal_layer	: et_pcb.type_signal_layer := et_pcb.type_signal_layer'first;
-- 	end record;

-- CS
-- 	type type_module_connector_symbol is record
-- 		circle	: et_libraries.type_circle := (
-- 						center		=> type_2d_point (set_point (x => 0.0, y => 0.0)),
-- 						radius		=> 2.0,
-- 						width		=> line_width_port_default);
-- 	end record;
	
	type type_submodule_port is record
-- 		position : type_port := (
						
		-- The position relative to the module center of the parent module:
-- 			position	=> et_coordinates.type_2d_point (et_coordinates.set_point (x => 0.0, y => 0.0)),
-- 			length		=> 5.0,
-- 			rotation	=> 0.0);

		position : et_coordinates.type_2d_point;
		
		-- The net of the submodule is here the port name:
		name : et_general.type_net_name.bounded_string; -- CLOCK_GENERATOR_OUT

		-- CS symbol : type_module_connector_symbol;
	end record;

	package type_submodule_ports is new doubly_linked_lists (type_submodule_port);
	
end netchangers;

-- Soli Deo Gloria
