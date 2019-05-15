------------------------------------------------------------------------------
--                                                                          --
--                          SYSTEM ET SUBMODULES                            --
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
with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers;            use ada.containers;
-- with ada.containers.vectors;
with ada.containers.doubly_linked_lists;
-- with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
-- with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_general;
with et_coordinates;		use et_coordinates;
with et_libraries;			use et_libraries;
with et_pcb;
with et_pcb_coordinates;

package submodules is

	type type_submodule_size is record
		x, y : et_coordinates.type_distance; -- size x/y of the box
	end record;

	function to_submodule_size (size : in type_submodule_size) return string;

	function at_edge (
	-- Returns true if the given point sits at the edge of a submodule box.
		point	: in et_coordinates.type_point;
		size	: in submodules.type_submodule_size)
		return boolean;
	
	type type_submodule_view_mode is (
		ORIGIN,		-- device names and net names displayed as drawn in the generic submodule
		INSTANCE	-- device names and net names displayed after renumbering and prefixing
		);

	function to_string (view : in type_submodule_view_mode) return string;
	function to_view_mode (mode : in string) return type_submodule_view_mode;
	
	submodule_path_length_max : constant positive := 300;

	-- The full name of a submodule like $ET_TEMPLATES/motor_driver.mod
	package type_submodule_path is new generic_bounded_length (submodule_path_length_max);
	function to_submodule_path (path : in string) return type_submodule_path.bounded_string;
	function to_string (path : in submodules.type_submodule_path.bounded_string) return string;
	
	type type_submodule_port is record
		-- the position somewhere at the edge of the box
		position : et_coordinates.type_point;
		
		-- CS symbol : type_module_connector_symbol;
	end record;
	
	package type_submodule_ports is new ordered_maps (
		element_type	=> type_submodule_port,
		-- The net inside the submodule is here the port name:
		"<"				=> et_general.type_net_name."<",
		key_type		=> et_general.type_net_name.bounded_string); -- CLOCK_GENERATOR_OUT

	procedure move_ports (
	-- Moves the given ports by the given offset.
		ports	: in out type_submodule_ports.map; -- the portlist
		offset	: in et_coordinates.type_coordinates); -- the offset (only x/y matters)

	-- THIS IS THE GRAPHICAL REPRESENTATION OF A SUBMODULE ->
	-- THE RECTANGULAR BOX AT THE SHEET WHERE IT THE SUBMODULE IS INSTANTIATED.
	type type_submodule is record
		file				: type_submodule_path.bounded_string; -- $ET_TEMPLATES/motor_driver.mod
		position		    : et_coordinates.type_coordinates;
		size				: type_submodule_size;
		position_in_board	: et_pcb_coordinates.type_point_2d_with_angle;
		view_mode			: type_submodule_view_mode := ORIGIN;
		device_names_offset	: et_libraries.type_device_name_index := 1000;	-- R88 turns to R1088
		ports				: type_submodule_ports.map;
	end record;

	package type_submodules is new ordered_maps (
		-- The instance name like MOT_DRV_3:
		key_type		=> et_general.type_module_instance_name.bounded_string, 
		"<" 			=> et_general.type_module_instance_name."<",
		element_type	=> type_submodule);

	netchanger_id_max : constant positive := 10000; -- CS  increase if necessary
	type type_netchanger_id is range 1 .. netchanger_id_max;

	function to_netchanger_id (id : in string) return type_netchanger_id;
	function to_string (id : in type_netchanger_id) return string;		
	
	type type_netchanger_port_name is (MASTER, SLAVE);

	function to_port_name (name : in string) return type_netchanger_port_name;
	function to_string (name : in type_netchanger_port_name) return string;	
	
	type type_netchanger_port is record
		position	: type_point;
		length		: type_port_length; 
		rotation	: et_coordinates.type_rotation;
	end record;

	position_master_port_default : constant type_point := type_point (set_point (x =>  10.0, y => 0.0));
	position_slave_port_default  : constant type_point := type_point (set_point (x => -10.0, y => 0.0));	
	
	type type_netchanger_symbol is record
		master_port	: type_netchanger_port := (
						position	=> position_master_port_default,
						length		=> 5.0,
						rotation	=> 0);

		slave_port	: type_netchanger_port := (
						position	=> position_slave_port_default,
						length		=> 5.0,
						rotation	=> 180);

		-- the arc that connects the ports
		arc	: et_libraries.type_arc := (
						center		=> type_point (set_point (x => 0.0, y => 0.0)),
						radius		=> 5.0,
						start_point	=> type_point (set_point (x => -5.0, y => 0.0)),
						end_point	=> type_point (set_point (x =>  5.0, y => 0.0)),
						width		=> line_width_port_default);
	end record;

	type type_netchanger is record
		position_sch	: et_coordinates.type_coordinates; -- x,y,sheet
		rotation		: et_coordinates.type_rotation := 0;
		--symbol			: type_netchanger_symbol; -- CS for visualisation only
		
		position_brd	: et_pcb_coordinates.type_point_2d; -- x,y
		-- in board there is no rotation because the netchanger is just a point in x/y.
		layer			: et_pcb.type_signal_layer := et_pcb.type_signal_layer'first;
	end record;

	package type_netchangers is new ordered_maps (
		key_type		=> type_netchanger_id,
		element_type	=> type_netchanger);

	type type_netchanger_ports is record
		master	: et_coordinates.type_point := position_master_port_default;
		slave	: et_coordinates.type_point := position_slave_port_default;
	end record;
	
	function netchanger_ports (
	-- Returns the absolute x/y positions of the given netchanger.
		netchanger_cursor	: in type_netchangers.cursor)
		return type_netchanger_ports;
	
	-- A module connector connects the parent module with the submodule.
	-- The connector consists of an external port and and internal port.
	-- The external port connects with the parent module. The internal port connects
	-- with the submodule.
-- 	type type_module_connector is record
-- 		external	: type_port := (
-- 						
-- 						-- the position is relative to the module center:
-- 						position	=> type_point (set_point (x => 0.0, y => 0.0)),
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
-- 						center		=> type_point (set_point (x => 0.0, y => 0.0)),
-- 						radius		=> 2.0,
-- 						width		=> line_width_port_default);
-- 	end record;
	
	
end submodules;

-- Soli Deo Gloria
