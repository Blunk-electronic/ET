------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             NETCHANGERS                                  --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                -- 
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
-- ToDo: 
--
--



with ada.containers;				use ada.containers;
with ada.containers.ordered_maps;

-- with et_logging;					use et_logging;
with et_schematic_geometry;			use et_schematic_geometry;
with et_schematic_coordinates;		use et_schematic_coordinates;
with et_pcb_signal_layers;			use et_pcb_signal_layers;
with et_board_coordinates;
with et_board_geometry;
with et_symbol_ports;				use et_symbol_ports;
with et_symbol_shapes;				use et_symbol_shapes;
with et_directions;					use et_directions;


package et_netchangers is

	use pac_geometry_2;
	-- use pac_text_schematic;

	use pac_geometry_sch;

	
-- ID:

	netchanger_id_max : constant positive := 10000; -- CS  increase if necessary
	type type_netchanger_id is range 1 .. netchanger_id_max;


	
	function to_netchanger_id (
		id : in string) 
		return type_netchanger_id;
		

		
	function to_string (
		id : in type_netchanger_id) 
		return string;		


		
		
-- PORT NAMES:

	type type_netchanger_port_name is (MASTER, SLAVE);
	
	function to_port_name (
		name : in string) 
		return type_netchanger_port_name;
		

	function to_string (
		name : in type_netchanger_port_name)
		return string;	


	
	function opposide_port (
		port : in type_netchanger_port_name) 
		return type_netchanger_port_name;
		


		
		
-- PORT:

	type type_netchanger_port is record
		position	: type_vector_model;
		length		: type_port_length; 
		rotation	: type_rotation_model;
	end record;


	
	position_master_port_default : constant type_vector_model := (x =>  10.0, y => 0.0);
	position_slave_port_default  : constant type_vector_model := (x => -10.0, y => 0.0);

	
	
	
	
	
-- SYMBOL:
	
	type type_netchanger_symbol is record
		master_port	: type_netchanger_port := (
						position	=> position_master_port_default,
						length		=> 5.0,
						rotation	=> zero_rotation);

		slave_port	: type_netchanger_port := (
						position	=> position_slave_port_default,
						length		=> 5.0,
						rotation	=> 180.0);

		-- the arc that connects the ports
		arc	: type_symbol_arc := (type_arc (to_arc (
						center	=> (x =>  0.0, y => 0.0),
						A		=> (x => -5.0, y => 0.0),
						B		=> (x =>  5.0, y => 0.0),
						direction	=> CW))
						with port_line_width);

	end record;


	
	
	
	
	type type_netchanger is record
		position_sch	: type_object_position; -- x,y,sheet,rotation
		--symbol			: type_netchanger_symbol; -- CS for visualisation only
		
		position_brd	: et_board_geometry.pac_geometry_2.type_vector_model; -- x,y
		-- in board there is no rotation because the netchanger is just a point in x/y.
		layer			: type_signal_layer := type_signal_layer'first;
	end record;

	
	
	package pac_netchangers is new ordered_maps (
		key_type		=> type_netchanger_id,
		element_type	=> type_netchanger);


	
	type type_netchanger_ports is record
		master	: type_vector_model := position_master_port_default;
		slave	: type_vector_model := position_slave_port_default;
	end record;



	
	-- Returns the absolute x/y positions of the given netchanger.
	function netchanger_ports (
		netchanger_cursor	: in pac_netchangers.cursor)
		return type_netchanger_ports;
	
	
	
end et_netchangers;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
