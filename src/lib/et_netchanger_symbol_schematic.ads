------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                     NETCHANGER SYMBOL IN SCHEMATIC                       --
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

with et_net_linewidth;				use et_net_linewidth;
with et_schematic_geometry;			use et_schematic_geometry;
with et_symbol_port_measures;		use et_symbol_port_measures;
with et_symbol_shapes;				use et_symbol_shapes;
with et_directions;					use et_directions;


package et_netchanger_symbol_schematic is

	use pac_geometry_2;


		
		
-- PORT:

	-- A port is basically a line with a linewidth equal to those
	-- of net segments (global constant net_linewidth).
	-- Its start point is the port position.
	-- At the start point a net will be attached.
	-- The end point points towards the symbol body. Depending on the port
	-- rotation the end tail points:
	--  to the left if rotation is 0 degree. net attached from the right.
	--  to the right if rotation is 180 degree. net attached from the left.
	--  downwards if the rotation is 90 degree. net attached from above.
	--  upwards if the rotation is 270 degree. net attached from below.

	type type_netchanger_port is record
		-- This is the place where a net is connected:
		position	: type_vector_model;
		
		-- From the position a line starts.
		-- This line represents a port.
		-- The linewidth is the global constant net_linewidth:
		length		: type_port_length := 5.0; 
		
		rotation	: type_rotation_model;
		--  90.0 -- to be connected with a net from above,
		-- -90.0 -- from below,
		-- 180.0 -- from the left,
		--   0.0 -- from the right
	end record;


	
	position_master_port_default : constant 
		type_vector_model := (x =>  7.5, y => 0.0);
	
	
	position_slave_port_default  : constant 
		type_vector_model := (x => -7.5, y => 0.0);

	-- CS: instead of master/slave notation use A/B ?
	
	
	
	-- CS use subtype defined in et_symbol_port_measures:
	
	name_to_origin_offset : constant type_distance_positive := 2.0;
	
	-- The distance between the start point of a port and the
	-- origin of the port name:
	port_name_spacing_start : constant type_distance_positive := 1.7;
	
	-- The size of the name (like N31):
	name_size : constant type_distance_positive := 2.0;
	
	-- The size of the port name:
	port_size : constant type_distance_positive := 2.0;
	

	
	
	
	-- For netchangers we use this hardcoded symbol:
	
	type type_netchanger_symbol is record -- CS make private
		master_port	: type_netchanger_port := (
						position	=> position_master_port_default,
						rotation	=> zero_rotation,
						others		=> <>);

		slave_port	: type_netchanger_port := (
						position	=> position_slave_port_default,						
						rotation	=> 180.0,
						others		=> <>);

		-- the arc that connects the ports
		arc	: type_symbol_arc := (type_arc (to_arc (
						center	=> (x =>  0.0, y => 0.0),
						A		=> (x => -2.5, y => 0.0),
						B		=> (x =>  2.5, y => 0.0),
						direction	=> CW))
						with net_linewidth);

	end record;

	
	
	netchanger_symbol : constant type_netchanger_symbol := (others => <>);

	
end et_netchanger_symbol_schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
