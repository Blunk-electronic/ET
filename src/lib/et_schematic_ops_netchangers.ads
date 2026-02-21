------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   SCHEMATIC OPERATIONS / NETCHANGERS                     --
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
-- To Do: 
-- - rework procedures so that a module cursor is taken
--   instead of a module name.
--
-- - rename paramaters "module" to "module_cursor"
--

with et_schematic_coordinates;			use et_schematic_coordinates;
with et_schematic_geometry;				use et_schematic_geometry;
use et_schematic_geometry.pac_geometry_2;

with et_module_names;					use et_module_names;
with et_generic_modules;				use et_generic_modules;
with et_netchangers;					use et_netchangers;
with et_netchangers.schematic;			use et_netchangers.schematic;

with et_nets;							use et_nets;
with et_net_names;						use et_net_names;
with et_netlists;

with et_sheets;							use et_sheets;
with et_coordinates_abs_rel;			use et_coordinates_abs_rel;
with et_logging;						use et_logging;



package et_schematic_ops_netchangers is

	use pac_generic_modules;
	

	
	procedure netchanger_not_found (
		index : in type_netchanger_id);

	

	
	-- Returns true if given port of netchanger 
	-- is connected with any net:
	function port_connected (
		module	: in pac_generic_modules.cursor;	
		port	: in et_netlists.type_port_netchanger)
		return boolean;

	


	
	-- Drags the net segments according to the given netchanger ports.
	-- Changes the position of start or end points of segments.
	-- Does NOT create new connections with segments if a port
	-- lands on the start or end point of another segment.
	-- Does NOT create a new connection with a segments if a port
	-- lands between start and end point.
	procedure drag_net_segments (
		module			: in pac_generic_modules.cursor;	-- the module
		ports_before	: in type_netchanger_ports;	-- the old port positions
		ports_after		: in type_netchanger_ports;	-- the new port positions
		sheet			: in type_sheet;			-- the sheet to look at
		log_threshold	: in type_log_level);
	


	procedure insert_ports (
		module_cursor	: in pac_generic_modules.cursor;
		index			: in type_netchanger_id;
		ports			: in type_netchanger_ports;
		sheet			: in type_sheet;
		log_threshold	: in type_log_level);



	-- Returns the next available netchanger index in the module.
	function next_netchanger_index (
		module_cursor	: in pac_generic_modules.cursor)
		return type_netchanger_id;

	
	
	
	-- Returns true if given netchanger exists in 
	-- the given module:
	function exists_netchanger (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		index			: in type_netchanger_id) -- 1, 2, 3, ...
		return boolean;


	
	-- Returns the sheet/x/y position of the given netchanger port.
	function get_netchanger_port_position (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in type_netchanger_id; -- 1,2,3,...
		port			: in type_netchanger_port_name; -- SLAVE/MASTER
		log_threshold	: in type_log_level)
		return type_object_position;

	

	-- Adds a netchanger to the schematic.
	procedure add_netchanger (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		place			: in type_object_position; -- sheet/x/y
		log_threshold	: in type_log_level);
	

	-- Drags the given netchanger within the schematic.
	-- Already existing connections with net segments are kept.
	-- Net segment positions are modified.
	-- This operation applies to a single sheet. Dragging from one sheet
	-- to another is not possible.
	procedure drag_netchanger (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in type_netchanger_id; -- 1,2,3,...
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level);



	-- Moves the given netchanger. Disconnects the netchanger from
	-- start or end points of net segments BEFORE the move. 
	-- Connects netchanger ports with segment end or strart points AFTER the move.
	procedure move_netchanger (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in type_netchanger_id; -- 1,2,3,...
		coordinates		: in type_coordinates; -- relative/absolute
		sheet			: in type_sheet_relative; -- -3/0/2
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level);



	-- Rotates the given netchanger. Disconnects it from
	-- start or end points of net segments.
	-- The rotation can either be 0 or 90 degrees. Other angles are
	-- not accepted:
	procedure rotate_netchanger (
		module_cursor	: in pac_generic_modules.cursor; -- the module
		index			: in type_netchanger_id; -- 1,2,3,...
		rotation		: in et_schematic_geometry.type_rotation_model; -- 90
		log_threshold	: in type_log_level);

	
	
	-- Deletes a netchanger.
	procedure delete_netchanger (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in type_netchanger_id; -- 1,2,3,...
		log_threshold	: in type_log_level);



	-- Deletes ports of the given netchanger in nets:
	procedure delete_ports (
		module			: in pac_generic_modules.cursor; -- the module
		index			: in type_netchanger_id;	-- the netchanger id
		sheet			: in type_sheet;		-- the sheet where the netchanger is
		log_threshold	: in type_log_level);

	
end et_schematic_ops_netchangers;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
