------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             SUBMODULES                                   --
--                                                                          --
--                               S p e c                                    --
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


with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings;				use ada.strings;
with ada.strings.fixed;			use ada.strings.fixed;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.strings.maps;			use ada.strings.maps;
with ada.containers;            use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.ordered_sets;

with cairo;

with et_logging;				use et_logging;
with et_general;				use et_general;
with et_net_names;				use et_net_names;
with et_text;
with et_coordinates;			use et_coordinates;
with et_pcb;
with et_pcb_stack;
with et_pcb_coordinates;
with et_symbols;

package et_submodules is
	use pac_geometry_sch;
	
	nesting_depth_max : constant positive := 10; -- CS increase if nessecary

	subtype type_submodule_edge_length is pac_geometry_sch.type_distance_positive
		range 20.0 .. 1000.0; -- unit is mm

	keyword_size	: constant string := "size";
	keyword_file	: constant string := "file";
	
	type type_submodule_size is record
		x : type_submodule_edge_length := 60.0; -- width of the box
		y : type_submodule_edge_length := 40.0; -- height of the box
	end record;

	function to_submodule_size (size : in type_submodule_size) return string;

	function at_edge (
	-- Returns true if the given point sits at the edge of a submodule box.
	-- Issues a warning if the point sits at the lower edge of the box
	-- because the attached net may overlap with the text (instance, file,
	-- position in board, ...) below the box.
		point	: in type_point;
		size	: in type_submodule_size)
		return boolean;
	
	type type_submodule_view_mode is (
		ORIGIN,		-- device names and net names displayed as drawn in the generic submodule
		INSTANCE	-- device names and net names displayed after renumbering and prefixing
		);

	function to_string (view : in type_submodule_view_mode) return string;
	function to_view_mode (mode : in string) return type_submodule_view_mode;
	
	submodule_path_length_max : constant positive := 300;

	-- The full name of a submodule like $ET_TEMPLATES/motor_driver.mod
	package pac_submodule_path is new generic_bounded_length (submodule_path_length_max);
	function to_submodule_path (path : in string) return pac_submodule_path.bounded_string;
	function to_string (path : in pac_submodule_path.bounded_string) return string;

	function to_module_name (path : in pac_submodule_path.bounded_string) 
		return pac_module_name.bounded_string;
	-- Removes the file extension from given path and returns the module name.




	
	type type_netchanger_port_name is (MASTER, SLAVE);
	
	keyword_direction : constant string := "direction";
	
	function to_port_name (name : in string) return type_netchanger_port_name;
	function to_string (name : in type_netchanger_port_name) return string;	


	
	
	-- GUI relevant only: The port of a submodule is just a small rectangle:
	port_symbol_width	: constant type_float_internal_positive := 4.0;
	port_symbol_height	: constant type_float_internal_positive := 2.0;
	
	type type_port_symbol is record
		width, height : type_float_internal_positive;
	end record;

	port_symbol : constant type_port_symbol := (port_symbol_width, port_symbol_height);
	port_symbol_line_width : constant et_symbols.type_line_width := 0.2;

	
	-- GUI relevant only: The font of the port name:
	port_name_font : constant et_text.type_font := (
		family	=> et_text.to_family ("monospace"),
		slant	=> cairo.CAIRO_FONT_SLANT_NORMAL,
		weight	=> cairo.CAIRO_FONT_WEIGHT_NORMAL);

	-- The font size of the port name:
	port_name_font_size : constant et_symbols.pac_text.type_text_size := 2.0;

	-- The spacing between port rectangle and port name
	port_name_spacing : constant type_distance_positive := 0.5;


	-- GUI relevant only: The font of the port direction:
	port_direction_font : constant et_text.type_font := (
		family	=> et_text.to_family ("monospace"),
		slant	=> cairo.CAIRO_FONT_SLANT_NORMAL,
		weight	=> cairo.CAIRO_FONT_WEIGHT_NORMAL);

	-- The font size of the port direction:
	port_direction_font_size : constant et_symbols.pac_text.type_text_size :=
		et_symbols.pac_text.type_text_size (port_symbol_height) 
		- 3.0 * port_symbol_line_width;

	-- Required in the GUI to indicate the direction of a submodule port:
	port_direction_abbrevation_master : constant string := "M";
	port_direction_abbrevation_slave  : constant string := "S";

	-- Returns an M for MASTER and an S for SLAVE.
	function to_direction_abbrevation (direction : in type_netchanger_port_name) return string;
	


	
	
	type type_submodule_port is record
		-- the position somewhere at the edge of the box
		position	: type_point;

		-- The direction of inheriting net names when a netlist is exported:
		-- Slave means: The net inside the submodule enforces its name onto the
		-- net in the parent module.
		-- Master means: The net in the parent module enforces its name onto the
		-- net in the submodule:
		direction	: type_netchanger_port_name := MASTER;
	end record;

	use pac_net_name;
	package pac_submodule_ports is new ordered_maps (
		element_type	=> type_submodule_port,
		-- The net inside the submodule is here the port name:
		key_type		=> pac_net_name.bounded_string); -- CLOCK_GENERATOR_OUT

	procedure move_ports (
	-- Moves the given submodule ports by the given offset.
		ports	: in out pac_submodule_ports.map; -- the portlist
		offset	: in et_coordinates.type_position); -- the offset (only x/y matters)

	-- THIS IS THE GRAPHICAL REPRESENTATION OF A SUBMODULE ->
	-- THE RECTANGULAR BOX AT THE SHEET WHERE THE SUBMODULE IS INSTANTIATED.
	type type_submodule is record
		file				: pac_submodule_path.bounded_string; -- $ET_TEMPLATES/motor_driver.mod
		position		    : et_coordinates.type_position; -- the lower left corner
		size				: type_submodule_size; -- CS default ?
		position_in_board	: et_pcb_coordinates.pac_geometry_brd.type_position := et_pcb_coordinates.pac_geometry_brd.origin_zero_rotation;
		view_mode			: type_submodule_view_mode := ORIGIN;
		ports				: pac_submodule_ports.map;
	end record;

	-- GUI relevant only: The line width of the box:
	submod_box_line_width : constant et_symbols.type_line_width := 0.2;


	
	-- GUI relevant only: The font of the instance name:
	instance_font : constant et_text.type_font := (
		family	=> et_text.to_family ("monospace"),
		slant	=> cairo.CAIRO_FONT_SLANT_NORMAL,
		weight	=> cairo.CAIRO_FONT_WEIGHT_NORMAL);

	instance_font_size : constant et_symbols.pac_text.type_text_size := 2.0;



	-- GUI relevant only: The font of the file name:
	file_font : constant et_text.type_font := (
		family	=> et_text.to_family ("monospace"),
		slant	=> cairo.CAIRO_FONT_SLANT_ITALIC,
		weight	=> cairo.CAIRO_FONT_WEIGHT_NORMAL);

	file_font_size : constant et_symbols.pac_text.type_text_size := 2.0;


	

	-- GUI relevant only: The font of the position in board:
	position_board_font : constant et_text.type_font := (
		family	=> et_text.to_family ("monospace"),
		slant	=> cairo.CAIRO_FONT_SLANT_NORMAL,
		weight	=> cairo.CAIRO_FONT_WEIGHT_NORMAL);

	position_board_font_size : constant et_symbols.pac_text.type_text_size := 2.0;


	
	-- GUI relevant only: The space between lower box edge, instance name, 
	-- file name, board position, view mode:
	text_spacing : constant type_distance_positive := 1.0;


	
	use pac_module_instance_name;
	
	package pac_submodules is new ordered_maps (
		key_type		=> pac_module_instance_name.bounded_string, -- MOT_DRV_3
		element_type	=> type_submodule);

	netchanger_id_max : constant positive := 10000; -- CS  increase if necessary
	type type_netchanger_id is range 1 .. netchanger_id_max;

	function to_netchanger_id (id : in string) return type_netchanger_id;
	function to_string (id : in type_netchanger_id) return string;		

	function opposide_port (port : in type_netchanger_port_name) return type_netchanger_port_name;

	type type_netchanger_port is record
		position	: type_point;
		length		: et_symbols.type_port_length; 
		rotation	: type_rotation;
	end record;

	position_master_port_default : constant type_point := type_point (set (x =>  10.0, y => 0.0));
	position_slave_port_default  : constant type_point := type_point (set (x => -10.0, y => 0.0));	
	
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
		arc	: et_symbols.type_arc := (
						center		=> type_point (set (x => 0.0, y => 0.0)),
						start_point	=> type_point (set (x => -5.0, y => 0.0)),
						end_point	=> type_point (set (x =>  5.0, y => 0.0)),
						direction	=> CW,
						width		=> et_symbols.port_line_width);
	end record;

	type type_netchanger is record
		position_sch	: et_coordinates.type_position; -- x,y,sheet,rotation
		--symbol			: type_netchanger_symbol; -- CS for visualisation only
		
		position_brd	: et_pcb_coordinates.pac_geometry_brd.type_point; -- x,y
		-- in board there is no rotation because the netchanger is just a point in x/y.
		layer			: et_pcb_stack.type_signal_layer := et_pcb_stack.type_signal_layer'first;
	end record;

	package pac_netchangers is new ordered_maps (
		key_type		=> type_netchanger_id,
		element_type	=> type_netchanger);

	type type_netchanger_ports is record
		master	: type_point := position_master_port_default;
		slave	: type_point := position_slave_port_default;
	end record;
	
	function netchanger_ports (
	-- Returns the absolute x/y positions of the given netchanger.
		netchanger_cursor	: in pac_netchangers.cursor)
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
	
	
end et_submodules;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
