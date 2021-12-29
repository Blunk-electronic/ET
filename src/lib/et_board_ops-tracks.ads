------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     BOARD OPERATIONS ON TRACKS                           --
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
with ada.characters.latin_1;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers;            use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_nets;
with et_net_names;				use et_net_names;
with et_general;				use et_general;
with et_geometry;				use et_geometry;
with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;
with et_project.modules;		use et_project.modules;
with et_schematic;
with et_vias;					use et_vias;
with et_terminals;				use et_terminals;
with et_packages;				use et_packages;
with et_pcb;					use et_pcb;
with et_pcb_stack;				use et_pcb_stack;
with et_pcb_coordinates;		use et_pcb_coordinates;
use et_pcb_coordinates.pac_geometry_brd;

with et_board_shapes_and_text;

-- with submodules;
with et_assembly_variants;
with et_pick_and_place;
with et_devices;				use et_devices;
with et_conventions;
with et_design_rules;			use et_design_rules;
with et_conductor_text.boards;		use et_conductor_text.boards;
with et_conductor_segment.boards;	use et_conductor_segment.boards;
with et_conductor_polygons;		use et_conductor_polygons;
with et_route_restrict.boards;	use et_route_restrict.boards;
with et_via_restrict.boards;	use et_via_restrict.boards;
with et_stop_mask;				use et_stop_mask;
with et_stop_mask.boards;		use et_stop_mask.boards;
with et_stencil;				use et_stencil;
with et_stencil.boards;			use et_stencil.boards;
with et_silkscreen;				use et_silkscreen;
with et_silkscreen.boards;		use et_silkscreen.boards;
with et_assy_doc;				use et_assy_doc;
with et_assy_doc.boards;		use et_assy_doc.boards;
with et_keepout;				use et_keepout;
with et_keepout.boards;			use et_keepout.boards;
with et_pcb_contour;			use et_pcb_contour;
with et_text;

package et_board_ops.tracks is

	-- CS rework procedures so that a module cursor
	-- is used instead the module_name.

	use et_board_shapes_and_text.pac_geometry_2;
	use et_board_shapes_and_text;
	use pac_polygons;
	use pac_text_fab;
	use pac_net_name;


	
	-- Adds a line track segment to the given net in the given module.
	procedure add_named_track (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- reset_n
		line			: in type_conductor_line);

	
	-- Draws track line. If net_name is empty a freetrack will be drawn.
	procedure draw_track_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		line			: in type_conductor_line;
		log_threshold	: in type_log_level);

	
	-- Draws a named track line.
	-- Assumes that module_cursor and net_cursor point to existing objects.
	procedure draw_track_line (
		module_cursor	: in pac_generic_modules.cursor;
		net_cursor		: in et_schematic.pac_nets.cursor; -- reset_n
		line			: in type_conductor_line;
		log_threshold	: in type_log_level);

	
	procedure draw_track_line (
	-- Draws a track starting at a terminal. The track ends
	-- after the given length in given direction.
	-- If the terminal is a THT type, then the track may start at any signal layer.
	-- If the terminal is a SMT type, then the track may start at either the top or bottom
	-- signal layer. If operator indeed whishes an inner layer a warning is issued.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		layer			: in et_pcb_stack.type_signal_layer;
		width			: in type_track_width;
		device			: in type_device_name;
		terminal		: in pac_terminal_name.bounded_string;
		direction		: in type_rotation;
		length			: in type_distance_positive;
		log_threshold	: in type_log_level);

	
	procedure draw_track_line (
	-- Draws a track starting at a terminal. The track ends
	-- after the given number of notches along the given axis.
	-- If the terminal is a THT type, then the track may start at any signal layer.
	-- If the terminal is a SMT type, then the track may start at either the top or bottom
	-- signal layer. If operator indeed whishes an inner layer a warning is issued.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		layer			: in et_pcb_stack.type_signal_layer;
		width			: in type_track_width;
		device			: in type_device_name;
		terminal		: in pac_terminal_name.bounded_string;
		direction		: in type_rotation;
		axis			: in type_axis_2d;
		notches			: in type_grid_notches;
		log_threshold	: in type_log_level);

	
	procedure draw_track_line (
	-- Draws a track starting at a terminal. The track ends at the given point.
	-- If the terminal is a THT type, then the track may start at any signal layer.
	-- If the terminal is a SMT type, then the track may start at either the top or bottom
	-- signal layer. If operator indeed whishes an inner layer a warning is issued.								  
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		layer			: in et_pcb_stack.type_signal_layer;
		width			: in type_track_width;
		device			: in type_device_name;
		terminal		: in pac_terminal_name.bounded_string;
		end_point		: in type_point;
		log_threshold	: in type_log_level);

	
	procedure draw_track_line (
	-- Draws a track starting at a terminal. The track runs into the 
	-- given direction and ends after the given number of notches along the given axis.
	-- If the terminal is a THT type, then the track may start at any signal layer.
	-- If the terminal is a SMT type, then the track may start at either the top or bottom
	-- signal layer. If operator indeed whishes an inner layer a warning is issued.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		layer			: in et_pcb_stack.type_signal_layer;
		width			: in type_track_width;
		device			: in type_device_name;
		terminal		: in pac_terminal_name.bounded_string;
		axis			: in type_axis_2d;
		notches			: in type_grid_notches;
		log_threshold	: in type_log_level);

	
	-- Draws a track arc. If net_name is empty a freetrack will be drawn.
	procedure draw_track_arc (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		arc				: in type_conductor_arc;
		log_threshold	: in type_log_level);

	
	
	-- Rips up the track segment of a net that crosses the given point in given layer.
	-- CS currently rips up the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
	procedure ripup_track_segment (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		layer			: in et_pcb_stack.type_signal_layer;
		point			: in type_point; -- x/y
		accuracy		: in type_catch_zone;
		log_threshold	: in type_log_level);

											
end et_board_ops.tracks;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
