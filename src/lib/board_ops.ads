------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           BOARD OPERATIONS                               --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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

with et_general;				use et_general;
with et_string_processing;		use et_string_processing;
with et_project;				use et_project;
with et_schematic;
with schematic_ops;				use schematic_ops;
with et_packages;				use et_packages;
with et_pcb;					use et_pcb;
with et_pcb_stack;
with et_pcb_coordinates;		use et_pcb_coordinates;
-- with submodules;
with assembly_variants;
with pick_and_place;
with et_devices;				use et_devices;

package board_ops is

	procedure move_board (
	-- Moves the origin of the board to the given point (relative to the lower left 
	-- corner of the drawing frame):
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		coordinates		: in type_coordinates; -- relative/absolute		
		point			: in geometry.type_point; -- x/y
		log_threshold	: in type_log_level);
	
	procedure add_layer (
	-- Adds a signal layer to the board.
	-- Renumbers the signal layers.							
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		layer			: in et_pcb_stack.type_layer; -- incl. conductor and dieelectic thickness
		log_threshold	: in type_log_level);

	function layer_count (module_cursor	: in et_project.type_modules.cursor) 
	-- Returns the total number of signal layers used by the given module.
		return et_pcb_stack.type_signal_layer;

	procedure test_layer (
	-- Tests whether the given layer is allowed according to current layer stack
	-- of the given board.
		module_cursor	: in et_project.type_modules.cursor;
		layer			: in et_pcb_stack.type_signal_layer);
	
	procedure delete_layer (
	-- Deletes a signal layer in the board.
	-- Renumbers the signal layers.							   
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		layer			: in et_pcb_stack.type_signal_layer;
		log_threshold	: in type_log_level);
	
	procedure move_device (
	-- Moves a device in the board layout in x/y direction.
	-- Leaves rotation and face (top/bottom) as it is.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_name; -- IC45
		coordinates		: in type_coordinates; -- relative/absolute		
		point			: in geometry.type_point; -- x/y
		log_threshold	: in type_log_level);

	procedure rotate_device (
	-- Rotates a device in the board layout.
	-- Leaves x/y and face (top/bottom) as it is.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_name; -- IC45
		coordinates		: in type_coordinates; -- relative/absolute		
		rotation		: in et_pcb_coordinates.type_rotation; -- 90
		log_threshold	: in type_log_level);
	
	procedure flip_device (
	-- Flips a device in the board layout from top to bottom or vice versa.
	-- Leaves x/y and rotation as it is.
	-- Warns operator if device already on desired face of board.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_name; -- IC45
		face			: in type_face; -- top/bottom
		log_threshold	: in type_log_level);

	procedure move_submodule (
	-- Moves a submodule instance within the parent module layout in x/y direction.
	-- Leaves rotation and face (top/bottom) as it is.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in type_module_instance_name.bounded_string; -- OSC1
		coordinates		: in type_coordinates; -- relative/absolute		
		point			: in geometry.type_point; -- x/y
		log_threshold	: in type_log_level);
	
	procedure make_pick_and_place (
	-- Exports a pick & place file from the given top module and assembly variant.
	-- CS: The rotation of submodules is currently ignored. The rotation defaults to zero degree.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		log_threshold	: in type_log_level);
	
	-- For laying out traces we need a type that provides for a terminal information about
	-- x/y/rotation/technology and optionally the face. Face is available if technology is SMT.
	type type_terminal_position (technology	: et_packages.type_assembly_technology) is new geometry.type_position with record
		case technology is
			when SMT => face : type_face;
			when THT => null;
		end case;
	end record;

	function locate_device (
	-- Returns a cursor to the requested device in the given module.
		module_cursor	: in et_project.type_modules.cursor;
		device_name		: in type_name)
		return et_schematic.type_devices.cursor;
	
	function terminal_position (
	-- Returns the position of a terminal of the given device in the board.
	-- The device must be real (appearance SCH_PCB).
		module_cursor	: in et_project.type_modules.cursor;
		device_cursor	: in et_schematic.type_devices.cursor; -- IC45
		terminal_name	: in type_terminal_name.bounded_string) -- H7, 14
		return type_terminal_position;

	procedure set_grid (
	-- Sets the grid of the module.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		grid			: in geometry.type_grid;
		log_threshold	: in type_log_level);		

-- TRACKS AND FREETRACKS

	procedure add_named_track (
	-- Adds a line track segment to the given net in the given module.
		module_cursor	: in type_modules.cursor;
		net_name		: in type_net_name.bounded_string; -- reset_n
		line			: in et_pcb.type_copper_line);
	
	procedure draw_track_line (
	-- Draws track line. If net_name is empty a freetrack will be drawn.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in type_net_name.bounded_string; -- reset_n
		line			: in et_pcb.type_copper_line;
		log_threshold	: in type_log_level);

	procedure draw_track_line (
	-- Draws a named track line.
	-- Assumes that module_cursor and net_cursor point to existing objects.
		module_cursor	: in type_modules.cursor;
		net_cursor		: in et_schematic.type_nets.cursor; -- reset_n
		line			: in et_pcb.type_copper_line;
		log_threshold	: in type_log_level);

	procedure draw_track_line (
	-- Draws a track starting at a terminal. The track ends
	-- after the given length in given direction.
	-- If the terminal is a THT type, then the track may start at any signal layer.
	-- If the terminal is a SMT type, then the track may start at either the top or bottom
	-- signal layer. If operator indeed whishes an inner layer a warning is issued.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in type_net_name.bounded_string; -- reset_n
		layer			: in et_pcb_stack.type_signal_layer;
		width			: in type_track_width;
		device			: in type_name;
		terminal		: in type_terminal_name.bounded_string;
		direction		: in type_rotation;
		length			: in geometry.type_distance_positive;
		log_threshold	: in type_log_level);

	procedure draw_track_line (
	-- Draws a track starting at a terminal. The track ends
	-- after the given number of notches along the given axis.
	-- If the terminal is a THT type, then the track may start at any signal layer.
	-- If the terminal is a SMT type, then the track may start at either the top or bottom
	-- signal layer. If operator indeed whishes an inner layer a warning is issued.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in type_net_name.bounded_string; -- reset_n
		layer			: in et_pcb_stack.type_signal_layer;
		width			: in type_track_width;
		device			: in type_name;
		terminal		: in type_terminal_name.bounded_string;
		direction		: in type_rotation;
		axis			: in type_axis_2d;
		notches			: in type_grid_notches;
		log_threshold	: in type_log_level);

	procedure draw_track_line (
	-- Draws a track starting at a terminal. The track ends at the given point.
	-- If the terminal is a THT type, then the track may start at any signal layer.
	-- If the terminal is a SMT type, then the track may start at either the top or bottom
	-- signal layer. If operator indeed whishes an inner layer a warning is issued.								  
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in type_net_name.bounded_string; -- reset_n
		layer			: in et_pcb_stack.type_signal_layer;
		width			: in type_track_width;
		device			: in type_name;
		terminal		: in type_terminal_name.bounded_string;
		end_point		: in geometry.type_point;
		log_threshold	: in type_log_level);

	procedure draw_track_line (
	-- Draws a track starting at a terminal. The track runs into the 
	-- given direction and ends after the given number of notches along the given axis.
	-- If the terminal is a THT type, then the track may start at any signal layer.
	-- If the terminal is a SMT type, then the track may start at either the top or bottom
	-- signal layer. If operator indeed whishes an inner layer a warning is issued.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in type_net_name.bounded_string; -- reset_n
		layer			: in et_pcb_stack.type_signal_layer;
		width			: in type_track_width;
		device			: in type_name;
		terminal		: in type_terminal_name.bounded_string;
		axis			: in type_axis_2d;
		notches			: in type_grid_notches;
		log_threshold	: in type_log_level);
	
	procedure draw_track_arc (
	-- Draws a track arc. If net_name is empty a freetrack will be drawn.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in type_net_name.bounded_string; -- reset_n
		arc				: in et_pcb.type_copper_arc;
		log_threshold	: in type_log_level);

	procedure ripup_track_segment (
	-- Rips up the track segment of a net that crosses the given point in given layer.
	-- CS currently rips up the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in type_net_name.bounded_string; -- reset_n
		layer			: in et_pcb_stack.type_signal_layer;
		point			: in geometry.type_point; -- x/y
		accuracy		: in geometry.type_accuracy;
		log_threshold	: in type_log_level);

-- ROUTE RESTRICT
	
	procedure draw_route_restrict_line (
	-- Draws route restrict line.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		line			: in type_route_restrict_line;
		log_threshold	: in type_log_level);

	procedure draw_route_restrict_arc (
	-- Draws a route restrict arc.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		arc				: in type_route_restrict_arc;
		log_threshold	: in type_log_level);

	procedure draw_route_restrict_circle (
	-- Draws a route restrict circle.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		circle			: in type_route_restrict_circle;
		log_threshold	: in type_log_level);	

	procedure delete_route_restrict (
	-- Deletes the segment of route restrict that crosses the given point.
	-- CS currently rips up the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		point			: in geometry.type_point; -- x/y
		accuracy		: in geometry.type_accuracy;
		log_threshold	: in type_log_level);

-- VIA RESTRICT

	procedure draw_via_restrict_line (
	-- Draws a via restrict line.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		line			: in type_via_restrict_line;
		log_threshold	: in type_log_level);

	procedure draw_via_restrict_arc (
	-- Draws a via restrict arc.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		arc				: in type_via_restrict_arc;
		log_threshold	: in type_log_level);

	procedure draw_via_restrict_circle (
	-- Draws a via restrict circle.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		circle			: in type_via_restrict_circle;
		log_threshold	: in type_log_level);

	procedure delete_via_restrict (
	-- Deletes the segment of via restrict that crosses the given point.
	-- CS currently rips up the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		point			: in geometry.type_point; -- x/y
		accuracy		: in geometry.type_accuracy;
		log_threshold	: in type_log_level);

	
-- BOARD OUTLINE / CONTOUR

	procedure draw_outline_line (
	-- Draws a line in the PCB outline.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		line			: in et_pcb.type_pcb_contour_line;
		log_threshold	: in type_log_level);

	procedure draw_outline_arc (
	-- Draws an arc in the PCB outline.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		arc				: in et_pcb.type_pcb_contour_arc;
		log_threshold	: in type_log_level);

	procedure draw_outline_circle (
	-- Draws a circle in the PCB outline.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		circle			: in et_pcb.type_pcb_contour_circle;
		log_threshold	: in type_log_level);

	procedure delete_outline (
	-- Deletes the segment of the outline that crosses the given point.
	-- CS currently rips up the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		point			: in geometry.type_point; -- x/y
		accuracy		: in geometry.type_accuracy;
		log_threshold	: in type_log_level);

-- SILK SCREEN

	procedure draw_silk_screen_line (
	-- Draws a line in the PCB silk_screen.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		line			: in type_silk_line;
		log_threshold	: in type_log_level);

	procedure draw_silk_screen_arc (
	-- Draws an arc in the PCB silk_screen.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		arc				: in type_silk_arc;
		log_threshold	: in type_log_level);

	procedure draw_silk_screen_circle (
	-- Draws a circle in the PCB silk_screen.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;	
		circle			: in type_fillable_circle;
		log_threshold	: in type_log_level);

	procedure delete_silk_screen (
	-- Deletes the segment of the silk_screen that crosses the given point.
	-- CS currently deletes the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		point			: in geometry.type_point; -- x/y
		accuracy		: in geometry.type_accuracy;
		log_threshold	: in type_log_level);


-- ASSEMBLY DOCUMENTATION
	procedure draw_assy_doc_line (
	-- Draws a line in the assembly documentation.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		line			: in type_doc_line;
		log_threshold	: in type_log_level);

	procedure draw_assy_doc_arc (
	-- Draws an arc in the assembly documentation.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		arc				: in type_doc_arc;
		log_threshold	: in type_log_level);

	procedure draw_assy_doc_circle (
	-- Draws a circle in the assembly documentation.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;	
		circle			: in type_fillable_circle;
		log_threshold	: in type_log_level);

	procedure delete_assy_doc (
	-- Deletes the segment of the assembly documentation that crosses the given point.
	-- CS currently deletes the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		point			: in geometry.type_point; -- x/y
		accuracy		: in geometry.type_accuracy;
		log_threshold	: in type_log_level);

-- KEEPOUT
	procedure draw_keepout_line (
	-- Draws a line in the keepout layer.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		line			: in type_keepout_line;
		log_threshold	: in type_log_level);

	procedure draw_keepout_arc (
	-- Draws an arc in the keepout layer.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		arc				: in type_keepout_arc;
		log_threshold	: in type_log_level);

	procedure draw_keepout_circle (
	-- Draws a circle in the keepout layer.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;	
		circle			: in type_fillable_circle_solid;
		log_threshold	: in type_log_level);

	procedure delete_keepout (
	-- Deletes the segment in the keepout layer that crosses the given point.
	-- CS currently deletes the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		point			: in geometry.type_point; -- x/y
		accuracy		: in geometry.type_accuracy;
		log_threshold	: in type_log_level);

-- STOP MASK
	procedure draw_stop_line (
	-- Draws a line in the stop mask layer.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		line			: in type_stop_line;
		log_threshold	: in type_log_level);

	procedure draw_stop_arc (
	-- Draws an arc in the stop mask layer.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		arc				: in type_stop_arc;		
		log_threshold	: in type_log_level);

	procedure draw_stop_circle (
	-- Draws an circle in the stop mask layer.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		circle			: in type_fillable_circle;
		log_threshold	: in type_log_level);

	procedure delete_stop (
	-- Deletes the segment of the stop mask that crosses the given point.
	-- CS currently deletes the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		point			: in geometry.type_point; -- x/y
		accuracy		: in geometry.type_accuracy;
		log_threshold	: in type_log_level);

-- STENCIL

	procedure draw_stencil_line (
	-- Draws a line in the stencil layer.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		line			: in type_stencil_line;
		log_threshold	: in type_log_level);

	procedure draw_stencil_arc (
	-- Draws an arc in the stencil layer.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		arc				: in type_stencil_arc;		
		log_threshold	: in type_log_level);

	procedure draw_stencil_circle (
	-- Draws an circle in the stencil layer.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		circle			: in type_fillable_circle;
		log_threshold	: in type_log_level);

	procedure delete_stencil (
	-- Deletes the segment of the stencil that crosses the given point.
	-- CS currently deletes the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		point			: in geometry.type_point; -- x/y
		accuracy		: in geometry.type_accuracy;
		log_threshold	: in type_log_level);



	
end board_ops;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
