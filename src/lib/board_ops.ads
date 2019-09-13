------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           BOARD OPERATIONS                               --
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
with et_libraries;				use et_libraries;
with et_string_processing;		use et_string_processing;
with et_project;				use et_project;
with et_schematic;
with schematic_ops;				use schematic_ops;
with et_pcb;					use et_pcb;
with et_pcb_coordinates;		use et_pcb_coordinates;
-- with submodules;
with assembly_variants;
with pick_and_place;
-- with numbering;
-- with material;
-- with netlists;

package board_ops is

	procedure move_device (
	-- Moves a device in the board layout in x/y direction.
	-- Leaves rotation and face (top/bottom) as it is.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		coordinates		: in type_coordinates; -- relative/absolute		
		point			: in geometry.type_point; -- x/y
		log_threshold	: in type_log_level);

	procedure rotate_device (
	-- Rotates a device in the board layout.
	-- Leaves x/y and face (top/bottom) as it is.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		coordinates		: in type_coordinates; -- relative/absolute		
		rotation		: in et_pcb_coordinates.type_rotation; -- 90
		log_threshold	: in type_log_level);
	
	procedure flip_device (
	-- Flips a device in the board layout from top to bottom or vice versa.
	-- Leaves x/y and rotation as it is.
	-- Warns operator if device already on desired face of board.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
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
	type type_terminal_position (technology	: et_pcb.type_assembly_technology) is new geometry.type_position with record
		case technology is
			when SMT => face : type_face;
			when THT => null;
		end case;
	end record;

	function locate_device (
	-- Returns a cursor to the requested device in the given module.
		module_cursor	: in et_project.type_modules.cursor;
		device_name		: in type_device_name)
		return et_schematic.type_devices.cursor;
	
	function terminal_position (
	-- Returns the position of a terminal of the given device in the board.
	-- The device must be real (appearance SCH_PCB).
		module_cursor	: in et_project.type_modules.cursor;
		device_cursor	: in et_schematic.type_devices.cursor; -- IC45
		terminal_name	: in et_libraries.type_terminal_name.bounded_string) -- H7, 14
		return type_terminal_position;

	
-- TRACKS AND FREETRACKS

	procedure add_named_track (
	-- Adds a line track segment to the given net in the given module.
		module_cursor	: in type_modules.cursor;
		net_name		: in type_net_name.bounded_string; -- reset_n
		line			: in type_copper_line_pcb);
	
	procedure draw_track_line (
	-- Draws track line. If net_name is empty a freetrack will be drawn.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in type_net_name.bounded_string; -- reset_n
		line			: in type_copper_line_pcb;
		log_threshold	: in type_log_level);

	procedure draw_track_line (
	-- Draws a named track line.
	-- Assumes that module_cursor and net_cursor point to existing objects.
		module_cursor	: in type_modules.cursor;
		net_cursor		: in et_schematic.type_nets.cursor; -- reset_n
		line			: in type_copper_line_pcb;
		log_threshold	: in type_log_level);

	procedure draw_track_line (
	-- Draws a track starting at a terminal. The track ends
	-- after the given length in given direction.
	-- If the terminal is a THT type, then the track may start at any signal layer.
	-- If the terminal is a SMT type, then the track may start at either the top or bottom
	-- signal layer. If operator indeed whishes an inner layer a warning is issued.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in type_net_name.bounded_string; -- reset_n
		layer			: in type_signal_layer;
		width			: in type_track_width;
		device			: in type_device_name;
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
		layer			: in type_signal_layer;
		width			: in type_track_width;
		device			: in type_device_name;
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
		layer			: in type_signal_layer;
		width			: in type_track_width;
		device			: in type_device_name;
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
		layer			: in type_signal_layer;
		width			: in type_track_width;
		device			: in type_device_name;
		terminal		: in type_terminal_name.bounded_string;
		axis			: in type_axis_2d;
		notches			: in type_grid_notches;
		log_threshold	: in type_log_level);
	
	procedure draw_track_arc (
	-- Draws a track arc. If net_name is empty a freetrack will be drawn.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in type_net_name.bounded_string; -- reset_n
		arc				: in type_copper_arc_pcb;
		log_threshold	: in type_log_level);

	procedure ripup_track_segment (
	-- Rips up the track segment of a net that crosses the given point in given layer.
	-- CS currently rips up the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in type_net_name.bounded_string; -- reset_n
		layer			: in type_signal_layer;
		point			: in geometry.type_point; -- x/y
		accuracy		: in geometry.type_accuracy;
		log_threshold	: in type_log_level);

-- BOARD OUTLINE / CONTOUR
	procedure draw_outline_line (
	-- Draws a line in the PCB outline.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		line			: in type_pcb_contour_line;
		log_threshold	: in type_log_level);

	procedure draw_outline_arc (
	-- Draws an arc in the PCB outline.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		arc				: in type_pcb_contour_arc;
		log_threshold	: in type_log_level);

	procedure draw_outline_circle (
	-- Draws a circle in the PCB outline.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		circle			: in type_pcb_contour_circle;
		log_threshold	: in type_log_level);

	procedure delete_outline (
	-- Deletes the segment of the outline that crosses the given point.
	-- CS currently rips up the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		point			: in geometry.type_point; -- x/y
		accuracy		: in geometry.type_accuracy;
		log_threshold	: in type_log_level);


-- VIA RESTRICT

-- ROUTE RESTRICT

-- KEEPOUT

-- STOP MASK

-- STENCIL / SOLDER PASTE

-- ASSEMBLY DOCUMENTATION

-- SILK SCREEN
	procedure draw_silk_screen_line (
	-- Draws a line in the PCB silk_screen.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		line			: in type_silk_line;
		log_threshold	: in type_log_level);
		-- CS use type_silk_line

	procedure draw_silk_screen_arc (
	-- Draws an arc in the PCB silk_screen.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		arc				: in type_silk_arc;
		log_threshold	: in type_log_level);
		-- CS use type_silk_arc

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

	
end board_ops;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
