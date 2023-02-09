------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     BOARD OPERATIONS / CONDUCTOR OBJECTS                 --
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

with et_conductor_segment.boards;		use et_conductor_segment.boards;
with et_fill_zones;						use et_fill_zones;
with et_conductor_text.boards;			use et_conductor_text.boards;

with et_board_shapes_and_text;


package et_board_ops.conductors is

	-- CS rework procedures so that a module cursor
	-- is used instead the module_name.

	use et_board_shapes_and_text;
	use pac_net_name;


-- TRACKS:
	
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
	--procedure draw_track_line (
		--module_cursor	: in pac_generic_modules.cursor;
		--net_cursor		: in et_schematic.pac_nets.cursor; -- reset_n
		--line			: in type_conductor_line;
		--log_threshold	: in type_log_level);

	
	-- Draws a track starting at a terminal. The track ends
	-- after the given length in given direction.
	-- If the terminal is a THT type, then the track may start at any signal layer.
	-- If the terminal is a SMT type, then the track may start at either the top or bottom
	-- signal layer. If operator indeed whishes an inner layer a warning is issued.
	procedure draw_track_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		layer			: in et_pcb_stack.type_signal_layer;
		width			: in type_track_width;
		device			: in type_device_name;
		terminal		: in pac_terminal_name.bounded_string;
		direction		: in type_rotation;
		length			: in type_distance_positive;
		log_threshold	: in type_log_level);

	
	-- Draws a track starting at a terminal. The track ends
	-- after the given number of notches along the given axis.
	-- If the terminal is a THT type, then the track may start at any signal layer.
	-- If the terminal is a SMT type, then the track may start at either the top or bottom
	-- signal layer. If operator indeed whishes an inner layer a warning is issued.
	procedure draw_track_line (
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

	
	-- Draws a track starting at a terminal. The track ends at the given point.
	-- If the terminal is a THT type, then the track may start at any signal layer.
	-- If the terminal is a SMT type, then the track may start at either the top or bottom
	-- signal layer. If operator indeed whishes an inner layer a warning is issued.								  
	procedure draw_track_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		layer			: in et_pcb_stack.type_signal_layer;
		width			: in type_track_width;
		device			: in type_device_name;
		terminal		: in pac_terminal_name.bounded_string;
		end_point		: in type_point;
		log_threshold	: in type_log_level);

	
	-- Draws a track starting at a terminal. The track runs into the 
	-- given direction and ends after the given number of notches along the given axis.
	-- If the terminal is a THT type, then the track may start at any signal layer.
	-- If the terminal is a SMT type, then the track may start at either the top or bottom
	-- signal layer. If operator indeed whishes an inner layer a warning is issued.
	procedure draw_track_line (
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



-- FILL ZONES
	
	procedure place_fill_zone (
		module_cursor	: in pac_generic_modules.cursor;
		zone			: in type_zone'class;
		log_threshold	: in type_log_level;

		-- Net name is relevant if filil zone is part of a route.
		-- The type of the given fill zone is the cirteria:
		net_name		: in pac_net_name.bounded_string := no_name);


	-- Fills fill zones. If nets is empty, then all
	-- zones will be filled (even those who are floating).
	-- If nets contains net names then only the zones of these
	-- nets will be filled:
	procedure fill_zones (
		module_cursor	: in pac_generic_modules.cursor;	
		log_category	: in type_log_category;
		log_threshold	: in type_log_level;
		nets 			: in pac_net_names.list := no_net_names); -- GND, GNDA, P3V3, ...

	
	
-- TEXTS:
	
	-- Places a text in a conductor layer.
	procedure place_text_in_conductor_layer (
		module_cursor	: in pac_generic_modules.cursor;
		signal_layer	: in type_signal_layer;
		text			: in type_text_fab_with_content;
		log_threshold	: in type_log_level);

	
	-- Returns all texts in the vicinity of the given point:
	function get_texts (
		module_cursor	: in pac_generic_modules.cursor;
		point			: in type_point;
		catch_zone		: in type_catch_zone; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_conductor_texts.list;


	-- Moves a text:
	procedure move_text (
		module_cursor	: in pac_generic_modules.cursor;
		text			: in type_conductor_text;
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_point;
		log_threshold	: in type_log_level);

	
end et_board_ops.conductors;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
