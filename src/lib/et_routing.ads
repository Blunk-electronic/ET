------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               ROUTING                                    --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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
--                                                                          --
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
-- DESCRIPTION:
-- 

with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_sets;

with et_general;				use et_general;
with et_geometry;				use et_geometry;
with et_design_rules;			use et_design_rules;
with et_net_names;				use et_net_names;
with et_vias;					use et_vias;
with et_terminals;				use et_terminals;
with et_devices;
with et_packages;
with et_schematic;
with et_nets;						use et_nets;

with et_schematic_ops;
with et_pcb;						use et_pcb;
with et_pcb_contour;
with et_pcb_stack;					use et_pcb_stack;
with et_pcb_coordinates;			use et_pcb_coordinates;
with et_board_shapes_and_text;		use et_board_shapes_and_text;
with et_string_processing;			use et_string_processing;
with et_logging;					use et_logging;
with et_project.modules;			use et_project.modules;
with et_fill_zones;					use et_fill_zones;
with et_fill_zones.boards;
with et_conductor_segment;
with et_conductor_segment.boards;	use et_conductor_segment.boards;
with et_conductor_text.packages;
with et_conductor_text.boards;

package et_routing is
	
	use pac_geometry_brd;
	use pac_geometry_2;
	use pac_contours;
	

	
	

	-- When inquiring the clearance of a terminal then
	-- these types are required:

	-- A terminal may or may not be connected with a net:
	type type_connected is new boolean;

	-- If the terminal is connected with a net then the
	-- clearance exists:
	type type_get_terminal_clearance_result (
		connected : type_connected := true)
	is record
		case connected is
			when TRUE => clearance : type_track_clearance := type_track_clearance'first;
			when FALSE => null;
		end case;
	end record;


	-- Returns the clearance required to a terminal of a device.
	-- If the terminal is not connected to a net then it returns
	-- no clearance:
	function get_clearance (
		module	: in pac_generic_modules.cursor;
		device	: in et_schematic.pac_devices_sch.cursor;
		terminal: in pac_terminals.cursor)
		return type_get_terminal_clearance_result;


	
	-- Returns the distance to the nearest point
	-- on the board edge. Objects that are regarded as board
	-- edges are outline segments and hole segments:
	function get_distance_to_edge (
		module_cursor	: in pac_generic_modules.cursor;
		point			: in type_point;
		log_category	: in type_log_category;
		lth				: in type_log_level)
		return type_distance_polar;
	

	-- Tests whether the given point is in the usable board area. 
	-- Returns false if the point is outside the board or inside a hole.
	-- Returns false if the point is exactly on the edge of the board.
	-- Returns true if the point is exactly on the edge of a hole.
	--function on_board (
		--module_cursor	: in pac_generic_modules.cursor;
		--point			: in type_point;
		--log_category	: in type_log_category;
		--lth				: in type_log_level)
		--return boolean;

	
	
	-- A track starts at a certain point and travels into
	-- a certain direction. It has a width and a
	-- clearance to other objects. 
	-- The clearance may assume zero if a fill zones is to be filled
	-- because in that case the track reaches the border of the fill area.
	type type_track is record
		center		: type_line_vector; -- incl. start point and direction
		width		: type_track_width; -- of the conductor (usually copper) -- CS rename to conductor_width
		clearance	: type_distance_positive;
	end record;


	
	type type_place is (
		BEFORE,
		AFTER);					

	function to_string (place : in type_place) return string;

	
	-- The dimensions of a track:
	--type type_track_dimensions is record
		--direction	: type_rotation; -- the direction of travel
		--offset		: type_distance_relative; -- start point relative to the origin
		--boundaries	: type_boundaries; -- incl. clearance
		--upper_edge	: type_line; -- a horizontal line
		--center_line	: type_line; -- a horizontal line
		--lower_edge	: type_line; -- a horizontal line
	--end record;

	-- Calculates the dimensions of a track:
	--function get_dimensions (
		--track	:  in type_track)
		--return type_track_dimensions;

	--type type_obstacle (shape : type_shape) is record
		--case shape is
			--when LINE	=> line : type_line;
			--when ARC	=> arc : type_arc;
			--when CIRCLE	=> circle : type_circle;
		--end case;
	--end record;
	

	-- This function searches the break point before or after
	-- an obstacle along the x-axis. The result is an x-position
	-- before or after the obstacle.
	-- This function assumes the travel direction of the fill line is zero.
	--function get_break (
		--init			: in type_distance; -- the start point of the search
		--place			: in type_place; -- before/after
		--obstacle		: in type_obstacle;
		--clearance		: in type_distance_positive; -- the clearance to the obstacle
		--log_category	: in type_log_category;
		--lth				: in type_log_level) 
		--return type_distance;

	
	-- A break may or may not exist. If it exists, then the point
	-- is where a track ends (before an obstacle) or where it starts
	-- (after an obstacle). Point is the center of the cap of the track.
	type type_break (exists : boolean) is record
		case exists is
			when TRUE => point : type_point;
			when FALSE => null;
		end case;
	end record;
	
	-- Returns the point where a track is broken/interrupted
	-- by a line that crosses or overlaps the track.
	-- If place is BEFORE: 
	--  - Returns the point, after the start of the track, where the break begins.
	-- If place is AFTER:
	--  - Returns the point, after the start of the track, where the break ends.
	-- If track and line do not overlap:
	--  - Returns false (no break).
	-- The returned break point is the center of the cap of the track.
	--function get_break_by_line (
		--track				: in type_track;
		--track_dimensions	: in type_track_dimensions;
		--line				: in type_line;
		--place				: in type_place;
		--log_category		: in type_log_category;
		--lth					: in type_log_level)
		--return type_break;

	
	-- A break of a track with an arc may consist of up to
	-- two points where the arc intersects the track:
	subtype type_break_count is natural range 0..2;
	type type_break_double (count : type_break_count) is record
		case count is
			when 0 => null;
			when 1 => point : type_point;
			when 2 => point_1, point_2 : type_point;
		end case;
	end record;
	
	-- Returns the point where a track is broken/interrupted
	-- by an arc that crosses or overlaps the track.
	-- If place is BEFORE: 
	--  - Returns the points, after the start of the track, where the break begins.
	-- If place is AFTER:
	--  - Returns the points, after the start of the track, where the break ends.
	-- If track and line do not overlap:
	--  - Returns a count zero (means no break).
	-- The returned break points are the center of the cap of the track.
	--function get_break_by_arc (
		--track				: in type_track;
		--track_dimensions	: in type_track_dimensions;
		--arc					: in type_arc;
		--place				: in type_place;
		--log_category		: in type_log_category;
		--lth					: in type_log_level)
		--return type_break_double;

	-- Returns the point where a track is broken/interrupted
	-- by a circle that crosses or overlaps the track.
	-- If place is BEFORE: 
	--  - Returns the points, after the start of the track, where the break begins.
	-- If place is AFTER:
	--  - Returns the points, after the start of the track, where the break ends.
	-- If track and line do not overlap:
	--  - Returns a count zero (means no break).
	-- The returned break points are the center of the cap of the track.
	--function get_break_by_circle (
		--track				: in type_track;
		--track_dimensions	: in type_track_dimensions;
		--circle				: in type_circle;
		--place				: in type_place;
		--log_category		: in type_log_category;
		--lth					: in type_log_level)
		--return type_break_double;
	

	-- Returns true if the given point comes after the 
	-- start point of the given track. 
	-- Assumes that the point is on the center line of the track. 
	-- If the point is not on the track, raises constraint error:
	--function after_start_of_track (
		--track	: in type_track;
		--point	: in type_point)
		--return boolean;
	

	type type_valid is (VALID, INVALID);
	
	type type_route_distance (status : type_valid) is record
		case status is
			when VALID		=> distance : type_distance_positive;
			when INVALID	=> null;
		end case;
	end record;


	type type_observe_foreign_nets is new boolean;
	
	
	type type_fill_zone (observe : boolean := FALSE) is record
		case observe is
			when TRUE => 
				outline : et_fill_zones.type_zone (SOLID);
				-- The fill style does not matter.

			when FALSE => null;
		end case;
	end record;
	
	-- If place is BEFORE: 
	--  - Returns the distance from start_point to the nearest obstacle
	--    with status VALID.
	--  - If the start point does not qualify to start a track then
	--    the return is INVALID.
	--  - If there is no obstacle then the return is VALID and the
	--    returned distance is the maxium (type_distance'last).
	-- If place is AFTER: 
	--  - Returns the distance from start_point to the nearest point,
	--    after one or more obstacles, that qualifies to start a track.
	--  - If no suitable point found then the return is INVALID.
	--  - If no obstacle found then the return is INVALID.
	-- If the parameter "ignore_same_net" is true, then the segments
	-- of the same net as indicated by net_cursor are ignored. When filling
	-- fill areas (polygons) this setting should be used.
	--function get_distance (
		--module_cursor	: in pac_generic_modules.cursor;
		--design_rules	: in type_design_rules;
		--bottom_layer	: in type_signal_layer;
		--start_point		: in type_point;
		--place			: in type_place := BEFORE;
		--direction		: in type_rotation;
		--net_cursor		: in et_schematic.pac_nets.cursor := et_schematic.pac_nets.no_element;
		--net_class		: in type_net_class;
		--fill_zone		: in type_fill_zone;
		--layer			: in type_signal_layer;
		--width			: in type_track_width;
		--ignore_same_net	: in boolean;
		--log_category	: in type_log_category := log_category_default;
		--lth				: in type_log_level)
		--return type_route_distance;


	-- Returns true if a track can be started at the given start_point.
	-- If parameter "ignore_same_net" is true then the segments of
	-- the net indicated by net_cursor are ignored. This setting should
	-- be used when filling fill areas (polygons):
	--function clear_for_track (
		--module_cursor	: in pac_generic_modules.cursor;
		--design_rules	: in type_design_rules;
		--bottom_layer	: in type_signal_layer;
		--start_point		: in type_point;
		--net_cursor		: in et_schematic.pac_nets.cursor;
		--net_class		: in type_net_class;
		--fill_zone		: in type_fill_zone;
		--layer			: in type_signal_layer;
		--width			: in type_track_width;
		--ignore_same_net	: in boolean;
		--log_category	: in type_log_category := log_category_default;
		--lth				: in type_log_level)		
		--return boolean;

	
end et_routing;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
