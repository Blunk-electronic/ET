------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS SCHEMATIC NETS                            --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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

with ada.containers;				use ada.containers;
with ada.containers.doubly_linked_lists;

with et_general;					use et_general;
with et_canvas_general;				use et_canvas_general;
with et_coordinates;				use et_coordinates;
use et_coordinates.pac_geometry_sch;

with et_project.modules;			use et_project.modules;
with et_schematic;					use et_schematic;
with et_schematic_ops;
with et_schematic_ops.nets;			use et_schematic_ops.nets;
with et_string_processing;			use et_string_processing;

package et_canvas_schematic_nets is

	use et_project.modules.pac_generic_modules;

	use type_nets;
	use type_strands;
	use type_net_segments;

	-- Whenever a segment is selected via the GUI, we store its
	-- parent net, strand and the segment itself via this type:
	--type type_selected_segment is tagged record
	type type_selected_segment is record
		net		: type_nets.cursor;
		strand	: type_strands.cursor;
		segment	: type_net_segments.cursor;
	end record;

	-- Deletes a selected segment of a net.
	procedure delete_selected_segment (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		segment			: in type_selected_segment; -- net/strand/segment
		log_threshold	: in type_log_level);

	
	package pac_proposed_segments is new doubly_linked_lists (type_selected_segment);
	use pac_proposed_segments;
	
	-- These variables are used by the GUI when the operator selects a segment:
	proposed_segments	: pac_proposed_segments.list;
	selected_segment	: pac_proposed_segments.cursor;

	procedure clear_proposed_segments;
	
	-- Searches the module for an anonymous net with the lowest index available.
	-- Example: If the module contains nets like N$2, N$4, N$5 and N$101 then
	-- the lowest available name would be N$3.
	function lowest_available_anonymous_net (
		module		: in pac_generic_modules.cursor)
		return type_net_name.bounded_string; -- N$3
	
	-- Returns the net name of the first segment in 
	-- given list of net segments.
	-- If the given list is empty then an empty net name will be returned.
	function first_net (segments : in pac_proposed_segments.list) 
		return type_net_name.bounded_string; -- RESET_N, MASTER_CLOCK
	
	-- Returns true if segments contains more than one segment:
	function more_than_one (segments : in pac_proposed_segments.list) return boolean;

	-- Tests if all given segments belong to the same net. 
	-- Returns false if net names differ.
	function all_belong_to_same_net (
		segments	: in pac_proposed_segments.list)
		return boolean;

	-- Tests if point sits between start and end point of any of the given segments.
	-- Returns true in that case.
	function between_start_and_end_point_of_sloping_segment (
		point		: in type_point;
		segments	: in pac_proposed_segments.list)
		return boolean;
	
-- 	-- Deletes a selected segment of a net.
-- 	procedure delete_selected_segment (
-- 		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
-- 		segment			: in type_selected_segment; -- net/strand/segment
-- 		log_threshold	: in type_log_level);
	
	-- Collects all net segments in the vicinity of the given point:
	function collect_segments (
		module			: in pac_generic_modules.cursor;
		place			: in et_coordinates.type_position; -- sheet/x/y
		catch_zone		: in type_catch_zone; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_proposed_segments.list;


-- DELETE NET SEGMENT
	
	status_delete : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to delete net segment." 
		& status_hint_for_abort;
	
	-- Deletes a net segment in the vicinity of given point.
	-- If more than one segment near point found, then it sets the
	-- cursor selected_segment to the first segment and requests
	-- for clarification.
	procedure delete_net_segment (point : in type_point);

	-- Advances cursor selected_segment to next segment in list proposed_segments.
	procedure clarify_net_segment;

	-- Deletes the net segment being pointed at by cursor selected_segment.
	procedure delete_selected_net_segment;


-- DRAW NET SEGMENT
	
	-- When a net route is being drawn, then this global variable
	-- shall be used:
	net_route : et_schematic.pac_shapes.type_route_live;
	
	-- Resets the components of the net route.
	-- Exception: Leaves the bend style as it is.
	procedure reset_net_route;

	-- Inserts a net segment in the module.
	procedure insert_net_segment (
		module			: in pac_generic_modules.cursor;
		sheet			: in type_sheet;
		segment			: in et_schematic.type_net_segment;
		log_threshold	: in type_log_level);

	-- Returns true if the given point qualifies as start or end point
	-- of a net segment:
	function valid_for_net_segment (
		point			: in type_point;
		log_threshold	: in type_log_level)
		return boolean;


-- DRAG/MOVE NET SEGMENT

	type type_being_moved is new boolean;
	type type_finalizing_granted is new boolean;

	type type_segment is record
		being_moved				: type_being_moved := false;
		tool					: type_tool := MOUSE;
		point_of_attack			: type_point;
		finalizing_granted		: type_finalizing_granted := false;
	end record;

	segment : type_segment;

	status_drag : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to drag net segment." 
		& status_hint_for_abort;
	
	status_move : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to move net segment." 
		& status_hint_for_abort;

	-- This procedure:
	-- - Clears list of proposed segments.
	-- - Sets global variable selected_segment.
	-- - resets global variable "segment" to its default values
	procedure reset_segment;

	-- Assigns the given destination after the drag to the selected segment.
	-- Resets the global variable "segment".
	procedure finalize_drag (
		destination		: in type_point;
		log_threshold	: in type_log_level);
	
	-- Locates all net segments in the vicinity of given point.
	-- If more than one segment near point found, then it sets the
	-- cursor selected_segment to the first segment and requests
	-- for clarification.
	procedure find_segments (point : in type_point);

	
end et_canvas_schematic_nets;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
