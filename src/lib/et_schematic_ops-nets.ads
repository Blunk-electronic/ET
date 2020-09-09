------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    SCHEMATIC OPERATIONS ON NETS                          --
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

package et_schematic_ops.nets is
	
	type type_net_scope is (
		STRAND,
		SHEET,
		EVERYWHERE
		);

	function between_start_and_end_point (
	-- Returns true if given point sits between start and end point of given segment.
	-- The catch_zone is a means of reducing the accuracy. The greater the catch_zone
	-- the greater can be the distance of point from the segment.
		point 		: in type_point;
		segment 	: in type_net_segments.cursor;
		catch_zone	: in type_catch_zone := zero)
		return boolean;

	function on_segment (
	-- Returns true if given point sits on the given segment.
	-- The catch_zone is a means of reducing the accuracy. The greater the catch_zone
	-- the greater can be the distance of point from the segment.
		point 		: in type_point;
		segment 	: in type_net_segments.cursor;
		catch_zone	: in type_catch_zone := zero)
		return boolean;
	
	procedure rename_net (
	-- Renames a net. The scope determines whether to rename a certain strand,
	-- all strands on a certain sheet or on all sheets.
	-- CS If a particular strand on a sheet is to be renamed, the argument "place"
	-- must provide sheet and x/y start position of strand. In the future x/y can be
	-- any point on any segment of the strand.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name_before	: in et_general.type_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		net_name_after	: in et_general.type_net_name.bounded_string; -- RESET_N, MOTOR_ON_OFF_N	
		scope			: in type_net_scope; -- strand, sheet, everywhere
		place			: in et_coordinates.type_position; -- sheet/x/y
		log_threshold	: in type_log_level);

	procedure delete_net (
	-- Deletes a net. The scope determines whether to delete a certain strand,
	-- all strands on a certain sheet or on all sheets.
	-- CS If a particular strand on a sheet is to be deleted, the argument "place"
	-- must provide sheet and x/y start position of strand. In the future x/y can be
	-- any point on any segment of the strand.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in et_general.type_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		scope			: in type_net_scope; -- strand, sheet, everywhere
		place			: in et_coordinates.type_position; -- sheet/x/y
		log_threshold	: in type_log_level);

	-- Deletes a segment of a net.
	procedure delete_segment (
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in et_general.type_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		place			: in et_coordinates.type_position; -- sheet/x/y
		log_threshold	: in type_log_level);
	
	function no_ports (ports : in type_ports) return boolean;
	-- Returns true if the given record of ports is completely emtpty.
	
	-- Drags a segment of a net.
	procedure drag_segment (
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in et_general.type_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		place			: in et_coordinates.type_position; -- sheet/x/y, the point of attack, this addresses the segment
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_point; -- x/y, the new position 
		log_threshold	: in type_log_level);

	package type_net_names is new doubly_linked_lists (
		element_type	=> et_general.type_net_name.bounded_string,
		"="				=> et_general.type_net_name."=");

	-- Returns lists of nets that cross the given place.
	function nets_at_place (
		module_name		: in type_module_name.bounded_string;
		place			: in et_coordinates.type_position;
		log_threshold	: in type_log_level)
		return type_net_names.list;

	-- Inserts a net segment in the module.
	-- 1. If the start or end point of the new segment
	--    meets a port then the port will be connected with the segment.
	-- 2. If the segment_new collides with a foreign net, an error is raised.
	-- 3. If the net_name is a name of an already existing net, then the
	--    given net segment_new will be added to the existing net. A junction will be
	--    automaticall be placed where the new segment joins the existing net.
	-- 4. If net_cursor equals no_element then a new net named after net_name will be created.
	-- 5. After this procedure net_cursor points to the net that has just been created
	--    or extended by segment_new.
	procedure insert_segment (
		module_cursor	: in pac_generic_modules.cursor;
		net_cursor		: in out type_nets.cursor;
		sheet			: in type_sheet;
		net_name		: in type_net_name.bounded_string;
		segment_new		: in et_schematic.type_net_segment;
		log_threshold	: in type_log_level);

	procedure insert_net (
	-- See description for procedure insert_segment.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in et_general.type_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		start_point		: in et_coordinates.type_position; -- sheet/x/y
		end_point		: in type_point; -- x/y
		log_threshold	: in type_log_level);

	procedure set_scope (
	-- Sets the scope of a net.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in et_general.type_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		scope			: in et_netlists.type_net_scope; -- local/global
		log_threshold	: in type_log_level);

	procedure place_junction (
	-- Places a net junction at the given position.
	-- If the junction is to be placed between start and end point of a segment, then the segment 
	-- is split in two new segments with the junction between them.
	-- If there is no net segment at the given position, no junction is placed and warning issued.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		place			: in et_coordinates.type_position; -- sheet/x/y, rotation doesn't matter
		log_threshold	: in type_log_level);

-- CS
-- 	procedure place_junction (
-- 	-- Places a net junction at the given position.
-- 	-- If the junction is to be placed between start and end point of a segment, then the segment 
-- 	-- is split in two new segments with the junction between them.
-- 	-- If there is no net segment at the given position, no junction is placed and warning issued.
-- 		module_cursor	: in pac_generic_modules.cursor;
-- 		place			: in et_coordinates.type_position; -- sheet/x/y, rotation doesn't matter
-- 		log_threshold	: in type_log_level);

	
	procedure place_net_label (
	-- Places a label next to a segment at position.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		segment_position: in et_coordinates.type_position; -- sheet/x/y
		label_position	: in type_point := origin; -- x/y
		rotation		: in et_coordinates.type_rotation := zero_rotation; -- 0, 90, 180. Relevant for simple labels only.
		appearance 		: in type_net_label_appearance; -- simple/tag label		
		direction		: in et_schematic.type_net_label_direction; -- INPUT, OUTPUT, PASSIVE, ...
		log_threshold	: in type_log_level);

	procedure delete_net_label (
	-- Deletes a label.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		position		: in et_coordinates.type_position; -- sheet/x/y
		log_threshold	: in type_log_level);

	-- Queries the position of the given net. If a stub is at the
	-- given position returns the direction of the stub.
	function query_stub (
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in et_general.type_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		position		: in et_coordinates.type_position; -- sheet/x/y
		log_threshold	: in type_log_level)
		return type_stub;
		
end et_schematic_ops.nets;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
