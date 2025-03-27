------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     BOARD OPERATIONS / RATSNEST                          --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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
--   ToDo: 


with ada.containers;   		         	use ada.containers;
with ada.containers.doubly_linked_lists;

with et_nets;							use et_nets;
with et_net_names;						use et_net_names;
with et_ratsnest;						use et_ratsnest;
with et_logging;						use et_logging;

package et_board_ops.ratsnest is


	use pac_net_name;


	-- Returns the start and end positions (x/y) of all track 
	-- segments (lines and arcs) of the given net:
	-- The list of returned points uses fixed point coordinates
	-- as the tracks are placed by the operator (their ends are man-made):
	function get_track_ends (
		net_cursor : in pac_nets.cursor)
		return pac_points.list;


	
	-- (Re)generates the ratsnest of all nets according to the current
	-- positions of vias, tracks and terminals:
	procedure update_ratsnest (
		module_cursor	: in pac_generic_modules.cursor;
		lth				: in type_log_level);



	-- When airwires are to be collected in the vicinity of a certain 
	-- point then each airwire can be identified with a net name:
	
	type type_proposed_airwire is record
		wire		: type_airwire;
		net_name	: pac_net_name.bounded_string; -- RESET_N
	end record;

	package pac_proposed_airwires is new doubly_linked_lists (type_proposed_airwire);
	use pac_proposed_airwires;
	

	-- Returns all airwires in the vicinity of the given point:
	function get_airwires (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
		return pac_proposed_airwires.list;


	-- Sets the proposed-flag of all airwires which are
	-- in the given zone around the given place
	-- Adds to count the number of airwires that have been found:
	procedure propose_airwires (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level);


	-- Clears the proposed-flag and the selected-flag of all airwires.
	procedure reset_proposed_airwires (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);


	type type_object_airwire is record
		wire_cursor	: pac_airwires.cursor;
		net_cursor	: pac_nets.cursor;
	end record;

	package pac_objects is new doubly_linked_lists (type_object_airwire);
	

	-- Returns the number of items stored in the given list:
	function get_count (
		objects : in pac_objects.list)
		return natural;


	
	-- Returns the first airwire according to the given flag.
	-- If no airwire has been found,
	-- then the selector wire_cursor in the return is no_element
	-- and the selector net_cursor is no_element:
	function get_first_object (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_airwire;

	
	-- Collects all airwires
	-- according to the given flag and returns them in a list:
	function get_objects (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return pac_objects.list;

	
	-- Modifies the status flag of an airwire:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object_airwire;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);


	-- Modifies the status flag of an airwire indicated by a cursor:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		object_cursor	: in pac_objects.cursor;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);
	
											
end et_board_ops.ratsnest;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
