------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       BOARD OPERATIONS / VIAS                            --
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


with ada.containers;   	         	use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;

with et_nets;						use et_nets;
with et_vias;						use et_vias;
with et_net_names;					use et_net_names;
with et_logging;					use et_logging;


package et_board_ops.vias is

	
	-- Returns the positions (x/y) of all vias of the given net.
	-- The list of returned points uses fixed point coordinates
	-- as the vias are placed by the operator (their positions are man-made):
	function get_via_positions (
		net_cursor : in pac_nets.cursor)
		return pac_points.list;



	
	-- When a via is to be modified or deleted in the board, then
	-- it must be clearly identified. Since vias have no name, a useful
	-- means to identify a via is the associated net:
	type type_proposed_via (category : type_via_category := THROUGH) is record
		via	: type_via (category);
		net	: pac_net_name.bounded_string := no_name; -- GND, CLK
	end record;


	-- When vias are selected among others then we collect them in 
	-- a list:
	package pac_proposed_vias is new indefinite_doubly_linked_lists (type_proposed_via);

	
	-- Returns the position and net name of a proposed via:
	function to_string (
		via	: in pac_proposed_vias.cursor)
		return string;
	
	
	-- Returns all vias in the vicinity of the given point:
	function get_vias (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
		return pac_proposed_vias.list;


	-- Sets the proposed-flag of all vias which are
	-- in the given zone around the given place
	-- Adds to count the number of vias that have been found:
	procedure propose_vias (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level);


	-- Clears the proposed-flag and the selected-flag of all vias:
	procedure reset_proposed_vias (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);


	type type_object_via is record
		via_cursor	: pac_vias.cursor;
		net_cursor	: pac_nets.cursor;
	end record;

	package pac_objects is new doubly_linked_lists (type_object_via);


	function get_net_name (
		object : in pac_objects.cursor)
		return pac_net_name.bounded_string;
	
	

	-- Returns the number of items stored in the given list:
	function get_count (
		objects : in pac_objects.list)
		return natural;



	-- Returns the first via according to the given flag.
	-- If no vias has been found,
	-- then the selector via_cursor in the return is no_element
	-- and the selector net_cursor is no_element:
	function get_first_object (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_via;

	
	-- Collects all vias
	-- according to the given flag and returns them in a list:
	function get_objects (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return pac_objects.list;



	-- Modifies the status flag of a via:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object_via;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);


	-- Modifies the status flag of a via indicated by a cursor:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		object_cursor	: in pac_objects.cursor;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);


	
	
	-- Places a via in the given net:
	procedure place_via (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- reset_n
		via				: in type_via;
		log_threshold	: in type_log_level);


	-- Returns the name of the net that is connected
	-- with the given via:
	function get_net (
		module_cursor	: in pac_generic_modules.cursor;
		via				: in type_via)
		return pac_net_name.bounded_string;
	
	
	-- Moves an object:
	procedure move_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object_via;
		coordinates		: in type_coordinates; -- relative/absolute		
		destination		: in type_vector_model; -- x/y
		log_threshold	: in type_log_level);


	-- Deletes an object:
	procedure delete_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object_via;
		log_threshold	: in type_log_level);

	
end et_board_ops.vias;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
