------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     BOARD OPERATIONS / NETCHANGERS                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                -- 
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
-- To Do: 
--
--
--

with ada.containers;           			use ada.containers;
with ada.containers.indefinite_doubly_linked_lists;

with et_board_coordinates;				use et_board_coordinates;
with et_board_geometry;					use et_board_geometry;
use et_board_geometry.pac_geometry_2;

with et_module_names;					use et_module_names;
with et_generic_modules;				use et_generic_modules;
with et_netchangers;					use et_netchangers;

with et_object_status;					use et_object_status;

with et_coordinates_abs_rel;			use et_coordinates_abs_rel;
with et_pcb_signal_layers;				use et_pcb_signal_layers;
with et_logging;						use et_logging;



package et_board_ops_netchangers is

	use pac_generic_modules;
	

	-- Returns the x/y coordinates of the given
	-- netchanger:
	function get_netchanger_position (
		module_cursor	: in pac_generic_modules.cursor;
		index			: in type_netchanger_id) -- 1,2,3,...
		return type_vector_model;
		


	-- Moves the given netchanger. 
	-- It is assumed that the netchanger indicated by index
	-- exists in the module. If the netchanger does not exist,
	-- then an exception is raised.
	procedure move_netchanger (
		module_cursor	: in pac_generic_modules.cursor;
		index			: in type_netchanger_id; -- 1,2,3,...
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model; -- x/y, destination or offset
		log_threshold	: in type_log_level);



	-- Sets the signal layer of the given netchanger. 
	-- It is assumed that the netchanger indicated by index
	-- exists in the module. If the netchanger does not exist,
	-- then an exception is raised.
	procedure set_netchanger_layer (
		module_cursor	: in pac_generic_modules.cursor;
		index			: in type_netchanger_id; -- 1,2,3,...
		layer			: in type_signal_layer; -- 8
		log_threshold	: in type_log_level);

	
	-- CS show_netchanger



	type type_object_netchanger is record
		netchanger_cursor : pac_netchangers.cursor;
	end record;
	

	-- Returns the full name of the object netchanger:
	function get_object_name (
		object : in type_object_netchanger)
		return string;


	-- Returns the index of the object netchanger:
	function get_object_id (
		object : in type_object_netchanger)
		return type_netchanger_id;

	

	-- Modifies the status flag of a netchanger.
	-- If the netchanger is set as moving, then its
	-- original position will be backup
	-- in global variable object_original_position:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		netchanger		: in type_object_netchanger;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);



	-- Sets the proposed-flag of all netchangers which are in the
	-- given zone around the given place.
	-- Adds to count the number of netchangers that have been found:
	procedure propose_netchangers (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level);

	

	-- Resets the status flags of netchanger:
	procedure reset_status_netchangers (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);



	-- Returns the first netchanger according to the given flag.
	-- If no netchanger has been found,
	-- then the return is no_element:
	function get_first_netchanger (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_netchanger;





------------------------------------------------------------------------------------------

-- OBJECTS:


	type type_object_category is (
		CAT_VOID,
		CAT_NETCHANGER
		-- CS properties, notes, ...
		);


	-- This type wraps all kinds of objects into a single type:
	type type_object (cat : type_object_category) is record
		case cat is
			when CAT_VOID => null;
			
			when CAT_NETCHANGER =>
				netchanger : type_object_netchanger;

		end case;
	end record;


	
	
	package pac_objects is new indefinite_doubly_linked_lists (type_object);

	
	-- Returns the number of items stored in the given list:
	function get_count (
		objects : in pac_objects.list)
		return natural;




	-- Returns the first object
	-- according to the given flag.
	-- If nothing found, then the return is a void object (CAT_VOID):
	function get_first_object (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object;




	-- Collects all objects 
	-- according to the given flag and returns them in a list:
	function get_objects (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return pac_objects.list;


	


	-- Modifies the status flag of an object:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);

	
	
	-- Modifies the status flag of an object indicated by a cursor:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		object_cursor	: in pac_objects.cursor;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);



	-- This is a collective procedure that resets
	-- the status flags of all 
	-- objects (netchangers, notes, properties, ...):
	procedure reset_status_objects (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);

	
	
	-- Moves an object to the given destination:
	procedure move_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level);

	
end et_board_ops_netchangers;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
