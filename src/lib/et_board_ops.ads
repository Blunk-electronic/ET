------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           BOARD OPERATIONS                               --
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
-- 	  - rework procedures so that a module cursor
--		is used instead the module_name.
--    - move stuff related to signal layers to separate child package


with ada.text_io;				use ada.text_io;
with ada.strings;				use ada.strings;
with ada.exceptions;			use ada.exceptions;

with ada.containers;			use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;

with et_meta;
with et_nets;						use et_nets;
with et_net_names;					use et_net_names;
with et_module_names;				use et_module_names;
with et_module_instance;			use et_module_instance;
with et_primitive_objects;			use et_primitive_objects;
with et_axes;						use et_axes;
with et_string_processing;			use et_string_processing;
with et_logging;					use et_logging;
with et_generic_module;				use et_generic_module;

with et_text;

with et_terminals;					use et_terminals;
with et_pcb;						use et_pcb;
with et_pcb_signal_layers;			use et_pcb_signal_layers;
with et_pcb_stack;					use et_pcb_stack;
with et_pcb_sides;					use et_pcb_sides;
with et_board_layer_category;		use et_board_layer_category;
with et_board_coordinates;			use et_board_coordinates;
with et_board_geometry;				use et_board_geometry;
use et_board_geometry.pac_geometry_2;

with et_board_text;					use et_board_text;

with et_assembly_variant_name;		use et_assembly_variant_name;
with et_pick_and_place;
with et_design_rules_board;			use et_design_rules_board;

with et_exceptions;					use et_exceptions;
with et_object_status;				use et_object_status;
with et_logging;					use et_logging;
with et_exceptions;					use et_exceptions;


package et_board_ops is

	use pac_generic_modules;


	-- This function fetches the basic meta 
	-- information of the board:
	function get_basic_meta_information (
		module : in pac_generic_modules.cursor)
		return et_meta.type_basic;


	
	-- Returns the list of preferred board 
	-- libraries (non-electrical packages):
	function get_preferred_libraries (
		module : in pac_generic_modules.cursor)
		return et_meta.pac_preferred_libraries_board.list;

	
	
	-- Adds a signal layer to the board.
	-- Renumbers the signal layers.							
	procedure add_layer (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		layer			: in et_pcb_stack.type_layer; -- incl. conductor and dieelectic thickness
		log_threshold	: in type_log_level);

	
	-- Returns the total number of signal layers used by the given module.
	function get_layer_count (
		module_cursor	: in pac_generic_modules.cursor) 
		return type_signal_layer;

	
	-- Tests whether the given layer is allowed according to current layer stack
	-- of the given board.
	procedure test_layer (
		module_cursor	: in pac_generic_modules.cursor;
		layer			: in type_signal_layer);

	
	-- Deletes a signal layer in the board.
	-- Renumbers the signal layers.							   
	procedure delete_layer (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		layer			: in type_signal_layer;
		log_threshold	: in type_log_level);

	
	-- Moves a submodule instance within the parent module layout in x/y direction.
	-- Leaves rotation and face (top/bottom) as it is.
	procedure move_submodule (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		coordinates		: in type_coordinates; -- relative/absolute		
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level);

	
	-- Exports a pick & place file from the given top module and assembly variant.
	-- CS: The rotation of submodules is currently ignored. The rotation defaults to zero degree.
	--     See comment in procedure query_submodules.
	procedure make_pick_and_place (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		log_threshold	: in type_log_level);

	
	
	-- Tests the given set of signal layers whether each of them is available
	-- according to the current layer stack of the given module.
	procedure test_layers (
		module_cursor	: in pac_generic_modules.cursor;
		layers 			: in pac_signal_layers.set);	



	-- Returns the index of the deepest conductor layer of the given module:
	function get_deepest_conductor_layer (
		module	: in pac_generic_modules.cursor)
		return type_signal_layer;





	


	-- Returns true if a design rules file for the layout has been
	-- assigned to the given module.
	function layout_rules_assigned (
		module	: in pac_generic_modules.cursor) -- the module like motor_driver
		return boolean;

	
	-- Returns the PCB design rules of the given module:
	function get_pcb_design_rules (
		module	: in pac_generic_modules.cursor) -- the module like motor_driver
		return type_design_rules_board; -- JLP_ML4_standard.dru

	
	function get_user_settings (
		module	: in pac_generic_modules.cursor) -- the module like motor_driver
		return et_pcb.type_user_settings;

	
end et_board_ops;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
