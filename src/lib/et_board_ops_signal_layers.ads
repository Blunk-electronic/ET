------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    BOARD OPERATIONS / SIGNAL LAYERS                      --
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
--   ToDo: 
--
--
--


with et_module_names;				use et_module_names;
with et_generic_modules;			use et_generic_modules;
with et_module;						use et_module;

with et_pcb_signal_layers;			use et_pcb_signal_layers;
with et_pcb_stack;					use et_pcb_stack;

with et_board_coordinates;			use et_board_coordinates;
with et_board_geometry;				use et_board_geometry;
use et_board_geometry.pac_geometry_2;

with et_logging;					use et_logging;



package et_board_ops_signal_layers is

	use pac_generic_modules;



	
	
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

	
	
	
	-- Tests the given set of signal layers whether each of them is available
	-- according to the current layer stack of the given module.
	procedure test_layers (
		module_cursor	: in pac_generic_modules.cursor;
		layers 			: in pac_signal_layers.set);	



	-- Returns the index of the deepest conductor layer of the given module:
	function get_deepest_conductor_layer (
		module	: in pac_generic_modules.cursor)
		return type_signal_layer;


	
end et_board_ops_signal_layers;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
