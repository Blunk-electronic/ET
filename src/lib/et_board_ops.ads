------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           BOARD OPERATIONS                               --
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

with ada.text_io;				use ada.text_io;
with ada.characters.latin_1;
with ada.strings;				use ada.strings;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers;            use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;
with ada.exceptions;			use ada.exceptions;
with ada.tags;

with et_nets;
with et_net_names;				use et_net_names;
with et_general;				use et_general;
with et_geometry;				use et_geometry;
with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;
with et_project.modules;		use et_project.modules;
with et_schematic;				use et_schematic;
with et_terminals;				use et_terminals;
with et_pcb;					use et_pcb;
with et_pcb_stack;				use et_pcb_stack;
with et_pcb_coordinates;		use et_pcb_coordinates;
use et_pcb_coordinates.pac_geometry_2;

with et_board_shapes_and_text;

with et_assembly_variants;
with et_pick_and_place;
with et_devices;					use et_devices;
with et_conventions;
with et_design_rules;				use et_design_rules;
with et_conductor_segment.boards;	use et_conductor_segment.boards;


with et_route_restrict.boards;
with et_via_restrict.boards;

with et_stop_mask;
with et_stencil;

with et_silkscreen;
with et_silkscreen.boards;

with et_assy_doc;
with et_assy_doc.boards;

with et_text;

with et_exceptions;					use et_exceptions;


package et_board_ops is

	-- CS rework procedures so that a module cursor
	-- is used instead the module_name.

	use et_board_shapes_and_text;
	use pac_contours;
	use pac_text_board;
	use pac_net_name;

	
	-- Moves the origin of the board to the given point (relative to the lower left 
	-- corner of the drawing frame):
	procedure move_board (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		coordinates		: in type_coordinates; -- relative/absolute		
		point			: in type_point; -- x/y
		log_threshold	: in type_log_level);

	
	-- Adds a signal layer to the board.
	-- Renumbers the signal layers.							
	procedure add_layer (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		layer			: in et_pcb_stack.type_layer; -- incl. conductor and dieelectic thickness
		log_threshold	: in type_log_level);

	
	-- Returns the total number of signal layers used by the given module.
	function layer_count (module_cursor	: in pac_generic_modules.cursor) 
		return et_pcb_stack.type_signal_layer;

	
	-- Tests whether the given layer is allowed according to current layer stack
	-- of the given board.
	procedure test_layer (
		module_cursor	: in pac_generic_modules.cursor;
		layer			: in et_pcb_stack.type_signal_layer);

	
	-- Deletes a signal layer in the board.
	-- Renumbers the signal layers.							   
	procedure delete_layer (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		layer			: in et_pcb_stack.type_signal_layer;
		log_threshold	: in type_log_level);

	
	-- Moves a submodule instance within the parent module layout in x/y direction.
	-- Leaves rotation and face (top/bottom) as it is.
	procedure move_submodule (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		coordinates		: in type_coordinates; -- relative/absolute		
		point			: in type_point; -- x/y
		log_threshold	: in type_log_level);

	
	-- Exports a pick & place file from the given top module and assembly variant.
	-- CS: The rotation of submodules is currently ignored. The rotation defaults to zero degree.
	--     See comment in procedure query_submodules.
	procedure make_pick_and_place (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		log_threshold	: in type_log_level);

	

	-- Returns the start and end positions (x/y) of all track 
	-- segments (lines and arcs) of the given net:
	-- The list of returned points uses fixed point coordinates
	-- as the tracks are placed by the operator (their ends are man-made):
	function get_track_ends (
		net_cursor : in et_schematic.pac_nets.cursor)
		return pac_points.list;

	
	-- Sets the grid of the module.
	procedure set_grid (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		grid			: in type_grid;
		log_threshold	: in type_log_level);		

	
	procedure set_grid (
		module_cursor	: in pac_generic_modules.cursor;
		grid			: in type_grid;
		log_threshold	: in type_log_level);


	
-- RATSNEST / TRACKS / FREETRACKS


	-- (Re)generates the ratsnest of all nets according to the current
	-- positions of vias, tracks and terminals:
	procedure update_ratsnest (
		module_cursor	: in pac_generic_modules.cursor;
		lth				: in type_log_level);


	
	-- Tests the given set of signal layers whether each of them is available
	-- according to the current layer stack of the given module.
	procedure test_layers (
		module_cursor	: in et_project.modules.pac_generic_modules.cursor;
		layers 			: in et_pcb_stack.type_signal_layers.set);	

	

	

	
-- TEXT / PLACEHOLDERS

	-- Maps from the meaning of a text placeholder
	-- to its actutal content:
	function to_placeholder_content (
		module_cursor	: in pac_generic_modules.cursor;
		meaning 		: in et_pcb.type_text_meaning)										
		return et_text.pac_text_content.bounded_string;

	
	-- Places a text in a non conductor layer like
	-- silkscreen or assembly doc:
	procedure place_text_in_non_conductor_layer (
		module_cursor	: in pac_generic_modules.cursor;
		layer_category	: in type_text_layer_non_conductor;
		face			: in type_face; -- top/bottom
		text			: in type_text_fab_with_content;
		log_threshold	: in type_log_level);

											
end et_board_ops;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
