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
with ada.exceptions;
with ada.tags;

with et_nets;
with et_net_names;				use et_net_names;
with et_general;				use et_general;
with et_geometry;				use et_geometry;
with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;
with et_project.modules;		use et_project.modules;
with et_schematic;				use et_schematic;
with et_vias;					use et_vias;
with et_terminals;				use et_terminals;
with et_pcb;					use et_pcb;
with et_pcb_stack;				use et_pcb_stack;
with et_pcb_coordinates;		use et_pcb_coordinates;
use et_pcb_coordinates.pac_geometry_2;

with et_board_shapes_and_text;

with et_assembly_variants;
with et_pick_and_place;
with et_devices;				use et_devices;
with et_conventions;
with et_design_rules;			use et_design_rules;
with et_conductor_text.boards;		use et_conductor_text.boards;
with et_conductor_segment.boards;	use et_conductor_segment.boards;
with et_fill_zones;				use et_fill_zones;
with et_route_restrict.boards;	use et_route_restrict.boards;
with et_via_restrict.boards;	use et_via_restrict.boards;
with et_stop_mask;				use et_stop_mask;
with et_stop_mask.boards;		use et_stop_mask.boards;
with et_stencil;				use et_stencil;
with et_stencil.boards;			use et_stencil.boards;
with et_silkscreen;				use et_silkscreen;
with et_silkscreen.boards;		use et_silkscreen.boards;
with et_assy_doc;				use et_assy_doc;
with et_assy_doc.boards;		use et_assy_doc.boards;
with et_keepout;				use et_keepout;
with et_keepout.boards;			use et_keepout.boards;
with et_pcb_contour;			use et_pcb_contour;
with et_text;

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

	
	-- Returns the positions (x/y) of all vias of the given net.
	-- The list of returned points uses fixed point coordinates
	-- as the vias are placed by the operator (their positions are man-made):
	function get_via_positions (
		net_cursor : in et_schematic.pac_nets.cursor)
		return pac_points.list;


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

	
	-- Places a via in the given net:
	procedure place_via (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- reset_n
		via				: in type_via;
		log_threshold	: in type_log_level);

	
	
-- ROUTE RESTRICT
	
	-- Draws route restrict line.
	procedure draw_route_restrict_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		line			: in type_route_restrict_line;
		log_threshold	: in type_log_level);

	
	-- Draws a route restrict arc.
	procedure draw_route_restrict_arc (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		arc				: in type_route_restrict_arc;
		log_threshold	: in type_log_level);

	
	-- Draws a route restrict circle.
	procedure draw_route_restrict_circle (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		circle			: in type_route_restrict_circle;
		log_threshold	: in type_log_level);	

	
	-- Deletes the segment of route restrict that crosses the given point.
	-- CS currently rips up the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
	procedure delete_route_restrict (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		point			: in type_point; -- x/y
		accuracy		: in type_catch_zone;
		log_threshold	: in type_log_level);


	
-- VIA RESTRICT

	-- Draws a via restrict line.
	procedure draw_via_restrict_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		line			: in type_via_restrict_line;
		log_threshold	: in type_log_level);

	
	-- Draws a via restrict arc.
	procedure draw_via_restrict_arc (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		arc				: in type_via_restrict_arc;
		log_threshold	: in type_log_level);

	
	-- Draws a via restrict circle.
	procedure draw_via_restrict_circle (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		circle			: in type_via_restrict_circle;
		log_threshold	: in type_log_level);

	
	-- Deletes the segment of via restrict that crosses the given point.
	-- CS currently rips up the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
	procedure delete_via_restrict (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		point			: in type_point; -- x/y
		accuracy		: in type_catch_zone;
		log_threshold	: in type_log_level);

	
-- BOARD OUTLINE / HOLES / CONTOUR / EDGE CUTS

	-- Assigns the given module a PCB outer edge.
	-- Overwrites the already existing outline as there can
	-- be only one outline:
	procedure set_outline (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		outline			: in type_outer_contour;
		log_threshold	: in type_log_level);


	-- Returns the outer edge of the PCB:
	function get_outline (
		module_cursor	: in pac_generic_modules.cursor)
		return type_outer_contour;

	
	function get_outline (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		log_threshold	: in type_log_level)
		return type_outer_contour;
	

	-- Deletes the segment of the outline that crosses the given point.
	-- CS currently rips up the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
	procedure delete_outline (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		point			: in type_point; -- x/y
		accuracy		: in type_catch_zone;
		log_threshold	: in type_log_level);

	
	
	
	-- Adds a hole to the already existing holes:
	procedure add_hole (
		module_cursor	: in pac_generic_modules.cursor;
		hole			: in type_hole;
		log_threshold	: in type_log_level);

	
	procedure add_hole (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		hole			: in type_hole;
		log_threshold	: in type_log_level);

	
	-- Returns the holes of the given module:
	function get_holes (
		module_cursor	: in pac_generic_modules.cursor)
		return pac_holes.list;

	
	-- Deletes the segment of a hole that crosses the given point.
	-- CS currently rips up the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
	procedure delete_hole (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		point			: in type_point; -- x/y
		accuracy		: in type_catch_zone;
		log_threshold	: in type_log_level);



	
	
-- SILK SCREEN

	procedure draw_silk_screen_line (
	-- Draws a line in the PCB silk_screen.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		line			: in type_silk_line;
		log_threshold	: in type_log_level);

	procedure draw_silk_screen_arc (
	-- Draws an arc in the PCB silk_screen.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		arc				: in type_silk_arc;
		log_threshold	: in type_log_level);

	procedure draw_silk_screen_circle (
	-- Draws a circle in the PCB silk_screen.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;	
		circle			: in type_fillable_circle;
		log_threshold	: in type_log_level);

	procedure delete_silk_screen (
	-- Deletes the segment of the silk_screen that crosses the given point.
	-- CS currently deletes the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		point			: in type_point; -- x/y
		accuracy		: in type_catch_zone;
		log_threshold	: in type_log_level);


-- ASSEMBLY DOCUMENTATION
	-- Draws a line in the assembly documentation.
	procedure draw_assy_doc_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		line			: in et_assy_doc.type_doc_line;
		log_threshold	: in type_log_level);

	procedure draw_assy_doc_arc (
	-- Draws an arc in the assembly documentation.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		arc				: in type_doc_arc;
		log_threshold	: in type_log_level);

	procedure draw_assy_doc_circle (
	-- Draws a circle in the assembly documentation.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;	
		circle			: in type_fillable_circle;
		log_threshold	: in type_log_level);

	procedure delete_assy_doc (
	-- Deletes the segment of the assembly documentation that crosses the given point.
	-- CS currently deletes the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		point			: in type_point; -- x/y
		accuracy		: in type_catch_zone;
		log_threshold	: in type_log_level);

-- KEEPOUT
	procedure draw_keepout_line (
	-- Draws a line in the keepout layer.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		line			: in type_keepout_line;
		log_threshold	: in type_log_level);

	procedure draw_keepout_arc (
	-- Draws an arc in the keepout layer.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		arc				: in type_keepout_arc;
		log_threshold	: in type_log_level);

	procedure draw_keepout_circle (
	-- Draws a circle in the keepout layer.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;	
		circle			: in type_fillable_circle_solid;
		log_threshold	: in type_log_level);

	procedure delete_keepout (
	-- Deletes the segment in the keepout layer that crosses the given point.
	-- CS currently deletes the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		point			: in type_point; -- x/y
		accuracy		: in type_catch_zone;
		log_threshold	: in type_log_level);

-- STOP MASK
	procedure draw_stop_line (
	-- Draws a line in the stop mask layer.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		line			: in type_stop_line;
		log_threshold	: in type_log_level);

	procedure draw_stop_arc (
	-- Draws an arc in the stop mask layer.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		arc				: in type_stop_arc;		
		log_threshold	: in type_log_level);

	procedure draw_stop_circle (
	-- Draws an circle in the stop mask layer.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		circle			: in type_fillable_circle;
		log_threshold	: in type_log_level);

	procedure delete_stop (
	-- Deletes the segment of the stop mask that crosses the given point.
	-- CS currently deletes the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		point			: in type_point; -- x/y
		accuracy		: in type_catch_zone;
		log_threshold	: in type_log_level);

-- STENCIL

	procedure draw_stencil_line (
	-- Draws a line in the stencil layer.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		line			: in type_stencil_line;
		log_threshold	: in type_log_level);

	procedure draw_stencil_arc (
	-- Draws an arc in the stencil layer.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		arc				: in type_stencil_arc;		
		log_threshold	: in type_log_level);

	procedure draw_stencil_circle (
	-- Draws an circle in the stencil layer.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		circle			: in type_fillable_circle;
		log_threshold	: in type_log_level);

	procedure delete_stencil (
	-- Deletes the segment of the stencil that crosses the given point.
	-- CS currently deletes the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		point			: in type_point; -- x/y
		accuracy		: in type_catch_zone;
		log_threshold	: in type_log_level);

	
-- TEXT
	
	-- Places a text in a non conductor layer like
	-- silkscreen or assembly doc:
	procedure place_text_in_non_conductor_layer (
		module_cursor	: in pac_generic_modules.cursor;
		layer_category	: in type_layer_category_non_conductor;
		face			: in type_face; -- top/bottom
		text			: in type_text_fab_with_content;
		log_threshold	: in type_log_level);

	
	-- Places a text in the outline layer:
	procedure place_text_in_outline_layer (
		module_cursor	: in pac_generic_modules.cursor;
		layer_category	: in type_layer_category_outline;
		text			: in type_text_fab_with_content;
		log_threshold	: in type_log_level);

	
	-- Places a text in a conductor layer.
	-- Detects by the layer category whether mirroring the text is required or not:
	procedure place_text_in_conductor_layer (
		module_cursor	: in pac_generic_modules.cursor;
		layer_category	: in type_layer_category_conductor;
		signal_layer	: in type_signal_layer;
		text			: in type_text_fab_with_content;
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
	procedure fill_fill_zones (
		module_cursor	: in pac_generic_modules.cursor;	
		log_category	: in type_log_category;
		log_threshold	: in type_log_level;
		nets 			: in pac_net_names.list := no_net_names); -- GND, GNDA, P3V3, ...

											
end et_board_ops;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
