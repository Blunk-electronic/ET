------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           CANVAS FOR BOARD                               --
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
-- DESCRIPTION:
-- 
-- This package draws the pcb layout (board) via various child packages.
-- This package instantiates the generic canvas package (et_canvas_general.pac_canvas)
-- and extends the type_view by the type_drawing. The latter is the link
-- to the actual drawing. The type_drawing provides information on sheet size,
-- fame bounding box, paper size etc. This information is frequently used
-- by various draw operations.
--  Further-on the generic package for primitve draw operations (et_canvas_draw.pac_draw)
-- is instantiated here so that lots of draw operations can use pac_draw_package.

with gdk.event;						use gdk.event;
with gdk.types;						use gdk.types;
with gdk.types.keysyms;				use gdk.types.keysyms;

with gtk;
with gtk.widget;					use gtk.widget;
with gtk.gentry;					use gtk.gentry;
with gtk.button;					use gtk.button;

with glib;							use glib;
with cairo;							use cairo;

-- with et_net_names;					use et_net_names;
with et_script_names;				use et_script_names;
with et_logical_pixels;				use et_logical_pixels;

with et_pcb_sides;					use et_pcb_sides;
with et_board_geometry;				use et_board_geometry;
with et_board_coordinates;			use et_board_coordinates;

with et_board_text;
with et_board_layer_category;			use et_board_layer_category;
-- with et_vias;						use et_vias;
-- with et_terminals;					use et_terminals;
-- with et_conductor_segment;
with et_project;
with et_generic_module;				use et_generic_module;
-- with et_frames;

with et_canvas.drawing_frame;
with et_canvas.contours;
with et_canvas.text;

with et_string_processing;			use et_string_processing;
with et_logging;					use et_logging;


package et_canvas_board_2 is

	use pac_generic_modules;

	
	-- In the title bar of the main window follwing information
	-- should be displayed:
	-- - system name like ET
	-- - project name
	-- - domain (schematic, board, library (sym, pac, dev))
	-- - module name
	-- This procedure sets the title bar according to
	-- the given project and module name:
	procedure set_title_bar (
		-- CS project name								
		module		: in pac_generic_modules.cursor);

	
	-- Updates the verb/noun display:
	procedure update_mode_display;


-- 	use pac_net_name;

	
	-- This procedure should be called each time after the current active module 
	-- changes. It calls procedures that initialize the values used in property
	-- bars for vias, tracks, ...
	procedure init_property_bars;
	-- CS probably no need anymore ?
	
	
	-- Instantiate the general canvas package:
	package pac_canvas is new et_canvas (
		canvas_name		=> "board", -- CS provide domain name like scripting.type_domain
		pac_geometry	=> et_board_geometry.pac_geometry_2,
		pac_grid		=> et_board_geometry.pac_grid,
		pac_path		=> et_board_geometry.pac_path_and_bend,
		pac_polygons	=> et_board_geometry.pac_polygons,
		pac_offsetting	=> et_board_geometry.pac_polygon_offsetting,
		pac_contours	=> et_board_geometry.pac_contours,
		pac_text		=> et_board_text.pac_text_board		
		);

	
	use pac_canvas;	
	use et_board_geometry.pac_geometry_2;

	package pac_drawing_frame is new pac_canvas.drawing_frame;
	
	package pac_draw_contours is new pac_canvas.contours;

	package pac_draw_text is new pac_canvas.text;



	
	-- This procedure parses the whole database of model objects
	-- and the primitive objects of the drawing frame,
	-- detects the smallest and greatest x and y values used by the model
	-- and sets the global variable bounding_box accordingly.
	-- If the bounding_box has changed, then the flag bounding_box_changed is
	-- set (See below).
	--
	-- It modifies following global veriables:
	-- - bounding_box
	-- - bounding_box_changed
	-- - bounding_box_error
	--
	-- The arguments can be used to:
	-- - Abort on first error. Means NOT to parse the whole database but to
	--   abort the parsing on the first violation of the maximal allowed 
	--   dimensions (width and height).
	-- - Ignore errors. Means to generate a bounding-box that might be
	--   wider or taller than actually allowed. This is useful for debugging
	--   and testing the effects of violations of maximal bounding-box 
	--   dimensions.
	-- - Test only. Means to simulate the compuation of the bounding-box only.
	--   The global variable bounding_box will NOT be touched in any case.
	procedure compute_bounding_box (
		abort_on_first_error	: in boolean := false; 
		-- CS currently not implemented
		
		ignore_errors			: in boolean := false;
		test_only				: in boolean := false);

	
	
	-- This procedure sets the global zoom factor S and translate-offset T
	-- so that all objects of bounding-box fit into the scrolled window.
	-- The zoom center is the top-left corner of bounding-box.
	procedure zoom_to_fit_all;


	-- This callback procedure is called each time the 
	-- button "zoom fit" is clicked.
	procedure cb_zoom_to_fit (
		button : access gtk_button_record'class);


	-- This callback procedure is called each time the 
	-- button "zoom area" is clicked.
	procedure cb_zoom_area (
		button : access gtk_button_record'class);

	
	-- Connects additional button signals with subprograms:
	procedure set_up_command_buttons;
	

	-- This function is called each time the operator
	-- hits a key on the keyboard. It does not matter
	-- which widget inside the main window has the focus.
	-- This callback function is at the top of the event-chain.
	-- It is called at first on a key-press event.
	-- If it returns false, then it signals to the 
	-- next widget in the chain downwards to handle the event
	-- further.
	-- The return should depend on the severity of the key.
	-- For example in case of an "emergency-exit" 
	-- the operator hits the ESC key, which causes the abort of
	-- all pending operations. In this case the return would be true
	-- and the event would not be passed on to any widgets down
	-- the chain.
	function cb_window_key_pressed (
		window	: access gtk_widget_record'class;
		event	: gdk_event_key)
		return boolean;
	


	-- Connects additional key signals with subprograms:
	procedure set_up_main_window;
	

	-- This procedure draws the text that is being placed in a
	-- paired layer. THIS IS ABOUT NON-CONDUCTOR LAYERS.
	-- The properties are taken from variable et_canvas_board_texts.preliminary_text.
	-- The color must be set in advance by the caller.
	-- The verb must be VERB_PLACE and the noun must be NOUN_TEXT. Otherwise
	-- nothing happens here:
	procedure draw_text_being_placed (
		face		: in type_face;
		category	: in type_layer_category);



	-- Draws a live path being drawn in a given layer category.
	-- Uses the parameters in variable preliminary_line.
	-- Computes the bend point (if required) and sets it accordingly
	-- in preliminary_line.
	-- 1. color: The color must be set by the caller. 
	--    Exception: If a conductor
	--    layer is given, then the color is set according to
	--    preliminary_line.signal_layer.
	-- 2. linewidth: The linewidth is taken from preliminary_line.width.
	--    Exception: For route restrict the linewidth zero is used.
	-- 3. Use it also for drawing freetracks in conductor layers.
	-- 4. NOTE: This is NOT for tracks of nets ! See procedure draw_conductors.
	procedure draw_path ( -- CS rename to draw_live_path
		cat : in type_layer_category);
	

	-- CS unify this procedure with draw_path
	procedure draw_live_zone ( 
		cat : in type_layer_category);

	
	
	-- This function is called each time the canvas 
	-- is to be refreshed.
	-- It is called by the signal "on_draw" emitted by the canvas.
	-- The connection is set up in procedure set_up_canvas.
	--
	-- NOTE: This function is also called by other signals, such as
	-- "grab_focus". The corresponding connection is active by default.
	--
	-- It draws everything related to the board: frame, grid, cursor, objects
	function cb_draw (
		canvas		: access gtk_widget_record'class;
		context_in	: in cairo.cairo_context)
		return boolean;



-- UNDO / REDO:
	
	procedure undo;
	
	procedure redo;


	
-- RESET:

	-- This procedure resets a lot of stuff and should
	-- be called when the operator presses the ESCAPE key.
	-- Here the commands to abort any pending 
	-- operations related to the canvas should be placed:
	procedure reset;


	

	-- This callback function is called each time the
	-- operator hits a key and if the canvas has the focus:
	function cb_canvas_key_pressed (
		canvas	: access gtk_widget_record'class;
		event	: gdk_event_key)
		return boolean;


	-- This callback function is called each time the operator
	-- clicks on the canvas.
	-- It sets the focus on the canvas and moves the cursor
	-- to the place where the operator has clicked the canvas.
	function cb_canvas_button_pressed (
		canvas	: access gtk_widget_record'class;
		event	: gdk_event_button)
		return boolean;


	-- This callback function is called each time the operator
	-- releases a mouse button on the canvas.
	function cb_canvas_button_released (
		canvas	: access gtk_widget_record'class;
		event	: gdk_event_button)
		return boolean;



	-- This callback function is called each time the operator
	-- moves the pointer (or the mouse) inside the canvas:
	function cb_canvas_mouse_moved (
		canvas	: access gtk_widget_record'class;
		event	: gdk_event_motion)
		return boolean;

	
	
	-- Connects additional canvas signals with subprograms:
	procedure set_up_canvas;
	

	
-- CONSOLE:
	
	procedure connect_console;
	

	
	-- Composes a console command like 
	-- "board motor_driver execute script my_script.scr"
	-- and sends it to procedure et_scripting.board_cmd
	-- to be executed.
	-- Resets verb and noun in all domains:
	procedure execute_script_console (
		script : in pac_script_name.bounded_string);	

	
	-- Executes a command as typed on the console by the operator
	-- like "rename device R1 R2".
	-- Calls et_scripting.board_cmd for the actual execution.
	procedure execute_command (self : access gtk_entry_record'class);
 
	



	
-- REDRAW / REFRESH:
	
	-- Redraws the board:
	procedure redraw_board;

	-- Redraws the schematic:
	procedure redraw_schematic;

	-- Redraws both schematic and board:
	procedure redraw;



-- MODULE SELECT:
	
	-- Updates title bars, grid display
	-- of the board editor window according to the
	-- current active module:
	procedure update_board_editor;

	


-- 	-- Saves the current module by calling 
-- 	-- et_canvas_schematic.save_module:
-- 	overriding procedure save_drawing (
-- 		self : not null access type_view);
-- 
-- 
-- 	
-- -- UNDO / REDO:
-- 	
-- 	overriding procedure undo (
-- 		self : not null access type_view);
-- 
-- 	overriding procedure redo (
-- 		self : not null access type_view);

	
end et_canvas_board_2;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
