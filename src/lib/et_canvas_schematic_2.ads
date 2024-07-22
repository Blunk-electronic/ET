------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           CANVAS SCHEMATIC                               --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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
-- This package draws the schematic via various child packages.
-- This package instantiates the generic canvas package (et_canvas_general.pac_canvas)
-- and extends the type_view by the type_drawing. The latter is the link
-- to the actual drawing. The type_drawing provides information on sheet size,
-- fame bounding box, paper size etc. This information is frequently used
-- by various draw operations.
--  Further-on the generic package for primitve draw operations (et_canvas_draw.pac_draw)
-- is instantiated here so that lots of draw operations can use pac_draw_package.

with ada.strings;					use ada.strings;
--with ada.characters;				use ada.characters;
--with ada.characters.handling;		use ada.characters.handling;
with ada.strings.fixed; 			use ada.strings.fixed;

with gdk.event;						use gdk.event;
with gdk.types;						use gdk.types;
with gdk.types.keysyms;				use gdk.types.keysyms;

with gtk;
with gtk.widget;					use gtk.widget;
with gtk.window;
with gtk.gentry;					use gtk.gentry;
with gtk.button;					use gtk.button;
with gtk.box;						use gtk.box;
with gtk.label;						use gtk.label;
with gtk.combo_box_text;			use gtk.combo_box_text;

with glib;							use glib;
with cairo;							use cairo;

with et_logical_pixels;				use et_logical_pixels;


with et_general;					use et_general;
-- with et_geometry;					use et_geometry;

with et_coordinates_2;				use et_coordinates_2;
-- use et_coordinates.pac_geometry_sch;
-- use et_coordinates.pac_geometry_2;
-- 
with et_schematic_shapes_and_text;		use et_schematic_shapes_and_text;

with et_project;
with et_project.modules;			use et_project.modules;
-- with et_symbols;
-- with et_schematic;
-- with et_schematic_ops;
-- with et_schematic_ops.nets;
-- with et_schematic_ops.units;
-- with et_frames;
-- with et_text;						use et_text;

with et_canvas.drawing_frame;
with et_string_processing;			use et_string_processing;
with et_logging;					use et_logging;


package et_canvas_schematic_2 is

	
	use et_project.modules.pac_generic_modules;


	title : constant string := system_name & " SCHEMATIC ";
	
	
	procedure set_title_bar (
		-- CS project name								
		module		: in pac_module_name.bounded_string);


	-- Updates the verb/noun display:
	procedure update_mode_display;

	
	-- Instantiate the canvas package:
	package pac_canvas is new et_canvas (
		canvas_name		=> "schematic", -- CS provide domain name like scripting.type_domain
		pac_geometry	=> et_coordinates_2.pac_geometry_2,
		pac_grid		=> et_coordinates_2.pac_grid,								
		pac_offsetting	=> et_coordinates_2.pac_polygon_offsetting,
		pac_polygons	=> et_coordinates_2.pac_polygons,
		pac_contours	=> et_coordinates_2.pac_contours,
		pac_text		=> pac_text_schematic
		);
		
	
	use pac_canvas;
	use et_coordinates_2.pac_geometry_2;

	package pac_drawing_frame is new pac_canvas.drawing_frame;
	

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
	

	-- This function is called each time the canvas 
	-- is to be refreshed.
	-- It is called by the signal "on_draw" emitted by the canvas.
	-- The connection is set up in procedure set_up_canvas.
	--
	-- NOTE: This function is also called by other signals, such as
	-- "grab_focus". The corresponding connection is active by default.
	--
	-- It draws everything related to the schematic: frame, grid, cursor, objects
	function cb_draw (
		canvas		: access gtk_widget_record'class;
		context_in	: in cairo.cairo_context)
		return boolean;


	-- Connects additional canvas signals with subprograms:
	procedure set_up_canvas;

	

-- SHEET:
	
	box_sheet		: gtk_vbox;
	label_sheet		: gtk_label;
	cbox_sheet		: gtk_combo_box_text;

	procedure update_sheet_number_display;
	procedure build_sheet_number_display;

	

	-- The current active sheet:
	current_active_sheet : et_coordinates_2.type_sheet := type_sheet'first;

	

-- CONSOLE:
	
	procedure connect_console;


	
	-- Composes a console command like 
	-- "schematic motor_driver execute script my_script.scr"
	-- and sends it to procedure et_scripting.schematic_cmd
	-- to be executed.:
	procedure execute_script (script : in pac_script_name.bounded_string);	

	
	-- Executes a command as typed on the console by the operator
	-- like "rename device R1 R2".
	-- Calls et_scripting.schematic_cmd for the actual execution.
	procedure execute_command (self : access gtk_entry_record'class);


	
-- 	label_console_text : constant string := 
-- 		(8 * " ") & "switch module: F11 / F12";
-- 
-- 	-- Appends the label_console_text to the existing text
-- 	-- of label_console:
-- 	procedure set_label_console;



-- REDRAW / REFRESH:

	-- Redraws the schematic:
	procedure redraw_schematic;

	-- Redraws the board:
	procedure redraw_board;

	-- Redraws both schematic and board:
	procedure redraw;


	

-- MODULE SELECT:
	
	-- Advances no previous generic module. If there is no
	-- previous module, selects the last module of 
	-- collection of generic modules.
	procedure next_module;

	
	-- Advances no next generic module. If there is no
	-- next module, selects the first module of 
	-- collection of generic modules.
	procedure previous_module;

	
	
	-- Returns the name of the currently active module:
	function active_module return pac_module_name.bounded_string;


	-- Sets the active module to be displayed in the canvas.
	-- The module must exist inside the current project directory.
	procedure set_module (
		module	: in pac_module_name.bounded_string); -- motor_driver

	
	-- Sets the global variables "current_active_module" and "current_active_sheet".
	-- Sets the grid values to be displayed in the coordinates display.
	procedure init_drawing (
		module	: in et_project.modules.pac_generic_modules.cursor; -- the module to be drawn in schematic and layout
		sheet	: in et_coordinates_2.type_sheet := et_coordinates_2.type_sheet'first); -- the sheet to be drawn

	

-- 	-- These procedures set the grid as entered in the grid box:
-- 	
-- 	procedure set_grid_x (self : access gtk_entry_record'class);
-- 	-- Additionally sets the grid for y. Mostly grid of x and y axis are the same.
-- 	
-- 	procedure set_grid_y (self : access gtk_entry_record'class);
-- 
-- 
-- 	-- Multiplier for grid densities:
-- 	grid_density_multiplier_coarse	: constant type_distance_positive := 10.0;
-- 	grid_density_multiplier_normal	: constant type_distance_positive := 1.0;
-- 	grid_density_multiplier_fine	: constant type_distance_positive := 0.1;
-- 
-- 	-- Resets the grid density to default and snaps the cursor
-- 	-- to the nearest grid point.
-- 	-- Updates the coordinates display.
-- 	procedure reset_grid_and_cursor (
-- 		self : not null access type_view);
-- 
-- 	-- Sets the grid density and snaps the cursor
-- 	-- to the nearest grid point.
-- 	-- Updates the coordinates display.
-- 	procedure set_grid (
-- 		self	: not null access type_view;
-- 		density	: in type_grid_density); -- COARSE, NORMAL, FINE
-- 
-- 	
-- 	overriding procedure move_cursor (
-- 		self		: not null access type_view;
-- 		coordinates	: in type_coordinates;  -- relative/absolute
-- 		cursor		: in out type_cursor;
-- 		position	: in type_vector_model);
-- 
-- 	overriding procedure move_cursor (
-- 		self		: not null access type_view;
-- 		direction	: in type_cursor_direction; -- right, left, up, down
-- 		cursor		: in out type_cursor);
-- 
-- 	cursor_line_width : constant type_distance_positive := 0.8;
-- 	cursor_half_size : constant type_distance_positive := 50.0;
-- 	type type_cursor_line is new et_coordinates.pac_geometry_2.type_line with null record;
-- 	-- CS: Cursor stuff must be based on float numbers.
-- 	
-- 	overriding procedure draw_cursor (
-- 		self		: not null access type_view;
-- 		cursor		: in type_cursor);
-- 
-- 	overriding function get_grid (
-- 		self : not null access type_view)
-- 		return type_grid;
-- 
-- 	overriding function get_frame (
-- 		self : not null access type_view)
-- 		return et_frames.type_frame;
-- 
-- 	
-- 	overriding function get_frame_height (
-- 		self : not null access type_view)
-- 		return type_float_positive;
-- 
-- 	
-- 	overriding function frame_width (
-- 		self : not null access type_view)
-- 		return type_float_positive;
-- 	
-- 	
-- 	overriding function title_block_position (
-- 		self : not null access type_view)
-- 		return et_frames.type_position;
-- 
-- 
-- 	overriding function get_verb (
-- 		self	: not null access type_view)
-- 		return string;
-- 
-- 	overriding function get_noun (
-- 		self	: not null access type_view)
-- 		return string;
-- 
-- 	-- Resets global variables required for selections, clarifications, ...
-- 	-- Verb and noun remain as they are
-- 	-- so that the mode is unchanged.
-- 	-- Should be called when exception rises in order to clean up.
-- 	-- Should also be called when the operator hits ESC.
-- 	procedure reset_selections;

	-- Clears list of proposed objects such as net segments, units, ...
	procedure clear_proposed_objects;
	
-- 	overriding procedure key_pressed (
-- 		self		: not null access type_view;
-- 		key			: in gdk_key_type;
-- 		key_shift	: in gdk_modifier_type);
-- 
-- 	overriding procedure mouse_moved (
-- 		self	: not null access type_view;
-- 		point	: in type_vector_model);
-- 	
-- 	overriding procedure button_pressed (
-- 		self	: not null access type_view;
-- 		button	: in type_mouse_button;
-- 		point	: in type_vector_model);
-- 
-- 	overriding procedure reset_properties_selection (
-- 		self : not null access type_view);
-- 
-- 
-- 	status_text_module_saved : constant string := "Module saved.";
-- 
-- 	-- This procedure saves the current active module 
-- 	-- in its file. This procedure is called from
-- 	-- the overridden procedure save_drawing.
-- 	procedure save_module;
-- 
-- 	-- Saves the current module by calling save_module 
-- 	-- (see above):
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
-- 

	
end et_canvas_schematic_2;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
