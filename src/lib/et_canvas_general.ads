------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            CANVAS GENERAL                                --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
--                                                                          --
--         Bases on the package gtkada.canvas_view written by               --
--         E. Briot, J. Brobecker and A. Charlet, AdaCore                   --
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
--	This package provides general things required to set up a canvas. A canvas
--  is also referred to as "view". 
--  Since the canvas is used in various drawings like schematic, pcb-layout, ...
--  it must be instantiated with the package that provides the respective
--  measurement system.

with gtk.main;					use gtk.main;
with gtk.widget;  				use gtk.widget;
with gtk.window; 				use gtk.window;
with gtk.box;					use gtk.box;
with gtk.frame;					use gtk.frame;
with gtk.scrolled_window;		use gtk.scrolled_window;
with gtk.handlers;				use gtk.handlers;
with gtk.enums;					use gtk.enums;
with gtk.adjustment;			use gtk.adjustment;
with gtk.combo_box_text;		use gtk.combo_box_text;
with gtk.gentry;				use gtk.gentry;
with gtk.label;					use gtk.label;
with gtk.toolbar; 				use gtk.toolbar;
with gtk.tool_button;			use gtk.tool_button;

with gdk;						use gdk;
with gdk.types;					use gdk.types;
with gdk.event;					use gdk.event;

with glib;						use glib;
with glib.object;				use glib.object;
with glib.values;				use glib.values;
with glib.properties.creation;	use glib.properties.creation;

with gtkada.style;
with gdk.rgba;

with cairo;						use cairo;

with pango.layout;				use pango.layout;
with system.storage_elements;	use system.storage_elements;

with et_general;				use et_general;
with et_geometry;				use et_geometry;

with et_geometry_1.et_polygons;
with et_geometry_1.et_polygons.offsetting;

with et_geometry_2;
with et_geometry_2.contours;
with et_text;
with et_frames;
with et_meta;
with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;
with et_colors;

package et_canvas_general is
	
	status_hint_for_abort		: constant string := " Hit ESC to abort.";
	status_click_left			: constant string := "LEFT click ";
	status_click_right			: constant string := "RIGHT click ";
	status_press_space			: constant string := "press SPACE ";
	status_start_point			: constant string := "start point";
	status_end_point			: constant string := "end point";
	status_set_end_point		: constant string := "to set end point.";
	status_set_start_point		: constant string := "to set start point.";

	status_next_object_clarification : constant string := 
		"For next object click RIGHT or press page-down.";
		--& " Confirm selection with LEFT click or SPACE key.";


	message_border_reached	: constant string := "Border of drawing area reached !";

	
	primary_tool_default : constant type_tool := MOUSE;

	
	

	-- In graphical mode (other than runmode headless) and 
	-- single command entry mode (cmd_entry_mode) a command may be
	-- incomplete. It will then be completed via an interactive
	-- graphical completition process.
	-- If the command is waiting for finalization (like pressing space key
	-- to place a unit or to draw a net) then the flag 
	-- finalization_pending goes true.
	type type_single_cmd_status is record

		-- the command to be executed like "schematic blood_sample_analyzer set value C1 100n"
		cmd			: type_fields_of_line;

		-- Goes false if too few arguments given via console:
		complete	: boolean := true;

		-- Indicates that the command is in progress,
		-- but not finalized yet:
		finalization_pending : boolean := false;
	end record;	


	-- In graphical mode, scripts can be nested.
	-- In script mode we register only the first
	-- exception regardless of the nesting depth.
	-- Because the operator needs to know which script
	-- has actually failed at which line.
	-- The failed script will then be output in the status bar.
	-- IN HEADLESS MODE THIS STUFF HAS NO MEANING !
	-- For this reason this type is provided:
	type type_script_cmd_status is record
		-- the name of the script file like "rename_power_nets.scr":
		script_name	: pac_script_name.bounded_string;

		-- the command to be executed like "schematic blood_sample_analyzer set value C1 100n"
		cmd			: type_fields_of_line;

		-- the flag that indicates whether the command failed
		failed		: boolean := false;
	end record;

	-- The global variable that stores the status of the latest
	-- script command.
	-- IN HEADLESS MODE THIS STUFF HAS NO MEANING !
	script_cmd_status : type_script_cmd_status;



	
	


---------------------------------------------------------------------------
	
generic

	canvas_name : string; -- schematic, board, package, device, symbol, ...

	with package pac_geometry_2 is new et_geometry_2 (<>);
	with package pac_polygons is new pac_geometry_2.pac_geometry_1.et_polygons;
	with package pac_offsetting is new pac_polygons.offsetting;
	with package pac_contours is new pac_geometry_2.contours;
	
	with package pac_text is new et_text.generic_pac_text (
		-- The used text package must have been instantiated with
		-- these packages:
		pac_geometry_2	=> pac_geometry_2,
		pac_polygons	=> pac_polygons,
		pac_offsetting	=> pac_offsetting,
		others			=> <>);


package pac_canvas is
	
	--use pac_geometry_2;
	use pac_geometry_2.pac_geometry_1; -- inside pac_geometry_2

	use pac_contours;

	
	window 					: gtk_window; -- the main window.	

	box_main				: gtk_vbox;
	box_back				: gtk_hbox;
	box_left, box_right		: gtk_vbox;

	label_status			: gtk_label;

	procedure set_status (text : in string);
	procedure status_clear;
	procedure status_enter_verb;
	procedure status_enter_noun;
	procedure status_verb_invalid;
	procedure status_noun_invalid;

	
	box_toolbars				: gtk_hbox;
	toolbar_left, toolbar_right	: gtk_toolbar;
	
	-- Builds the background boxes box_back, box_left, and box_right:
	procedure build_background_boxes;
	
	-- main position display:
	box_positions			: gtk_vbox;		-- the main box around all kinds of position readouts

	-- primary tool
	box_primary_tool		: gtk_vbox;
	label_primary_tool		: gtk_label;
	cbox_primary_tool		: gtk_combo_box_text;

	procedure update_primary_tool_display;
	procedure build_primary_tool_display;
	
	-- grid
	label_grid				: gtk_label;
	box_grid				: gtk_vbox;
	box_grid_density		: gtk_hbox;
	box_grid_x, box_grid_y	: gtk_hbox;

	label_grid_density		: gtk_label;
	cbox_grid_density		: gtk_combo_box_text;
	
	label_grid_x, label_grid_y	: gtk_label;
	grid_x, grid_y				: gtk_combo_box_text;
	
	-- mouse position
	label_mouse_position	: gtk_label;
	box_mouse_position		: gtk_vbox;
	box_mouse_position_x	: gtk_hbox;
	box_mouse_position_y	: gtk_hbox;
	
	label_mouse_position_x, label_mouse_position_y : gtk_label;
	mouse_position_x, mouse_position_y : gtk_combo_box_text;

	-- cursor position
	label_cursor_position	: gtk_label;
	box_cursor_position		: gtk_vbox;
	box_cursor_position_x	: gtk_hbox;
	box_cursor_position_y	: gtk_hbox;
	
	label_cursor_position_x, label_cursor_position_y : gtk_label;
	cursor_position_x, cursor_position_y : gtk_combo_box_text;
	
	-- distances
	type type_distances is record
		label	: gtk_label;
		box		: gtk_vbox;
		box_x, box_y, box_abs, box_angle : gtk_hbox;
		label_x, label_y, label_abs, label_angle : gtk_label;
		display_x, display_y, display_abs, display_angle : gtk_combo_box_text;
	end record;

	distances : type_distances;


	
	-- mode
	type type_mode is record
		box_mode			: gtk_vbox;
		box_mode_verb		: gtk_hbox;
		box_mode_noun		: gtk_hbox;
		label_mode			: gtk_label;
		label_mode_verb		: gtk_label;
		label_mode_noun		: gtk_label;
		cbox_mode_verb		: gtk_combo_box_text;
		cbox_mode_noun		: gtk_combo_box_text;
	end record;

	mode : type_mode;
	
	procedure build_mode_display;
	
		
	
	-- Builds the boxes and combo boxes that display mouse and cursor position,
	-- distance between cursor and mouse pointer.
	-- Places them in box_left.
	procedure build_coordinates_display;

	
	box_console		: gtk_vbox;
	label_console	: gtk_label;
	console			: gtk_combo_box_text;

	-- Builds the console and places it in box_right.
	procedure build_console;

	-- Appends an argument to the given command.
	-- If trim is true, the first two fields of the command
	-- (domain and module name) will be removed.
	-- Displays the command on the console.
	procedure append_argument_to_command (
		cmd		: in out type_fields_of_line;
		argument: in string;
		trim	: in boolean := true);

	
	box_drawing	: gtk_hbox;
	frame		: gtk_frame;
	scrolled	: gtk_scrolled_window;
	
	-- Builds the drawing area and places it in box_right.
	procedure build_canvas;

-- 	-- Scales the canvas so that the frame fits into.
-- 	function window_resized (
-- 		self  : access gtk_widget_record'class;
-- 		event : gdk.event.gdk_event_configure) 
-- 		return boolean;
-- CS: causes the view to shift on moving the window. better don't use it.

	procedure terminate_main;
	
	procedure terminate_main (self : access gtk_widget_record'class);
	--procedure zoom_to_fit (self : access glib.object.gobject_record'class);

	
	procedure set_cursor_position_x (self : access gtk.gentry.gtk_entry_record'class);
	procedure set_cursor_position_y (self : access gtk.gentry.gtk_entry_record'class);

	--procedure set_grid_x (self : access gtk.gentry.gtk_entry_record'class) is null;
	--procedure set_grid_y (self : access gtk.gentry.gtk_entry_record'class) is null;
	
	procedure build_toolbars;

	
	-- This variable serves for logging debug messages an other stuff.
	-- It is assigned with the log level on initializing a main window.
	log_threshold : type_log_level := type_log_level'first;


	-- This signal is emitted by the drawing whenever items are added, moved, resized, ...
-- 	signal_layout_changed : constant glib.signal_name := "layout_changed";
	

	signal_viewport_changed : constant glib.signal_name := "viewport_changed";
	-- This signal is emitted whenever the view is zoomed or scrolled.


	

-- VIEW COORDINATES:
	
	-- The view coordinates are the 
	-- coordinates of items on the screen and are expressed in pixels.
	-- They change when the operators zooms or scrolls.
	-- Subtype type_view_coordinate is gdouble; -- gdouble is a real floating-point type (see glib.ads)
	-- CS: experimental narrowing the range of type_view_coordinate:
	subtype type_view_coordinate is gdouble range -1_000_000.0 .. 1_000_000.0;
	subtype type_view_coordinate_positive is type_view_coordinate range 0.0 .. type_view_coordinate'last;

	-- A point in the view:
	type type_view_point is record
		x, y : type_view_coordinate;
	end record;

	
	function to_string (p : in type_view_point) return string;

	
	-- A rectangular regions of the view:
	type type_view_rectangle is record
		x, y			: type_view_coordinate;
		width, height	: type_view_coordinate_positive;
	end record;

	--  The number of blank pixels on each sides of the view. This avoids having
	--  items displays exactly next to the border of the view.
	view_margin : constant type_view_coordinate := 20.0;


	

-- MODEL COORDINATES:

	-- The model coordinates are required as an intermediate format
	-- to convert between drawning and view coordinates.
	
	type type_model_point is record
		x, y : type_float := 0.0;
	end record;

	model_origin : constant type_model_point := (0.0, 0.0);


	-- Converts from drawing point to model point:
	function to_model_point (
		point	: in pac_geometry_2.type_point)
		return type_model_point;


	-- Inverts a model point along the given axis:
	function invert (
		place	: in type_model_point;
		axis	: in type_axis_2d)
		return type_model_point;


	-- Converts a model point to type offset:
	function to_offset (
		place	: in type_model_point)
		return type_offset;



	-- In connection with boundaries and bounding boxes a type to model
	-- a rectangular area is required.
	-- NOTE: The bounding box is something required in the model plane only.
	type type_bounding_box is record -- CS rename to type_bounding_box
		x, y			: type_float; -- position, upper left corner
		width, height	: type_float_positive; -- size
	end record;

	no_area : constant type_bounding_box := (others => 0.0);

	function to_string (rectangle : in type_bounding_box) return string;

	
	-- Moves the rectangle by the given offset:
	procedure move_by (
		rectangle	: in out type_bounding_box;
		offset		: in type_offset);

	
	-- Returns true if the given two rectangles intersect each other in some way:
	function intersects (rect1, rect2 : type_bounding_box) return boolean;

	
	
	
-- VIEW

	type type_view is abstract new gtk.widget.gtk_widget_record with record

		-- The upper left corner of the visible area has its initial value at 0/0.
		-- NOTE: This has nothing to do with the upper left corner of the
		-- drawing sheet. topleft is not a constant and is changed on by procedure
		-- set_scale or by procedure scale_to_fit.
		topleft  	: type_model_point;

		-- The drawing scale:
		-- - increases on zoom in
		-- - decreases on zoom out
		scale     	: type_scale := scale_default;
		
		layout		: pango.layout.pango_layout; -- CS for displaying text. not used yet

		-- Required for the scrollbars:
		hadj, vadj	: gtk.adjustment.gtk_adjustment;

		-- CS: currently no need:
		-- connections to model signals
		-- id_layout_changed : gtk.handlers.handler_id := (gtk.handlers.null_handler_id, null);

		scale_to_fit_requested	: gdouble := 0.0; -- gdouble is a real floating-point type (see glib.ads)
		scale_to_fit_area		: type_bounding_box;
	end record;

	-- The pointer to the canvas/view:
	type type_view_ptr is access all type_view'class;
	
	canvas	: type_view_ptr;

	
	-- redraws the canvas:
	procedure redraw (view : in type_view_ptr);

	


	function on_key_event (
		self	: access gtk_widget_record'class;
		event	: in gdk_event_key) 
		return boolean;

	
	-- Updates the coordinates display (grid, cursor, pointer, distances, ...).
	-- Called on mouse or cursor movement.
	procedure update_coordinates_display (
		self	: not null access type_view'class);



	
	function get_verb (
		self	: not null access type_view)
		return string is abstract;

	
	function get_noun (
		self	: not null access type_view)
		return string is abstract;

	
	procedure update_mode_display (
		self	: not null access type_view'class);


	
	procedure viewport_changed (self : not null access type_view'class);

-- 	function on_layout_changed (
-- 		self : not null access type_model'class;
-- 		call : not null access procedure (self : not null access gobject_record'class);
-- 		slot : access gobject_record'class := null)
-- 		return gtk.handlers.handler_id;

	
	function get_scale (self : not null access type_view) return type_scale;

	
	-- The cairo context to perform the actual drawing.
	-- NOTE: The final drawing is performed in the view (hence in view coordinates):
	type type_draw_context is record
		cr     : cairo.cairo_context := cairo.null_context;
		layout : pango.layout.pango_layout := null; -- CS for displaying text. not used yet
		view   : type_view_ptr := null;
	end record;

-- 	procedure layout_changed (self : not null access type_model'class);

	
	procedure set_transform (
		self	: not null access type_view'class;
		cr		: cairo.cairo_context);

	
	-- Returns the position of the pointer in the drawing:
	function mouse_position (
		self	: not null access type_view'class)
		return pac_geometry_2.type_point;

	
	function vtm (
		view_point	: in type_view_point;
		scale		: in type_scale;
		topleft		: in type_model_point) 
		return type_model_point;

	
	function view_to_model (
		self   : not null access type_view;
		p      : in type_view_point) 
		return type_model_point;
	

	-- Converts the given area of the view to a model rectangle:
	function view_to_model (
		self   : not null access type_view;
		rect   : in type_view_rectangle) -- position and size are in pixels
		return type_bounding_box;
	

	function mtv (
		model_point	: in type_model_point;
		scale		: in type_scale;
		topleft		: in type_model_point) 
		return type_view_point;

	
	-- Converts the given point in the model to a point in the view.
	function model_to_view (
		self   : not null access type_view;
		p      : in type_model_point) -- position x/y given as a float type
		return type_view_point;

	
	-- Converts the given area of the model to a view rectangle:	
-- 	function model_to_view (
-- 		self   : not null access type_view;
-- 		rect   : in type_bounding_box) -- position x/y and size given as a float type
-- 		return type_view_rectangle;



	
	-- Converts a model point to a drawing point. 
	-- NOTE: The model point is in a coordinate system with y-axis
	-- going downwards. The drawing point is in a system where y-axis
	-- goes upwards. The origin of the drawing coordinate system is the
	-- lower left corner of the drawing frame.
	function model_to_drawing (
		self		: not null access type_view;
		model_point : in type_model_point)
		return pac_geometry_2.type_point is abstract;

	
	-- Converts a drawing point to a model point. See comments on function model_to_drawing.
	function drawing_to_model (
		self			: not null access type_view;
		drawing_point	: in pac_geometry_2.type_point)	
		return type_model_point is abstract;

	
	-- Returns the bounding box of all items.	
	function bounding_box (self : not null access type_view)
		return type_bounding_box is abstract;

	
	procedure set_adjustment_values (self : not null access type_view'class);	

	function view_get_type return glib.gtype;
	pragma convention (c, view_get_type);
	--  return the internal type


	procedure init (
		self  : not null access type_view'class);


	-- Changes the scaling factor for self.
	-- this also scrolls the view so that either preserve or the current center
	-- of the view remains at the same location in the widget, as if the user
	-- was zooming towards that specific point.
	procedure set_scale (
		self     : not null access type_view;
		scale    : in type_scale := scale_default;
		preserve : in type_model_point := model_origin);

	
	function get_visible_area (self : not null access type_view'class)
		return type_bounding_box;
	-- Return the area of the model (or the sheet) that is currently displayed in the view.
	-- This is in model coordinates (since the canvas coordinates are always
	-- from (0,0) to (self.get_allocation_width, self.get_allocation_height).

	
	
	-- This procedure should be called every time items are moved, added or removed.
	-- Call this procedure after having created after a view has been created for the model.
-- 	procedure refresh_layout (
-- 		self        : not null access type_model;
-- 		send_signal : boolean := true); -- sends "layout_changed" signal when true


	
	-- The grid density is used to switch the grid size (the spacing between the grid points).
	-- Depending on the grid density, a multiplier will be applied to
	-- the default grid (as defined in the module database).
	type type_grid_density is (
			COARSE,
			NORMAL,
			FINE);

	function to_string (density : in type_grid_density) return string;
	
	grid_density_default : constant type_grid_density := NORMAL;
	grid_density : type_grid_density := NORMAL;

	procedure next_grid_density;

	procedure reset_grid_density;


	
	-- Redraw either the whole view, or a specific part of it only.
	-- The transformation matrix has already been set on the context.
	procedure draw_internal (
		self    : not null access type_view;
		context : type_draw_context;
		area    : type_bounding_box) is null;

	
	procedure scale_to_fit (
		self      : not null access type_view'class;
		rect      : in type_bounding_box := no_area;
		min_scale : in type_scale := 1.0 / 4.0;
		max_scale : in type_scale := 4.0);

	
	-- This function converts a x-value from the drawing to a x-value in the view.
	-- It just converts from type_float to type_view_coordinate. No shifting, no inverting.
	function convert_x (x : in type_float) 
		return type_view_coordinate;

	-- This function converts a y-value from the drawing to a y-value in the view.	
	-- It just converts from type_float to type_view_coordinate. No shifting, no inverting.	
	function convert_y (y : in type_float) 
		return type_view_coordinate renames convert_x;
	

	

	-- This function calculates the grid coordinate on the axis that comes
	-- before the given coordinate.
	-- Example 1: If coordinate is 215.6 and grid size is 10, then x becomes 210.
	-- Example 2: If coordinate is 166.5 and grid size is 5, then x becomes 165.
	function lower_grid_coordinate (
		coordinate	: in pac_geometry_2.type_distance;
		grid		: in pac_geometry_2.type_distance_grid)
		return type_view_coordinate;

	
	-- The grid density threshold above which the grid will no longer be drawn.
	-- The grid density equals 1/(grid_size * scale).
	threshold_grid_density : constant type_view_coordinate := 0.05;

	
	-- Returns the given point x/y rounded to the current grid.
	function snap_to_grid (
		self	: not null access type_view'class;
		point	: in pac_geometry_2.type_point) 
		return pac_geometry_2.type_point;

	
	-- This procedure draws a grid on the given context for the given
	-- area of the drawing. If the grid density in x AND y is below 
	-- threshold_grid_density then the grid will be drawn.
	procedure draw_grid (
		context	: in type_draw_context;
		area	: in type_bounding_box;  -- the area of the drawing to be displayed
		grid	: in pac_geometry_2.type_grid;		
		start_x	: in type_view_coordinate;
		start_y	: in type_view_coordinate;
		color	: in et_colors.type_color);

	
	-- Moves the view so that the given point is on the center.
	-- Uses the current scale and leaves it as it is.
	procedure center_on (
		self		: not null access type_view'class;
		center_on	: pac_geometry_2.type_point); -- in drawing

	
	procedure zoom_in (
		point	: in type_model_point; 	-- model point
		step	: in type_scale);	-- the increment of scale change
	
	procedure zoom_out (
		point	: in type_model_point; 	-- model point
		step	: in type_scale);	-- the increment of scale change

	


	
-- CURSOR
	
	type type_cursor is record
		position	: pac_geometry_2.type_point;
		-- CS blink, color, ...
	end record;

	cursor_main : type_cursor;

	type type_cursor_direction is (RIGHT, LEFT, UP, DOWN);

	
	-- Moves the given cursor relative/absolute by/to the given position.
	procedure move_cursor (
		self		: not null access type_view;
		coordinates	: in type_coordinates;  -- relative/absolute
		cursor		: in out type_cursor;
		position	: in pac_geometry_2.type_point) is null;

	
	-- Moves the given cursor in the given direction by the current grid.
	procedure move_cursor (
		self		: not null access type_view;
		direction	: in type_cursor_direction; -- right, left, up, down
		cursor		: in out type_cursor) is null;

	
	-- Shifts the current area of the view so that the given cursor comes into view.
	procedure shift_area (
		self		: not null access type_view'class;
		cursor		: in type_cursor);

	
	procedure draw_cursor (
		self		: not null access type_view;
		in_area		: in type_bounding_box := no_area;
		context 	: in type_draw_context;
		cursor		: in type_cursor) is null;

	
	-- Returns the grid of the current active module
	-- scaled by a multiplier.
	-- The multiplier depends on the current grid_density (COARSE,NORMAL,FINE):
	function get_grid (
		self : not null access type_view)
		return pac_geometry_2.type_grid is abstract;

	
	-- Returns the frame:
	function get_frame (
		self : not null access type_view)
		return et_frames.type_frame is abstract;

	
	-- Returns the height of the drawing frame:
	function frame_height (
		self : not null access type_view)
		return type_float_positive is abstract;

	
	-- Returns the width of the drawing frame:
	function frame_width (
		self : not null access type_view)
		return type_float_positive is abstract;

	
	-- Returns the bounding box of the drawing frame:
	function frame_bounding_box (
		self : not null access type_view'class)
		return type_bounding_box;

	-- Returns the bounding box of the paper:
	function paper_bounding_box (
		self : not null access type_view'class)
		return type_bounding_box;

	-- Returns the position of the title block:
	function title_block_position (
		self : not null access type_view)
		return et_frames.type_position is abstract;

	-- Evaluates the given keyboard key and sets the operation mode:
	procedure evaluate_key (
		self	: not null access type_view;
		key		: in gdk_key_type) is null;

	type type_mouse_button is new positive range 1 .. 7; -- CS range correct ?
	-- 1 - left button
	-- 2 - middle
	-- 3 - right button
	-- CS others ?

	-- Returns something like "right button" or "left button":
	function to_string (b : in type_mouse_button) return string;

	procedure evaluate_mouse_position (
		self	: not null access type_view;
		point	: in pac_geometry_2.type_point) is null;
	
	procedure button_pressed (
		self	: not null access type_view;
		button	: in type_mouse_button;
		point	: in pac_geometry_2.type_point) is null;


	
	-- Whenever the operator is required to clarify which object is meant,
	-- we use this type:
	type type_request_clarification is (NO, YES);

	-- A global status variable that indicates whether clarification is required or not:
	request_clarificaton : type_request_clarification := NO;

	procedure set_request_clarification;
	procedure reset_request_clarification;

	-- Returns true if the global flag "request_clarificaton" is YES:
	function clarification_pending return boolean;


	-- In order to count a number of clicks or keys pressed:
	subtype type_activate_counter is natural range 0 .. 2;
	activate_counter : type_activate_counter := type_activate_counter'first;
	
	procedure reset_activate_counter;
	procedure increment_activate_counter;



	-- The status of the single command entered on the console:
	single_cmd_status : type_single_cmd_status;

	procedure reset_single_cmd_status;


	-- The primary tool used for drawing and navigating within the canvas:
	primary_tool : type_tool := primary_tool_default;

	procedure change_primary_tool;


	-- Returns the position where the primary tool
	-- is pointing at. 
	-- If the primary tool is MOUSE, then the return value 
	-- is snapped to the nearest grid point.
	-- If the primary tool is KEYBOARD, then the return 
	-- value is the position of cursor_main. The cursor
	-- is always on a grid position (no snap required).
	function tool_position (
		view : not null access type_view'class)
		return pac_geometry_2.type_point;

	
	

	procedure evaluate_exception (
		name	: in string; -- exception name
		message : in string); -- exception message




	type type_properties_window is record
		window	: gtk.window.gtk_window;

		-- This flag indicates that the properties
		-- window is open. The purpose of this flag is
		-- to prevent the window from opening multiple
		-- times:
		open	: boolean := false;
	end record;
	
	window_properties : type_properties_window;
	
	-- Returns the status of the "open" flag.
	-- True if the properties window is open.
	-- False if the window is not open.
	function window_properties_is_open return boolean;

	-- This procedure must be called when the operator closes
	-- the properties window. This is a null procedure, means
	-- in schematic, layout and other canvas are different things to do
	-- in order to reset variables related to any properties.
	procedure reset_properties_selection (
		self : not null access type_view) is null;
	
	-- Here we display the property in its old state before changing it:
	label_property_old	: gtk.label.gtk_label;
	entry_property_old	: gtk_gentry;

	label_property_new	: gtk.label.gtk_label;
	
	label_properties_status	: gtk.label.gtk_label;
	
	properties_confirmed : boolean := false;

	
	
	procedure build_window_properties;

	procedure set_status_properties (text : in string);
	procedure set_property_before (text : in string);


	-- Saves the drawing in its current state.
	-- Since this procedure is generic, the overridden
	-- procedure of save_drawing dispatches to the 
	-- actual subprogram to save the module, symbol,
	-- package or device:
	procedure save_drawing (
		self : not null access type_view) is null;




	
--PRIMITIVE DRAW OPERATIONS------------------

	function make_bounding_box (
		height		: in type_float;
		boundaries	: in type_boundaries)
		return type_bounding_box;

	
	-- This procedure draws the given line on the given context.
	-- The line is shifted in y to a plane of given height. This plane
	-- has y-axis going downwards.
	-- The line will be drawn if its bounding box intersects the given area.
	-- If area is no_rectangle then the line would be drawn in any case.
	procedure draw_line (
		area	: in type_bounding_box;	
		context	: in type_draw_context;
		line	: in pac_geometry_2.pac_geometry_1.type_line_fine;

		-- The line width is used for calculating the boundaries.
		-- The width for the actual drawing must be set by the caller.
		width	: in pac_geometry_2.type_distance_positive;
		height	: in type_float_positive);
		

	-- This procedure draws the given arc on the given context.
	-- The arc is shifted in y to a plane of given height. This plane
	-- has y-axis going downwards.
	-- The arc will be drawn if its bounding box intersects the given area.
	-- If area is no_rectangle then the arc would be drawn in any case.
	procedure draw_arc (
		area	: in type_bounding_box;
		context	: in type_draw_context;
		arc		: in pac_geometry_2.pac_geometry_1.type_arc;

		-- The line width is used for calculating the boundaries.
		-- The width for the actual drawing must be set by the caller.
		width	: in pac_geometry_2.type_distance_positive;
		height	: in type_float_positive);

	
	-- This procedure draws the given circle on the given context.
	-- The circle is shifted in y to a plane of given height. This plane
	-- has y-axis going downwards.
	-- The circle will be drawn if its bounding box intersects the given area.
	-- If area is no_rectangle then the circle would be drawn in any case.
	-- If the circle is filled, the line width will be set to zero
	-- and left with this setting.
	procedure draw_circle (
		area	: in type_bounding_box;
		context	: in type_draw_context;
		circle	: in pac_geometry_2.type_circle'class;
		filled	: in type_filled;

		-- The line width is used for calculating the boundaries.
		-- The width for the actual drawing must be set by the caller.
		width	: in pac_geometry_2.type_distance_positive;
		height	: in type_float_positive);
		-- CS fill style ?


	
	procedure draw_contour (
		area	: in type_bounding_box;
		context	: in type_draw_context;
		contour	: in type_contour'class;
		filled	: in type_filled;
		-- CS fill style

		-- The line width is used for calculating the boundaries
		-- of the segments. 
		-- The width for the actual drawing must be set by the caller.
		width	: in pac_geometry_2.type_distance_positive;
		
		height	: in type_float_positive;

		-- This flag is set if the contour has been drawn
		-- because it is inside the given area:
		drawn	: in out boolean);


	procedure draw_contour_with_circular_cutout (
		area			: in type_bounding_box;
		context			: in type_draw_context;
		outer_border	: in type_contour'class;
		inner_border	: in pac_geometry_2.type_circle'class;
		height			: in type_float_positive);

	
	procedure draw_contour_with_arbitrary_cutout (
		area			: in type_bounding_box;
		context			: in type_draw_context;
		outer_border	: in type_contour'class;
		inner_border	: in type_contour'class;
		height			: in type_float_positive);
	
	
	-- This procedure draws the a rectangle on the given context.
	-- The rectangle is shifted in y to a plane of given height. This plane
	-- has y-axis going downwards.
	-- The rectangle will be drawn if its bounding box intersects the given area.
	-- If area is no_rectangle then the rectangle would be drawn in any case.
	procedure draw_rectangle (
		area			: in type_bounding_box;
		context			: in type_draw_context;
		position		: in pac_geometry_2.type_point;	-- position of the rectangle (lower left corner)
		width			: in type_float_positive; -- widht of the rectangle
		height			: in type_float_positive; -- height of the rectangle
		frame_height	: in type_float_positive;
		extend_boundaries	: in boolean := false;
		boundaries_to_add	: in type_boundaries := pac_geometry_2.boundaries_default);
		-- CS fill style ?



	
-- TEXT
	use et_text;
	use pac_text;
	
	conversion_factor_mm_to_pt : constant gdouble := 1.53; -- CS use exact factor
	
	function to_points (size : in pac_text.type_text_size)
		return gdouble;

	
	-- Draws a text right in the view.
	-- Does not care about area and bounding box. It is assumed that the calling
	-- unit has decided whether the text is to be drawn or not. No area check here.
	procedure draw_text (
		context		: in type_draw_context;
		content		: in pac_text_content.bounded_string;
		size		: in pac_text.type_text_size;
		font		: in et_text.type_font;
		x,y			: in gdouble; -- the anchor point in the view
		origin		: in boolean; -- when true, an origin is drawn at the anchor point
		rotation	: in pac_geometry_2.type_rotation;
		alignment	: in type_text_alignment);

	
	-- Computes for the given text content, size and font the extents.
	function get_text_extents (
		context		: in type_draw_context;
		content		: in pac_text_content.bounded_string;
		size		: in pac_text.type_text_size;
		font		: in et_text.type_font)
		return cairo_text_extents;

	
	-- Draws a text in the drawing plane.
	-- Draws the text in case it is inside the given area or if the
	-- text intersects the given area.
	-- If area is no_rectangle then the text would be drawn in any case.
	procedure draw_text (
		area		: in type_bounding_box; -- in model plane
		context		: in type_draw_context;
		content		: in pac_text_content.bounded_string;
		size		: in pac_text.type_text_size;
		font		: in et_text.type_font;
		position	: in pac_geometry_2.type_point; -- the anchor point in the drawing, the origin
		origin		: in boolean; -- when true, an origin is drawn at the anchor point
		rotation	: in pac_geometry_2.type_rotation;
		alignment	: in type_text_alignment;
		height		: in type_float_positive); -- the height of the drawing frame

	
	-- Draw a vectorized text:
	procedure draw_vector_text (
		area	: in type_bounding_box;
		context	: in type_draw_context;
		text	: in type_vector_text;

		-- The line width is used for calculating the boundaries
		-- of the line segments:
		width	: in pac_geometry_2.type_distance_positive;
		height	: in type_float_positive);



-------------
	-- frame

	-- Draws the lines of the title block:
	procedure draw_title_block_lines (
		area		: in type_bounding_box;
		context		: in type_draw_context;	
		lines		: in et_frames.pac_lines.list;
		tb_pos		: in et_frames.type_position;
		frame_size	: in et_frames.type_frame_size);

	
	-- Draws the outer an inder border of the frame:
	procedure draw_border ( -- CS rename to draw_frame_border
		area			: in type_bounding_box;
		context			: in type_draw_context;	
		frame_size		: in et_frames.type_frame_size;
		border_width	: in et_frames.type_border_width;
		height			: in et_frames.type_distance); -- CS no need. already in frame_size


	-- The sector delimiters are short lines between outer an 
	-- inner border of the frame.
	-- Between the delimiters are the row and column indexes.
	procedure draw_sector_delimiters (
		area			: in type_bounding_box;
		context			: in type_draw_context;	
		sectors			: in et_frames.type_sectors;
		frame_size		: in et_frames.type_frame_size;
		border_width	: in et_frames.type_border_width);

	
		-- Draws a single text of the title block.
	-- The line position is relative to the lower left corner of the title block.	
	procedure draw_text ( -- CS rename to draw_text_title_block
		area	: in type_bounding_box;
		context	: in type_draw_context;
		content	: in pac_text_content.bounded_string;
		size	: in et_frames.type_text_size;
		font	: in type_font;
		pos		: in et_frames.type_position;
		tb_pos	: in et_frames.type_position;
		height	: in et_frames.type_distance);

	

	-- Draw common placeholders. They apply to both schematic and layout.
	-- Common placeholders are project name, module file name and assembly variant.
	-- Draws other texts such as "approved" or "edited". Such texts have no placeholders:
	procedure draw_texts ( -- CS rename to draw_title_block_texts
		area		: in type_bounding_box;
		context		: in type_draw_context;
		ph_common	: in et_frames.type_placeholders_common;
		ph_basic	: in et_frames.type_placeholders_basic;
		texts		: in et_frames.pac_texts.list;
		meta		: in et_meta.type_basic;
		tb_pos		: in et_frames.type_position;
		height		: in et_frames.type_distance);
	
	
private

	-- Called when the operator closes the properties window
	-- (for example with Alt-F4):
	procedure close_window_properties (
		self : access gtk_widget_record'class);
	
	access_on_window_properties_closed : constant 
		cb_gtk_widget_void := close_window_properties'access;
	-------
	
	-- Called when a key is pressed in the properties window.
	-- Closes the window if the operator presses ESC.
	function window_properties_key_event (
		self	: access gtk_widget_record'class;
		event	: gdk.event.gdk_event_key) 
		return boolean;

	access_on_window_properties_key_event : constant
		cb_gtk_widget_gdk_event_key_boolean := window_properties_key_event'access;
	-------

	-- Called when one of the scrollbars has changed value.
	procedure on_adj_value_changed (
		view : access glib.object.gobject_record'class);

	access_on_adj_value_changed : constant
		gtk.adjustment.cb_gobject_void := on_adj_value_changed'access;
	-------
	
	procedure view_set_property (
		object        : access glib.object.gobject_record'class;
		prop_id       : property_id;
		value         : glib.values.gvalue;
		property_spec : param_spec);

	access_view_set_property : constant
		set_property_handler := view_set_property'access;
	-------
	
	procedure view_get_property (
		object        : access glib.object.gobject_record'class;
		prop_id       : property_id;
		value         : out glib.values.gvalue;
		property_spec : param_spec);

	access_view_get_property : constant
		get_property_handler := view_get_property'access;
	-------
	
	function on_view_draw (
		view	: system.address; 
		cr		: cairo_context) return gboolean;
	
	pragma convention (c, on_view_draw);
	--  default handler for "draw" on views.

	access_on_view_draw : constant draw_handler := on_view_draw'access;
	-------

	procedure on_size_allocate (
		view	: system.address;
		alloc	: gtk_allocation);

	pragma convention (c, on_size_allocate);
	--  default handler for "size_allocate" on views.
	
	access_on_size_allocate : constant
		size_allocate_handler := on_size_allocate'access;
	-------

	procedure on_view_realize (widget : system.address);

	pragma convention (c, on_view_realize);
	--  called when the view is realized

	access_on_view_realize : constant
		realize_handler := on_view_realize'access;
	-------

	procedure view_class_init (self : gobject_class);

	pragma convention (c, view_class_init);

	access_view_class_init : constant
		ada_class_init := view_class_init'access;
	-------

	-- Updates the mouse position display.
	-- Updates the distances display.	
	function on_mouse_movement (
		view  : access gtk_widget_record'class;
		event : gdk_event_motion) return boolean;

	access_on_mouse_movement : constant
		cb_gtk_widget_gdk_event_motion_boolean := on_mouse_movement'access;
	-------

	function on_scroll_event (
		view	: access gtk_widget_record'class;
		event	: gdk_event_scroll) return boolean;
	
	access_on_scroll_event : constant
		cb_gtk_widget_gdk_event_scroll_boolean := on_scroll_event'access;
	-------

	function on_button_event (
		view  : access gtk_widget_record'class;
		event : gdk_event_button)
		return boolean;

	access_on_button_event : constant
		cb_gtk_widget_gdk_event_button_boolean := on_button_event'access;
	------

	function on_key_pressed_event (
		view  : access gtk_widget_record'class;
		event : gdk_event_key) return boolean;

	access_on_key_pressed_event : constant
		cb_gtk_widget_gdk_event_key_boolean := on_key_pressed_event'access;
	------
	
end pac_canvas;
	
end et_canvas_general;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
