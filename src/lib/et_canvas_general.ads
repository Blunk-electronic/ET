------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            CANVAS GENERAL                                --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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

-- with ada.text_io;

with gtk.widget;  			use gtk.widget;
with gtk.window; 			use gtk.window;
with gtk.box;				use gtk.box;
with gtk.frame;				use gtk.frame;
with gtk.scrolled_window;	use gtk.scrolled_window;
with gtk.handlers;			use gtk.handlers;
with gtk.enums;				use gtk.enums;
with gtk.adjustment;		use gtk.adjustment;
with gtk.combo_box_text;	use gtk.combo_box_text;
with gtk.gentry;			use gtk.gentry;
with gtk.label;				use gtk.label;

with gdk;
with gdk.types;
with gdk.event;				use gdk.event;

with glib;					use glib;
with glib.object;			use glib.object;
with glib.values;			use glib.values;
with glib.properties.creation;	use glib.properties.creation;

with gtkada.style;
with gdk.rgba;

with cairo;						use cairo;

with pango.layout;				use pango.layout;
with system.storage_elements;	use system.storage_elements;

with et_general;
with et_geometry;				use et_geometry;
with et_frames;
with et_string_processing;		use et_string_processing;
with et_colors;

package et_canvas_general is

generic

	canvas_name : string; -- schematic, board, package, device, symbol, ...

	-- The system of measurement:
	with package geometry is new et_geometry.geometry_operations_2d (<>);
	
package pac_canvas is
	use geometry;

	window : gtk_window; -- This is he main window.	
	
	box_back				: gtk_box; -- This is an access/pointer to the actual box.
	box_left, box_right		: gtk_box;

	-- Builds the background boxes box_back, box_left, and box_right:
	procedure build_background_boxes;

	
	-- main position display:
	box_positions			: gtk_box;		-- the main box around all kinds of position readouts
	
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
	
	-- Builds the boxes and combo boxes that display mouse and cursor position.
	-- Places them in box_left.
	procedure build_position_display;

	procedure update_position_display_cursor;
	
	box_console	: gtk_box;
	console		: gtk_combo_box_text;

	-- Builds the console and places it in box_right.
	procedure build_console;


	
	box_drawing	: gtk_box;
	frame		: gtk_frame;
	scrolled	: gtk_scrolled_window;
	
	-- Builds the drawing area and places it in box_right.
	procedure build_canvas;



	
	-- This variable serves for logging debug messages an other stuff.
	-- It is assigned with the log level on initializing a main window.
	log_threshold : type_log_level := type_log_level'first;


	-- This signal is emitted by the drawing whenever items are added, moved, resized, ...
-- 	signal_layout_changed : constant glib.signal_name := "layout_changed";
	

	signal_viewport_changed : constant glib.signal_name := "viewport_changed";
	-- This signal is emitted whenever the view is zoomed or scrolled.


	-- The view coordinates are the 
	-- coordinates of items on the screen and are expressed in pixels.
	-- They change when the operators zooms or scrolls.
	subtype type_view_coordinate is gdouble; -- gdouble is a real floating-point type (see glib.ads)
	subtype type_view_coordinate_positive is type_view_coordinate range 0.0 .. type_view_coordinate'last;

	-- The point inside the view.
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


	
-- VIEW

	-- scale
	scale_min : constant gdouble := 0.1;
	scale_max : constant gdouble := 1000.0;
	subtype type_scale is gdouble range scale_min .. scale_max;
	scale_default : constant type_scale := 1.0;
	scale_delta_on_zoom : constant type_scale := 0.2;
	
	type type_view is abstract new gtk.widget.gtk_widget_record with record

		-- The upper left corner of the visible area has its initial value at 0/0.
		-- NOTE: This has nothing to do with the upper left corner of the
		-- drawing sheet. topleft is not a constant and is changed on by procedure
		-- set_scale or by procedure scale_to_fit.
		topleft  	: type_point := origin;

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
		scale_to_fit_area		: type_rectangle;

		grid : geometry.type_grid;
	end record;

	-- The pointer to the canvas/view:
	type type_view_ptr is access all type_view'class;

	canvas	: type_view_ptr;


	-- Returns the distance on the given axis rounded to the current grid.
	function to_string (
		self	: not null access type_view;
		point	: in type_point;
		axis	: in et_general.type_axis_2d)
		return string;
	
	-- Returns the given point x/y rounded to the current grid.
	function to_string (
		self	: not null access type_view;
		point	: in type_point) 
		return string;
	
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



	function vtm (
		view_point	: in type_view_point;
		scale		: in type_scale;
		topleft		: in type_point) 
		return type_point;

	function view_to_model (
		self   : not null access type_view;
		p      : in type_view_point) 
		return type_point;

	-- Converts the given area of the view to a model rectangle:
	function view_to_model (
		self   : not null access type_view;
		rect   : in type_view_rectangle) -- position and size are in pixels
		return type_rectangle;

	function mtv (
		drawing_point	: in type_point;
		scale			: in type_scale;
		topleft			: in type_point) 
		return type_view_point;
	
	-- Converts the given point in the model to a point in the view.
	function model_to_view (
		self   : not null access type_view;
		p      : in type_point) -- position x/y given as a float type
		return type_view_point;

	-- Converts the given area of the model to a view rectangle:	
-- 	function model_to_view (
-- 		self   : not null access type_view;
-- 		rect   : in type_rectangle) -- position x/y and size given as a float type
-- 		return type_view_rectangle;



	
	-- Converts a model point to a drawing point. 
	-- NOTE: The model point is in a coordinate system with y-axis
	-- going downwards. The drawing point is in a system where y-axis
	-- goes upwards. The origin of the drawing coordinate system is the
	-- lower left corner of the drawing frame.
	function model_to_drawing (
		self		: not null access type_view;
		model_point : in type_point)
		return type_point is abstract;

	-- Converts a drawing point to a model point. See comments on function model_to_drawing.
	function drawing_to_model (
		self			: not null access type_view;
		drawing_point : in type_point)	
		return type_point is abstract;

	
	-- Returns the bounding box of all items.	
	function bounding_box (self : not null access type_view)
		return type_rectangle is abstract;
	
	procedure set_adjustment_values (self : not null access type_view'class);	

	function view_get_type return glib.gtype;
	pragma convention (c, view_get_type);
	--  return the internal type


	procedure init (
		self  : not null access type_view'class);
	
	procedure set_scale (
		self     : not null access type_view;
		scale    : in type_scale := scale_default;
		preserve : in type_point := origin);
	-- Changes the scaling factor for self.
	-- this also scrolls the view so that either preserve or the current center
	-- of the view remains at the same location in the widget, as if the user
	-- was zooming towards that specific point.

	function get_visible_area (self : not null access type_view'class)
		return type_rectangle;
	-- Return the area of the model (or the sheet) that is currently displayed in the view.
	-- This is in model coordinates (since the canvas coordinates are always
	-- from (0,0) to (self.get_allocation_width, self.get_allocation_height).

	
	
	-- This procedure should be called every time items are moved, added or removed.
	-- Call this procedure after having created after a view has been created for the model.
-- 	procedure refresh_layout (
-- 		self        : not null access type_model;
-- 		send_signal : boolean := true); -- sends "layout_changed" signal when true


	grid_default : constant type_distance_positive := 10.0;
	
	-- Redraw either the whole view, or a specific part of it only.
	-- The transformation matrix has already been set on the context.
	procedure draw_internal (
		self    : not null access type_view;
		context : type_draw_context;
		area    : type_rectangle) is null;

	procedure scale_to_fit (
		self      : not null access type_view'class;
		rect      : in type_rectangle := no_rectangle;
		min_scale : in type_scale := 1.0 / 4.0;
		max_scale : in type_scale := 4.0);

	-- This function converts a x-value from the drawing to a x-value in the view.
	-- It just converts from type_distance to type_view_coordinate. No shifting, no inverting.
	function convert_x (x : in type_distance) return type_view_coordinate;

	-- This function converts a y-value from the drawing to a y-value in the view.	
	-- It just converts from type_distance to type_view_coordinate. No shifting, no inverting.	
	function convert_y (y : in type_distance) return type_view_coordinate renames convert_x;
	

	

	-- This function converts a y-value from the drawing to the view.
	-- The input y increases upwards. The output y increases downwards.
	function convert_and_shift_y ( -- CS remove
		self	: not null access type_view;
		y		: in type_distance)
		return type_view_coordinate is abstract;

	-- This function converts a y-value from the view to the drawing.
	-- The input y increases upwards. The output y increases downwards.
	function convert_and_shift_y ( -- CS remove
		self	: not null access type_view;
		y		: in type_distance) 
		return type_distance is abstract;


	-- This function calculates the grid coordinate on the axis that comes
	-- before the given coordinate.
	-- Example 1: If coordinate is 215.6 and grid size is 10, then x becomes 210.
	-- Example 2: If coordinate is 166.5 and grid size is 5, then x becomes 165.
	function lower_grid_coordinate (
		coordinate	: in type_distance;
		grid		: in type_distance_grid)
		return type_view_coordinate;

	-- This procedure draws a grid on the given context for the given
	-- area of the drawing.

	threshold_grid_density : constant type_view_coordinate := 0.06;
	
	procedure draw_grid (
		context	: in type_draw_context;
		area	: in type_rectangle;  -- the area of the drawing to be displayed
		grid	: in geometry.type_grid;		
		start_x	: in type_view_coordinate;
		start_y	: in type_view_coordinate;
		color	: in et_colors.type_color);

	-- Moves the view so that the given point is on the center.
	-- Uses the current scale and leaves it as it is.
	procedure center_on (
		self		: not null access type_view'class;
		center_on	: type_point); -- in drawing

	procedure zoom_in (
		point	: in type_point; 	-- model point
		step	: in type_scale);	-- the increment of scale change
	
	procedure zoom_out (
		point	: in type_point; 	-- model point
		step	: in type_scale);	-- the increment of scale change

	


	-- CURSOR
	type type_cursor is record
		position	: type_point;
		-- CS blink, color, ...
	end record;

	cursor_main : type_cursor;

	type type_cursor_direction is (RIGHT, LEFT, UP, DOWN);

	-- Moves the given cursor relative/absolute by/to the given position.
	procedure move_cursor (
		self		: not null access type_view;
		coordinates	: in type_coordinates;  -- relative/absolute
		cursor		: in out type_cursor;
		position	: in type_point);

	-- Moves the given cursor in the given direction by the current grid.
	procedure move_cursor (
		self		: not null access type_view;
		direction	: in type_cursor_direction; -- right, left, up, down
		cursor		: in out type_cursor);

	
	-- Shifts the current area of the view so that the given cursor comes into view.
	procedure shift_area (
		self		: not null access type_view'class;
		cursor		: in type_cursor);
	
	procedure draw_cursor (
		self		: not null access type_view;
		in_area		: in type_rectangle := no_rectangle;
		context 	: in type_draw_context;
		cursor		: in type_cursor) is null;

	-- Returns the frame:
	function get_frame (
		self : not null access type_view)
		return et_frames.type_frame is abstract;
	
	-- Returns the height of the drawing frame:
	function frame_height (
		self : not null access type_view)
		return type_distance_positive is abstract;

	-- Returns the width of the drawing frame:
	function frame_width (
		self : not null access type_view)
		return type_distance_positive is abstract;
	
	-- Returns the bounding box of the drawing frame:
	function frame_bounding_box (
		self : not null access type_view'class)
		return type_rectangle;

	-- Returns the bounding box of the paper:
	function paper_bounding_box (
		self : not null access type_view'class)
		return type_rectangle;

	-- Returns the position of the title block:
	function title_block_position (
		self : not null access type_view)
		return et_frames.type_position is abstract;
									  
private
	procedure on_adj_value_changed (view : access glib.object.gobject_record'class);
	-- Called when one of the scrollbars has changed value.

	access_on_adj_value_changed : constant gtk.adjustment.cb_gobject_void := on_adj_value_changed'access;
	-------
	
	procedure view_set_property (
		object        : access glib.object.gobject_record'class;
		prop_id       : property_id;
		value         : glib.values.gvalue;
		property_spec : param_spec);

	access_view_set_property : constant set_property_handler := view_set_property'access;
	-------
	
	procedure view_get_property (
		object        : access glib.object.gobject_record'class;
		prop_id       : property_id;
		value         : out glib.values.gvalue;
		property_spec : param_spec);

	access_view_get_property : constant get_property_handler := view_get_property'access;
	-------
	
	function on_view_draw (
		view	: system.address; 
		cr		: cairo_context) return gboolean;
	
	pragma convention (c, on_view_draw);
	--  default handler for "draw" on views.

	access_on_view_draw : constant draw_handler := on_view_draw'access;
	-------

	procedure on_size_allocate (view : system.address; alloc : gtk_allocation);

	pragma convention (c, on_size_allocate);
	--  default handler for "size_allocate" on views.
	
	access_on_size_allocate : constant size_allocate_handler := on_size_allocate'access;
	-------

	procedure on_view_realize (widget : system.address);

	pragma convention (c, on_view_realize);
	--  called when the view is realized

	access_on_view_realize : constant realize_handler := on_view_realize'access;
	-------

	procedure view_class_init (self : gobject_class);

	pragma convention (c, view_class_init);

	access_view_class_init : constant ada_class_init := view_class_init'access;
	-------

	function on_mouse_movement (
		view  : access gtk_widget_record'class;
		event : gdk_event_motion) return boolean;

	access_on_mouse_movement : constant cb_gtk_widget_gdk_event_motion_boolean := on_mouse_movement'access;
	-------

	function on_scroll_event (
		view	: access gtk_widget_record'class;
		event	: gdk_event_scroll) return boolean;
	
	access_on_scroll_event : constant cb_gtk_widget_gdk_event_scroll_boolean := on_scroll_event'access;
	-------

	function on_button_event (
		view  : access gtk_widget_record'class;
		event : gdk_event_button)
		return boolean;

	access_on_button_event : constant cb_gtk_widget_gdk_event_button_boolean := on_button_event'access;
	------

	function on_key_pressed_event (
		view  : access gtk_widget_record'class;
		event : gdk_event_key) return boolean;

	access_on_key_pressed_event : constant cb_gtk_widget_gdk_event_key_boolean := on_key_pressed_event'access;
	------
	
end pac_canvas;
	
end et_canvas_general;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
