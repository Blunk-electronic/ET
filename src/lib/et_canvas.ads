------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                CANVAS                                    --
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

with ada.text_io;				use ada.text_io;

with glib;

with gdk.event;					use gdk.event;
with gdk.types;					use gdk.types;
with gdk.types.keysyms;			use gdk.types.keysyms;

with gtk.widget;				use gtk.widget;
with gtk.container;				use gtk.container;
with gtk.window;				use gtk.window;
with gtk.separator;				use gtk.separator;
with gtk.box;					use gtk.box;
with gtk.gentry;				use gtk.gentry;
with gtk.combo_box;				use gtk.combo_box;
with gtk.combo_box_text;		use gtk.combo_box_text;
with gtk.drawing_area;			use gtk.drawing_area;

with gtk.scrolled_window;		use gtk.scrolled_window;
with gtk.adjustment;			use gtk.adjustment;
with gtk.scrollbar;				use gtk.scrollbar;

with gtk.table;					use gtk.table;
with gtk.label;					use gtk.label;
with gtk.button;				use gtk.button;
with gtk.text_view;				use gtk.text_view;
with gtk.text_buffer;			use gtk.text_buffer;

with cairo;

with et_primitive_objects;		use et_primitive_objects;
with et_logical_pixels;			use et_logical_pixels;
with et_geometry_2a;
with et_geometry_2a.grid;
with et_geometry_2a.path;
with et_window_dimensions;		use et_window_dimensions;

with et_canvas_messages;		use et_canvas_messages;
with et_cmd_sts;				use et_cmd_sts;
with et_canvas_tool;			use et_canvas_tool;
with et_logging;				use et_logging;

with et_geometry_1.et_polygons;
with et_geometry_1.et_polygons.offsetting;
with et_geometry_2a.contours;
with et_text;
with et_mirroring;				use et_mirroring;


generic
	canvas_name : string; -- schematic, board, package, device, symbol, ...
	
	with package pac_geometry is new et_geometry_2a (<>);
	with package pac_grid is new pac_geometry.grid;
	with package pac_path is new pac_geometry.path;

	with package pac_polygons is new pac_geometry.pac_geometry_1.et_polygons;
	with package pac_offsetting is new pac_polygons.offsetting;
	with package pac_contours is new pac_geometry.contours;
	
	with package pac_text is new et_text.generic_pac_text (
		-- The used text package must have been instantiated with
		-- these packages:
		pac_geometry	=> pac_geometry,
		pac_polygons	=> pac_polygons,
		pac_offsetting	=> pac_offsetting,
		others			=> <>);


	
package et_canvas is
	use pac_geometry;
	use pac_grid;
	

	-- This variable serves for logging debug messages an other stuff.
	-- It is assigned with the log level on initializing a main window.
	log_threshold : type_log_level := type_log_level'first;


	

-- TRANSLATE-OFFSET:
	
	-- The global translate-offset by which all draw operations on the canvas
	-- are translated when the operator zooms on the pointer or the cursor:
	T : type_logical_pixels_vector := (0.0, 0.0);

	
	
-- ZOOM:

	-- This is the specification of the zoom factor
	-- (German: Vergroesserungsfaktor)
	type type_zoom_factor is digits 3 range 0.10 .. 500.0;
	

	-- This is the global zoom factor:
	S : type_zoom_factor := 1.0;

	-- This is the multiplier that is used when the
	-- global zoom factor is increased or decreased:
	SM : constant type_zoom_factor := 1.2;
	

	-- Converts the given zoom factor to a string.
	-- CS: Since type_zoom_factor is a float type, the output is
	-- something like 1.44E+00. Instead the output should be something
	-- simpler like 1.44:
	function to_string (
		zf : in type_zoom_factor)
		return string;


	-- Converts a string like "5.6" to a zoom factor:
	function to_zoom_factor (
		zf : in string)
		return type_zoom_factor;

	
		
	-- This procedure sets the zoom factor:
	procedure set_zoom_factor (
		s_in : in type_zoom_factor);

		
	-- This procedure increases the global zoom factor
	-- by multiplying it by SM:
	procedure increase_zoom_factor;

	
	-- This procedure decreases the global zoom factor
	-- by dividing it by SM:
	procedure decrease_zoom_factor;

	

	-- There are two kinds of zoom-operations:
	type type_zoom_direction is (ZOOM_IN, ZOOM_OUT);


	
	
-- CONVERSIONS:

	-- DEVICE PIXELS (CS3) <-> LOGICAL PIXELS (CS2):

	-- Converts device pixels to logical pixels.
	-- NOTE: Device pixels are integers. They can also be negative
	-- in connection with size changes of widgets. So this conversion
	-- allows both negative and positive numbers.
	-- The function is basically just a direct type conversion.
	function to_lp (
		dp : in glib.gint)
		return type_logical_pixels;
	
	
	-- REAL (CS1) <-> VIRTUAL MODEL COORDINATES (CS2):
	
	-- Converts a virtual model point to a real model point,
	-- both in CS1:
	function to_real (
		point : in type_vector_model)
		return type_vector_model;

	
	-- Converts a real model point to a virtual model point,
	-- both in CS1:
	function to_virtual (
		point : in type_vector_model)
		return type_vector_model;

	

	-- CANVAS (CS2 <-> MODEL (CS1):

	-- Converts the given model distance to
	-- a canvas distance according to the current zoom factor S:
	function to_distance (
		d : in type_distance_positive)
		return type_logical_pixels_positive;

	
	-- Converts the given canvas distance to
	-- a model distance according to the current zoom factor S:
	function to_distance (
		d : in type_logical_pixels_positive)
		return type_distance_positive;



	
	-- Converts a given virtual model point (CS1)
	-- to a point on the canvas (CS2) according to 
	-- the given zoom factor.
	-- Optionally, if argument "translate" is true, then current
	-- tranlate-offset will be taken into account.
	function virtual_to_canvas (
		V	 		: in type_vector_model;
		zf			: in type_zoom_factor;
		translate	: in boolean)
		return type_logical_pixels_vector;
	
	-- Converts a given canvas point (CS2) back
	-- to a virtual model point (CS1) according 
	-- to the given zoom factor.
	-- The current tranlate-offset is ALWAYS taken into account.
	function canvas_to_virtual (
		P			: in type_logical_pixels_vector;
		zf			: in type_zoom_factor)
		return type_vector_model;


	

	-- Converts a real model point (CS1) to a canvas point (CS2)
	-- according to the given zoom factor, the current
	-- base-offset, the current translate-offset and
	-- then the position of the current bounding-box:
	function real_to_canvas (
		M 	: in type_vector_model;
		zf	: in type_zoom_factor)
		return type_logical_pixels_vector;
	
	-- Converts a canvas point (CS2) back to a real model point (CS1)
	-- according to the given zoom factor, the current
	-- base-offset, the current translate-offset and
	-- then the position of the current bounding-box:
	function canvas_to_real (
		P	: in type_logical_pixels_vector;
		zf	: in type_zoom_factor)
		return type_vector_model;
		


	-- In connection with zoom-operations we need the corners of the
	-- bounding-box in canvas coordinates. This composite type serves this
	-- purpose:
	type type_bounding_box_corners is record
		BL, BR, TL, TR : type_logical_pixels_vector;
	end record;

	
	-- This function returns the current corners of the bounding-box
	-- in canvas-coordinates. The return depends on the current zoom factor S
	-- and translate-offset:
	function get_bounding_box_corners
		return type_bounding_box_corners;
		






	
	
-- BASE-OFFSET:

	-- The place on the canvas where the model 
	-- coordinates system has its origin.
	-- It is a global variable. We call it "base-offset":
	F : type_logical_pixels_vector;

	-- Sets the global base-offset F according to the current
	-- bounding-box and the maximal allowed zoom factor:
	procedure set_base_offset;

	
	


-- ZOOM:
	
	-- This procedure sets the global translate-offset T that is
	-- required for a zoom-operation.
	-- After changing the zoom factor S (either by zoom on mouse pointer or
	-- by zoom on cursor), the translate-offset T must
	-- be calculated anew. The computation requires as input values
	-- the zoom center as virtual model point (CS1) or as canvas point (CS2).
	-- So there is a procedure set_translation_for_zoom that takes a canvas 
	-- point and another that takes a real model point.
	-- Later, when the actual drawing takes place (see function 
	-- cb_draw_objects) the drawing will be dragged back by the 
	-- translate-offset so that the operator gets the impression of a 
	-- zoom-in or zoom-out effect.
	-- Without applying a translate-offset the drawing would be appearing as 
	-- expanding to the upper-right (on zoom-in) or shrinking toward the 
	-- lower-left:
	procedure set_translation_for_zoom (
		-- The zoom factor before zoom:
		S1	: in type_zoom_factor;		

		-- The zoom factor after zoom:
		S2	: in type_zoom_factor;		

		-- The zoom center as canvas point:
		Z1	: in type_logical_pixels_vector); 


	
	procedure set_translation_for_zoom (
		-- The zoom factor before zoom:
		S1	: in type_zoom_factor;

		-- The zoom factor after zoom:
		S2	: in type_zoom_factor;

		-- The zoom center as a real model point:
		M	: in type_vector_model); 



	

	-- This composite type provides the ingredients
	-- required to do a zoom-to-area operation:
	type type_zoom_area is record
		-- This flag indicates that the operation is active.
		-- As soon as the operator clicks the "Zoom Area" button,
		-- this flag is set. It is cleared when the operator
		-- releases the right mouse button after she/he has
		-- defined the zoom-area. The zoom-area is a
		-- rectangle:
		active	: boolean := false; 

		-- This is the first corner of the area. It is assigned
		-- when the operator presses the right mouse button
		-- on the canvas to define the start point of the zoom-area:
		k1		: type_vector_model;

		-- This is the second corner of the area. It is assigned
		-- when the operator releases the right mouse button
		-- on the canvas to define end point of the the zoom-area:
		k2		: type_vector_model;

		-- This is the actual area to be zoomed to. It gets fully
		-- specified when the operator releases the right mouse button.
		-- The area will then be passed to the function zoom_to_fit
		-- in order to have the area displayed on the canvas:
		area	: type_area;

		---------------------------------------------------------------
		-- In order to display a rectangle that indicates the
		-- currently selected area we need this stuff.
		-- This is all in the canvas domain and has nothing to
		-- do with the area in the model domain (see above):
		
		-- This flag indicates that the operator has started
		-- the selection. It is cleared when the operator is done
		-- with the selection by releasing the right mouse button:
		started	: boolean := false;

		-- The corners of the selected area:
		l1		: type_logical_pixels_vector; -- the start point
		l2		: type_logical_pixels_vector; -- the end point
	end record;



	-- This is the instance of the zoom-area:
	zoom_area : type_zoom_area;

	
	-- This is the linewidth of the rectangle that
	-- marks the selected zoom area:
	zoom_area_linewidth : constant type_logical_pixels_positive := 2.0;


	-- This procedure resets the zoom_area (see above)
	-- to its default values. 
	-- Use this procedure to abort a zoom-to-area operation.
	procedure reset_zoom_area;



	-- Zooms in or out at the current cursor position.
	-- If the direction is ZOOM_IN, then the global zoom factor S
	-- is increased by multplying it with the zoom multiplier SM.
	-- If direction is ZOOM_OUT then it decreases by dividing
	-- by SM:
	procedure zoom_on_cursor (
		direction : in type_zoom_direction);


	-- Sets the cursor at the given position and zooms
	-- according to the given zoom level.
	-- Refreshes the canvas.
	procedure zoom_to (
		target	: in type_vector_model;
		level	: in type_zoom_factor);

	
	-- This procedure sets the global zoom factor S and translate-offset T
	-- so that all objects of the given area fit into the scrolled window.
	-- The zoom center is the top-left corner of the given area.
	procedure zoom_to_fit (
		area : in type_area);



	-- If a zoom-to-area operation has started, then
	-- this procedure draws the rectangle around the
	-- area to be zoomed at.
	-- The rectangle is drawn directly on the cairo_context.
	procedure draw_zoom_area;



-- VISIBLE AREA:

	-- Returns the currently visible area of the model.
	-- The visible area depends the current zoom factor,
	-- base-offset, translate-offset, dimensions of the scrolled window
	-- and the current settings of the scrollbars.
	function get_visible_area (
		canvas	: access gtk_widget_record'class)
		return type_area;

	

	-- This visible area is a global variable.
	-- It is updated by procedure cb_draw_objects.
	-- Some subprograms rely on it, for example those which
	-- move the cursor. For this reason the visible area is
	-- stored in a global variable.
	-- For the future: If the operator searches for a
	-- particular object, then the result of a search could be
	-- a message like "The object is outside the visible
	-- area at position (x/y)."
	visible_area : type_area;



	-- This procedure sets the translate-offset so that
	-- the given area gets centered in the visible area.
	-- The given area can be wider or higher than the visible area:
	procedure center_to_visible_area (
		area : in type_area);


	
	-- In MODE_3_ZOOM_FIT, here the last visible area
	-- immediately before the dimensions of the scrolled window
	-- change, is stored as reference.
	-- In other canvas modi it has no meaning:
	last_visible_area : type_area;


	-- This procedure takes an area and stores it in
	-- the global variable last_visible_area (see above).
	-- Its purpose is to be prepared to fit the area into the 
	-- scrolled window in MODE_3_ZOOM_FIT.
	-- It must be called after operations that result in a
	-- new visibible area. Such operations are:
	-- - scrollbar released
	-- - scroll up/down/right/left
	-- - move cursor
	-- - zoom on cursor
	-- - zoom to fit all 
	-- - zoom to fit area
	-- - zoom on mouse pointer
	-- In in other canvas modi but MODE_3_ZOOM_FIT this
	-- procedure has no meaning:
	procedure backup_visible_area (
		area : in type_area);

	
	

-- MAIN WINDOW:
	
	main_window	: gtk_window;
	
	-- inside main_window:
	box_v0		: gtk_vbox;		

	-- inside box_v0:
	box_h0 		: gtk_hbox;

	-- inside box_h0:
	box_v1		: gtk_vbox;		-- for coord. display, verb/noun
	separator	: gtk_separator;
	box_v2		: gtk_vbox;		-- for command buttons


	-- inside box_v0:
	box_v3 		: gtk_hbox;		-- for console

	-- inside box_v0:
	box_v4 		: gtk_hbox;		-- for properties of objects
	
	
	-- This procedure creates the main window and
	-- the boxes box_h, box_v1 and the separator:
	procedure create_window;



	

-- SCROLLED WINDOW:

	swin : gtk_scrolled_window;

	-- Inside the scrolled window the canvas exists.


	-- SCROLLBARS:
	
	scrollbar_h_adj, scrollbar_v_adj : gtk_adjustment;
	scrollbar_v, scrollbar_h : gtk_scrollbar;


	-- This composite type contains the settings
	-- of a scrollbar:
	type type_scrollbar_settings is record
		lower		: type_logical_pixels_positive;
		upper		: type_logical_pixels_positive;
		value		: type_logical_pixels_positive;
		page_size	: type_logical_pixels_positive;
	end record;

	-- These are the places where the initial settings of
	-- the scrollbars are stored:
	scrollbar_v_init : type_scrollbar_settings;
	scrollbar_h_init : type_scrollbar_settings;

	-- These are the places where we backup the settings of
	-- the scrollbars:
	scrollbar_h_backup, scrollbar_v_backup : type_scrollbar_settings;

	
	
	type type_scroll_direction is (
		SCROLL_UP,
		SCROLL_DOWN,
		SCROLL_RIGHT,
		SCROLL_LEFT);


	

	-- This is the initial size of the scrolled window.
	-- IMPORTANT: The height must be greater than the sum
	-- of the height of all other widgets in the main window !
	-- Otherwise the canvas may freeze and stop emitting signals.
	swin_size_initial : constant type_window_size := (
		width	=> 800,
		height	=> 700);
	
	-- The current size of the scrolled window. It gets updated
	-- in procedure set_up_swin_and_scrollbars and 
	-- in cb_swin_size_allocate. This variable is required
	-- in order to detect size changes of the scrolled window:
	swin_size : type_window_size;

	

	
	-- When the scrolled window is resized, then the canvas can
	-- operate in in several ways. Currently these modes are defined:
	type type_scrolled_window_zoom_mode is (
		-- No zoom. No moving. Just more or less of 
		-- the canvas area is exposed:
		MODE_1_EXPOSE_CANVAS,

		-- Center of visible canvas area remains in the center. 
		-- Around the center more or less of the canvas area is exposed:
		MODE_2_KEEP_CENTER,

		-- The visible area remains fit into the scrolled window.
		MODE_3_ZOOM_FIT);


	-- Here the zoom mode of the scrolled window is fixed:
	zoom_mode : constant type_scrolled_window_zoom_mode := 
		-- MODE_1_EXPOSE_CANVAS;
		-- MODE_2_KEEP_CENTER;
		MODE_3_ZOOM_FIT;



	
	
	-- Creates the scrolled window and its scrollbars:
	procedure create_scrolled_window_and_scrollbars;



	-- This procedure does a backup of the current settings
	-- of both the horizontal and the vertical scrollbar:
	procedure backup_scrollbar_settings;


	-- This procedure restores the settings of the vertical
	-- and horizontal scrollbar from the backup:
	procedure restore_scrollbar_settings;



	-- Sets the initial scrollbar settings based on
	-- current base-offset and bounding-box:
	procedure set_initial_scrollbar_settings;
	
	
	-- For debugging, these procedures output the settings
	-- of the scrollbars on the console:
	procedure show_adjustments_v;
	procedure show_adjustments_h;


	-- This function calculates the zoom factor required to
	-- fit the given area into the current scrolled window.
	-- The scrolled window has an initial size on startup. Later, when
	-- the operator resizes the main window, the scrolled window gets
	-- larger or smaller. This results in a situation depended zoom factor:
	function get_ratio (
		area : in type_area)
		return type_zoom_factor;


	-- Updates the limits of the scrollbars.
	-- The argument C1 provides the old corners of the 
	-- bounding-box on the canvas and C2 the new corners:
	procedure update_scrollbar_limits (
		C1, C2 : in type_bounding_box_corners);



	

-- CANVAS:
	
	-- This is the canvas where all the drawing takes place:
	canvas : gtk_drawing_area;


	-- This is the global drawing context.
	-- It is updated by the function cb_draw_objects:
	context : cairo.cairo_context;


	
	-- This is the size of the canvas in device pixels.
	-- It is set by procedure compute_canvas_size on system startup:
	canvas_size : type_window_size;


	-- This procedure should be called in order to schedule
	-- a refresh (or redraw) of the canvas:
	procedure refresh;
	

	-- This procedure computes the dimensions of the canvas
	-- and assigns them to variable canvas_size.
	-- The computation bases on the maximum allowed width
	-- and height of the bounding-box and the maximal
	-- zoom factor.
	-- CS: The computed dimensions may be not sufficient
	-- with very very large screens:
	procedure compute_canvas_size;

	
	-- This procedure outputs the current dimensions
	-- of the canvas on the console:
	procedure show_canvas_size;

	-- This procedure creates the canvas, sets its size,
	-- and adds it into the scrolled window.
	-- It adds events the canvas is to respond to.
	-- It also inserts the scrolled window (swin) into box_h0:
	procedure create_canvas;


	-- Shifts the scrolled window into the given direction
	-- by the given distance.
	-- If the scrolled window moves to the right, then the
	-- drawing area on the right becomes visible. At the same
	-- time drawing area on the left becomes invisible.
	-- Likewise, this applies if the scrolled window is moved left,
	-- down or up:
	procedure shift_swin (
		direction	: type_direction_RLUD;
		distance	: type_distance);


	-- Sets the focus to the canvas.
	-- It does more than just canvas.grab_focus.
	-- See implementation for details:
	procedure focus_canvas;
	

	
-- COORDINATES-DISPLAY:

	table : gtk_table;

	pointer_header							: gtk_label;
	pointer_x_label, pointer_y_label		: gtk_label;
	pointer_x_value, pointer_y_value		: gtk_text_view;
	pointer_x_buf, pointer_y_buf			: gtk_text_buffer;
	
	cursor_header							: gtk_label;
	cursor_x_label, cursor_y_label			: gtk_label;
	cursor_x_value, cursor_y_value			: gtk_text_view;
	cursor_x_buf, cursor_y_buf				: gtk_text_buffer;
	
	distances_header						: gtk_label;
	distances_dx_label, distances_dy_label	: gtk_label;
	distances_absolute_label				: gtk_label;
	distances_angle_label					: gtk_label;
	distances_dx_value, distances_dy_value	: gtk_text_view;
	distances_absolute_value				: gtk_text_view;
	distances_angle_value					: gtk_text_view;
	distances_dx_buf, distances_dy_buf		: gtk_text_buffer;
	distances_absolute_buf					: gtk_text_buffer;
	distances_angle_buf						: gtk_text_buffer;

	grid_header								: gtk_label;
	grid_x_label, grid_y_label				: gtk_label;
	grid_x_value, grid_y_value				: gtk_text_view;
	grid_x_buf, grid_y_buf					: gtk_text_buffer;

	zoom_label								: gtk_label;
	zoom_value								: gtk_text_view;
	zoom_buf								: gtk_text_buffer;

	scale_label								: gtk_label;
	scale_value								: gtk_text_view;
	scale_buf								: gtk_text_buffer;

	
	-- Creates the display for pointer/mouse and cursor position,
	-- distances and angle:
	procedure set_up_coordinates_display;


	-- Updates the cursor coordinates display
	-- by the current cursor position:
	procedure update_cursor_coordinates;


	-- Updates the distances display:
	procedure update_distances_display;


	-- Updates the zoom display:
	procedure update_zoom_display;

	
	-- Updates the grid display:
	procedure update_grid_display;


	-- Updates the scale display:
	procedure update_scale_display;


	
-- VERB AND NOUN DISPLAY:

	type type_mode is record
		box_mode			: gtk_vbox;
		box_mode_verb		: gtk_hbox;
		box_mode_noun		: gtk_hbox;
		label_mode			: gtk_label;
		label_mode_verb		: gtk_label;
		label_mode_noun		: gtk_label;
		-- cbox_mode_verb		: gtk_combo_box_text;
		-- cbox_mode_noun		: gtk_combo_box_text;
		cbox_mode_verb		: gtk_combo_box;
		cbox_mode_noun		: gtk_combo_box;

	end record;

	mode_display : type_mode;

	
	procedure build_mode_display;

	
	
-- GRID:
	
	-- This is the grid used by the canvas.
	-- It is primarily a copy of the grid settings of the database.
	-- Whenever a module is to be drawn on the canvas,
	-- this variable must be set according to the settings
	-- of the module (in database). This must be done for example when:
	-- - the GUI is started the first time with a module to be displayed.
	-- - the active module is changed 
	--
	-- If no grid is set by the user, then a default grid is applied
	-- as specified in package pac_grid.
	-- However, procedure set_grid_to_scale may modify it
	-- according to the scale M.
	grid : type_grid;


	-- This procedure sets the grid spacing
	-- according to the scale specified
	-- by the operator:
	procedure set_grid_to_scale;
	
	
	-- This function returns the grid point that is
	-- closest to the given model point;
	function snap_to_grid (
		point : in type_vector_model)
		return type_vector_model;




	-- This function returns the space between
	-- the grid columns or rows. It returns the lesser
	-- spacing of them. It calculates the spacing by this
	-- equation:
	-- x = grid.spacing.x * zoom factor
	-- y = grid.spacing.y * zoom factor
	-- Then the lesser one, either x or y will be returned:
	function get_grid_spacing (
		grid : in type_grid)
		return type_logical_pixels_positive;



	-- This procedure draws the grid in the visible area.
	-- Outside the visible area nothing is drawn in order to save time.
	-- The procedure works as follows:
	-- 1. Define the begin and end of the visible area in 
	--    x and y direction.
	-- 2. Find the first column that comes after the begin of 
	--    the visible area (in x direction).
	-- 3. Find the last column that comes before the end of the 
	--    visible area (in x direction).
	-- 4. Find the first row that comes after the begin of the 
	--    visible area (in y direction).
	-- 5. Find the last row that comes before the end of the 
	--    visible area (in y direction).
	-- 6. Draw the grid as dots or lines, depending on the user specified
	--    settings.
	procedure draw_grid;


	

-- CURSOR:

	-- The cursor is a crosshair that can be moved by the
	-- cursor keys (arrow keys) about the canvas:
	type type_cursor is record
		position	: type_vector_model := origin;

		-- For drawing the cursor:
		linewidth_1	: type_logical_pixels_positive := 1.0;
		linewidth_2	: type_logical_pixels_positive := 4.0;
		length_1	: type_logical_pixels_positive := 20.0;
		length_2	: type_logical_pixels_positive := 20.0;
		radius		: type_logical_pixels_positive := 25.0;
		
		-- CS: blink, color, ...
	end record;

	
	-- This is the instance of the cursor:
	cursor : type_cursor;


	-- This procedure moves the cursor to the given destination:
	procedure move_cursor (
		destination : type_vector_model);


	-- This procedure moves the cursor into 
	-- the given direction by the current grid spacing:
	procedure move_cursor (
		direction : type_direction_RLUD);


	-- This procedure moves the cursor by
	-- the given vector:
	procedure move_cursor_by (
		vector : type_vector_model);

	
	-- Returns the current position of the cursor:
	function get_cursor_position
		return type_vector_model;
	
	
	-- This procedure draws the cursor at its current
	-- position. To keep things simple, the cursor is
	-- drawn always, regardless whether it is in the visible
	-- area or not:
	procedure draw_cursor;


	
-- MOUSE / POINTER POSITION:

	-- This function returns the current pointer/mouse
	-- position in model coordinates:
	function get_mouse_position
		return type_vector_model;

	

-- SCALE:

	-- The scale is a floating point type.
	-- Its range is defined here:
	type type_scale is digits 3 range 0.01 .. 100.0;
	-- If you intend to change this declaration, please see the
	-- comments in function to_string.

	-- This is the global scale:
	M : type_scale := 1.0; 
	
	-- Examples for usage:
	-- 1)
	-- If M is set to 10 then the scale is 1:10 (in words one to ten).
	-- This means: A distance of 1mm in the model represents
	-- a distance of 10mm in the real world.
	-- The drawing shows the reality downsized.
	
	-- 2)
	-- If M is set to 0.1 then the scale is 10:1 (in words ten to one).
	-- This means: A distance of 10mm in the model represents
	-- a distance of 1mm in the real world. 
	-- The drawing shows the reality enlarged.


	package pac_scale_io is new ada.text_io.float_io (type_scale);
	
	
	-- Converts the given scale factor to a string like 1:100
	-- or 100:1:
	function to_string (
		scale : in type_scale)
		return string;

	
	-- Converts a distance of the model to a distance 
	-- in reality:
	function to_reality (
		d : in type_distance)
		return type_distance;

	procedure to_reality (
		d : in out type_distance);

	
	
	-- Converts a distance of the reality to
	-- a distance in the model:
	function to_model (
		d : in type_distance)
		return type_distance;

	procedure to_model (
		d : in out type_distance);



	-- Converts a vector from model to reality:
	function to_reality (
		v : in type_vector_model)
		return type_vector_model;


	-- Converts a vector from reality to model:
	function to_model (
		v : in type_vector_model)
		return type_vector_model;



	


-- BUTTONS:

	buttons_table		: gtk_table;
	
	button_zoom_fit		: gtk_button;
	button_zoom_area	: gtk_button;
	button_move			: gtk_button;
	button_add			: gtk_button;
	button_delete		: gtk_button;
	button_export		: gtk_button;
	
	-- This procedure creates the buttons:
	procedure create_buttons;



	
-- CONSOLE AND STATUS:

	label_console	: gtk_label;
	console			: gtk_combo_box_text;


	procedure build_console; -- incl. status bar


	label_status	: gtk_label;

	procedure set_status (text : in string);
	procedure status_clear;
	procedure status_enter_verb;
	procedure status_enter_noun;
	procedure status_verb_invalid;
	procedure status_noun_invalid;
	
	
---------------------------------------------------------------------
-- INITIALISATION AND CALLBACKS:

	
	



-- MAIN WINDOW:

	procedure cb_window_focus (
		window : access gtk_window_record'class);



	-- This procedure instantiates the main window,
	-- sets the title bar, connects signals with callback
	-- subprograms and creates boxes inside the window:
	procedure set_up_main_window;


	

	
-- SCROLLED WINDOW AND SCROLLBARS:
			
	-- This procedure creates the scrolled window,
	-- assigns to it the initial size (widht and height)
	-- and sets the behaviour of them.
	-- It also connects the signals emitted by the scrollbars
	-- with the callback subprograms.
	procedure set_up_swin_and_scrollbars;




	
-- CANVAS:

	-- This procedure is called when the canvas changes
	-- its size. It is not used currently because the canvas
	-- has a fixed size in this demo program (see below):
	-- procedure cb_canvas_size_allocate (
	-- 	canvas		: access gtk_widget_record'class;
	-- 	allocation	: gtk_allocation);

	
	-- This procedure creates the canvas, assigns to
	-- it a fixed size and connects its signals
	-- with callback subprograms:
	procedure set_up_canvas;







	
-- EDIT PROCESS STATUS:

	type type_editing_process is private;

	-- Resets all components of the editing_process
	-- to their default values:
	procedure reset_editing_process;
	


-- CLARIFICATION:
	
	procedure set_request_clarification;

	procedure reset_request_clarification;
		
	-- Returns true if the flag "request_clarificaton" is set:
	function clarification_pending return boolean;




-- EDIT PROCESS RUNNNING:

	-- Sets the flag running. Clears the escape_counter:
	procedure set_edit_process_running; -- CS rename to edit_process_running_set

	procedure reset_edit_process_running;
	
	function edit_process_running return boolean;



-- FINALIZING GRANTED:
	
	procedure set_finalizing_granted;  -- CS rename to finalizing_granted_set
	
	procedure reset_finalizing_granted;

	function finalizing_granted return boolean;

	

-- ESCAPE KEY EVENT COUNTER:

	-- Increments the escape counter:
	procedure escape_key_pressed;

	-- We count only up to 2:
	type type_escape_count is new natural range 0 .. 2;
	
	function get_escape_counter return type_escape_count;
	

	

-- OBJECT AND PATH:
	
	-- When an object is to be moved or dragged, then
	-- this global variable should be used:
	object_point_of_attack : type_vector_model;

	-- When an object is being moved, then this can be done
	-- via mouse or keyboard. This global variable should be used:
	object_tool : type_tool := MOUSE;


	-- When a path, consisting of one or two lines is drawn
	-- then it is temprarily stored here:
	live_path : pac_path.type_path_live;

	-- When a line or a path is being drawn, then this
	-- is the linewidth:
	object_linewidth : type_distance_positive := 0.15;


	
	-- CS: It is probably not a good idea to reset all
	-- properties of the object at once in a single procedure
	-- like reset_object. 
	-- Instead individual small procedures could be useful.
	
	
	-- This function returns the object tool position
	-- according to the current value of object_tool.
	-- If the tool is MOUSE then the return is snapped
	-- to the nearest grid point:
	function get_object_tool_position
		return type_vector_model;


	



	
-- LIVE PATH:
	
	-- Outputs the start point of the live_path in
	-- the status bar along with some advises.
	-- The string given via 'prepend' will be prepended
	-- to the message:
	procedure status_bar_path_show_A (
		prepend : in string := "");
	
	
	-- Builds a path. This procedure requires to be called twice:
	-- first time for the start and the second time for the 
	-- end point of the path.
	-- The current bend style in live_path is taken into account.
	-- The path may be started and finished with different tools. 
	-- For example start with MOUSE and finish with KEYBOARD 
	-- or vice versa:
	procedure make_path (
		tool	: in type_tool;
		point	: in type_vector_model;
		process	: not null access procedure (line : in type_line));






-- PRIMARY TOOL:	

	box_primary_tool		: gtk_vbox;
	label_primary_tool		: gtk_label;
	cbox_primary_tool		: gtk_combo_box_text;

	-- The primary tool used for drawing and navigating within the canvas:
	primary_tool : type_tool := primary_tool_default;

	procedure build_primary_tool_display;
	
	procedure update_primary_tool_display;

	-- Toggles between tool keyboard and mouse.
	-- If keyboard is activated, then the focus
	-- is set on the canvas. This way the operator
	-- can immediately move the cursor via the keyboard:
	procedure change_primary_tool;


	-- Returns the position of the current primary
	-- tool (that is mouse or cursor). The position
	-- is snapped to the current grid:
	function get_primary_tool_position
		return type_vector_model;

	

-- CATCH ZONE:

	catch_zone_radius_default : type_logical_pixels_positive := 20.0;
	
	
	-- Returns a catch zone radius according to the current
	-- zoom factor (S) and the given distance in pixels.
	-- The zone is a circular area with a radius
	-- expressed in logical pixels:
	function get_catch_zone (
		pixels	: in type_logical_pixels_positive)
		return type_zone_radius;



	
-- PROPERTIES BOX:

	
	-- This procedure clears out the prperties box.
	-- It iterates all children of the box and calls
	-- procedure cb_delete_box_properties_child:
	procedure clear_out_properties_box;






-- RENAME WINDOW:

	-- The window to rename objects is used in all domains
	-- and in various situations. For this reason we make it
	-- a generic object:
	rename_window : gtk.window.gtk_window;

	-- This flag indicates that the rename window is open:
	rename_window_open : boolean := false;
	
	-- CS: these variables should be moved to procedure
	-- build_rename_window:
	-- This is the field inside the rename_window 
	-- where the operator enters the new name of an object:
	rename_new : gtk_gentry;
	
	-- This is the field inside the rename_window 
	-- where the old name of an object is shown:
	rename_old : gtk_gentry;

	
	-- This procedure assembles the rename_window with 
	-- all its basic properties.
	-- It connects the "on_key_press_event" with the
	-- callback function cb_rename_window_key_pressed (see below).
	-- This procedure DOES NOT show the rename window. It just prepares
	-- basic things. The actual showing is preformed by a procedure in
	-- the package where the canvas is instantiated (see
	-- for example procedure show_rename_window 
	-- in et_canvas_schematic):
	procedure build_rename_window;


	

-- VISIBILITY THRESHOLD:

	
	-- If an object occupies a space that is wider or
	-- higher than this constant, then it will be drawn on the screen:
	visibility_threshold : constant type_logical_pixels_positive := 2.0;

	
	-- Returns true if the given area is large enough
	-- to display objects therein:
	function above_visibility_threshold (
		a : in type_area)
		return boolean;
	





	

	


	
	
-- PRIMITIVE DRAW OPERATIONS:

	-- Strokes the global context (see above):
	procedure stroke;


	minimal_linewidth : constant type_logical_pixels_positive := 1.0;
	

	-- Sets the linewidth according to the
	-- current zoom-factor:
	procedure set_linewidth (
		w : in type_distance_positive);



	
	
	-- This is a primitive draw operation that draws a line.
	-- The argument pos contains the position and rotation
	-- of the parent complex object.
	-- In case there is no parent object then the pos argument
	-- can be omitted which results in a default of (0;0) and 0 degree.
	-- Regarding the argument do_stroke there are two modes:
	-- 1. If the argument do_stroke is false (default) then
	--  no setting of linewidth and no stroking will be done. In this
	--  case it is assumed that the caller has already set a linewidth
	--  and a color and that the caller will later care for a stroke command. 
	--  The given linewidth has no meaning in this case. This mode
	--  requires less time for drawing the line than with do_stroke enabled
	--  and should be used when many lines of same linewidth have to be drawn.
	--  An explicit call of the stroke procedure is required finally.
	-- 2. If do_stroke is true, then the given linewidth is applied,
	--  the color previously set by the caller is applied,
	--  the line drawn and finally a stroke command executed.
	--  If the given linewidth is zero, then a minimum linewidth is
	--  ensured internally that is independed of the zoom-factor.
	-- 3. If force is true, then the line will be drawn independed
	--  of its bounding-box. CS: not implemented yet, likewise for circle and arc
	-- 4. If the line is being moved by the
	--  operator then it gets drawn according to the current
	--  point_of_attack and object_tool:
	-- 	
	-- NOTE: The polyline argument is a makeshift as long as there
	-- in no procedure to draw a polyline. If polyline is true, then
	-- the start point of the given line is drawn via a line_to cairo-operation.
	--
	-- NOTE: CS: The line style is currently ignored.
	procedure draw_line (
		line		: in type_line'class;
		pos			: in type_position := origin_zero_rotation; -- includes x,y, rotation
		width		: in type_distance_positive;
		mirror		: in type_mirror := MIRROR_NO;
		style		: in type_line_style := CONTINUOUS;
		do_stroke	: in boolean := false;
		polyline	: in boolean := false;
		force		: in boolean := false);


	-- This is a primitive draw operation that draws a circle.
	-- For arguments see procedure draw_line.
	-- It is recommended to set the linewidth to zero when
	-- the circle is to be filled.
	-- NOTE: CS: The line style is currently ignored.
	procedure draw_circle (
		circle		: in type_circle'class;
		pos			: in type_position := origin_zero_rotation; -- includes x,y, rotation
		filled		: in type_filled;
		width		: in type_distance_positive;
		mirror		: in type_mirror := MIRROR_NO;
		style		: in type_line_style := CONTINUOUS;
		do_stroke	: in boolean := false);


	-- This is a primitive draw operation that draws an arc.
	-- For arguments see draw_line:
	-- NOTE: CS: The line style is currently ignored.
	procedure draw_arc (
		arc			: in type_arc'class;
		pos			: in type_position := origin_zero_rotation; -- includes x,y, rotation
		width		: in type_distance_positive;
		mirror		: in type_mirror := MIRROR_NO;
		style		: in type_line_style := CONTINUOUS;
		do_stroke	: in boolean := false);

	
	procedure draw_rectangle (
		rectangle	: in type_area;
		pos			: in type_position := origin_zero_rotation;
		mirror		: in type_mirror := MIRROR_NO;
		-- CS ? style		: in type_line_style := CONTINUOUS;
		width		: in type_distance_positive);


	
-- ORIGIN OF TEXTS AND COMPLEX OBJECTS:

	-- These origins have a size and linewidth given in the model-domain.
	-- If the operator zooms-in on such an origin then it
	-- gets magnified -> it depends on the zoom-factor:
	origin_arm_length : constant type_distance_positive := 1.0;
	origin_linewidth : constant type_distance_positive := 0.1;

	-- Draws an origin at the given position.
	-- Since the given position also includes the
	-- rotation, the origin can be rotated if required:
	procedure draw_origin (
		position	: in type_position);

	

-- ORIGIN OF THE DRAWING:

	-- This is the origin of the drawing. It is fixed to
	-- position (0;0). It is drawn in the canvas-domain
	-- in order to have a constant size and linewidth:

	origin_drawing_arm_length : constant type_logical_pixels_positive := 20.0;
	origin_drawing_linewidth  : constant type_logical_pixels_positive := 1.0;

	-- This procedure draws the drawing origin:
	procedure draw_drawing_origin;

	

	

-- MOUSE EVENTS
	
	type type_mouse_event is record
		point		: type_vector_model;
		button		: type_mouse_button;
	end record;


	function to_string (
		event	: in type_mouse_event)
		return string;

	
	-- This function processes a "mouse button pressed event"
	-- (on the canvas) and converts it to the affected button
	-- and the model point at which the event took place.
	-- It also handles the start of a "zoom-to-area" operation:
	function get_mouse_button_pressed_event (
		event	: gdk_event_button)
		return type_mouse_event;

	
	-- This function processes a "mouse button released event"
	-- (on the canvas) and converts it to the affected button
	-- and the model point at which the event took place.
	-- It also handles the end of a "zoom-to-area" operation:
	function get_mouse_button_released_event (
		event	: gdk_event_button)
		return type_mouse_event;



	function get_mouse_moved_event (
		event	: gdk_event_motion)
		return type_vector_model;



	-- Converts the key type to a string in a form
	-- like "key 115"
	-- CS: Improvement required so that "key 115 (s)" is returned.
	function to_string (
		key : in gdk_key_type)
		return string;
	
	
private
	
	
	type type_editing_process is record

		-- This flag indicates that the operator has started
		-- editing an object (moving, deleting, routing, ...):
		running 				: boolean := false;

		-- When the operator is requred to clarify which object
		-- is meant, then this flag indicates whether clarification is 
		-- required or not:
		request_clarificaton	: boolean := false;

		-- If the editing process is allowed to be finalized,
		-- then this flag should be used:
		finalizing_granted		: boolean := false;

		-- An editing process can be cancelled partly or
		-- completely. For this reason we have a counter that
		-- tells how often the operator has pressed the ESC-key:
		escape_counter			: type_escape_count := 0;
	end record;


	editing_process : type_editing_process;
	
	
-- CALLBACKS:

	-- BUTTONS:

	
	-- This callback procedure is called each time the 
	-- button "add" is clicked.
	procedure cb_add (
		button : access gtk_button_record'class);

	access_cb_add : constant cb_gtk_button_void := cb_add'access;

	
	
	-- This callback procedure is called each time the 
	-- button "delete is clicked.
	procedure cb_delete (
		button : access gtk_button_record'class);

	access_cb_delete : constant cb_gtk_button_void := cb_delete'access;


	
	
	-- This callback procedure is called each time the 
	-- button "move" is clicked.
	procedure cb_move (
		button : access gtk_button_record'class);

	access_cb_move: constant cb_gtk_button_void := cb_move'access;

	
	-- This callback procedure is called each time the 
	-- button "export" is clicked:
	procedure cb_export (
		button : access gtk_button_record'class);

	access_cb_export : constant cb_gtk_button_void := cb_export'access;

	

-- MAIN WINDOW:

	-- This procedure is called when the operator terminates
	-- the main window by clicking the X in the upper right corner
	-- of the main window:
	procedure cb_terminate (
		window : access gtk_widget_record'class);

	access_cb_terminate : constant cb_gtk_widget_void := cb_terminate'access;


	-- This callback procedure is called each time the 
	-- size_allocate signal is emitted by the main window:
	procedure cb_main_window_size_allocate (
		window		: access gtk_widget_record'class;
		allocation	: gtk_allocation);

	access_cb_main_window_size_allocate : constant 
		cb_gtk_widget_gtk_allocation_void := cb_main_window_size_allocate'access;



	-- -- This function is called each time the operator hits a key:
	-- function cb_main_window_key_pressed (
	-- 	window	: access gtk_widget_record'class;
	-- 	event	: gdk_event_key)
	-- 	return boolean;
 -- 
	-- access_cb_main_window_key_pressed : constant
	-- 	cb_gtk_widget_gdk_event_key_boolean := cb_main_window_key_pressed'access;

	

	-- This function is called each time the operator
	-- presses a mouse button.
	function cb_window_button_pressed (
		window	: access gtk_widget_record'class;
		event	: gdk_event_button)
		return boolean;

	access_cb_window_button_pressed : constant
		cb_gtk_widget_gdk_event_button_boolean := cb_window_button_pressed'access;


	

	function cb_main_window_configure (
		window		: access gtk_widget_record'class;
		event		: gdk.event.gdk_event_configure)
		return boolean;

	access_cb_main_window_configure : constant
		cb_gtk_widget_gdk_event_configure_boolean := cb_main_window_configure'access;
	
	
	

	function cb_main_window_state_change (
		window		: access gtk_widget_record'class;
		event		: gdk.event.gdk_event_window_state)
		return boolean;

	access_cb_main_window_state_change : constant
		cb_gtk_widget_gdk_event_window_state_boolean := cb_main_window_state_change'access;



	procedure cb_main_window_realize (
		window	: access gtk_widget_record'class);

	access_cb_main_window_realize : constant
		cb_gtk_widget_void := cb_main_window_realize'access;



	procedure cb_main_window_activate (
		window : access gtk_window_record'class);

	access_cb_main_window_activate : constant
		cb_gtk_window_void := cb_main_window_activate'access;
	


-- SCROLLED WINDOW AND SCROLLBARS:
			
	-- This callback procedure is called each time the size_allocate signal
	-- is emitted by the scrolled window.
	procedure cb_swin_size_allocate (
		swin		: access gtk_widget_record'class;
		allocation	: gtk_allocation);
	
	access_cb_swin_size_allocate : constant
		cb_gtk_widget_gtk_allocation_void := cb_swin_size_allocate'access;
	


	-- This procedure is called whenever the horizontal scrollbar is moved, 
	-- either by the operator or by internal calls.
	procedure cb_horizontal_moved (
		scrollbar : access gtk_adjustment_record'class);

	access_cb_horizontal_moved : constant
		cb_gtk_adjustment_void := cb_horizontal_moved'access;
	

	
	-- This procedure is called whenever the vertical scrollbar is moved, 
	-- either by the operator or by internal calls.
	procedure cb_vertical_moved (
		scrollbar : access gtk_adjustment_record'class);

	access_cb_vertical_moved : constant
		cb_gtk_adjustment_void := cb_vertical_moved'access;
	


	-- This procedure is called when the operator clicks
	-- on the vertical scrollbar:
	function cb_scrollbar_v_pressed (
		bar		: access gtk_widget_record'class;
		event	: gdk_event_button)
		return boolean;

	access_cb_scrollbar_v_pressed : constant
		cb_gtk_widget_gdk_event_button_boolean := cb_scrollbar_v_pressed'access;
	

	-- This procedure is called when the operator releases
	-- the mouse button after clicking on the vertical scrollbar:
	function cb_scrollbar_v_released (
		bar		: access gtk_widget_record'class;
		event	: gdk_event_button)
		return boolean;

	access_cb_scrollbar_v_released : constant
		cb_gtk_widget_gdk_event_button_boolean := cb_scrollbar_v_released'access;


	
	-- This procedure is called when the operator clicks
	-- on the horizontal scrollbar:
	function cb_scrollbar_h_pressed (
		bar		: access gtk_widget_record'class;
		event	: gdk_event_button)
		return boolean;
	
	access_cb_scrollbar_h_pressed : constant
		cb_gtk_widget_gdk_event_button_boolean := cb_scrollbar_h_pressed'access;



	
	-- This procedure is called when the operator releases
	-- the mouse button after clicking on the horizontal scrollbar:
	function cb_scrollbar_h_released (
		bar		: access gtk_widget_record'class;
		event	: gdk_event_button)
		return boolean;

	access_cb_scrollbar_h_released : constant
		cb_gtk_widget_gdk_event_button_boolean := cb_scrollbar_h_released'access;
	




	-- This function is called each time the mouse wheel is
	-- rolled inside the canvas by the operator:
	function cb_mouse_wheel_rolled (
		canvas	: access gtk_widget_record'class;
		event	: gdk_event_scroll)
		return boolean;

	access_cb_mouse_wheel_rolled : constant
		cb_gtk_widget_gdk_event_scroll_boolean := cb_mouse_wheel_rolled'access;



	
	-- This procedure deletes a child of the prperties box:
	procedure cb_delete_box_properties_child (
		child : not null access gtk_widget_record'class);	

	access_cb_delete_box_properties_child : constant 
		gtk_callback := cb_delete_box_properties_child'access;



-- RENAME WINDOW:

	-- See comments on rename window above.
	
	-- This callback function is called whenever
	-- the operator presses a key in the rename window.
	-- If ESC key pressed, then the window is destroyed
	-- by emitting the "destroy" signal. The connection
	-- to the "destroy" signal is estabilshed in the package
	-- where the canvas is instantiated.
	-- All other key-press events are propagated to the
	-- field where the new name is entered (see 
	-- variable "rename_new" above):
	function cb_rename_window_key_pressed (
		window	: access gtk_widget_record'class;
		event	: gdk_event_key)
		return boolean;
	

	access_cb_rename_window_key_pressed : constant
		cb_gtk_widget_gdk_event_key_boolean := cb_rename_window_key_pressed'access;


	
end et_canvas;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
