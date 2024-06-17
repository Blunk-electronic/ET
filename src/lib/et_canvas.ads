------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                CANVAS                                    --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2024                                                       --
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

with gdk.event;					use gdk.event;

with gtk.widget;				use gtk.widget;
with gtk.window;				use gtk.window;
with gtk.separator;				use gtk.separator;
with gtk.box;					use gtk.box;
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


with et_logical_pixels;			use et_logical_pixels;
with et_geometry_2a;
with et_window_dimensions;		use et_window_dimensions;


generic
	with package pac_geometry_2 is new et_geometry_2a (<>);

	
package et_canvas is
	use pac_geometry_2;


-- ZOOM:

	-- This is the specification of the zoom factor
	-- (German: Vergroesserungsfaktor)
	type type_zoom_factor is digits 3 range 0.10 .. 100.0;
	

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


	-- This procedure increases the global zoom factor
	-- by multiplying it by SM:
	procedure increase_zoom_factor;

	
	-- This procedure decreases the global zoom factor
	-- by dividing it by SM:
	procedure decrease_zoom_factor;

	

	-- There are two kinds of zoom-operations:
	type type_zoom_direction is (ZOOM_IN, ZOOM_OUT);


	
	
-- CONVERSIONS:

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
		d : in type_distance_model_positive)
		return type_logical_pixels_positive;

	
	-- Converts the given canvas distance to
	-- a model distance according to the current zoom factor S:
	function to_distance (
		d : in type_logical_pixels_positive)
		return type_distance_model_positive;



	
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
	
	box_h 		: gtk_hbox;
	box_v0		: gtk_vbox;
	box_v1		: gtk_vbox;
	separator	: gtk_separator;

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
		width	=> 400,
		height	=> 400);
	
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

	
	-- Strokes the global context (see above):
	procedure stroke;

	
	-- This is the size of the canvas in device pixels.
	-- It is set by procedure compute_canvas_size on system startup:
	canvas_size : type_window_size;


	-- This procedure should be called in order to schedule
	-- a refresh (or redraw) of the canvas:
	procedure refresh (
		canvas	: access gtk_widget_record'class);
	

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
	-- It also adds events to be sensitive to:
	procedure create_canvas;


	-- Shifts the canvas into the given direction
	-- by the given distance:
	procedure shift_canvas (
		direction	: type_direction;
		distance	: type_distance_model);



	
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


	
	

-- GRID:

	-- The grid helps the operator to align or place objects:
	type type_grid_on_off is (GRID_ON, GRID_OFF);
	type type_grid_style is (STYLE_DOTS, STYLE_LINES);

	-- The linewidth of the grid lines:
	grid_width_lines : constant type_logical_pixels_positive := 0.5;

	-- The linewidth of the circles which form the grid dots:
	grid_width_dots : constant type_logical_pixels_positive := 1.0;
	grid_radius_dots : constant type_logical_pixels_positive := 0.5;

	
	-- The arm length of a grid point if drawn as a cross:
	grid_cross_arm_length : constant type_logical_pixels_positive := 1.0;
	

	-- The default grid size in in the model domain:
	grid_spacing_default : constant type_distance_model_positive := 10.0; 
	-- use it for the example with the rectangle, triangle and circle

	-- grid_spacing_default : constant type_distance_model_positive := 100.0;
	-- use it for the bridge example
	
	-- grid_spacing_default : constant type_distance_model_positive := 1.0; 
	-- use it for the screw example

	-- If the displayed grid is too dense, then it makes no
	-- sense to draw a grid. For this reason we define a minimum
	-- distance between grid rows and columns. If the spacing becomes
	-- greater than this threshold then the grid will be drawn:
	grid_spacing_min : constant type_logical_pixels_positive := 10.0;

	
	type type_grid is record
		on		: type_grid_on_off := GRID_ON;
		-- on		: type_grid_on_off := GRID_OFF;
		spacing : type_vector_model := (others => grid_spacing_default);
		style	: type_grid_style := STYLE_DOTS;
		--style	: type_grid_style := STYLE_LINES;
	end record;

	
	-- This is the grid used by this demo program:
	grid : type_grid;


	-- This procedure sets the grid spacing
	-- according to the scale specified
	-- by the operator (see package spec. demo_scale):
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


	-- This procedure moves the cursor into the given direction:
	procedure move_cursor (
		direction : type_direction);


	-- This procedure draws the cursor at its current
	-- position. To keep things simple, the cursor is
	-- drawn always, regardless whether it is in the visible
	-- area or not:
	procedure draw_cursor;



-- SCALE:

	-- The scale is a floating point type.
	-- Its ranges are defined here:
	type type_scale is digits 3 range 0.01 .. 100.0;
	-- If you intend to change this declaration, please see the
	-- comments in function to_string.

	-- This is the global scale:
	M : type_scale := 1.0; 
	-- use it for the rectangle, triangle and circle

	-- M : type_scale := 50.0; 
	-- use it for the bridge example

	-- M : type_scale := 0.1;
	-- M : type_scale := 0.04;
	--use it for the screw example
	
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
		d : in type_distance_model)
		return type_distance_model;

	procedure to_reality (
		d : in out type_distance_model);

	
	
	-- Converts a distance of the reality to
	-- a distance in the model:
	function to_model (
		d : in type_distance_model)
		return type_distance_model;

	procedure to_model (
		d : in out type_distance_model);



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


private

-- CALLBACKS:

	-- BUTTONS:


	-- This callback procedure is called each time the 
	-- button "zoom area" is clicked.
	procedure cb_zoom_area (
		button : access gtk_button_record'class);

	access_cb_zoom_area : constant cb_gtk_button_void := cb_zoom_area'access;



	
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
	-- the demo program by clicking the X in the upper right corner
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
	
		


	-- This callback function is called each time the operator
	-- clicks on the canvas.
	-- It sets the focus on the canvas and moves the cursor
	-- to the place where the operator has clicked the canvas.
	function cb_canvas_button_pressed (
		canvas	: access gtk_widget_record'class;
		event	: gdk_event_button)
		return boolean;

	access_cb_canvas_button_pressed : constant
		cb_gtk_widget_gdk_event_button_boolean := cb_canvas_button_pressed'access;



	-- This callback function is called each time the operator
	-- releases a mouse button after clicking on the canvas.
	function cb_canvas_button_released (
		canvas	: access gtk_widget_record'class;
		event	: gdk_event_button)
		return boolean;

	access_cb_canvas_button_released : constant
		cb_gtk_widget_gdk_event_button_boolean := cb_canvas_button_released'access;




	-- This callback function is called each time the operator
	-- moves the pointer (or the mouse) inside the canvas:
	function cb_canvas_mouse_moved (
		canvas	: access gtk_widget_record'class;
		event	: gdk_event_motion)
		return boolean;

	access_cb_canvas_mouse_moved : constant
		cb_gtk_widget_gdk_event_motion_boolean := cb_canvas_mouse_moved'access;




	-- This function is called each time the mouse wheel is
	-- rolled inside the canvas by the operator:
	function cb_mouse_wheel_rolled (
		canvas	: access gtk_widget_record'class;
		event	: gdk_event_scroll)
		return boolean;

	access_cb_mouse_wheel_rolled : constant
		cb_gtk_widget_gdk_event_scroll_boolean := cb_mouse_wheel_rolled'access;




	-- This callback function is called each time the
	-- operator hits a key and if the canvas has the focus:
	function cb_canvas_key_pressed (
		canvas	: access gtk_widget_record'class;
		event	: gdk_event_key)
		return boolean;

	access_cb_canvas_key_pressed : constant
		cb_gtk_widget_gdk_event_key_boolean := cb_canvas_key_pressed'access;
	
end et_canvas;

