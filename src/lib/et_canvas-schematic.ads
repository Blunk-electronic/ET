------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS FOR SCHEMATIC                             --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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

with gtk.main;
with gtk.window; 			use gtk.window;
with gtk.widget;  			use gtk.widget;
with gtk.box;				use gtk.box;
with gtk.button;     		use gtk.button;
with gtk.handlers;			use gtk.handlers;
with gtk.toolbar; 			use gtk.toolbar;
with gtk.tool_button;		use gtk.tool_button;
with gtk.enums;				use gtk.enums;
with gtk.gentry;			use gtk.gentry;
with gtk.combo_box_text;	use gtk.combo_box_text;
with gtk.frame;				use gtk.frame;
with gtk.scrolled_window;	use gtk.scrolled_window;
with gtk.adjustment;		use gtk.adjustment;

with gdk;
with gdk.types;

with glib;					use glib;
with glib.object;			use glib.object;
with glib.values;			use glib.values;
with cairo;					use cairo;
with cairo.pattern;			use cairo.pattern;
with gtkada.style;     		use gtkada.style;

with pango.layout;			use pango.layout;

with ada.containers;		use ada.containers;
with ada.containers.doubly_linked_lists;

with et_coordinates;		use et_coordinates;
with et_project;			--use et_project;
with et_frames;				--use et_frames;

with et_canvas_2;

package et_canvas.schematic is

	package pac_canvas is new et_canvas_2.pac_canvas (
		type_distance	=> et_coordinates.type_distance,
		geometry		=> et_coordinates.geometry);

	use pac_canvas;
	


-- MODEL
	type type_model is new pac_canvas.type_model with record
		module	: et_project.type_modules.cursor;

		-- These variables are frequently used. Procedure set_module
		-- sets them. Other operations are free to access
		-- them.
		frame				: et_frames.type_frame (et_frames.SCHEMATIC);
		frame_bounding_box	: type_model_rectangle;

		paper_bounding_box	: type_model_rectangle;
		paper_height		: type_model_coordinate;
		paper_width			: type_model_coordinate;

		title_block_position	: et_frames.type_position;

		-- the active sheet
		sheet	: et_coordinates.type_sheet := type_sheet'first;
	end record;

	type type_model_ptr is access all type_model'class;
-- 	type type_model_ptr is access all type_model;

	model	: type_model_ptr;


	
	-- Creates a new model (or a drawing sheet according to the example above):
	procedure gtk_new (self : out type_model_ptr);

-- 	-- Initializes the internal data so that the model can send signals:
-- 	procedure init (self : not null access type_model'class);


-- VIEW

-- 	-- scale
-- 	scale_min : constant gdouble := 0.1;
-- 	scale_max : constant gdouble := 10.0;
-- 	subtype type_scale is gdouble range scale_min .. scale_max;
-- 	scale_default : constant type_scale := 1.0;
-- 	scale_delta_on_zoom : constant type_scale := 0.1;
-- 	
-- 	
	-- The view (or canvas) displays a certain region of the model (or the sheet) 
	-- depending on scrolling or zoom.
	type type_view_sch is new pac_canvas.type_view with null record;

	-- The pointer to the canvas/view:
	type type_view_ptr_sch is access all type_view_sch'class;


	canvas	: type_view_ptr_sch;



	
-- 	procedure viewport_changed (self : not null access type_view'class);

-- 	function on_layout_changed (
-- 		self : not null access type_model'class;
-- 		call : not null access procedure (self : not null access gobject_record'class);
-- 		slot : access gobject_record'class := null)
-- 		return gtk.handlers.handler_id;
-- 	
-- 	function get_scale (self : not null access type_view) return type_scale;

	-- The cairo context to perform the actual drawing.
	-- NOTE: The final drawing is performed in the view (hence in view coordinates):
-- 	type type_draw_context is record
-- 		cr     : cairo.cairo_context := cairo.null_context;
-- 		layout : pango.layout.pango_layout := null; -- CS for displaying text. not used yet
-- 		view   : type_view_ptr := null;
-- 	end record;

-- 	procedure layout_changed (self : not null access type_model'class);

-- 	procedure set_transform (
-- 		self	: not null access type_view;
-- 		cr		: cairo.cairo_context);

	
-- CONVERSIONS BETWEEN COORDINATE SYSTEMS

-- 	function view_to_model (
-- 		self   : not null access type_view;
-- 		p      : in type_view_point) 
-- 		return type_model_point;
-- 	
-- 	-- Converts the given area of the view to a model rectangle:
-- 	function view_to_model (
-- 		self   : not null access type_view;
-- 		rect   : in type_view_rectangle) -- position and size are in pixels
-- 		return type_model_rectangle;
-- 
-- 	
-- 	-- Converts the given point in the model to a point in the view.
-- 	function model_to_view (
-- 		self   : not null access type_view;
-- 		p      : in type_model_point) -- position x/y given as a float type
-- 		return type_view_point;
-- 	
-- 	-- Converts the given area of the model to a view rectangle:	
-- 	function model_to_view (
-- 		self   : not null access type_view;
-- 		rect   : in type_model_rectangle) -- position x/y and size given as a float type
-- 		return type_view_rectangle;
-- 
	-- Converts a model point to a drawing point. 
	-- NOTE: The model point is in a coordinate system with y-axis
	-- going downwards. The drawing point is in a system where y-axis
	-- goes upwards. The origin of the drawing coordinate system is the
	-- lower left corner of the drawing frame.
	overriding function model_to_drawing (
		model		: not null access type_model;
		model_point : in type_model_point)
		return type_model_point;

	overriding function bounding_box (self : not null access type_model)
		return type_model_rectangle;


-- 	procedure set_adjustment_values (self : not null access type_view'class);	

-- 	function view_get_type return glib.gtype;
-- 	pragma convention (c, view_get_type);
-- 	--  return the internal type

	procedure gtk_new (
		self	: out type_view_ptr_sch;
		model	: access type_model'class := null);
	
-- 	procedure set_scale (
-- 		self     : not null access type_view;
-- 		scale    : in type_scale := scale_default;
-- 		preserve : in type_model_point := geometry.origin);
	-- Changes the scaling factor for self.
	-- this also scrolls the view so that either preserve or the current center
	-- of the view remains at the same location in the widget, as if the user
	-- was zooming towards that specific point.

-- 	function get_visible_area (self : not null access type_view) return type_model_rectangle;
	-- Return the area of the model (or the sheet) that is currently displayed in the view.
	-- This is in model coordinates (since the canvas coordinates are always
	-- from (0,0) to (self.get_allocation_width, self.get_allocation_height).

	
	
	-- This procedure should be called every time items are moved, added or removed.
	-- Call this procedure after having created after a view has been created for the model.
-- 	procedure refresh_layout (
-- 		self        : not null access type_model;
-- 		send_signal : boolean := true); -- sends "layout_changed" signal when true


-- 	grid_default : constant type_model_coordinate_positive := 10.0;
	
-- 	procedure set_grid_size (
-- 		self : not null access type_view'class;
-- 		size : in type_model_coordinate_positive := grid_default);

-- 	procedure draw_grid (
-- 		self    : not null access type_view'class;
-- 		style   : gtkada.style.drawing_style;
-- 		context : type_draw_context;
-- 		area    : type_model_rectangle);

	procedure draw_frame (
		model	: not null access type_model;
		in_area	: in type_model_rectangle := no_rectangle;
		context : in type_draw_context);

	procedure draw_nets (
		model	: not null access type_model;
		in_area	: in type_model_rectangle := no_rectangle;
		context : in type_draw_context);
	
	procedure draw_units (
		model	: not null access type_model;
		in_area	: in type_model_rectangle := no_rectangle;
		context : in type_draw_context);

	
	-- Redraw either the whole view, or a specific part of it only.
	-- The transformation matrix has already been set on the context.
	overriding procedure draw_internal (
		self    : not null access type_view_sch;
		context : type_draw_context;
		area    : type_model_rectangle);

-- 	procedure draw (
-- 		self    : not null access type_view;
-- 		context : type_draw_context;
-- 		area    : type_model_rectangle);

	
-- 	procedure scale_to_fit (
-- 		self      : not null access type_view;
-- 		rect      : in type_model_rectangle := no_rectangle;
-- 		min_scale : in type_scale := 1.0 / 4.0;
-- 		max_scale : in type_scale := 4.0);


	-- Assign the module to be edited to the model:
	procedure set_module (
		self	: not null access type_model;
		module	: in et_project.type_modules.cursor;
		sheet	: in et_coordinates.type_sheet := et_coordinates.type_sheet'first); -- the sheet to be opened

-- 	-- This function converts a x-value from the drawing to a x-value in the view.
-- 	function convert_x (x : in et_coordinates.type_distance) return type_view_coordinate;
-- 
-- 	-- This function converts a y-value from the drawing to a y-value in the view.	
-- 	function convert_y (y : in et_coordinates.type_distance) return type_view_coordinate renames convert_x;
-- 	
-- 	-- This function converts a x-value from the drawing to a x-value in the model.	
-- 	function convert_x (x : in et_coordinates.type_distance) return type_model_coordinate;
-- 
-- 	-- This function converts a y-value from the drawing to a y-value in the model.	
-- 	function convert_y (y : in et_coordinates.type_distance) return type_model_coordinate renames convert_x;
-- 

	-- This function converts a y-value from the drawing to a y-value in the view.
	-- The input y increases upwards. The output y increases downwards.
	overriding function convert_and_shift_y (
 		model	: not null access type_model;
		y		: in type_distance)
		return type_view_coordinate;

	-- This function converts a y-value from the drawing to a y-value in the model.
	-- The input y increases upwards. The output y increases downwards.
	overriding function convert_and_shift_y (
		model	: not null access type_model;
		y		: in type_distance) 
		return type_model_coordinate;

	
end et_canvas.schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
