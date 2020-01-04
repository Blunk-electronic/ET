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


package et_canvas.schematic is

	subtype type_model_coordinate is et_coordinates.type_distance;
	subtype type_model_point is et_coordinates.geometry.type_point; -- x/y only

-- 	subtype type_item_coordiante is et_coordinates.type_distance;
-- 	subtype type_item_point is et_coordinates.geometry.type_point;
	

	-- A rectangular area of the model:
	type type_model_rectangle is record
		x, y			: type_model_coordinate; -- position
		width, height	: type_model_coordinate; -- size
	end record;

	no_rectangle : constant type_model_rectangle := (0.0, 0.0, 0.0, 0.0);

	


-- MODEL
	type type_model is new glib.object.gobject_record with record
		layout	: pango.layout.pango_layout;
		module	: et_project.type_modules.cursor;
	end record;

	type type_model_ptr is access all type_model'class;
	

	model	: type_model_ptr;


	
	-- Creates a new model (or a drawing sheet according to the example above):
	procedure gtk_new (self : out type_model_ptr);

	-- Initializes the internal data so that the model can send signals:
	procedure init (self : not null access type_model'class);


-- VIEW
	
	-- The view (or canvas) displays a certain region of the model (or the sheet) 
	-- depending on scrolling or zoom.
	type type_view is new gtk.widget.gtk_widget_record with record
		model 		: type_model_ptr;

		-- The upper left corner of the visible area has its initial value at 0/0.
		-- NOTE: This has nothing to do with the upper left corner of the
		-- drawing sheet. topleft is not a constant and is changed on by procedure
		-- set_scale or by procedure scale_to_fit.
		topleft  	: type_model_point := geometry.origin; -- CS rename to topleft
		
		scale     	: gdouble := 1.0; -- gdouble is a real floating-point type (see glib.ads)
		grid_size 	: type_model_coordinate := 20.0;
		
		layout		: pango.layout.pango_layout; -- CS for displaying text. not used yet

		-- Required for the scrollbars:
		hadj, vadj	: gtk.adjustment.gtk_adjustment;

		-- connections to model signals
		id_layout_changed : gtk.handlers.handler_id := (gtk.handlers.null_handler_id, null);

		scale_to_fit_requested : gdouble := 0.0; -- gdouble is a real floating-point type (see glib.ads)
		scale_to_fit_area : type_model_rectangle;
	end record;

	-- The pointer to the canvas/view:
	type type_view_ptr is access all type_view'class;

	canvas	: type_view_ptr;
	scale	: gdouble := 1.0;
	
	procedure viewport_changed (self : not null access type_view'class);

	function on_layout_changed (
		self : not null access type_model'class;
		call : not null access procedure (self : not null access gobject_record'class);
		slot : access gobject_record'class := null)
		return gtk.handlers.handler_id;
	
	function get_scale (self : not null access type_view) return gdouble;

	-- The cairo context to perform the actual drawing.
	-- NOTE: The final drawing is performed in the view (hence in view coordinates):
	type type_draw_context is record
		cr     : cairo.cairo_context := cairo.null_context;
		layout : pango.layout.pango_layout := null; -- CS for displaying text. not used yet
		view   : type_view_ptr := null;
	end record;

	procedure layout_changed (self : not null access type_model'class);

	procedure set_transform (
		self	: not null access type_view;
		cr		: cairo.cairo_context);

	
-- CONVERSIONS BETWEEN COORDINATE SYSTEMS

	function view_to_model (
		self   : not null access type_view;
		p      : in type_view_point) 
		return type_model_point;
	
	-- Converts the given area of the view to a model rectangle:
	function view_to_model (
		self   : not null access type_view;
		rect   : in type_view_rectangle) -- position and size are in pixels
		return type_model_rectangle;

	
	-- Converts the given point in the model to a point in the view.
	function model_to_view (
		self   : not null access type_view;
		p      : in type_model_point) -- position x/y given as a float type
		return type_view_point;
	
	-- Converts the given area of the model to a view rectangle:	
	function model_to_view (
		self   : not null access type_view;
		rect   : in type_model_rectangle) -- position x/y and size given as a float type
		return type_view_rectangle;


	
	function bounding_box (
		self   : not null access type_model;
		margin : type_model_coordinate := 0.0)
		return type_model_rectangle;

	procedure set_adjustment_values (self : not null access type_view'class);	

	function view_get_type return glib.gtype;
	pragma convention (c, view_get_type);
	--  return the internal type

	procedure gtk_new (
		self	: out type_view_ptr;
		model	: access type_model'class := null);
	
	procedure set_scale (
		self     : not null access type_view;
		scale    : gdouble := 1.0;
		preserve : type_model_point := geometry.origin);
	-- Changes the scaling factor for self.
	-- this also scrolls the view so that either preserve or the current center
	-- of the view remains at the same location in the widget, as if the user
	-- was zooming towards that specific point.

	function get_visible_area (self : not null access type_view) return type_model_rectangle;
	-- Return the area of the model (or the sheet) that is currently displayed in the view.
	-- This is in model coordinates (since the canvas coordinates are always
	-- from (0,0) to (self.get_allocation_width, self.get_allocation_height).

	
	
	-- This procedure should be called every time items are moved, added or removed.
	-- Call this procedure after having created after a view has been created for the model.
	procedure refresh_layout (
		self        : not null access type_model;
		send_signal : boolean := true); -- sends "layout_changed" signal when true


	procedure set_grid_size (
		self : not null access type_view'class;
		size : type_model_coordinate := 30.0);

	procedure draw_grid_dots (
		self    : not null access type_view'class;
		style   : gtkada.style.drawing_style;
		context : type_draw_context;
		area    : type_model_rectangle);

	-- Redraw either the whole view, or a specific part of it only.
	-- The transformation matrix has already been set on the context.
	procedure draw_internal (
		self    : not null access type_view;
		context : type_draw_context;
		area    : type_model_rectangle);

	procedure scale_to_fit (
		self      : not null access type_view;
		rect      : type_model_rectangle := no_rectangle;
		min_scale : gdouble := 1.0 / 4.0;
		max_scale : gdouble := 4.0);
--

	-- Assign the module to be edited to the model:
	procedure set_module (
		model	: not null access type_model;
		module	: in et_project.type_modules.cursor);

	
end et_canvas.schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
