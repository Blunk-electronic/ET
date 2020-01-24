------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               CANVAS                                     --
--                                                                          --
--                               B o d y                                    --
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


with ada.text_io;			use ada.text_io;

with interfaces.c.strings;	use interfaces.c.strings;

with gtk.main;
with gtk.window; 			use gtk.window;
with gtk.widget;  			use gtk.widget;
with gtk.box;				use gtk.box;
with gtk.button;     		use gtk.button;
with gtk.toolbar; 			use gtk.toolbar;
with gtk.tool_button;		use gtk.tool_button;
with gtk.enums;				use gtk.enums;
with gtk.gentry;			use gtk.gentry;
with gtk.combo_box_text;	use gtk.combo_box_text;
with gtk.frame;				use gtk.frame;
with gtk.handlers;			use gtk.handlers;
with gtk.scrolled_window;	use gtk.scrolled_window;
with gtk.adjustment;		use gtk.adjustment;
with gtk.bin;				use gtk.bin;
with gtk.scrollable;		use gtk.scrollable;
with gtk.style_context;		use gtk.style_context;
with gtk.accel_group;

with glib.properties.creation;	use glib.properties.creation;
with cairo;					use cairo;
with gtkada.types;			use gtkada.types;
with gtkada.handlers;		use gtkada.handlers;
with gtkada.bindings;		use gtkada.bindings;
with gdk;					use gdk;
with gdk.window;			use gdk.window;
with gdk.window_attr;		use gdk.window_attr;
with gdk.event;				use gdk.event;
with gdk.types;				--use gdk.types;
with gdk.types.keysyms;

with gdk.rgba;
with pango.layout;					use pango.layout;
with system.storage_elements;		use system.storage_elements;

with ada.unchecked_deallocation;
with ada.containers;				use ada.containers;
with ada.containers.doubly_linked_lists;

with et_general;
with et_project;
with et_frames;
-- with et_coordinates;			use et_coordinates;
-- use et_coordinates.geometry;

package body et_canvas_2 is

package body pac_canvas is
	use geometry;

	function to_string (d : in gdouble) return string is begin
		return gdouble'image (d);
	end;

	function to_string (d : in gint) return string is begin
		return gint'image (d);
	end;
	
	function to_string (p : in type_view_point) return string is begin
		return ("view x/y [pixels]" & to_string (gint (p.x)) & "/" & to_string (gint (p.y)));
	end;

	function to_string (d : in type_model_coordinate) return string is begin
		return type_model_coordinate'image (d);
	end;
	
	function to_string (p : in type_model_point) return string is begin
		return ("model x/y [mm]" & to_string (p.x) & "/" & to_string (p.y));
	end;
		
	model_signals : constant gtkada.types.chars_ptr_array := (
		1 => new_string (string (signal_layout_changed))
		);
	
	view_signals : constant gtkada.types.chars_ptr_array := (
		1 => new_string (string (signal_viewport_changed))
		);

	h_adj_property    : constant property_id := 1;
	v_adj_property    : constant property_id := 2;
	h_scroll_property : constant property_id := 3;
	v_scroll_property : constant property_id := 4;

	model_class_record : glib.object.ada_gobject_class := glib.object.uninitialized_class;
	view_class_record : aliased glib.object.ada_gobject_class := glib.object.uninitialized_class;
	
	function model_get_type return glib.gtype is begin
		glib.object.initialize_class_record (
			ancestor     => gtype_object,
			signals      => model_signals,
			class_record => model_class_record,
			type_name    => "gtkada_model",
			parameters   => (
				1 => (1 => gtype_none)  	-- layout_changed
				)
			);  
		return model_class_record.the_type;
	end model_get_type;

	procedure union (
		rect1 : in out type_model_rectangle;
		rect2 : type_model_rectangle) is
		right : constant type_model_coordinate := 
			type_model_coordinate'max (rect1.x + rect1.width, rect2.x + rect2.width);
		bottom : constant type_model_coordinate :=
			type_model_coordinate'max (rect1.y + rect1.height, rect2.y + rect2.height);
	begin
		rect1.x := type_model_coordinate'min (rect1.x, rect2.x);
		rect1.width := right - rect1.x;

		rect1.y := type_model_coordinate'min (rect1.y, rect2.y);
		rect1.height := bottom - rect1.y;
	end;

	procedure set_transform (
		self	: not null access type_view;
		cr		: cairo.cairo_context)
	is
		model_p : type_model_point := origin;
		view_p  : type_view_point;
	begin
		-- compute a view point according to current model point:
		view_p := self.model_to_view (model_p);

		-- Set the CTM so that following draw operations are relative
		-- to the current view point:
		translate (cr, view_p.x, view_p.y);

		-- Set the CTM so that following draw operations are scaled
		-- according to the scale factor of the view:
		cairo.scale (cr, self.scale, self.scale);

	end set_transform;
	
	procedure refresh (
		self : not null access type_view'class;
		cr   : cairo.cairo_context;
		area : type_model_rectangle := no_rectangle)
	is
		a : type_model_rectangle;
		c : type_draw_context;
	begin
		if area = no_rectangle then
			a := self.get_visible_area;
		else
			a := area;
		end if;

		--  gdk already clears the exposed area to the background color, so
		--  we do not need to clear ourselves.

		c := (
			cr		=> cr,
			layout	=> self.layout,
			view	=> type_view_ptr (self));

		save (cr);
		self.set_transform (cr);
		self.draw_internal (c, a);
		restore (cr);
	end refresh;

-- 	function on_view_draw (
-- 		view	: system.address; 
-- 		cr		: cairo_context) return gboolean;
-- 	
-- 	pragma convention (c, on_view_draw);
-- 	--  default handler for "draw" on views.

	function on_view_draw (
		view	: system.address; 
		cr		: cairo_context) return gboolean is
		
		self : constant type_view_ptr := type_view_ptr (glib.object.convert (view));
		x1, y1, x2, y2 : gdouble;
	begin
		clip_extents (cr, x1, y1, x2, y2);

		if x2 < x1 or else y2 < y1 then
			refresh (self, cr);
		else
			refresh (self, cr, self.view_to_model ((x1, y1, x2 - x1, y2 - y1)));
		end if;

		return 1;

	exception
		when e : others =>
			process_exception (e);
			return 0;
	end on_view_draw;

-- 	procedure on_view_realize (widget : system.address);
-- 	pragma convention (c, on_view_realize);
-- 	--  called when the view is realized
	
	procedure on_view_realize (widget : system.address) is
		w          : constant gtk_widget := gtk_widget (get_user_data_or_null (widget));
		allocation : gtk_allocation;
		window     : gdk_window;
		attr       : gdk.window_attr.gdk_window_attr;
		mask       : gdk_window_attributes_type;
	begin
		if not w.get_has_window then
			inherited_realize (view_class_record, w);
		else
			w.set_realized (true);
			w.get_allocation (allocation);

			gdk_new (
				attr,
				window_type => gdk.window.window_child,
				x           => allocation.x,
				y           => allocation.y,
				width       => allocation.width,
				height      => allocation.height,
				wclass      => gdk.window.input_output,
				visual      => w.get_visual,
				event_mask  => w.get_events or exposure_mask);
			
			mask := wa_x or wa_y or wa_visual;

			gdk_new (window, w.get_parent_window, attr, mask);
			register_window (w, window);
			w.set_window (window);
			get_style_context (w).set_background (window);

			--  see also handler for size_allocate, which moves the window to its
			--  proper location.
		end if;
	end on_view_realize;


	
	function intersects (rect1, rect2 : type_model_rectangle) return boolean is begin
		return not (
			rect1.x > rect2.x + rect2.width            --  r1 on the right of r2
			or else rect2.x > rect1.x + rect1.width    --  r2 on the right of r1
			or else rect1.y > rect2.y + rect2.height   --  r1 below r2
			or else rect2.y > rect1.y + rect1.height); --  r1 above r2
	end intersects;

	
	procedure on_layout_changed_for_view (view : not null access gobject_record'class) is
		self  : constant type_view_ptr := type_view_ptr (view);
		alloc : gtk_allocation;
	begin
		self.get_allocation (alloc);

		--  on_adjustments_set will be called anyway when size_allocate is called
		--  so no need to call it now if the size is unknown yet.

		if alloc.width > 1 then
			set_adjustment_values (self);
			self.queue_draw;
		end if;

	end on_layout_changed_for_view;

	procedure viewport_changed (self : not null access type_view'class) is begin
		object_callback.emit_by_name (self, signal_viewport_changed);
	end viewport_changed;

	function on_layout_changed (
		self : not null access type_model'class;
		call : not null access procedure (self : not null access gobject_record'class);
		slot : access gobject_record'class := null)
		return gtk.handlers.handler_id is
	begin
		if slot = null then
			return object_callback.connect (
				self,
				signal_layout_changed,
				object_callback.to_marshaller (call));
		else
			return object_callback.object_connect (
				self,
				signal_layout_changed,
				object_callback.to_marshaller (call),
				slot);
		end if;
	end on_layout_changed;
	
	procedure set_model (
		self  : not null access type_view'class;
		model : access type_model'class) is
	begin
		if self.model = type_model_ptr (model) then
			return;
		end if;

		if self.model /= null then
			disconnect (self.model, self.id_layout_changed);
			unref (self.model);
		end if;

		self.model := type_model_ptr (model);

		if self.model /= null then
			ref (self.model);
			self.id_layout_changed := model.on_layout_changed (on_layout_changed_for_view'access, self);
		end if;

		if self.model /= null and then self.model.layout = null then
			self.model.layout := self.layout;  --  needed for layout
			ref (self.model.layout);
			self.model.refresh_layout;
		else
			set_adjustment_values (self);
			self.queue_draw;
		end if;

		self.viewport_changed;
	end set_model;
	

	procedure gtk_new (self : out type_model_ptr) is begin
		self := new type_model;
		init (self);
	end;	

	procedure init (self : not null access type_model'class) is begin
		if not self.is_created then
			g_new (self, model_get_type);
		end if;
	end;

	
	function get_scale (self : not null access type_view) return type_scale is
	begin
		return self.scale;
	end get_scale;

	
	procedure layout_changed (self : not null access type_model'class) is begin
		object_callback.emit_by_name (self, signal_layout_changed);
	end layout_changed;



	
-- CONVERSIONS BETWEEN COORDINATE SYSTEMS
	
	function view_to_model (
		self   : not null access type_view;
		p      : in type_view_point) 
		return type_model_point is
	begin
-- 		return type_model_point (set (
-- 			x	=> type_model_coordinate (p.x / self.scale) + self.topleft.x,
-- 			y	=> type_model_coordinate (p.y / self.scale) + self.topleft.y
-- 			));
		return (
			x	=> type_model_coordinate (p.x / self.scale) + self.topleft.x,
			y	=> type_model_coordinate (p.y / self.scale) + self.topleft.y
			);
	end view_to_model;

	function view_to_model (
		self   : not null access type_view;
		rect   : in type_view_rectangle) -- position and size are in pixels
		return type_model_rectangle is
	begin
		return (x      => type_model_coordinate (rect.x / self.scale) + self.topleft.x,
				y      => type_model_coordinate (rect.y / self.scale) + self.topleft.y,
				width  => type_model_coordinate (rect.width / self.scale),
				height => type_model_coordinate (rect.height / self.scale));
	end view_to_model;

	
	function model_to_view (
		self   : not null access type_view;
		p      : in type_model_point) 
		return type_view_point is
	begin
		return (
			x => type_view_coordinate (p.x - self.topleft.x) * self.scale,
			y => type_view_coordinate (p.y - self.topleft.y) * self.scale
			);
	end model_to_view;

	function model_to_view (
		self   : not null access type_view;
		rect   : in type_model_rectangle)
		return type_view_rectangle is
		result : type_view_rectangle;
	begin
		result := (
			x      => type_view_coordinate (rect.x - self.topleft.x) * self.scale,
			y      => type_view_coordinate (rect.y - self.topleft.y) * self.scale,
			width  => type_view_coordinate (rect.width) * self.scale,
			height => type_view_coordinate (rect.height) * self.scale
			);
		
		return result;
	end model_to_view;

	function model_to_drawing (
		model		: not null access type_model;
		model_point : in type_model_point)
		return type_model_point is
	begin
		return origin;
	end;

	function bounding_box (self : not null access type_model)
		return type_model_rectangle is
	begin
		return no_rectangle;
	end;

	procedure set_adjustment_values (self : not null access type_view'class) is
		box   : type_model_rectangle;
		area  : constant type_model_rectangle := self.get_visible_area;
		min, max : gdouble;
	begin
		if self.model = null or else area.width <= 1.0 then
			--  not allocated yet
			return;
		end if;

		-- The bounding box of the whole model is the bounding box of the drawing sheet
		-- which seems sufficient for now.
		--box := self.model.paper_bounding_box;
		box := self.model.bounding_box;

		--  we set the adjustments to include the model area, but also at least
		--  the current visible area (if we don't, then part of the display will
		--  not be properly refreshed).

		if self.hadj /= null then
			min := gdouble'min (gdouble (area.x), gdouble (box.x));
			max := gdouble'max (gdouble (area.x + area.width), gdouble (box.x + box.width));
			self.hadj.configure (
				value          => gdouble (area.x),
				lower          => min,
				upper          => max,
				step_increment => 5.0,
				page_increment => 100.0,
				page_size      => gdouble (area.width));
		end if;

		if self.vadj /= null then
			min := gdouble'min (gdouble (area.y), gdouble (box.y));
			max := gdouble'max (gdouble (area.y + area.height), gdouble (box.y + box.height));
			self.vadj.configure (
				value          => gdouble (area.y),
				lower          => min,
				upper          => max,
				step_increment => 5.0,
				page_increment => 100.0,
				page_size      => gdouble (area.height));
		end if;

		self.viewport_changed;
	end set_adjustment_values;

	procedure on_adj_value_changed (view : access glib.object.gobject_record'class) is
	-- Called when one of the scrollbars has changed value.		
		self : constant type_view_ptr := type_view_ptr (view);
-- 		pos  : constant type_model_point := type_model_point (set (
-- 							x => type_model_coordinate (self.hadj.get_value),
-- 							y => type_model_coordinate (self.vadj.get_value)));
		pos  : constant type_model_point := (
							x => type_model_coordinate (self.hadj.get_value),
							y => type_model_coordinate (self.vadj.get_value));

	begin
		if pos /= self.topleft then
			self.topleft := pos;
			self.viewport_changed;
			queue_draw (self);
		end if;
	end on_adj_value_changed;

	procedure view_set_property (
		object        : access glib.object.gobject_record'class;
		prop_id       : property_id;
		value         : glib.values.gvalue;
		property_spec : param_spec)
	is
		pragma unreferenced (property_spec);
		self : constant type_view_ptr := type_view_ptr (object);
	begin
		case prop_id is
			when h_adj_property =>
				self.hadj := gtk_adjustment (get_object (value));
				if self.hadj /= null then
					set_adjustment_values (self);
					--self.hadj.on_value_changed (on_adj_value_changed'access, self);
					self.hadj.on_value_changed (access_on_adj_value_changed, self);
					self.queue_draw;
				end if;

			when v_adj_property => 

				self.vadj := gtk_adjustment (get_object (value));

				if self.vadj /= null then
					set_adjustment_values (self);
					--self.vadj.on_value_changed (on_adj_value_changed'access, self);
					self.vadj.on_value_changed (access_on_adj_value_changed, self);
					self.queue_draw;
				end if;

			when h_scroll_property => null;

			when v_scroll_property => null;

			when others => null;
		end case;
	end view_set_property;

	procedure view_get_property (
		object        : access glib.object.gobject_record'class;
		prop_id       : property_id;
		value         : out glib.values.gvalue;
		property_spec : param_spec)
	is
		pragma unreferenced (property_spec);
		self : constant type_view_ptr := type_view_ptr (object);
	begin
		case prop_id is
			when h_adj_property => set_object (value, self.hadj);
			when v_adj_property => set_object (value, self.vadj);
			when h_scroll_property => set_enum (value, gtk_policy_type'pos (policy_automatic));
			when v_scroll_property => set_enum (value, gtk_policy_type'pos (policy_automatic));
			when others => null;
		end case;
	end view_get_property;
	
-- 	procedure on_size_allocate (view : system.address; alloc : gtk_allocation);
-- 	pragma convention (c, on_size_allocate);
-- 	--  default handler for "size_allocate" on views.
	
	procedure on_size_allocate (view : system.address; alloc : gtk_allocation) is
		self : constant type_view_ptr := type_view_ptr (glib.object.convert (view));
		salloc : gtk_allocation := alloc;
	begin
		--  for some reason, when we maximize the toplevel window in testgtk, or
		--  at least enlarge it horizontally, we are starting to see an alloc
		--  with x < 0 (likely related to the gtkpaned). the drawing area then
		--  moves the gdkwindow, which would introduce an extra ofset in the
		--  display (and influence the clipping done automatically by gtk+
		--  before it emits "draw"). so we prevent the automatic offseting done
		--  by gtkdrawingarea.

		salloc.x := 0;
		salloc.y := 0;
		self.set_allocation (salloc);
		set_adjustment_values (self);

		if self.get_realized then
			if self.get_has_window then
				move_resize (self.get_window, alloc.x, alloc.y, alloc.width, alloc.height);
			end if;

			--  send_configure event ?
		end if;

		if self.scale_to_fit_requested /= 0.0 then
			
			self.scale_to_fit (
				rect      => self.scale_to_fit_area,
				max_scale => self.scale_to_fit_requested);
			
		end if;
	end on_size_allocate;

-- 	procedure view_class_init (self : gobject_class);
-- 	pragma convention (c, view_class_init);
	
	procedure view_class_init (self : gobject_class) is begin
		--set_properties_handlers (self, view_set_property'access, view_get_property'access);
		set_properties_handlers (self, access_view_set_property, access_view_get_property);

		override_property (self, h_adj_property, "hadjustment");
		override_property (self, v_adj_property, "vadjustment");
		override_property (self, h_scroll_property, "hscroll-policy");
		override_property (self, v_scroll_property, "vscroll-policy");

		--set_default_draw_handler (self, on_view_draw'access);
		set_default_draw_handler (self, access_on_view_draw);
		--set_default_size_allocate_handler (self, on_size_allocate'access);
		set_default_size_allocate_handler (self, access_on_size_allocate);
		--set_default_realize_handler (self, on_view_realize'access);
		set_default_realize_handler (self, access_on_view_realize);
	end;

	function view_get_type return glib.gtype is
		info : access ginterface_info;
	begin
		if glib.object.initialize_class_record (
			ancestor     => gtk.bin.get_type,
			signals      => view_signals,
			class_record => view_class_record'access,
			type_name    => "GtkadaCanvasView",
			parameters   => (
				1 => (1 => gtype_none)
				),
			returns      => (1 => gtype_none, 2 => gtype_boolean),
			--class_init   => view_class_init'access
			class_init   => access_view_class_init
			)
		then
			info := new ginterface_info' (
				interface_init     => null,
				interface_finalize => null,
				interface_data     => system.null_address);
				glib.object.add_interface (
					view_class_record,
					iface => gtk.scrollable.get_type,
					info  => info
				);
		end if;

		return view_class_record.the_type;
	end view_get_type;


	-- For demonstrating the difference between view coordinates (pixels) and model coordinates
	-- this function outputs them at the console.
	function on_mouse_movement (
		view  : access gtk_widget_record'class;
		event : gdk_event_motion) return boolean is
		
		-- the point where the mouse pointer is pointing at
		view_point : type_view_point;

		-- The conversion from view to model coordinates requires a pointer to
		-- the view. This command sets self so that it points to the view:
		self : constant type_view_ptr := type_view_ptr (view);

		-- The point in the model (or on the sheet) expressed in millimeters:
		model_point : type_model_point;

		drawing_point : type_model_point;
	begin
		new_line;
		put_line ("mouse movement ! new positions are:");

		-- Fetch the position of the mouse pointer and output it on the console:
		view_point := (x => event.x, y => event.y);
		put_line (" " & to_string (view_point));

		-- Convert the view point (pixels) to the position (millimeters) in the model
		-- and output in on the console:
		model_point := self.view_to_model (view_point);
		put_line (" model " & to_string (model_point));

		--drawing_point := model_to_drawing (canvas.model, model_point);
		drawing_point := model_to_drawing (self.model, model_point);
		put_line (" drawing " & to_string (drawing_point));
		
		return true; -- indicates that event has been handled
	end on_mouse_movement;

-- 	procedure center_on (
-- 		self         : not null access type_view;
-- 		center_on    : type_model_point;
-- 		x_pos, y_pos : gdouble := 0.5)
-- 	is
-- 		area : constant type_model_rectangle := self.get_visible_area;
-- 		pos  : constant type_model_point := (
-- 			center_on.x - area.width * x_pos,
-- 			center_on.y - area.height * y_pos);
-- 	begin
-- 		self.scale_to_fit_requested := 0.0;
-- 
-- 		self.topleft := pos;
-- 		self.set_adjustment_values;
-- 		self.queue_draw;
-- 
-- 	end center_on;

	
	function on_scroll_event (
		view	: access gtk_widget_record'class;
		event	: gdk_event_scroll) return boolean is

		result : boolean := false; -- to be returned
		-- When true, no other handler will process the event.

		procedure event_handled is begin result := true; end;
		procedure event_not_handled is begin result := false; end;
		
		use gdk.types;
		use gdk.types.keysyms;
		use gtk.accel_group;

		-- Provides information on pressed keys:
		accel_mask : gdk_modifier_type := get_default_mod_mask;

		-- The amount of wheel rotation. We are interested in
		-- its sign only. Negative means zooming in, positive means zooming out.
		dy : gdouble := event.delta_y;
		
		self    : constant type_view_ptr := type_view_ptr (view);

		-- Get the current scale:
		scale	: type_scale := get_scale (self);

		-- The point at which the zooming takes place:
		point	: type_model_point;
		
	begin -- on_scroll_event
		if self.model /= null then
			--new_line;
			--put_line ("scroll detected");

			-- If CTRL is being pressed, zoom in our out depending on dy:
			if (event.state and accel_mask) = control_mask then

				-- Get the center of the zooming operation:
				point := view_to_model (self, (event.x, event.y));
				
				-- CS: Testing event.direction would be more useful 
				-- but for some reason always returns SMOOTH_SCROLL.
				if dy > 0.0 then
					put_line ("zoom out");
					set_scale (self, scale - scale_delta_on_zoom, point);
					event_handled;
				else
					put_line ("zoom in");
					set_scale (self, scale + scale_delta_on_zoom, point);
					event_handled;
				end if;
						
			end if;
			
		end if;

		-- CS: exception handler if scale range check fails

		return result;
	end on_scroll_event;
	
	function on_key_pressed_event (
		view  : access gtk_widget_record'class;
		event : gdk_event_key) return boolean is

		use gdk.types;
		use gdk.types.keysyms;
		
		self    : constant type_view_ptr := type_view_ptr (view);

-- 		key_ctrl : gdk_modifier_type := event.state and control_mask;
		key : gdk_key_type := event.keyval;
	begin
		--put_line ("key pressed");
		
		if self.model /= null then
			new_line;

			put_line (gdk_key_type'image (key));

			case key is
				when GDK_Control_L | GDK_Control_R =>
					put_line ("ctrl pressed");

				when others => 
					put_line ("other key pressed");
			end case;
		
		end if;
		
		return true; -- indicates that event has been handled
	end on_key_pressed_event;

	function on_key_released_event (
		view  : access gtk_widget_record'class;
		event : gdk_event_key) return boolean is

		use gdk.types;
		use gdk.types.keysyms;
		
		self    : constant type_view_ptr := type_view_ptr (view);

-- 		key_ctrl : gdk_modifier_type := event.state and control_mask;
		key : gdk_key_type := event.keyval;
	begin
		--put_line ("key pressed");
		
		if self.model /= null then
			new_line;

			put_line (gdk_key_type'image (key));

			case key is
				when GDK_Control_L | GDK_Control_R =>
					put_line ("ctrl released");

				when others => 
					put_line ("other key released");
			end case;
		
		end if;
		
		return true; -- indicates that event has been handled
	end on_key_released_event;
	
	function on_button_event (
		view  : access gtk_widget_record'class;
		event : gdk_event_button)
		return boolean
	is
		self    : constant type_view_ptr := type_view_ptr (view);
	begin
		put_line ("mouse button pressed");

		return true; -- indicates that event has been handled
	end on_button_event;
	
	procedure init (
		self  : not null access type_view'class;
		model : access type_model'class := null) is
	begin
		g_new (self, view_get_type);
		self.layout := self.create_pango_layout;
		self.set_has_window (true);

		-- These are the signals the view is to receive from input devices
		-- like keyboard, mouse or touchpad:
		self.add_events (
			scroll_mask or smooth_scroll_mask or touch_mask
				or button_press_mask or button_release_mask
				or button1_motion_mask
				or button2_motion_mask
				or button3_motion_mask
				or pointer_motion_mask -- whenever the mouse is being moved inside the canvas
				-- key_press_mask -- no need
			);

		-- reaction to mouse movements in the canvas
		--self.on_motion_notify_event (on_mouse_movement'access);
		self.on_motion_notify_event (access_on_mouse_movement);

		-- reaction to mouse wheel being rotated
		--self.on_scroll_event (on_scroll_event'access);
		self.on_scroll_event (access_on_scroll_event);

		-- reaction to mouse buttons pressed
		--self.on_button_press_event (on_button_event'access);
		self.on_button_press_event (access_on_button_event);

		-- reaction to keys pressed on the keyboard		
		--self.on_key_press_event (on_key_pressed_event'access);
		self.on_key_press_event (access_on_key_pressed_event);

		-- reaction to keys released on the keyboard		
		--self.on_key_release_event (on_key_released_event'access);
		self.on_key_release_event (access_on_key_released_event);
		
		self.set_can_focus (true);

		self.set_model (model);
	end init;

	procedure gtk_new (
		self	: out type_view_ptr;
		model	: access type_model'class := null) is 
	begin
		self := new type_view;
		init (self, model);
	end;

	
	procedure set_scale (
		self     : not null access type_view;
		scale    : in type_scale := scale_default;
		--preserve : in type_model_point := geometry.origin)
		preserve : in type_model_point := origin)
	is
		-- backup the current scale
		old_scale : constant type_scale := self.scale;
		
		box : type_model_rectangle;
		p   : type_model_point;
	begin
		if preserve /= origin then
			-- set p at the point given by preserve
			p := preserve;
		else
			-- get the visible area
			box := self.get_visible_area;

			-- set p at the center of the visible area
			--p := type_model_point (set (box.x + box.width / 2.0, box.y + box.height / 2.0));
			p := (x => box.x + box.width / 2.0, y => box.y + box.height / 2.0);
		end if;

		self.scale := scale;

		-- calculate the new topleft corner of the visible area:
-- 		self.topleft := type_model_point (set (
-- 			p.x - (p.x - self.topleft.x) * type_model_coordinate (old_scale / scale),
-- 			p.y - (p.y - self.topleft.y) * type_model_coordinate (old_scale / scale)));

		self.topleft := (
			p.x - (p.x - self.topleft.x) * type_model_coordinate (old_scale / scale),
			p.y - (p.y - self.topleft.y) * type_model_coordinate (old_scale / scale));
		
		self.scale_to_fit_requested := 0.0;
		self.set_adjustment_values;
		self.queue_draw;
	end set_scale;

	function get_visible_area (self : not null access type_view)
		return type_model_rectangle is
	begin
		return self.view_to_model (
			-- Assemble a type_view_rectangle which will be converted
			-- to a type_model_rectangle by function view_to_model.
			(
			-- The visible area of the view always starts at 0/0 (topleft corner):
			x		=> 0.0, 
			y		=> 0.0,

			-- The view size is adjusted by the operator. So it must be inquired
			-- by calling get_allocated_width and get_allocated_height.
			-- get_allocated_width and get_allocated_height return an integer type
			-- which corresponds to the number of pixels required by self in y and x
			-- direction. Since the model coordinates are gdouble (a float type),
			-- the number of pixels must be converted to a gdouble type:
			width	=> gdouble (self.get_allocated_width),
			height	=> gdouble (self.get_allocated_height)
			));
	end get_visible_area;

	procedure refresh_layout (
		self        : not null access type_model;
		send_signal : boolean := true) is
	begin
		-- Update the width and height of all items:
		
		-- CS no need for size request. All items will have properties width and heigth.

		if send_signal then
			type_model'class (self.all).layout_changed;
		end if;
	end refresh_layout;


	procedure set_grid_size (
		self : not null access type_view'class;
		size : in type_model_coordinate_positive := grid_default) is
	begin
		self.grid_size := size;
	end set_grid_size;

	
	procedure draw_internal (
		self    : not null access type_view;
		context : type_draw_context;
		area    : type_model_rectangle) 
	is
		-- prepare draing style so that white grid dots will be drawn.
		style : drawing_style := gtk_new (stroke => gdk.rgba.white_rgba);
		
	begin
		put_line ("draw internal gen ...");
		
		if self.model /= null then

			-- draw a black background:
			set_source_rgb (context.cr, 0.0, 0.0, 0.0);
			paint (context.cr);

			-- draw white grid dots:
			set_grid_size (self, grid_default);
-- 			draw_grid (self, style, context, area);

-- 			self.model.draw_frame (area, context); -- separate unit
-- 			self.model.draw_nets (area, context); -- separate unit
-- 			self.model.draw_units (area, context); -- separate unit
			-- CS self.model.draw_texts (area, context);
			-- CS self.model.draw_submodules (area, context);
			
		end if;
	end draw_internal;

	procedure scale_to_fit (
		self      : not null access type_view;
		rect      : in type_model_rectangle := no_rectangle;
		min_scale : in type_scale := 1.0 / 4.0;
		max_scale : in type_scale := 4.0)
	is
		box     : type_model_rectangle;
		w, h, s : gdouble;
		alloc   : gtk_allocation;
		tl      : type_model_point;
		wmin, hmin : gdouble;
	begin
		put_line ("scale to fit ...");
		self.get_allocation (alloc);
		if alloc.width <= 1 then
			self.scale_to_fit_requested := max_scale;
			self.scale_to_fit_area := rect;

		elsif self.model /= null then
			self.scale_to_fit_requested := 0.0;
			
			if rect = no_rectangle then
				--box := self.model.paper_bounding_box;
				box := self.model.bounding_box;
			else
				box := rect;
			end if;

			if box.width /= 0.0 and then box.height /= 0.0 then
						  
				w := gdouble (alloc.width);
				h := gdouble (alloc.height);

				--  the "-1.0" below compensates for rounding errors, since
				--  otherwise we are still seeing the scrollbar along the axis
				--  used to compute the scale.
				wmin := (w - 2.0 * view_margin - 1.0) / type_view_coordinate (box.width);
				hmin := (h - 2.0 * view_margin - 1.0) / type_view_coordinate (box.height);
				wmin := gdouble'min (wmin, hmin);
				s := gdouble'min (max_scale, wmin);
				s := gdouble'max (min_scale, s);

				-- calculate the new topleft corner of the visible area:
-- 				tl := type_model_point (set (
-- 					x	=> box.x - (type_model_coordinate (w / s) - box.width) / 2.0,
-- 					y	=> box.y - (type_model_coordinate (h / s) - box.height) / 2.0)
-- 					);

				tl := (
					x	=> box.x - (type_model_coordinate (w / s) - box.width) / 2.0,
					y	=> box.y - (type_model_coordinate (h / s) - box.height) / 2.0
					);
				
				self.scale := s;
				self.topleft := tl;
				self.set_adjustment_values;
				self.queue_draw;

			end if;
		end if;
	end scale_to_fit;

-- 
-- 	procedure set_module (
-- 		self	: not null access type_model;
-- 		module	: in et_project.type_modules.cursor;
-- 		sheet	: in et_coordinates.type_sheet := et_coordinates.type_sheet'first) -- the sheet to be opened
-- 	is
-- 		use et_general;
-- 		use et_frames;
-- 		use et_project;
-- 		
-- 	begin 
-- 		self.module := module;
-- 
-- 		-- set some variables frequently used regarding frame and paper:
-- 		self.frame := type_modules.element (model.module).frames.frame;
-- 		
-- 		self.paper_height := type_model_coordinate (paper_dimension (
-- 							paper_size	=> self.frame.paper,
-- 							orientation	=> self.frame.orientation,
-- 							axis		=> Y));
-- 
-- 		self.paper_width := type_model_coordinate (paper_dimension (
-- 							paper_size	=> self.frame.paper,
-- 							orientation	=> self.frame.orientation,
-- 							axis		=> X));
-- 
-- 		-- The drawing frame has a bounding box:
-- 
-- 		-- position (upper left corner):
-- 		self.frame_bounding_box.x := (self.paper_width - type_model_coordinate (self.frame.size.x)) / 2.0;
-- 		self.frame_bounding_box.y := (self.paper_height - type_model_coordinate (self.frame.size.y)) / 2.0;
-- 
-- 		-- width and height
-- 		self.frame_bounding_box.width := type_model_coordinate (self.frame.size.x);
-- 		self.frame_bounding_box.height := type_model_coordinate (self.frame.size.y);
-- 
-- 		-- The sheet has a drawing box:
-- 		self.paper_bounding_box := (0.0, 0.0, self.paper_width, self.paper_height);
-- 
-- 		-- Drawing of the title block items is relative to the title block position:
-- 		self.title_block_position := self.frame.title_block_schematic.position;
-- 
-- 		-- set active sheet
-- 		self.sheet := sheet;
-- 	end set_module;

	
	function convert_x (x : in type_distance) return type_view_coordinate is begin
		return type_view_coordinate (
			type_model_coordinate (x)
			);
	end;

	function convert_x (x : in type_distance) return type_model_coordinate is begin
		return type_model_coordinate (x);
	end;

	function convert_and_shift_y (
		model	: not null access type_model;
		y		: in type_distance)
		return type_view_coordinate is 
	begin
		return type_view_coordinate 
			(
			--model.frame_bounding_box.height 
			- type_model_coordinate (y)
			);
	end;
		
	function convert_and_shift_y (
		model	: not null access type_model;
		y		: in type_distance)
		return type_model_coordinate is 
	begin
		return (
			--model.frame_bounding_box.height 
			- type_model_coordinate (y)
			);
	end;

end pac_canvas;
	
end et_canvas_2;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
