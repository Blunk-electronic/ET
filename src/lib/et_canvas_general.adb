------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            CANVAS GENERAL                                --
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

with ada.text_io;				use ada.text_io;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings;				use ada.strings;
with ada.strings.fixed;			use ada.strings.fixed;

with interfaces.c.strings;		use interfaces.c.strings;

with gtk.bin;					use gtk.bin;
with gtk.scrollable;			use gtk.scrollable;
with gtk.style_context;			use gtk.style_context;
with gtk.accel_group;

with gtkada.types;				use gtkada.types;
with gtkada.handlers;			use gtkada.handlers;
with gtkada.bindings;			use gtkada.bindings;

with gdk.window;				use gdk.window;
with gdk.window_attr;			use gdk.window_attr;
with gdk.types.keysyms;

with et_project;

package body et_canvas_general is

package body pac_canvas is
	
	procedure build_background_boxes is begin
		-- background box
		gtk_new_hbox (box_back);
		set_spacing (box_back, 10);
		add (window, box_back);

		-- left box
		gtk_new_vbox (box_left);
		set_spacing (box_left, 10);
		pack_start (box_back, box_left, expand => false);

		-- right box
		gtk_new_vbox (box_right);
		set_spacing (box_right, 10);
		add (box_back, box_right);
	end build_background_boxes;

	procedure zoom_to_fit (self : access glib.object.gobject_record'class) is 
	begin
		put_line ("zoom to fit ...");
		scale_to_fit (canvas);
-- 		put_line (to_string (get_scale (canvas)));
	end;
	
	procedure build_toolbars is
		spacing : gint;
	begin
		spacing := 10;
		
		-- The main box around all toolbars:
		gtk_new_hbox (box_toolbars);
		set_spacing (box_toolbars, spacing);
		set_border_width (box_toolbars, 10);
		pack_start (box_left, box_toolbars, expand => false);
		
		----------------------------------------------------------
		-- TOOLBAR ON THE LEFT
		gtk_new (toolbar_left);
		set_orientation (toolbar_left, orientation_vertical);
		pack_start (box_toolbars, toolbar_left, expand => false);

		-- Create a button and place it in the toolbar:
		gtk_new (button_zoom_to_fit, label => "FIT");
		insert (toolbar_left, button_zoom_to_fit);

		----------------------------------------------------------
		-- TOOLBAR ON THE RIGHT
		gtk_new (toolbar_right);
		set_orientation (toolbar_right, orientation_vertical);
		pack_start (box_toolbars, toolbar_right, expand => false);

		-- Create a button and place it in the toolbar:
		gtk_new (button_demo, label => "DEMO");
		insert (toolbar_right, button_demo);
		

		
	end build_toolbars;
	
	procedure build_coordinates_display is 
		spacing : gint;
	begin
		spacing := 10;
		
		-- The main box around all kinds of position readouts:
		gtk_new_vbox (box_positions);
		set_spacing (box_positions, spacing);
		set_border_width (box_positions, 10);
		pack_start (box_left, box_positions, expand => false);

		-- The box for mouse pointer:
		gtk_new_vbox (box_mouse_position);
		pack_start (box_positions, box_mouse_position, expand => false);
		gtk_new (label_mouse_position, "pointer");
		pack_start (box_mouse_position, label_mouse_position, expand => false);

		-- X
		spacing := 30;
		gtk_new_hbox (box_mouse_position_x);
		set_spacing (box_mouse_position_x, spacing);
		pack_start (box_mouse_position, box_mouse_position_x, expand => false);
		gtk_new (label_mouse_position_x, "X");
		pack_start (box_mouse_position_x, label_mouse_position_x, expand => false);
		gtk_new_with_entry (mouse_position_x);
		pack_start (box_mouse_position_x, mouse_position_x, expand => false);

		-- Y
		gtk_new_hbox (box_mouse_position_y);
		set_spacing (box_mouse_position_y, spacing);
		pack_start (box_mouse_position, box_mouse_position_y, expand => false);
		gtk_new (label_mouse_position_y, "Y");
		pack_start (box_mouse_position_y, label_mouse_position_y, expand => false);
		gtk_new_with_entry (mouse_position_y);
		pack_start (box_mouse_position_y, mouse_position_y, expand => false);


		-- The box for cursor position:
		gtk_new_vbox (box_cursor_position);
		pack_start (box_positions, box_cursor_position, expand => false);
		gtk_new (label_cursor_position, "cursor");
		pack_start (box_cursor_position, label_cursor_position, expand => false);

		-- X
		gtk_new_hbox (box_cursor_position_x);
		set_spacing (box_cursor_position_x, spacing);
		pack_start (box_cursor_position, box_cursor_position_x, expand => false);
		gtk_new (label_cursor_position_x, "X");
		pack_start (box_cursor_position_x, label_cursor_position_x, expand => false);
		gtk_new_with_entry (cursor_position_x);
		pack_start (box_cursor_position_x, cursor_position_x, expand => false);

		-- Y
		gtk_new_hbox (box_cursor_position_y);
		set_spacing (box_cursor_position_y, spacing);
		pack_start (box_cursor_position, box_cursor_position_y, expand => false);
		gtk_new (label_cursor_position_y, "Y");
		pack_start (box_cursor_position_y, label_cursor_position_y, expand => false);
		gtk_new_with_entry (cursor_position_y);
		pack_start (box_cursor_position_y, cursor_position_y, expand => false);


		
		-- The box for distances:
		gtk_new_vbox (distances.box);
		pack_start (box_positions, distances.box, expand => false);
		gtk_new (distances.label, "cursor to pointer");
		pack_start (distances.box, distances.label, expand => false);

		-- dX
		spacing := 20;
		gtk_new_hbox (distances.box_x);
		set_spacing (distances.box_x, spacing);
		pack_start (distances.box, distances.box_x, expand => false);
		gtk_new (distances.label_x, "dX");
		pack_start (distances.box_x, distances.label_x, expand => true);
		gtk_new_with_entry (distances.display_x);
		pack_start (distances.box_x, distances.display_x, expand => true);

		-- dY
		gtk_new_hbox (distances.box_y);
		set_spacing (distances.box_y, spacing);
		pack_start (distances.box, distances.box_y, expand => false);
		gtk_new (distances.label_y, "dY");
		pack_start (distances.box_y, distances.label_y, expand => true);
		gtk_new_with_entry (distances.display_y);
		pack_start (distances.box_y, distances.display_y, expand => true);

		-- ABSOLUTE
		spacing := 10;
		gtk_new_hbox (distances.box_abs);
		set_spacing (distances.box_abs, spacing);
		pack_start (distances.box, distances.box_abs, expand => false);
		gtk_new (distances.label_abs, "ABS");
		pack_start (distances.box_abs, distances.label_abs, expand => true);
		gtk_new_with_entry (distances.display_abs);
		pack_start (distances.box_abs, distances.display_abs, expand => true);

		-- ANGLE
		gtk_new_hbox (distances.box_angle);
		set_spacing (distances.box_angle, spacing);
		pack_start (distances.box, distances.box_angle, expand => false);
		gtk_new (distances.label_angle, "DEG");
		pack_start (distances.box_angle, distances.label_angle, expand => true);
		gtk_new_with_entry (distances.display_angle);
		pack_start (distances.box_angle, distances.display_angle, expand => false);
		
	end build_coordinates_display;

	procedure update_coordinates_display (
		self	: not null access type_view'class)
	is 
		use et_general;

		-- The x/y position of the mouse pointer:
		position_pointer_x : gint;
		position_pointer_y : gint;

		-- The point in the view (in pixels):
		view_point : type_view_point;

		-- The point in the model (in millimeters):
		model_point : type_point;

		-- The point in the drawing (in millimeters):
		drawing_point : type_point;

		-- The distance in x,y from cursor to mouse pointer:
		distance_xy : type_point;

		-- The distance in polar coordinates from cursor to mouse pointer:
		distance_pol : type_distance_polar;
	begin
		-- Get the mouse position:
		self.get_pointer (position_pointer_x, position_pointer_y);

		-- Convert mouse position to view_point:
		view_point.x := type_view_coordinate (position_pointer_x);
		view_point.y := type_view_coordinate (position_pointer_y);

		-- Convert the view_point to a model_point:
		model_point := self.view_to_model (view_point);

		-- Convert the model_point to the point in the drawing:
		drawing_point := model_to_drawing (self, model_point);

		-- Get the distance (in x and y) from cursor to mouse position:
		distance_xy := type_point (distance_relative (cursor_main.position, drawing_point));

		-- Get the distance (in x and y) from cursor to mouse position:
		distance_pol := distance_polar (cursor_main.position, drawing_point);

		
		-- update distance display:
		gtk_entry (distances.display_x.get_child).set_text (to_string (self, distance_xy, X));
		gtk_entry (distances.display_y.get_child).set_text (to_string (self, distance_xy, Y));
		gtk_entry (distances.display_abs.get_child).set_text (to_string (distance_pol.absolute));
		gtk_entry (distances.display_angle.get_child).set_text (to_string (distance_pol.angle));

		-- update cursor position
		gtk_entry (cursor_position_x.get_child).set_text (trim (to_string (x (cursor_main.position)), left));
		gtk_entry (cursor_position_y.get_child).set_text (trim (to_string (y (cursor_main.position)), left));

	end update_coordinates_display;

	procedure build_console is begin
		-- box for console on the right top
		gtk_new_vbox (box_console);
		set_spacing (box_console, 10);
		pack_start (box_right, box_console, expand => false);

		-- the command line
		gtk_new_with_entry (console);
		
		-- console2.on_changed (echo_command'access); -- for every key pressed
		
		pack_start (box_console, console, expand => false);

		-- on startup the keyboard must focus on the console:
		console.grab_focus;
	end build_console;

	-- Builds the drawing area and places it in box_right.
	procedure build_canvas is begin
		-- drawing area on the right bottom
		gtk_new_hbox (box_drawing);
		set_spacing (box_drawing, 10);
		add (box_right, box_drawing);

		-- frame inside the drawing box
		gtk_new (frame);
		pack_start (box_drawing, frame);

		-- scrolled window inside the frame
		gtk_new (scrolled);
		set_policy (scrolled, policy_automatic, policy_automatic);
		add (frame, scrolled);
	end build_canvas;
	
	function to_string (d : in gdouble) return string is begin
		return gdouble'image (d);
	end;

	function to_string (d : in gint) return string is begin
		return gint'image (d);
	end;
	
	function to_string (p : in type_view_point) return string is begin
		return ("view x/y [pixels]" & to_string (gint (p.x)) & "/" & to_string (gint (p.y)));
	end;

	
	view_signals : constant gtkada.types.chars_ptr_array := (
		1 => new_string (string (signal_viewport_changed))
		);

	h_adj_property    : constant property_id := 1;
	v_adj_property    : constant property_id := 2;
	h_scroll_property : constant property_id := 3;
	v_scroll_property : constant property_id := 4;

	view_class_record : aliased glib.object.ada_gobject_class := glib.object.uninitialized_class;

	-- This procedure unifies two rectangles to one.
	procedure union (
		rect1 : in out type_rectangle;
		rect2 : type_rectangle) is
		right : constant type_distance := 
			type_distance'max (rect1.x + rect1.width, rect2.x + rect2.width);
		bottom : constant type_distance :=
			type_distance'max (rect1.y + rect1.height, rect2.y + rect2.height);
	begin
		rect1.x := type_distance'min (rect1.x, rect2.x);
		rect1.width := right - rect1.x;

		rect1.y := type_distance'min (rect1.y, rect2.y);
		rect1.height := bottom - rect1.y;
	end;

	procedure set_transform (
		self	: not null access type_view'class;
		cr		: cairo.cairo_context)
	is
		model_p : type_point := origin;
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
		area : type_rectangle := no_rectangle)
	is
		a : type_rectangle;
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

	function get_scale (self : not null access type_view) return type_scale is
	begin
		return self.scale;
	end get_scale;


	
-- CONVERSIONS BETWEEN COORDINATE SYSTEMS

	function vtm (
		view_point	: in type_view_point;
		scale		: in type_scale;
		topleft		: in type_point) 
		return type_point is
	begin
		return type_point (set (
			x	=> type_distance (view_point.x / scale) + (x (topleft)),
			y	=> type_distance (view_point.y / scale) + (y (topleft))
			));
	end;
	
	function view_to_model (
		self   : not null access type_view;
		p      : in type_view_point) 
		return type_point is
	begin
		return vtm (p, self.scale, self.topleft);
	end view_to_model;

	function view_to_model (
		self   : not null access type_view;
		rect   : in type_view_rectangle) -- position and size are in pixels
		return type_rectangle is

		-- get the position of the given rectangle in drawing coordinatess
		p1 : type_point := vtm ((rect.x, rect.y), self.scale, self.topleft);
	begin
		return (x      => p1.x,
				y      => p1.y,
				width  => type_distance (rect.width / self.scale),
				height => type_distance (rect.height / self.scale));
	end view_to_model;

	function mtv (
		drawing_point	: in type_point;
		scale			: in type_scale;
		topleft			: in type_point) 
		return type_view_point is
	begin
		return (
			x => type_view_coordinate (drawing_point.x - topleft.x) * scale,
			y => type_view_coordinate (drawing_point.y - topleft.y) * scale
			);
	end mtv;
	
	function model_to_view (
		self   : not null access type_view;
		p      : in type_point) 
		return type_view_point is
	begin
		return mtv (p, self.scale, self.topleft);
	end model_to_view;

-- 
-- 	function model_to_view (
-- 		self   : not null access type_view;
-- 		rect   : in type_rectangle)
-- 		return type_view_rectangle is
-- 		result : type_view_rectangle;
-- 	begin
-- 		result := (
-- 			x      => type_view_coordinate (rect.x - self.topleft.x) * self.scale,
-- 			y      => type_view_coordinate (rect.y - self.topleft.y) * self.scale,
-- 			width  => type_view_coordinate (rect.width) * self.scale,
-- 			height => type_view_coordinate (rect.height) * self.scale
-- 			);
-- 		
-- 		return result;
-- 	end model_to_view;

	procedure set_adjustment_values (self : not null access type_view'class) is
		box   : type_rectangle;
		area  : constant type_rectangle := self.get_visible_area;
		min, max : gdouble;
	begin
		if area.width <= 1.0 then
			--  not allocated yet
			return;
		end if;

		-- Get the bounding box of the whole drawing:
		box := bounding_box (self);

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
		pos  : constant type_point := type_point (set (
							x => type_distance (self.hadj.get_value),
							y => type_distance (self.vadj.get_value)));

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
					self.hadj.on_value_changed (access_on_adj_value_changed, self);
					self.queue_draw;
				end if;

			when v_adj_property => 

				self.vadj := gtk_adjustment (get_object (value));

				if self.vadj /= null then
					set_adjustment_values (self);
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

	procedure view_class_init (self : gobject_class) is begin
		set_properties_handlers (self, access_view_set_property, access_view_get_property);

		override_property (self, h_adj_property, "hadjustment");
		override_property (self, v_adj_property, "vadjustment");
		override_property (self, h_scroll_property, "hscroll-policy");
		override_property (self, v_scroll_property, "vscroll-policy");

		set_default_draw_handler (self, access_on_view_draw);
		set_default_size_allocate_handler (self, access_on_size_allocate);
		set_default_realize_handler (self, access_on_view_realize);
	end;

	function view_get_type return glib.gtype is
		info : access ginterface_info;
	begin
		if glib.object.initialize_class_record (
			ancestor     => gtk.bin.get_type,
			signals      => view_signals,
			class_record => view_class_record'access,
			type_name    => canvas_name, -- provided as parameter for the package
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



	function on_mouse_movement (
		view  : access gtk_widget_record'class;
		event : gdk_event_motion) return boolean 
	is
		use et_general;
		
		-- the point where the mouse pointer is pointing at
		view_point : type_view_point;

		-- The conversion from view to model coordinates requires a pointer to
		-- the view. This command sets self so that it points to the view:
		self : constant type_view_ptr := type_view_ptr (view);

		-- The point in the model expressed in millimeters:
		model_point : type_point;

		-- The point in the drawing:
		drawing_point : type_point;

	begin
		-- new_line;
		-- put_line ("mouse movement ! new positions are:");

		-- Get the position of the mouse pointer:
		view_point := (x => event.x, y => event.y); -- pixels
		-- put_line (" " & to_string (view_point));

		-- Convert the view point (pixels) to the position (millimeters) in the model:
		model_point := self.view_to_model (view_point);
		-- put_line (" model " & to_string (model_point));

		-- Convert model_point to drawing_point:
		drawing_point := model_to_drawing (self, model_point);
		-- put_line (" drawing " & to_string (self, drawing_point));

		-- update mouse position display (left of the canvas):
		gtk_entry (mouse_position_x.get_child).set_text (to_string (self, drawing_point, X));
		gtk_entry (mouse_position_y.get_child).set_text (to_string (self, drawing_point, Y));

		update_coordinates_display (self);

		return true; -- indicates that event has been handled
	end on_mouse_movement;

	procedure center_on (
		self		: not null access type_view'class;
		center_on	: type_point) -- in drawing
	is
		-- Convert the given point to a point in the model:
		center_on_model : type_point := drawing_to_model (self, center_on);

		-- Get the visible area of the model
		area : constant type_rectangle := self.get_visible_area; -- model

		-- Calculate the new topleft corner:
		pos  : constant type_point := type_point (set (
			center_on_model.x - area.width * 0.5,
			center_on_model.y - area.height * 0.5));
	begin
		self.scale_to_fit_requested := 0.0;
		self.topleft := pos;
		self.set_adjustment_values;
		self.queue_draw;
	end center_on;

	procedure shift_area (
		self		: not null access type_view'class;
		cursor		: in type_cursor) is

		area : constant type_rectangle := self.get_visible_area;

		area_center : constant type_point := type_point (set (
										x => area.x + area.width * 0.5,
										y => area.y + area.height * 0.5));
		
		area_center_drawing : type_point := self.model_to_drawing (area_center);
		
		-- Calculate the position of the area in the drawing.
		-- This is the upper left corner of the area:
		p : constant type_point := self.model_to_drawing (type_point (set (area.x, area.y)));

		-- Build the area of the drawing:
		a : constant type_rectangle := (
							x 		=> x (p),
							y 		=> y (p), 
							width 	=> area.width,
							height 	=> area.height);

		border_left		: constant type_distance := a.x;
		border_right	: constant type_distance := a.x + a.width;
		border_top		: constant type_distance := a.y;
		border_bottom	: constant type_distance := a.y - a.height;
		
		dxr : constant type_distance := x (cursor.position) - border_right;
		dxl : constant type_distance := border_left - x (cursor.position);
		dyt : constant type_distance := y (cursor.position) - border_top;
		dyb : constant type_distance := border_bottom - y (cursor.position);
	begin
		if dxr >= zero then
			self.center_on (type_point (move (point => area_center_drawing, direction => 0.0, distance => dxr)));
		elsif dxl >= zero then
			self.center_on (type_point (move (point => area_center_drawing, direction => 180.0, distance => dxl)));
			
		elsif dyt >= zero then
			self.center_on (type_point (move (point => area_center_drawing, direction => 90.0, distance => dyt)));
		elsif dyb >= zero then
			self.center_on (type_point (move (point => area_center_drawing, direction => -90.0, distance => dyb)));
		end if;
		
	end shift_area;

	
	procedure zoom_in (
		point	: in type_point; -- model point
		step	: in type_scale) 
	is
		scale : gdouble := canvas.get_scale * step;
	begin
		-- Ensure scale is within allowed range. If outside range,
		-- then the canvas scale will be left unchanged:
		if scale in type_scale then
			canvas.set_scale (scale, point);
		end if;
	end zoom_in;

	procedure zoom_out (
		point	: in type_point; -- model point
		step	: in type_scale) 
	is
		scale : gdouble := canvas.get_scale / step;
	begin
		-- Ensure scale is within allowed range. If outside range,
		-- then the canvas scale will be left unchanged:
		if scale in type_scale then
			canvas.set_scale (scale, point);
		end if;
	end zoom_out;

	
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
		
		self : constant type_view_ptr := type_view_ptr (view);

		-- The model point at which the zooming takes place:
		point : type_point;
		
	begin -- on_scroll_event

		-- If CTRL is being pressed, zoom in our out depending on dy:
		if (event.state and accel_mask) = control_mask then

			-- Get the center of the zooming operation:
			point := view_to_model (self, (event.x, event.y));
			
			-- CS: Testing event.direction would be more useful 
			-- but for some reason always returns SMOOTH_SCROLL.
			if dy > 0.0 then
				--put_line ("zoom out");
				--put_line ("zoom out at " & to_string (point));
				--set_scale (self, scale - scale_factor_on_zoom, point);
				zoom_out (point, scale_factor_on_zoom);
				event_handled;
			else
				--put_line ("zoom in");
				--put_line ("zoom in at  " & to_string (point));
				--set_scale (self, scale + scale_factor_on_zoom, point);
				zoom_in (point, scale_factor_on_zoom);
				event_handled;
			end if;
					
		end if;

		return result;
	end on_scroll_event;
	
	function on_key_pressed_event (
		view  : access gtk_widget_record'class;
		event : gdk_event_key) return boolean is

		-- If this function returns false, navigating with the TAB key
		-- through the main window is enabled.
		event_handled : boolean := false;
		
		use gdk.types;
		use gdk.types.keysyms;
		
		self    : constant type_view_ptr := type_view_ptr (view);

		key_ctrl : gdk_modifier_type := event.state and control_mask;
		key : gdk_key_type := event.keyval;
	begin
-- 		put_line ("key pressed");
-- 		new_line;
-- 		put_line (gdk_key_type'image (key));

-- 		put_line (gdk_modifier_type'image (key_ctrl));

		-- Zoom in/out on ctrl and +/- key:
		if key_ctrl = control_mask then 
			case key is
				when GDK_KP_Add | GDK_PLUS =>
-- 					put_line ("zoom in");
					zoom_in (
						point	=> drawing_to_model (self, cursor_main.position),
						step	=> scale_factor_on_zoom);
					
				when GDK_KP_Subtract | GDK_minus =>
-- 					put_line ("zoom out");
					zoom_out (
						point	=> drawing_to_model (self, cursor_main.position),
						step	=> scale_factor_on_zoom);
					
				when others => null;
			end case;
		else
		
			case key is
-- 				when GDK_Control_L | GDK_Control_R =>
-- 					put_line ("ctrl pressed");

				when GDK_Right =>
					canvas.move_cursor (RIGHT, cursor_main);
					self.queue_draw; -- without frame and grid initialization
					event_handled := true;

				when GDK_Left =>
					canvas.move_cursor (LEFT, cursor_main);
					self.queue_draw; -- without frame and grid initialization
					event_handled := true;
					
				when GDK_Up =>
					canvas.move_cursor (UP, cursor_main);
					self.queue_draw; -- without frame and grid initialization
					event_handled := true;
					
				when GDK_Down =>
					canvas.move_cursor (DOWN, cursor_main);
					self.queue_draw; -- without frame and grid initialization
					event_handled := true;
					
				when others =>
					-- put_line ("other key pressed");

					-- CS: test the TAB key explicitely in order to return false ?
					event_handled := false;
			end case;

		end if;

		return event_handled;
	end on_key_pressed_event;
	
	function on_button_event (
		view  : access gtk_widget_record'class;
		event : gdk_event_button)
		return boolean
	is
		self : constant type_view_ptr := type_view_ptr (view);
		
		mouse_button : constant positive := positive (event.button);
		view_point : constant type_view_point := (event.x, event.y);
		model_point : constant type_point := self.view_to_model (view_point);
		drawing_point : constant type_point := self.model_to_drawing (model_point);
	begin
-- 		put_line ("mouse button " & positive'image (mouse_button) & " pressed");

		case mouse_button is
			when 1 => -- left button
				self.move_cursor (ABSOLUTE, cursor_main, drawing_point);
				self.queue_draw; -- without frame and grid initialization

			when others => null;
		end case;

		-- After any click somewhere in the canvas, the canvas gets the keyboard focus:
		self.grab_focus;
		
		return true; -- indicates that event has been handled
	end on_button_event;
	
	procedure init (
		self  : not null access type_view'class) is
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
		self.on_motion_notify_event (access_on_mouse_movement);

		-- reaction to mouse wheel being rotated
		self.on_scroll_event (access_on_scroll_event);

		-- reaction to mouse buttons pressed
		self.on_button_press_event (access_on_button_event);

		-- reaction to keys pressed on the keyboard		
		self.on_key_press_event (access_on_key_pressed_event);
		
		self.set_can_focus (true);
	end init;

	
	procedure set_scale (
		self     : not null access type_view;
		scale    : in type_scale := scale_default;
		preserve : in type_point := origin)
	is
		-- backup old scale
		old_scale : constant type_distance := type_distance (self.scale);

		-- save requested scale
		new_scale : constant type_distance := type_distance (scale);

		-- for calculating the new topleft point we need those tempoarily variables:
		cx, cy : type_distance;
		
		box : type_rectangle;
		p   : type_point;

	begin
		--put_line (type_scale'image (scale));
				  
		if preserve /= origin then
			-- set p at the point given by preserve
			p := preserve;
		else
			-- get the visible area
			box := self.get_visible_area;

			-- set p at the center of the visible area
			p := type_point (set (
				x => box.x + box.width / 2.0,
				y => box.y + box.height / 2.0));
		end if;

		self.scale := scale;

		-- Calculate the new topleft corner of the visible area:
		-- Reason: The next time a model point is computed (via view_to_model)
		-- the point must not change. So topleft is now moved so that
		-- function view_to_model returns for the same view point the same
		-- model point.
		cx := p.x - self.topleft.x;
		cx := cx * old_scale;
		
		cy := p.y - self.topleft.y;
		cy := cy * old_scale;
		
		self.topleft := type_point (set (
			p.x - cx / new_scale,
			p.y - cy / new_scale)
			);
		
		self.scale_to_fit_requested := 0.0;
		self.set_adjustment_values;
		self.queue_draw;
	end set_scale;

	function get_visible_area (self : not null access type_view'class)
		return type_rectangle is
	begin
		return self.view_to_model (
			-- Assemble a type_view_rectangle which will be converted
			-- to a type_rectangle by function view_to_model.
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

-- 	procedure refresh_layout (
-- 		self        : not null access type_model;
-- 		send_signal : boolean := true) is
-- 	begin
-- 		-- Update the width and height of all items:
-- 		
-- 		-- CS no need for size request. All items will have properties width and heigth.
-- 
-- 		if send_signal then
-- 			type_model'class (self.all).layout_changed;
-- 		end if;
-- 	end refresh_layout;

	procedure scale_to_fit (
		self      : not null access type_view'class;
		rect      : in type_rectangle := no_rectangle;
		min_scale : in type_scale := 1.0 / 4.0;
		max_scale : in type_scale := 4.0)
	is
		box     : type_rectangle;
		w, h, s : gdouble;
		alloc   : gtk_allocation;
		wmin, hmin : gdouble;
	begin
-- 		put_line ("scale to fit ...");
		self.get_allocation (alloc);
		if alloc.width <= 1 then
			self.scale_to_fit_requested := max_scale;
			self.scale_to_fit_area := rect;

		else
			self.scale_to_fit_requested := 0.0;
			
			if rect = no_rectangle then
				box := bounding_box (self);
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

				self.scale := s;

				-- calculate the new topleft corner of the visible area:
				self.topleft := type_point (set (
					x	=> box.x - (type_distance (w / s) - box.width) / 2.0,
					y	=> box.y - (type_distance (h / s) - box.height) / 2.0
					));
				
				self.set_adjustment_values;
				self.queue_draw;

			end if;
		end if;
	end scale_to_fit;
	
	function convert_x (x : in type_distance) return type_view_coordinate is begin
		return type_view_coordinate (x);
	end;


	function lower_grid_coordinate (
		coordinate	: in type_distance;
		grid		: in type_distance_grid) 
	-- This function calculates the grid coordinate on the axis that comes
	-- before the given coordinate.
	-- Example 1: If coordinate is 215.6 and grid size is 10, then x becomes 210.
	-- Example 2: If coordinate is 166.5 and grid size is 5, then x becomes 165.
		return type_view_coordinate is 
		
		g : float := float (grid);
		f : float := float'floor (float (coordinate) / g);
		
	begin
		return type_view_coordinate (f * g);
	end;

	procedure draw_grid (
		context	: in type_draw_context;
		area	: in type_rectangle; -- the area of the drawing to be displayed
		grid	: in geometry.type_grid;
		start_x	: in type_view_coordinate;
		start_y	: in type_view_coordinate;
		color	: in et_colors.type_color) is

		x : type_view_coordinate := start_x;
		y : type_view_coordinate := start_y;

		-- CS Currently very small crosses are drawn.
		-- Find a way to draw dots !
		
		dot_size : type_view_coordinate;
		line_width : type_view_coordinate;

		density_x, density_y : type_view_coordinate;
	begin
		-- Calcuate the grid density in x and y.
		density_x := 1.0 / (type_view_coordinate (grid.x) * canvas.scale);
		density_y := 1.0 / (type_view_coordinate (grid.y) * canvas.scale);

		-- If density in x AND in y is below the threshold_grid_density
		-- then draw the grid.
		if density_x < threshold_grid_density and density_y < threshold_grid_density then

			-- set the grid color
			cairo.set_source_rgb (context.cr, color.red, color.green, color.blue);

			dot_size := 2.0 / canvas.scale;
			-- CS dot_size_multiplier as constant in spec ?
			-- CS limit to max and min ?
			
			line_width := 1.0 / canvas.scale;
			-- CS limit to max and min ?
			-- CS line_widht_multiplier as constant in spec ?
			
			-- set the line with of the crosses
			cairo.set_line_width (context.cr, line_width);

			-- We draw the grid in x-axis from left to right:
			while x < type_view_coordinate (area.x + area.width) loop

				-- We draw the grid in y-axis upwards:
				y := start_y;
				
				while y > type_view_coordinate (area.y) loop

					-- draw a very small cross (so that it seems like a dot):
					cairo.move_to (context.cr, x - dot_size, y);
					cairo.line_to (context.cr, x + dot_size, y);

					cairo.move_to (context.cr, x, y - dot_size);
					cairo.line_to (context.cr, x, y + dot_size);

					-- advance to next upper row on y-axis
					y := y - type_view_coordinate (grid.y);
				end loop;

				-- advance to next column on the right on x-axis
				x := x + type_view_coordinate (grid.x);
			end loop;

			cairo.stroke (context.cr);
		end if;
	end draw_grid;


	function frame_bounding_box (
		self : not null access type_view'class)
		return type_rectangle is
		box : type_rectangle; -- to be returned

		use et_general;
		use et_frames;

		paper_height : constant type_distance_positive := type_distance_positive (paper_dimension (
						paper_size	=> self.get_frame.paper,
						orientation	=> self.get_frame.orientation,
						axis		=> Y));

		paper_width : constant type_distance_positive := type_distance_positive (paper_dimension (
						paper_size	=> self.get_frame.paper,
						orientation	=> self.get_frame.orientation,
						axis		=> X));

	begin
		-- position (upper left corner):
		box.x := (paper_width - type_distance_positive (self.get_frame.size.x)) / 2.0;
		box.y := (paper_height - type_distance_positive (self.get_frame.size.y)) / 2.0;

		-- width and height
		box.width := type_distance_positive (self.get_frame.size.x);
		box.height := type_distance_positive (self.get_frame.size.y);

		return box;
	end frame_bounding_box;
	
	function paper_bounding_box (
		self : not null access type_view'class)
		return type_rectangle is

		use et_general;
		use et_frames;

		paper_height : constant type_distance_positive := type_distance_positive (paper_dimension (
						paper_size	=> self.get_frame.paper,
						orientation	=> self.get_frame.orientation,
						axis		=> Y));

		paper_width : constant type_distance_positive := type_distance_positive (paper_dimension (
						paper_size	=> self.get_frame.paper,
						orientation	=> self.get_frame.orientation,
						axis		=> X));
		
	begin
		return (0.0, 0.0, paper_width, paper_height);
	end paper_bounding_box;
	
end pac_canvas;
	
end et_canvas_general;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
