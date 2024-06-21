------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                CANVAS                                    --
--                                                                          --
--                               B o d y                                    --
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

with ada.strings.bounded;
with ada.strings;
with ada.strings.fixed;

with gdk.types;
with gdk.types.keysyms;
with gtk.accel_group;
with gdk.event;
with glib;						use glib;
with gtk.enums;					use gtk.enums;
with gtk.main;					use gtk.main;

package body et_canvas is

-- ZOOM:

	function to_string (
		zf : in type_zoom_factor)
		return string
	is begin
		return type_zoom_factor'image (zf);
	end to_string;
	
	
	procedure increase_zoom_factor is begin
		S := S * SM;
		
		exception 
			when constraint_error =>
				put_line ("upper zoom limit reached");
			when others => null;
	end increase_zoom_factor;

	
	procedure decrease_zoom_factor is begin
		S := S / SM;
		
		exception 
			when constraint_error => 
				put_line ("lower zoom limit reached");
			when others => null;
	end decrease_zoom_factor;



	
	
-- CONVERSIONS:

	function to_real (
		point : in type_vector_model)
		return type_vector_model
	is	
		result : type_vector_model := point;
	begin
		move_by (result, bounding_box.position);
		return result;
	end to_real;


	function to_virtual (
		point : in type_vector_model)
		return type_vector_model
	is
		result : type_vector_model := point;
	begin
		move_by (result, invert (bounding_box.position));
		return result;
	end to_virtual;



	function to_distance (
		d : in type_distance_model_positive)
		return type_logical_pixels_positive
	is begin
		return type_logical_pixels (d) * type_logical_pixels (S);
	end to_distance;


	function to_distance (
		d : in type_logical_pixels_positive)
		return type_distance_model_positive
	is begin
		return type_distance_model_positive (d / type_logical_pixels (S));
	end to_distance;

	

	function virtual_to_canvas (
		V 			: in type_vector_model;
		zf			: in type_zoom_factor;
		translate	: in boolean)
		return type_logical_pixels_vector
	is
		Z : type_logical_pixels_vector;
	begin
		Z.x :=  (type_logical_pixels (V.x) * type_logical_pixels (zf)
					+ F.x);
		
		Z.y := -(type_logical_pixels (V.y) * type_logical_pixels (zf)
					+ F.y);

		-- If required: Move Z by the current translate-offset:
		if translate then
			Z.x := Z.x + T.x;
			Z.y := Z.y + T.y;
		end if;
		
		return Z;
	end virtual_to_canvas;

	
	
	function canvas_to_virtual (
		P			: in type_logical_pixels_vector;
		zf			: in type_zoom_factor)
		return type_vector_model
	is 
		result : type_vector_model;
		-- debug : boolean := false;
	begin
		result.x := type_distance_model 
			(( (P.x - T.x) - F.x) / type_logical_pixels (zf));
		
		result.y := type_distance_model 
			((-(P.y - T.y) - F.y) / type_logical_pixels (zf));

		return result;
	end canvas_to_virtual;

	


	function real_to_canvas (
		M 	: in type_vector_model;
		zf	: in type_zoom_factor)
		return type_logical_pixels_vector
	is
		V : type_vector_model;
		Z : type_logical_pixels_vector;
	begin
		-- Convert the given real model point 
		-- to a virtual model point:
		V := to_virtual (M);

		-- Convert the virtual model point V to a 
		-- canvas point and take the current translate-offset
		-- into account:
		Z := virtual_to_canvas (V, zf, translate => true);
	
		return Z;
	end real_to_canvas;

	
	function canvas_to_real (
		P	: in type_logical_pixels_vector;
		zf	: in type_zoom_factor)
		return type_vector_model
	is 
		M : type_vector_model;
		debug : boolean := false;
	begin
		if debug then
			put_line ("canvas_to_real");
			put_line ("T " & to_string (T));
		end if;
		
		-- Convert the given canvas point to a virtual
		-- model point:
		M := canvas_to_virtual (P, zf);
		
		-- Convert the virtual model point to
		-- a real model point:
		return to_real (M);

		exception
			when constraint_error =>
				put_line ("ERROR: conversion from canvas point "
					& "to model point failed !");
				put_line (" point " & to_string (P));
				put_line (" zf    " & to_string (zf));
				put_line (" T     " & to_string (T));
				put_line (" F     " & to_string (F));
				raise;						  
	end canvas_to_real;



-- BOUNDING-BOX:
	
	
	function get_bounding_box_corners
		return type_bounding_box_corners
	is
		result : type_bounding_box_corners;

		-- The corners of the given area in model-coordinates:
		BC : constant type_area_corners := get_corners (bounding_box);

	begin
		-- Convert the corners of the bounding-box to canvas coordinates:
		result.TL := real_to_canvas (BC.TL, S);
		result.TR := real_to_canvas (BC.TR, S);
		result.BL := real_to_canvas (BC.BL, S);
		result.BR := real_to_canvas (BC.BR, S);
		
		return result;
	end get_bounding_box_corners;



	
	
-- BASE-OFFSET:

	procedure set_base_offset is
		debug : boolean := false;
		
		x, y : type_logical_pixels;

		-- The maximum zoom factor:
		S_max : constant type_logical_pixels := 
			type_logical_pixels (type_zoom_factor'last);

		-- The width and height of the bounding-box:
		Bh : constant type_logical_pixels := 
			type_logical_pixels (bounding_box.height);
		
		Bw : constant type_logical_pixels := 
			type_logical_pixels (bounding_box.width);
		
	begin
		x :=   Bw * (S_max - 1.0);
		y := - Bh * S_max;

		-- Set the base-offset:
		F := (x, y);

		-- Output a warning if the base-offset is outside
		-- the canvas dimensions:
		if  x >   type_logical_pixels (canvas_size.width) or
			y < - type_logical_pixels (canvas_size.height) then

			put_line ("WARNING: base-offset outside canvas !");
			put_line (" F: " & to_string (F));
		end if;
		

		if debug then
			put_line ("base offset: " & to_string (F));
		end if;
	end set_base_offset;

	
	
	
-- ZOOM:


	procedure set_translation_for_zoom (
		S1	: in type_zoom_factor;
		S2	: in type_zoom_factor;
		Z1	: in type_logical_pixels_vector) -- a canvas point
	is 
		debug : boolean := false;

		-- Convert the given canvas point to a
		-- virtual model point according to the old zoom factor:
		V : constant type_vector_model := canvas_to_virtual (Z1, S1);

		Z2 : type_logical_pixels_vector;
	begin			
		if debug then
			put_line ("set_translation_for_zoom");
		end if;

		-- Starting at the virtual model point V,
		-- compute the prospected canvas point according to the 
		-- new zoom factor. 
		-- The current translate-offset will NOT be taken into account:
		Z2 := virtual_to_canvas (V, S2, false);

		-- This is the offset from Z1 to the prospected
		-- point Z2. The offset must be multiplied by -1 because the
		-- drawing must be dragged-back to the given pointer position:
		T.x := -(Z2.x - Z1.x);
		T.y := -(Z2.y - Z1.y);
		-- CS simplify or use vector mathematics specified in 
		-- package demo_logical_pixels

		if debug then
			put_line (" T: " & to_string (T));
		end if;
	end set_translation_for_zoom;


	

	procedure set_translation_for_zoom (
		S1	: in type_zoom_factor;
		S2	: in type_zoom_factor;
		M	: in type_vector_model) -- real model point
	is 
		debug : boolean := false;
		
		-- Convert the given real model point to 
		-- a virtual model point:
		V : constant type_vector_model := to_virtual (M);
		
		Z1, Z2 : type_logical_pixels_vector;		
	begin			
		if debug then
			put_line ("set_translation_for_zoom");
		end if;

		-- Convert the virtual model point to a canvas point
		-- according to the old zoom factor.
		-- The current translate-offset will NOT be taken into account:
		Z1 := virtual_to_canvas (V, S1, translate => true);
		
		-- Compute the prospected canvas point according to the 
		-- new zoom factor.
		-- The current translate-offset will NOT be taken into account:
		Z2 := virtual_to_canvas (V, S2, translate => false);
		-- put_line ("Z2 " & to_string (Z2));

		-- This is the offset from point Z1 to the prospected
		-- point Z2. The offset must be multiplied by -1 because the
		-- drawing must be dragged-back to the given pointer position:
		T.x := -(Z2.x - Z1.x);
		T.y := -(Z2.y - Z1.y);
		-- CS simplify or use vector mathematics specified in 
		-- package demo_logical_pixels
		
		if debug then
			put_line (" T: " & to_string (T));
		end if;
	end set_translation_for_zoom;



	procedure reset_zoom_area is begin
		put_line ("reset_zoom_area");
		zoom_area := (others => <>);
	end reset_zoom_area;



	procedure zoom_on_cursor (
		direction : in type_zoom_direction)
	is
		S1 : constant type_zoom_factor := S;

		-- The corners of the bounding-box on the canvas before 
		-- and after zooming:
		C1, C2 : type_bounding_box_corners;
	begin
		put_line ("zoom_on_cursor " & type_zoom_direction'image (direction));

		C1 := get_bounding_box_corners;

		case direction is
			when ZOOM_IN =>
				increase_zoom_factor;
				put_line (" zoom in");
				
			when ZOOM_OUT => 
				decrease_zoom_factor;
				put_line (" zoom out");
				
			when others => null;
		end case;

		update_zoom_display;
		
		-- put_line (" S" & to_string (S));

		-- After changing the zoom factor, the translate-offset must
		-- be calculated anew. When the actual drawing takes 
		-- place (see function cb_draw_objects)
		-- then the drawing will be dragged back by the translate-offset
		-- so that the operator gets the impression of a zoom-into or 
		-- zoom-out effect.
		-- Without applying a translate-offset the drawing would be appearing
		-- as expanding to the upper-right (on zoom-in) or shrinking toward 
		-- the lower-left:
		set_translation_for_zoom (S1, S, cursor.position);

		C2 := get_bounding_box_corners;
		update_scrollbar_limits (C1, C2);

		-- show_adjustments_v;

		backup_visible_area (get_visible_area (canvas));
		
		-- schedule a redraw:
		refresh (canvas);		
	end zoom_on_cursor;


	

	procedure zoom_to_fit (
		area : in type_area)
	is
		debug : boolean := false;
	begin
		put_line ("zoom_to_fit");

		-- Calculate the zoom factor that is required to
		-- fit the given area into the scrolled window:
		S := get_ratio (area);
		
		if debug then
			put_line (" S: " & type_zoom_factor'image (S));
		end if;

		update_zoom_display;
		-----------------------------------------------------

		-- Calculate the translate-offset that is required to
		-- center the given area on the visible area:
		center_to_visible_area (area);

		if debug then
			show_adjustments_h;
			show_adjustments_v;
		end if;

		--backup_scrollbar_settings;
	end zoom_to_fit;





	procedure draw_zoom_area is
		use cairo;
		
		x, y : type_logical_pixels;
		w, h : type_logical_pixels;

		l1 : type_logical_pixels_vector renames zoom_area.l1;
		l2 : type_logical_pixels_vector renames zoom_area.l2;
	begin
		if zoom_area.started then

			-- Set the color of the rectangle:
			set_source_rgb (context, 0.5, 0.5, 0.5); -- gray

			-- Compute the position and dimensions of
			-- the rectangle:

			-- x-position:
			if l1.x < l2.x then
				x := l1.x;
			else
				x := l2.x;
			end if;

			-- y-position:
			if l1.y < l2.y then
				y := l1.y;
			else
				y := l2.y;
			end if;

			-- width and height:
			w := abs (l1.x - l2.x);
			h := abs (l1.y - l2.y);

			set_line_width (context, to_gdouble (zoom_area_linewidth));
			
			rectangle (context, 
				to_gdouble (x),
				to_gdouble (y),
				to_gdouble (w),
				to_gdouble (h));
				
			stroke;
		end if;
	end draw_zoom_area;

	
	
-- VISIBLE AREA:

	function get_visible_area (
		canvas	: access gtk_widget_record'class)
		return type_area
	is
		result : type_area;

		-- The allocation of the scrolled window:
		W : gtk_allocation;
		
		h_start, h_length, h_end : type_logical_pixels;
		v_start, v_length, v_end : type_logical_pixels;

		-- The four corners of the visible area:
		BL, BR, TL, TR : type_vector_model;
	begin
		-- Inquire the allocation of the scrolled window
		-- inside the main window:
		get_allocation (swin, W);

		
		-- X-AXIS:
		
		-- The visible area along the x-axis starts at the
		-- position of the horizontal scrollbar:
		h_start  := to_lp (scrollbar_h_adj.get_value);

		-- The visible area along the x-axis is as wide as
		-- the scrolled window:
		h_length := type_logical_pixels (W.width);

		-- The visible area ends here:
		h_end    := h_start + h_length;


		-- Y-AXIS:
		
		-- The visible area along the y-axis starts at the
		-- position of the vertical scrollbar:
		v_start := to_lp (scrollbar_v_adj.get_value);

		-- The visible area along the y-axis is as high as
		-- the scrolled window:
		v_length := type_logical_pixels (W.height);

		-- The visible area along the y-axis ends here:
		v_end := v_start + v_length;

		
		-- Compute the corners of the visible area.
		-- The corners are real model coordinates:
		BL := canvas_to_real ((h_start, v_end),   S);
		BR := canvas_to_real ((h_end, v_end),     S);
		TL := canvas_to_real ((h_start, v_start), S);
		TR := canvas_to_real ((h_end, v_start),   S);

		-- put_line ("BL " & to_string (BL));
		-- put_line ("BR " & to_string (BR));
		-- put_line ("TR " & to_string (TR));
		-- put_line ("TL " & to_string (TL));

		-- The position of the visible area is the lower left 
		-- corner:
		result.position := BL;
		
		-- Compute the width and the height of the
		-- visible area:
		result.width := TR.x - TL.x;
		result.height := TL.y - BL.y;

		-- CS: more effective ?
		-- result.width    := type_distance_model 
		--		(h_length) * type_distance_model (S);
		-- result.height   := type_distance_model 
		--		(v_length) * type_distance_model (S);

		-- put_line ("visible area " & to_string (result));
		return result;
	end get_visible_area;



	procedure center_to_visible_area (
		area : in type_area)
	is
		-- debug : boolean := true;
		debug : boolean := false;
		
		-- The offset required to "move" all objects into
		-- the center of the visible area:
		dx, dy : type_distance_model;
		
		-- Get the currently visible model area:
		v : constant type_area := get_visible_area (canvas);

		w1 : constant type_distance_model := v.width;
		w2 : constant type_distance_model := area.width;

		h1 : constant type_distance_model := v.height;
		h2 : constant type_distance_model := area.height;

		a, b : type_distance_model;

		x0 : constant type_distance_model := area.position.x;
		y0 : constant type_distance_model := area.position.y;
		
		x1 : constant type_distance_model := v.position.x;
		y1 : constant type_distance_model := v.position.y;

		-- The given area will end up at this target position:
		x2, y2 : type_distance_model;
		
	begin
		if debug then
			put_line ("given   " & to_string (area));
			put_line ("visible " & to_string (v));
		end if;
		
		a := (w1 - w2) * 0.5;
		x2 := x1 + a;
		dx := x2 - x0;

		b := (h1 - h2) * 0.5;
		y2 := y1 + b;
		dy := y2 - y0;

		if debug then
			put_line ("dx:" & to_string (dx));
			put_line ("dy:" & to_string (dy));
		end if;

		-- Convert the model offset (dx;dy) to a canvas offset
		-- and apply it to the global translate-offset.
		-- Regarding y: T is in the canvas system (CS2)
		-- where the y-axis goes downward. So we must multiply by -1:
		T.x :=   type_logical_pixels (dx) * type_logical_pixels (S);
		T.y := - type_logical_pixels (dy) * type_logical_pixels (S);
		if debug then
			put_line ("T: " & to_string (T));
		end if;

	end center_to_visible_area;


	
	procedure backup_visible_area (
		area : in type_area)
	is begin
		last_visible_area := area;
	end backup_visible_area;



	
	
-- MAIN WINDOW:
	
	procedure create_window is begin
		put_line ("create_window");
		

		main_window := gtk_window_new (WINDOW_TOPLEVEL);
		main_window.set_border_width (10);

		-- CS: Set the minimum size of the main window ?
		-- CS show main window size
		-- main_window.set_size_request (1000, 500);

		-- main_window.set_redraw_on_allocate (false);
		

		gtk_new_hbox (box_h);

		
		-- vertical box for coordinates display:
		gtk_new_vbox (box_v1);
		box_v1.set_border_width (10);
		
		-- The left vbox shall not change its width when the 
		-- main window is resized:
		box_h.pack_start (box_v1, expand => false);

		-- Place a separator between the left and right
		-- vertical box:
		separator := gtk_separator_new (ORIENTATION_VERTICAL);
		box_h.pack_start (separator, expand => false);

		-- The right vbox shall expand upon resizing the main window:
		-- box_h.pack_start (box_v2);

		main_window.add (box_h);
		
	end create_window;



	procedure set_title_bar (
		title : in string)
	is begin
		main_window.set_title (title);
	end;

	

-- SCROLLED WINDOW:

	procedure create_scrolled_window_and_scrollbars is
	begin
		put_line ("create_scrolled_window");

		-- Create a scrolled window:
		swin := gtk_scrolled_window_new (
			hadjustment => null, vadjustment => null);

		-- Set the minimum size of the scrolled window and
		-- the global swin_size variable.
		-- There are two ways to do that:
		--
		-- 1. Basing on the global bounding-box which has been calculated
		--    by parsing the model database. This causes the scrolled window
		--    to adapt on startup to the model.
		--    IMPORTANT: The height must be greater than the sum
		--    of the height of all other widgets in the main window !
		--    Otherwise the canvas may freeze and stop emitting signals:
		--
		-- swin.set_size_request (
		-- 	gint (bounding_box.width),
		-- 	gint (bounding_box.height)); -- Mind a minimal height !
		--  -- See above comment.
		-- 
		-- swin_size := (
		-- 	width	=> positive (bounding_box.width),
		-- 	height	=> positive (bounding_box.height));

		
		-- 2. A static startup-configuration based on a certain 
		--    minimal width and height. This ensures that the scrolled
		--    window has a predictable and well defined size.
		--    This is to be prefered over approach 1 (see above):
		swin.set_size_request (
			gint (swin_size_initial.width),
			gint (swin_size_initial.height));
  
		swin_size := (
			width	=> swin_size_initial.width,
			height	=> swin_size_initial.height);


		
		-- CS show window size

		put_line ("scrolled window zoom mode: " 
			& type_scrolled_window_zoom_mode'image (zoom_mode));

		
		-- swin.set_border_width (10);
		-- swin.set_redraw_on_allocate (false);

		
		scrollbar_h_adj := swin.get_hadjustment;
		scrollbar_v_adj := swin.get_vadjustment;



		
		-- behaviour:
		swin.set_policy ( -- for scrollbars
			hscrollbar_policy => gtk.enums.POLICY_AUTOMATIC,
			-- hscrollbar_policy => gtk.enums.POLICY_NEVER, 
			vscrollbar_policy => gtk.enums.POLICY_AUTOMATIC);
			-- vscrollbar_policy => gtk.enums.POLICY_NEVER);


		-- CS: Attempt to disable auto-scrolling of scrollbars
		-- when the canvas get the focus:
		-- set_focus_hadjustment (
		-- 	container	=> swin,
		-- 	adjustment	=> scrollbar_h_adj);

		-- scrollbar_h.set_can_focus (false);
		-- swin.grab_focus;

		-- swin.set_propagate_natural_height (true);

		
	end create_scrolled_window_and_scrollbars;

	

	procedure backup_scrollbar_settings is begin
		--put_line ("backup_scrollbar_settings");
		scrollbar_h_backup.lower := to_lp (scrollbar_h_adj.get_lower);
		scrollbar_h_backup.value := to_lp (scrollbar_h_adj.get_value);
		scrollbar_h_backup.page_size := to_lp (scrollbar_h_adj.get_page_size);
		scrollbar_h_backup.upper := to_lp (scrollbar_h_adj.get_upper);

		scrollbar_v_backup.lower := to_lp (scrollbar_v_adj.get_lower);
		scrollbar_v_backup.value := to_lp (scrollbar_v_adj.get_value);
		scrollbar_v_backup.page_size := to_lp (scrollbar_v_adj.get_page_size);
		scrollbar_v_backup.upper := to_lp (scrollbar_v_adj.get_upper);
	end backup_scrollbar_settings;
	

	procedure restore_scrollbar_settings is begin
		scrollbar_h_adj.set_lower (to_gdouble (scrollbar_h_backup.lower));
		scrollbar_h_adj.set_value (to_gdouble (scrollbar_h_backup.value));
		
		scrollbar_h_adj.set_page_size (
			to_gdouble (scrollbar_h_backup.page_size));
		
		scrollbar_h_adj.set_upper (to_gdouble (scrollbar_h_backup.upper));

		scrollbar_v_adj.set_lower (to_gdouble (scrollbar_v_backup.lower));
		scrollbar_v_adj.set_value (to_gdouble (scrollbar_v_backup.value));

		scrollbar_v_adj.set_page_size (
			to_gdouble (scrollbar_v_backup.page_size));

		scrollbar_v_adj.set_upper (to_gdouble (scrollbar_v_backup.upper));
	end restore_scrollbar_settings;



	procedure set_initial_scrollbar_settings is
		debug : boolean := false;
		-- debug : boolean := true;
	begin
		put_line ("set initial scrollbar settings");
		
		scrollbar_v_init.upper := - F.y;			
		
		scrollbar_v_init.lower := scrollbar_v_init.upper - 
			type_logical_pixels (bounding_box.height);
		
		scrollbar_v_init.page_size := 
			type_logical_pixels (bounding_box.height);
		
		scrollbar_v_init.value := scrollbar_v_init.lower;

		if debug then
			put_line (" vertical:");
			put_line ("  lower" & 
				to_string (scrollbar_v_init.lower));

			put_line ("  upper" & 
				to_string (scrollbar_v_init.upper));

			put_line ("  page " & 
				to_string (scrollbar_v_init.page_size));

			put_line ("  value" & 
				to_string (scrollbar_v_init.value));
		end if;
		
		scrollbar_h_init.lower := F.x;
		scrollbar_h_init.upper := scrollbar_h_init.lower + 
			type_logical_pixels (bounding_box.width);
		
		scrollbar_h_init.page_size := 
			type_logical_pixels (bounding_box.width);
		
		scrollbar_h_init.value := scrollbar_h_init.lower;

		if debug then
			put_line (" horizontal:");
			put_line ("  lower" & 
				to_string (scrollbar_h_init.lower));

			put_line ("  upper" &
				to_string (scrollbar_h_init.upper));

			put_line ("  page " & 
				to_string (scrollbar_h_init.page_size));

			put_line ("  value" & 
				to_string (scrollbar_h_init.value));
		end if;

	
		----------------------------------------------------------------------
		-- CS: This code is experimental in order to make the canvas
		-- dimensions adjust DYNAMICALLY to the scrollbar limits. So far this
		-- was not successful because the canvas size can not be changed
		-- for some unknown reason after initialization:
		
-- 		declare
-- 			w, h : gint;
-- 			a : gtk_allocation;
-- 		begin
-- 			w := gint (scrollbar_h_init.lower + scrollbar_h_init.upper);
-- 			h := gint (scrollbar_v_init.lower + scrollbar_v_init.upper);
-- 
-- 			canvas.get_allocation (a);
-- 			a.width := w;
-- 			a.height := h;
-- 			-- canvas.set_allocation (a);
-- 			-- canvas.size_allocate (a);
-- 			-- canvas.set_size_request (w, h);
-- 			
-- 			if debug then
-- 				show_canvas_size;
-- 				-- put_line ("x/y : " & gint'image (a.x) & "/" 
-- 					& gint'image (a.y));
-- 			end if;
-- 		end;
		----------------------------------------------------------------------

		
		-- put_line ("vertical:");
		scrollbar_v_adj.set_upper (to_gdouble (scrollbar_v_init.upper));
		scrollbar_v_adj.set_lower (to_gdouble (scrollbar_v_init.lower));
		
		scrollbar_v_adj.set_page_size (
			to_gdouble (scrollbar_v_init.page_size));

		scrollbar_v_adj.set_value (to_gdouble (scrollbar_v_init.value));

		-- put_line ("horizontal:");
		scrollbar_h_adj.set_upper (to_gdouble (scrollbar_h_init.upper));
		scrollbar_h_adj.set_lower (to_gdouble (scrollbar_h_init.lower));

		scrollbar_h_adj.set_page_size (
			to_gdouble (scrollbar_h_init.page_size));

		scrollbar_h_adj.set_value (to_gdouble (scrollbar_h_init.value));

		-- show_adjustments_h;
		-- show_adjustments_v;
		
		backup_scrollbar_settings;
		
	end set_initial_scrollbar_settings;

	
	

	procedure show_adjustments_v is 
		v_lower : type_logical_pixels := 
			to_lp (scrollbar_v_adj.get_lower);

		v_value : type_logical_pixels := 
			to_lp (scrollbar_v_adj.get_value);

		v_upper : type_logical_pixels := 
			to_lp (scrollbar_v_adj.get_upper);

		v_page  : type_logical_pixels := 
			to_lp (scrollbar_v_adj.get_page_size);
		
	begin
		put_line ("vertical scrollbar adjustments:");
		put_line (" lower" & to_string (v_lower));
		put_line (" value" & to_string (v_value));
		put_line (" page " & to_string (v_page));
		put_line (" upper" & to_string (v_upper));
	end show_adjustments_v;
				  

	procedure show_adjustments_h is 
		h_lower : type_logical_pixels := to_lp (scrollbar_h_adj.get_lower);
		h_value : type_logical_pixels := to_lp (scrollbar_h_adj.get_value);
		h_upper : type_logical_pixels := to_lp (scrollbar_h_adj.get_upper);
		
		h_page  : type_logical_pixels := 
			to_lp (scrollbar_h_adj.get_page_size);
	begin
		put_line ("horizontal scrollbar adjustments:");
		put_line (" lower" & to_string (h_lower));
		put_line (" value" & to_string (h_value));
		put_line (" page " & to_string (h_page));
		put_line (" upper" & to_string (h_upper));
	end show_adjustments_h;



	function get_ratio (
		area : in type_area)
		return type_zoom_factor
	is
		-- The allocation of the scrolled window provides
		-- its width and height:
		a : gtk_allocation;
		
		-- The two zoom factors: one based on the width and another
		-- based on the height of the given area:
		sw, sh : type_zoom_factor;
	begin
		-- put_line ("get_ratio");

		-- Get the current width and height of the scrolled window:
		swin.get_allocation (a);

		-- Get the ratio of width and height based on the current dimensions
		-- of the scrolled window:
		sw := type_zoom_factor 
			(type_distance_model (a.width) / area.width);
		
		sh := type_zoom_factor 
			(type_distance_model (a.height) / area.height);

		-- CS: Alternatively the ratio can be based on the initial dimensions
		-- of the scrolled window. A boolean argument for this function 
		-- could be used to switch between current dimensions and initial 
		-- dimensions:
		-- sw := type_zoom_factor 
		-- 	(type_distance_model (swin_size_initial.width) / area.width);
		-- sh := type_zoom_factor 
		--	(type_distance_model (swin_size_initial.height) / area.height);
		
		-- put_line ("sw: " & to_string (sw));
		-- put_line ("sh: " & to_string (sh));

		-- The smaller of sw and sh has the final say:
		return type_zoom_factor'min (sw, sh);
	end get_ratio;
	


	procedure update_scrollbar_limits (
		C1, C2 : in type_bounding_box_corners)
	is
		debug : boolean := false;
		scratch : type_logical_pixels;

		HL : type_logical_pixels := to_lp (scrollbar_h_adj.get_lower);
		HU : type_logical_pixels := to_lp (scrollbar_h_adj.get_upper);

		VL : type_logical_pixels := to_lp (scrollbar_v_adj.get_lower);
		VU : type_logical_pixels := to_lp (scrollbar_v_adj.get_upper);

		dHL, dHU : type_logical_pixels;
		dVL, dVU : type_logical_pixels;
	begin
		if debug then
			put_line ("VL     " & to_string (VL));
			put_line ("VU     " & to_string (VU));

			put_line ("C1.TL.y" & to_string (C1.TL.y));
			put_line ("C1.BL.y" & to_string (C1.BL.y));

			put_line ("C2.TL.y" & to_string (C2.TL.y));
			put_line ("C2.BL.y" & to_string (C2.BL.y));
		end if;
		
		dHL := C2.BL.x - C1.BL.x;
		dHU := C2.BR.x - C1.BR.x;

		dVL := C2.TL.y - C1.TL.y;
		dVU := C2.BL.y - C1.BL.y;

		if debug then
			put_line ("dVL    " & to_string (dVL));
			put_line ("dVU    " & to_string (dVU));
		end if;
		

		-- horizontal:

		-- The left end of the scrollbar is the same as the position
		-- (value) of the scrollbar.
		-- If the left edge of the bounding-box is farther to the
		-- left than the left end of the bar, then the lower limit
		-- moves to the left. It assumes the value of the left edge
		-- of the bounding-box:
		HL := HL + dHL;
		if HL <= to_lp (scrollbar_h_adj.get_value) then
			clip_min (HL, 0.0); -- suppress negative value
			scrollbar_h_adj.set_lower (to_gdouble (HL));
		else
		-- If the left edge of the box is farther to the right than
		-- the left end of the bar, then the lower limit can not be
		-- moved further to the right. So the lower limit can at most assume
		-- the value of the left end of the bar:
			scrollbar_h_adj.set_lower (scrollbar_h_adj.get_value);
		end if;

		-- The right end of the scrollbar is the sum of its position (value)
		-- and its length (page size):
		scratch := to_lp (scrollbar_h_adj.get_value + 
						  scrollbar_h_adj.get_page_size);
		
		HU := HU + dHU;
		-- CS clip_max (HU, type_logical_pixels (scrolled_window_size.width));
		-- If the right edge of the bounding-box is farther to the
		-- right than the right end of the bar, then the upper limit
		-- moves to the right. It assumes the value of the right edge
		-- of the bounding-box:
		if HU >= scratch then
			scrollbar_h_adj.set_upper (to_gdouble (HU));
		else
		-- If the right edge of the box is farther to the left than
		-- the right end of the bar, then the upper limit can not be
		-- moved further to the left. So the upper limit can at most assume
		-- the value of the right end of the bar:
			scrollbar_h_adj.set_upper (to_gdouble (scratch));
		end if;

		
		-- vertical:

		-- The upper end of the scrollbar is the same as the position
		-- (value) of the scrollbar.
		-- If the upper edge of the bounding-box is higher
		-- than the upper end of the bar, then the lower limit
		-- moves upwards. It assumes the value of the upper edge
		-- of the bounding-box:
		VL := VL + dVL;
		if VL <= to_lp (scrollbar_v_adj.get_value) then
			clip_min (VL, 0.0); -- suppress negative value
			scrollbar_v_adj.set_lower (to_gdouble (VL));
		else
		-- If the upper edge of the box is below
		-- the upper end of the bar, then the lower limit can not be
		-- moved further upwards. So the lower limit can at most assume
		-- the value of the upper end of the bar:
			scrollbar_v_adj.set_lower (scrollbar_v_adj.get_value);
		end if;

		-- The lower end of the scrollbar is the sum of its position (value)
		-- and its length (page size):
		scratch := to_lp (scrollbar_v_adj.get_value + 
						  scrollbar_v_adj.get_page_size);
		
		VU := VU + dVU;
		-- CS clip_max (VU, 
		--   type_logical_pixels (scrolled_window_size.height));
		-- If the lower edge of the bounding-box is below the
		-- lower end of the bar, then the upper limit
		-- moves further downwards. It assumes the value of the lower edge
		-- of the bounding-box:
		if VU >= scratch then
			scrollbar_v_adj.set_upper (to_gdouble (VU));
		else
		-- If the lower edge of the box is above
		-- the lower end of the bar, then the upper limit can not be
		-- moved further downwards. So the upper limit can at most assume
		-- the value of the lower end of the bar:
			scrollbar_v_adj.set_upper (to_gdouble (scratch));
		end if;

		-- show_adjustments_v;
	end update_scrollbar_limits;

	

	


-- CANVAS:
	
	procedure stroke is begin
		cairo.stroke (context);
	end stroke;

	
	procedure refresh (
		canvas	: access gtk_widget_record'class)
	is
		drawing_area : constant gtk_drawing_area := 
			gtk_drawing_area (canvas);
	begin
		-- put_line ("refresh " & image (clock)); 
		drawing_area.queue_draw;
	end refresh;


	procedure compute_canvas_size is
		debug : boolean := true;

		-- The maximal base-offset:
		F_max : type_logical_pixels_vector;
		
		-- The maximum zoom factor:
		S_max : constant type_logical_pixels := 
			type_logical_pixels (type_zoom_factor'last);

		-- The maximum width and height of the bounding-box:
		Bw : constant type_logical_pixels := 
			type_logical_pixels (bounding_box_width_max);
		
		Bh : constant type_logical_pixels := 
			type_logical_pixels (bounding_box_height_max);
	begin
		if debug then
			put_line ("compute_canvas_size");
			put_line (" S_max : " & to_string (S_max));
			put_line (" Bw_max: " & to_string (Bw));
			put_line (" Bh_max: " & to_string (Bh));
		end if;

		-- compute the maximal base-offset:
		F_max.x :=   Bw * (S_max - 1.0);
		F_max.y := - Bh * S_max;

		if debug then
			put_line (" F_max : " & to_string (F_max));
		end if;

		-- compute the canvas width and height:
		canvas_size.width  := positive (  F_max.x + Bw * S_max);
		canvas_size.height := positive (- F_max.y + Bh * (S_max - 1.0));

		if debug then
			put_line (" Cw    : " & positive'image (canvas_size.width));
			put_line (" Ch    : " & positive'image (canvas_size.height));
		end if;
	end compute_canvas_size;



	
	procedure show_canvas_size is 
		-- use glib;
		a : gtk_allocation;
		width, height : gint;
	begin
		canvas.get_allocation (a);
		put_line ("canvas size allocated (w/h):" 
			& gint'image (a.width) & " /" & gint'image (a.height));
		
		canvas.get_size_request (width, height);
		put_line ("canvas size minimum   (w/h):" 
			& gint'image (width) & " /" & gint'image (height));
	end show_canvas_size;

	

	procedure create_canvas is
		use gdk.event;
		use glib;
	begin
		-- Set up the drawing area:
		gtk_new (canvas);

		-- Set the size (width and height) of the canvas:
		canvas.set_size_request (
			gint (canvas_size.width), gint (canvas_size.height));
		
		show_canvas_size;

		-- Make the canvas responding to mouse button clicks:
		canvas.add_events (gdk.event.button_press_mask);
		canvas.add_events (gdk.event.button_release_mask);

		-- Make the canvas responding to mouse movement:
		canvas.add_events (gdk.event.pointer_motion_mask);

		-- Make the canvas responding to the mouse wheel:
		canvas.add_events (gdk.event.scroll_mask);

		-- Make the canvas responding to the keyboard:
		canvas.set_can_focus (true);
		canvas.add_events (key_press_mask);

		
		-- Add the canvas as a child to the scrolled window:
		put_line ("add canvas to scrolled window");
		swin.add (canvas); 

		-- Insert the scrolled window in box_h:
		put_line ("add scrolled window to box_h");
		box_h.pack_start (swin);
		
	end create_canvas;



	procedure shift_canvas (
		direction	: type_direction;
		distance	: type_distance_model)
	is
		
		-- Convert the given model distance to 
		-- a canvas distance:
		d : constant type_logical_pixels := 
			type_logical_pixels (distance) * type_logical_pixels (S);

		-- Scratch values for upper limit, lower limit and value
		-- of scrollbars:
		u, l, v : type_logical_pixels;
	begin
		case direction is
			when DIR_RIGHT =>
				-- Get the maximal allowed value for the
				-- horizontal scrollbar:
				u := to_lp (scrollbar_h_adj.get_upper);

				-- Compute the required scrollbar position:
				v := to_lp (scrollbar_h_adj.get_value) + d;

				-- Clip the required position if necesary,
				-- then apply it to the scrollbar:
				clip_max (v, u);
				scrollbar_h_adj.set_value (to_gdouble (v));

				
			when DIR_LEFT =>
				-- Get the minimal allowed value for the
				-- horizontal scrollbar:
				l := to_lp (scrollbar_h_adj.get_lower);

				-- Compute the required scrollbar position:
				v := to_lp (scrollbar_h_adj.get_value) - d;

				-- Clip the required position if necesary,
				-- then apply it to the scrollbar:
				clip_min (v, l);
				scrollbar_h_adj.set_value (to_gdouble (v));

				
			when DIR_UP =>
				-- Get the minimal allowed value for the
				-- vertical scrollbar:
				l := to_lp (scrollbar_v_adj.get_lower);
				
				-- Compute the required scrollbar position:
				v := to_lp (scrollbar_v_adj.get_value) - d;

				-- Clip the required position if necesary,
				-- then apply it to the scrollbar:
				clip_min (v, l);
				scrollbar_v_adj.set_value (to_gdouble (v));

				
			when DIR_DOWN =>
				-- Get the maximal allowed value for the
				-- vertical scrollbar:
				u := to_lp (scrollbar_v_adj.get_upper);

				-- Compute the required scrollbar position:
				v := to_lp (scrollbar_v_adj.get_value) + d;

				-- Clip the required position if necesary,
				-- then apply it to the scrollbar:
				clip_max (v, u);
				scrollbar_v_adj.set_value (to_gdouble (v));
				
		end case;

		backup_scrollbar_settings;
	end shift_canvas;



	
-- COORDATES-DISPLAY:

	procedure set_up_coordinates_display is
		use glib;
		
		-- The width of the text view shall be wide enough
		-- to fit the greatest numbers:
		pos_field_width_min : constant gint := 80;
	begin
		-- CS To disable focus use
		-- procedure Set_Focus_On_Click
		--    (Widget         : not null access Gtk_Widget_Record;
		--     Focus_On_Click : Boolean);

		-- Create a table, that contains headers, text labels
		-- and text views for the actual coordinates:
		gtk_new (table, rows => 11, columns => 2, 
			homogeneous => false);
		-- table.set_col_spacings (50);
		-- table.set_border_width (10);

		-- The table shall not expand downward:
		box_v1.pack_start (table, expand => false);


		-- POINTER / MOUSE:
		gtk_new (pointer_header, "POINTER");		
		gtk_new (pointer_x_label, "x:"); -- create a text label

		-- The label shall be aligned in the column.
		-- The discussion at:
		-- <https://stackoverflow.com/questions/26345989/
		-- gtk-how-to-align-a-label-to-the-left-in-a-table>
		-- gave the solution. See also package gtk.misc for details:
		pointer_x_label.set_alignment (0.0, 0.0);	
		gtk_new (pointer_x_value); -- create a text view vor the value
		-- A minimum width must be set for the text.
		-- Setting the size request is one way. The height is
		-- not affected, therefore the value -1:
		pointer_x_value.set_size_request (pos_field_width_min, -1);
		-- See also discussion at:
		-- <https://stackoverflow.com/questions/24412859/
		-- gtk-how-can-the-size-of-a-textview-be-set-manually>
		-- for a way to achieve this using a tag.

		gtk_new (pointer_x_buf); -- create a text buffer

		-- align the value left
		pointer_x_value.set_justification (JUSTIFY_RIGHT);
		pointer_x_value.set_editable (false); -- the value is not editable
		pointer_x_value.set_cursor_visible (false); -- do not show a cursor

		gtk_new (pointer_y_label, "y:"); -- create a text label
		pointer_y_label.set_alignment (0.0, 0.0);	
		gtk_new (pointer_y_value);
		pointer_y_value.set_size_request (pos_field_width_min, -1);
		gtk_new (pointer_y_buf); -- create a text buffer
		
		-- align the value left
		pointer_y_value.set_justification (JUSTIFY_RIGHT); 
		pointer_y_value.set_editable (false); -- the value is not editable
		pointer_y_value.set_cursor_visible (false); -- do not show a cursor

		----------------------------------------------------------------------
		
		-- CURSOR
		gtk_new (cursor_header, "CURSOR");

		gtk_new (cursor_x_label, "x:");
		cursor_x_label.set_alignment (0.0, 0.0);	
		gtk_new (cursor_x_value);
		cursor_x_value.set_size_request (pos_field_width_min, -1);

		gtk_new (cursor_x_buf);
		cursor_x_value.set_justification (JUSTIFY_RIGHT);
		cursor_x_value.set_editable (false);
		cursor_x_value.set_cursor_visible (false);

		gtk_new (cursor_y_label, "y:");
		cursor_y_label.set_alignment (0.0, 0.0);	
		gtk_new (cursor_y_value);
		cursor_y_value.set_size_request (pos_field_width_min, -1);
		gtk_new (cursor_y_buf);
		cursor_y_value.set_justification (JUSTIFY_RIGHT);
		cursor_y_value.set_editable (false);
		cursor_y_value.set_cursor_visible (false);

		----------------------------------------------------------------------
		-- DISTANCES		
		gtk_new (distances_header, "DISTANCE");
		gtk_new (distances_dx_label, "dx:");
		distances_dx_label.set_alignment (0.0, 0.0);	
		gtk_new (distances_dx_value);
		distances_dx_value.set_size_request (pos_field_width_min, -1);

		gtk_new (distances_dx_buf);
		distances_dx_value.set_justification (JUSTIFY_RIGHT);
		distances_dx_value.set_editable (false);
		distances_dx_value.set_cursor_visible (false);

		
		gtk_new (distances_dy_label, "dy:");
		distances_dy_label.set_alignment (0.0, 0.0);	
		gtk_new (distances_dy_value);
		distances_dy_value.set_size_request (pos_field_width_min, -1);

		gtk_new (distances_dy_buf);
		distances_dy_value.set_justification (JUSTIFY_RIGHT);
		distances_dy_value.set_editable (false);
		distances_dy_value.set_cursor_visible (false);


		gtk_new (distances_absolute_label, "abs:");
		distances_absolute_label.set_alignment (0.0, 0.0);	
		gtk_new (distances_absolute_value);
		distances_absolute_value.set_size_request (pos_field_width_min, -1);

		gtk_new (distances_absolute_buf);
		distances_absolute_value.set_justification (JUSTIFY_RIGHT);
		distances_absolute_value.set_editable (false);
		distances_absolute_value.set_cursor_visible (false);


		gtk_new (distances_angle_label, "angle:");
		distances_angle_label.set_alignment (0.0, 0.0);	
		gtk_new (distances_angle_value);
		distances_angle_value.set_size_request (pos_field_width_min, -1);

		gtk_new (distances_angle_buf);
		distances_angle_value.set_justification (JUSTIFY_RIGHT);
		distances_angle_value.set_editable (false);
		distances_angle_value.set_cursor_visible (false);

		----------------------------------------------------------------------
		-- GRID
		gtk_new (grid_header, "GRID");
		gtk_new (grid_x_label, "x:");
		grid_x_label.set_alignment (0.0, 0.0);	
		gtk_new (grid_x_value);
		grid_x_value.set_size_request (pos_field_width_min, -1);

		gtk_new (grid_x_buf);
		grid_x_value.set_justification (JUSTIFY_RIGHT);
		grid_x_value.set_editable (false);
		grid_x_value.set_cursor_visible (false);


		gtk_new (grid_y_label, "y:");
		grid_y_label.set_alignment (0.0, 0.0);	
		gtk_new (grid_y_value);
		grid_x_value.set_size_request (pos_field_width_min, -1);

		gtk_new (grid_y_buf);
		grid_y_value.set_justification (JUSTIFY_RIGHT);
		grid_y_value.set_editable (false);
		grid_y_value.set_cursor_visible (false);		

		----------------------------------------------------------------------
		-- ZOOM FACTOR
		-- gtk_new (zoom_header, "ZOOM");
		gtk_new (zoom_label, "zoom:");
		zoom_label.set_alignment (0.0, 0.0);	
		gtk_new (zoom_value);
		zoom_value.set_size_request (pos_field_width_min, -1);

		gtk_new (zoom_buf);
		zoom_value.set_justification (JUSTIFY_RIGHT);
		zoom_value.set_editable (false);
		zoom_value.set_cursor_visible (false);

		----------------------------------------------------------------------
		-- SCALE
		gtk_new (scale_label, "scale:");
		scale_label.set_alignment (0.0, 0.0);	
		gtk_new (scale_value);
		scale_value.set_size_request (pos_field_width_min, -1);

		gtk_new (scale_buf);
		scale_value.set_justification (JUSTIFY_RIGHT);
		scale_value.set_editable (false);
		scale_value.set_cursor_visible (false);
		
		----------------------------------------------------------------------
		-- Put the items in the table:

		-- MOUSE / POINTER:
		table.attach (pointer_header, 
			left_attach	=> 0, right_attach	=> 2, 
			top_attach	=> 0, bottom_attach	=> 1);

		-- x-coordinate:
		table.attach (pointer_x_label, 
			left_attach	=> 0, right_attach	=> 1, 
			top_attach	=> 1, bottom_attach	=> 2);

		table.attach (pointer_x_value, 
			left_attach	=> 1, right_attach	=> 2, 
			top_attach	=> 1, bottom_attach	=> 2);

		-- y-coordinate:
		table.attach (pointer_y_label, 
			left_attach	=> 0, right_attach	=> 1, 
			top_attach	=> 2, bottom_attach	=> 3);
  
		table.attach (pointer_y_value, 
			left_attach	=> 1, right_attach	=> 2, 
			top_attach	=> 2, bottom_attach	=> 3);


		-- CURSOR:
		table.attach (cursor_header, 
			left_attach	=> 0, right_attach	=> 2, 
			top_attach	=> 3, bottom_attach	=> 4);

		-- x-coordinate:
		table.attach (cursor_x_label, 
			left_attach	=> 0, right_attach	=> 1, 
			top_attach	=> 4, bottom_attach	=> 5);

		table.attach (cursor_x_value, 
			left_attach	=> 1, right_attach	=> 2, 
			top_attach	=> 4, bottom_attach	=> 5);

		-- y-coordinate:
		table.attach (cursor_y_label, 
			left_attach	=> 0, right_attach	=> 1, 
			top_attach	=> 5, bottom_attach	=> 6);
  
		table.attach (cursor_y_value, 
			left_attach	=> 1, right_attach	=> 2, 
			top_attach	=> 5, bottom_attach	=> 6);



		-- DISTANCES:
		table.attach (distances_header, 
			left_attach	=> 0, right_attach	=> 2, 
			top_attach	=> 6, bottom_attach	=> 7);

		-- x-coordinate:
		table.attach (distances_dx_label, 
			left_attach	=> 0, right_attach	=> 1, 
			top_attach	=> 7, bottom_attach	=> 8);

		table.attach (distances_dx_value, 
			left_attach	=> 1, right_attach	=> 2, 
			top_attach	=> 7, bottom_attach	=> 8);

		-- y-coordinate:
		table.attach (distances_dy_label, 
			left_attach	=> 0, right_attach	=> 1, 
			top_attach	=> 9, bottom_attach	=> 10);
  
		table.attach (distances_dy_value, 
			left_attach	=> 1, right_attach	=> 2, 
			top_attach	=> 9, bottom_attach	=> 10);

		-- absolute:
		table.attach (distances_absolute_label, 
			left_attach	=> 0, right_attach	=> 1, 
			top_attach	=> 10, bottom_attach => 11);
  
		table.attach (distances_absolute_value, 
			left_attach	=> 1, right_attach	=> 2, 
			top_attach	=> 10, bottom_attach => 11);
		
		-- angle:
		table.attach (distances_angle_label, 
			left_attach	=> 0, right_attach	=> 1, 
			top_attach	=> 11, bottom_attach => 12);
  
		table.attach (distances_angle_value, 
			left_attach	=> 1, right_attach	=> 2, 
			top_attach	=> 11, bottom_attach => 12);


		
		-- GRID:
		table.attach (grid_header, 
			left_attach	=> 0, right_attach	=> 2, 
			top_attach	=> 12, bottom_attach => 13);

		-- x-axis:
		table.attach (grid_x_label, 
			left_attach	=> 0, right_attach	=> 1, 
			top_attach	=> 13, bottom_attach => 14);
  
		table.attach (grid_x_value, 
			left_attach	=> 1, right_attach	=> 2, 
			top_attach	=> 13, bottom_attach => 14);

		-- y-axis:
		table.attach (grid_y_label, 
			left_attach	=> 0, right_attach	=> 1, 
			top_attach	=> 14, bottom_attach => 15);
  
		table.attach (grid_y_value, 
			left_attach	=> 1, right_attach	=> 2, 
			top_attach	=> 14, bottom_attach => 15);

		
		-- ZOOM:
		table.attach (zoom_label, 
			left_attach	=> 0, right_attach	=> 1, 
			top_attach	=> 15, bottom_attach => 16);
  
		table.attach (zoom_value, 
			left_attach	=> 1, right_attach	=> 2, 
			top_attach	=> 15, bottom_attach => 16);

		
		-- SCALE:
		table.attach (scale_label, 
			left_attach	=> 0, right_attach	=> 1, 
			top_attach	=> 16, bottom_attach => 17);
  
		table.attach (scale_value, 
			left_attach	=> 1, right_attach	=> 2, 
			top_attach	=> 16, bottom_attach => 17);

	end set_up_coordinates_display;


	procedure update_cursor_coordinates is 
	begin
		-- x-axis:
		cursor_x_buf.set_text (to_string (to_reality (cursor.position.x)));
		cursor_x_value.set_buffer (cursor_x_buf);
 
		-- y-axis:
		cursor_y_buf.set_text (to_string (to_reality (cursor.position.y)));
		cursor_y_value.set_buffer (cursor_y_buf);
	end update_cursor_coordinates;


	
	procedure update_distances_display is 
		-- use glib;
	
		px, py : gint; -- the pointer position
		cp : type_logical_pixels_vector;
		mp : type_vector_model;

		dx, dy : type_distance_model;
		dabs : type_distance_model;
		angle : type_rotation_model;
	begin
		-- Get the current pointer/mouse position:
		canvas.get_pointer (px, py);
		cp := (type_logical_pixels (px), type_logical_pixels (py));
		
		-- Convert the pointer position to a real
		-- point in the model:
		mp := canvas_to_real (cp, S);

		-- Compute the relative distance from cursor
		-- to pointer:
		dx := mp.x - cursor.position.x;
		dy := mp.y - cursor.position.y;

		-- Compute the absolute distance from
		-- cursor to pointer:
		dabs := get_distance (
			p1 => (0.0, 0.0),
			p2 => (dx, dy));

		-- Compute the angle of direction from cursor
		-- to pointer:
		angle := get_angle (
			p1 => (0.0, 0.0),
			p2 => (dx, dy));

		
		-- Output the relative distances on the display:

		-- dx:
		distances_dx_buf.set_text (to_string (to_reality (dx)));
		distances_dx_value.set_buffer (distances_dx_buf);

		-- dy:
		distances_dy_buf.set_text (to_string (to_reality (dy)));
		distances_dy_value.set_buffer (distances_dy_buf);

		-- absolute:
		distances_absolute_buf.set_text (to_string (to_reality (dabs)));
		distances_absolute_value.set_buffer (distances_absolute_buf);

		-- angle:
		distances_angle_buf.set_text (to_string (angle));
		distances_angle_value.set_buffer (distances_angle_buf);
	end update_distances_display;



	procedure update_zoom_display is begin
		zoom_buf.set_text (to_string (S));
		zoom_value.set_buffer (zoom_buf);
	end update_zoom_display;



	procedure update_grid_display is 
	begin
		-- x-axis:
		grid_x_buf.set_text (to_string (to_reality (grid.spacing.x)));
		grid_x_value.set_buffer (grid_x_buf);

		-- y-axis:
		grid_y_buf.set_text (to_string (to_reality (grid.spacing.y)));
		grid_y_value.set_buffer (grid_y_buf);
	end update_grid_display;

	
	procedure update_scale_display is 
	begin
		scale_buf.set_text (to_string (M));
		scale_value.set_buffer (scale_buf);
	end update_scale_display;

	
	


-- GRID:

	procedure set_grid_to_scale is
	begin
		to_model (grid.spacing.x);
		to_model (grid.spacing.y);
	end set_grid_to_scale;

	
	
	function snap_to_grid (
		point : in type_vector_model)
		return type_vector_model
	is
		n : integer;
		type type_float is new float; -- CS refinement required
		f : type_float;
		result : type_vector_model;
	begin
		n := integer (point.x / grid.spacing.x);
		f := type_float (n) * type_float (grid.spacing.x);
		result.x := type_distance_model (f);

		n := integer (point.y / grid.spacing.y);
		f := type_float (n) * type_float (grid.spacing.y);
		result.y := type_distance_model (f);
		
		return result;
	end snap_to_grid;


	function get_grid_spacing (
		grid : in type_grid)
		return type_logical_pixels_positive
	is
		sg : constant type_logical_pixels := type_logical_pixels (S);
		x, y : type_logical_pixels;
	begin
		x := type_logical_pixels (grid.spacing.x) * sg;
		y := type_logical_pixels (grid.spacing.y) * sg;
		return type_logical_pixels_positive'min (x, y);
	end get_grid_spacing;



	procedure draw_grid is
		use cairo;
		-- use demo_conversions;
		-- use demo_grid;
		-- use demo_visible_area;
		-- use demo_canvas;
		
		type type_float_grid is new float; -- CS refinement required

		-- X-AXIS:

		-- The first and the last column:
		x1, x2 : type_distance_model;

		-- The start and the end of the visible area:
		ax1 : constant type_float_grid := 
			type_float_grid (visible_area.position.x);
		
		ax2 : constant type_float_grid := 
			ax1 + type_float_grid (visible_area.width);

		-- The grid spacing:
		gx : constant type_float_grid := 
			type_float_grid (grid.spacing.x);

		
		-- Y-AXIS:

		-- The first and the last row:
		y1, y2 : type_distance_model;

		-- The start and the end of the visible area:
		ay1 : constant type_float_grid := 
			type_float_grid (visible_area.position.y);
		
		ay2 : constant type_float_grid := 
			ay1 + type_float_grid (visible_area.height);

		-- The grid spacing:
		gy : constant type_float_grid := 
			type_float_grid (grid.spacing.y);

		c : type_float_grid;

		-- debug : boolean := false;

		
		procedure compute_first_and_last_column is begin
			-- Compute the first column:
			-- put_line (" ax1 " & type_float_grid'image (ax1));
			c := type_float_grid'floor (ax1 / gx);
			x1 := type_distance_model ((gx * c) + gx);
			-- put_line (" x1  " & type_distance_model'image (x1));

			-- Compute the last column:
			-- put_line (" ax2 " & type_float_grid'image (ax2));
			c := type_float_grid'floor (ax2 / gx);
			x2 := type_distance_model (gx * c);
			-- put_line (" x2  " & type_distance_model'image (x2));
		end compute_first_and_last_column;


		procedure compute_first_and_last_row is begin
			-- Compute the first row:
			-- put_line (" ay1 " & type_float_grid'image (ay1));
			c := type_float_grid'floor (ay1 / gy);
			y1 := type_distance_model ((gy * c) + gy);
			-- put_line (" y1  " & type_distance_model'image (y1));

			-- Compute the last row:
			-- put_line (" ay2 " & type_float_grid'image (ay2));
			c := type_float_grid'floor (ay2 / gy);
			y2 := type_distance_model (gy * c);
			-- put_line (" y2  " & type_distance_model'image (y2));
		end compute_first_and_last_row;
		

		-- This procedure draws the dots of the grid:
		-- 1. Assemble from the first row and the first colum a real
		--    model point MP.
		-- 2. Advance PM from row to row and column to column in a 
		--    matrix like order.
		-- 3. Draw a very small circle, which will appear like a dot,
		--    (or alternatively a very small cross) at MP.
		procedure draw_dots is 
			MP : type_vector_model;
			CP : type_logical_pixels_vector;
		begin
			-- Set the linewidth of the dots:
			set_line_width (context, to_gdouble (grid_width_dots));
			
			-- Compose a model point from the first column and 
			-- the first row:
			MP := (x1, y1);

			-- Advance PM from column to column:
			while MP.x <= x2 loop

				-- Advance PM from row to row:
				MP.y := y1;
				while MP.y <= y2 loop
					-- Convert the current real model point MP to a
					-- point on the canvas:
					CP := real_to_canvas (MP, S);

					-- Draw a very small circle with its center at CP:
					-- arc (context, CP.x, CP.y, 
					-- 	 radius => grid_radius_dots, angle1 => 0.0, 
					--    angle2 => 6.3);
					-- stroke (context);

					-- Alternatively, draw a very small cross at CP.
					-- This could be more efficient than a circle:
					
					-- horizontal line:
					move_to (context, 
						to_gdouble (CP.x - grid_cross_arm_length),
						to_gdouble (CP.y));
					
					line_to (context, 
						to_gdouble (CP.x + grid_cross_arm_length),
						to_gdouble (CP.y));
						

					-- vertical line:
					move_to (context, 
						to_gdouble (CP.x), 
						to_gdouble (CP.y - grid_cross_arm_length));
					
					line_to (context,
						to_gdouble (CP.x),
						to_gdouble (CP.y + grid_cross_arm_length));

											
					-- Advance one row up:
					MP.y := MP.y + grid.spacing.y;
				end loop;

				-- Advance one column to the right:
				MP.x := MP.x + grid.spacing.x;
			end loop;
		end draw_dots;


		-- This procedure draws the lines of the grid:
		procedure draw_lines is 
			MP1 : type_vector_model;
			MP2 : type_vector_model;

			CP1 : type_logical_pixels_vector;
			CP2 : type_logical_pixels_vector;

			ax1f : type_distance_model := visible_area.position.x;
			ax2f : type_distance_model := ax1f + visible_area.width;
			
			ay1f : type_distance_model := visible_area.position.y;
			ay2f : type_distance_model := ay1f + visible_area.height;
		begin
			-- Set the linewidth of the lines:
			set_line_width (context, to_gdouble (grid_width_lines));
			
			-- VERTICAL LINES:

			-- All vertical lines start at the bottom of 
			-- the visible area:
			MP1 := (x1, ay1f);

			-- All vertical lines end at the top of the 
			-- visible area:
			MP2 := (x1, ay2f);

			-- The first vertical line runs along the first column. 
			-- The last vertical line runs along the last column.
			-- This loop advances from one column to the next and
			-- draws a vertical line:
			while MP1.x <= x2 loop
				CP1 := real_to_canvas (MP1, S);
				CP2 := real_to_canvas (MP2, S);
				
				move_to (context, 
					to_gdouble (CP1.x), to_gdouble (CP1.y));
				
				line_to (context, 
					to_gdouble (CP2.x), to_gdouble (CP2.y));

				MP1.x := MP1.x + grid.spacing.x;
				MP2.x := MP2.x + grid.spacing.x;
			end loop;

			
			-- HORIZONTAL LINES:

			-- All horizontal lines start at the left edge of the 
			-- visible area:
			MP1 := (ax1f, y1);

			-- All horizontal lines end at the right edge of the 
			-- visible area:
			MP2 := (ax2f, y1);

			-- The first horizontal line runs along the first row. 
			-- The last horizontal line runs along the last row.
			-- This loop advances from one row to the next and
			-- draws a horizontal line:
			while MP1.y <= y2 loop
				CP1 := real_to_canvas (MP1, S);
				CP2 := real_to_canvas (MP2, S);
				
				move_to (context, 
					to_gdouble (CP1.x), to_gdouble (CP1.y));

				line_to (context, 
					to_gdouble (CP2.x), to_gdouble (CP2.y));

				MP1.y := MP1.y + grid.spacing.y;
				MP2.y := MP2.y + grid.spacing.y;
			end loop;
		end draw_lines;
		

	begin -- draw_grid

		-- Draw the grid if it is enabled and if the spacing
		-- is greater than the minimal required spacing:
		if grid.on = GRID_ON and then
			get_grid_spacing (grid) >= grid_spacing_min then

			
			-- put_line ("draw_grid");
			compute_first_and_last_column;
			compute_first_and_last_row;

			-- Set the color of the grid:
			set_source_rgb (context, 0.5, 0.5, 0.5); -- gray

			case grid.style is
				when STYLE_DOTS =>
					draw_dots;

				when STYLE_LINES =>
					draw_lines;
			end case;

			-- Since all dots or lines are
			-- drawn with the same linewidth and color
			-- this single stroke command is sufficient:
			stroke;
		end if;
		
	end draw_grid;


-- CURSOR:

	procedure move_cursor (
		destination : type_vector_model)
	is begin
		cursor.position := destination;
		update_cursor_coordinates;
		update_distances_display;
		
		-- Output the cursor position on the terminal:
		put_line ("position " & to_string (cursor.position));
	end move_cursor;



	procedure move_cursor (
		direction : type_direction)
	is 
	begin
		-- Move the cursor by the grid spacing into the given direction:
		put_line ("move cursor " & type_direction'image (direction));
		
		case direction is
			when DIR_RIGHT =>
				cursor.position.x := cursor.position.x + grid.spacing.x;

			when DIR_LEFT =>
				cursor.position.x := cursor.position.x - grid.spacing.x;

			when DIR_UP =>
				cursor.position.y := cursor.position.y + grid.spacing.y;

			when DIR_DOWN =>
				cursor.position.y := cursor.position.y - grid.spacing.y;
		end case;

		-- CS Limit cursor position to range of type_distance_model
		-- Exception handler ?

		-- If the cursor is outside the visible area, then the
		-- canvas must be shifted with the cursor:
		if not in_area (cursor.position, visible_area) then
			put_line ("cursor not in visible area");

			case direction is
				when DIR_RIGHT =>
					-- If the cursor is right of the visible area,
					-- then shift the canvas to the right:
					if cursor.position.x > 
						visible_area.position.x + visible_area.width then
						shift_canvas (direction, grid.spacing.x);
					end if;
					
				when DIR_LEFT =>
					-- If the cursor is left of the visible area,
					-- then shift the canvas to the left:
					if cursor.position.x < visible_area.position.x then
						shift_canvas (direction, grid.spacing.x);
					end if;
					
				when DIR_UP =>
					-- If the cursor is above of the visible area,
					-- then shift the canvas up:
					if cursor.position.y > 
						visible_area.position.y + visible_area.height then
						shift_canvas (direction, grid.spacing.y);
					end if;

				when DIR_DOWN =>
					-- If the cursor is below of the visible area,
					-- then shift the canvas down:
					if cursor.position.y < visible_area.position.y then
						shift_canvas (direction, grid.spacing.y);
					end if;

			end case;

		end if;
			
		refresh (canvas);		
		
		update_cursor_coordinates;
		update_distances_display;

		-- Output the cursor position on the terminal:
		put_line ("cursor at " & to_string (cursor.position));

		backup_visible_area (get_visible_area (canvas));
	end move_cursor;
	


	procedure draw_cursor is
		use cairo;
		
		cp : type_logical_pixels_vector := 
			real_to_canvas (cursor.position, S);

		-- These are the start and stop positions for the
		-- horizontal lines:
		h1, h2, h3, h4 : type_logical_pixels;

		-- These are the start and stop positions for the
		-- vertical lines:
		v1, v2, v3, v4 : type_logical_pixels;

		-- This is the total length of an arm:
		l : constant type_logical_pixels := 
			cursor.length_1 + cursor.length_2;
		
	begin
		set_source_rgb (context, 0.5, 0.5, 0.5); -- gray

		-- Compute the start and stop positions:
		h1 := cp.x - l;
		h2 := cp.x - cursor.length_1;
		h3 := cp.x + cursor.length_1;
		h4 := cp.x + l;
		
		v1 := cp.y - l;
		v2 := cp.y - cursor.length_1;
		v3 := cp.y + cursor.length_1;
		v4 := cp.y + l;

		-- Draw the horizontal line from left to right:
		-- thick
		set_line_width (context, to_gdouble (cursor.linewidth_2));
		move_to (context, to_gdouble (h1), to_gdouble (cp.y));
		line_to (context, to_gdouble (h2), to_gdouble (cp.y));
		stroke;

		-- thin
		set_line_width (context, to_gdouble (cursor.linewidth_1));
		move_to (context, to_gdouble (h2), to_gdouble (cp.y));
		line_to (context, to_gdouble (h3), to_gdouble (cp.y));
		stroke;

		-- thick
		set_line_width (context, to_gdouble (cursor.linewidth_2));
		move_to (context, to_gdouble (h3), to_gdouble (cp.y));
		line_to (context, to_gdouble (h4), to_gdouble (cp.y));
		stroke;
		
		-- Draw the vertical line from top to bottom:
		-- thick
		move_to (context, to_gdouble (cp.x), to_gdouble (v1));
		line_to (context, to_gdouble (cp.x), to_gdouble (v2));
		stroke;

		-- thin
		set_line_width (context, to_gdouble (cursor.linewidth_1));
		move_to (context, to_gdouble (cp.x), to_gdouble (v2));
		line_to (context, to_gdouble (cp.x), to_gdouble (v3));
		stroke;

		-- thick
		set_line_width (context, to_gdouble (cursor.linewidth_2));
		move_to (context, to_gdouble (cp.x), to_gdouble (v3));
		line_to (context, to_gdouble (cp.x), to_gdouble (v4));
		stroke;

		-- arc
		set_line_width (context, to_gdouble (cursor.linewidth_1));
		arc (context, to_gdouble (cp.x), to_gdouble (cp.y), 
				radius => to_gdouble (cursor.radius), 
				angle1 => 0.0, angle2 => 6.3);
		
		stroke;

		-- CS: To improve performance on drawing, it might help
		-- to draw all objects which have a thin line first, then
		-- all object with a thick line.
	end draw_cursor;



	

-- SCALE:

	function to_string (
		scale : in type_scale)
		return string
	is 
		use pac_scale_io;
		use ada.strings.bounded;
		use ada.strings;
		use ada.strings.fixed;

		package pac_scale_bounded is new generic_bounded_length (10);
		use pac_scale_bounded;
		
		m_bounded : pac_scale_bounded.bounded_string;

		-- This string holds temporarily the given scale.
		-- The length of the string should be set in advance
		-- here in order to take the longest possible combination
		-- of charecters according to the declaration of type_scale.
		-- Mind, the comma/point. It must also taken into account here:
		m_fixed : string (1 .. type_scale'digits + 3);
		-- CS find something more elegantly here.

		m_reciprocal : type_scale;
	begin
		--put_line ("scale" & type_scale'image (scale));

		-- Since we want an output like 1:100 or 100:1 the given scale
		-- must be checked whether it is greater or less than 1.0:
		if scale >= 1.0 then

			-- Output the given scale to a fixed string
			-- without an exponent:
			put (
				to		=> m_fixed, -- like 100.00
				item	=> scale,
				exp		=> 0); -- no exponent

			-- Trim the string on both ends and store it in m_bounded:
			m_bounded := trim (to_bounded_string (m_fixed), both);
			-- CS remove leading zeroes after the comma.

			-- Return a nicely formatted expression like 1:100
			return "1:" & to_string (m_bounded);
		else
			-- The scale is smaller than 1.0. So we first 
			-- calculate the reciprocal of scale.
			-- For example: scale 0.01 turns to 100.0:
			m_reciprocal := 1.0 / scale;

			-- Output the given scale to a fixed string
			-- without an exponent:
			put (
				to		=> m_fixed, -- like 100.0
				item	=> m_reciprocal,
				exp		=> 0); -- no exponent

			-- Trim the string on both ends and store it in m_bounded:
			m_bounded := trim (to_bounded_string (m_fixed), both);
			-- CS remove leading zeroes after the comma.
			
			-- Return a nicely formatted expression like 100:1
			return to_string (m_bounded) & ":1";
		end if;
	end to_string;


	function to_reality (
		d : in type_distance_model)
		return type_distance_model
	is begin
		return type_distance_model_positive (M) * d;
	end to_reality;


	procedure to_reality (
		d : in out type_distance_model)
	is begin
		d := type_distance_model_positive (M) * d;
	end to_reality;


	
	function to_model (
		d : in type_distance_model)
		return type_distance_model
	is begin
		return type_distance_model_positive (1.0 / M) * d;
	end to_model;

	procedure to_model (
		d : in out type_distance_model)
	is begin
		d := type_distance_model_positive (1.0 / M) * d;
	end to_model;

	

	function to_reality (
		v : in type_vector_model)
		return type_vector_model
	is begin
		return (x => to_reality (v.x), y => to_reality (v.y));
	end to_reality;

	
	function to_model (
		v : in type_vector_model)
		return type_vector_model
	is begin
		return (x => to_model (v.x), y => to_model (v.y));
	end to_model;



	
-- BUTTONS:

	procedure create_buttons is begin
		put_line ("create_buttons");
		
		gtk_new_vbox (box_v0);
		box_h.pack_start (box_v0, expand => false);


		gtk_new (buttons_table, rows => 5, columns => 1, 
			homogeneous => false);
		-- table.set_col_spacings (50);
		-- table_coordinates.set_border_width (10);


		gtk_new (button_zoom_fit, "ZOOM FIT");
		gtk_new (button_zoom_area, "ZOOM AREA");
		gtk_new (button_add, "ADD");
		gtk_new (button_delete, "DELETE");
		gtk_new (button_move, "MOVE");
		gtk_new (button_export, "EXPORT");
		-- CS add other buttons
		

		
		-- The table shall not expand downward:
		box_v0.pack_start (buttons_table, expand => false);

		
		buttons_table.attach (button_zoom_fit,
			left_attach => 0, right_attach => 1,
			top_attach  => 0, bottom_attach => 1);

		buttons_table.attach (button_zoom_area,
			left_attach => 0, right_attach => 1,
			top_attach  => 1, bottom_attach => 2);
		
		buttons_table.attach (button_add,
			left_attach => 0, right_attach => 1,
			top_attach  => 2, bottom_attach => 3);

		buttons_table.attach (button_delete,
			left_attach => 0, right_attach => 1,
			top_attach  => 3, bottom_attach => 4);

		buttons_table.attach (button_move,
			left_attach => 0, right_attach => 1,
			top_attach  => 4, bottom_attach => 5);

		buttons_table.attach (button_export,
			left_attach => 0, right_attach => 1,
			top_attach  => 5, bottom_attach => 6);
				
	end create_buttons;	



-----------------------------------------------------------------------	
-- INITIALISATION AND CALLBACKS:


	procedure cb_zoom_area (
		button : access gtk_button_record'class)
	is
		-- debug : boolean := true;
		debug : boolean := false;
	begin
		put_line ("cb_zoom_area");

		zoom_area.active := true;
	end cb_zoom_area;

	

	procedure cb_add (
		button : access gtk_button_record'class)
	is begin
		put_line ("cb_add");
		-- add_object;

		-- Redraw the canvas:
		refresh (canvas);
	end cb_add;

	
	procedure cb_delete (
		button : access gtk_button_record'class)
	is begin
		put_line ("cb_delete");
		-- delete_object;

		-- Redraw the canvas:
		refresh (canvas);
	end cb_delete;

	
	procedure cb_move (
		button : access gtk_button_record'class)
	is begin
		put_line ("cb_move");
		-- CS
	end cb_move;
	

	procedure cb_export (
		button : access gtk_button_record'class)
	is
	begin
		put_line ("cb_export");
		-- CS
	end cb_export;


	
	
	
-- MAIN WINDOW:
	
	procedure cb_terminate (
		window : access gtk_widget_record'class) 
	is begin
		put_line ("cb_terminate");
		gtk.main.main_quit;
	end cb_terminate;


	procedure cb_window_focus (
		window : access gtk_window_record'class) 
	is begin
		put_line ("cb_window_focus");
	end cb_window_focus;

	
	function cb_window_button_pressed (
		window	: access gtk_widget_record'class;
		event	: gdk_event_button)
		return boolean
	is
		use glib;
		event_handled : boolean := true;

		-- The point where the operator has clicked:
		point : constant type_logical_pixels_vector :=
			(to_lp (event.x), to_lp (event.y));
	begin
		null;
		
		-- Output the button id, x and y position:
		put_line ("cb_window_button_pressed "
		 	& "button" & guint'image (event.button) & " "
		 	& to_string (point));
		
		return event_handled;
	end cb_window_button_pressed;



		
		
	procedure cb_main_window_size_allocate (
		window		: access gtk_widget_record'class;
		allocation	: gtk_allocation)
	is 
	begin
		null;		
		-- put_line ("cb_main_window_size_allocate " & image (clock)); 

		-- put_line ("cb_window_size_allocate. (x/y/w/h): " 
		-- 	& gint'image (allocation.x) 
		-- 	& " /" & gint'image (allocation.y)
		-- 	& " /" & gint'image (allocation.width)
		-- 	& " /" & gint'image (allocation.height));		
	end cb_main_window_size_allocate;


	
	function cb_main_window_configure (
		window		: access gtk_widget_record'class;
		event		: gdk.event.gdk_event_configure)
		return boolean
	is
		result : boolean := false;
	begin
		-- put_line ("cb_main_window_configure " & image (clock)); 
		return result;
	end cb_main_window_configure;


	procedure cb_main_window_realize (
		window	: access gtk_widget_record'class)
	is begin
		null;
		-- put_line ("cb_main_window_realize " & image (clock)); 
	end cb_main_window_realize;
	

	function cb_main_window_state_change (
		window		: access gtk_widget_record'class;
		event		: gdk.event.gdk_event_window_state)
		return boolean
	is
		result : boolean := false;
	begin
		-- put_line ("cb_main_window_state_change " & image (clock)); 
		return result;
	end cb_main_window_state_change;


	procedure cb_main_window_activate (
		window		: access gtk_window_record'class)
	is begin
		null;
		-- put_line ("cb_main_window_activate " & image (clock)); 
	end cb_main_window_activate;


	procedure set_up_command_buttons is
	begin
		put_line ("set_up_command_buttons (general)");

		create_buttons;

		-- Connect button signals with subprograms:
		
		-- button_zoom_area.on_clicked (cb_zoom_area'access);
		button_zoom_area.on_clicked (access_cb_zoom_area);
		
		--button_add.on_clicked (cb_add'access);
		button_add.on_clicked (access_cb_add);
		
		--button_delete.on_clicked (cb_delete'access);
		button_delete.on_clicked (access_cb_delete);
		
		-- button_move.on_clicked (cb_move'access);
		button_move.on_clicked (access_cb_move);
		
		-- button_export.on_clicked (cb_export'access);
		button_export.on_clicked (access_cb_export);

	end set_up_command_buttons;

	
	
	procedure set_up_main_window is begin
		-- put_line ("set_up_main_window (general)");
		log (text => "set_up_main_window (general)", level => log_threshold);
		
		create_window; -- incl. boxes and a separator	
		
		-- connect signals:
		--main_window.on_destroy (cb_terminate'access);
		main_window.on_destroy (access_cb_terminate);
		
		--main_window.on_size_allocate (cb_main_window_size_allocate'access);
		main_window.on_size_allocate (access_cb_main_window_size_allocate);
		
		--main_window.on_button_press_event (cb_window_button_pressed'access);
		main_window.on_button_press_event (access_cb_window_button_pressed);
		
		--main_window.on_configure_event (cb_main_window_configure'access);
		main_window.on_configure_event (access_cb_main_window_configure);
		
		-- main_window.on_window_state_event (cb_main_window_state_change'access);
		main_window.on_window_state_event (access_cb_main_window_state_change);
		
		-- main_window.on_realize (cb_main_window_realize'access);
		main_window.on_realize (access_cb_main_window_realize);
		
		--main_window.on_activate_default (cb_main_window_activate'access);
		main_window.on_activate_default (access_cb_main_window_activate);

		-- Not used:
		-- main_window.on_activate_focus (cb_window_focus'access);

		set_up_command_buttons;

	end set_up_main_window;



	
	
	procedure cb_swin_size_allocate (
		swin		: access gtk_widget_record'class;
		allocation	: gtk_allocation)
	is 
		-- Each time ths procedure is called, the argument "allocation"
		-- provides the new size of the scrolled window. Later this size will 
		-- be compared with the old size (stored in global 
		-- variable scrolled_window_size):
		new_size : constant type_window_size := (
			width	=> positive (allocation.width),
			height	=> positive (allocation.height));

		-- This is the difference between new width and old width:
		dW : type_logical_pixels;

		-- This is the difference between new height and old height:
		dH : type_logical_pixels;


		-- For debugging. Outputs the dimensions and size
		-- changes of the main window:
		procedure show_size is begin
			put_line (" size old (w/h): " 
				& positive'image (swin_size.width)
				& " /" & positive'image (swin_size.height));
			
			put_line (" size new (w/h): " 
				& positive'image (new_size.width)
				& " /" & positive'image (new_size.height));

			put_line (" dW : " & to_string (dW));
			put_line (" dH : " & to_string (dH));

			-- put_line ("S1:" & to_string (S1));
		end show_size;
		
		
		-- When the scrolled window is resized, then it expands away
		-- from its top left corner or it shrinks toward its top-left
		-- corner. In both cases the bottom of the
		-- window moves down or up. So the bottom of the canvas must 
		-- follow the bottom of the scrolled window. This procedure 
		-- moves the bottom of the canvas by the same
		-- extent as the bottom of the scrolled window:
		procedure move_canvas_bottom is begin
			-- Approach 1:
			-- One way to move the canvas is to change the y-component
			-- of the base-offset.
			F.y := F.y - dh;

			-- Schedule a refresh to make the size change appear smoothly:
			refresh (canvas);
			
			-- Approach 2: -- CS never tried
			-- Modify the y-component of the translate-offset
		end move_canvas_bottom;
		

		

		-- This procedure zooms to the area, stored in last_visible_area,
		-- so that it fits into the current scrolled window.
		-- It is required for MODE_3_ZOOM_FIT:
		procedure zoom_visible_area is 
			-- Get the corners of the bounding-box on the canvas before 
			-- and after zooming:
			C1, C2 : type_bounding_box_corners;			
		begin
			C1 := get_bounding_box_corners;

			-- Reset the translate-offset:
			T := (0.0, 0.0);			

			-- Fit the last visible area into the current
			-- scrolled window:
			zoom_to_fit (last_visible_area);

			C2 := get_bounding_box_corners;
			update_scrollbar_limits (C1, C2);
			backup_scrollbar_settings;			
		end zoom_visible_area;

		
		-- This procedure moves the canvas so that the center of the visible
		-- area remains in the center.
		-- This procedure is required when zoom mode MODE_KEEP_CENTER is 
		-- enabled:
		procedure move_center is
		begin
			F.x := F.x + dW * 0.5;
			F.y := F.y + dH * 0.5;
			-- put_line ("F : " & to_string (F));
		end move_center;

		
	begin -- cb_swin_size_allocate
		
		-- put_line ("cb_swin_size_allocate " & image (clock)); 
		-- put_line ("cb_swin_size_allocate. (x/y/w/h): " 
		-- 	& gint'image (allocation.x) 
		-- 	& " /" & gint'image (allocation.y)
		-- 	& " /" & gint'image (allocation.width)
		-- 	& " /" & gint'image (allocation.height));

		-- This procedure is called on many occasions. We are interested
		-- only in cases where the size changes.
		-- So we watch for changes of width and height only:
		
		-- Compare the new size with the old size. The global variable 
		-- swin_size provides the size of the window BEFORE this
		-- procedure has been called. If the size has changed, then proceed
		-- with other actions. If the size has not changed, then nothing 
		-- happens:
		if new_size /= swin_size then
			new_line;
			put_line ("scrolled window size changed");

			-- Opon resizing the scrolled window, the settings of the 
			-- scrollbars (upper, lower and page size) adapt to the size of
			-- the scrolled window. But we do NOT want this behaviour. 
			-- Instead we restore the settings
			-- as they where BEFORE this procedure has been called:
			restore_scrollbar_settings;
			-- show_adjustments_h;
			-- show_adjustments_v;
			
			-- Compute the change of width and height:
			dW := type_logical_pixels (new_size.width - swin_size.width);
			dH := type_logical_pixels (new_size.height - swin_size.height);

			-- for debugging:
			-- show_size;

			-- Move the canvas so that its bottom follows
			-- the bottom of the scrolled window:
			move_canvas_bottom;
			

			case zoom_mode is
				when MODE_1_EXPOSE_CANVAS =>
					null; -- nothing more to do
					
				when MODE_2_KEEP_CENTER =>
					move_center;
					
				when MODE_3_ZOOM_FIT =>
					zoom_visible_area;

			end case;
			

			-- Update the swin_size which is required
			-- for the next time this procedure is called:
			swin_size := new_size;

		end if;
	end cb_swin_size_allocate;


	

	procedure set_up_swin_and_scrollbars is	begin
		put_line ("set_up_swin_and_scrollbars");

		create_scrolled_window_and_scrollbars;		

		
		-- connect signals:
		-- swin.on_size_allocate (cb_swin_size_allocate'access);
		swin.on_size_allocate (access_cb_swin_size_allocate);
		-- After executing procedure cb_swin_size_allocate
		-- the canvas is refreshed (similar to refresh (canvas)) 
		-- automatically.

		-- Connect the signal "value-changed" of the scrollbars with
		-- procedures cb_vertical_moved and cb_horizontal_moved. So the user
		-- can watch how the signals are emitted:
		--scrollbar_v_adj.on_value_changed (cb_vertical_moved'access);
		scrollbar_v_adj.on_value_changed (access_cb_vertical_moved);
		--scrollbar_h_adj.on_value_changed (cb_horizontal_moved'access);
		scrollbar_h_adj.on_value_changed (access_cb_horizontal_moved);

		scrollbar_v := swin.get_vscrollbar;
		-- scrollbar_v.on_button_press_event (cb_scrollbar_v_pressed'access);
		scrollbar_v.on_button_press_event (access_cb_scrollbar_v_pressed);
		--scrollbar_v.on_button_release_event (cb_scrollbar_v_released'access);
		scrollbar_v.on_button_release_event (access_cb_scrollbar_v_released);

		scrollbar_h := swin.get_hscrollbar;
		--scrollbar_h.on_button_press_event (cb_scrollbar_h_pressed'access);
		scrollbar_h.on_button_press_event (access_cb_scrollbar_h_pressed);
		-- scrollbar_h.on_button_release_event (cb_scrollbar_h_released'access);
		scrollbar_h.on_button_release_event (access_cb_scrollbar_h_released);

		
		update_cursor_coordinates;
	end set_up_swin_and_scrollbars;


	
	
	procedure cb_horizontal_moved (
		scrollbar : access gtk_adjustment_record'class)
	is begin
		-- put_line ("horizontal moved " & image (clock));
		-- show_adjustments_h;
		refresh (canvas);
	end cb_horizontal_moved;

	
	procedure cb_vertical_moved (
		scrollbar : access gtk_adjustment_record'class)
	is begin		
		-- put_line ("vertical moved " & image (clock));
		-- show_adjustments_v;
		refresh (canvas);
	end cb_vertical_moved;


	function cb_scrollbar_v_pressed (
		bar		: access gtk_widget_record'class;
		event	: gdk_event_button)
		return boolean
	is
		event_handled : boolean := false;
	begin
		-- put_line ("cb_scrollbar_v_pressed");
		return event_handled;
	end cb_scrollbar_v_pressed;

	
	function cb_scrollbar_v_released (
		bar		: access gtk_widget_record'class;
		event	: gdk_event_button)
		return boolean
	is
		event_handled : boolean := false;
	begin
		-- put_line ("cb_scrollbar_v_released");
		backup_scrollbar_settings;

		backup_visible_area (get_visible_area (canvas));
		return event_handled;
	end cb_scrollbar_v_released;



	function cb_scrollbar_h_pressed (
		bar		: access gtk_widget_record'class;
		event	: gdk_event_button)
		return boolean
	is
		event_handled : boolean := false;
	begin
		-- put_line ("cb_scrollbar_h_pressed");
		return event_handled;
	end cb_scrollbar_h_pressed;

	
	function cb_scrollbar_h_released (
		bar		: access gtk_widget_record'class;
		event	: gdk_event_button)
		return boolean
	is
		event_handled : boolean := false;
	begin
		-- put_line ("cb_scrollbar_h_released");
		backup_scrollbar_settings;

		backup_visible_area (get_visible_area (canvas));
		return event_handled;
	end cb_scrollbar_h_released;



	
-- CANVAS:
	
-- 
-- 	procedure cb_canvas_size_allocate (
-- 		canvas		: access gtk_widget_record'class;
-- 		allocation	: gtk_allocation)
-- 	is begin
-- 		null;
-- 		-- new_line;
-- 		-- put_line ("cb_canvas_size_allocate");
-- 
-- 		-- put_line ("cb_canvas_size_allocate. (x/y/w/h): " 
-- 		--  & gint'image (allocation.x) 
-- 		-- 	& " /" & gint'image (allocation.y)
-- 		-- 	& " /" & gint'image (allocation.width)
-- 		-- 	& " /" & gint'image (allocation.height));
--   
-- 	end cb_canvas_size_allocate;


	
	
	procedure set_up_canvas is begin
		put_line ("set_up_canvas (general)");

		create_canvas;
		
		-- Connect signals:

		-- Not used:
		-- canvas.on_size_allocate (cb_canvas_size_allocate'access);
		-- canvas.set_redraw_on_allocate (false);
		
		--canvas.on_button_press_event (cb_canvas_button_pressed'access);
		canvas.on_button_press_event (access_cb_canvas_button_pressed);
		
		--canvas.on_button_release_event (cb_canvas_button_released'access);
		canvas.on_button_release_event (access_cb_canvas_button_released);
		
		-- canvas.on_motion_notify_event (cb_canvas_mouse_moved'access);
		canvas.on_motion_notify_event (access_cb_canvas_mouse_moved);
		
		--canvas.on_scroll_event (cb_mouse_wheel_rolled'access);
		canvas.on_scroll_event (access_cb_mouse_wheel_rolled);
		
		--canvas.on_key_press_event (cb_canvas_key_pressed'access);
		canvas.on_key_press_event (access_cb_canvas_key_pressed);

	end set_up_canvas;


	

-- STATUS:

	procedure set_status (text : in string) is begin
		-- label_status.set_text (text);
		-- CS
		null;
	end set_status;

	procedure status_clear is begin
		set_status ("");
	end status_clear;

	procedure status_enter_verb is begin
		set_status ("Enter verb !" & status_hint_for_abort);
	end status_enter_verb;
	
	procedure status_enter_noun is begin
		set_status ("Enter noun ! " & status_hint_for_abort);
	end status_enter_noun;
	
	procedure status_verb_invalid is begin
		set_status ("Verb invalid ! ");
	end status_verb_invalid;

	procedure status_noun_invalid is begin
		set_status ("Noun invalid ! " & status_hint_for_abort);
	end status_noun_invalid;
		
	
	

-- CLARIFICATION:

	procedure set_request_clarification is begin
		request_clarificaton := YES;

		-- show instruction in status bar
		set_status ("clarify object by right click or page-down key !");
	end set_request_clarification;

	
	procedure reset_request_clarification is begin
		request_clarificaton := NO;
	end reset_request_clarification;

	
	function clarification_pending return boolean is begin
		case request_clarificaton is 
			when YES => return true;
			when NO  => return false;
		end case;
	end clarification_pending;


	procedure reset_activate_counter is begin
		activate_counter := type_activate_counter'first;
	end reset_activate_counter;

	
	procedure increment_activate_counter is begin
		activate_counter := activate_counter + 1;
	end increment_activate_counter;



-- COMMAND STATUS:

	procedure reset_single_cmd_status is begin
		single_cmd_status := (others => <>);
	end reset_single_cmd_status;

	
	
	
-- TOOL:

	procedure change_primary_tool is begin
		if primary_tool = MOUSE then
			primary_tool := KEYBOARD;
			canvas.grab_focus;
		else
			primary_tool := MOUSE;
		end if;

		-- CS update_primary_tool_display;
	end change_primary_tool;
	


	
-- CATCH ZONE:

	function get_catch_zone return type_catch_zone is 
	begin
		-- return 20.0 / type_float_positive (global_scale);
		-- CS rework, use type_logical_pixels_positive ?
		return 0.0;
	end get_catch_zone;

	


-- PROPERTIES WINDOW:


	function window_properties_is_open return boolean is begin
		return window_properties.open;
	end window_properties_is_open;


	procedure build_window_properties is begin
		properties_confirmed := false;
			
		gtk_new (window_properties.window);

		-- If the operator closes the properties window:
	-- CS window_properties.window.on_destroy (access_on_window_properties_closed);

		-- If the operator presses a key in the properties window:
	-- CS window_properties.window.on_key_press_event (access_on_window_properties_key_event);
		
		-- Mark window as open. This prevents the window
		-- from opening multiple times:
		window_properties.open := true;
		
		window_properties.window.set_title ("Properties");
	end build_window_properties;

	
	procedure set_status_properties (text : in string) is begin
		label_properties_status.set_text (text);
	end set_status_properties;

	
	procedure set_property_before (text : in string) is begin
		entry_property_old.set_text (text);
	end set_property_before;
	




-- CALLBACKS:	
	
	function cb_canvas_button_pressed (
		canvas	: access gtk_widget_record'class;
		event	: gdk_event_button)
		return boolean
	is
		event_handled : boolean := true;

		-- This is the point in the canvas where the operator
		-- has clicked:
		cp : constant type_logical_pixels_vector := 
			(to_lp (event.x), to_lp (event.y));

		-- Convert the canvas point to the corresponding
		-- real model point:
		mp : constant type_vector_model := canvas_to_real (cp, S);

		-- CS: For some reason the value of the scrollbars
		-- must be saved and restored if the canvas grabs the focus:
		h, v : type_logical_pixels;
		-- A solution might be:
		-- <https://stackoverflow.com/questions/26693042/
		-- gtkscrolledwindow-disable-scroll-to-focused-child>
		-- or
		-- <https://discourse.gnome.org/t/disable-auto-scrolling-in-
		-- gtkscrolledwindow-when-grab-focus-in-children/13058>
	begin
		put_line ("cb_canvas_button_pressed");

		-- Output the button id, x and y position:
		-- put_line ("cb_canvas_button_pressed "
		-- 	& " button" & guint'image (event.button) & " "
		-- 	& to_string (cp));

		-- Output the model point in the terminal:
		put_line (to_string (mp));


		-- Set the focus on the canvas,
		-- But first save the scrollbar values:
		h := to_lp (scrollbar_h_adj.get_value);
		v := to_lp (scrollbar_v_adj.get_value);
		-- CS: backup_scrollbar_settings does not work for some reason.
		-- put_line (to_string (v));
		
		canvas.grab_focus;

		scrollbar_h_adj.set_value (to_gdouble (h));
		scrollbar_v_adj.set_value (to_gdouble (v));
		-- CS: restore_scrollbar_settings does not work for some reason.
		-- put_line (to_string (v));


		-- If no zoom-to-area operation is active, then
		-- just place the cursor where the operator has clicked the canvas.
		-- If the operator has started a zoom-to-area operation, then
		-- set the first corner of the area:
		if zoom_area.active then
			zoom_area.k1 := mp;
			--put_line ("zoom area k1: " & to_string (zoom_area.k1));

			-- For the routine that draws a rectangle around the
			-- selected area: Indicate that a selection has started 
			-- and a start point has been defined:
			zoom_area.started := true;
			zoom_area.l1 := cp;
			--put_line ("zoom area l1: " & to_string (zoom_area.l1));
		else
		-- Otherwise move the cursor to the nearest grid point:
			move_cursor (snap_to_grid (mp));
		end if;

		
		refresh (canvas);
		
		return event_handled;
	end cb_canvas_button_pressed;




	function cb_canvas_button_released (
		canvas	: access gtk_widget_record'class;
		event	: gdk_event_button)
		return boolean
	is
		event_handled : boolean := true;

		debug : boolean := false;
		

		-- This is the point in the canvas where the operator
		-- released the button:
		cp : constant type_logical_pixels_vector := 
			(to_lp (event.x), to_lp (event.y));

		-- Convert the canvas point to the corresponding
		-- real model point:
		mp : constant type_vector_model := canvas_to_real (cp, S);

		-- The corners of the bounding-box on the canvas before 
		-- and after zooming:
		C1, C2 : type_bounding_box_corners;
		
	begin
		put_line ("cb_canvas_button_released");

		
		-- Output the button id, x and y position:
		-- put_line ("cb_canvas_button_pressed "
		-- 	& " button" & guint'image (event.button) & " "
		-- 	& to_string (cp));

		-- Output the model point in the terminal:
		-- put_line (to_string (mp));

		-- Move the cursor to the nearest grid point:
		-- move_cursor (snap_to_grid (mp));


		-- If the operator is finishing a zoom-to-area operation,
		-- then the actual area of interest is computed here
		-- and passed to procedure zoom_to_fit.
		-- If start and end point of the area are equal,
		-- then nothing happens here.
		if zoom_area.active then
			C1 := get_bounding_box_corners;

			-- Set the second corner of the zoom-area:
			zoom_area.k2 := mp;

			-- Compute the area from the corner points k1 and k2
			-- if they are different. Otherwise nothing happens here:
			if zoom_area.k1 /= zoom_area.k2 then
				
				if debug then
					put_line ("zoom area c1: " & to_string (zoom_area.k1));
					put_line ("zoom area c2: " & to_string (zoom_area.k2));
				end if;


				-- x-position:
				if zoom_area.k1.x < zoom_area.k2.x then
					zoom_area.area.position.x := zoom_area.k1.x;
				else
					zoom_area.area.position.x := zoom_area.k2.x;
				end if;

				-- y-position:
				if zoom_area.k1.y < zoom_area.k2.y then
					zoom_area.area.position.y := zoom_area.k1.y;
				else
					zoom_area.area.position.y := zoom_area.k2.y;
				end if;

				-- width and height:
				zoom_area.area.width  := 
					abs (zoom_area.k2.x - zoom_area.k1.x);
				
				zoom_area.area.height := 
					abs (zoom_area.k2.y - zoom_area.k1.y);

				if debug then
					put_line ("zoom " & to_string (zoom_area.area));
				end if;


				
				-- Reset the translate-offset:
				T := (0.0, 0.0);			
				zoom_to_fit (zoom_area.area);

				C2 := get_bounding_box_corners;
				update_scrollbar_limits (C1, C2);
				backup_scrollbar_settings;

				-- The operation comes to an end here:
				zoom_area.active := false;

				-- For the routine that draws a rectangle around the
				-- selected area: Indicate that the rectangle shall
				-- no longer be drawn:
				zoom_area.started := false;


				backup_visible_area (zoom_area.area);
			end if;
		end if;

		
		refresh (canvas);
		
		return event_handled;
	end cb_canvas_button_released;


	
	
	
	function cb_canvas_mouse_moved (
		canvas	: access gtk_widget_record'class;
		event	: gdk_event_motion)
		return boolean
	is
		event_handled : boolean := true;

		cp : constant type_logical_pixels_vector := 
			(to_lp (event.x), to_lp (event.y));

		-- Get the real model coordinates:
		mp : constant type_vector_model := canvas_to_real (cp, S);
	begin
		-- put_line ("cb_canvas_mouse_moved");

		-- output on the terminal:
		-- Output the x/y position of the pointer
		-- in logical and model coordinates:
		-- put_line (
			-- to_string (cp)
			-- & " " & to_string (mp)

		-- Update the coordinates display with the pointer position:
		-- x-axis:
		--pointer_x_buf.set_text (to_string (mp.x));
		pointer_x_buf.set_text (to_string (to_reality (mp.x)));
		pointer_x_value.set_buffer (pointer_x_buf);
  
		-- y-axis:
		pointer_y_buf.set_text (to_string (to_reality (mp.y)));
		pointer_y_value.set_buffer (pointer_y_buf);
		-- CS move to demo_coordinates_display

		update_distances_display;

		-- While a zoom-to-area operation is active,
		-- set the end point of the selected area.
		-- The routine that draws the rectangle uses this
		-- point to compute the rectangle on the fly:
		if zoom_area.active then
			zoom_area.l2 := cp;
			--put_line ("zoom area l2: " & to_string (zoom_area.l2));

			-- The canvas must be refreshed in order to
			-- show the rectangle as the mouse is being moved:
			refresh (canvas);
		end if;
		
		return event_handled;
	end cb_canvas_mouse_moved;



	
	
	function cb_canvas_key_pressed (
		canvas	: access gtk_widget_record'class;
		event	: gdk_event_key)
		return boolean
	is
		event_handled : boolean := true;

		use gdk.types;		
		use gdk.types.keysyms;
		
		key_ctrl	: gdk_modifier_type := event.state and control_mask;
		key_shift	: gdk_modifier_type := event.state and shift_mask;
		key			: gdk_key_type := event.keyval;
		
	begin
		-- Output the the gdk_key_type (which is
		-- just a number (see gdk.types und gdk.types.keysyms)):
		put_line ("cb_canvas_key_pressed "
			& " key " & gdk_key_type'image (event.keyval));

		if key_ctrl = control_mask then 
			case key is
				when GDK_KP_ADD | GDK_PLUS =>
					zoom_on_cursor (ZOOM_IN);

				when GDK_KP_SUBTRACT | GDK_MINUS =>
					zoom_on_cursor (ZOOM_OUT);
					
				when others => null;
			end case;

		else
			case key is
				when GDK_ESCAPE =>
					-- Here the commands to abort any pending 
					-- operations related to the canvas should be placed:

					null;
					
					
				when GDK_Right =>
					move_cursor (DIR_RIGHT);

				when GDK_Left =>
					move_cursor (DIR_LEFT);

				when GDK_Up =>
					move_cursor (DIR_UP);

				when GDK_Down =>
					move_cursor (DIR_DOWN);

				when GDK_HOME | GDK_KP_HOME =>
					-- Move the cursor to the grid point that
					-- is nearest to the center of the visible area:
					put_line ("move cursor to center");
					move_cursor (snap_to_grid (get_center (visible_area)));
					refresh (canvas);

				-- when GDK_F2 =>

					
				when others => null;
			end case;
		end if;
		
		return event_handled;
	end cb_canvas_key_pressed;




	
	function cb_mouse_wheel_rolled (
		canvas	: access gtk_widget_record'class;
		event	: gdk_event_scroll)
		return boolean
	is
		--debug : boolean := false;
		debug : boolean := true;
		
		use gdk.types;
		use gtk.accel_group;
		event_handled : boolean := true;

		accel_mask : constant gdk_modifier_type := 
			get_default_mod_mask;

		-- The direction at which the operator is turning the wheel:
		wheel_direction : constant gdk_scroll_direction :=
			event.direction;


		procedure zoom is
			-- The given point on the canvas where the operator is 
			-- zooming in or out:
			Z : constant type_logical_pixels_vector := 
				(to_lp (event.x), to_lp (event.y));

			-- The corners of the bounding-box on the canvas before 
			-- and after zooming:
			C1, C2 : type_bounding_box_corners;
			S1 : constant type_zoom_factor := S;
			
		begin -- zoom
			-- put_line (" zoom old" & to_string (S));

			C1 := get_bounding_box_corners;
			
			case wheel_direction is
				when SCROLL_UP =>
					increase_zoom_factor;
					if debug then
						put_line (" zoom in");
					end if;
					update_zoom_display;
					
				when SCROLL_DOWN => 
					decrease_zoom_factor;
					if debug then
						put_line (" zoom out");
					end if;
					update_zoom_display;
					
				when others => null;
			end case;

			
			if debug then
				put_line (" S" & to_string (S));
			end if;
			
			-- After changing the zoom factor, the translate-offset must
			-- be calculated anew. When the actual drawing takes 
			-- place (see function cb_draw)
			-- then the drawing will be dragged back by the 
			-- translate-offset so that the operator gets the impression 
			-- of a zoom-into or zoom-out effect.
			-- Without applying a translate-offset the drawing would be 
			-- appearing as expanding to the upper-right (on zoom-in) 
			-- or shrinking toward the lower-left:
			set_translation_for_zoom (S1, S, Z);

			-- show_adjustments_v;
			-- backup_scrollbar_settings;

			C2 := get_bounding_box_corners;
			update_scrollbar_limits (C1, C2);

			backup_visible_area (get_visible_area (canvas));
			
			-- schedule a redraw:
			refresh (canvas);
		end zoom;


		procedure scroll (
			direction : in type_scroll_direction)
		is
			v1, dv, v2 : type_logical_pixels;

			-- This procedure computes the amount
			-- by which the scrollbar value is to be changed:
			procedure set_delta is begin
				null;
				-- CS: This is emperical for the time being.
				-- Rework required.
				dv := 10.0 * type_logical_pixels (S);
			end set_delta;
			
		begin
			if debug then
				put_line (" " & type_scroll_direction'image (direction));
			end if;
			

			case direction is
				when SCROLL_DOWN =>
					-- Get the current value of the scrollbar:
					v1 := to_lp (scrollbar_v_adj.get_value);

					-- Compute the amout by which the 
					-- scrollbar is to be moved:
					set_delta;
					
					-- Compute the new value of the scrollbar:
					v2 := v1 + dv;

					-- Set the new value of the scrollbar:
					scrollbar_v_adj.set_value (to_gdouble (v2));

					
				when SCROLL_UP =>
					v1 := to_lp (scrollbar_v_adj.get_value);
					set_delta;
					v2 := v1 - dv;
					scrollbar_v_adj.set_value (to_gdouble (v2));

				when SCROLL_RIGHT =>
					v1 := to_lp (scrollbar_h_adj.get_value);
					set_delta;
					v2 := v1 + dv;
					scrollbar_h_adj.set_value (to_gdouble (v2));
					
				when SCROLL_LEFT =>
					v1 := to_lp (scrollbar_h_adj.get_value);
					set_delta;
					v2 := v1 - dv;
					scrollbar_h_adj.set_value (to_gdouble (v2));

				-- CS clip ?
			end case;

			backup_visible_area (get_visible_area (canvas));
		end scroll;
		
		
	begin -- cb_mouse_wheel_rolled

		if debug then
			put_line ("cb_mouse_wheel_rolled");
			-- put_line (" direction " 
			-- & gdk_scroll_direction'image (wheel_direction));
		end if;


		-- If CTRL is being pressed, then zoom in or out:
		if (event.state and accel_mask) = control_mask then
			zoom;

		-- If SHIFT is being pressed, then scroll up or down:
		elsif (event.state and accel_mask) = shift_mask then
			case wheel_direction is
				when SCROLL_UP =>
					scroll (SCROLL_RIGHT);
					
				when SCROLL_DOWN => 
					scroll (SCROLL_LEFT);
					
				when others => null;
			end case;

		-- If no key is being pressed, then scroll right or left:
		else
			case wheel_direction is
				when SCROLL_UP =>
					scroll (SCROLL_UP);

				when SCROLL_DOWN => 
					scroll (SCROLL_DOWN);
					
				when others => null;
			end case;
		end if;

		backup_scrollbar_settings;
		
		-- update_visible_area (canvas);
		
		return event_handled;
	end cb_mouse_wheel_rolled;

	
end et_canvas;
