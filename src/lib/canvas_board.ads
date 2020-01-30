------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           CANVAS FOR BOARD                               --
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

with et_pcb_coordinates;	use et_pcb_coordinates;
with et_project;			--use et_project;
with et_frames;				--use et_frames;

with et_canvas;

package canvas_board is

	-- Instantiate the canvas package:
	package pac_canvas is new et_canvas.pac_canvas (
		canvas_name		=> "board", -- CS provide domain name like scripting.type_domain
		geometry		=> et_pcb_coordinates.geometry);

	use pac_canvas;
	

	use et_pcb_coordinates.geometry;
	

	type type_drawing is record	
		module	: et_project.type_modules.cursor;

		-- These variables are frequently used. Procedure set_module
		-- sets them. Other operations are free to access
		-- them.
		frame				: et_frames.type_frame_pcb;
		frame_bounding_box	: type_rectangle;

		paper_bounding_box	: type_rectangle;
		paper_height		: geometry.type_distance_positive;
		paper_width			: geometry.type_distance_positive;

		title_block_position	: et_frames.type_position;

		-- CS grid_size 	: type_distance_positive_positive := 20.0;
	end record;
	

-- 	-- Initializes the internal data so that the model can send signals:
-- 	procedure init (self : not null access type_model'class);


-- VIEW
	type type_view is new pac_canvas.type_view with record
		drawing	: type_drawing;
	end record;

	-- Returns the bounding box of all items.
	overriding function bounding_box (self : not null access type_view)
		return type_rectangle;

	-- This function converts a y-value from the drawing to a y-value in the model.
	-- The input y increases upwards. The output y increases downwards.
	overriding function convert_and_shift_y (
		self	: not null access type_view;
		y		: in type_distance_total) 
		return type_distance_total;
		
	-- This function converts a y-value from the drawing to a y-value in the view.
	-- The input y increases upwards. The output y increases downwards.
	overriding function convert_and_shift_y (
		self	: not null access type_view;
		y		: in type_distance_total)
		return type_view_coordinate;

	-- Converts a model point to a drawing point. 
	-- NOTE: The model point is in a coordinate system with y-axis
	-- going downwards. The drawing point is in a system where y-axis
	-- goes upwards. The origin of the drawing coordinate system is the
	-- lower left corner of the drawing frame.
	overriding function model_to_drawing (
		self		: not null access type_view;
		model_point : in type_point)
		return type_point;


	-- Creates a new view:
	procedure gtk_new (
		self	: out type_view_ptr);

	
	
	-- Draws the frame:
-- 	procedure draw_frame (
-- 		self	: not null access type_view;
-- 		in_area	: in type_rectangle := no_rectangle;
-- 		context : in type_draw_context);


	-- Computes a bounding box from the given boundaries:
	function make_bounding_box (
		self		: not null access type_view;
		boundaries	: in type_boundaries)
		return type_rectangle;
	

	
	-- Redraw either the whole view, or a specific part of it only.
	-- The transformation matrix has already been set on the context.
	overriding procedure draw_internal (
		self    : not null access type_view;
		context : type_draw_context;
		area    : type_rectangle);


	
	-- Init the drawing:
	procedure init_drawing (
		view	: in type_view_ptr;
		module	: in et_project.type_modules.cursor);



	
end canvas_board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
