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

-- with gtk.main;
-- with gtk.window; 			use gtk.window;
-- with gtk.widget;  			use gtk.widget;
-- with gtk.box;				use gtk.box;
-- with gtk.button;     		use gtk.button;
-- with gtk.handlers;			use gtk.handlers;
-- with gtk.toolbar; 			use gtk.toolbar;
-- with gtk.tool_button;		use gtk.tool_button;
-- with gtk.enums;				use gtk.enums;
-- with gtk.gentry;			use gtk.gentry;
-- with gtk.combo_box_text;	use gtk.combo_box_text;
-- with gtk.frame;				use gtk.frame;
-- with gtk.scrolled_window;	use gtk.scrolled_window;
-- with gtk.adjustment;		use gtk.adjustment;
-- 
-- with gdk;
-- with gdk.types;
-- 
-- with glib;					use glib;
-- with glib.object;			use glib.object;
-- with glib.values;			use glib.values;
with cairo;					use cairo;
-- with cairo.pattern;			use cairo.pattern;
-- with gtkada.style;     		use gtkada.style;

-- with pango.layout;			use pango.layout;

-- with ada.containers;		use ada.containers;
-- with ada.containers.doubly_linked_lists;

-- with et_pcb_coordinates;	use et_pcb_coordinates;
-- with et_project;			--use et_project;
-- with et_frames;				--use et_frames;
with et_geometry;
with et_canvas;

package et_canvas_draw is

generic

	with package pac_canvas is new et_canvas.pac_canvas (<>);
	with package pac_shapes is new et_geometry.shapes_2d (<>);
	
package pac_draw is
	use pac_canvas;
	use pac_shapes;
	use pac_shapes.geometry;

	-- A rectangular area of the drawing:
	type type_rectangle is record
		x, y			: pac_shapes.geometry.type_distance; -- position, upper left corner
		width, height	: pac_shapes.geometry.type_distance_positive; -- size
	end record;

	no_rectangle : constant type_rectangle := (0.0, 0.0, 0.0, 0.0);

-- 	function intersects (rect1, rect2 : type_rectangle) return boolean;
	
	
	-- This function converts a x-value from the drawing to a x-value in the view.
	function convert_x (x : in pac_shapes.geometry.type_distance) return type_view_coordinate;

	-- This function converts a y-value from the drawing to a y-value in the view.	
	function convert_y (y : in pac_shapes.geometry.type_distance) return type_view_coordinate renames convert_x;
	

	

-- 	-- This function converts a y-value from the drawing to the view.
-- 	-- The input y increases upwards. The output y increases downwards.
-- 	function shift_y (
-- 		height	: in pac_shapes.geometry.type_distance;
-- 		offset	: in pac_shapes.geometry.type_distance)
-- 		return type_view_coordinate;


	-- This procedure draws the given line on the given context.
	-- The line is shifted in y to a plane of given height. This plane
	-- has y-axis going downwards.
	-- The line will be drawn if its bounding box intersects the given area.
	procedure draw_line (
		area	: in type_rectangle;	
		context	: in type_draw_context;
		line	: in type_line'class;
		height	: in pac_shapes.geometry.type_distance);

	-- This procedure draws the given arc on the given context.
	-- The arc is shifted in y to a plane of given height. This plane
	-- has y-axis going downwards.
	-- The arc will be drawn if its bounding box intersects the given area.
	procedure draw_arc (
		area	: in type_rectangle;
		context	: in type_draw_context;
		arc		: in type_arc'class;
		height	: in pac_shapes.geometry.type_distance);


end pac_draw;

end et_canvas_draw;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
