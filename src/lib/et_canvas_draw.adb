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

with ada.text_io;				use ada.text_io;
-- with cairo;					use cairo;
-- with cairo.pattern;			use cairo.pattern;
-- with gtkada.style;     		use gtkada.style;

-- with pango.layout;			use pango.layout;

-- with ada.containers;		use ada.containers;
-- with ada.containers.doubly_linked_lists;

-- with et_pcb_coordinates;	use et_pcb_coordinates;
-- with et_project;			--use et_project;
-- with et_frames;				--use et_frames;

-- with et_canvas;

package body et_canvas_draw is

	
package body pac_draw is

	function convert_x (x : in pac_shapes.geometry.type_distance) 
		return type_view_coordinate is 
	begin
		return type_view_coordinate (x);
	end;

	function shift_y (
		y		: in pac_shapes.geometry.type_distance;
		height	: in pac_shapes.geometry.type_distance)
		return type_view_coordinate is
	begin
		return type_view_coordinate (height - y);
	end;

	function shift_y (
		y		: in pac_shapes.geometry.type_distance;
		height	: in pac_shapes.geometry.type_distance)
		return pac_shapes.geometry.type_distance is
	begin
		return (height - y);
	end;

	function make_bounding_box (
		height		: in pac_shapes.geometry.type_distance;
		boundaries	: in type_boundaries)
		return type_rectangle is
	begin
		return (
			-- The bounding box origin is the upper left corner.
			-- The box position in x is the smallest_x.
			-- The box position in y is the greatest_y (upwards).
			-- The box position in y is additonally converted to y axis going downwards.
			x		=> boundaries.smallest_x,
			y		=> shift_y (boundaries.smallest_y, height),

			-- The box width is the difference between greatest x and smallest x.
			-- The box height is the difference between greatest y and smallest y.
			width	=> boundaries.greatest_x - boundaries.smallest_x,
			height	=> boundaries.greatest_y - boundaries.smallest_y
			);
	end make_bounding_box;

	function intersects (rect1, rect2 : type_rectangle) return boolean is begin
		return not (
			rect1.x > rect2.x + rect2.width            --  r1 on the right of r2
			or else rect2.x > rect1.x + rect1.width    --  r2 on the right of r1
			or else rect1.y > rect2.y + rect2.height   --  r1 below r2
			or else rect2.y > rect1.y + rect1.height); --  r1 above r2
	end intersects;

	
	procedure draw_line (
		area	: in type_rectangle;
		context	: in type_draw_context;
		line	: in type_line'class;
		height	: in pac_shapes.geometry.type_distance) is

		-- compute the boundaries (greatest/smallest x/y) of the given line:
		boundaries : type_boundaries := pac_shapes.boundaries (line);

		-- compute the bounding box of the given line
		bounding_box : type_rectangle := make_bounding_box (height, boundaries);
	begin
		-- We draw the segment if:
		--  - no area given or
		--  - if the bounding box of the segment intersects the given area
		if (area = no_rectangle
			or else intersects (area, bounding_box)) 
		then

	-- CS test size 
	-- 			if not size_above_threshold (self, context.view) then
	-- 				return;
	-- 			end if;
			
			save (context.cr);
			
			-- start point
			cairo.move_to (
				context.cr,
				convert_x (line.start_point.x),
				shift_y (line.start_point.y, height)
				);

			-- end point
			cairo.line_to (
				context.cr,
				convert_x (line.end_point.x),
				shift_y (line.end_point.y, height)
				);

			restore (context.cr);

		end if;
	end draw_line;

	
end pac_draw;

end et_canvas_draw;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
