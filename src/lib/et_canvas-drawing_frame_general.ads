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

-- with ada.text_io;				use ada.text_io;
-- 
-- with gdk.event;					use gdk.event;
-- 
-- with gtk.widget;				use gtk.widget;
-- with gtk.window;				use gtk.window;
-- with gtk.separator;				use gtk.separator;
-- with gtk.box;					use gtk.box;
-- with gtk.gentry;				use gtk.gentry;
-- with gtk.combo_box;				use gtk.combo_box;
-- with gtk.combo_box_text;		use gtk.combo_box_text;
-- with gtk.drawing_area;			use gtk.drawing_area;
-- 
-- with gtk.scrolled_window;		use gtk.scrolled_window;
-- with gtk.adjustment;			use gtk.adjustment;
-- with gtk.scrollbar;				use gtk.scrollbar;
-- 
-- with gtk.table;					use gtk.table;
-- with gtk.label;					use gtk.label;
-- with gtk.button;				use gtk.button;
-- with gtk.text_view;				use gtk.text_view;
-- with gtk.text_buffer;			use gtk.text_buffer;
-- 
-- with cairo;
-- 
-- 
-- with et_logical_pixels;			use et_logical_pixels;
-- with et_logging;				use et_logging;

-- with et_frames;					use et_frames;


generic
	
package et_canvas.drawing_frame_general is
	-- use pac_geometry_2;
	-- use pac_grid;
	

	procedure dummy_2;


	-- This procedure draws the outer border of the frame:
	-- procedure draw_border (
	-- 	size	: in type_frame_size);
	
	
end et_canvas.drawing_frame_general;

