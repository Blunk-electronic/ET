------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           CANVAS FOR BOARD                               --
--                                                                          --
--                               B o d y                                    --
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


with ada.text_io;			use ada.text_io;
with et_general;
with et_project;
with et_frames;
with et_pcb_coordinates;	use et_pcb_coordinates;
use et_pcb_coordinates.geometry;

with et_canvas_schematic;

package body et_canvas_board is

	procedure set_title_bar (
		-- CS project name
		module		: in et_general.type_module_name.bounded_string)
	is
		use et_general;
	begin
		window.set_title (title & to_string (module));
	end set_title_bar;

	function model_to_drawing (
		self		: not null access type_view;
		model_point : in type_point)	
		return type_point is 
		use et_general;
		p : type_point; -- to be returned
	begin
		set (point	=> p,
			 axis	=> X, 
			 value	=> model_point.x 
						- self.frame_bounding_box.x
						- x (self.board_origin) -- because board origin is not the same as drawing origin
			);
		
		set (point	=> p,
			 axis	=> Y,
			 value	=> type_distance (self.frame_height) 
						- model_point.y 
						+ self.frame_bounding_box.y
						- y (self.board_origin)  -- because board origin is not the same as drawing origin
			);

		return p;
	end;

	function drawing_to_model (
		self			: not null access type_view;
		drawing_point : in type_point)	
		return type_point is 
		use et_general;
		p : type_point; -- to be returned
	begin
		set (point	=> p,
			 axis	=> X, 
			 value	=> drawing_point.x 
						+ self.frame_bounding_box.x
						+ x (self.board_origin) -- because board origin is not the same as drawing origin
			);
		
		set (point	=> p,
			 axis	=> Y,
			 value	=> type_distance (self.frame_height) 
						- drawing_point.y 
						+ self.frame_bounding_box.y
						- y (self.board_origin)  -- because board origin is not the same as drawing origin
			);

		return p;
	end;


	
	function active_module (self : not null access type_view) 
		return string is
		use et_general.type_module_name;
		use et_project.type_modules;
		use et_canvas_schematic;
	begin
		return to_string (key (current_active_module)); -- motor_driver (without extension)
	end active_module;
	
	function bounding_box (self : not null access type_view)
		return type_rectangle is
	begin
		return self.paper_bounding_box; -- CS should include all items of the current sheet.
		-- means: also items outside the frame
	end;


	
	procedure gtk_new (
		self	: out type_view_ptr) is
	begin
		self := new type_view;
		init (self);
	end;


	procedure draw_grid (
		self    : not null access type_view;
		context : type_draw_context;
		area    : type_rectangle) is separate;

	procedure draw_frame (
		self    : not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context) is separate;

	procedure draw_outline (
		self    : not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context) is separate;

	procedure draw_silk_screen (
		self    : not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context;
		face	: in type_face) is separate;

	procedure draw_keepout (
		self    : not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context;
		face	: in type_face) is separate;
	
	procedure draw_internal (
		self    : not null access type_view;
		context : type_draw_context;
		area    : type_rectangle) 
	is
		use et_general;

		-- The given area must be shifted (left and up) by the position
		-- of the drawing frame. This is required for all objects in the 
		-- drawing frame.
		-- Take a copy of the given area:
		area_shifted : type_rectangle := area;

		-- Calculate the new position of area_shifted:
		area_shifted_new_position : type_point := type_point (set (
						x => - self.frame_bounding_box.x,
						y => - self.frame_bounding_box.y));

	begin
-- 		put_line ("draw internal ...");
		
		-- draw a black background:
		set_source_rgb (context.cr, 0.0, 0.0, 0.0);
		paint (context.cr);

		draw_grid (self, context, area);
		
		-- move area_shifted according to frame position:
		move_by (area_shifted, area_shifted_new_position);

		-- draw the frame:
		save (context.cr);
		-- Prepare the current transformation matrix (CTM) so that
		-- all following drawing is relative to the upper left frame corner.
		translate (
			context.cr,
			convert_x (self.frame_bounding_box.x),
			convert_y (self.frame_bounding_box.y));

		draw_frame (self, area_shifted, context);
		restore (context.cr);

		
		-- move area_shifted according to board position:
		move_by (area_shifted, type_point (invert (self.board_origin, X)));
		
		save (context.cr);
		-- Prepare the current transformation matrix (CTM) so that
		-- all following drawing is relative to the upper left frame corner.
		
		-- The drawing must further-on shifted to the right and up by the board position
		-- so that the board origin is not at the lower left corner of the frame.
		translate (
			context.cr,
			convert_x (self.frame_bounding_box.x + x (self.board_origin)),
			convert_y (self.frame_bounding_box.y - y (self.board_origin)));

		draw_cursor (self, area_shifted, context, cursor_main);
		draw_outline (self, area_shifted, context);
		draw_silk_screen (self, area_shifted, context, TOP);
		draw_keepout (self, area_shifted, context, TOP);
		-- CS draw_packages (self, area, context); -- separate unit
		-- CS self.model.draw_texts (area, context);
		-- CS self.model.draw_submodules (area, context);

		restore (context.cr);
	end draw_internal;

	procedure set_grid (view : in type_view_ptr) is  -- CS remove
		use et_project;
		use et_canvas_schematic;
		type type_local_view_ptr is access all type_view;
		self : type_local_view_ptr := type_local_view_ptr (view);
	begin
		self.grid := type_modules.element (current_active_module).board.grid;
	end set_grid;
	
	procedure redraw (view : in type_view_ptr) is begin
		set_grid (view);
		queue_draw (view);
	end;
	
	function convert_and_shift_y (
		self	: not null access type_view;
		y		: in type_distance_total)
		return type_view_coordinate is 
	begin
		return type_view_coordinate 
			(
			self.frame_height 
			- y
			);
	end;
		
	function convert_and_shift_y (
		self	: not null access type_view;
		y		: in type_distance_total)
		return type_distance_total is 
	begin
		return (
			self.frame_height 
			- y
			);
	end;


	procedure execute_command (
		self			: not null access type_view;
		cmd				: in type_fields_of_line;
		log_threshold	: in type_log_level) is separate;
	
	procedure draw_cursor (
		self		: not null access type_view;
		in_area		: in type_rectangle := no_rectangle;
		context 	: in type_draw_context;
		cursor		: in type_cursor)
	is
		lh : type_cursor_line; -- the horizontal line
		lv : type_cursor_line; -- the vertical line
	begin
		-- set start and end point of horizontal line
		lh.start_point := type_point (set (
			x	=> x (cursor.position) - cursor_half_size,
			y	=> y (cursor.position)));

		lh.end_point := type_point (set (
			x	=> x (cursor.position) + cursor_half_size,
			y	=> y (cursor.position)));

		-- set start and end point of vertical line
		lv.start_point := type_point (set (
			x	=> x (cursor.position),
			y	=> y (cursor.position) + cursor_half_size));

		lv.end_point := type_point (set (
			x	=> x (cursor.position),
			y	=> y (cursor.position) - cursor_half_size));

		-- draw the cursor
		cairo.set_line_width (context.cr, type_view_coordinate (cursor_line_width));
		cairo.set_source_rgb (context.cr, gdouble (1), gdouble (1), gdouble (1)); -- white

		pac_draw_package.draw_line (
			area		=> in_area,
			context		=> context,
			line		=> lh,
			height		=> self.frame_height);

		pac_draw_package.draw_line (
			area		=> in_area,
			context		=> context,
			line		=> lv,
			height		=> self.frame_height);
		
		cairo.stroke (context.cr);		
	end draw_cursor;


	function get_frame (
		self : not null access type_view)
		return et_frames.type_frame is

		use et_canvas_schematic;
		use et_project.type_modules;
	begin
		return element (current_active_module).board.frame.frame;
	end get_frame;

	function frame_height (
		self : not null access type_view)
		return type_distance_positive is 

		use et_canvas_schematic;
		use et_project.type_modules;
	begin
		return type_distance_positive (element (current_active_module).board.frame.frame.size.y);
	end frame_height;

	function frame_width (
		self : not null access type_view)
		return type_distance_positive is 

		use et_canvas_schematic;
		use et_project.type_modules;
	begin
		return type_distance_positive (element (current_active_module).board.frame.frame.size.x);
	end frame_width;
	
	function title_block_position (
		self : not null access type_view)
		return et_frames.type_position is
	begin
		return self.get_frame.title_block_pcb.position;
	end title_block_position;

	function board_origin (
		self : not null access type_view)
		return geometry.type_point is

		use et_canvas_schematic;
		use et_project.type_modules;
	begin
		return element (current_active_module).board.origin;
	end board_origin;
	
end et_canvas_board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
