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

	function to_string (
		self	: not null access type_view;
		point	: in type_point;
		axis	: in et_general.type_axis_2d)
		return string 
	is
		use et_general;
	begin
		case axis is
			when X => return to_string (round (x (point), self.drawing.grid.x));
			when Y => return to_string (round (y (point), self.drawing.grid.y));
		end case;
	end;
	
	function to_string (
		self	: not null access type_view;
		point	: in type_point)
		return string is
	begin
		return round_to_string (point, self.drawing.grid);
	end;
	
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
						- self.drawing.frame_bounding_box.x
						- x (self.drawing.board_origin) -- because board origin is not the same as drawing origin
			);
		
		set (point	=> p,
			 axis	=> Y,
			 value	=> type_distance (self.drawing.frame.frame.size.y) 
						- model_point.y 
						+ self.drawing.frame_bounding_box.y
						- y (self.drawing.board_origin)  -- because board origin is not the same as drawing origin
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
						+ self.drawing.frame_bounding_box.x
						+ x (self.drawing.board_origin) -- because board origin is not the same as drawing origin
			);
		
		set (point	=> p,
			 axis	=> Y,
			 value	=> type_distance (self.drawing.frame.frame.size.y) 
						- drawing_point.y 
						+ self.drawing.frame_bounding_box.y
						- y (self.drawing.board_origin)  -- because board origin is not the same as drawing origin
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
		return self.drawing.paper_bounding_box; -- CS should include all items of the current sheet.
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
						x => - self.drawing.frame_bounding_box.x,
						y => - self.drawing.frame_bounding_box.y));

	begin
-- 		put_line ("draw internal ...");
		
		-- draw a black background:
		set_source_rgb (context.cr, 0.0, 0.0, 0.0);
		paint (context.cr);

		-- draw the grid:
		draw_grid (self, context, area);
		
		draw_frame (self, area, context);

		-- move area_shifted according to frame position:
		move_by (area_shifted, area_shifted_new_position);

		-- move area_shifted according to board position:
		move_by (area_shifted, type_point (invert (self.drawing.board_origin, X)));
		
		save (context.cr);
		-- Prepare the current transformation matrix (CTM) so that
		-- all following drawing is relative to the upper left frame corner.
		
		-- The drawing must further-on shifted to the right and up by the board position
		-- so that the board origin is not at the lower left corner of the frame.
		translate (
			context.cr,
			convert_x (self.drawing.frame_bounding_box.x + x (self.drawing.board_origin)),
			convert_y (self.drawing.frame_bounding_box.y - y (self.drawing.board_origin)));

		draw_cursor (self, area_shifted, context, cursor_main);
		draw_outline (self, area_shifted, context);
		draw_silk_screen (self, area_shifted, context, TOP);
		-- CS draw_packages (self, area, context); -- separate unit
		-- CS self.model.draw_texts (area, context);
		-- CS self.model.draw_submodules (area, context);

		restore (context.cr);
	end draw_internal;

	procedure set_grid (view : in type_view_ptr) is
		use et_project;
		use et_canvas_schematic;
		type type_local_view_ptr is access all type_view;
		self : type_local_view_ptr := type_local_view_ptr (view);
	begin
		self.drawing.grid := type_modules.element (current_active_module).board.grid;
	end set_grid;
	
	procedure init_drawing (
		view	: in type_view_ptr)
	is
		use et_general;
		use et_frames;
		use et_project;

		type type_local_view_ptr is access all type_view;
		self : type_local_view_ptr := type_local_view_ptr (view);

		-- set a cursor to the currently active module:
		am : et_project.type_modules.cursor := et_canvas_schematic.current_active_module;
	begin
		-- set some variables frequently used regarding frame and paper:
		self.drawing.frame := type_modules.element (am).board.frame;
		
		self.drawing.paper_height := type_distance_positive (paper_dimension (
							paper_size	=> self.drawing.frame.frame.paper,
							orientation	=> self.drawing.frame.frame.orientation,
							axis		=> Y));

		self.drawing.paper_width := type_distance_positive (paper_dimension (
							paper_size	=> self.drawing.frame.frame.paper,
							orientation	=> self.drawing.frame.frame.orientation,
							axis		=> X));

		-- The drawing frame has a bounding box:

		-- position (upper left corner):
		self.drawing.frame_bounding_box.x := (self.drawing.paper_width - type_distance_positive (self.drawing.frame.frame.size.x)) / 2.0;
		self.drawing.frame_bounding_box.y := (self.drawing.paper_height - type_distance_positive (self.drawing.frame.frame.size.y)) / 2.0;

		-- width and height
		self.drawing.frame_bounding_box.width := type_distance_positive (self.drawing.frame.frame.size.x);
		self.drawing.frame_bounding_box.height := type_distance_positive (self.drawing.frame.frame.size.y);

		-- The sheet has a drawing box:
		self.drawing.paper_bounding_box := (0.0, 0.0, self.drawing.paper_width, self.drawing.paper_height);

		-- Drawing of the title block items is relative to the title block position:
		self.drawing.title_block_position := self.drawing.frame.frame.title_block_pcb.position;

		-- The schematic drawing has a grid:
		set_grid (view);

		-- Get the board position (distance relative to the lower left corner of the drawing frame):
		self.drawing.board_origin := type_modules.element (am).board.origin;

	end init_drawing;

	procedure redraw (view : in type_view_ptr) is begin
		init_drawing (view);
		queue_draw (view);
	end;
	
	function convert_and_shift_y (
		self	: not null access type_view;
		y		: in type_distance_total)
		return type_view_coordinate is 
	begin
		return type_view_coordinate 
			(
			self.drawing.frame_bounding_box.height 
			- y
			);
	end;
		
	function convert_and_shift_y (
		self	: not null access type_view;
		y		: in type_distance_total)
		return type_distance_total is 
	begin
		return (
			self.drawing.frame_bounding_box.height 
			- y
			);
	end;


	procedure execute_command (
		self			: not null access type_view;
		cmd				: in type_fields_of_line;
		log_threshold	: in type_log_level) is separate;

	procedure move_cursor_to (
		self		: not null access type_view;
		cursor		: in out type_cursor;
		position	: in type_point) is 
		use et_general;
	begin
		cursor.position := type_point (round (position, self.drawing.grid));
		update_position_display_cursor;
		self.shift_area (cursor);		
	end move_cursor_to;

	procedure move_cursor_by (
		self		: not null access type_view;								 
		cursor		: in out type_cursor;
		position	: in type_point) is 
	begin
		cursor.position := type_point (round (cursor.position + position, self.drawing.grid));
		update_position_display_cursor;
		self.shift_area (cursor);
	end move_cursor_by;

	procedure move_cursor (
		self		: not null access type_view;
		direction	: in type_cursor_direction;
		cursor		: in out type_cursor) is
	begin
		case direction is
			when RIGHT =>
				cursor.position := type_point (move (cursor.position, 0.0, self.drawing.grid.x));

			when LEFT =>
				cursor.position := type_point (move (cursor.position, 180.0, self.drawing.grid.x));

			when UP =>
				cursor.position := type_point (move (cursor.position, 90.0, self.drawing.grid.y));

			when DOWN =>
				cursor.position := type_point (move (cursor.position, -90.0, self.drawing.grid.y));
		end case;
		
		update_position_display_cursor;
		self.shift_area (cursor);
	end move_cursor;
	
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
			height		=> self.drawing.frame_bounding_box.height);

		pac_draw_package.draw_line (
			area		=> in_area,
			context		=> context,
			line		=> lv,
			height		=> self.drawing.frame_bounding_box.height);
		
		cairo.stroke (context.cr);		
	end draw_cursor;

	
end et_canvas_board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
