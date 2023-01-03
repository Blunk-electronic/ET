------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          BOARD DRAW KEEPOUT                              --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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

--with ada.text_io;				use ada.text_io;
with et_keepout;				use et_keepout;
with et_colors;					use et_colors;

separate (et_canvas_board)

procedure draw_keepout (
	self    : not null access type_view;
	in_area	: in type_bounding_box := no_area;
	context : in type_draw_context;
	face	: in type_face) 
is
	use pac_geometry_2;	

	use pac_keepout_contours;
	use pac_keepout_cutouts;


	-- CS must be overwritten according to select status:
	brightness : type_brightness := NORMAL;

	
	procedure query_polygon (c : in pac_keepout_contours.cursor) is 
		drawn : boolean := false;
	begin
		draw_contour (
			area	=> in_area,
			context	=> context,
			contour	=> element (c),
			filled	=> NO,
			width	=> zero,
			height	=> self.frame_height,
			drawn	=> drawn);

	end query_polygon;

	
	procedure query_cutout (c : in pac_keepout_cutouts.cursor) is 
		drawn : boolean := false;
	begin
		set_color_background (context.cr);
		
		draw_contour (
			area	=> in_area,
			context	=> context,
			contour	=> element (c),
			filled	=> YES,
			width	=> zero,
			height	=> self.frame_height,
			drawn	=> drawn);

	end query_cutout;

	

	
	procedure query_items (
		module_name	: in pac_module_name.bounded_string;
		module		: in et_schematic.type_module) 
	is begin
		-- All keepout segments will be drawn with the same color:
		set_color_keepout (context.cr, face, brightness);

		cairo.set_line_width (context.cr, type_view_coordinate (keepout_line_width));
		
		case face is
			when TOP =>
				iterate (module.board.keepout.top.zones, query_polygon'access);
				iterate (module.board.keepout.top.cutouts, query_cutout'access);

			when BOTTOM =>
				iterate (module.board.keepout.bottom.zones, query_polygon'access);
				iterate (module.board.keepout.bottom.cutouts, query_cutout'access);
		end case;
	end query_items;
	
	
begin -- draw_keepout
-- 	put_line ("draw board keepout ...");
	
	pac_generic_modules.query_element (
		position	=> current_active_module,
		process		=> query_items'access);
	
end draw_keepout;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
