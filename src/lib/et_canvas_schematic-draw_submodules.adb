------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           DRAW SUBMODULES                                --
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

with ada.text_io;				use ada.text_io;
with cairo;						use cairo;
with pango.layout;				use pango.layout;

with et_general;				use et_general;
with et_project;				use et_project;
with et_coordinates;			use et_coordinates;
use et_coordinates.geometry;

with et_schematic;
with submodules;				use submodules;
use et_project.type_modules;

separate (et_canvas_schematic)

procedure draw_submodules (
	self	: not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context) is

	use type_submodules;
	
	procedure query_submods (cursor : in type_submodules.cursor) is
		
		procedure draw_box is begin

			cairo.set_line_width (context.cr, type_view_coordinate (submod_box_line_width));
			cairo.set_source_rgb (context.cr, gdouble (1), gdouble (0), gdouble (1)); -- mangenta

			pac_draw_misc.draw_rectangle (
				area			=> in_area,
				context			=> context,
				position		=> element (cursor).position,
				width			=> element (cursor).size.x,
				height			=> element (cursor).size.y,
				frame_height	=> self.drawing.frame_bounding_box.height);
								
			cairo.stroke (context.cr);
		end draw_box;

		procedure draw_ports is 
			use type_submodule_ports;

			procedure draw_port (pc : in type_submodule_ports.cursor) is 

				-- First get the position of the submodule box:
				pos_port : type_point := type_point (element (cursor).position);
			begin
				-- CS detect the edge where the port sits at. Depending on the edge
				-- the port must be drawn 0, 90, 180 or 270 degree.
				
				-- Move pos_port by the position of the port:
				move (pos_port, element (pc).position);

				-- Move pos_port down so that the port sits excatly at
				-- the point where a net will be connected:
				move (pos_port, set (zero, - port_symbol_height / 2.0));
				
				pac_draw_misc.draw_rectangle (
					area			=> in_area,
					context			=> context,
					position		=> pos_port,
					width			=> port_symbol_width,
					height			=> port_symbol_height,
					frame_height	=> self.drawing.frame_bounding_box.height);

				-- CS draw something that indicates the direction (master/slave).
				-- element (pc).direction

				-- CS draw port name
				-- key (pc)
			end draw_port;
			
		begin -- draw_ports
			cairo.set_line_width (context.cr, type_view_coordinate (port_symbol_line_width));
			cairo.set_source_rgb (context.cr, gdouble (1), gdouble (1), gdouble (1)); -- white

			iterate (element (cursor).ports, draw_port'access);
		
			cairo.stroke (context.cr);
		end draw_ports;
			
	begin -- query_submods
		save (context.cr);
		
		-- Prepare the current transformation matrix (CTM) so that
		-- all following drawing is relative to the upper left frame corner.
		translate (
			context.cr,
			convert_x (self.drawing.frame_bounding_box.x),
			convert_y (self.drawing.frame_bounding_box.y));


		draw_box;
		draw_ports;

		-- CS draw file name, instance name, position in board, view mode

		restore (context.cr);
		
	end query_submods;
		
begin
-- 	put_line ("draw submodules ...");

	iterate (element (current_active_module).submods, query_submods'access);
	
end draw_submodules;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
