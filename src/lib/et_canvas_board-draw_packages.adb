------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          BOARD DRAW PACKAGES                             --
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
with et_symbols;
with et_schematic;				use et_schematic;
use et_schematic.type_nets;

with et_project;				use et_project;
with et_pcb_coordinates;		use et_pcb_coordinates;
with et_packages;				--use et_packages;
use et_pcb_coordinates.geometry;

with et_pcb;					--use et_pcb;
-- use et_pcb.pac_vias;

-- with et_pcb_stack;				use et_pcb_stack;

with et_display.board;			use et_display.board;

with et_canvas_primitive_draw_ops;

separate (et_canvas_board)

procedure draw_packages (
	self    : not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context;
	face	: in type_face) is

	procedure draw_package (
		model			: in et_packages.type_package_model_file.bounded_string;
		position		: in et_pcb_coordinates.type_package_position; -- incl. angle and face
		flip			: in et_pcb.type_flipped;
		placeholders	: in et_packages.type_text_placeholders) is

		use et_packages;
		use et_packages.pac_shapes;
		use type_packages;

		function flipped return boolean is 
			use et_pcb;
		begin
			if flip = NO then return false;
			else return true;
			end if;
		end flipped;
			
		cursor : et_packages.type_packages.cursor := locate_package_model (model);
	

		procedure draw_silkscreen is 
			use et_pcb;
			
			use type_silk_lines;
			line : type_silk_line;

			procedure draw (f : in type_face) is begin
				rotate_by (line, rot (position));
				move_by (line, type_point (position));

				set_color_silkscreen (context.cr, f);
				set_line_width (context.cr, type_view_coordinate (line.width));
				pac_draw_package.draw_line (in_area, context, line, self.frame_height);
				stroke (context.cr);
			end draw;
			
			use type_silk_arcs;
			use type_silk_circles;


			
			procedure query_line_top (c : in type_silk_lines.cursor) is begin
				line := element (c);
				
				if flipped then
					if silkscreen_enabled (BOTTOM) then
						mirror (line, Y);
						draw (BOTTOM);
					end if;
				else
					if silkscreen_enabled (TOP) then
						draw (TOP);
					end if;
				end if;
			end query_line_top;

			procedure query_line_bottom (c : in type_silk_lines.cursor) is begin
				line := element (c);
				
				if flipped then
					if silkscreen_enabled (TOP) then
						mirror (line, Y);
						draw (TOP);
					end if;
				else
					if silkscreen_enabled (BOTTOM) then
						draw (BOTTOM);
					end if;
				end if;
			end query_line_bottom;

			
		begin
			element (cursor).silk_screen.top.lines.iterate (query_line_top'access);
			element (cursor).silk_screen.bottom.lines.iterate (query_line_bottom'access);
		end draw_silkscreen;
		
	begin
		draw_silkscreen;
	end draw_package;
	
	procedure query_devices (
		module_name	: in type_module_name.bounded_string;
		module		: in type_module) is
		use et_schematic.type_devices;

		procedure query_device (d : in et_schematic.type_devices.cursor) is
			use et_symbols;
			use et_pcb;
		begin
			if element (d).appearance = PCB then
				
				draw_package (
					model			=> package_model (d), -- libraries/packages/smd/SOT23.pac
					position		=> element (d).position, -- x/y/rotation
					flip			=> element (d).flipped,
					placeholders	=> element (d).text_placeholders);
				
			end if;
		end query_device;
		
	begin -- query_devices
		module.devices.iterate (query_device'access);
	end query_devices;
	
begin -- draw_packages
-- 	put_line ("draw packages ...");
	
	type_modules.query_element (
		position	=> et_canvas_schematic.current_active_module,
		process		=> query_devices'access);

	
	-- CS non-electrical packages ? like fiducials
end draw_packages;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
